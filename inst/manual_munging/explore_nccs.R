## install.packages("devtools")
library(devtools)
## devtools::install_github("UrbanInstitute/nccsdata")
library(nccsdata)
library(purrr)
library(jtls)
library(pmdata)
library(RClickhouse)
library(RSQLite)
library(stringr)
library(parallel)



## dtx <- get_data(dsname = "core", time = "2010", ntee = "A", geo.city = "san francisco")

## dt_a51 <- get_data(dsname = "bmf", time = "2010", ntee = "A51", geo.city = "san francisco")

dt_nccs_urls <- data.table(
  orgtype = c("501C3-CHARITIES" ,  "501CE-NONPROFIT" , "501C3-PRIVFOUND"),
  scope = c("PZ"           ,  "PZ"         , "PF"),
  start_year = c(1989     ,  1989         , 1989),
  end_year = c(2019       ,  2019         , 2019)) %>%
  .[, .(year = start_year:end_year), .(orgtype, scope)] %>%
  .[!(orgtype == "501C3-PRIVFOUND" & year %in% c(1993, 2016, 2017, 2018))] # yeet some years w/o data

  

download_nccs <- function(orgtype, scope, year) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' download: download.file is actually good

    DIR_NCCS <- "/run/media/johannes/data/nccs"

    url <- sprintf("https://nccsdata.s3.us-east-1.amazonaws.com/legacy/core/CORE-%s-%s-%s.csv",
                   year, orgtype, scope)

    ## get_data(dsname = 'core', time = as.character(year), scope.orgtype = "NONPROFIT", scope.formtype = "PC")

    ## get_data(dsname = 'core', time = c("2021"))

    target_file <- sprintf("%s/CORE-%s-%s-%s.csv", DIR_NCCS, year, orgtype, scope)

    l_files_already_there <- list.files(DIR_NCCS, full.names = T)

    ## print("target_file: %s ", target_file)
    print(target_file)
    

    if (target_file %!in% l_files_already_there) {
        download.file(url, destfile = target_file)
        Sys.sleep(20) 
        ## print(url)
    }
}




gc_nccs_schema <- function(nccs_filepath, con) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    dt_nccs <- fread(nccs_filepath)

    ## RSQLite::dbDataType(con2, mtcars)

    ## get the columns for which there all NAs -> don't use them
    l_all_NAs <- dt_nccs[, map(.SD, ~all(is.na(.x)))] %>%
        melt(measure.vars = names(.), variable.factor = F) %>%
        .[value == TRUE, variable]

    ## 
    l_relcols <- setdiff(names(dt_nccs), l_all_NAs)

    ## generate the classification for all the columns
    l_schema <- map(l_relcols,
                    ~RClickhouse::dbDataType(con, dt_nccs[ , get(.x)])) %>% unlist

    dt_schema <- data.table(vrbl = c(l_relcols, l_all_NAs),
                            schema = c(l_schema, rep("allNAs", len(l_all_NAs))))

    
    year_ext <- as.integer(str_extract(nccs_filepath, "(?<=CORE-)(\\d{4})(?=-501)"))
    orgtype_ext <- str_extract(nccs_filepath, "(?<=CORE-\\d{4}-501C[3E]-)([^-]+)(?=-P)")
    scope_ext <- fifelse(orgtype_ext == "PRIVFOUND", "PF", "PZ")

    dt_schema[, `:=`(year = year_ext, orgtype = orgtype_ext, scope = scope_ext)]

    return(dt_schema)
}

## gc_nccs_schema(l_nccs_files[1], con)






## ** slice out museum variables

gc_vrblcfg <- function(orgtype, year) {
    #' select the proper variables for an overall concept based on orgtype and year
    #' construction needs checking of all the data codes on
    #' https://urbaninstitute.github.io/nccs-legacy/dictionary/core/core_archive_html/CORE-1989-501CE-NONPROFIT-PZ
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    
    c_vrblcfg <- list(
        ein = "ein",
        nteecc = "nteecc",
        taxper = "taxper",
        styear = "styear",
        outnccs = "outnccs",
        ## contributions:
        cont = if (orgtype == "NONPROFIT") {if (year < 2000) {"p1tcont"} else {"cont"}}
                else if (orgtype == "CHARITIES") {"cont"}
                else if (orgtype == "PRIVFOUND") {"p1tcont"},
        ## total revenue
        totrev = if (orgtype == "CHARITIES") {"totrev"}
                 else if (orgtype == "NONPROFIT") {if (year %in% c(2000, 2001)) {"totrev2"}
                                                   else if (year %in% c(1991, 1992)) {"estp1totrev"}
                                                   else if (year %in% c(1989, 1990)) {"make_me_NA"}
                                                   else if (year < 2000) {"p1totrev"}
                                                   else {"totrev"}}
                 else if (orgtype == "PRIVFOUND") {"p1totrev"},
        ## program revenues not there for foundations -> need to make this NA later on
        progrev = if (orgtype == "NONPROFIT") {if (year >= 2000) {"progrev"}
                                               else if (year %in% c(1989, 1990)) {"make_me_NA"}
                                               else {"p1psrev"}}
                  else if (orgtype == "CHARITIES") {"progrev"}
                  else if (orgtype == "PRIVFOUND") {"make_me_NA"},
        ## expenses
        exps = if (orgtype == "NONPROFIT") {if (year < 2000) {"p1totexp"} else {"exps"}}
               else if (orgtype == "CHARITIES")  {"exps"}
               else if (orgtype == "PRIVFOUND") {"p1totexp"},
        assets = if (orgtype ==  "NONPROFIT") {if (year < 2000) {"p1naseoy"} else {"fundbal"}}
                 else if (orgtype == "CHARITIES") {"fundbal"}
        ## FIXME: can probably calculate net assets for privfound pre 1997 from available data
                 else if (orgtype == "PRIVFOUND") {if (year >= 1997) {"p3eytfnd"} else {"make_me_NA"}} 
    )

    return(c_vrblcfg)
    
}

## gc_vrblcfg("NONPROFIT", 1995)



gd_extract_museums <- function(nccs_filepath) {
    #' get all the museum with some key information
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    print(nccs_filepath)

    dt_nccs <- fread(nccs_filepath) %>% setnames(old = names(.), new = tolower(names(.)))

    year_ext <- as.integer(str_extract(nccs_filepath, "(?<=CORE-)(\\d{4})(?=-501)"))
    orgtype_ext <- str_extract(nccs_filepath, "(?<=CORE-\\d{4}-501C[3E]-)([^-]+)(?=-P)")
    scope_ext <- fifelse(orgtype_ext == "PRIVFOUND", "PF", "PZ")

    ## get variables to extract
    l_vrbls <- gc_vrblcfg(orgtype_ext, year_ext)
    l_na_vrbls <- keep(l_vrbls, ~.x == "make_me_NA")

    ## if there are variables that are not available, yeet them and make them NA later
    if (len(l_na_vrbls) > 0) {
        l_vrbls <- keep(l_vrbls, ~.x != "make_me_NA")
    }

    ## filter out only museums, and ony the columns
    dt_subset <- dt_nccs[substring(nteecc, 1,2) == "A5", .SD, .SDcols = unlist(l_vrbls)] %>%
        setnames(old = unlist(l_vrbls), new = names(l_vrbls)) %>%
        .[, `:=`(year = year_ext, orgtype = orgtype_ext, scope = scope_ext)]

    ## set non-existing variables (for PRIVFOUND) to NA
    if (len(l_na_vrbls) > 0) {
        dt_subset[, (names(l_na_vrbls)) := NA]
    }

    return(dt_subset)
       
}

## gd_extract_museums(l_nccs_files[35])

## * main




## ** download
if (interactive()) {stop("it's interactive time")}

DIR_NCCS <- "/run/media/johannes/data/nccs"
lapply(split(dt_nccs_urls, 1:nrow(dt_nccs_urls)), \(x) download_nccs(x$orgtype, x$scope, x$year))


## ** schema construction

l_nccs_files <- list.files(DIR_NCCS, full.names = T)
con <- DBI::dbConnect(RClickhouse::clickhouse(), dbname = "nccs")
dt_schema <- mclapply(l_nccs_files, \(x) gc_nccs_schema(x, con), mc.cores = 6, mc.preschedule = F) %>% rbindlist

## *** schema eval

## how much do schemas differ? 
dt_schema[, .(nbr_unique = uniqueN(schema)), vrbl][, .N, nbr_unique]


dt_schema[, (schema = unique(schema)), vrbl]

## search where the EIN is char

dt_debug <- fread(paste0(DIR_NCCS, "/CORE-2005-501CE-NONPROFIT-PZ.csv"))

## check that conversion to lower case still keeps uniqueness
dt_schema[, .(nunq_atm = uniqueN(vrbl), nunq_lower = uniqueN(tolower(vrbl))), .(orgtype, scope, year)] %>%
    .[nunq_atm != nunq_lower]
## looks good

## ** constructing gc_vrblcfg by seeing how variable names differ across form/years

dt_schema[, .SD["NTEECC" %!in% vrbl], .(year, orgtype, scope)]

## search which columns are used for NTEE codes
dt_schema[, .SD[!any(vrbl == "NTEECC")], .(year, orgtype, scope)] %>%
    .[grepl("NTEE", vrbl)]
    ## .[, .N, .(year, orgtype, scope)]

dt_schema[, .N, vrbl][order(-N)] %>% print(n=80)

dt_schema[year > 2000, .(N = .N, Nform = uniqueN(schema), vlu = paste0(unique(schema), collapse = ",")), vrbl] %>% 
    .[order(-N)] %>% print(n=80)


dt_schema[year == 2010 & (vrbl == "TOTREV2" | vrbl == "P1TOTREV")]



dt_schema[orgtype == "CHARITIES" & year >= 2000][, head(.SD), year] %>% print(n=300)

dt_schema[year >= 2000 & orgtype == "CHARITIES" & vrbl == "OUTNCCS"]

dt_schema[vrbl == "OUTNCCS", .N, year]

dt_schema[year == 2005][grepl("out", vrbl, ignore.case = T)]

dt_schema[orgtype == "CHARITIES" & vrbl == "PROGREV"]
dt_schema[orgtype == "NONPROFIT" & vrbl == "STYEAR"]

dt_schema[orgtype == "PRIVFOUND" ][vrbl == "P1TOTREV"]
dt_schema[orgtype == "PRIVFOUND" & year >= 2000][grepl("fis|yr", vrbl, ignore.case = T)]


dt_schema[orgtype == "NONPROFIT" & grepl("p1psrev", vrbl, ignore.case = T)]
dt_schema[year == 2002 & grepl("prog", vrbl, ignore.case = T)]
dt_schema[grepl("longitude", vrbl, ignore.case = T)]
dt_schema[grepl("address", vrbl, ignore.case = T)]

dt_schema[orgtype == "CHARITIES" & grepl("totrev", vrbl, ignore.case = T)]
dt_schema[orgtype == "CHARITIES" & vrbl == "FUNDBAL"]



## ** museum 

dtx <- fread("/run/media/johannes/data/nccs/CORE-2019-501CE-NONPROFIT-PZ.csv")


dt_muem <- mclapply(l_nccs_files, \(x) gd_extract_museums(x), mc.cores = 6, mc.preschedule = F) %>%
    rbindlist(use.names = T)

## *** basic museum stats

dt_muem[nteecc == "A51"]

dt_muem[nteecc == "A51", .N, year]
dt_muem[nteecc == "A51", uniqueN(ein)]

dt_muem[nteecc == "A51" & year > 2013, uniqueN(ein)]
## 641 -> there are 681 who have filed before 2014 who haven't filed afterwards

## check type: mostly charities
dt_muem[nteecc == "A51", uniqueN(ein), orgtype]

## could use name to identify at some point
## dt_muem[nteecc == "A51", uniqueN(name)]

library(ggridges)

## distribution of assets
dt_muem[, .(assets = mean(assets)), .(ein, nteecc)] %>%
    .[, map(c("mean", "sd", "median"), ~get(.x)(assets, na.rm = T)), nteecc] %>% atb
## art museums have yuge variation: median assets 194k, mean assets 13m: mean 70x the median oooooooof
    

dt_muem[, .(assets = mean(assets)), .(ein, nteecc)] %>%
    .[assets > 0] %>% 
    ggplot(aes(x=log(assets), y = nteecc)) + 
    geom_density_ridges() + theme_ridges()

## geom_density() +


    

## *** basic museum survival 
## check survival
## get closed museums: not active since 2013: first get all,
## then subtract from them the ones that are active post-2013

dt_muem_closed <- dt_muem[, .(ein = unique(ein))] %>%
    .[!dt_muem[styear > 2013, .(ein = unique(ein))], on = "ein"]
    
## carry over other stats, will be lagged away anyways in survival
dt_muem_closed_year <- dt_muem[dt_muem_closed, on = "ein"][, .SD[which.max(year)], ein] %>% copy %>%
    .[, `:=`(year = year + 1, closed = 1)]

## set up grid to fill up temporary non-filing
dt_muem_grid <- dt_muem[, .(year = min(year):max(year)), ein]

## join with closed

dt_muem_cbn <- rbind(
    merge(dt_muem_grid, dt_muem, by = c("ein", "year"), all.x = T)[, closed := 0],
    dt_muem_closed_year)




dt_muem_cbn[, .(prop_closed = sum(closed)/.N, .N, n_closed = sum(closed)), .(styear, nteecc)] %>%
    .[styear %between% c(2000, 2014),
      .(prop_closed = mean(prop_closed), total_atrisk = sum(N), total_closed = sum(n_closed),
        avg_closed = mean(n_closed), avg_atrisk = mean(N)), nteecc]

## check whether same museum can have multiple neetcc
dt_muem[, .(uniqueN_nteecc = uniqueN(nteecc)), ein][, .N, uniqueN_nteecc]
## yeah quite some have multiple

## check whether museum can be in multiple years
dt_muem[, .(muem_year_N = .N), .(ein, styear)][, .N, muem_year_N]
## yup quite some are in multiple years





## ## ** Sara data: time range
## dt_sara1 <- fread("/home/johannes/Dropbox/phd/sara/fmerged_990.csv")
## dt_sara1[, min(tax_pd, na.rm = T)]

## dt_sara2 <- fread("/home/johannes/Dropbox/phd/sara/fmerged_990pf.csv")
## dt_sara2[, min(TAX_PRD, na.rm =T)]


## ** big query? IRS directly?

dt_irs <- fread("~/Downloads/eo_ut.csv") %>% adt
dt_irs <- fread("~/Downloads/11in54cm.xlsx") %>% adt

