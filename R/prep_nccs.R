
#' generate data.table of which URLs to download from NCCS
#' structured as orgtype, scope, year
gd_nccs_urls <- function() {
    
    dt_nccs_urls <- data.table(
        orgtype = c("501C3-CHARITIES" ,  "501CE-NONPROFIT" , "501C3-PRIVFOUND"),
        scope = c("PZ"           ,  "PZ"         , "PF"),
        start_year = c(1989     ,  1989         , 1989),
        end_year = c(2019       ,  2019         , 2019)) %>%
        .[, .(year = start_year:end_year), .(orgtype, scope)] %>%
        .[!(orgtype == "501C3-PRIVFOUND" & year %in% c(1993, 2016, 2017, 2018))] # yeet some years w/o data

    return(dt_nccs_urls)
}



#' download an individual NCCS file
#'
#' don't use the official package, which just also downloads everything (no good server-side filtering)
#' downloading everything is like 10 GB so super doable
#' @param orgtype one of 501C3-CHARITIES, 501CE-NONPROFIT, 501C3-PRIVFOUND
#' @param scope PZ or PF
#' @param year year
download_nccs_vid <- function(orgtype, scope, year) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' download: download.file is actually good


    url <- sprintf("https://nccsdata.s3.us-east-1.amazonaws.com/legacy/core/CORE-%s-%s-%s.csv",
                   year, orgtype, scope)

    ## get_data(dsname = 'core', time = as.character(year), scope.orgtype = "NONPROFIT", scope.formtype = "PC")

    ## get_data(dsname = 'core', time = c("2021"))

    target_file <- sprintf("%s/CORE-%s-%s-%s.csv", DIR_NCCS_CORE, year, orgtype, scope)

    l_files_already_there <- list.files(DIR_NCCS_CORE, full.names = T)

    ## print("target_file: %s ", target_file)
    print(target_file)
    

    if (target_file %!in% l_files_already_there) {
        download.file(url, destfile = target_file)
        Sys.sleep(20) 
        ## print(url)
    }
}


#' download all the NCCS files that aren't downloaded yet
#'
#' @param DIR_NCCS_CORE storage path for NCCS files
#' @export
download_nccs_files <- function(DIR_NCCS_CORE = PMDATA_LOCS$DIR_NCCS_CORE) {

    dt_nccs_urls <- gd_nccs_urls() 

    lapply(split(dt_nccs_urls, 1:nrow(dt_nccs_urls)), \(x) download_nccs(x$orgtype, x$scope, x$year))

}

#' get SQL schema for an invididual (vid) NCCS file
#' 
#' @param nccs_filepath filepath to NCCS file
gc_nccs_schema_vid <- function(nccs_filepath) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    con <- dbConnect(SQLite())
    
    dt_nccs <- fread(nccs_filepath, nrows = 5)

    ## RSQLite::dbDataType(con2, mtcars)

    ## get the columns for which there all NAs -> don't use them
    l_all_NAs <- dt_nccs[, map(.SD, ~all(is.na(.x)))] %>%
        melt(measure.vars = names(.), variable.factor = F) %>%
        .[value == TRUE, variable]

    ## 
    l_relcols <- setdiff(names(dt_nccs), l_all_NAs)

    ## generate the classification for all the columns
    l_schema <- map(l_relcols,
                    ~RSQLite::dbDataType(con, dt_nccs[ , get(.x)])) %>% unlist
                    
    dbDisconnect(con)
    dt_schema <- data.table(vrbl = c(l_relcols, l_all_NAs),
                            schema = c(l_schema, rep("allNAs", len(l_all_NAs))))

    
    year_ext <- as.integer(str_extract(nccs_filepath, "(?<=CORE-)(\\d{4})(?=-501)"))
    orgtype_ext <- str_extract(nccs_filepath, "(?<=CORE-\\d{4}-501C[3E]-)([^-]+)(?=-P)")
    scope_ext <- fifelse(orgtype_ext == "PRIVFOUND", "PF", "PZ")

    dt_schema[, `:=`(year = year_ext, orgtype = orgtype_ext, scope = scope_ext)]

    return(dt_schema)
}

#' generate a SQLite schema for all the NCCS files
#'
#' @param DIR_NCCS_CORE directory to NCCS core files
#' @export
gd_nccs_schema <- function(DIR_NCCS_CORE = PMDATA_LOCS$DIR_NCCS_CORE) {

    l_nccs_files <- list.files(DIR_NCCS_CORE, full.names = T)
    gc_nccs_schema_vid(l_nccs_files[1])

    mclapply(l_nccs_files, \(x) gc_nccs_schema_vid(x), mc.cores = 6, mc.preschedule = F) %>% rbindlist
    
}


gc_nccs_vrblcfg <- function(orgtype, year) {
    #' select the proper variables for an overall concept based on orgtype and year
    #' construction needs checking of all the data codes on
    #' https://urbaninstitute.github.io/nccs-legacy/dictionary/core/core_archive_html/CORE-1989-501CE-NONPROFIT-PZ
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    
    c_vrblcfg <- list(
        ein = "ein",
        nteecc = "nteecc",
        taxper = "taxper",
        name = "name",
        address = "address",
        city = "city",
        state = "state",
        zip = "zip",
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



gd_nccs_extract_museums <- function(nccs_filepath) {
    #' get all the museum with some key information
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    print(nccs_filepath)

    dt_nccs <- fread(nccs_filepath) %>% setnames(old = names(.), new = tolower(names(.)))

    year_ext <- as.integer(str_extract(nccs_filepath, "(?<=CORE-)(\\d{4})(?=-501)"))
    orgtype_ext <- str_extract(nccs_filepath, "(?<=CORE-\\d{4}-501C[3E]-)([^-]+)(?=-P)")
    scope_ext <- fifelse(orgtype_ext == "PRIVFOUND", "PF", "PZ")

    ## get variables to extract
    l_vrbls <- gc_nccs_vrblcfg(orgtype_ext, year_ext)
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

#' generate data.frame with NCCS museum data (NTEE A5X)
#' takes a while to write to file for later usage
#'
#' @param DIR_NCCS_CORE directory with NCCS core files
#' @param FILE_NCCS_MUSEUMS file for museum df
gwd_nccs_muem <- function(DIR_NCCS_CORE = PMDATA_LOCS$DIR_NCCS_CORE,
                          FILE_NCCS_MUSEUMS = PMDATA_LOCS$FILE_NCCS_MUSEUMS) {

    l_nccs_files <- list.files(DIR_NCCS_CORE, full.names = T)

    dt_nccs_muem <- mclapply(l_nccs_files, \(x) gd_nccs_extract_museums(x), mc.cores = 6, mc.preschedule = F) %>%
        rbindlist(use.names = T)
    

    fwrite(dt_nccs_muem, FILE_NCCS_MUSEUMS)

}

#' read the NCCS museum file
#'
#' @param FILE_NCCS_MUSEUMS file for museum df
#' @export
gd_nccs_muem <- function(FILE_NCCS_MUSEUMS = PMDATA_LOCS$FILE_NCCS_MUSEUMS) {
    fread(FILE_NCCS_MUSEUMS)
}
    
