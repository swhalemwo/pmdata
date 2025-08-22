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



## ** slice out museum variables

## gc_vrblcfg("NONPROFIT", 1995)

## gc_nccs_schema_vid(l_nccs_files[1])[, .N, schema]



## gd_extract_museums(l_nccs_files[35])

## * main


gd_extract_museums(l_nccs_files[1])

## ** download
if (interactive()) {stop("it's interactive time")}


## ** schema construction

dt_nccs_schema <- gd_nccs_schema()

dt_nccs_schema[, .N, .(orgtype, year)]

dt_comcols <- dt_nccs_schema %>% split(~orgtype + year) %>% keep(~nrow(.x) > 0) %>% 
    Reduce(\(x,y) data.table(vrbl = intersect(x[, vrbl], y[, vrbl])), .)



## *** schema eval

## how much do schemas differ? 
dt_nccs_schema[, .(nbr_unique = uniqueN(schema)), vrbl][, .N, nbr_unique]

dt_nccs_schema[, .N, vrbl] %>% print(n=300)


dt_nccs_schema[, (schema = unique(schema)), vrbl]

## search where the EIN is char

dt_debug <- fread(paste0(DIR_NCCS, "/CORE-2005-501CE-NONPROFIT-PZ.csv"))

## check that conversion to lower case still keeps uniqueness
dt_nccs_schema[, .(nunq_atm = uniqueN(vrbl), nunq_lower = uniqueN(tolower(vrbl))), .(orgtype, scope, year)] %>%
    .[nunq_atm != nunq_lower]
## looks good


## ** constructing gc_vrblcfg by seeing how variable names differ across form/years

dt_nccs_schema[, .SD["NTEECC" %!in% vrbl], .(year, orgtype, scope)]

## search which columns are used for NTEE codes
dt_nccs_schema[, .SD[!any(vrbl == "NTEECC")], .(year, orgtype, scope)] %>%
    .[grepl("NTEE", vrbl)]
    ## .[, .N, .(year, orgtype, scope)]

dt_nccs_schema[, .N, vrbl][order(-N)] %>% print(n=80)

dt_nccs_schema[year > 2000, .(N = .N, Nform = uniqueN(schema), vlu = paste0(unique(schema), collapse = ",")), vrbl] %>% 
    .[order(-N)] %>% print(n=80)


dt_nccs_schema[year == 2010 & (vrbl == "TOTREV2" | vrbl == "P1TOTREV")]


dt_nccs_schema[orgtype == "CHARITIES" & year >= 2000][, head(.SD), year] %>% print(n=300)

dt_nccs_schema[year >= 2000 & orgtype == "CHARITIES" & vrbl == "OUTNCCS"]

dt_nccs_schema[vrbl == "OUTNCCS", .N, year]

dt_nccs_schema[year == 2005][grepl("out", vrbl, ignore.case = T)]

dt_nccs_schema[orgtype == "CHARITIES" & vrbl == "PROGREV"]
dt_nccs_schema[orgtype == "NONPROFIT" & vrbl == "STYEAR"]

dt_nccs_schema[orgtype == "PRIVFOUND" ][vrbl == "P1TOTREV"]
dt_nccs_schema[orgtype == "PRIVFOUND" & year >= 2000][grepl("fis|yr", vrbl, ignore.case = T)]


dt_nccs_schema[orgtype == "NONPROFIT" & grepl("p1psrev", vrbl, ignore.case = T)]
dt_nccs_schema[year == 2002 & grepl("prog", vrbl, ignore.case = T)]
dt_nccs_schema[grepl("longitude", vrbl, ignore.case = T)]
dt_nccs_schema[grepl("address", vrbl, ignore.case = T)]

dt_nccs_schema[orgtype == "CHARITIES" & grepl("totrev", vrbl, ignore.case = T)]
dt_nccs_schema[orgtype == "CHARITIES" & vrbl == "FUNDBAL"]

## ** comparing old and new format

dt_2019_char <- fread("~/Downloads/CORE-2019-501C3-CHARITIES-PZ-HRMN.csv")
dt_2019_char_old <- fread(paste0(PMDATA_LOCS$DIR_NCCS, "/CORE-2019-501C3-CHARITIES-PZ.csv"))

## dt_2019_char_old[, .SD, .SDcols = dt_comcols[, vrbl]] %>% head(n=50) %>% view_xl

intersect(names(dt_2019_char), names(dt_2019_char_old))

## dt_2019_char[, .N, substring(EIN2, 1,6)] %>% print(n=80)

dt_2019_char_old[, EIN2 := sprintf("EIN-01")]

dt_bmf <- fread("~/Downloads/BMF_UNIFIED_V1.1.csv", nrows = 10) 
dt_bmf[, .(EIN, EIN2)]

## GIGAYIKES: super different


## ** museum 

## dtx <- fread("/run/media/johannes/data/nccs/core/CORE-2019-501CE-NONPROFIT-PZ.csv")
## dtx <- fread("/run/media/johannes/data/nccs/core/CORE-2019-501C3-PRIVFOUND-PF.csv")



dt_nccs_muem <- gd_nccs_muem()


## *** basic museum stats

dt_nccs_muem[nteecc == "A51"]

dt_nccs_muem[nteecc == "A51", .N, year]
dt_nccs_muem[nteecc == "A51", uniqueN(ein)]

dt_nccs_muem[nteecc == "A51" & year > 2013, uniqueN(ein)]
## 641 -> there are 681 who have filed before 2014 who haven't filed afterwards

## check type: mostly charities
dt_nccs_muem[nteecc == "A51", uniqueN(ein), orgtype]

## could use name to identify at some point
## dt_nccs_muem[nteecc == "A51", uniqueN(name)]

library(ggridges)

## distribution of assets
dt_nccs_muem[, .(assets = mean(assets)), .(ein, nteecc)] %>%
    .[, map(c("mean", "sd", "median"), ~get(.x)(assets, na.rm = T)), nteecc] %>% atb
## art museums have yuge variation: median assets 194k, mean assets 13m: mean 70x the median oooooooof
    

dt_nccs_muem[, .(assets = mean(assets)), .(ein, nteecc)] %>%
    .[assets > 0] %>% 
    ggplot(aes(x=log(assets), y = nteecc)) + 
    geom_density_ridges() + theme_ridges()

## geom_density() +


    

## *** basic museum survival 
## check survival
## get closed museums: not active since 2013: first get all,
## then subtract from them the ones that are active post-2013

dt_muem_closed <- dt_nccs_muem[, .(ein = unique(ein))] %>%
    .[!dt_nccs_muem[styear > 2013, .(ein = unique(ein))], on = "ein"]
    
## carry over other stats, will be lagged away anyways in survival
dt_muem_closed_year <- dt_nccs_muem[dt_muem_closed, on = "ein"][, .SD[which.max(year)], ein] %>% copy %>%
    .[, `:=`(year = year + 1, closed = 1)]

## set up grid to fill up temporary non-filing
dt_muem_grid <- dt_nccs_muem[, .(year = min(year):max(year)), ein]

## join with closed

dt_muem_cbn <- rbind(
    merge(dt_muem_grid, dt_nccs_muem, by = c("ein", "year"), all.x = T)[, closed := 0],
    dt_muem_closed_year)




dt_muem_cbn[, .(prop_closed = sum(closed)/.N, .N, n_closed = sum(closed)), .(styear, nteecc)] %>%
    .[styear %between% c(2000, 2014),
      .(prop_closed = mean(prop_closed), total_atrisk = sum(N), total_closed = sum(n_closed),
        avg_closed = mean(n_closed), avg_atrisk = mean(N)), nteecc]

## check whether same museum can have multiple neetcc
dt_nccs_muem[, .(uniqueN_nteecc = uniqueN(nteecc)), ein][, .N, uniqueN_nteecc]
## yeah quite some have multiple

## check whether museum can be in multiple years
dt_nccs_muem[, .(muem_year_N = .N), .(ein, styear)][, .N, muem_year_N]
## yup quite some are in multiple years





## ## ** Sara data: time range
## dt_sara1 <- fread("/home/johannes/Dropbox/phd/sara/fmerged_990.csv")
## dt_sara1[, min(tax_pd, na.rm = T)]

## dt_sara2 <- fread("/home/johannes/Dropbox/phd/sara/fmerged_990pf.csv")
## dt_sara2[, min(TAX_PRD, na.rm =T)]


## ** big query? IRS directly?



## dt_irs <- fread("~/Downloads/eo_ut.csv") %>% adt
## dt_irs <- fread("~/Downloads/11in54cm.xlsx") %>% adt

## gc_vrblcfg
## have contributions, revenue, program revenues, expenses, assets


# 
dt_soi <- fread("/run/media/johannes/data/nccs/soi/SOI-MICRODATA-2012-501C3-CHARITIES-PC.csv")
keep(names(dt_soi), ~grepl("exp", .x, ignore.case = T))
keep(names(dt_soi), ~grepl("rev", .x, ignore.case = T))

# use consult/emas for this
dt_core_vrbls <- fread("/run/media/johannes/data/nccs/CORE-HRMN_dd.csv")

dt_nccs_muem
dt_nccs_muem[nteecc == "A51"]
dt_nccs_muem[, .N, nteecc]


## ** get museum locations


dt_nccs_muem[, .(ein, name, address)] %>% unique
dt_nccs_muem[, .(ein, address)] %>% unique
dt_nccs_muem[, .(ein, name)] %>% unique




dt_nccs_muem[nteecc == "A51", .(ein, name, address)] %>% .[, uniqueN(address)]


dt_nccs_muem[, .(address, city, state, zip)]

dt_nccs_muem[, .(nchar_zip = nchar(zip))][, .N, nchar_zip]
