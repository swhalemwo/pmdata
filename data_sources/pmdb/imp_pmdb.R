## * import PMDB

args <- commandArgs(trailingOnly = T)
options(width = 110)


## library(readxl)
library(purrr)
library(countrycode)
## library(pryr)
library(lobstr)
## library(xlsx)
## library(googlesheets4)
## library(rmonad)


vrbl_fndr <- function(df, penl_vrbls) {
    #' find the variable referred by by potential variables (penl_vrbls)

    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (len(vrbl) == 0 ) {
        stop("no variable found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else {
        vrbl
    }
}




gendt_pmdb_excl <- function(PMDB_FILE, only_pms=T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' just read in the excel file, add a few auxiliary columns 

    dt_pmdb_excl <- fread( PMDB_FILE)[2:.N]# skip second column
    ## dt_pmdb_excl[, map(.SD, class)] %>% melt(id.vars = "ID") %>% .[value != "character"] %>% .[, .N, value]
    ## dtx <- read.xlsx2(PMDB_FILE, sheetName = "Blad1") %>% adt() %>% .[2:.N]
    ## dtx[653:.N] %>% melt(id.vars = "ID") %>% .[value != ""]
    ## some additional basic aux columns, no transformation
    basic_cols <- list(
        country = c("Country where museum is located", "Museum_country"),
        name = c("Museum_name", "Name of museum"),
        year_opened_str =  c("Opening year", "Museum_opening year"),
        year_closed_str = c("Closing year", "Museum_closing year"),
        museum_status = c("Museum status", "Museum_status"))
    ## throw error if any of the aux columns are already there
    if (len(setdiff(names(basic_cols), names(dt_pmdb_excl))) != len(names(basic_cols))) {
        stop("new cols already there")}
    ## select basic_cols subset of columns, rename it
    dt_pmdb_excl2 <- dt_pmdb_excl[, .SD, .SDcols = unlist(map(basic_cols, ~vrbl_fndr(dt_pmdb_excl, .x)))] %>% 
        setnames(new = names(basic_cols))%>% # update names 
        cbind(dt_pmdb_excl) %>% # combine with old data
        .[, `:=`(ID = as.integer(ID), # edit ID columns 
                 iso3c = countrycode(country, "country.name", "iso3c"), # add iso3c column
                 year_opened = as.integer(year_opened_str),
                 year_closed = as.integer(year_closed_str))]

    ## check that all countries with country string convert to country code
    if (dt_pmdb_excl2[is.na(iso3c) & country != "", .N] > 0) {stop("not all countries are matched")}

    
    
    return(dt_pmdb_excl2)
        
}


gendt_pmdb_excl(PMDB_FILE, only_pms = F)



PMDATA_DIR <- "/home/johannes/Dropbox/phd/pmdata/"
PMDB_DIR <- paste0(PMDATA_DIR, "data_sources/pmdb/")
PMDB_FILE <- paste0(PMDB_DIR, "Private museum database_v1.csv")

## maybe add version number later

## * main

if (identical(args, character(0))) {
    stop("functions are done")
} else if (is.null(args[[1]])) {
    stop("functions are DONE")
}



dtx2 <- fread(paste0(PMDB_DIR, "dt_pmdb_excl.csv"))

gendt_pmdb_excl(PMDB_FILE, only_pms = F)


x <- read_sheet(PMDB_URL)





for(i in 1:10) {
    ## dtx <- leaker_test()
    dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F, rff = "fread")
    rm(dtx)    
    gc()
    print(as.integer(mem_used()))
}




dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F, rff = T)

fwrite()
object_size(dtx)

## memory profiling

mem_change(dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F))
object_size(dtx)


for(i in 1:40) {
    dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F)
    rm(dtx)    
    gc()
    print(pryr::mem_used())
}






    
    
    



