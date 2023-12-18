## * main file


#' @importFrom purrr map
#' @importFrom magrittr `%>%`
#' @importFrom countrycode countrycode
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringi stri_split_fixed
#' @import data.table
#' @import collapse
#' @rawNamespace import(stats, except = D) # don't import D, which comes from collapse
## https://stackoverflow.com/questions/51899220/import-all-the-functions-of-a-package-except-one-when-building-a-package

.datatable.aware = T


#' generate list of data source locations
#'
#' main goals here:
#' 1. access data files reliably across projects
#' 2. allow porting to other directories
#' 3. have option to generate data frames without passing explicit location argument (e.g. gd_X())
#' For this purpose `gen_data_locs` generates a list of locations of files used for input
#' which is used as default argument for data generation functions that read files.
#'
#' requires updating of the values in this list if data source changes (e.g. new google sheets version)
#' 
#' @param DATA_DIR location which includes data sources, default set to current dir
#' @return list of strings of location of data files
#' @export
gc_pmdata_locs <- function(DATA_DIR = "/home/johannes/Dropbox/phd/pmdata/data_sources/") {

    ## FIXME: move artnews to DATA_DIR: atm the ownCloud directory is big (~300 mb) due to including all the HTMLs
    ## the ARTNEWS_COLLECTOR_ENTRIES_FILE is already in the correct directory
    ARTNEWS_DIR_old <- "/home/johannes/ownCloud/org_pop_data/artnews/"
        
    data_locs <- list(
        ## PMDB section
        PMDB_FILE                    = paste0(DATA_DIR, "pmdb/Private museum database_v6.csv"),
        PMDB_FOUNDER_PERSON_FILE_ORG = paste0(DATA_DIR, "pmdb/pmdb_founder_person.org"),
        PMDB_FOUNDER_PERSON_FILE_CSV = paste0(DATA_DIR, "pmdb/pmdb_founder_person.csv"),
        PMDB_PPECPRN_FILE            = paste0(DATA_DIR, "pmdb/pmdb_ppecprn.csv"),
        PMDB_FOUNDER_PERSON_FILE_WID = paste0(DATA_DIR, "pmdb/pmdb_founder_person_wid.csv"),
        ## MOW section 
        MOW_PMDB_MATCHRES_FILE = paste0(DATA_DIR, "mow/mow_pmdb_matchres.csv"),
        MOW_INFO_FILE          = paste0(DATA_DIR, "mow/mow.csv"),
        MOW_CLSFCN_FILE        = paste0(DATA_DIR, "mow/classification.csv"),
        MOW_TAG_FILE           = paste0(DATA_DIR, "mow/type.csv"),
        ## ARTNEWS section
        ARTNEWS_TIME_FILE                 = paste0(DATA_DIR, "artnews/an_ranking_time.csv"),
        ARTNEWS_COLLECTOR_ENTRIES_FILE    = paste0(DATA_DIR, "artnews/an_clctr_entries.csv"),
        ARTNEWS_COLLECTOR_PERSON_FILE     = paste0(DATA_DIR, "artnews/an_clctr_person.csv"),
        ARTNEWS_COLLECTOR_PERSON_FILE_ORG = paste0(DATA_DIR, "artnews/an_clctr_person_org.csv"),
        ARTNEWS_PERSON_FILE               = paste0(DATA_DIR, "artnews/an_person.csv"), # file for person uniqueness
        ARTNEWS_COLLECTOR_PERSON_FILE_WID = paste0(DATA_DIR, "artnews/an_person_collector_wid.csv"), # with ID
        ARTNEWS_APECPRN_FILE              = paste0(DATA_DIR, "artnews/an_apecprn.csv"), # check person uniqueness
        ARTNEWS_LOCTABLE_FILE             = paste0(DATA_DIR, "artnews/an_loctable.csv"),
        ARTNEWS_PMDB_MATCHRES_FILE        = paste0(DATA_DIR, "artnews/an_pmdb_matchres.csv"),
        ## GEOGRAPHIC INFORMATION
        COUNTRY_BOUNDARIES_FILE  = paste0(DATA_DIR, "cry_boundaries/geodatasource_land_boundaries.csv")
        ## MARITIME_BOUNDARIES_FILE = paste0(DATA_DIR, "cry_boundaries/9051-world-maritime-boundaries.rdf")
    )
        

    

    return(data_locs)
}

globalVariables(c("PMDATA_LOCS", ".","%>%", "fstd", "adt"))


## PMDATA_LOCS <- gc_pmdata_locs()
#' test me
#' @export
## testf <- function() {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
        
##     ## cooperation <- NULL
##     ## acts <- NULL
    
##     ## walk(c("cooperation", "acts"), ~assign(.x, NULL, envir = environment(testf)))
##     ## walk(c("cooperation", "acts"), ~assign(.x, NULL)) # , envir = environment(testf)))

##     globalVariables
##     suppressForeignCheck
##     envx <- environment()
##     walk(c("pepega", "keepo"), ~assign(.x, NULL, envir = envx))
##     walk(c("pepega", "keepo"), ~assign(.x, NULL, envir = parent.frame()))
    

##     dtx <- data.table(pepega = 1)[, keepo := pepega + 2]

##     ## print(cooperation)
##     ## print(acts)
## }

## testf()

## rm(cooperation)
## rm(acts)

                          

#' ## test function
## #'
## #' this is some description
## #' @export 
## testf1 <- memoise(function() {
##     15
## })

## #' @export 
## testf2 <- memoise(function() {
##     290 + testf1()
## })
