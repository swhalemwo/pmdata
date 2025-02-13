## * main file


#' @importFrom purrr map
#' @importFrom magrittr `%>%`
#' @importFrom countrycode countrycode
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringi stri_split_fixed stri_trans_general
#' @import data.table
#' @importFrom terra rast vect project extract
#' @importFrom RSQLite dbConnect dbGetQuery SQLite
#' @importFrom wbstats wb_data
#' @importFrom haven read_sav
#' @importFrom fuzzyjoin stringdist_inner_join
#' @rawNamespace import(stats, except = D) # don't import D, which comes from collapse
#' @rawNamespace import(collapse, except = fdroplevels)
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
        PMDB_FILE                    = paste0(DATA_DIR, "pmdb/Private museum database_v19.csv"),
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
        COUNTRY_BOUNDARIES_FILE  = paste0(DATA_DIR, "cry_boundaries/geodatasource_land_boundaries.csv"),
        ## MARITIME_BOUNDARIES_FILE = paste0(DATA_DIR, "cry_boundaries/9051-world-maritime-boundaries.rdf")
        ARTFACTS_SQLITE_DB          = paste0(DATA_DIR, "artfacts/db_artfacts.sqlite"),
        FILE_ARTFACTS_PMDB_MATCHES  = paste0(DATA_DIR, "artfacts/artfacts_pmdb_matches.csv"),
        FILE_ARTFACTS_INST_CLSFCN   = paste0(DATA_DIR, "artfacts/af_inst_clsfcn.csv"),
        ## GHSL (Global human settlement layer) from european commission
        DIR_GHSL = paste0(DATA_DIR, "ghsl/"),
        ## WAYBACK results
        FILE_WAYBACK_RES = paste0(DATA_DIR, "wayback/wayback.csv"),
        ## LOTSEARCH
        DIR_LOTSEARCH              = paste0(DATA_DIR, "lotsearch/"),
        FILE_LOTSEARCH_RES         = paste0(DATA_DIR, "lotsearch/lotsearch_res.csv"),
        FILE_DB_LOTSEARCH          = paste0(DATA_DIR, "lotsearch/db_lotsearch.sqlite"),
        FILE_LOTSEARCH_STRINGMATCH = paste0(DATA_DIR, "lotsearch/dists.csv"),
        ## EGMUS
        DIR_DATA_EGMUS  = paste0(DATA_DIR, "egmus/"),
        ## ARTPRICE
        FILE_AP_ARTIST_YEAR = paste0(DATA_DIR, "artprice/2006_2021.sav"),
        FILE_AP_ARTIST_ID = paste0(DATA_DIR, "artprice/artprice_artist_id.csv"),
        FILE_AP_UNQCHECK = paste0(DATA_DIR, "artprice/artprice_unqcheck.csv")
    )
        

    

    return(data_locs)
}

globalVariables(c("PMDATA_LOCS", ".","%>%", "fstd", "adt"))

## use this for developing
## PMDATA_LOCS <- gc_pmdata_locs()
