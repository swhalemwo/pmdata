## * main file


#' @importFrom purrr map
#' @importFrom countrycode countrycode
#' @import data.table
#' @import collapse


.datatable.aware = T


#' generate list of data source locations
#'
#' main goals here:
#' 1. access data files reliably across projects
#' 2. allow porting to other directories
#' 3. have option to generate data frames without passing explicit location argument (e.g. gendt_X())
#' For this purpose `gen_data_locs` generates a list of locations of files used for input
#' which is used as default argument for data generation functions that read files.
#'
#' requires updating of the values in this list if data source changes (e.g. new google sheets version)
#' 
#' @param DATA_DIR location which includes data sources, default set to current dir
#' @return list of strings of location of data files
#' @export
gen_data_locs <- function(DATA_DIR = "/home/johannes/Dropbox/phd/pmdata/data_sources/") {
    data_locs <- list(
        PMDB_FILE = paste0(DATA_DIR, "pmdb/Private museum database_v1.csv"))

    return(data_locs)
}
                          
#' ## testf
## #'
## #' @export
## testf <- function() {10}


## #' testf2 is a function
## #'
## #' that returns something 
## #' @export
## testf2 <- function(DATA_DIR = "/home/johannes/Dropbox/phd/pmdata/data_sources/") {
##     data_locs <- list(
##         PMDB_FILE = paste0(DATA_DIR, "pmdb/Private museum database_v1.csv"))
    
## }
