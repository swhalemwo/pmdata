## * main file


#' @importFrom purrr map
#' @importFrom countrycode countrycode
#' @import data.table
#' @import collapse
#' @importFrom memoise memoise

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
gc_pmdata_locs <- function(DATA_DIR = "/home/johannes/Dropbox/phd/pmdata/data_sources/") {
    data_locs <- list(
        PMDB_FILE = paste0(DATA_DIR, "pmdb/Private museum database_v4.csv"))

    return(data_locs)
}
                          

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
