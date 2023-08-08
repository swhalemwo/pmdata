
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



## for some funny reason the first call always leaks some memory?



## ** testthat-based tests
## test_gendt_pmdb_excl <- function(dt_pmdb_excl2) {
##     test_that("true is true", {expect_true(T)})
##     test_that("all_PMs have ID", {expect_true(dt_pmdb_excl2[, none(ID, is.na)])})

    
## }

## test_gendt_pmdb_excl(dtx)


## test_file("~/Dropbox/phd/pmdata/data_sources/pmdb/tests_gendt_pmdb_excl.R")

## path <- testthat_example("failure")
## test_file(path)

    
    
    
    
## ** compare dt and collapse
## compare dts: 
dt_pmdb_excl_dt <- gendt_pmdb_excl(PMDB_FILE)[, `:=`(`Museum_opening year` = NULL, Museum_name = NULL,
                       Museum_country = NULL, `Museum_closing year` = NULL,
                       Museum_status = NULL)]

dt_pmdb_excl_clps <- gendtc_pmdb_excl(PMDB_FILE) %>% .[, .SD, .SDcols = names(dt_pmdb_excl_dt)]

identical(dt_pmdb_excl_dt, dt_pmdb_excl_clps)
fsetdiff(dt_pmdb_excl_dt, dt_pmdb_excl_clps)




microbenchmark(
    dt = gendt_pmdb_excl(PMDB_FILE),
    clps = gendtc_pmdb_excl(PMDB_FILE), times = 5)



