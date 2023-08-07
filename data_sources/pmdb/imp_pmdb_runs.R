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
    m1 <- as.integer(pryr::mem_used())
    ## dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F)
    ## dtx <- gendtm_pmdb_excl(PMDB_FILE, only_pms = F)
    dtx <- 10 %>>% sqrt
    rm(dtx)    
    gc()
    m2 <- as.integer(pryr::mem_used())
    print(m2-m1)
}





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



