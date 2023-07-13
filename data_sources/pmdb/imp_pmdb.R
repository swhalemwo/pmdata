

gendt_pmdb_excl <- function(PMDB_FILE, only_pms=T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}


    dt_pmdb_excl <- fread(paste0(PMDB_DIR, PMDB_FILE))

}


PMDATA_DIR <- "/home/johannes/Dropbox/phd/pmdata/"
PMDB_DIR <- paste0(PMDATA_DIR, "data_sources/pmdb/")
PMDB_FILE <- paste0(PMDB_DIR, "Private museum database_v1.xlsx")
## maybe add version number later
