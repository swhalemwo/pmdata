
# Placeholder with simple test
expect_equal(1 + 1, 2, info = "check if tinytest works")

## expect_equal(1+1, 3)

## expect_true(F)

source("~/Dropbox/technical_stuff_general/dotfiles/.Rprofile")

PMDATA_LOCS <- gc_pmdata_locs()
## expect_equal(length(PMDATA_LOCS), 5)

## restrict PMDB to PMs that are relevant to analysis
dt_pmdb_excl <- gd_pmdb_excl(PMDATA_LOCS$PMDB_FILE, only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")]

## define objects 
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = F)

dt_mow_pmdb_match <- pmdata:::gd_mow_pmdb_matchres(PMDATA_LOCS$MOW_PMDB_MATCHRES_FILE)

## check that each PM with status open, closed or NLPM has been checked for match in MOW
expect_true(dt_mow_pmdb_match[dt_pmdb, on = .(PMDB_ID = ID)][, all(!is.na(MOW_ID))],
             info = "check that each PM with status open, closed or NLPM has been checked for match in MOW")

## expect_true(F)



