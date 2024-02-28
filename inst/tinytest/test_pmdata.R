
# Placeholder with simple test
## expect_equal(1 + 1, 2, info = "check if tinytest works")

## expect_equal(1+1, 3)

## expect_true(F)

library(collapse)
source("~/Dropbox/technical_stuff_general/dotfiles/.Rprofile")
fstd <- c()

library(ggplot2)


## add section for manually debuggging? hopefully works
if (interactive()) {
    library(pmdata)
    library(tinytest)
}
    
PMDATA_LOCS <- gc_pmdata_locs()
## expect_equal(length(PMDATA_LOCS), 5)

## ** -------------- PMDB import ------------
## turn of warnings to not clutter the tinytest output when it throws an error
options(warn=-1)

## restrict PMDB to PMs that are relevant to analysis
dt_pmdb_excl <- gd_pmdb_excl(PMDATA_LOCS$PMDB_FILE, only_pms = F) %>%
    .[museum_status %in% c("private museum", "no longer a private museum", "closed")]

## define objects 
dt_pmdb <- gd_pmdb(dt_pmdb_excl, verbose = F)

options(warn=0)

## ** -------------- PMDB checks ------------


expect_false(dt_pmdb[, .(founder_name, founder_id)] %>% funique %>%
             .[founder_id != ""] %>% # yeet cases without proper founder_id
             .[, any(sapply(.SD, any_duplicated))],
             info = "check that a founder_id refers to a unique name")


expect_true(pmdata:::t_gwd_pmdb_founder_person(
                         dt_pmdb[museum_status %in% c("private museum", "no longer a private museum", "closed")],
                         PMDB_FOUNDER_PERSON_FILE_ORG = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_ORG,
                         PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV),
            info = paste0("check that every founder_id in dt_pmdb has been checked wrt to being part of couple.",
                          "and that founder_person_id is unique"))


## dt_pmdb_founder_person <- gd_pmdb_founder_person(PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID)    

expect_true(pmdata:::t_gwd_ppecprn(PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID,
                                   PMDB_PPECPRN_FILE = PMDATA_LOCS$PMDB_PPECPRN_FILE),
            info = paste0("test that every pair of founder names that have a low string distance ",
                          "have been checked manually."))

expect_true(pmdata:::t_gwd_pmdb_founder_person_wid(
                         PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV,
                         PMDB_PPECPRN_FILE = PMDATA_LOCS$PMDB_PPECPRN_FILE,
                         PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID),
            info = paste0("test that current file version of PMDB founder_ids, founder_person_id, founder_name ",
                          "and pmdb_person_id is still the same as it would be if it were generated again",
                          "if this test fails, you probably have to add entires to the input files"))
                          

expect_true(dt_pmdb[museum_status %in% c("private museum", "closed")] %>%
            .[is.na(lat) | is.na(long) | is.na(address_formatted), .N] == 0,
            info = "check that all PMs have lat/long location")



## expect_equal(pmdata:::t_gwd_ppecprn(dt_pmdb_founder_person, PMDB_PPECPRN_FILE = PMDATA_LOCS$PMDB_PPECPRN_FILE),
##              "j", info = "kappa")

## expect_equal("jj", "kk")

## expect_true("İnan Kıraç" > "Suna Kıraç")
             
             

## **  ------------- MOW CHECKS -------------

dt_mow_pmdb_match <- pmdata:::gd_mow_pmdb_matchres(PMDATA_LOCS$MOW_PMDB_MATCHRES_FILE)



## check that each PM with status open, closed or NLPM has been checked for match in MOW
expect_true(dt_mow_pmdb_match[dt_pmdb, on = .(PMDB_ID = ID)][, all(!is.na(MOW_ID))],
             info = "check that each PM with status open, closed or NLPM has been checked for match in MOW")

## ** -------------- ARTnews checks ---------

## expect_true(F)

## dt_acpe_wid <- gd_artnews_collector_person(
##     ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG,
##     ARTNEWS_APECPRN_FILE = PMDATA_LOCS$ARTNEWS_APECPRN_FILE)


## dt_acpe_wid <- gd_artnews_collector_person(
##     ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID)

## just_some_test()


expect_true(pmdata:::t_gwd_apecprn(PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID,
                                   PMDATA_LOCS$ARTNEWS_APECPRN_FILE),
            ## info = "kappa")
            info = paste0("test that all pairs with similar names have been checked in ARTNEWS_APECPRN_FILE. ",
                          "make sure they are covered, e.g. by adding manually entries to ARTNEWS_APECPRN_FILE",
                          "or re-running the fwrite-call. ",
                          "make sure tho that this doesn't yeet other or even more clctr_names."))

expect_true(pmdata:::t_gwd_artnews_clctr(PMDATA_LOCS$ARTNEWS_TIME_FILE,
                                         PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE),
            info = paste0("test all clctr_names that are present in the ranking have been checked",
                          " in ARTNEWS_COLLECTOR_ENTRIES_FILE. make sure they are covered, e.g. by ",
                          "adding manually entries to ARTNEWS_COLLECTOR_ENTRIES_FILE or re-running ",
                          "the fwrite-call.",
                          "make sure tho that this doesn't yeet other or even more clctr_names."))

expect_true(pmdata:::t_gwd_artnews_collector_person(
                        ARTNEWS_COLLECTOR_ENTRIES_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE,
                        ARTNEWS_COLLECTOR_PERSON_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE,
                        ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG),
            info = paste0("test that all an_collector_person_ids that are follow ",
                          "from ARTNEWS_COLLECTOR_ENTRIES_FILE",
                          "(via couple indicator) have been checked in ARTNEWS_COLLECTOR_ENTRIES_FILE. ",
                          "make sure they are covered, e.g. by ",
                          "adding manually entries to  ARTNEWS_COLLECTOR_PERSON_FILE_ORG or ",
                          "re-running the fwrite-call ",
                          "exporting that to org-table, making changes there, and exporting that again.",
                          "make sure tho that this doesn't yeet other or even more an_collector_person_id s."))
            

expect_true(pmdata:::t_gwd_artnews_collector_person_wid(
                         ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG,
                         ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID,
                         ARTNEWS_APECPRN_FILE = PMDATA_LOCS$ARTNEWS_APECPRN_FILE),
            info = paste0("test that all IDs and names are generated in the way they have been previously ",
                          "when they were written to file"))
   

expect_true(pmdata:::t_pmdb_artnews_match_coverage(
                         dt_pmdb,
                         ARTNEWS_PMDB_MATCHRES_FILE = PMDATA_LOCS$ARTNEWS_PMDB_MATCHRES_FILE,
                         PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID),
            info = "test that all entries in dt_pmdb have been checked for potential matchin PMDB")


## if artnews data gets updated, I have to re-run all nomatch pmdb_founder_id:
## any of them could be matched by new artnews entries


            
## t_gwd_artnews_clctr()
## t_gwd_apecprn(dt_acpe_w_id1, ARTNEWS_APECPRN_FILE)

## ** -------------- GHSL checks ------------

expect_true(pmdata:::test_gd_circle(),
            info = "test that circles are properly generated")

expect_true(pmdata:::test_imp_ghsl(PMDATA_LOCS),
            info = "test that some pop generation is the same as in previous runs")


test_gd_circle_plot <- function() {
    ## plot some circles on a map to see that circles look ok
    
    FIG_DIR <- "/home/johannes/Dropbox/phd/pmdata/inst/figures/"
    
    ## generate some data
    dt_centers <- data.table(city = c("NY", "HH", "BJ"),
                             lat = c(40.7128, 53.5488, 39.9042),
                             lon = c(-74.0060, 9.9872, 116.4074))

    ## generate circles around them
    dt_circles <- dt_centers[, pmdata:::gd_circle(lat, lon, 2e5), city] %>% 
        .[, .(object = as.integer(factor(city)), part = 1, lon, lat)]
    
    ## pre-process for st_multipolygon: list of lists with numeric matrices          
    l_circles <- split(dt_circles, dt_circles$object) %>% purrr::map(~list(as.matrix(.x[, .(lon, lat)])))
    mp_circles <- sf::st_multipolygon(l_circles) %>% sf::st_sfc(crs = 4326)

    ## load world data
    world_map <- map_data("world")
    
    ## hamburg plot
    p1 <- ggplot() +
        geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) + 
        geom_sf(mp_circles, mapping = aes(geometry = geometry), alpha = 0.7) +
        coord_sf(xlim = c(0, 15), ylim = c(50, 60)) +
        labs(title = "200km circle (HH)")

    ## generate other circles
    dt_circles2 <- dt_centers[, pmdata:::gd_circle(lat, lon, 1e6), city] %>% 
        .[, .(object = as.integer(factor(city)), part = 1, lon, lat)]

    ## preprocess them as well
    l_circles2 <- split(dt_circles2, dt_circles2$object) %>% purrr::map(~list(as.matrix(.x[, .(lon, lat)])))
    
    ## multi-polygon: st_multipolygon', list of lists with numeric matrices          
    mp_circles2 <- sf::st_multipolygon(l_circles2) %>% sf::st_sfc(crs = 4326)

    ## generate the plot
    p2 <- ggplot() +
        geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) +
        geom_sf(mp_circles2, mapping = aes(geometry = geometry), alpha = 0.7) +
        labs(title = "1000km circle (NY, HH, BJ)")

    ## generate the plot
    p3 <- ggplot() +
        geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) + 
        geom_sf(mp_circles2, mapping = aes(geometry = geometry), alpha = 0.7) +
        coord_sf(xlim = c(-10, 25), ylim = c(40, 70)) +
        labs(title = "1000km circle (HH)")
    
    library(patchwork)
    ## plot to file

    pdf(paste0(FIG_DIR, "circletest.pdf"), width = 8, height = 12)
    plot((p2 / (p1 + p3)))
    dev.off()

    return(T)

}

expect_true(test_gd_circle_plot(),  info = "generate example circles")
