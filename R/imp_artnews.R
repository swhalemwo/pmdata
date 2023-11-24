

gd_artnews_time <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    

    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    ## check whether location is handled properly: yea seems to be unique within collector
    dt_artnews_time[, .N, clctr_name]
    dt_artnews_time[, .N, .(clctr_name, location)]
    dt_artnews_time[grepl("David Bowie", clctr_name)]

    ## look at couples, "and" seems to be pretty good differentiator
    dt_artnews_time[grepl("and", clctr_name), .N, clctr_name] %>% print(n=300)

    
}

PMDATA_LOCS <- gc_pmdata_locs()
gd_artnews_time()


#' generate the artnews collector file
#' here manually it has to be checked manually whether a collector entry in artnews ranking refers to single person
gwd_artnews_clctr <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE,
                              ARTNEWS_DIR_old = PMDATA_LOCS$ARTNEWS_DIR_old,
                              ARTNEWS_COLLECTOR_ENTRIES_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE) {

    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    
    
    dt_artnews_time[, .(clctr_name = funique(clctr_name))] %>%
        .[, .(an_entry_id = paste0("ACE", 1:.N), clctr_name,
              is_couple = fifelse(grepl(" and ", clctr_name), 1, 0))] %>%        
        fwrite(paste0(ARTNEWS_COLLECTOR_ENTRIES_FILE))

}

## ONLY RUN ONCE 
## gwd_artnews_clctr()


gwd_artnews_person <- function(



