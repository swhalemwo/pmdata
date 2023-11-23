

gd_artnews_time <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    

    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    ## check whether location is handled properly: yea seems to be unique within collector
    dt_artnews_time[, .N, clctr_name]
    dt_artnews_time[, .N, .(clctr_name, location)]

    ## look at couples, "and" seems to be pretty good differentiator
    dt_artnews_time[grepl("and", clctr_name), .N, clctr_name] %>% print(n=300)

}

gd_artnews_time()
