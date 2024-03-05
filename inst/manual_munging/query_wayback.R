

library(pmdata)
library(countrycode)
library(collapse)
library(purrr)

options(width = 115)



PMDATA_LOCS <- gc_pmdata_locs()

dt_pmdb_prep <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb

dt_pmdb <- dt_pmdb_prep[, .(ID, name, museum_status, iso3c, city, website)] %>%
    .[museum_status %in% c("private museum", "closed")]

## dt_pmdb[, .(website)] %>% print(n=300)

gd_wayback_entries <- function(pm_url) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' retrieve hits of url from wayback machine

    
    ## use the wayback CDX enpoint to get timestamp and digest (hash -> ID)
    wayback_api_url <- sprintf("http://web.archive.org/cdx/search/cdx?url=%s/&fl=timestamp,digest", pm_url)

    con_wayback <- curl(wayback_api_url)
    res_wayback <- readLines(con_wayback) 
    
   
    dt_wayback <- data.table(raw = res_wayback) %>%
        .[, c("timestamp", "digest") := tstrsplit(raw, " ")] %>%
        .[, .(timestamp, digest)]
    
    Sys.sleep(2)

    return(dt_wayback)

}

url1 <- "https://starakfoundation.org/en/kolekcja"
url2 <- "https://starakfoundation.org/"
url1 <- "https://www.p-city.com/front/artSpace/overview"
url2 <- "https://www.p-city.com/"

gd_wayback_entries(url1)

gd_wayback_entries(url2)

gd_wayback_entries(dt_pmdb[2, website])

dtx <- dt_pmdb[2:4, gd_wayback_entries(website), ID]

dtx[, .N, ID]
