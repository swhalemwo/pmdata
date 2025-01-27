library(rvest)
library(RSelenium)
library(purrr)
library(pmdata)
library(jtls)
library(httr)
library(jsonlite)
library(reticulate)
library(DBI)
library(RSQLite)

use_virtualenv("/home/johannes/litanai/")
source_python("~/Dropbox/phd/pmdata/inst/manual_munging/python_funcs.py")


website_url <- "https://www.lotsearch.net"
base_url <- "https://www.lotsearch.net/artist/browse"


gc_driver <- function() {
    "set up the driver"

    drvr <- remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "chrome"
    )

    drvr$open()
    Sys.sleep(2)

    drvr$navigate(base_url)

    Sys.sleep(5)
    ## drvr$navigate("https://google.com")

    ## drvr$quit()


    ## Using the ID selector to find the cookie accept button
    tryCatch({
        cookie_button <- drvr$findElement(using = "css selector", "#acceptCookies")
        cookie_button$clickElement()
        cat("Cookie consent button clicked successfully.\n")
    }, error = function(e) {
        cat("Error finding or clicking the cookie button:", conditionMessage(e), "\n")
    })

    Sys.sleep(2)
    return(drvr)
}


## Extract all links (anchor tags) from the parsed HTML
## all_links <- webpage %>% html_nodes("a") 
## all_links %>% len

gl_links_browse <- function(all_links) {
    #' get all browse links (section links)
    keep(all_links, ~grepl("^/artist/browse", html_attr(.x, "href")))
}

gl_links_browse_subletter <- function(browse_links_all) {
    ## only keep browse links referring to subsections (including All)
    keep(browse_links_all, ~nchar(trimws(html_text(.x))) > 1)
}


gl_links_artist <- function(all_links) {
    ## generate artist link
    keep(all_links, ~grepl("^/artist/[^/]+$", html_attr(.x, "href")))
}

gd_links_artist <- function(l_artist_links, letter, subletter, url) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' convert links to dt

    dt_links_artist <- map(l_artist_links, ~list(url = paste0(website_url, html_attr(.x, 'href')),
                              atname = trimws(html_text(.x)))) %>% rbindlist

    ## check if there are any links, if not, return empty dt and mark url as bad
    if (nrow(dt_links_artist) > 0) {
        dt_links_artist[, `:=`(letter = letter, subletter = subletter)]
        
    } else {
        dt_links_artist <- data.table(url = character(), atname = character(), letter = character(), subletter = character())

        fwrite(data.table(bad_url = gsub(website_url, "", url)), FILE_BAD_URLS, append = T)
    }
        
    return(dt_links_artist)
}

gl_links_all <- function() {
    #' get all links on current website
    
    html <- driver$getPageSource()[[1]]

    webpage <- read_html(html)

    all_links <- webpage %>% html_nodes("a")

    return(all_links)
}

dl_subletter_page <- function(url, letter, subletter) {
    #' navigate to a page and download all the artists links there
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    print(sprintf("letter: %s", letter))
    print(sprintf("subletter: %s", subletter))
    print(sprintf("url: %s", url))

    

    driver$navigate(url)

    ## if the page is still loading, wait 5 seconds
    Sys.sleep(2) # wait for navigate command to be send
    while (driver$executeScript("return document.readyState") != "complete") {
        print("waiting for page to load")
        Sys.sleep(3)
        
    }

    ## driver$navigate("https://www.lotsearch.net/artist/browse/B/B.")

    ## driver$navigate("https://spiegel.de")
    ## xx <- driver$executeScript("return document.readyState")


    l_links_all <- gl_links_all()
    

    dt_artist_links <- gl_links_artist(l_links_all) %>% gd_links_artist(letter, subletter, url)

    fwrite(dt_artist_links, PMDATA_LOCS$FILE_LOTSEARCH_RES, append = T)

}


scrape_letter_site <- function(letter) {
    #' download a letter site by downloading all the subletter sections
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ## check what is already there

    
    dt_res <- if (file.exists(PMDATA_LOCS$FILE_LOTSEARCH_RES)) {
                  fread(PMDATA_LOCS$FILE_LOTSEARCH_RES)
              } else {
                  data.table(url = character(), atname = character(), letter = character(), subletter = character())
              }

    ## get bad urls (no artists, probably error messages)
    l_bad_urls <- fread(FILE_BAD_URLS)[, bad_url]
    
    driver$navigate(paste0(base_url, "/", letter))

    l_links_all <- gl_links_all()
    
    ## check which have already been downloaded
    l_links_subletter <- l_links_all %>% gl_links_browse %>% gl_links_browse_subletter

    l_links_subletter_remaining <- keep(l_links_subletter,
                                        ~(trimws(html_text(.x)) %!in% dt_res[letter == letter, unique(subletter)]
                                            & (html_attr(.x, 'href') %!in% l_bad_urls)))
    
    
    ## download remaining ones
    
    map(l_links_subletter_remaining, ~dl_subletter_page(paste0(website_url, html_attr(.x, 'href')),
                                                        letter, trimws(html_text(.x))))
}


scrape_lotsearch <- function() {
    
    map(LETTERS, scrape_letter_site)
}

dl_artist_fintech <- function(url, proxy) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    # browser()
    
    Sys.sleep(0.5)

    url_fintech <- gsub("/artist/", "/fintech/", url)

    # proxy <- "http://222.252.194.204:8080"
    ## proxy <- "http://3.9.71.167:1080"
    ## proxy <- "http://113.160.132.195:8080"

    ret_obj <- tryCatch({

        response <- GET(url_fintech, config = use_proxy(proxy), timeout(10))

        ## xx <- GET(url_fintech, use_proxy("http://35.76.62.196", 80, auth = "any"))
        
        ## Check if the request was successful
        if (response$status_code == 200) {
            ## Parse the content as JSON and convert to a list
            json_data <- content(response, as = "text")
            ## data_list <- fromJSON(json_data)
            
            ## return(data_list)
        } else {
            ## If the request was unsuccessful, print an error message
            stop(paste("Failed to download data. Status code:", response$status_code))
        }

        dbExecute(DB_LOTSEARCH, "INSERT INTO auction_res (url, auc_json) VALUES (?,?)",
                  params = list(url, json_data))

        ## dbGetQuery(DB_LOTSEARCH2, "SELECT url, auc_json from auction_res") %>% adt %>% .[, auc_json] %>% fromJSON

        ## filename <- sprintf("%s%s.json.gz", DIR_LOTSEARCH_AUCTION_DATA,
        ##                     gsub("https://www.lotsearch.net/fintech/", "", url_fintech))
        
        ## writeLines(json_data, gzfile(filename))

        ret_obj <- list(status = "ok")
        
        
    }, error = function(e) {
        print(conditionMessage(e))

        ret_obj <- list(status = "bad")
    })
    
    return(ret_obj)
    
}

insert_json_res_to_sqlite <- function() {
    #' run this to update the sqlite db: previously each artist was downloaded as json and saved,
    #' but later moved to saving json string in sqlitedb
    #' first checks which artists that have been downloaded to json are not in DB
    #' then inserts them into the DB
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

 

    l_json_files_dldd <- list.files(DIR_LOTSEARCH_AUCTION_DATA) %>%
        gsub(".json.gz", "", .) %>% 
        paste0("https://www.lotsearch.net/artist/", .)
        
    
    l_sqlite_dldd <- dbGetQuery(DB_LOTSEARCH, "SELECT distinct url from auction_res")$url

    
    l_json_to_insert <- setdiff(l_json_files_dldd, l_sqlite_dldd)
    ## intersect(l_json_files_dldd, l_sqlite_dldd)
    
    print(len(l_json_to_insert))

    ## xx %>% gsub("https://www.lotsearch.net/artist/", "", .) %>%
    ##     sprintf("%s%s.json.gz", DIR_LOTSEARCH_AUCTION_DATA, .) %>% gzfile %>% readLines
    

    map(l_json_to_insert, ~dbExecute(
                             DB_LOTSEARCH, "INSERT INTO auction_res (url, auc_json) VALUES (?,?)",
                             params = list(.x,
                                           gsub("https://www.lotsearch.net/artist/", "", .) %>%
                                           sprintf("%s%s.json.gz", DIR_LOTSEARCH_AUCTION_DATA, .) %>%
                                           gzfile %>% readLines)))
}


## dl_artist_fintech("https://www.lotsearch.net/artist/lei-li", "http://222.252.194.204:8080")





refresh_proxies <- function() {
    l_new_proxies <- get_random_proxies(count=20L)
    walk(l_new_proxies, ~dbExecute(DB_LOTSEARCH,
                                   paste0(c("INSERT OR IGNORE INTO proxies ",
                                            "(proxy, good_attempts, bad_attempts) VALUES (?, 0,0)")),
                                   params = list(.x)))
    l_good_proxies <- dbGetQuery(DB_LOTSEARCH,
                                 paste0(c("SELECT proxy from proxies where ",
                                          "good_attempts > 1 OR bad_attempts < 4"))) %>% adt %>%
                      .[, proxy]
    print(len(l_good_proxies))
    return(l_good_proxies)
}


dl_auction_data <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    dt_distres <- fread(FILE_STRINGMATCH) # first get similarity measures
    dt_lsurls <- fread(PMDATA_LOCS$FILE_LOTSEARCH_RES)

    l_artist_dldd_prep1 <- list.files(DIR_LOTSEARCH_AUCTION_DATA) %>%
        gsub(".json.gz", "", .) %>% 
        paste0("https://www.lotsearch.net/artist/", .)

    l_artist_dldd_prep2 <- dbGetQuery(DB_LOTSEARCH, "SELECT distinct url from auction_res")$url
    l_artist_dldd <- c(l_artist_dldd_prep1, l_artist_dldd_prep2) %>% unique

    l_urls_all <- c(dt_distres[order(dist), unique(ls_url)], dt_lsurls[, unique(url)]) %>% unique()
    l_urls_to_dl <- setdiff(l_urls_all, l_artist_dldd)
    
    l_good_proxies <- dbGetQuery(DB_LOTSEARCH,
                                 "SELECT proxy from proxies where good_attempts > 1 OR bad_attempts < 4")$proxy
 
    print(l_good_proxies)
    print(len(l_good_proxies))
    
    for (url in l_urls_to_dl) {
        
        while(T) {
            proxy_to_use <- sample(l_good_proxies, 1)
            print(sprintf("url: %s, proxy :%s", url, proxy_to_use))

            dl_res <- dl_artist_fintech(url, proxy_to_use)
            
            if (dl_res$status == "ok") {
                dbExecute(DB_LOTSEARCH, "INSERT OR IGNORE INTO proxies (proxy, good_attempts) VALUES (?, 1)",
                          params = list(proxy_to_use))
                dbExecute(DB_LOTSEARCH, "UPDATE proxies SET good_attempts = good_attempts + 1 WHERE proxy = ?",
                          params = list(proxy_to_use))

                break
            } else {
                dbExecute(DB_LOTSEARCH, "INSERT OR IGNORE INTO proxies (proxy, bad_attempts) VALUES (?, 1)",
                          params = list(proxy_to_use))
                dbExecute(DB_LOTSEARCH, "UPDATE proxies SET bad_attempts = bad_attempts + 1 WHERE proxy = ?",
                          params = list(proxy_to_use))

                ratio_good <- dbGetQuery(DB_LOTSEARCH,
                                         "select good_attempts, bad_attempts from proxies where proxy = ?",
                                         params = list(proxy_to_use)) %>% adt %>%
                              .[, good_attempts/(good_attempts + bad_attempts)]
                print(sprintf("ratio_good: %s", ratio_good))

                if (ratio_good < 0.5) {# only retire good proxies if they really don't do well
                    l_good_proxies <- keep(l_good_proxies, ~.x != proxy_to_use)
                }
            }

            if (len(l_good_proxies) < 5) { ## not enough proxies: get more
                l_good_proxies <- refresh_proxies()
            }
        }
    }
 
}






## * main

PMDATA_LOCS <- gc_pmdata_locs()
## DIR_LOTSEARCH_AUCTION_DATA <- paste0(PMDATA_LOCS$DIR_LOTSEARCH, "auction_data/")
DIR_LOTSEARCH_AUCTION_DATA <- "/run/media/johannes/data/ownCloud2/pmdata/lotsearch/auction_data/"

FILE_BAD_URLS <- paste0(PMDATA_LOCS$DIR_LOTSEARCH, "bad_urls.csv")

FILE_STRINGMATCH <- paste0(PMDATA_LOCS$DIR_LOTSEARCH, "dists.csv")

DB_LOTSEARCH <- dbConnect(SQLite(), paste0(PMDATA_LOCS$DIR_LOTSEARCH, "db_lotsearch.sqlite"))
## dbExecute(DB_LOTSEARCH, "CREATE TABLE proxies (proxy TEXT PRIMARY KEY, bad_attempts INTEGER
##     DEFAULT 0, good_attempts INTEGER DEFAULT 0)")
## dbExecute(DB_LOTSEARCH, "CREATE TABLE auction_res (url TEXT PRIMARY KEY, auc_json TEXT)")

## insert_json_res_to_sqlite() # only necessary to do once



dl_auction_data()

stop("halt stop")

# driver <- gc_driver()
## driver$quit()

## scrape_lotsearch()
## scrape_letter_site("B")
## driver$navigate("https://www.lotsearch.net/artist/browse/B")


## * exploration


dt_res <- fread(PMDATA_LOCS$FILE_LOTSEARCH_RES)

## dt_res[, .N, .(letter, subletter)] %>% print.data.table(n=100)

## dt_res[, uniqueN(url), letter]

## dt_res[, uniqueN(url)]

## ## how to match

library(stringdist)
library(fuzzyjoin)
library(stringi)


gwd_dists <- function(dt_af_people_match_subset, dt_lotsearch_people_match) {

    dt_match <- stringdist_inner_join(dt_af_people_match_subset, dt_lotsearch_people_match,
                                  by = "full_name", method = "jw",
                                  distance_col = "dist",
                                  max_dist = 0.15) %>% adt

    fwrite(dt_match, FILE_STRINGMATCH, append = T)

    }

dt_af_people_match <- gd_af_people() %>%
    .[, Name := fifelse(Name == "NULL", "", Name)] %>% 
    .[, .(AF_PID = ID, full_name = tolower(trimws(sprintf("%s %s", trimws(Name), trimws(Surname)))) %>% stri_trans_general("Latin-ASCII"))] %>%
    .[, first_char := substring(full_name, 1,1)]


dt_lotsearch_people_match <- dt_res %>% .[, .(atname, url)] %>% funique %>% 
    .[, c("Surname", "Name") := tstrsplit(atname, ", ", fixed = TRUE)] %>%
    .[, .(ls_url= url, full_name = tolower(sprintf("%s %s", trimws(Name), trimws(Surname)) %>%
                                           stri_trans_general("Latin-ASCII")))]




# do all the 
map(dt_af_people_match[, unique(first_char)], ~gwd_dists(dt_af_people_match[first_char == .x], dt_lotsearch_people_match))

# do it again with null people corrected, only for null people
gwd_dists(dt_af_people_match[gd_af_people()[Name == "NULL", .(ID)], on = .(AF_PID = ID)], dt_lotsearch_people_match)


dt_distres <- fread(FILE_STRINGMATCH)


dt_distres[dist == 0, .(.N, nunique_AF_PID = uniqueN(AF_PID), nunique_ls_url = uniqueN(ls_url))]


dt_distres[dist < 0.07 & dist > 0]

dt_distres[dist > 0.05 & dist < 0.07, .(full_name.x, full_name.y, dist)] %>% print.data.table(n=50)

dt_af_people_match[grepl("qing", full_name)]

dt_distres[gd_af_people()[Name == "NULL", .(ID)], on = .(AF_PID = ID), nomatch = NULL][dist < 0.05]



gd_af_people()[grepl("qing", paste0(Surname, Name), ignore.case = T)]

gd_af_people()[is.null(Name) == T]

gd_af_people()[Surname == "Chen Qingqing", Name]

gd_af_people()[Surname == "NULL"] %>% print.data.table(n=300)

t1 = Sys.time()
t2 = Sys.time()
t2-t1


dt_match[order(-dist)] %>% print.data.table(n=30)

dt_af_people[grepl("van der", Name)]
dt_af_people[grepl("van der", Surname)]


stringdist_inner_join

## ** reticulate

library(reticulate)

use_python("/usr/bin/python")
use_virtualenv("/home/johannes/litanai/")
source_python("~/Dropbox/phd/pmdata/inst/manual_munging/python_funcs.py")

l_good_proxies <- get_random_proxies(count=5L)

mod_nbr(5)

## library(devtools)
## install_github("selesnow/getProxy")

## library(ryandexdirect)
## library(getProxy)
 
## getProxy(port = "3128", country = "RU", supportsHttps = TRUE, action = "start")

## myToken <- yadirGetToken()
