library(rvest)
library(RSelenium)
library(purrr)


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
## all_links %>% gl_links_artist %>% gd_links_artist(letter = "B", subletter = "All")


if (interactive()) {stop("it's interactive time")}
## * main


PMDATA_LOCS <- gc_pmdata_locs()

driver <- gc_driver()
## driver$quit()

scrape_lotsearch()
## driver$navigate("https://www.lotsearch.net/artist/browse/B")
