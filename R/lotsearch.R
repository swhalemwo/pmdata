library(rvest)
library(RSelenium)
library(purrr)


website_url <- "https://www.lotsearch.net"
base_url <- "https://www.lotsearch.net/artist/browse/"


gc_driver <- function() {
    "set up the driver"

    driver <- remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "chrome"
    )

    driver$open()
    Sys.sleep(2)

    driver$navigate(base_url)

    Sys.sleep(5)
    ## driver$navigate("https://google.com")

    ## driver$quit()


    tryCatch({
                                        # Using the ID selector to find the cookie accept button
        cookie_button <- driver$findElement(using = "css selector", "#acceptCookies")
        cookie_button$clickElement()
        cat("Cookie consent button clicked successfully.\n")
    }, error = function(e) {
        cat("Error finding or clicking the cookie button:", conditionMessage(e), "\n")
    })

    Sys.sleep(2)
    return(driver)
}


# Extract all links (anchor tags) from the parsed HTML
## all_links <- webpage %>% html_nodes("a") 
## all_links %>% len

gl_browse_links <- function(all_links) {

    keep(all_links, ~grepl("^/artist/browse", html_attr(.x, "href")))
}

gl_browse_links_subletter <- function(browse_links_all) {
    ## only keep browse links referring to subsections (including All)
    keep(browse_links_all, ~nchar(trimws(html_text(.x))) > 1)
}


gl_artist_links <- function(all_links) {
    ## generate artist link
    keep(all_links, ~grepl("^/artist/[^/]+$", html_attr(.x, "href")))
}

gd_artist_links <- function(l_artist_links, letter, subletter) {

    map(l_artist_links, ~list(url = paste0(website_url, html_attr(.x, 'href')),
                              aname = trimws(html_text(.x)))) %>% rbindlist %>%
        .[, `:=`(letter = letter, subletter = subletter)]
}
           
dl_subletter_page <- function(letter, subletter) {
    driver$navigate(URL)

    html <- driver$getPageSource()[[1]]

    webpage <- read_html(html)

    all_links <- webpage %>% html_nodes("a")

    dt_artist_links <- gl_artist_links(all_links) %>% gd_artist_links

    fwrite(dt_artist_links, PMDATA_LOCS$FILE_LOTSEARCH_RES, append = T)

    


scrape_letter_site <- function(letter) {
    # check what is already there
    dt_res <- fread(PMDATA_LOCS$FILE_LOTSEARCH_RES)

    ## check which are there
    map(l_subletters_remaining, 
    
    

main_scraper <- function() {
    map(LETTERS, scrape_letter_site)

## all_links %>% gl_artist_links %>% gd_artist_links(letter = "B", subletter = "All")


## * main
driver <- gc_driver()
# driver$navigate("https://www.lotsearch.net/artist/browse/B")




