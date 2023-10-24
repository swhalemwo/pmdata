library(pmdata)

gw_mow_emacs_file <- function()  {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' convert MOW museums to alist to allow easy reading into elisp

    mow_museums_file <- paste0(dirname(gc_pmdata_locs()$MOW_INFO_FILE), "/mow-museums.el")

    dt_mow_info <- fread(gc_pmdata_locs()$MOW_INFO_FILE)

    mow_alist_segs <- dt_mow_info[, .(idx, name, founding_date1, country)] %>%
        ## .[, sprintf('(\"%s\" . \"%s\")', name, idx)]
        .[, sprintf("(\"%s\" . ((id . \"%s\") (info . \"%s-%s\")))", name, idx, country, founding_date1)]
    
    mow_alist_str <- c("(setq mow-museums '(", mow_alist_segs, "))")
    
    fileConn <- file(mow_museums_file)
    writeLines(mow_alist_str, fileConn)
    close(fileConn)
    
}


## gw_mow_emacs_file()

check_mow_pmdb_match <- function(dt_pmdb_matchy, pmdb_ID) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' check an individual museum from PMDB for match with MOW 

    ## sprintf("%s -- %s", mtcars[1, 1:2])
    ## sprintf("%s -- %s", x=2, zazzzz=3)
    ## do.call(sprintf, c(list(fmt = "%s -- %s"), as.list(mtcars[1, 1:2])))

    do.call(sprintf, c(list(fmt = "%s, %s--%s"),
                       as.list(dt_pmdb_matchy[ID==pmdb_ID, .(name, country, year_opened)]))) %>% print


    ## print(sprintf("%s, %s-%s", dt_pmdb_matchy[ID==pmdb_ID, name], )

    ## use mow-pmdb-match (C-c m) to fuzzy-search whether PMDB museum is in MOW
    MOW_ID <- readline("MOW ID: ")

    if (substring(MOW_ID,1,1) != "M") MOW_ID <- "nomatch"

    dt_res <- data.table(PMDB_ID = pmdb_ID, MOW_ID = MOW_ID)
    fwrite(dt_res, MOW_PMDB_MATCH_RES_FILE, append = T)
    

}

## check_mow_pmdb_match(dt_pmdb_matchy, 11)


gw_mow_pmdb_matches <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## library(collapse)
    library(countrycode)

    ## generate PMDB: these have to be checked
    dt_pmdb_matchy <- gd_pmdb_excl(gc_pmdata_locs()$PMDB_FILE, only_pms = F) %>%
        .[, .(ID, name, museum_status, year_opened, country = countrycode(iso3c, "iso3c", "country.name"))] %>%
        .[museum_status %!in% c("no private museum", "duplicate")]


    ## look at how many stuff is already checked
    if (file.exists(MOW_PMDB_MATCH_RES_FILE)) {
        dt_res <- fread(MOW_PMDB_MATCH_RES_FILE)
    } else {
        dt_res <- data.table(PMDB_ID=character(), MOW_ID = character())
    }
 
    pmdb_IDS_to_check <- setdiff(dt_pmdb_matchy$ID, dt_res$PMDB_ID)

    map(pmdb_IDS_to_check, ~check_mow_pmdb_match(dt_pmdb_matchy, .x))
    
}

## here go the results
MOW_PMDB_MATCH_RES_FILE <- paste0(dirname(gc_pmdata_locs()$MOW_INFO_FILE), "/mow_pmdb_match_res.csv")

gw_mow_pmdb_matches()
