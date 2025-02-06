
#' import data.table of lotsearch auction results
#'
#' @param FILE_DB_LOTSEARCH sqlite db of lotsearch data
#' @export
gd_ls_aucres <- function(FILE_DB_LOTSEARCH = PMDATA_LOCS$FILE_DB_LOTSEARCH) {

    db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)

    dt_ls_aucres <- dbGetQuery(db_ls, "select url, year, count, price from clustered_prices") %>% adt

    return(dt_ls_aucres)
}

#' import data.table of matches between lotsearch and artfacts
#'
#' @param FILE_LOTSEARCH_STRINGMATCH file with string similarity results
#' @export
gd_af_ls_matches <- function(FILE_LOTSEARCH_STRINGMATCH = PMDATA_LOCS$FILE_LOTSEARCH_STRINGMATCH) {
    ## FIXME: use proper stringmatch with proper SQLite table
    ## db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    dt_ls_stringmatch <- fread(FILE_LOTSEARCH_STRINGMATCH)

    dt_match_prep <- dt_ls_stringmatch[dist < 0.02] %>% .[, .SD[which.min(dist)], AF_PID] %>%
        .[, N_url := .N, ls_url] # get marker of where a url matches multiple people

    ## FIXME: better dealing with cases where one ls url matches multiple AF_PIDs
    ## dt_match_prep %>% .[N_url > 1] %>% .[order(ls_url)]

    return(dt_match_prep[N_url == 1, .(AF_PID, ls_url)])
}
