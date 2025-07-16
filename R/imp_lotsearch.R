
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

    AF_PID <- N_url <- ls_url <- NULL

    dt_ls_stringmatch <- fread(FILE_LOTSEARCH_STRINGMATCH)

    dt_match_prep <- dt_ls_stringmatch[dist < 0.02] %>%
        .[, .SD[which.min(dist)], AF_PID] %>%
        .[, N_url := .N, ls_url] # get marker of where a url matches multiple people

    ## FIXME: better dealing with cases where one ls url matches multiple AF_PIDs
    ## dt_match_prep %>% .[N_url > 1] %>% .[order(ls_url)]

    return(dt_match_prep[N_url == 1, .(AF_PID, ls_url)])
}

#' generate IDs for lotsearch artists
#'
#' should not be necessary to be called -> requires confirmation
#' @param FILE_DB_LOTSEARCH link to lotsearch sqlite DB
gwd_ls_id <- function(FILE_DB_LOTSEARCH = PMDATA_LOCS$FILE_DB_LOTSEARCH, i_am_sure = F) {
    #' generate ID table
    db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)

    ## dt_ls_id_prep <- dbGetQuery(db_ls, "select distinct url from clustered_prices") %>% adt

    ## dt_ls_id_prep[grepl('elek-canzi', url)]
    if (i_am_sure) {

        ## dt_ls_id_prep1 <- dbGetQuery(db_ls, "select distinct url from auction_res") %>% adt %>%
        ##     .[, ls_id := paste0("lsid", 1:.N)]

        dt_ls_id_prep1 <- fread(PMDATA_LOCS$FILE_LOTSEARCH_RES) %>% .[, .(url, atname)] %>% unique
        
        ## dt_ls_id_prep2[, n_comma := stri_count(atname, fixed = ",")] %>% 
        ##     ## .[, .N, n_comma]
        ##     .[n_comma == 0]

        dt_ls_id_prep2 <- dt_ls_id_prep1[, c("lastname", "firstname") := tstrsplit(atname, ",")] %>%
            .[, .(url, atname, fullname = paste0(firstname, " ", lastname))] %>%
            .[, fullname_clean := fullname %>% tolower %>% trimws %>% stri_trans_general("Latin-ASCII")] %>%
            .[, ls_id := paste0("lsid", 1:.N)]

        ## dt_ls_id_prep2[grepl('-', atname)]
        ## dt_ls_id_prep2[grepl('abesch',url)]
        print(head(dt_ls_id_prep2))

        dbWriteTable(db_ls, "lotsearch_id", dt_ls_id_prep2, overwrite = T)
    }
}

#' read lotsearch ID data.frame
#'
#' @param FILE_DB_LOTSEARCH link to lotsearch sqlite DB
#' @export
gd_ls_id <- function(FILE_DB_LOTSEARCH = PMDATA_LOCS$FILE_DB_LOTSEARCH) {

    db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)

    dt_ls_id <- dbGetQuery(db_ls, "select url, fullname_clean, ls_id from lotsearch_id") %>% adt 
        
    return(dt_ls_id)
}



                           

## dt_af_people <- gd_af_people()

## dt_af_people[grepl('van der', Surname) | grepl('van der', Name)]
## ## Raaf van der            Sman   
## ## Joachim van der           Vlugt 
## ## Eloïse  van der Heyden 
## ## Bea van der Heijden 
## ## hahaha fuck consistency am i right

## dt_ap_id <- gd_ap_id()
## dt_ap_id[grepl('van de', name, ignore.case = T)]

## dt_ap_id[grepl('b\\.', name, ignore.case = T)]

## dt_ap_id[grepl('\\d', name, ignore.case = T)] %>% print.data.table(n=300)



## dt_ap_id[grepl('wook choi', name, ignore.case = T)] 


## gd_ls_ap_matches <- function(

                             
                             
