#' generates and writes to file Artprice uniqe artist IDs
#'
#' re-running this will regenerate IDs, and thus requires rechecking for duplicates
#' therefore writing is commented out
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
#' @param FILE_AP_ARTIST_ID new file with artist names and IDs
gwd_ap_id <- function(FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR,
                      FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID) {

    name <- N <- ap_id <- NULL
    
    dt_ap_yr_prep <- gd_ap_prep(FILE_AP_ARTIST_YEAR)

    dt_ap_artist_id <- dt_ap_yr_prep[, .N, name][order(-N)][, ap_id := paste0("ap", 1:.N)][, N := NULL]

    ## uncommment this only if you're sure you wanna re-run ID generation
    ## fwrite(dt_ap_artist_id, FILE_AP_ARTIST_ID)
}

#' clean artprice name: do some generic pre-processing: yeet birth year, lowercase, trim, latin-ascii
#'
#' @param string name string to clean
artprice_clean_name <- function(string) {
    string %>%
        gsub("\\s*\\(.*?\\)", "", .) %>% # yeet birth year in bracktes
        tolower %>%
        trimws %>%
        stri_trans_general("Latin-ASCII")

}
        

#' read the artprice top 500 ranking id file
#'
#' @param FILE_AP_ARTIST_ID file with artist names and IDs
gd_ap_id <- function(FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID) {

    name_cleaned2 <- name <- ap_id <- NULL
    
    fread(FILE_AP_ARTIST_ID) %>%
        ## .[, name_cleaned2 := artprice_clean_name(name)] %>% 
        ## .[, name_cleaned := gsub("\\s*\\(.*?\\)", "", name)] %>%
        ## .[, name_cleaned2 := trimws(tolower(name_cleaned)) %>%
        ##         stri_trans_general("Latin-ASCII")] %>%
        ## .[, .(ap_id, name = name_cleaned2)]
        .[, .(ap_id, name)]
}

#' read the uniqueness checks back
#'
#' @param FILE_AP_UNQCHECK file with uniqueness checks
gd_ap_unqcheck <- function(FILE_AP_UNQCHECK){
    fread(FILE_AP_UNQCHECK)
}

#' basic read in of Artprice top 500 ranking data, used for further processing
#'
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
gd_ap_prep <- function(FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' reads the file Artprice art market top 500 contemporary ranking file, provided by Olav
    #' does some basic cleaning

    year_begin <- Jaarrapport <- iso3 <- iso3c <- Lastname_firstname <- NULL
    Tophammerpriceeuro <- Lotssold <- Rank <- top_price <- Auctionsalesturnovereuro <- NULL

    dt_ap_yr_prep <- read_sav(FILE_AP_ARTIST_YEAR) %>% adt %>%
        .[, year_begin := as.integer(substring(Jaarrapport, 1,4))] %>%
        # recode ? to NA for country
        .[iso3 != "?", iso3c := countrycode(iso3, "iso2c", "iso3c", custom_match = c("PO" = "PL"))] %>% 
        .[, .(name = Lastname_firstname, year_begin, turnover = Auctionsalesturnovereuro,
              top_price = Tophammerpriceeuro, lotssold = Lotssold, rank = Rank, iso3c)] %>%
        .[order(year_begin, -top_price)]

    return(dt_ap_yr_prep)
}




#' generate-and-write OR tests whether all suspiciously close artprice names have been checked
#'
#' is run as part of package compilation (to not slow down imports)
#' fwrite call is uncommented, is to be used only for manual updates
#'
#' @param FILE_AP_UNQCHECK file with artprice IDs to check
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
#' @param FILE_AP_ARTIST_ID with artist names and IDs
t_gwd_ap_unqcheck <- function(
                              FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR,
                              FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID,
                              FILE_AP_UNQCHECK = PMDATA_LOCS$FILE_AP_UNQCHECK) {
    
    name <- ap_id <- ap_id2 <- id1 <- id2 <- cprn <- year_begin <- N <- cprn_fmt <- NULL
    name.x <- name.y <- name_cleaned <- max_unq <- cnt_unq <- Var1 <- name_pob <- NULL
    iso3c <- turnover <- name1 <- name2 <- NULL

    
    
    ## dt_ap_id1 <- gd_ap_id(FILE_AP_ARTIST_ID)
    ## dt_ap_id2 <- dt_ap_id1[, .(name, ap_id2 = ap_id)]
    
    ## dt_dist <- stringdist_inner_join(dt_ap_id1, dt_ap_id2, max_dist = 0.2,
    ##                                  by = "name", method = "jw", distance_col = "dist") %>% adt

    dt_ap_id <- gd_ap_id(FILE_AP_ARTIST_ID)[, name := artprice_clean_name(name)]

    dt_ap_name_cpnts <- dt_ap_id %>%
        .[, .(name_cpnt = trimws(unlist(tstrsplit(name, " ")))), ap_id]
    
        ## .[, name_cleaned := gsub("\\s*\\(.*?\\)", "", name)] %>%
        ## .[, .(name_cpnt = unlist(tstrsplit(trimws(tolower(name_cleaned)), " ")) %>%
        ##           stri_trans_general("Latin-ASCII")), ap_id] 
    
    ## get all unique name component combinations: possible (pob) names
    dt_ap_name_cbns <- dt_ap_name_cpnts %>% # head(500) %>%
        .[, .(name_pob = do.call("expand.grid", c(list(map(1:.SD[,.N], ~.SD[, name_cpnt])),
                                                  stringsAsFactors = F)) %>% 
                  ## apply(1, paste0, collapse = ' ')), ap_id] # works, but then assessing only unique elements iffy
                  cbind(max_unq = max(apply(., 1, uniqueN))) %>%
                  cbind(cnt_unq = apply(., 1, uniqueN) -1) %>% # have to subtract 1 due to max_unq added already
                  subset(max_unq == cnt_unq, select = Var1:get(paste0("Var", .SD[,.N]))) %>%
                  apply(1, paste0, collapse = ' ')), ap_id]

    dt_ap_name_cbns2 <- dt_ap_name_cbns[, .(ap_id2 = ap_id, name_pob)]

    ## print(head(dt_ap_name_cbns))
    dt_dist <- stringdist_inner_join(dt_ap_name_cbns, dt_ap_name_cbns2, max_dist = 0.1, by = "name_pob",
                                     method = "jw", distance_col = "dist") %>% adt

    
    ## get comparisons to check: only where id1 > id2
    dt_tocheck <- dt_dist[dist >= 0 & dist < 0.1] %>%
        .[, `:=`(id1 = as.integer(gsub("ap", "",ap_id)), id2 = as.integer(gsub("ap", "",ap_id2)))] %>%
        .[id1 > id2] %>% .[, `:=`(id1 = NULL, id2 = NULL)] %>% # filter out one way        
        .[, cprn := paste0(ap_id, '-', ap_id2)] %>% # create comparison object
        .[, .SD[1], cprn]
        

    ## merge to year: if both people of suspicious comparison appear in same year, they can't be identical
    dt_ap_prep <- gd_ap_prep(FILE_AP_ARTIST_YEAR)[, .(name, year_begin, iso3c, turnover)] %>%
        .[, name := artprice_clean_name(name)]

    
    dt_ap_prep2 <- merge(dt_ap_prep, dt_ap_id, by = "name", all.x = T)
    if (dt_ap_prep2[is.na(ap_id), .N] > 0) stop("not all entries have artprice id, re-check/gen them")

    
    ## get the sus comparisons, 
    ## merge comparison to each artist once, see how many times the comparison appears in a year
    ## afterwards, re-add original artists names
    dt_sus_cprns <- merge(dt_ap_prep2, dt_tocheck[, .(ap_id, cprn)], by = "ap_id", all.x = T) %>%
        merge(dt_tocheck[, .(ap_id = ap_id2, cprn2 = cprn)], by = "ap_id", all.x = T, allow.cartesian = T) %>%
        melt(id.vars = "year_begin", measure.vars = c("cprn", "cprn2"), value.name = "cprn") %>% na.omit %>%
        .[, .N, .(cprn, year_begin)] %>%
        .[, .SD[all(N == 1)], cprn] %>% .[, .(cprn = unique(cprn))] %>%
        .[, c("ap_id1", "ap_id2") := tstrsplit(cprn, split = "-")] %>%
        dt_ap_id[, .(name1 = name, ap_id1 = ap_id)][., on = "ap_id1"] %>%
        dt_ap_id[, .(name2 = name, ap_id2 = ap_id)][., on = "ap_id2"] %>%
        .[order(cprn)]


    ## get all the sus artists (involved in sus comparisons)
    dt_sus_long <- dt_tocheck[dt_sus_cprns, on ="cprn"] %>%
        .[, cprn_fmt := sprintf("%s (%s)\n%s (%s)", name1, ap_id, name2, ap_id2)] %>% 
        melt(id.vars = c("cprn", "cprn_fmt"), measure.vars = c("ap_id", "ap_id2"), value.name = "ap_id")
    

    ## ## plot suspicious ones
    ## p_sus <- dt_ap_prep2[dt_sus_long, on = "ap_id"] %>%
    ##     .[, kappa := as.numeric(factor(ap_id)), cprn_fmt] %>%
    ##     .[order(cprn)] %>%
    ##     .[, cprn_fmt := factor(cprn_fmt, levels = unique(cprn_fmt))] %>%         
    ##     ggplot(aes(x=year_begin, y = log10(turnover), color = factor(kappa))) + geom_point() +
    ##     facet_wrap(~cprn_fmt, ncol = 4)

    ## ggsave("/home/johannes/Dropbox/phd/pmdata/inst/figures/p_sus.pdf", height = 18, width = 10)

    ## write sus comparisons to file: this should only be run once and is therefore commented out
    ## default decisions is that none are to be merged
    ## use plotting/googling (the two blocks above) to check whether to merge artists
    ## fwrite(dt_sus_cprns[, .(cprn, name1, name2, decision= "keep_separate")], FILE_AP_UNQCHECK)

    ## a separate append call that was used for 0 dists, should probably never be uncommented
    ## fwrite(dt_sus_cprns[!dt_sus_cprns_ff, on = "cprn"][, .(cprn, name1, name2, decision= "keep_separate")],
    ##        FILE_AP_UNQCHECK, append = T)

    dt_sus_cprns_ff <- gd_ap_unqcheck(FILE_AP_UNQCHECK)

    ## check that all sus comparison that arise have been checked 
    nrow(dt_sus_cprns[!dt_sus_cprns_ff, on = "cprn"]) == 0
}

#' generate basic artprice data.table
#'
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
#' @param FILE_AP_ARTIST_ID with artist names and IDs
#' @param FILE_AP_UNQCHECK file with artprice IDs to check
#' @export
gd_ap_yr <- function(FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR,
                     FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID,
                     FILE_AP_UNQCHECK = PMDATA_LOCS$FILE_AP_UNQCHECK) {

    decision <- cprn <- ap_id <- i.ap_id_new <- ap_id_old <- year_begin <- NULL
    name <- N <- i.name2 <- NULL

    dt_ap_prep <- gd_ap_prep(FILE_AP_ARTIST_YEAR) 
        ## .[, name := artprice_clean_name(name)]

    dt_ap_id <- gd_ap_id(FILE_AP_ARTIST_ID)

    ## have to check order: 
    ## ap840-ap714 has to come before ap714-ap636:
    ## first rename to 840 to 714, then 714 to 636: otherwise I end up with 840 recoded to 714 but not 636
    ## could probably write some expensive test for that
    ## would need some graph processing: each component
    dt_ap_unqcheck <- gd_ap_unqcheck(FILE_AP_UNQCHECK) %>%
        .[decision == "merge"] %>%
        .[, c("ap_id_old", "ap_id_new") := tstrsplit(cprn, "-")]
        ## .[, name2 := artprice_clean_name(name2)]

    ## in case this (merging data to ids) happens more, functionalize
    dt_ap_prep2 <- merge(dt_ap_prep, dt_ap_id, by = "name", all.x = T)
    if (dt_ap_prep2[is.na(ap_id), .N] > 0) stop("not all entries have artprice id, re-check/gen them")

    ## rename old ap_ids and names with update join
    dt_ap_prep3 <- copy(dt_ap_prep2)[dt_ap_unqcheck, 
                                     `:=`(ap_id = i.ap_id_new, name =i.name2), on = .(ap_id = ap_id_old)] %>%
        .[order(ap_id, year_begin)] %>%
        .[, name := artprice_clean_name(name)]

    ## dt_ap_prep3[grepl('chapelle', name, ignore.case = T)]
    dt_ap_prep3[, head(.SD,1), .(ap_id, name)][, N := .N, ap_id][N >1]
    
    if (nrow(dt_ap_prep3[, head(.SD,1), .(ap_id, name)][, N := .N, ap_id][N >1]) > 0) stop("ap names not good")

    return(dt_ap_prep3)
    
}




## gd_ap_yr()[, .(.N, uniqueN(ap_id)), year_begin]

## gd_ap_yr()[, .N, .(name, ap_id, year_begin)][N > 1]

## gd_ap_yr()[, cnt := .N, .(ap_id, year_begin)][cnt > 1]

## gd_ap_prep()[year_begin == 2011 & grepl('lele', name, ignore.case = T)]

## gd_ap_prep()[grepl("lele", name, ignore.case=T)]
## gd_ap_prep()[grepl("liu wei 1972", name, ignore.case=T)]


check_ap_ls_cprn <- function(dt_sus_cbn, dt_sus_cprns, cprn_to_check, FILE_DB_LOTSEARCH) {
    ## browser()
    
    dt_sus_cprns[cprn == cprn_to_check] %>%
        melt(id.vars = "cprn", measure.vars = c("name_ls", "name_ap")) %>% print.data.table


    px <- dt_sus_cbn[cprn == cprn_to_check] %>%
        ggplot(aes(x=year_begin, y=turnover, color = factor(src))) +
        geom_point() + geom_line()
    print(px)

    
    db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)
    
    decision <- readline("merge (m) or keep separate (else): ")

    if (decision == "m") {
        readline("confirm?")
        dbExecute(db_ls, "UPDATE sus_cprns_ap_ls SET decision = 'merge' WHERE cprn = ?",
                  params = list(cprn_to_check))
    } else if (decision == "k") {
        print("no change")
    }

    if (decision %in% c("k", "m")) {
        ## update checked indicator
        dbExecute(db_ls, "UPDATE sus_cprns_ap_ls SET checked = 1 where cprn = ?", params = list(cprn_to_check))
    }
    
}

gd_duckdb_sim_ap_ls <- function(dt_ap_cpnts, dt_ls_cpnts) {

   con <- dbConnect(duckdb())
    dbWriteTable(con, "dt_ap_cpnts", dt_ap_cpnts, overwrite = T)
    dbWriteTable(con, "dt_ls_cpnts", dt_ls_cpnts, overwrite = T)

    
    query <- glue("
       select a.ap_id, b.ls_id, a.pob_name as pob_name_ap, b.pob_name as pob_name_ls,
       jaro_winkler_similarity(a.pob_name, b.pob_name) as dist
       from dt_ap_cpnts as a CROSS JOIN dt_ls_cpnts as b
       where dist > 0.9")

    ## dbGetQuery(con, "SELECT current_setting('memory_limit') AS threads;")
    ## dbGetQuery(con, "SELECT * from duckdb_settings();") %>% adt %>% print.data.table(n = 800)

    ## takes around 1 minute %>% 26m per secs, not bad
    ## maybe not quite clickhouse level, but basically no memory
    ##  dt_dist <- dbGetQuery(con, query) %>% adt
    ## fwrite(dt_dist, "/home/johannes/Dropbox/phd/pmdata/data_sources/artprice/match_artprice_lotsearch.csv")
    
        dt_dist <- dbGetQuery(con, query) %>% adt
        return(dt_dist)
}


## gd_duckdb_sim_ap_ls <- memoise(gd_duckdb_sim_ap_ls(




gwd_ap_ls_match <- function(FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID,
                              FILE_DB_LOTSEARCH = PMDATA_LOCS$FILE_DB_LOTSEARCH) {

    dt_ap_id <- gd_ap_id(FILE_AP_ARTIST_ID)[, name := artprice_clean_name(name)]

    dt_ls_id <- gd_ls_id(FILE_DB_LOTSEARCH)

    ## not perfect: permutations doesn't work well with repeated elements, but this should concern only super few people
    ## strsplit("fischli & weiss peter & david", " ")[[1]] %>%
    ##     ## permutations(n=uniqueN(.), r = uniqueN(.), v = .)
    ##     permutations(n=len(.), r = len(.), v = .)

    dt_ap_cpnts <- dt_ap_id %>% 
        .[, .(pob_name  = strsplit(name, " ")[[1]] %>%
                  permutations(n=uniqueN(.), r= uniqueN(.), v = .) %>%
                  apply(1, paste0, collapse = " ")), ap_id]

    dt_ls_cpnts <- dt_ls_id %>% head(1000) %>% 
        .[, .(pob_name  = strsplit(fullname_clean, " ")[[1]] %>%
                  permutations(n=uniqueN(.), r= uniqueN(.), v = .) %>%
                  apply(1, paste0, collapse = " ")), ls_id]

    dt_dist <- gd_duckdb_sim_ap_ls(dt_ap_cpnts, dt_ls_cpnts)
    
        
    return(dt_dist)

}

#' generate disk cache used for time-intensive tasks (so far only tests)
#'
#' @param DIR_MEMOISE filepath of memoise cache
#' @return memoise cache
#' @export
gc_cache_pmdata <- function(DIR_MEMOISE = gc_pmdata_locs()$DIR_MEMOISE) {
    cm <- memoise::cache_filesystem(path = DIR_MEMOISE)
    return(cm)
}


## #' see which ap and ls are similar (memoised)
## #'
## #' @param gwd_ap_ls_match function to memoise
## #' @return dt_dist
## #' @export

## gc_pmdata_locs <- function() {
##     list(DIR_MEMOISE = "/home/johannes/tec/memoise_cache/")
## }

## gwd_ap_ls_match <- memoise(f = gwd_ap_ls_match, cache = gc_cache_pmdata()) 

## t_gwd_ap_ls_match2 <- function() {
    
##     dt_dist <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artprice/match_artprice_lotsearch.csv")

##     ## can check: is the input the same: if it is, I don't need to redo similarity calculations

##     ## around 500 match completely
##     dt_matched <- dt_dist[dist == 1, .(ap_id, ls_id)] %>% unique

##     ## look at close matches: yeet those that are matched already
##     dt_sus_cprns <- dt_dist[dist > 0.93 & dist < 1, .(dist = min(dist)), .(ap_id, ls_id)] %>%
##         .[!dt_matched, on = "ap_id"] %>% ## yeet the ap/ls ids that are matched already
##         .[!dt_matched, on = "ls_id"] %>% 
##         dt_ap_id[, .(ap_id, name_ap = name)][., on = "ap_id"] %>% # add ap/ls names
##         dt_ls_id[, .(ls_id, name_ls = fullname_clean)][., on = "ls_id"] %>%
##         .[, cprn := paste0(ls_id, "-", ap_id)]

##     db_ls <- dbConnect(SQLite(), FILE_DB_LOTSEARCH)

##     ## use SQLite database to keep track of checks: allows easier field update
##     ## initial write, now 
##     ## dbWriteTable(db_ls, "sus_cprns_ap_ls",
##     ##              dt_sus_cprns[, .(ls_id, ap_id, cprn, name_ls, name_ap, dist, decision = "keep_separate",checked = 0, recheck = 0)],
##     ##              overwrite = T)



##     ## look at actual data to see if comparison makes sense
##     dt_ap_yr <- gd_ap_yr() # artprice
##     dt_ap_yr_sus <- dt_ap_yr[dt_sus_cprns[, .(ap_id = unique(ap_id))], on = "ap_id", nomatch = NULL]

##     dt_ls_aucres <- merge(gd_ls_aucres(), dt_ls_id, by = "url") %>% # lotsearch
##         .[, turnover := count*price]
##     dt_ls_yr_sus <- dt_ls_aucres[dt_sus_cprns[, .(ls_id = unique(ls_id))], on = "ls_id", nomatch = NULL]
        
##     dt_ls_aucres[ls_id == "lsid2494"]


##     ## first add ap/ls data to comparison separately
##     dt_sus_ap_added <- merge(dt_ap_yr_sus, dt_sus_cprns[, .(ap_id, cprn)], by = "ap_id", allow.cartesian = T) %>%
##         .[, .(id = ap_id, cprn, name = name, year_begin, turnover, src = "ap")]

##     dt_sus_ls_added <- merge(dt_ls_yr_sus, dt_sus_cprns[, .(ls_id, cprn)], by = "ls_id", allow.cartesian = T) %>%
##         .[, .(id = ls_id, cprn, name = fullname_clean, year_begin = year, turnover, src = 'ls')]
    
##     ## then combine for comparison
##     dt_sus_cbn <- rbind(dt_sus_ap_added, dt_sus_ls_added) %>% .[year_begin >= 2006]

    
    
    
##     ## db_ls <- dbConnect(SQLite(), PMDATA_LOCS$FILE_DB_LOTSEARCH)
##     dt_sus_cprns_to_check <- dbGetQuery(db_ls, "select * from sus_cprns_ap_ls where checked = 0") %>% adt
##     ## check_ap_ls_cprn(dt_sus_cbn, dt_sus_cprns, "lsid636-ap639", FILE_DB_LOTSEARCH)

##     map(dt_sus_cprns_to_check[, cprn], ~check_ap_ls_cprn(dt_sus_cbn, dt_sus_cprns, .x, FILE_DB_LOTSEARCH))
    
## }

