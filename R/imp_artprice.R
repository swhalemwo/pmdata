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

#' read the artprice top 500 ranking id file
#'
#' @param FILE_AP_ARTIST_ID file with artist names and IDs
gd_ap_id <- function(FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID) {
    fread(FILE_AP_ARTIST_ID)
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
    name.x <- name.y <- NULL
    
    dt_ap_id1 <- gd_ap_id(FILE_AP_ARTIST_ID)
    dt_ap_id2 <- dt_ap_id1[, .(name, ap_id2 = ap_id)]
    
    dt_dist <- stringdist_inner_join(dt_ap_id1, dt_ap_id2, max_dist = 0.2,
                                     by = "name", method = "jw", distance_col = "dist") %>% adt

    
    ## get comparisons to check: only where id1 > id2
    dt_tocheck <- dt_dist[dist > 0 & dist < 0.1] %>%
        .[, `:=`(id1 = as.integer(gsub("ap", "",ap_id)), id2 = as.integer(gsub("ap", "",ap_id2)))] %>%
        .[id1 > id2] %>% .[, `:=`(id1 = NULL, id2 = NULL)] %>% # filter out one way        
        .[, cprn := paste0(ap_id, '-', ap_id2)] # create comparison object


    ## merge to year: if both people of suspicious comparison appear in same year, they can't be identical
    dt_ap_prep <- gd_ap_prep(FILE_AP_ARTIST_YEAR)

    
    dt_ap_prep2 <- merge(dt_ap_prep, dt_ap_id1, by = "name", all.x = T)
    if (dt_ap_prep2[is.na(ap_id), .N] > 0) stop("not all entries have artprice id, re-check/gen them")

    
    ## get comparisons where in each year there is always only one
    dt_sus_cprns <- merge(dt_ap_prep2, dt_tocheck[, .(ap_id, cprn)], by = "ap_id", all.x = T) %>%
        merge(dt_tocheck[, .(ap_id = ap_id2, cprn2 = cprn)], by = "ap_id", all.x = T, allow.cartesian = T) %>%
        melt(id.vars = "year_begin", measure.vars = c("cprn", "cprn2"), value.name = "cprn") %>% na.omit %>%
        .[, .N, .(cprn, year_begin)] %>%
        .[, .SD[all(N == 1)], cprn] %>% .[, .(cprn = unique(cprn))]

    ## get all the sus artists
    dt_sus_long <- dt_tocheck[dt_sus_cprns, on ="cprn"] %>%
        .[, cprn_fmt := sprintf("%s (%s)\n%s (%s)", name.x, ap_id, name.y, ap_id2)] %>% 
        melt(id.vars = c("cprn", "cprn_fmt"), measure.vars = c("ap_id", "ap_id2"), value.name = "ap_id")
    

    ## ## plot suspicious ones
    ## dt_ap_prep2[dt_sus_long, on = "ap_id"] %>%
    ##     .[, kappa := as.numeric(factor(ap_id)), cprn_fmt] %>% 
    ##     ggplot(aes(x=year_begin, y = log10(turnover), color = factor(kappa))) + geom_point() +
    ##     facet_wrap(~cprn_fmt, ncol = 4)

    ## write sus comparisons to file: this should only be run once and is therefore commented out
    ## default decisions is that none are to be merged
    ## use plotting/googling (the two blocks above) to check whether to merge artists
    ## fwrite(copy(dt_sus_cprns)[, 'decision' := "keep_separate"], FILE_AP_UNQCHECK)

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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    decision <- cprn <- ap_id <- i.ap_id_new <- ap_id_old <- NULL

    dt_ap_prep <- gd_ap_prep(FILE_AP_ARTIST_YEAR)

    dt_ap_id <- gd_ap_id(FILE_AP_ARTIST_ID)

    dt_ap_unqcheck <- gd_ap_unqcheck(FILE_AP_UNQCHECK) %>%
        .[decision == "merge"] %>%
        .[, c("ap_id_old", "ap_id_new") := tstrsplit(cprn, "-")]

    ## in case this (merging data to ids) happens more, functionalize
    dt_ap_prep2 <- merge(dt_ap_prep, dt_ap_id, by = "name", all.x = T)
    if (dt_ap_prep2[is.na(ap_id), .N] > 0) stop("not all entries have artprice id, re-check/gen them")

    ## rename with update join
    dt_ap_prep3 <- copy(dt_ap_prep2)[dt_ap_unqcheck, ap_id := i.ap_id_new, on = .(ap_id = ap_id_old)]

    return(dt_ap_prep3)

    
}
