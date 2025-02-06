#' generates and writes to file Artprice uniqe artist IDs
#'
#' re-running this will regenerate IDs, and thus requires rechecking for duplicates
#' therefore writing is commented out
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
#' @param FILE_AP_ARTIST_ID new file with artist names and IDs
gwd_ap_id <- function(FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR,
                      FILE_AP_ARTIST_ID = PMDATA_LOCS$FILE_AP_ARTIST_ID) {

    
    dt_ap_yr_prep <- gd_ap_prep(FILE_AP_ARTIST_YEARl)

    dt_ap_artist_id <- dt_ap_yr_prep[, .N, name][order(-N)][, ap_id := paste0("ap", 1:.N)][, N := NULL]

    fwrite(dt_ap_artist_id, FILE_AP_ARTIST_ID)
}

#' basic read in of Artprice top 500 ranking data, used for further processing
#'
#' @param FILE_AP_ARTIST_YEAR artist-year ranking file, provided by Olav
gd_ap_prep <- function(FILE_AP_ARTIST_YEAR = PMDATA_LOCS$FILE_AP_ARTIST_YEAR) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' reads the file Artprice art market top 500 contemporary ranking file, provided by Olav
    #' does some basic cleaning

    dt_ap_yr_prep <- read_sav(FILE_AP_ARTIST_YEAR) %>% adt %>%
        .[, year_begin := as.integer(substring(Jaarrapport, 1,4))] %>%
        # recode ? to NA for country
        .[iso3 != "?", iso3c := countrycode(iso3, "iso2c", "iso3c", custom_match = c("PO" = "PL"))] %>% 
        .[, .(name = Lastname_firstname, year_begin, turnover = Auctionsalesturnovereuro,
              top_price = Tophammerpriceeuro, lotssold = Lotssold, rank = Rank, iso3c)] %>%
        .[order(year_begin, -top_price)]

    return(dt_ap_yr_prep)
}


