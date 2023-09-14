## * import PMDB



#' finds a variable in a data.frame from candidates
#'
#' with quickly changing excel sheets, column/variable names can change as well. vrbl_fndr gets the
#' one variable that is present in the data from a vector of candidates.
#' if multiple or none variable is found, an error is thrown
#' @param df data.frame 
#' @param penl_vrbls vector of potential variables
#' @return the variable that is both in penl_vrbls and df
#' @export
vrbl_fndr <- function(df, penl_vrbls) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (len(vrbl) == 0 ) {
        stop("no variable found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else {
        vrbl
    }
}



## #' generates the most basic data.frame of the google sheet
## #' (only handful of columns standardized)
## #'
## #' @param PMBD_FILE path to CSV download from google sheets
## #' @param only_pms whether to include only private museums that are currently open
## #' @export
## gendtb_pmdb_excl <- function(PMDB_FILE = DATA_LOCS$PMDB_FILE, only_pms=T) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
##     dt_pmdb_excl <- fread( PMDB_FILE)[2:.N]# skip second column
##     ## dt_pmdb_excl[, map(.SD, class)] %>% melt(id.vars = "ID") %>% .[value != "character"] %>% .[, .N, value]
##     ## dtx <- read.xlsx2(PMDB_FILE, sheetName = "Blad1") %>% adt() %>% .[2:.N]
##     ## dtx[653:.N] %>% melt(id.vars = "ID") %>% .[value != ""]
##     ## some additional basic aux columns, no transformation
##     basic_cols <- list(
##         country = c("Country where museum is located", "Museum_country"),
##         name = c("Museum_name", "Name of museum"),
##         year_opened_str =  c("Opening year", "Museum_opening year"),
##         year_closed_str = c("Closing year", "Museum_closing year"),
##         museum_status = c("Museum status", "Museum_status"))
##     ## throw error if any of the aux columns are already there
##     if (len(setdiff(names(basic_cols), names(dt_pmdb_excl))) != len(names(basic_cols))) {
##         stop("new cols already there")}
##     ## select basic_cols subset of columns, rename it
##     dt_pmdb_excl2 <- dt_pmdb_excl[, .SD, .SDcols = unlist(map(basic_cols, ~vrbl_fndr(dt_pmdb_excl, .x)))] %>% 
##         setnames(new = names(basic_cols))%>% # update names 
##         cbind(dt_pmdb_excl) %>% # combine with old data
##         .[, `:=`(ID = as.integer(ID), # edit ID columns 
##                  iso3c = countrycode(country, "country.name", "iso3c"), # add iso3c column
##                  year_opened = as.integer(year_opened_str),
##                  year_closed = as.integer(year_closed_str))]

##     ## check that all countries with country string convert to country code
##     if (dt_pmdb_excl2[is.na(iso3c) & country != "", .N] > 0) {stop("not all countries are matched")}
    
    

##     ## test_file("~/Dropbox/phd/pmdata/data_sources/pmdb/tests_gendt_pmdb_excl.R")

##     ## test_that("all PMs have ID", {expect_true(dtx[is.na(ID), .N] == 0)})
##     ## test_that("all_PMs have ID", {expect_true(dtx[, none(ID, is.na)])})
##     ## if (dt_pmdb_excl2[is.na(ID), .N > 0]) {stop("not all PMs have ID")}
##     ## if (dt_pmdb_excl2[, uniqueN(ID) == .N]) {stop("not all IDs are unique")}
    
##     return(dt_pmdb_excl2)
        
## }





#' generates basic data.frame of the google sheet (using collapse)
#'
#' only a handful of columns are standardized here (countrycode, year_opened/year_closed, ID)
#' @param PMBD_FILE path to CSV download from google sheets
#' @param only_pms whether to include only private museums that are currently open
#' @export
gd_pmdb_excl <- function(PMDB_FILE = PMDATA_LOCS$PMDB_FILE, only_pms) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}

    dt_pmdb_excl <- fread(PMDB_FILE) %>% adt %>% .[2:.N] # skip second column
    ## dt_pmdb_excl[, map(.SD, class)] %>% melt(id.vars = "ID") %>% .[value != "character"] %>% .[, .N, value]
    ## dtx <- read.xlsx2(PMDB_FILE, sheetName = "Blad1") %>% adt() %>% .[2:.N]
    ## dtx[653:.N] %>% melt(id.vars = "ID") %>% .[value != ""]
    ## some additional basic aux columns, no transformation

    basic_cols <- list(
        country = c("Country where museum is located", "Museum_country"),
        name = c("Museum_name", "Name of museum"),
        year_opened_str =  c("Opening year", "Museum_opening year"),
        year_closed_str = c("Closing year", "Museum_closing year"),
        museum_status = c("Museum status", "Museum_status")) %>%
        map(., ~vrbl_fndr(dt_pmdb_excl, .x)) %>% unlist %>%
        setNames(names(.), .) # change name and value for collapse

    if (any(basic_cols %in% names(dt_pmdb_excl))) {stop("new cols already there")}

    dt_pmdb_excl2 <- frename(dt_pmdb_excl, basic_cols, cols = names(basic_cols)) %>%
        ftransform(ID = as.integer(ID),
            iso3c = countrycode(country, "country.name", "iso3c"),
            year_opened = as.integer(year_opened_str),
            year_closed = as.integer(year_closed_str))

    ## throw warning if some PMs don't have ID
    if (any(is.na(dt_pmdb_excl2$ID))) {
        warning("some IDs are NA")
        dt_pmdb_excl2 <- dt_pmdb_excl2[!is.na(ID)]
    }

    ## throw error if any duplicated IDs
    if (any_duplicated(dt_pmdb_excl2$ID)) {
        print(dt_pmdb_excl2[fduplicated(ID)])
        stop("duplicated IDs")
    }
    

    if (fsubset(dt_pmdb_excl2, is.na(iso3c) & country != "") %>% fnrow > 0) {stop("not all countries are matched")}

    if (only_pms) {
        return(sbt(dt_pmdb_excl2, museum_status == "private museum"))
    } else if (!only_pms) {
        return(dt_pmdb_excl2)
    }

}

#' generate a list/config of how columns produced in gd_pmdb_excl are to renamed
#'
#' keys are final variable names (custom abbrevations), values are vectors of columns of which
#' the only existing one is later converted into its respective key.
#' to be used in gd_pmdb in combination with vrbl_fndr,
#' but now moved into separate function to check which columns are left to standardize;
#' and to explore where there is value in additional standardization
#' @export
gc_rename_list <- function() {
    rename_list <- list(
        insta_flwrs       = c("IG_followers_LL"),
        insta_posts       = c("IG_posts_LL"),
        insta_bluetick    = c("IG_blue tick_LL"),
        insta_handle      = c("IG_handle_LL"),
        fb_flwrs          = c("Number of Facebook followers"),
        fb_likes          = c("Number of Facebook likes"),
        twitter_flwrs     = c("Number of Twitter followers"),
        youtube_flwrs     = c("Number of Youtube followers"),
        trpadvsr_rating   = c("Tripadvisor rating"),
        trpadvsr_nbrrvws  = c("Number of tripadvisor reviews"),
        google_nbrrvws    = c("Number of google reviews"),
        google_rating     = c("Google rating"),
        nationality       = c("Nationality_founder_LL"),
        gender            = c("Gender founder", "Founder_gender"),
        industry          = c("Industry of founder or family of founder_ ISIC", "Founder_industry_ ISIC"),
        birthyear         = c("Birthyear standardized", "Founder_birthyear standardized"),
        deathyear         = c("Deathyear standardized", "Founder_deathyear standardized"),
        ticket_price      = c("Ticket price standardized", "Museum_ticket price standardized"),
        opng_time         = c("Opening times standardized", "Museum_opening times standardized"),
        nbr_visitrs       = c("Number of visitors standardized", "Museum_number of visitors standardized"),
        staff_size        = c("Staff size standardized", "Museum_staff size standardized"),
        clctn_size        = c("Collection_size_standardized", "Size of collection standardized"),
        clctn_gnr_fcs     = c("Collection_genre_focus_dummy"),
        clctn_med_fcs     = c("Collection_medium focus"),
        clctn_med_fcs_nms = c("Collection_medium_non maintream list"),
        clctn_cry_fcs     = c("Collection_country focus"),
        clctn_reg_fcs     = c("Collection_region focus"),
        clctn_modctmp     = c("Collection_genre_modern_contemporary"),
        temp_exhibs       = c("Exhibitions_temporary dummy", "Temporary exhibitions",
                            "Collection_exhibitions_temporary dummy"),
        founder_name      = c("Founder_name", "Founder name"),
        buildgtype        = c("Building type standardized"),
        cafe_restrnt      = c("CafeRestaurant standardized"),
        webshop           = c("Webshop_standardized"),
        museumshop        = c("Museumshop / bookshop_standardized"),
        an_fyear          = c("Founder_artnews_first year"),
        an_lyear          = c("Founder_artnews_last year"),
        an_nyears         = c("Founder_artnews_number of years"),
        founder_gvrnc     = c("Founder_governance role", "Museum_founder_governance role_standardized"),
        slfidfcn          = c("Museum_institutional self-identification"),
        origin            = c("Origin"),
        founder_wealth    = c("Founder_wealth"),
        founder_weal_ustd = c("Wealth of founder or family of founder"),
        muem_fndr_name    = c("Museum_name of founder"),
        architect         = c("Building_architect_standardized"),
        llid              = c("ID from Larry's List"),
        realism           = c("Collection_genre_realism"),
        gvtsupport        = c("Museum_government support standardized"),
        donorprogram      = c("Private donor program_standardized"),
        endowment         = c("Endowment"),
        sponsorship       = c("Sponsorship"),
        rentalpossblt     = c("Building rental possibility_standardized"),
        act_tours         = c("Museum_actvts tours"),
        act_publications  = c("Museum_actvts publications"),
        act_support       = c("Museum_actvts support"),
        act_online        = c("Museum_actvts online"),
        act_events        = c("Museum_actvts performance_events"),
        act_education     = c("Museum_actvts education"),
        act_talks         = c("Museum_actvts talks_conferences"),
        act_research      = c("Museum_actvts research"),
        act_hedonic       = c("Museum_actvts hedonic"),
        act_volunteer     = c("Museum_actvts volunteer"),
        act_award         = c("Museum_actvts award"),
        act_preservation  = c("Museum_actvts preservation"),
        act_other         = c("Museum_actvts other"),
        reducedtickets    = c("Museum_reduced ticket groups"),
        multiplelocs      = c("Museum_multiple locations"),
        city              = c("Museum_city"),
        cooperation       = c("Museum_cooperation with other musea_standardized"),
        mission           = c("Museum_mission"),
        staff_diversity   = c("Museum_staff diversity"),
        foundation        = c("Foundation"),
        avbl_exhibhist    = c("Exhibition history available", "Collection_exhibition history available"),
        avbl_clctnhldngs  = c("Collection_holdings available"),
        avbl_legalstruct  = c("Museum_legal structure"),
        avbl_gvrncstruct  = c("Museum_governance structure"),
        avbl_floorsize    = c("Floor size"),
        avbl_exhibsize    = c("Exhibition size"),
        website           = c("Museum_website")
                              
    )

    return(rename_list)
}



#' generates more standardized data.frame of google sheets
#' 
#' almost all columns are standardized here
#' @param dt_pmdb_excl: data.frame of PMs, generated by gd_pmdb_excl
#' @export
gd_pmdb <- function(dt_pmdb_excl, verbose = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    rename_list <- gc_rename_list()

    rename_list2 <- map(rename_list, ~vrbl_fndr(dt_pmdb_excl, .x)) %>% unlist %>%
        setNames(names(.), .) #  change name and value for collapse 
            
    if (any(is.na(names(rename_list2)))) {stop("NAs in original variables")}
    if (len(intersect(names(dt_pmdb_excl), rename_list2)) > 0) {stop("overlapping variable names")}

    ## rename columns 
    dt_pmdb_rnmd <- frename(dt_pmdb_excl, rename_list2, cols = names(rename_list2))


    ## convert a bunch of columns string columns to binary (stil char) to measure presence of something
    ## actual values in those columns are too limited to cover entire DB, hopefully presence is still meaningful
    ## could be indicative of size (smaller values are missing) or transparency
    chartobin_vrbls <- .c(
        cooperation, multiplelocs, reducedtickets, staff_diversity, avbl_clctnhldngs,
        avbl_legalstruct, avbl_gvrncstruct, avbl_floorsize, avbl_exhibsize, avbl_exhibhist,
        founder_gvrnc, insta_bluetick)
                        
    ## do the actual conversion: convert zeroes and empty strings to empty string
    dt_pmdb_chartobinvrbls_cvrtd <- tfmv(dt_pmdb_rnmd, vars = chartobin_vrbls,
                                         FUN = \(x) fifelse(x == "0", "0", fifelse(x == "", "", "1")))

    
    

    ## convert a bunch of columns to int: stuff that is numeric, also add the chartobin_vrbls
    int_vrbls <- .c(
        clctn_size, an_fyear, an_lyear, birthyear, deathyear, an_nyears, muem_fndr_name,
        realism, cafe_restrnt, webshop, museumshop, llid, founder_wealth, gvtsupport,
        donorprogram, endowment, sponsorship, rentalpossblt, insta_flwrs, insta_posts,
        insta_bluetick, fb_flwrs, fb_likes, twitter_flwrs, youtube_flwrs, trpadvsr_nbrrvws,
        google_nbrrvws, clctn_gnr_fcs, temp_exhibs, act_tours, act_publications, act_support, act_online,
        act_events, act_education, act_talks, act_research, act_hedonic, act_volunteer,
        act_award, act_preservation, act_other, foundation) %>% c(chartobin_vrbls)

    ## convert integer columns to integer
    dt_pmdb_rnmd_intd <- tfmv(dt_pmdb_chartobinvrbls_cvrtd, vars = int_vrbls,
                              FUN = \(x) gsub(",", "", x) %>% as.integer)

    ## convert numeric vars (decimal values) to numeric
    num_vars <- .c(trpadvsr_rating, google_rating)

    dt_pmdb_num_cvrtd <- tfmv(dt_pmdb_rnmd_intd, vars = num_vars,
                              FUN = \(x) gsub(",", ".", x) %>% as.numeric)


    ## compare int and string variables:
    ## chrintcprn: character-int-comparison
    dt_chrintcprn <- dt_pmdb_num_cvrtd[, c("ID", int_vrbls), with = F] %>%
        melt(id.vars = "ID", value.name = "int") %>%
        .[melt(dt_pmdb_rnmd[, c("ID", int_vrbls), with = F], id.vars = "ID", value.name = "chr"),
          on = .(ID, variable)]

    if (fnrow(dt_chrintcprn[is.na(int) & chr != ""]) > 0) {
        if (verbose) {
            print(dt_chrintcprn[is.na(int) & chr != ""], n = 1000)
        } else {
            print(dt_chrintcprn[is.na(int) & chr != ""])
        }
        warning("FIXME: some strings don't convert to ints")
    }
    
    c_vrbls_stdzd_step1 <- .c(ID, name, museum_status, iso3c, year_opened, year_closed)
    
    ## only select standardized columns
    dt_all_stdzd <- slt(dt_pmdb_num_cvrtd, c(c_vrbls_stdzd_step1, names(rename_list)))

    return(dt_all_stdzd)
    
    
}

## gd_pmdb_excl(gc_data_locs()$PMDB_FILE, only_pms = F) %>% # .[, .N, `IG_blue tick_LL`]
##     gd_pmdb(verbose = T)
