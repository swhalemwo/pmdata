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
    if (length(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (length(vrbl) == 0 ) {
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
#' @param PMDB_FILE path to CSV download from google sheets, usually provided by PMDATA_LOCS
#' @param sel selection of museums, one of: "only_open_pm", "any_ever_pm", "only_always_pm", "all"
#' @export
gd_pmdb_excl <- function(PMDB_FILE = PMDATA_LOCS$PMDB_FILE, sel) {
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}

    ID <- country <- year_opened_str <- year_closed_str <- iso3c <- museum_status <- NULL

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

    ## only currently open PMs
    if (sel == "only_open_pm") {
        return(sbt(dt_pmdb_excl2, museum_status == "private museum"))
        ## everything that ever was a PM
    } else if (sel == "any_ever_pm") {
        return(sbt(dt_pmdb_excl2, museum_status %in% c("private museum", "closed", "no longer a private museum")))
        ## only those that always were PMs (open and closed)
    } else if (sel == "only_always_pm") {
        return(sbt(dt_pmdb_excl2, museum_status %in% c("private museum", "closed")))
    } else if (sel == "all") {
        return(dt_pmdb_excl2)
    } else if (sel %!in% c("only_open_pm", "any_ever_pm", "only_always_pm", "all")) {
        stop("sel is not one of only_open_pm, any_ever_pm, only_always_pm, all")
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
        founder_id        = c("Founder_ID"),
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
        lat               = c("Museum_latitude"),
        long              = c("Museum_longitude"),
        address_formatted = c("Museum_formatted_address"),
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
#' @param dt_pmdb_excl data.frame of PMs, generated by gd_pmdb_excl
#' @param verbose verbosity: if TRUE, print 1k lines of values not being properly converted from strings to integers
#' @export
gd_pmdb <- function(dt_pmdb_excl, verbose = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ## globalVariables(names(dt_pmdb_excl))

    ID <- variable <- int <- chr <- NULL

    rename_list <- gc_rename_list()

    rename_list2 <- map(rename_list, ~vrbl_fndr(dt_pmdb_excl, .x)) %>% unlist %>%
        setNames(names(.), .) #  change name and value for collapse 
            
    if (any(is.na(names(rename_list2)))) {stop("NAs in original variables")}
    if (length(intersect(names(dt_pmdb_excl), rename_list2)) > 0) {stop("overlapping variable names")}

    ## rename columns 
    dt_pmdb_rnmd <- frename(dt_pmdb_excl, rename_list2, cols = names(rename_list2))


    ## convert a bunch of columns string columns to binary (stil char) to measure presence of something
    ## actual values in those columns are too limited to cover entire DB, hopefully presence is still meaningful
    ## could be indicative of size (smaller values are missing) or transparency
    chartobin_vrbls <- c(
        "cooperation", "multiplelocs", "reducedtickets", "staff_diversity", "avbl_clctnhldngs",
        "avbl_legalstruct", "avbl_gvrncstruct", "avbl_floorsize", "avbl_exhibsize", "avbl_exhibhist",
        "founder_gvrnc", "insta_bluetick")
                        
    ## do the actual conversion: convert zeroes and empty strings to empty string
    dt_pmdb_chartobinvrbls_cvrtd <- tfmv(dt_pmdb_rnmd, vars = chartobin_vrbls,
                                         FUN = \(x) fifelse(x == "0", "0", fifelse(x == "", "", "1")))

    
    ## convert a bunch of columns to int: stuff that is numeric, also add the chartobin_vrbls
    int_vrbls <- .c(
        "clctn_size", "an_fyear", "an_lyear", "birthyear", "deathyear", "an_nyears", "muem_fndr_name",
        "realism", "cafe_restrnt", "webshop", "museumshop", "llid", "founder_wealth", "gvtsupport",
        "donorprogram", "endowment", "sponsorship", "rentalpossblt", "insta_flwrs", "insta_posts",
        "insta_bluetick", "fb_flwrs", "fb_likes", "twitter_flwrs", "youtube_flwrs", "trpadvsr_nbrrvws",
        "google_nbrrvws", "clctn_gnr_fcs", "temp_exhibs", "act_tours", "act_publications",
        "act_support", "act_online",
        "act_events", "act_education", "act_talks", "act_research", "act_hedonic", "act_volunteer",
        "act_award", "act_preservation", "act_other", "foundation") %>% c(chartobin_vrbls)

    ## convert integer columns to integer
    dt_pmdb_rnmd_intd <- tfmv(dt_pmdb_chartobinvrbls_cvrtd, vars = int_vrbls,
                              FUN = \(x) gsub(",", "", x) %>% as.integer)

    ## convert numeric vars (decimal values) to numeric
    num_vars <- c("trpadvsr_rating", "google_rating", "lat", "long")

    dt_pmdb_num_cvrtd <- tfmv(dt_pmdb_rnmd_intd, vars = num_vars,
                              FUN = \(x) gsub(",", ".", x) %>% as.numeric)


    ## compare int and string variables:
    ## chrintcprn: character-int-comparison
    dt_chrintcprn <- dt_pmdb_num_cvrtd[, c("ID", int_vrbls), with = F] %>%
        melt(id.vars = "ID", value.name = "int") %>%
        .[melt(dt_pmdb_rnmd[, c("ID", int_vrbls), with = F], id.vars = "ID", value.name = "chr"),
          on = .(ID, variable)]

    ## check which columns are not properly handled yet
    if (fnrow(dt_chrintcprn[is.na(int) & chr != ""]) > 0) {
        if (!is.null(verbose)) {
            if (verbose) {
                print.data.table(dt_chrintcprn[is.na(int) & chr != ""], n = 1000)
            } else {
                print(dt_chrintcprn[is.na(int) & chr != ""])
            }
        }
        warning("FIXME: some strings don't convert to ints")
    }
    
    c_vrbls_stdzd_step1 <- c("ID", "name", "museum_status", "iso3c", "year_opened", "year_closed")
    
    ## only select standardized columns
    dt_all_stdzd <- slt(dt_pmdb_num_cvrtd, c(c_vrbls_stdzd_step1, names(rename_list)))

    return(dt_all_stdzd)
    
    
}


## ** generating/testing links of museums to founders

#' generate founder_id and add it to google docs
#' really can't see how this can be run..
md_pmdb_gdocs_add_fid <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ID <- name <- founder_name <- founder_id <- N <- founder_id_num <- i.founder_id <- NULL


    dt_pmdb <- gd_pmdb_excl(sel = "all") %>% gd_pmdb()

    ## create founder ID
    dt_fndr <- copy(dt_pmdb)[order(ID), .(ID, name, founder_name)] %>%
        .[, founder_id := paste0("FID", 1:.N)]

    ## create dt to give repeated founder names the same founder ID
    dt_fndr_renamer <- dt_fndr[, .N, founder_name][N > 1] %>%
        dt_fndr[, .(founder_name, founder_id)][., on = "founder_name"] %>%
        .[, founder_id_num := as.integer(gsub("FID", "", founder_id))] %>%
        .[, .SD[which.min(founder_id_num)], founder_name]

    ## actually assign the first ID to repeated founders
    dt_fndr_id_renamed <- copy(dt_fndr)[dt_fndr_renamer, founder_id := i.founder_id, on = "founder_name"] %>%
        .[order(ID)] %>% .[, .(ID, founder_id, founder_name, name)]

    ## yeet the FIDs in gdocs for all the entries without proper founder name
    ## dt_fndr_id_renamed[founder_id == "FID5"] # FID5 is founder_name = ""

    ## open founder IDs in lowriter to add them to google docs
    ## library(jtls, include.only = "view_xl")
    ## view_xl(dt_fndr_id_renamed)

    ## manually check that museum_IDs and founder names are same, before deleting them
    ## have to use dt_pmdb_excl since I don't want them in the long term
    ## dt_pmdb_excl <- gd_pmdb_excl(only_pms = F)
    ## dt_pmdb_excl[, all(museum_id_check == ID) & all(Founder_name == founder_name_check)]
    ## dt_pmdb_excl[founder_name_check != Founder_name, .(Founder_name, founder_name_check)] %>% adf
    ## looks good.. differences in founder_name are most likely due to string-processing


}

## gd_pmdb_excl(gc_data_locs()$PMDB_FILE, only_pms = F) %>% # .[, .N, `IG_blue tick_LL`]
##     gd_pmdb(verbose = T)

#' test that founder names don't change
#' if founder names have changed, throw an error
#' @param dt_pmdb data.table of PMDB
#' @param PMDB_FOUNDER_PERSON_FILE_CSV FILE with founder-person links
t_pmdb_founder_name_change <- function(dt_pmdb,
                                       PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV) {
    if (as.character(match.call()[1]) %in% fstd){browser()}

    founder_id <- founder_string_pmdb <- founder_name <- founder_string_ff <- NULL

    ## dt_pmdb[, .(founder_id, founder_string_pmdb = founder_name)] # %>% 
        ## .[grepl("learsy", founder_name, ignore.case = T)]

    ## add founder_string_pmdb to PMDB_FOUNDER_PERSON_FILE_CSV to get name check
    
    ## read founder names from file (ff)
    dt_founder_person_ff <- fread(PMDB_FOUNDER_PERSON_FILE_CSV) %>%
        setnames(old = "founder_string_pmdb", new = "founder_string_ff")
    
    ## get founder names in current pmdb
    dt_pmdb_founder_string_prep <- dt_pmdb[, .(founder_id, founder_string_pmdb = founder_name)] %>% funique

    ## merge to compare
    dt_cpr <- merge(dt_founder_person_ff, dt_pmdb_founder_string_prep, by = "founder_id", all.x = T)

    dt_cpr_res <- dt_cpr[founder_string_ff != founder_string_pmdb]
        
    if (fnrow(dt_cpr_res) > 0) {
        print(dt_cpr_res)
        res <- F
    } else {
        res <- T
    }

    
    ## dt_founder_person_ff <- fread(PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV)
    
    ## dt_founder_person2 <- merge(dt_founder_person_ff, dt_pmdb_founder_string_prep, by = "founder_id",
    ##                             all.x = T, sort = F)
    
    ## dt_founder_person2[grepl("learsy", founder_string_pmdb, ignore.case = T)]

    ## dt_founder_person2[founder_name != founder_string_pmdb] %>% print.data.table(n=300)
    ## here writes to pmdb_founder_person2.csv, has since been renamed to pmdb_founder_person.csv
    ## fwrite(dt_founder_person2, "/home/johannes/Dropbox/phd/pmdata/data_sources/pmdb/pmdb_founder_person2.csv")
    return(res)
        

}

## test that founder names don't change
## t_pmdb_founder_name_change(dt_pmdb)

## this is how it looks when names do change: 
## t_pmdb_founder_name_change(copy(dt_pmdb)[grepl("learsy", founder_name, ignore.case = T), founder_name := "jj"])
    

    
    
    


#' generate founder-person links
#' @param dt_pmdb file database generated by gd_pmdb. this is now mostly run in tests, where dt_pmdb exists, so it should be fine...
#' @param PMDB_FOUNDER_PERSON_FILE_ORG the org-file with the org-table, where couples are adjust
#' @param PMDB_FOUNDER_PERSON_FILE_CSV the csv file where PMDB_FOUNDER_PERSON_FILE_ORG is exported to; is also read in here to check that every founder in dt_pmdb is covered
t_gwd_pmdb_founder_person <- function(
                               dt_pmdb,
                               PMDB_FOUNDER_PERSON_FILE_ORG = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_ORG,
                               PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    founder_name <- founder_id <- gender <- founder_person_id <- founder_id_nbr <- NULL

    ## dt_pmdb <- gd_pmdb_excl(only_pms = T) %>% gd_pmdb()

    ## dt_pmdb[grepl("Jens Faurschou", founder_name), .(ID, gender)]

    dt_founder <- dt_pmdb[, .(founder_name, founder_id, gender)] %>% funique %>% .[founder_id != ""]

    ## dt_founder[gender==""]
    ## dt_founder[, .N, gender]
    ## dt_founder[gender %!in% c("F", "M", "")] %>% print(n=100)


    ## duplicate not single rows
    dt_founder_person_prep <- rbind(dt_founder, dt_founder[gender %!in% c("F", "M", "")]) %>%
        .[order(-gender, founder_id)] %>%
        .[, founder_id_nbr := as.integer(gsub("FID", "", founder_id))] %>% 
        .[, founder_person_id := paste0("FID", founder_id_nbr, letters[1:.N]), founder_id] %>%
        .[, .(founder_id, founder_person_id, founder_name)]

    ## read back from file (ff), check that 
    dt_founder_person_ff <- fread(PMDB_FOUNDER_PERSON_FILE_CSV)

    ## checkt that all founder_ids in PMDB are covered
    
    tres_cvrg <- F
    dt_t_cvrg <- dt_founder_person_prep[!dt_founder_person_ff, on = "founder_id"]

    if (fnrow(dt_t_cvrg) > 0) {
        print(dt_t_cvrg)
        tres_cvrg <- F
    } else {
        tres_cvrg <- T
    }

    ## check that founder_person_id is unique

    dt_t_dupl <- dt_founder_person_ff[fduplicated(founder_person_id)]
    if (fnrow(dt_t_dupl) > 0) {
        print(dt_t_dupl)
        tres_dupl <- F
    } else {
        tres_dupl <- T
    }

    
    ## combine tests
    tres <- all(tres_cvrg, tres_dupl)
    
    
    return(tres)
    ## generate org file with org table
    ## this section is uncommented: if tests fail modify the org-table by adding/changing stuff

    ## first generate a long string with all the table cells
    ## str_founder_person_orgtable_cells <- dt_founder_person_prep[
    ##   , sprintf("| %s | %s | %s |", founder_id, founder_person_id, founder_name)] %>%
    ##     paste0(collapse = "\n")

    ## cat(str_founder_person_orgtable_cells)

    ## writeLines(
    ##     paste0("| founder_id | founder_person_id | founder_name |\n|-\n", str_founder_person_orgtable_cells),
    ##     PMDB_FOUNDER_PERSON_FILE_ORG)

    
}
    

## t_gwd_pmdb_founder_person(
##     ## dt_pmdb,
##     ## gd_pmdb_excl(only_pms = F) %>% gd_pmdb,
##     PMDB_FOUNDER_PERSON_FILE_ORG = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_ORG,
##     PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV)

    
#' test or generate/write the PMDB person comparison file
#' identify founder-persons that are actually the same
#' PPE: PMDB person entity
#' @param PMDB_FOUNDER_PERSON_FILE_WID file with links between founder_id, pmdb_id, founder_name
#' @param PMDB_PPECPRN_FILE the file of comparisons of similar names, read to compare with dt_pmdb_founder_person_wid
t_gwd_ppecprn <- function(PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID,
                          PMDB_PPECPRN_FILE = PMDATA_LOCS$PMDB_PPECPRN_FILE) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    pmdb_person_id <- founder_name <- founder_name2 <- NULL

    dt_pmdb_founder_person_wid <- gd_pmdb_founder_person(PMDB_FOUNDER_PERSON_FILE_WID)
    copy(dt_pmdb_founder_person_wid) %>% .[, .(clctr_name = founder_name)] %>% t_subset
            
    ## stringdist check
    ## heavily adapted from t_gwd_apecprn: use stringdistmatrix, check close ones, write them to file
    ## check the file later on if every close one has been checked manually
    dt_stringdist_prep <- dt_pmdb_founder_person_wid[, .(pmdb_person_id, founder_name)] %>% funique

    dt_stringdist_mat <- stringdist::stringdistmatrix(
                                        dt_stringdist_prep[, founder_name], dt_stringdist_prep[, founder_name],
                                          useNames = "strings") %>% adt %>% 
                         .[, founder_name := dt_stringdist_prep[, founder_name]]

    dt_stringdist_long <- melt(dt_stringdist_mat, id.vars = "founder_name", variable.name = "founder_name2",
                               value.name = "dist")

    dt_stringdist_close <- dt_stringdist_long[(founder_name != founder_name2) & dist < 5][order(dist)]

    dt_ppecprn <- dt_stringdist_close[founder_name > as.character(founder_name2)] %>%
        .[, `:=`(is_same_PPE = 0, correct_name = "")]

    dt_ppecprn_ff <- fread(PMDB_PPECPRN_FILE)

    ## check that all similar sounding names in PMDB are covered

    dt_redux <- dt_ppecprn[!dt_ppecprn_ff, on = .(founder_name, founder_name2)] %>%
        .[, founder_name2 := as.character(founder_name2)]

    tres <- fnrow(dt_redux) == 0
        
    ## dt_ppecprn_ff[founder_name == "İnan Kıraç"]


    return(tres)

    ## only uncomment this if you want to change PMDB_PPECPRN_FILE completely
    ## fwrite(dt_ppecprn, PMDB_PPECPRN_FILE)

}






#' test or generate/write link file of founder_id, founder_person_id and pmdb_person_id
#' usually just tests whether generated data.table is the same as the one currently written to file
#' writing is commented out, should only be uncommented when going here manually in debug mode
#' 
#' @param PMDB_FOUNDER_PERSON_FILE_CSV file where the PMDB_FOUNDER_PERSON_FILE_ORG has been exported to, each line has a person
#' @param PMDB_PPECPRN_FILE file with similar sounding names that have been checked manually, used to rename similar names in data.table based on PMDB_FOUNDER_PERSON_FILE_CSV
#' @param PMDB_FOUNDER_PERSON_FILE_WID file with links between founder_id, pmdb_id, founder_name
#' @export
t_gwd_pmdb_founder_person_wid <- function(
                                   PMDB_FOUNDER_PERSON_FILE_CSV = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_CSV,
                                   PMDB_PPECPRN_FILE = PMDATA_LOCS$PMDB_PPECPRN_FILE,
                                   PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    is_same_PPE <- founder_name <- correct_name <- founder_name2 <- pmdb_person_id <- i.correct_name <- NULL
    wrong_name <- N <- i.pmdb_person_id <- founder_string_pmdb <- NULL

    dt_ppecprn_checked <- fread(PMDB_PPECPRN_FILE)[is_same_PPE == 1]

    ## check that correct_name corresponds to one of founder_name and founder_name2
    if (!all(dt_ppecprn_checked[, founder_name == correct_name | founder_name2 == correct_name])) {
        stop("fix names")}
                                

    ## construct renamer dt to rename wrong names via update join
    dt_ppecprn_renamer <- copy(dt_ppecprn_checked)[
       , .(wrong_name = fifelse(founder_name == correct_name, founder_name2, founder_name), correct_name)]

    ## make ze IDs
    dt_pmdb_founder_person <- fread(PMDB_FOUNDER_PERSON_FILE_CSV) %>%
        .[, pmdb_person_id := 1:.N] %>%
        .[dt_ppecprn_renamer, founder_name := i.correct_name, on = .(founder_name = wrong_name)] # fix names

    ## get the simple duplicates
    dt_dupnames <- dt_pmdb_founder_person[, .N, founder_name][N>1] %>% 
        dt_pmdb_founder_person[., on = "founder_name"] %>%
        .[, .SD[which.min(pmdb_person_id)], founder_name]

    ## correct IDs of people who appear multiple times
    dt_pmdb_founder_person_wid <- copy(dt_pmdb_founder_person) %>% 
        .[dt_dupnames, pmdb_person_id := i.pmdb_person_id, on = "founder_name"] %>%
        .[, pmdb_person_id := paste0("PPE", pmdb_person_id)]


    if (fnrow(dt_pmdb_founder_person) != fnrow(dt_pmdb_founder_person_wid)) {stop("fnrow not the same")}

    ## check that past files version is the same as what this file would have generated
    dt_pmdb_founder_person_wid_ff <- gd_pmdb_founder_person(PMDB_FOUNDER_PERSON_FILE_WID)
    
    
    tres <- identical(copy(dt_pmdb_founder_person_wid)[, founder_string_pmdb := NULL],
                      dt_pmdb_founder_person_wid_ff)

    if (!tres) {
        dt_pmdb_founder_person_wid[!dt_pmdb_founder_person_wid_ff, on = "founder_person_id"] %>% print
        dt_pmdb_founder_person_wid_ff[!dt_pmdb_founder_person_wid, on = "founder_person_id"] %>% print
    }


    ## setdiff(dt_pmdb_founder_person_wid_ff, dt_pmdb_founder_person_wid)
    
    ## setdiff(dt_pmdb_founder_person_wid_ff$founder_name, dt_pmdb_founder_person_wid$founder_name)

    return(tres)

    ## only do this writing step once
    ## fwrite(dt_pmdb_founder_person_wid, PMDB_FOUNDER_PERSON_FILE_WID)
    
    
}

## t_gwd_pmdb_founder_person_wid()

    
    


#' provide the overall data.table for PMDB links between founder_id, founder_person_id, pmdb_person_id, founder_name
#' @param PMDB_FOUNDER_PERSON_FILE_WID file with links between founder_id, pmdb_id, founder_name
#' @export
gd_pmdb_founder_person <- function(
                                   PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID) {
    
    dt_pmdb_founder_person_wid <- fread(PMDB_FOUNDER_PERSON_FILE_WID)
    return(dt_pmdb_founder_person_wid)
}
    


#' generate list of unique PMDB founder-persons, with their pmdb_person_id
#' @param PMDB_FOUNDER_PERSON_FILE_WID to be passed to gd_pmdb_founder_person
#' @return dt_pmdb_person: dt with pmdb_person_id, founder_name, no duplicates in either
gd_pmdb_person <- function(
                           PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID) {
                           

    pmdb_person_id <- founder_name <- NULL

    dt_pmdb_founder_person <- gd_pmdb_founder_person(PMDB_FOUNDER_PERSON_FILE_WID)
                                                     

    dt_pmdb_person <- dt_pmdb_founder_person[, .(pmdb_person_id, founder_name)] %>% funique

    if (dt_pmdb_person[, any(sapply(.SD, any_duplicated))]) {stop("fix duplicates")}

    return(dt_pmdb_person)
}
        
## PMDATA_LOCS <- gc_pmdata_locs()
## gd_pmdb_person()   

## t1 <- Sys.time()
## gd_pmdb_founder_person()
## t2 <- Sys.time()


## gd_artnews_person()


## gd_pmdb_founder_person() %>% t_gwd_ppecprn


## ** generate PMDB proximity numbers

#' generate count of PMs in area (proximity + count -> proxcnt) with radius (km) around focal PM
#' @param dt_pmdb DT with ID, long, lat
#' @param radius_km radius in kilometers
#' @return dt with number of museums around ID
#' @export
gd_pmdb_proxcnt <- function(dt_pmdb, radius_km) {
    
    ID <- lat <- long <- NULL
    
    ## set up points
    dt_pmdb_points <- terra::vect(dt_pmdb[, .(ID, long, lat)], geom = c("long", "lat"), crs = "WGS84")

    ## set up distances (great circle)
    
    mat_dists <- terra::distance(dt_pmdb_points)

    ## aggregate
    dt_pmdb_proxcnt <- data.table(ID = dt_pmdb[, ID],
                                  proxcnt = rowSums(as.matrix(mat_dists) < radius_km*1000)) %>%
        setnames(old = "proxcnt", new = paste0("proxcnt", radius_km))
    
    ## plotting example
    ## world_map <- map_data("world")
    
    ## ggplot() +
    ##     geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) + 
    ##     geom_sf(dt_pmdb_points, mapping = aes(geometry = geometry), color = "red") + 
    ##     coord_sf(xlim = c(99, 103), ylim = c(-1, 1)) 

        
    return(dt_pmdb_proxcnt)
    
}







