## * import PMDB



#' finds a variable in a data.frame from candidates
#'
#' with quickly changing excel sheets, column/variable names can change as well. vrbl_fndr gets the
#' one variable that is present in the data from a vector of candidates.
#' if multiple or none variable is found, an error is thrown
#' @param df data.frame 
#' @param penl_vrbls vector of potential variables
#' @return the variable that is both in penl_vrbls and df
vrbl_fndr <- function(df, penl_vrbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' find the variable referred by by potential variables (penl_vrbls)

    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (len(vrbl) == 0 ) {
        stop("no variable found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else {
        vrbl
    }
}



#' generates the most basic data.frame of the google sheet
#' (only handful of columns standardized)
#'
#' @param PMBD_FILE path to CSV download from google sheets
#' @param only_pms whether to include only private museums that are currently open
#' @export
gendt_pmdb_excl <- function(PMDB_FILE = DATA_LOCS$PMDB_FILE, only_pms=T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    dt_pmdb_excl <- fread( PMDB_FILE)[2:.N]# skip second column
    ## dt_pmdb_excl[, map(.SD, class)] %>% melt(id.vars = "ID") %>% .[value != "character"] %>% .[, .N, value]
    ## dtx <- read.xlsx2(PMDB_FILE, sheetName = "Blad1") %>% adt() %>% .[2:.N]
    ## dtx[653:.N] %>% melt(id.vars = "ID") %>% .[value != ""]
    ## some additional basic aux columns, no transformation
    basic_cols <- list(
        country = c("Country where museum is located", "Museum_country"),
        name = c("Museum_name", "Name of museum"),
        year_opened_str =  c("Opening year", "Museum_opening year"),
        year_closed_str = c("Closing year", "Museum_closing year"),
        museum_status = c("Museum status", "Museum_status"))
    ## throw error if any of the aux columns are already there
    if (len(setdiff(names(basic_cols), names(dt_pmdb_excl))) != len(names(basic_cols))) {
        stop("new cols already there")}
    ## select basic_cols subset of columns, rename it
    dt_pmdb_excl2 <- dt_pmdb_excl[, .SD, .SDcols = unlist(map(basic_cols, ~vrbl_fndr(dt_pmdb_excl, .x)))] %>% 
        setnames(new = names(basic_cols))%>% # update names 
        cbind(dt_pmdb_excl) %>% # combine with old data
        .[, `:=`(ID = as.integer(ID), # edit ID columns 
                 iso3c = countrycode(country, "country.name", "iso3c"), # add iso3c column
                 year_opened = as.integer(year_opened_str),
                 year_closed = as.integer(year_closed_str))]

    ## check that all countries with country string convert to country code
    if (dt_pmdb_excl2[is.na(iso3c) & country != "", .N] > 0) {stop("not all countries are matched")}
    
    

    ## test_file("~/Dropbox/phd/pmdata/data_sources/pmdb/tests_gendt_pmdb_excl.R")

    ## test_that("all PMs have ID", {expect_true(dtx[is.na(ID), .N] == 0)})
    ## test_that("all_PMs have ID", {expect_true(dtx[, none(ID, is.na)])})
    ## if (dt_pmdb_excl2[is.na(ID), .N > 0]) {stop("not all PMs have ID")}
    ## if (dt_pmdb_excl2[, uniqueN(ID) == .N]) {stop("not all IDs are unique")}
    
    return(dt_pmdb_excl2)
        
}


#' generates the most basic data.frame of the google sheet as rmonad (only handful of columns standardized)
#'
#' @param PMBD_FILE path to CSV download from google sheets
#' @param only_pms whether to include only private museums that are currently open
#' @export
gendtm_pmdb_excl <- function(PMDB_FILE = DATA_LOCS$PMDB_FILE, only_pms=T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
        
    basic_cols <- as_monad(list(
        country = c("Country where museum is located", "Museum_country"),
        name = c("Museum_name", "Name of museum"),
        year_opened_str =  c("Opening year", "Museum_opening year"),
        year_closed_str = c("Closing year", "Museum_closing year"),
        museum_status = c("Museum status", "Museum_status"))) %>% tag("basic_cols")

    ## dt_input <- fread(PMDB_FILE)[2:5] %>>% as_monad %>% tag("input") 

    m_excl_prep <- PMDB_FILE %>>% (\(x) fread(x)[2:.N]) %>% tag("dt_input") %>%
        funnel(basic_cols) %>%
        tag2((\(x) if (any(names(x[[2]]) %in% names(x[[1]])))
                   {stop("new cols already there")}), "check_basic_cols")  %*>%
        (\(dtx, bcols) dtx[, .SD, .SDcols = unlist(map(bcols, ~vrbl_fndr(dtx, .x)))] %>%
                       setnames(new = names(bcols))) %>% tag("dt_wbcol") %>% 
        funnel(view(., "dt_input")) %*>% cbind %>>%
            d(, `:=`(ID = as.integer(ID), # edit ID columns 
                     iso3c = countrycode(country, "country.name", "iso3c"), # add iso3c column
                     year_opened = as.integer(year_opened_str),
                     year_closed = as.integer(year_closed_str))) %>% tag("dt_excl")

    ## plot(m_excl_prep, label = \(x) get_tag(x))
    ## m_excl_prep %>% missues

    ## run tests
    m_excl <- m_excl_prep %>%
        tag2((\(x) if (x[is.na(iso3c) & country != "", .N] > 0)
                       {stop("not all countries matched")}), "check_countrycode_match") %>%
        tag2((\(x) if (x[is.na(ID), .N] > 0) {stop("not all IDs there")}), "check_IDs_presence") %>%
        tag2((\(x) if (x[, .N, ID][, max(N)] > 1) {stop("not all IDs are unique")}), "check_ID_unqs")
        
    return(m_excl)

    ## mx <- 10 %>% as_monad %>>% sqrt %>% tag("s1") %>>% sqrt %>% tag("s2")
    ## plot(mx)

    ## funnel(view(mx, "s1"), view(mx, "s2")) %>% tag2(len, "len") %>% get_value(tag ="len")

    ## more complicated operation
    ## funnel(view(mx, "s1"), view(mx, "s2")) %*>% tag2((\(x,y) x+y), "sum")
    
    ## 
    ## funnel(view(mx, "s1"), view(mx, "s2")) %>% tag2((\(x) x[[1]] + x[[2]]), "sum") %*>%
    ##     (\(x,y) x * y) %>% plot(label = "code")

    ## funnel(view(mx, "s1"), view(mx, "s2")) %>% 

}


#' generates basic data.frame of the google sheet (using collapse)
#'
#' only a handful of columns are standardized here
#' @param PMBD_FILE path to CSV download from google sheets
#' @param only_pms whether to include only private museums that are currently open
#' @export
gendtc_pmdb_excl <- function(PMDB_FILE = DATA_LOCS$PMDB_FILE, only_pms = T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

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


    if (fsubset(dt_pmdb_excl2, is.na(iso3c) & country != "") %>% fnrow > 0) {stop("not all countries are matched")}

    if (only_pms) {
        return(sbt(dt_pmdb_excl2, museum_status == "private museum"))
    } else if (!only_pms) {
        return(dt_pmdb_excl2)
    }
    

}




## #' test function
## #'
## #' this is some description
## #' @export 
## testf <- function() {
##     15
## }

## #' @export 
## testf2 <- function() {
##     290
## }
