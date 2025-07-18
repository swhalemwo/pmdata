#' just move colname generation to own function so I don't have to deal with it in main egmus function
#' @return list of column names
gc_egmus_colnames <- function() {
    
    c_egmus_colnames <- c(
        country = "Country",
        year = "Year",
        def_wg = "1a) Definition : Working group definition",
        def_cry = "1b) Definition : National criteria",
        col_ttl = "2a) Number of museums according to type of collection : total",
        col_art_arc_hist = "2b) Number of museums according to type of collection : of which - Art, archaeology and history museums",
        col_art_sci_tec_eth = "2c) Number of museums according to type of collection : of which - Science and technology museums, ethnology museums",
        col_other = "2d) Number of museums according to type of collection : of which - Other museums",
        own_state = "3a) Ownership : state-owned museums - Total",
        own_loc_reg = "3b) Ownership : local-, regional-owned museums - Total",
        own_publicother = "3c) Ownership : other public-owned museums - Total",
        own_priv = "3d) Ownership : private-owned museums - Total",
        own_ppp = "3e) Ownership : private-owned museums - Of which - ppp",
        mgm_state = "4a) Management : state-managed museums - Total",
        mgm_loc_reg = "4b) Management : local-, regional-managed museums - Total",
        mgm_publicother = "4c) Management : other public-managed museums - Total",
        mgm_priv = "4d) Management : private-managed museums - Total",
        mgm_ppp = "4e) Management : private-managed museums - Of which - ppp",
        col_own_pub_lt50 = "5a) Ownership of permanent collection : Public ≤ 50%",
        col_own_pub_gt50 = "5b) Ownership of permanent collection : Public &amp;gt; 50%",
        visits_ttl = "6a) Number of visits : Total",
        visits_free = "6b) Number of visits : Of which - free admissions - Total",
        visits_free_perc = "6c) Number of visits : Of which - free admissions - %",
        visits_foreign = "6d) Number of visits : Of which - foreigners - Total",
        visits_foreign_perc = "6e) Number of visits : Of which - foreigners - %",
        visits_tempexhb = "6f) Number of visits : Of which - temporary exhibitions - Total",
        visits_tempexhb_perc = "6g) Number of visits : Of which - temporary exhibitions - %",
        staff_paid_ttl = "7a) Staff : Paid staff - Number - Total",
        staff_apid_spec = "7b) Staff : Paid staff - Number - Of which - specialised",
        staff_paid_fte = "7c) Staff : Paid staff - FTES a year - Total",
        staff_paid_fte_spec = "7d) Staff : Paid staff - FTES a year - Of which - specialised",
        staff_vol_ttl = "7e) Staff : Volunteers - Number - Total",
        staff_vol_spec = "7f) Staff : Volunteers - Number - Of which - specialised",
        staff_vol_fte = "7g) Staff : Volunteers - FTES a year - Total",
        staff_vol_fte_spec = "7h) Staff : Volunteers - FTES a year - Of which - specialised",
        adm_euro_lt25 = "8a) Admissions : Total number of museums with paid admissions with a price of tickets (in EUR) - &amp;lt; 2,5,-",
        adm_euro_25_50 = "8b) Admissions : Total number of museums with paid admissions with a price of tickets (in EUR) - 2,5 - 5,-",
        adm_euro_gt50 = "8c) Admissions : Total number of museums with paid admissions with a price of tickets (in EUR) - &amp;gt; 5,-",
        adm_free = "8d) Admissions : Number of museums - with free entrance",
        adm_card = "8e) Admissions : Number of museums - with museum card",
        tempexhb_ttl = "9a) Temporary exhibitions : Total",
        tempexhb_ownprod = "9b) Temporary exhibitions : Of which - Own production",
        tempexhb_joined = "9c) Temporary exhibitions : Of which - Joined productions",
        exp_ttl = "10a) Expenditure (of museums) : Total (In EUR)",
        exp_ord_ttl = "10b) Expenditure (of museums) : Of which - Ordinary expenditures - Total",
        exp_ord_staff = "10c) Expenditure (of museums) : Of which - Ordinary expenditures - Of which - Staff",
        exp_extra = "10d) Expenditure (of museums) : Of which - Extraordinary expenditures - Total",
        exp_extra_col = "10e) Expenditure (of museums) : Of which - Extraordinary expenditures - Of which - for new Collections",
        inc_ttl = "11a) Income (of museums) : Total (in EUR)",
        inc_entryfees = "11b) Income (of museums) : Of which - Entry fees",
        inc_subsidies = "11c) Income (of museums) : Of which - Public subsidies",
        pubexp_muem = "12a) Public expenditure (for museums) : Total (in EUR)",
        pubexp_muem_inv = "12b) Public expenditure (for museums) : Of which - Investments",
        computer = "13a) Number of museums making use of computers : Total number of museums equipped with at least one computer",
        computer_admin = "13b) Number of museums making use of computers : Of which - for administrative purpose",
        computer_visinfo = "13c) Number of museums making use of computers : Of which - for visitor's information purposes (e.g. interactive gallery system)",
        computer_inventory = "13d) Number of museums making use of computers : Of which - having a database for electronic inventory",
        computer_internet = "13e) Number of museums making use of computers : Of which - having an Internet access",
        inf_website_ttl1 = "14) Information - Diffusion - Education : Number of museums possessing a web-site - Total",
        inf_website_ttl2 = "15a) Information - Diffusion - Education : Number of museums possessing an own web-site - Total",
        inf_website_upd = "15b) Information - Diffusion - Education : Number of museums possessing an own web-site - Of wich - are updating themselves their web-site",
        inf_portal = "16a) Information - Diffusion - Education : Number of museums - connected to a museum portal - Total",
        inf_portal_more = "16b) Information - Diffusion - Education : Number of museums - connected to a museum portal - Of which - connected to more than one museum portal",
        inf_portal_more2 ="16c) Information - Diffusion - Education : Number of museums - connected to (an)other portal(s)",
        inf_edu = "17a) Information - Diffusion - Education : Number of museums with at least one special museum education programme - Total",
        inf_edu_children = "17b) Information - Diffusion - Education : Number of museums with at least one special museum education programme - Of which - for school children",
        inf_edu_ethnic = "17c) Information - Diffusion - Education : Number of museums with at least one special museum education programme - Of which - for ethnic minorities",
        inf_edu_senior = "17d) Information - Diffusion - Education : Number of museums with at least one special museum education programme - for senior citizens",
        info_edu_others = "17e) Information - Diffusion - Education : Number of museums with at least one special museum education programme - for others",
        muem_p100k_wg = "18) Museums per 100.000 inhabitants : Museums per 100.000 inhabitants",
        muem_p100k_nat = "19) Museums per 100.000 inhabitants : Museums per 100.000 inhabitants",
        visits_p100k_wg = "20) Visits per 100.000 inhabitants : Visits per 100.000 inhabitants",
        visits_p100k_nat = "21) Visits per 100.000 inhabitants : Visits per 100.000 inhabitants",
        avg_visits_wg = "22) Average number of visits per museum : Average number of visits per museum",
        avg_visits_nat = "23) Average number of visits per museum : Average number of visits per museum",
        staff_p100k = "24) Staff per 100.000 inhabitants : Staff per 100.000 inhabitants",
        exp_pcap = "25) Expenditure per capita (in EUR) : Expenditure per capita (in EUR)",
        pubexp = "26) Public Expenditure per 100.000 inhabitants (in EUR) : Public Expenditure per 100.000 inhabitants (in EUR)",
        muem_opnd_gt200d = "27) Number of museums opened 200 days and more a year : Number of museums opened 200 days and more a year",
        muem_visits_gt5k = "28) Number of museums with more than 5.000 visits a year : Number of museums with more than 5.000 visits a year",
        muem_nec_50perc_visits = "29a) Number of museums necessary to reach : 50 % of total visits (including free entries)",
        muem_nec_75perc_visits = "29b) Number of museums necessary to reach : 75 % of total visits (including free entries)",
        muem_spec_staff_gt0 ="30) Number of museums employing at least 1 specialised staff : Number of museums employing at least 1 specialised staff",
        avg_price = "31) Average price paid : Average price paid",
        muem_publication_gt0 = "32) Number of museums with at least one publication : Number of museums with at least one publication",
        meum_elec_datacarrier = "33) Number of museums with at least one publication on an electronic data carrier : Number of museums with at least one publication on an electronic data carrier",
        muem_participation = "34) Museum participation : Museum participation",
        pop = "35) Population : Population",
        density_km2 = "36) Density inhabitants/km² : Density inhabitants/km²",
        purch_power_parity = "37) Purchasing Power Parity in EUR : Purchasing Power Parity in EUR",
        V91 = "V91")

    return(c_egmus_colnames)

}


#' read in EGMUS data
#' 
#' @param DIR_DATA_EGMUS path to EGMUS data
#' @return data.table with EGMUS data 
#' @export 
gd_egmus <- function(DIR_DATA_EGMUS = PMDATA_LOCS$DIR_DATA_EGMUS) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    V91 <- value <- vlu_new <- NULL

    
    system(paste0("cd ", DIR_DATA_EGMUS, " && rm *_seded.csv")) # remove old seded files

    ## list files in directory
    l_dt_egmus_names <- list.files(DIR_DATA_EGMUS, full.names = TRUE)
    l_dt_egmus_names_seded <- gsub(".csv", "_seded.csv", l_dt_egmus_names) # prep names for seded files
    
    ## replace all the non-breaking strings (doesn't really seem possible in fread)
    
    
    system(paste0('cd ', DIR_DATA_EGMUS, '&& for i in $(ls | grep csv); do echo item: $i; sed "s/$(printf "\240")//g" $i > "${i%.*}_seded.${i##*.}"; done;'))

    l_dt_egmus <- map(l_dt_egmus_names_seded, ~fread(.x, dec = ",")) 

    ## get original colnames
    l_egmus_colnames <- map(l_dt_egmus, names) %>% unlist %>% table

    c_colnames <- gc_egmus_colnames()

    if (length(setdiff(names(l_egmus_colnames), c_colnames)) != 0 |
        length(setdiff(c_colnames, names(l_egmus_colnames)) != 0)) {
        stop("egmus column names not good")}

    ## read in data, rename, yeet weird additional columns
    dt_egmus <- rbindlist(l_dt_egmus) %>%
        setnames(setNames(names(c_colnames), c_colnames)) %>% .[, V91 := NULL] 

    ## set up char variables, which are not properly converted (due to german decimal point system)
    l_char_vrbls <- setdiff(char_vars(dt_egmus, return = "names"), c("country", "def_wg", "def_cry"))

    ## seeing which variables produce issues with conversion
    ## options(warn =2) # warning -> error to stop loop
    ## for (i in l_char_vrbls) {
    ##     print(i)
    ##     dt_egmus[, get(i)] %>% gsub("[.]", "", .) %>% gsub(",", ".", .) %>% as.numeric
    ## }
        
    ## first yeet decimal points, then convert commas (used as dec) to decimal points, then convert to numeric
    dt_egmus_new <- dt_egmus %>% copy %>%
        .[, (l_char_vrbls) := map(.SD, ~as.numeric(gsub(",", ".", gsub("[.]", "", .x)))),
          .SDcols = l_char_vrbls]
    
    ## check that all for all non-emptry string values, NA statuses don't differ (ensures proper conversion)
    dt_cpr <- merge(
        melt(dt_egmus_new, id.vars = c("country", "year"), value.name = "vlu_new"),
        melt(dt_egmus, id.vars = c("country", "year")), by = c("country", "year", "variable"))
    
    if (dt_cpr[value != ""][is.na(vlu_new) != is.na(value), .N] > 0) {
        stop("some values are not properly converted")}

    return(dt_egmus_new)

    

}

