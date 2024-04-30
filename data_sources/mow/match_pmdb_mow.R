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

    # if entry doesn't start with "M" assume missing data (just type "j" or whatever)
    if (substring(MOW_ID,1,1) != "M") MOW_ID <- "nomatch"

    dt_res <- data.table(PMDB_ID = pmdb_ID, MOW_ID = MOW_ID)
    fwrite(dt_res, MOW_PMDB_MATCH_RES_FILE, append = T)
    

}

## check_mow_pmdb_match(dt_pmdb_matchy, 11)


check_mow_match_old_recent <- function(dt_pmdb_matchy) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## manually compare recent (October 2023) to previous (Feb 2022?) MOW matching attempts
    ## previous matching didn't use PMDB_IDs yet -> need to compare names

    ## this function doesn't really produce anything, and is not intended to be run
    ## it is just a neat place of the manual steps that I run manually to check different match procedures
    

    dt_match <- fread(MOW_PMDB_MATCH_RES_FILE)[dt_pmdb_matchy[, .(ID, name)], on = .(PMDB_ID = ID)]

    ## checking duplicate MOW entries

    dt_match[MOW_ID != "nomatch", .N, MOW_ID][order(-N)]
    dt_pmdb_matchy[dt_match[MOW_ID == "fill duplicate ID here"], on = .(ID = PMDB_ID)]

    dt_pmdb_matchy[grepl("long", name, ignore.case = T)]



    ## dt_pmdb_matchy[grepl("prada", name, ignore.case = T)]
    ## prada: has the milano one -> PMDB_ID= 530

    ## dt_pmdb_matchy[grepl("jumex", name, ignore.case = T)]
    
    ## Museo Soumaya: MOW has the PL loreto one -> PMDB_ID == 173

    ## IMS
    ## MOW
    ## ("Instituto Moreira Salles" . ((id . "M131039") (info . "Brazil-NA")))
    ## ("Instituto Moreira Salles" . ((id . "M131088") (info . "Brazil-NA")))
    ## ("Instituto Moreira Salles" . ((id . "M131205") (info . "Brazil-NA")))
    ## ("Instituto Moreira Sales" . ((id . "M131373") (info . "Brazil-NA")))

    ## PMDB
    ##    ID name                                   
    ##   220 IMS (Instituto Moreira Salles)         Rio   M131205
    ##   646 Instituto Moreira Salles – IMS Paulista: Sao Paolo  M131373
    ##   647 Instituto Moreira Salles – IMS Poços   : pocos de calda M131088



    ## match_old.csv is a copy of data/degruyter/mow/dupl.csv
    dt_match_old <- fread(paste0(dirname(MOW_PMDB_MATCH_RES_FILE), "/match_old.csv"), 
                          col.names = (.c(name, match, MOW_ID_old)))

    ## compare number of matches between old and current matching process
    dt_match[MOW_ID != "nomatch", .N]
    dt_match_old[!is.na(MOW_ID_old), .N]
    ## current matching more than doubles the number of matches from 53 to 110

    ## have to match on name -> full join?
    dt_match_check1 <- merge(dt_match, dt_match_old, by = "name", all = T)
    ## fuck names changed so much, like around 80% 
    
    ## only 194 can be matched on name, 752 not (from both versions)
    dt_match_check1[!is.na(match) & !is.na(MOW_ID)]
    dt_match_check1[!(!is.na(match) & !is.na(MOW_ID))]

    
    ## try matching on MOW IDs
    
    dt_match_check4 <- merge(dt_match[MOW_ID != "nomatch"],
          dt_match_old[!is.na(MOW_ID_old), .(name_old = name, MOW_ID = MOW_ID_old)],
          by = "MOW_ID", all = T)

    ## search teh PMs that had an old name, but not a new one 
    dt_match_check4[is.na(name)]
    ## look at each of this individually, and make corrections in mow_pmdb_match_res.csv where necessary
    
    ## now all unclear cases fixed, remaining ones are not PMs
    ## dt_pmdb_matchy[grepl("klaus ", name, ignore.case = T)]

    ## notes of the manual checks here
    ## dt_match[PMDB_ID == 189]

    ## russian art mess: two museums
    ## TMORA: minneapolis -> M195205, PMDB_ID = 532
    ## MORA: jersey city -> M183479, PMDB_ID = 142

    ## dt_match[PMDB_ID == 532]
    ## dt_match[PMDB_ID %in% c(532, 142)]


    ## check the PMs where old and new name is different
    dt_match_check4[name_old != name] %>% print(n=200)
    ## looking good, all about different spellings
    
    


}






gw_mow_pmdb_matches <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## library(collapse)
    library(countrycode)

    ## generate PMDB: these have to be checked
    dt_pmdb_matchy <- gd_pmdb_excl(gc_pmdata_locs()$PMDB_FILE, only_pms = F) %>%
        .[, .(ID, name, museum_status, year_opened, country = countrycode(iso3c, "iso3c", "country.name"))] %>%
        .[museum_status %!in% c("no private museum", "duplicate")]

    dt_mow_pmdb_matches <- gd_mow_pmdb_matchres(gc_pmdata_locs()$MOW_PMDB_MATCHRES_FILE)
 
    pmdb_IDS_to_check <- setdiff(dt_pmdb_matchy$ID, dt_mow_pmdb_matches$PMDB_ID)

    ## print(pmdb_IDS_to_check)

    map(pmdb_IDS_to_check, ~check_mow_pmdb_match(dt_pmdb_matchy, .x))

    ## not run the comparison of current (oct 2023) matching procedure to previous one (feb 2022)
    ## check_mow_match_old_recent(dt_pmdb_matchy)
}




## here go the results
## MOW_PMDB_MATCH_RES_FILE <- paste0(dirname(gc_pmdata_locs()$MOW_INFO_FILE), "/mow_pmdb_match_res.csv")
MOW_PMDB_MATCH_RES_FILE <- gc_pmdata_locs()$MOW_PMDB_MATCHRES_FILE

gw_mow_pmdb_matches()





