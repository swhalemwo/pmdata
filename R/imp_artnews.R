## * import artnews data

## ** general data.table generation and checks:
## dt_artnews_time, dt_artnews_collector_person, dt_artnews_person, dt_artnews_location

gd_artnews_time <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    clctr_name <- location <- NULL

    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    ## check whether location is handled properly: yea seems to be unique within collector
    dt_artnews_time[, .N, clctr_name]
    dt_artnews_time[, .N, .(clctr_name, location)]
    dt_artnews_time[grepl("David Bowie", clctr_name)]

    ## look at couples, "and" seems to be pretty good differentiator
    dt_artnews_time[grepl("and", clctr_name), .N, clctr_name] %>% print(n=300)

    
}

## gd_artnews_time()


#' test OR generate and writes the artnews collector file
#' here manually it has to be checked manually whether a collector entry in artnews ranking refers to single person
#' @param ARTNEWS_TIME_FILE the year-ranking-collector file
#' @param ARTNEWS_COLLECTOR_ENTRIES_FILE the output that gets produced the first time (fwrite has to be manually uncommented): unique collectors, with ID (Artnews-collector-entity, ACE) and heuristic of whether they are a couple. This file is also read back in to test whether every collector in time-ranking has had his couple_status checked manually.
t_gwd_artnews_clctr <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE,
                              ARTNEWS_COLLECTOR_ENTRIES_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    is_couple <- clctr_name <- is_couple <- ace_nbr <- an_entry_id <- an_collector_person_id <- clctr_name <- NULL

    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    dt_ace <- dt_artnews_time[, funique(.SD), .SDcols = c("clctr_name", "id")] %>%
        setnames(old = "id", new = "an_entry_id") %>% 
        .[, is_couple := fifelse(grepl(" and ", clctr_name), 1, 0)]

    ## get dt_ace from file (ff), check that all entries in ranking have been checked (by being in on-disk file)
    dt_ace_ff <- fread(ARTNEWS_COLLECTOR_ENTRIES_FILE)

    res <- F
    res <- fnrow(dt_ace[!dt_ace_ff, on = "an_entry_id"]) == 0

    ## if (fnrow(dt_ace[!dt_ace_ff, on = "an_entry_id"]) > 0) {
    ##     stop(paste0("not all clctr_names that are present in the ranking have been checked",
    ##                 " in ARTNEWS_COLLECTOR_ENTRIES_FILE. make sure they are covered, e.g. by ",
    ##                 "adding manually entries to ARTNEWS_COLLECTOR_ENTRIES_FILE or re-running the fwrite-call.",
    ##                 "make sure tho that this doesn't yeet other or even more clctr_names."))
    ## }
    
    ## return(invisible(T))
    return(res)

    ## fwrite is uncommented -> has to be manually enabled if error arises
    ## fwrite(dt_ace[, .(an_entry_id, clctr_name, is_couple)], paste0(ARTNEWS_COLLECTOR_ENTRIES_FILE))

    ## compare old (no ID in python parsing, still no-breaking spaces) and new:

    ## dt_artnews_time_old <- fread("/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/ranking.csv")

    ## ## heuristic coding ("and" present)  whether collector refers to couple
    ## dt_ace_old <- dt_artnews_time_old[, .(clctr_name = funique(clctr_name))] %>%
    ##     .[, .(an_entry_id = paste0("ACE", 1:.N), clctr_name,
    ##           is_couple = fifelse(grepl(" and ", clctr_name), 1, 0))]
    
    ## join on ACE ID, check whether names differ
    ## dt_cprn <- join(dt_ace, dt_ace_old[, .(clctr_name_old = clctr_name, an_entry_id)], on = "an_entry_id") %>%
    ##     .[clctr_name != clctr_name_old] %>%
    ##     .[, dist := stringdist(clctr_name, clctr_name_old)] %>% 
    ##     .[, .(clctr_name, clctr_name_old, dist)]
    ## looks good: names are the same (-> therefore, IDs are the same), except for NBSP, which is fine
    
    ## get dt_ace from file (ff), check if ACE IDs are the same
    ## dt_ace_ff <- fread(ARTNEWS_COLLECTOR_ENTRIES_FILE)
    
    ## all(dt_ace$is_couple == dt_ace_ff$is_couple)
    ## yup they are: the David Bowie and Iman entry is no longer fucked by NBSP
    ## dt_ace[grepl("Bowie", clctr_name)]

    ## compare the current and the on from file: only difference is NBSP
    ## dt_cprn <- join(dt_ace, dt_ace_ff[, .(clctr_name_ff = clctr_name, an_entry_id)], on = "an_entry_id") %>%
    ##     .[clctr_name != clctr_name_ff] %>%
    ##     .[, dist := stringdist(clctr_name, clctr_name_ff)] %>% 
    ##     .[, .(clctr_name, clctr_name_ff, dist)] 

    ## dt_cprn[, .N, dist]

    

}




#' tests or reads ARTNEWS_COLLECTOR_ENTRIES_FILE, duplicates couple rows to make reduction to person simpler
#'
#' e.g. entry "John Doe and Jane Smith" is repeated twice in ARTNEWS_COLLECTOR_PERSON_FILE (since in previous step in t_gwd_artnews_clctr) identified as couple, and is now replaced manually by two entries, "John Doe" and "Jane Smith" (deleting takes less keystrokes than expanding)
#' manual checking takes place in org-mode due to better table-editing capabilities (edits don't yeet indentation, which is the case in csv-mode). `an_clctr_person.org` is manually generated from ARTNEWS_COLLECTOR_ENTRIES_FILE via macros/replace-string, and then exported to via `org-table-export` to .csv (ARTNEWS_COLLECTOR_PERSON_FILE_ORG)
#' @param ARTNEWS_COLLECTOR_ENTRIES_FILE file generated by t_gwd_artnews_clctr, !!!AFTER BEING MANUALLY CHECKED THAT ALL COLLECTOR ENTRIES INDICATED TO BE COUPLES ARE IN FACT COUPLES!!!
#' @param ARTNEWS_COLLECTOR_PERSON_FILE ARTNEWS_COLLECTOR_ENTRIES_FILE, but with duplicated couple rows so that reduction to persons is easier. also has a person ID (Artnews person entity, "APE"; with indexing for couples, .e.g "APE10b" for second person in collector couple that is ACE10
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_ORG the outcome with each line a single person, produced by org-mode table edits and org-table-export
t_gwd_artnews_collector_person <- function(
          ARTNEWS_COLLECTOR_ENTRIES_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE,
          ARTNEWS_COLLECTOR_PERSON_FILE = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE,
          ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    is_couple <- ace_nbr <- an_entry_id <- an_collector_person_id <- clctr_name <- NULL

    
    dt_an_clctr_entries <- fread(ARTNEWS_COLLECTOR_ENTRIES_FILE)
    
    ## already copy the couple lines, saves manually copying (just have to delete the wrong parts)
    dt_an_person_prep <- rbind(dt_an_clctr_entries, dt_an_clctr_entries[is_couple==1]) %>% 
        .[, ace_nbr := as.integer(substring(an_entry_id, 4, nchar(an_entry_id)))] %>%
        .[, an_collector_person_id := paste0("ACPE", ace_nbr, letters[1:.N]), an_entry_id] %>%
        .[order(-is_couple, ace_nbr)] %>%
        .[, .(an_entry_id, an_collector_person_id, clctr_name, is_couple)]

    dt_an_person_prep_ff <- fread(ARTNEWS_COLLECTOR_PERSON_FILE_ORG)

    

    tres <- F
    tres <- all(fnrow(dt_an_person_prep[!dt_an_person_prep_ff, on = "an_collector_person_id"]) == 0,
                dt_an_person_prep_ff[, !any_duplicated(an_collector_person_id)])

    return(tres)
    

    ## if (fnrow(dt_an_person_prep[!dt_an_person_prep_ff, on = "an_collector_person_id"]) > 0) {
    ##     stop("not all an_collector_person_ids that are follow from ARTNEWS_COLLECTOR_ENTRIES_FILE",
    ##          "(via couple indicator) have been checked in ARTNEWS_COLLECTOR_ENTRIES_FILE. ",
    ##          "make sure they are covered, e.g. by ",
    ##          "adding manually entries to  ARTNEWS_COLLECTOR_PERSON_FILE_ORG or re-running the fwrite-call ",
    ##          "exporting that to org-table, making changes there, and exporting that again.",
    ##          "make sure tho that this doesn't yeet other or even more an_collector_person_id s.")
    ## }

    ## fwrite is uncommented -> has to be manually enabled if error arises
    ## fwrite(dt_an_person_prep, ARTNEWS_COLLECTOR_PERSON_FILE)
    
}

## t_gwd_artnews_collector_person()


#' check whether any collector is a subset of another collector, e.g. "John Meyer" is a subset of "John W. Meyer"
#' only compares collectors who share at least one element
#' @param dt_acpe_w_id1 dt with a bunch of stuff, including clctr_name, which is all that matters
t_subset <- function(dt_acpe_w_id1) {

    clctr_name <- clctr_name_split <- clctr_name2 <- terms1 <- terms2 <- NULL
    
    ## library(parallel)
    ## library(stringi) ## 

    ## dt_test_selfjoin <- data.table(clctr_name = "James Kapppa Alsdorf",
    ##                                clctr_name_split = c("James", "Kappa", "Alsdorf"))

    ## split collector name into words
    dt_clctrname_split_prep <- dt_acpe_w_id1 %>% copy() %>% # head(10) %>% 
        .[, .(clctr_name_split = unlist(tstrsplit(clctr_name, " "))), clctr_name]
        ## rbind(dt_test_selfjoin)
    
    ## self-join on clctr_name_split to only compare collectors who share at least one element
    ## e.g. compare only all James, all Alsdorfs etc
    dt_clctrname_split_selfjoin <- copy(dt_clctrname_split_prep) %>%
        .[.[, .(clctr_name2 = clctr_name, clctr_name_split)], on = "clctr_name_split", allow.cartesian = T] %>%
        .[, .(clctr_name, clctr_name2)] %>% funique # can save some more time by only comparing unique pairs
        
    
    ## generate list columns, check membership
    ## t1 <- Sys.time()
    dt_subset_prep <- dt_clctrname_split_selfjoin %>% copy() %>% # head(n= 10) %>% 
    ## dt_subset_prep <- dt_stringdist_long %>% copy() %>% # head(n= 10) %>% 
        ## .[, `:=`(terms1 = as.list(strsplit(clctr_name, split = " ")),
        ##          terms2 = as.list(strsplit(as.character(clctr_name2), split = " ")))]
        ## .[,  `:=`(terms1 = stri_split_fixed(clctr_name, " ", simplify = F),
        ##           terms2 = stri_split_fixed(as.character(clctr_name2), " ", simplify = F))]
        .[, c("terms1", "terms2") := lapply(.SD, \(x) stri_split_fixed(x, " ")),
          .SDcols = c("clctr_name", "clctr_name2")]
    ## t2 <- Sys.time()
    ## t2-t1
    ## dt_subset_prep[, member := mcmapply(\(x,y) all(x %in% y), terms1, terms2, mc.cores = 6), verbose=T]
    ## dt_subset_prep[all(x %in% y, terms1, terms2), verbose=T]
    ## xx <- mapply(\(x,y) all(x %in% y), dt_subset_prep$terms1, dt_subset_prep$terms2)
    ## dt_subset_prep[, all_members := dapply(.SD, \(...) all(c(...)[[1]] %in% c(...)[[2]]),
    ## MARGIN = 1, mc.cores = 4), .SDcols = c("terms1", "terms2")] # not faster
    dt_subset_prep[, subset := unlist(Map(\(x,y) all(x %in% y), terms1, terms2))]
    ## dt_subset_prep[, subset := unlist(Map(\(x,y) all(match(x, y)), terms1, terms2))]    
    ## t3 <- Sys.time()
    ## t3-t2
    ## t3-t1

    ## print(nrow(dt_subset_prep))
    ## print(dt_subset_prep[grepl("Alsdorf", clctr_name)])

    if (fnrow(dt_subset_prep[subset==T & (clctr_name != clctr_name2)]) > 0) {

        print(dt_subset_prep[subset==T & (clctr_name != clctr_name2)])
        
        stop(paste0("not all subsets are dealt with. add them to ARTNEWS_APECPRN_FILE.",
                    "if there is a subset match that is not a real match, add an exception here or elsewhere"))}
        
    ## could maybe shave of another few hundreds of seconds by stringi grepl matching (in scrap), but not worth it
    
        
}


#' test or generate the ARTNEWS_APECPRN_FILE: here similar names are harmonized
#' it is later read in to rename deviant names, to get APE IDs to refer to the same person
#' this is not supposed to be run again: It has been run once after string similarity calculation, the results of which have been written to file and manually checked
#' a later subset check (John William Meyer -> John Meyer) has led to a bunch of additions to ARTNEWS_APECPRN_FILE
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_WID to generate dt with ACE, ACPE, APE ids, used for string similarity and name-word subset checks
#' @param ARTNEWS_APECPRN_FILE the file to write the similar sounding names to
t_gwd_apecprn <- function(
                          ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID,
                          ARTNEWS_APECPRN_FILE = PMDATA_LOCS$ARTNEWS_APECPRN_FILE) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    an_person_id <- clctr_name <- clctr_name2 <- an_entry_id <- ace_left <- ace_right <- NULL

    ## gd_artnews_collector_person_wid
    dt_acpe_wid <- gd_artnews_collector_person(ARTNEWS_COLLECTOR_PERSON_FILE_WID)

    
    ## test whether any collector is subset of another one
    t_subset(dt_acpe_wid)
        
    ## prep for pairwise comparison
    dt_strindist_prep <- dt_acpe_wid[, .(an_person_id, clctr_name)] %>% funique


    dt_stringdist_mat <- stringdist::stringdistmatrix(
                                        dt_strindist_prep[, clctr_name], dt_strindist_prep[, clctr_name],
                                          useNames = "strings") %>% adt %>% 
                         .[, clctr_name := dt_strindist_prep[, clctr_name]]

    dt_stringdist_long <- melt(dt_stringdist_mat, id.vars = "clctr_name", variable.name = "clctr_name2",
                               value.name = "dist")

    dt_stringdist_close <- dt_stringdist_long[(clctr_name != clctr_name2) & dist < 5][order(dist)] 

    ## get collector entries as list
    ## get ACEs of clctr_name
    dt_ace_left <- dt_acpe_wid[dt_stringdist_close, on = "clctr_name"] %>%
        .[, .(ace_left = paste(sort(unique(an_entry_id)), collapse = "--")), clctr_name]

    ## get ACEs of clctr_name2
    dt_ace_right <- dt_acpe_wid[dt_stringdist_close, on = .(clctr_name = clctr_name2)] %>% 
        .[, .(ace_right = paste(sort(unique(an_entry_id)), collapse = "--")), clctr_name]


    ## get all relevant comparisons
    dt_ape_cprn <- dt_stringdist_close[dt_ace_left, on = "clctr_name"] %>% # join left
        .[dt_ace_right, on = .(clctr_name2 = clctr_name)] %>% # join right
        .[!ace_left == ace_right & clctr_name > clctr_name2] %>% ## yeet couples and reverse
        .[order(dist)] %>%
        .[, `:=`(is_same_APE = 0, correct_name = "")]
    
    ## dt_ape_cprn[grepl("Vogel", clctr_name)]

    dt_apecprn_ff <- fread(ARTNEWS_APECPRN_FILE) ## ff = from file

    ## check that every entry in dt_ape_cprn is covered in dt_apecpprn_ff
    dt_redux <- dt_ape_cprn[!dt_apecprn_ff, on = .(clctr_name, clctr_name2)]

    res <- fnrow(dt_redux) == 0
    
    return(res)
    
    ## if (fnrow(dt_ape_cprn[!dt_apecprn_ff, on = .(clctr_name, clctr_name2)]) > 0) {
    ##     stop(paste0("not all pairs with similar names have been checked in ARTNEWS_APECPRN_FILE",
    ##                 "make sure they are covered, e.g. by adding manually entries to ARTNEWS_APECPRN_FILE",
    ##                 "or re-running the fwrite-call.",
    ##                 "make sure tho that this doesn't yeet other or even more clctr_names."))
    ## }
    
  
    ## THIS HAS TO REMAIN UNCOMMENTED: ARTNEWS_APECPRN_FILE is written only once, then manually edited
    ## fwrite(dt_ape_cprn, ARTNEWS_APECPRN_FILE)

    ## helper code to look up names in time_file to check for whether different collector names are the same,
    ## being in subsequent years is such an indicator
    ## dt_artnews_time <- fread(PMDATA_LOCS$ARTNEWS_TIME_FILE)
    ## dt_artnews_time[grepl("Tobin", clctr_name)] %>% print(n=30)
    ## dt_artnews_time[grepl("Charlotte", clctr_name) & grepl("Weber", clctr_name)] %>% print(n=30)

    ## dt_apce[, .N, clctr_name][N>1] %>% 
    ## dt_apce[grepl("Rachel Mellon", clctr_name)]

    ## dt_ace <- fread(PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE)
    ## dt_ace[grepl("Mellon", clctr_name)]
}

## t_gwd_apecprn()

        
## ONLY RUN ONCE:
## this duplicates couple rows so that manual deletion is easier
## names are not standardized for automatic detection of both names
## t_gwd_artnews_collector_person()

#' generate-and-writes or tests the artnews data.table with ACE, ACPE, and APE (link entries to persons)
#' 
#' fwrite command is uncommented, should be uncommentedif I want to rewrite the ID file after test failure.
#' 
#' was previously gd_artnews_collector_person, but now the reading is moved to separate function
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_ORG the file with single person per line (created by t_gwd_artnews_collector_person, and then manually edited in org-mode with org-table-export)
#' @param ARTNEWS_APECPRN_FILE file indicating whether two different names refer to the same person (generated by t_gwd_apecprn and then manually checked)
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_WID the file to write to, and to check whether this is the same
t_gwd_artnews_collector_person_wid <- function(
    ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG,
    ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID,
    ARTNEWS_APECPRN_FILE = PMDATA_LOCS$ARTNEWS_APECPRN_FILE) {

    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    is_same_APE <- clctr_name <- correct_name <- clctr_name2 <- an_person_id <- i.correct_name <- NULL
    wrong_name <- N <- i.an_person_id <- NULL

    ## read in the renamer
    dt_apecprn_checked <- fread(ARTNEWS_APECPRN_FILE)[is_same_APE==1]

    ## check that correct_name corresponds to one of clctr_name and clctr_name2
    if (!all(dt_apecprn_checked[, clctr_name == correct_name | clctr_name2==correct_name])) {stop("fix names")}

    ## construct renamer dt to rename wrong names via update join
    dt_apecprn_renamer <- copy(dt_apecprn_checked)[
       , .(wrong_name = fifelse(clctr_name == correct_name, clctr_name2, clctr_name), correct_name)]
    
    
    ## read in collector person file,  update wrong names with update join
    dt_acpe <- fread(ARTNEWS_COLLECTOR_PERSON_FILE_ORG) %>%
        .[, an_person_id := 1:.N] %>% # assign ID
        .[dt_apecprn_renamer, clctr_name := i.correct_name, on = .(clctr_name = wrong_name)] ## fix names
    
    if (any_duplicated(dt_acpe$an_person_collector_id)) {stop("ACPEs not unique")}
    

    ## get cases where name is literally duplicate
    dt_simpledups <- dt_acpe[, .N, clctr_name][N>1] %>%
        dt_acpe[., on = "clctr_name"] %>% # get all APE
        .[, .SD[which.min(an_person_id)], clctr_name]   # get first ID

    ## reassign simple duplicates with update join 
    dt_acpe_wid <- copy(dt_acpe)[dt_simpledups, an_person_id := i.an_person_id, on = "clctr_name"] %>%
        .[, an_person_id := paste0("APE", an_person_id)]

    if (nrow(dt_acpe) != nrow(dt_acpe_wid)) {stop("number of rows not the same")}


    ## fwrite(dt_acpe_wid, ARTNEWS_COLLECTOR_PERSON_FILE_WID)

    
    dt_acpe_wid_ff <- gd_artnews_collector_person(ARTNEWS_COLLECTOR_PERSON_FILE_WID)
    tres <- identical(dt_acpe_wid, dt_acpe_wid_ff)
    
    return(tres)
    
    ## return(dt_acpe_w_id1)
    
}

## PMDATA_LOCS <- gc_pmdata_locs()
## t_gwd_artnews_collector_person_wid()



#' reads the Artnews ID link file (collector to person, also founder names)
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_WID the file to read, previosly written by t_gwd_artnews_collector_person_wid
#' @export
gd_artnews_collector_person <- function(
                ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID) {

    dt_artnews_collector_person_wid <- fread(ARTNEWS_COLLECTOR_PERSON_FILE_WID)
    return(dt_artnews_collector_person_wid)
}
    

#' generate dt of unique artnews person with their APE ID, based on call to gd_artnews_collector_person 
#' can be matched to collector entries via dt_artnews_collector_person
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_WID file with artnews ID links (ACE, ACPE, APE) and clctr_names
#' @return data.table with an_persion_id, clctr_name
#' @export
gd_artnews_person <- function(
    ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID) {
        if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    an_person_id <- clctr_name <- NULL

    dt_artnews_collector_person <- gd_artnews_collector_person(
        ARTNEWS_COLLECTOR_PERSON_FILE_WID)

    dt_artnews_person <- dt_artnews_collector_person[, .(an_person_id, clctr_name)] %>% funique

    if (dt_artnews_person[, any(sapply(.SD, any_duplicated))]) {stop("fix duplicates")}

    return(dt_artnews_person)

}


                                                               


## t1 <- Sys.time()
## gd_artnews_collector_person()
## t2 <- Sys.time()
## t2-t1

## fread(PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE)




## stuff to move to overall import function
## - t_gwd_artnews_clctr()
## - t_gwd_artnews_collector_person()







#' read in the extra locations which are not auto matched by countrycode
#' These have been previously generated during org_pop
#' check that all of collector locations are covered: maybe that should be elsewhere
#' FIXME: put into two functions with having written countrycode to file to save time and have proper testing
#' @param ARTNEWS_LOCTABLE_FILE the loctable: non-standardized locations in Artnews to iso3c
#' @param ARTNEWS_TIME_FILE the ranking time
#' @export
t_gd_artnews_location <- function(ARTNEWS_LOCTABLE_FILE = PMDATA_LOCS$ARTNEWS_LOCTABLE_FILE,
                             ARTNEWS_TIME_FILE  = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    
    clctr_name <- i.country2 <- id <- iso3c <- loc_split <- location <- NULL

    
    dt_an_time <- fread(ARTNEWS_TIME_FILE)
    
    ## location doesn't change over time: every collector has one location for all observations
    fnrow(dt_an_time[, .(clctr_name, location)]) == fnrow(dt_an_time[, .(clctr_name)])

    ## get unique locations
    dt_an_clctr_loc <- dt_an_time[, .(clctr_name, id, location)] %>% funique
    
    ## first split AN locations by ";", then see what can be matched by countrycode by default
    dt_clctr_loc1 <- dt_an_clctr_loc[, .(loc_split = unlist(tstrsplit(location, ";"))), .(clctr_name, id)] %>%
        .[, loc_split := trimws(loc_split)] %>% 
        .[, iso3c := countrycode(loc_split, "country.name", "iso3c", warn = F)]

    ## read in the location table
    dt_an_loctable <- fread(ARTNEWS_LOCTABLE_FILE)

    ## see which remaing are matched by dt_an_loctable
    dt_clctr_loc1[dt_an_loctable, iso3c := i.country2, on= .(loc_split = location)]

    ## see which are still remaining: this has to be zero
    ## dt_clctr_loc1[is.na(iso3c), .(loc_split)] %>% funique %>% print(n=80)
    dt_clctr_loc1[is.na(iso3c), .N] == 0

    return(funique(dt_clctr_loc1[, .(ACE_id = id, iso3c)]))


    ## streamline dt_loctable: yeet the semicolon locations by pre-splitting them
    ## selecting the needed ones only after dt_clctr_loc2 has been generated, to generate it again easily
    ## ARTNEWS_LOCTABLE_FILE2 <-"/home/johannes/Dropbox/phd/pmdata/data_sources/artnews/an_loctable2.csv"
    
    ## dt_clctr_loc2[, .(location = loc_split, country2 = iso3c1)] %>% funique %>% fwrite(ARTNEWS_LOCTABLE_FILE)

}




## PMDATA_LOCS <- gc_pmdata_locs()

## t1 <- Sys.time()
## t_gd_artnews_location()
## t_gd_an_loctable()
## t2 <- Sys.time()

## ** match artnews and PMDB

#' write alist of artnews collectors to artnews-persons.el so that it can be read by emacs, and be fuzzily matched via consult
#' adds collector location to the margins as info
#' basically same format as with MOW
#' @param ARTNEWS_COLLECTOR_PERSON_FILE_WID file to match collectors entries to collector persons
#' @param ARTNEWS_LOCTABLE_FILE having location is good information when matching
#' @param ARTNEWS_TIME_FILE atm needed for constructing location data.table
gwl_artnews_clctrs_tomatch <- function(
                               ARTNEWS_COLLECTOR_PERSON_FILE_WID = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_WID,
                               ARTNEWS_LOCTABLE_FILE = PMDATA_LOCS$ARTNEWS_LOCTABLE_FILE,
                               ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    an_entry_id <- an_person_id <- iso3c <- clctr_name <- info <- NULL

    ## generated needed files
    dt_artnews_person <- gd_artnews_person(ARTNEWS_COLLECTOR_PERSON_FILE_WID)

    dt_artnews_location <- t_gd_artnews_location(ARTNEWS_LOCTABLE_FILE, ARTNEWS_TIME_FILE)

    dt_artnews_collector_person <- gd_artnews_collector_person(ARTNEWS_COLLECTOR_PERSON_FILE_WID)

    ## start with APE, join with ACE to add iso3c, collapse iso3c on clctr
    dt_info <- join(dt_artnews_person, dt_artnews_collector_person, how = "left", on = "an_person_id") %>%
        dt_artnews_location[., on = .(ACE_id = an_entry_id)] %>%
        .[, .(an_person_id, iso3c, clctr_name)] %>%
        .[, .(info = paste0(iso3c, collapse = "-")), .(clctr_name, an_person_id)]

    ## generate alist sections
    artnews_segs <- dt_info[, sprintf("(\"%s\" . ((id . \"%s\") (info . \"%s\")))", clctr_name, an_person_id, info)]

    ## combine into alist-string
    artnews_alist_str <- c("(setq artnews-persons '(", artnews_segs, "))")

    artnews_person_file_match <- paste0(dirname(ARTNEWS_TIME_FILE), "/artnews-persons.el")
    
    fileConn <- file(artnews_person_file_match)
    writeLines(artnews_alist_str, fileConn)
    close(fileConn)

}

## gwl_artnews_clctrs_tomatch()


## fread(PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG) %>%
##     .[grepl("Ganz", clctr_name)]

#' check whether a pmdb_person is also included in Artnews
#'
#' called as part of gw_artnews_pmdb_matches
#' will print the name of the pmdb_person to check (with his nationality, if available)
#' then you C-c m to call artnews-pmdb-match (defined in match-pmdb-artnews.el) where you search whether the person exists in Artnews
#' interactive fuzzy matching via consult allows to narrow down candidates quickly
#' if a match is found, press enter twice: first name enters the APE_ID to the R CLI, second one assigns this inserted APE_ID to an internal variable and writes the match to file
#' if no match is found: just type in j (or any other string that doesn't have "APE" as the first three letters) which will be converted to "nomatch"
#' @param dt_pmdb_person_matchy dt with pmdb person information, such as ID, name, nationality
#' @param pmdb_person_id_each single ID of PMDB person
check_artnews_pmdb_match <- function(dt_pmdb_person_matchy, pmdb_person_id_each) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    pmdb_person_id <- founder_name <- nationality <- NULL


    do.call(sprintf, c(list(fmt = "%s, %s"),
                       as.list(dt_pmdb_person_matchy[pmdb_person_id == pmdb_person_id_each,
                                                     .(founder_name, nationality)]))) %>% print

    APE_ID <- readline("Artnews ID: ")

    # if entry doesn't start with "APE" assume missing data (just type "j" or whatever)
    if (substring(APE_ID,1,3) != "APE") APE_ID <- "nomatch"

    dt_res <- data.table(pmdb_person_id = pmdb_person_id_each, artnews_person_id = APE_ID)
    fwrite(dt_res, PMDATA_LOCS$ARTNEWS_PMDB_MATCHRES_FILE, append = T)
        
}



#' overall function to check the matches between PMDB and artnews
#' map over each PMDB founder person, and check via consult whether it is present in Artnews
#' can stop inbetween, with gd_artnews_pmdb_matchres I can keep track of current status
#' have for now stopped providing the FILE_NAMES as arguments, are now relying on the lower level functions and PMDATA_LOCS specifications
gw_artnews_pmdb_matches <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    founder_id <- nationality <- founder_name <- pmdb_person_id <- NULL
    
    ## convoluted way of getting nationality to founder person, this should all be stored in one place
    dt_pmdb_founder_nat <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb %>%
        .[founder_id != "" & nationality != "", .(founder_id, nationality)] %>% funique 
        

    dt_pmdb_person <- gd_pmdb_person()
    dt_pmdb_founder_person <- gd_pmdb_founder_person()

    ## awkward filtering out of variation of nationality, FIXME in PMDB
    dt_pmdb_person_matchy <- join(dt_pmdb_founder_person, dt_pmdb_founder_nat, how = "l", on = "founder_id") %>%
        .[, .(founder_name, pmdb_person_id, nationality)] %>% funique %>%
        .[, .SD[all(is.na(nationality)) | !is.na(nationality)], .(founder_name, pmdb_person_id)] 
        ## .[grepl("Rachofsky", founder_name)]

    ## see PMDB persons have already been checked
    dt_artnews_pmdb_matches <- gd_artnews_pmdb_matchres()

    ## see which PMDB persons still need to be check
    pmdb_person_ids_to_check <- setdiff(dt_pmdb_person_matchy$pmdb_person_id,
                                        dt_artnews_pmdb_matches$pmdb_person_id)
    print(length(pmdb_person_ids_to_check))
    ## actually do the checking

    
    map(pmdb_person_ids_to_check, ~check_artnews_pmdb_match(dt_pmdb_person_matchy, .x))
    
    ## pmdb_founder_ids_tocheck <- dt_pmdb_founder_matchy$

}



#' read in already checked PMDB persons
#' @param ARTNEWS_PMDB_MATCHRES_FILE file with matches between artnews persons and PMDB persons
gd_artnews_pmdb_matchres <- function(ARTNEWS_PMDB_MATCHRES_FILE = PMDATA_LOCS$ARTNEWS_PMDB_MATCHRES_FILE) {
    

    if (file.exists(ARTNEWS_PMDB_MATCHRES_FILE)) {
        dt_res <- fread(ARTNEWS_PMDB_MATCHRES_FILE)
    } else {
        dt_res <- data.table(an_person_id = character(), pmdb_person_id = character())
    }

    return(dt_res)
}

#' test that all pmdb_founder_ids have been checked
#'
#' @param dt_pmdb provided in testing environment
#' @param ARTNEWS_PMDB_MATCHRES_FILE file of matches between Artnews and PMDB
#' @param PMDB_FOUNDER_PERSON_FILE_WID file including all pmdb_person_ids; check that these are covered by matchres
t_pmdb_artnews_match_coverage <- function(dt_pmdb,
                                          ARTNEWS_PMDB_MATCHRES_FILE = PMDATA_LOCS$ARTNEWS_PMDB_MATCHRES_FILE,
                                          PMDB_FOUNDER_PERSON_FILE_WID = PMDATA_LOCS$PMDB_FOUNDER_PERSON_FILE_WID) {
    if (as.character(match.call()[1]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## read in files
    dt_pmdb_founder_wid <- gd_pmdb_founder_person(
        PMDB_FOUNDER_PERSON_FILE_WID = PMDB_FOUNDER_PERSON_FILE_WID)

    dt_artnews_pmdb_matchres <- gd_artnews_pmdb_matchres(ARTNEWS_PMDB_MATCHRES_FILE)

    ## check that all pmdb_person_ids are covered in dt_artnews_pmdb_matchres
    dt_redux <- dt_pmdb_founder_wid[!dt_artnews_pmdb_matchres, on = "pmdb_person_id"]

    tres <- fnrow(dt_redux) == 0
    return(tres)

}

## gd_artnews_time()


## gd_artnews_pmdb_matchres()

                               
## gw_artnews_pmdb_matches()

## gd_artnews_collector_person()

## gd_pmdb_excl(only_pms = F) %>% gd_pmdb %>% .[!is.na(an_fyear), .N]
