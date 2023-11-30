

gd_artnews_time <- function(ARTNEWS_TIME_FILE = PMDATA_LOCS$ARTNEWS_TIME_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    

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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;



    dt_artnews_time <- fread(ARTNEWS_TIME_FILE)

    dt_ace <- dt_artnews_time[, funique(.SD), .SDcols = c("clctr_name", "id")] %>%
        setnames(old = "id", new = "an_entry_id") %>% 
        .[, is_couple := fifelse(grepl(" and ", clctr_name), 1, 0)]

    ## get dt_ace from file (ff), check that all entries in ranking have been checked (by being in on-disk file)
    dt_ace_ff <- fread(ARTNEWS_COLLECTOR_ENTRIES_FILE)

    if (fnrow(dt_ace[!dt_ace_ff, on = "an_entry_id"]) > 0) {
        stop(paste0("not all clctr_names that are present in the ranking have been checked",
                    " in ARTNEWS_COLLECTOR_ENTRIES_FILE. make sure they are covered, e.g. by ",
                    "adding manually entries to ARTNEWS_COLLECTOR_ENTRIES_FILE or re-running the fwrite-call.",
                    "make sure tho that this doesn't yeet other or even more clctr_names."))
    }
    
    return(invisible(T))

    ## stop("this function is not supposed to be run again")
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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    
    dt_an_clctr_entries <- fread(ARTNEWS_COLLECTOR_ENTRIES_FILE)
    
    ## already copy the couple lines, saves manually copying (just have to delete the wrong parts)
    dt_an_person_prep <- rbind(dt_an_clctr_entries, dt_an_clctr_entries[is_couple==1]) %>% 
        .[, ace_nbr := as.integer(substring(an_entry_id, 4, nchar(an_entry_id)))] %>%
        .[, an_collector_person_id := paste0("ACPE", ace_nbr, letters[1:.N]), an_entry_id] %>%
        .[order(-is_couple, ace_nbr)] %>%
        .[, .(an_entry_id, an_collector_person_id, clctr_name, is_couple)]

    dt_an_person_prep_ff <- fread(ARTNEWS_COLLECTOR_PERSON_FILE_ORG)

    if (fnrow(dt_an_person_prep[!dt_an_person_prep_ff, on = "an_collector_person_id"]) > 0) {
        stop("not all an_collector_person_ids that are follow from ARTNEWS_COLLECTOR_ENTRIES_FILE",
             "(via couple indicator) have been checked in ARTNEWS_COLLECTOR_ENTRIES_FILE. ",
             "make sure they are covered, e.g. by ",
             "adding manually entries to  ARTNEWS_COLLECTOR_PERSON_FILE_ORG or re-running the fwrite-call ",
             "exporting that to org-table, making changes there, and exporting that again.",
             "make sure tho that this doesn't yeet other or even more an_collector_person_id s.")
    }

    ## fwrite is uncommented -> has to be manually enabled if error arises
    ## fwrite(dt_an_person_prep, ARTNEWS_COLLECTOR_PERSON_FILE)
    
}



#' check whether any collector is a subset of another collector, e.g. "John Meyer" is a subset of "John W. Meyer"
#' only compares collectors who share at least one element
#' @param dt_acpe_w_id dt with a bunch of stuff, including clctr_name, which is all that matters
t_subset <- function(dt_acpe_w_id1) {
    
    ## library(parallel)
    library(stringi) ## 

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

    if (fnrow(dt_subset_prep[subset==T & (clctr_name != clctr_name2)]) > 1) {
        stop("not all subsets are dealt with")}
        
    ## could maybe shave of another few hundreds of seconds by stringi grepl matching (in scrap), but not worth it
    
        
}


#' generates the ARTNEWS_APECPRN_FILE: here similar names are harmonized
#' it is later read in to rename deviant names, to get APE IDs to refer to the same person
#' this is not supposed to be run again: It has been run once after string similarity calculation, the results of which have been written to file and manually checked
#' a later subset check (John William Meyer -> John Meyer) has led to a bunch of additions to ARTNEWS_APECPRN_FILE
#' @param dt_acpe_w_id1 the dt with AE, ACPE, APE ids, used for string similarity and name-word subset checks
#' @param ARTNEWS_APECPRN_FILE the file to write the similar sounding names to
gwd_apecprn <- function(dt_acpe_w_id1, ARTNEWS_APECPRN_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## this should not be in package declaration: stringdist used only once for setup?
    ## maybe add later when I have the maintenance added
    library(stringdist, include.only = c("stringdist", "stringdistmatrix"))
    
    
    ## prep for pairwise comparison
    dt_strindist_prep <- dt_acpe_w_id1[, .(an_person_id, clctr_name)] %>% funique


    dt_stringdist_mat <- stringdistmatrix(dt_strindist_prep[, clctr_name], dt_strindist_prep[, clctr_name],
                                          useNames = "strings") %>% adt %>% 
                         .[, clctr_name := dt_strindist_prep[, clctr_name]]

    dt_stringdist_long <- melt(dt_stringdist_mat, id.vars = "clctr_name", variable.name = "clctr_name2",
                               value.name = "dist")

    dt_stringdist_close <- dt_stringdist_long[(clctr_name != clctr_name2) & dist < 5][order(dist)] 

    ## get collector entries as list
    ## get ACEs of clctr_name
    dt_ace_left <- dt_acpe_w_id1[dt_stringdist_close, on = "clctr_name"] %>%
        .[, .(ace_left = paste(sort(unique(an_entry_id)), collapse = "--")), clctr_name]

    ## get ACEs of clctr_name2
    dt_ace_right <- dt_acpe_w_id1[dt_stringdist_close, on = .(clctr_name = clctr_name2)] %>% 
        .[, .(ace_right = paste(sort(unique(an_entry_id)), collapse = "--")), clctr_name]


    
    
    
    ## get all relevant comparisons
    dt_ape_cprn <- dt_stringdist_close[dt_ace_left, on = "clctr_name"] %>% # join left
        .[dt_ace_right, on = .(clctr_name2 = clctr_name)] %>% # join right
        .[!ace_left == ace_right & clctr_name > clctr_name2] %>% ## yeet couples and reverse
        .[order(dist)] %>%
        .[, `:=`(is_same_APE = 0, correct_name = "")]
    
    dt_ape_cprn[grepl("Vogel", clctr_name)]

    stop("this function is not supposed to be run")
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



        
## ONLY RUN ONCE:
## this duplicates couple rows so that manual deletion is easier
## names are not standardized for automatic detection of both names
## t_gwd_artnews_collector_person()

#' add the APE ID to ARTNEWS_COLLECTOR_PERSON_FILE
#' 
mwd_artnews_collector_person <- function(
    ARTNEWS_COLLECTOR_PERSON_FILE_ORG = PMDATA_LOCS$ARTNEWS_COLLECTOR_PERSON_FILE_ORG,
    ARTNEWS_APECPRN_FILE = PMDATA_LOCS$ARTNEWS_APECPRN_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;


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
    dt_acpe_w_id1 <- copy(dt_acpe)[dt_simpledups, an_person_id := i.an_person_id, on = "clctr_name"] %>%
        .[, an_person_id := paste0("APE", an_person_id)]

    ## not actually run, but keep this line here to indicate that gwd_apecprn generates ARTNEWS_APECPRN_FILE, which
    ## is then manually checked for duplicate persons
    ## ## gwd_apecprn(dt_acpe_w_id1, ARTNEWS_APECPRN_FILE)
    
    
    if (nrow(dt_apce) != nrow(dt_acpe_w_id1)) {stop("number of rows not the same")}
    return(dt_acpe_w_id1)
}


## PMDATA_LOCS <- gc_pmdata_locs()


## mwd_artnews_collector_person()
## fread(PMDATA_LOCS$ARTNEWS_COLLECTOR_ENTRIES_FILE)




## stuff to move to overall import function
## - t_gwd_artnews_clctr()
## - t_gwd_artnews_collector_person()
