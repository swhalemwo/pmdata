## * scrap

## ** trying to rewrite XML parsing in R, but XML handling is a mess
## library(XML) has somewhat ok API, but still requires xml2 to delete metadata node to run xmlApply

## gd_mow_prep <- function(MOW_FILE = PMDATA_LOCS$MOW_FILE) {
##     ## rename later
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
##     1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;


##     library(XML)
##     library(xml2)
    
    
##     x <- read_xml(MOW_FILE)
##     xx <- xmlParse(x)
##     str(xx)

##     ## kinda works
##     kk <- xml_find_all(x, "directory_record")

##     ## can look at one at the time
##     kk[[3]]
##     ## can look at its structure
##     xml_structure(kk[[3]])

##     nn <- kk[[3]]

##     ## find all doesn't care about subsetting, still searches the entire tree,
##     ## any xml_find_* command doesn't care about subsetting -> can easily crash R
##     xml_find_all(kk[[3]], "//place")
##     ## even when assigned to separate object 
##     xml_find_all(nn, "//place")

##     xml_attr(nn, "id")
##     xml_attr(kk, "id")

##     xml_find_all(kk, "id")

##     ## xml_text(

##     jj <- xmlToList(xx)

##     set_addresses <- xml_find_all(x, "//dir_address")
##     ## 58735 addresses, but only 55247 entries in kk

##     ## doesn't seem like the addresses still has an ID 
##     xml_attr(set_addresses, "id")
##     xml_text
    
##     ## xml_children can apply to subset
##     xml_children(set_addresses[[1]])

##     xml_structure(set_addresses[[13000]])
##     xml_child(set_addresses[[1]])
##     xml_contents(set_addresses[[1]])

##     ## getting information from child node is taking several seconds: probably has to crawl up and down the tree
##     xml_text(xml_child(set_addresses[[1]], "country_short"))
##     ## probably first gets the id, then all the texts, and then the correct text

##     xml_attrs(set_addresses[[1]])
##     xml_name(set_addresses[[1]])

##     ## with xml_path i can kinda get up 
##     xml_path(set_addresses[[3000]])

##     ## actually maybe I can use parents to get the ID: get all the addresses, and then get the ID via parents

##     xml_parents(set_addresses[[1]])

##     xml_name(fax_set[[2]])

##     fax_set <- xml_find_all(x, "//fax")
##     fax_parents_set <- xml_parent(xml_parent(xml_parent(fax_set[[1]])))

##     xml_text(fax_parents_set, "id") # doesn't work
##     xml_attr(fax_parents_set, "id")

##     dt_phone <- data.table(fax_nbr = xml_text(fax_set, "phone_number"),
##                            id = xml_attr(xml_parent(xml_parent(xml_parent(fax_set))), "id"))

##     dt_addresses <- data.table(country = xml_text(xml_child(set_addresses, "country_short")))
##     ## i think to access the text quickly it needs to be same level

##     set_country_short <- xml_find_all(x, "///country_short")

##     dt_addresses <- data.table(country = xml_text(set_country_short, "country_short"),
##                                addr_type = xml_attr(xml_parent(set_country_short), "type"),
##                                ID = xml_attr(xml_parent(xml_parent(xml_parent(set_country_short))), "id"))
##     ## bottom up doesn't work: some museums have multiple addresses, but parent nodes get deduplicated
##     ## -> xml_parents will yeet multiple IDs -> maybe I can go top-down over children?

##     xml_children(kk, "institution type")
##     xml_child(kk[[1]], search =1) %>% xml_child(search = 1) %>% xml_text ##  asssessing name 
    
##     ## this seems to work
##     xml_child(kk[[1]], search =1) %>% xml_child("inst_name_info") %>% xml_text

##     ## hmm now how to deal with addresses
##     xml_child(kk[[1]], search=1) %>% xml_child('dir_address type="main"') # no work
##     xml_child(kk[[1]], search=1) %>% xml_child('<dir_address type="main">') # no work
##     xml_child(kk[[1]], search=1) %>% xml_child('dir_address') %>% xml_child("country_short") %>% xml_text

    

##     dt_loc <- data.table(ID = xml_attr(kk, "id"),
##                          loc = xml_children(kk) %>% xml_child('dir_address') %>%
##                              xml_child("country_short") %>% xml_text)

##     lkk <- as.list(kk)
##     lkk <- lapply(kk, as.list)

##     xml_structure(lkk[[44]])
##     xml_child(lkk[[44]], "additional")
##     xml_children(lkk[[44]])
##     kk[[44]]

##     ## just use XML, not xml2

    
    
##     rootnode <- xmlRoot(xx)
##     xmlCDataNode
    
##     ## works pretty well tbh
##     rootnode[[3]][["institution"]][["dir_address"]][["country_short"]] %>% xmlValue

##     ## but still need to
##     ## - get text: xmlValue
##     ## - run logic

##     xmlApply

##     xmlTextNode
##     ?XML
##     xmlRoot
    
##     ## huh works with chuck, nice
##     chuck(rootnode[[88]], "institution", "dir_address", "country_short") %>% xmlValue

##     xmlSApply(rootnode, \(x) chuck(x, "institution", "dir_address", "country_short"))
##     oo <- xmlChildren(rootnode)

##     lenx <- xmlSApply(rootnode, xmlSize)
##     leny <- map(2:len(kk), ~xmlSize(rootnode[[.x]]))

##     for (i in 2:len(kk)) {
##         print(xmlSize(rootnode[[i]]))
##     }

##     jj <- xmlApply(rootnode, gl_mow)

    
##     kk_parsed <- xmlParse(kk) # nodeset can't be parsed

##     ## remove seems to be by reference
##     xml_metadata_yeeted <- xml_remove(xml_find_all(x, "metadata"))
##     mow_entries_parsed <- xmlParse(x)

##     somel <- xmlApply(xmlRoot(mow_entries_parsed), gl_mow) %>% rbindlist
    

    

    

## }


## gl_mow <- function(mow_node) {
    

##     list(
##         ID = xmlAttrs(mow_node)[["id"]],
##         country = fifelse("dir_address" %in% names(chuck(mow_node, "institution")), "address there", "no address")

##         ## chuck(mow_node, "institution", "dir_address", "country_short")
##     )
    
## }





## gd_mow_prep(gc_pmdata_locs()$MOW_FILE)

## ** MOW checks

## try chatgpt LUL
## head(dt_match_check1,16)
## dt_match_check1[, .(name)] %>% print(n=200)

## seems to work somewhat, but would need validation..
## change order randomly
## batches: context window is too short for entire dataset -> would have to aggregate over time
## or get chatgpt4 LUL


## check right joins
## dt_match_check2 <- dt_match[dt_match_old, on = "name"]
## dt_match_check2[!is.na(MOW_ID_old) & MOW_ID_old != MOW_ID]
## dt_match_check2[MOW_ID != "nomatch" & is.na(MOW_ID_old) & MOW_ID != MOW_ID_old]

## ## check other direction
## dt_match_check3 <- dt_match_old[dt_match, on = "name"]
## dt_match_check3[!is.na(MOW_ID_old) & MOW_ID_old != MOW_ID]
## dt_match_check3[!is.na(MOW_ID) & MOW_ID != MOW_ID_old]

## ## hmm this just doesn't work because so little is being matched: most entries are just NA


## library(fuzzyjoin)
## stringdist_full_join(dt_match, dt_match_old, by = "name", max_dist =3) %>% adt %>% tail
## fuzzyjoin also not helpful probably overmatches

## ## ** from imp_artnews: slow subset checks

##     ## old slow version: group_by is just a mess, much slowdown
##     ## ## subsetting: whether all terms of one are terms of other, e.g. "John Meier" and "John Meyer Smith"
##     ## ## for now, these entries are added manually to ARTNEWS_APECPRN_FILE
##     ## t1 <- Sys.time()
##     ## dt_subset <- dt_stringdist_long %>% copy() %>%  # head(n=1200000) %>% 
##     ##     .[, `:=`(terms1 = as.list(strsplit(clctr_name, split = " ")),
##     ##              terms2 = as.list(strsplit(as.character(clctr_name2), split = " ")))] %>% 
##     ##     ## .(clctr_name, clctr_name2)] %>%
##     ##     .[, subset := map2_int(terms1, terms2, ~all(.x %in% .y))]
##     ## ## just one direction is fine since dt_stringdist long contains both a,b and b,a
##     ## t2 <- Sys.time()
##     ## t2-t1


##     ## try to speed up the matching by first splitting names, using equality checks, and aggregate afterwards
##     ## doesn't work: aggregation is expensive:
##     ## even with collapse I don't think I can get it to under 2 secs
    
##     ## presplit

##     ## dt_subset %>% .[subset==1 & dist != 0] %>% print(n=50)
##     ## if (fnrow(dt_subset_prep[subset==1 & dist != 0]) > 0)     


##     ## maybe can filter out unique terms? can filter out everybody with one unique term
##     dt_setops <- copy(dt_clctrname_split_prep)[, nbr_occs := .N, clctr_name_split] %>%
##         .[, `:=`(has_unique_term = any(nbr_occs==1), all_unique = all(nbr_occs==1)) , clctr_name]
##         ## .[, .(clctr_name, has_unique_term)] %>% funique %>% .[, .N, has_unique_term] # 397
##         ## .[, .(clctr_name, all_unique)] %>% funique %>% .[, .N, all_unique] 173
##     ## there are only 400 who don't have unique term
##     ## but they could be subset of collectors with unique terms -> only doing 40% of comparisons should be enough
##     ## there are another 173 who have all unique terms
    
##     ## reduce number of comparisons
##     dt_stringdist_long[!dt_setops[has_unique_term==T, .(clctr_name = funique(clctr_name))], on = "clctr_name"] %>%
##         .[!dt_setops[all_unique==T, .(clctr_name2 = funique(clctr_name))], on = "clctr_name2"]
##     ## can bring it down to 30% necessary

    

    

##     ## expand twice
##     t4 <- Sys.time()
##     dt_namesplit_prep1 <- dt_cltrname_split_prep[dt_stringdist_long[clctr_name != clctr_name2],
##                                                  on = "clctr_name", allow.cartesian = T] %>% funique
##     t5 <- Sys.time()
##     dt_namesplit_stringi <- copy(dt_namesplit_prep1) %>%
##         .[, jj := stri_extract_first_fixed(clctr_name2, pattern = clctr_name_split)]
##     t6 <- Sys.time()
##     t6-t5

##     ## aggregation in data.table is "kinda" slow
##     ## %>% .[, .(member = all(!is.na(jj))), .(clctr_name, clctr_name2)]
    
    
    
##     ## aggregating with collapse: but still takes a while

##     ## aggregating without grouping 
##     ## catFUN = \(x) len(x))
    
##     ## aggregating with grouping: 
##     t7 <- Sys.time()
    
##     ## aggregating with grouping: 1.3
##     dt_namesplit_stringi_gpd <- fgroup_by(dt_namesplit_stringi, c("clctr_name", "clctr_name2"))
##     collapg(dt_namesplit_stringi_gpd, cols = "jj", catFUN = \(x) all(!is.na(x)), parallel = T, mc.cores = 2)

##     ## aggregating without grouping: 1.75
##     ## collap(dt_namesplit_stringi, ~clctr_name + clctr_name2, cols = "jj",
##     ##        catFUN = \(x) all(!is.na(x)), parallel = T, mc.cores = 4, return.order = F)
##     t8 <- Sys.time()
##     t8-t7
    
    
##     ## expanding twice

##     dt_namesplit_prep2 <- dt_cltrname_split_prep[, .(
##         clctr_name2_split = clctr_name_split, clctr_name2 = clctr_name)][
##         dt_namesplit_prep1, on = "clctr_name2", allow.cartesian=T]
##     t5 <- Sys.time()
    
    
##     ## aggregate expanded table
##     ## check itself is fine, but aggregating 1.2m groups takes some seconds

##     dt_namesplit_prep2 %>% # head(5005)
##         ## setkey(clctr_name, clctr_name2, clctr_name_split) %>% 
##         ## .[clctr_name != clctr_name2] %>%
##         ## .[, .(clctr_name, clctr_name_split, clctr_name2_split, clctr_name2)] %>%
##         ## .[, clctr_name := factor(clctr_name, levels =funique(clctr_name))] %>% # .[order(clctr_name)] %>% 
##         ## .[clctr_name_split == clctr_name2_split, has_match := T, verbose = T]
##         ## .[, has_match0 := clctr_name_split == clctr_name2_split]
##         .[, has_match1 := any(clctr_name_split == clctr_name2_split),
##           .(clctr_name, clctr_name2, clctr_name_split)]
##     ## t5 <- Sys.time()
##     ## t5-t4


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

