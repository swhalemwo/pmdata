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
