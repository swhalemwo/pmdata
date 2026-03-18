library(data.table)
library(jtls)
library(purrr)
library(igraph)
library(terra)
library(tidygeocoder)

PMDATA_LOCS <- gc_pmdata_locs()

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))

gd_tanp_city_geo <- function(FILE_TANP_CITY_ID = PMDATA_LOCS$FILE_TANP_CITY_ID) {
    #' read stuff from file
    dt_tanp_city_geo <- fread(FILE_TANP_CITY_ID)
    return(dt_tanp_city_geo)
}


gd_tanp_cbn <- function() {


    ## more systematic approach
    ## read in data
    dt_tanp_17 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_17.csv") %>%
        .[, map(.SD, trimws)] %>%
        .[, id := paste0("tanp17_", 1:.N)] %>% # drop duplicate MMCA
        .[!(museum == "National Museum of Modern and Contemporary Art"  & total == "1,218,504")] %>%
        ## rename one SAAM (SAAM already there on its own)
        .[museum == "SAAM/National Portrait Gallery", museum := "National Portrait Gallery"]
          
        



    dt_tanp_18 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_18.csv") %>%
        .[, `:=`(city = stringr::str_to_title(city), id = paste0("tanp18_", 1:.N))]


    dt_tanp_19 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_19.csv") %>%
        .[, `:=`(city = stringr::str_to_title(city), id = paste0("tanp19_", 1:.N))]
        
    
    dt_tanp_20 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_20.csv") %>%
        setnames(new = c("total", "museum", "city", "perc_chng_19", "days_closed_pandemic")) %>% 
        .[, id := paste0("tanp20_", 1:.N)] 

    dt_tanp_21 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_21.csv") %>%
        setnames(new = c("total", "museum", "city", "perc_chng_20", "perc_chng_19")) %>% 
        .[, id := paste0("tanp21_", 1:.N)] 

    

    dt_tanp_22 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_22.csv") %>%
        setnames(new = c("total", "museum", "city", "perc_chng_21", "perc_chng_19")) %>%
        .[, id := paste0("tanp22_", 1:.N)]


    dt_tanp_23 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_23.csv") %>%
        setnames(new = c("total", "museum", "city", "perc_chng_22", "perc_chng_19")) %>%
        .[, id := paste0("tanp23_", 1:.N)]

    dt_tanp_24 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_24.csv") %>%
        setnames(new = c("total", "museum", "city", "perc_chng_23", "perc_chng_19")) %>%
        .[, id := paste0("tanp24_", 1:.N)]

    ## combine
    dt_tanp_cbn <- map(list(dt_tanp_17, dt_tanp_18,
                            dt_tanp_19, dt_tanp_20, dt_tanp_21, dt_tanp_22, dt_tanp_23, dt_tanp_24),
                       ~.x[, .(id, museum, city, total)]) %>%
        rbindlist %>%
        .[museum == "teamLab Borderless: MORI Building", city := "Tokyo"] %>% # manual fixes
        .[, museum := trimws(gsub("\\*|†", "", museum))] %>%
        .[, total := as.integer(gsub(",", "", total))]
    
    ## smithsonian fix: sometimes grouped together with national portrait gallery
    dt_smithies <- dt_tanp_cbn[grepl("Smithsonian|SAAM", museum) & grepl("Portrait", museum)] %>%
        .[, .(museum = trimws(unlist(tstrsplit(museum, "/")))), .(city, total, id)]

    dt_smithies_new <- dt_smithies %>%  copy %>% 
        .[, nbr := 1:.N, id] %>% 
        .[, `:=`(id = paste0(id, letters[nbr]), total = total/2)] %>% # split audience
        .[, nbr := NULL]

    dt_tanp_cbn2 <- rbind(dt_tanp_cbn[id %!in% dt_smithies[, id]], dt_smithies_new)

    
    if (dt_tanp_cbn2[city == "", .N] > 0) {stop("not all museums have cities")}

    return(dt_tanp_cbn2)


}


gd_tanp_city <- function(dt_tanp_cbn) {
    #' get all the cities

    dt_tanp_city_unq <- dt_tanp_cbn[, .(city = unique(city))] # [, ID := paste0("city_", 1:.N)]

    ## need to deal with cities that have no ID yet
    dt_tanp_city_ff <- gd_tanp_city_geo()
    
    # assign new IDs: start where we have stopped previously (ff)
    dt_tanp_city_new <- dt_tanp_city_unq[!dt_tanp_city_ff, on = "city"] %>%
        .[, ID := paste0("city_", (dt_tanp_city_ff[, .N]+1):((dt_tanp_city_unq[, .N])))] 

    if (dt_tanp_city_new[, .N] > 0) { gwd_geocode_tanp_city(dt_tanp_city_new = dt_tanp_city_new)}

    ## get city geo again after potentially adding new ones
    dt_tanp_city_geo <- gd_tanp_city_geo()

    
    ## check links now
    dt_tanp_city_links_new <- gd_tanp_city_link_candidates(dt_tanp_city_geo)
    dt_tanp_city_links_ff <- gd_tanp_city_links()

    dt_tanp_city_links_to_check <- dt_tanp_city_links_new[!dt_tanp_city_links_ff, on = .(ID_i, ID_j)]

    if (dt_tanp_city_links_to_check[, .N] > 0) {
        fwrite(dt_tanp_city_links_to_check)
        stop("check city links")
    }

    ## correct labels here: first read links again, where actually 1
    
    dt_tanp_city_links_ff <- gd_tanp_city_links()[b_same == 1]
    
    
    ## then update join
    cnt_unq <- dt_tanp_city_geo[, uniqueN(ID)]
    
    ## iterative updates
    keep_going <- T
    while(keep_going) {
        print(cnt_unq)
        dt_tanp_city_geo[dt_tanp_city_links_ff, ID := i.ID_j, on = .(ID = ID_i)]
        keep_going <- cnt_unq != dt_tanp_city_geo[, uniqueN(ID)]
        cnt_unq <- dt_tanp_city_geo[, uniqueN(ID)]        
    }

    ## dt_tanp_city_geo[grepl("New York", city), .(city, ID, name)] 

    
    ## another update join for new city name
    dt_tanp_city_geo[, city_new := city] %>%
        .[dt_tanp_city_links_ff, city_new := i.new_name, on = .(ID = ID_j)]

    ## dt_tanp_city_geo[grepl("New York", city), .(city, ID, name, city_new)] 

    ## dt_tanp_city_geo[grepl("Washington", city), .(city, ID, city_new)]
    ## dt_tanp_city_geo[grepl("Paulo|Washington|Krak", city), .(city, ID, city_new)][order(ID)]
    ## dt_tanp_city_geo %>% copy %>% .[, N:= .N, ID] %>% .[N > 1] %>% .[order(-N, ID), .(city, ID, N)]
    
    ## dt_tanp_city_geo[dt_tanp_city_links_ff, `:=`(ID = i.ID_i, city_new = i.new_name) , on = .(ID = ID_j)]
    ## dt_tanp_city_geo[grepl("Kr|Pau|Washington", city), .(city, ID)]
    ## dt_tanp_city_geo[grepl("Kr|Pau|Washington", city), .(city, city_new, ID)]

    return(dt_tanp_city_geo)

}    


## ## look at city info
## 

## dt_tanp_city_cpr <- merge(dt_tanp_city[, .(city1 = city, id_city1 = ID, mrg = "x")],
##                           dt_tanp_city[, .(city2 = city, id_city2 = ID, mrg = "x")],
##                           by = "mrg", allow.cartesian = T) %>% .[city1 > city2] %>% .[, mrg := NULL]

## ## get features
## dt_tanp_city_wfeat <- gd_grid_wfeat(dt_tanp_city_cpr, name1 = "city1", name2 ="city2",
##                                     dt_qmod, l_mod_noq = "jw") %>%
##     .[, strdist_cosine_avg := rowMeans(.SD), .SDcols = patterns("strdist_cosine")] %>%
##     .[, strdist_jaccard_avg := rowMeans(.SD), .SDcols = patterns("strdist_jaccard")] %>%
##     .[, strdist_avg_all := rowMeans(.SD), .SDcols = c("strdist_cosine_avg", "strdist_jaccard_avg", "strdist_jw")]

## dt_tanp_city_wfeat[order(strdist_avg_all)]


gwd_geocode_tanp_city <- function(FILE_TANP_CITY_ID = PMDATA_LOCS$FILE_TANP_CITY_ID, dt_tanp_city_new) {
    #' geocode new cities and write them to file

    ## get existing cities
    ## dt_tanp_already_geo <- gd_tanp_city_geo()

    ## get new cities
    ## dt_new_cities <- dt_tanp_cbn[!dt_tanp_already_geo, on = "city"]

        ## geocode(dt_tanp_city[1:3], city = city, full_results = T, method = "osm")
    ## geocode(dt_tanp_city[grepl("Pau", city)], city = city, full_results = T, method = "osm")

    ## geocode new cities
    dt_newcities_geo <- geocode(dt_tanp_city_new, city = city, full_results = T, method = "osm") %>% adt

    wtf <- readline("write to file?")

    if (wtf == "y") {
        fwrite(dt_newcities_geo %>% adt %>% copy %>% .[, boundingbox := NULL], FILE_TANP_CITY_ID, append = T)
    }

    

}




gd_tanp_city_links <- function(FILE_TANP_CITY_LINKS= PMDATA_LOCS$FILE_TANP_CITY_LINKS) {
    fread(FILE_TANP_CITY_LINKS)
}

gd_tanp_city_link_candidates <- function(dt_tanp_city_geo) {
    #' get candidates that need to be checked
    dt_tanp_city_vect <- terra::vect(dt_tanp_city_geo[, .(ID, long, lat)], geom = c("long", "lat"), crs = "WGS84")
    mat_dist <- terra::distance(dt_tanp_city_vect)

    dt_tanp_city_match_id <- i1d2d(which(mat_dist < 1000), mat_dist) %>% adt %>% 
        .[, id_link := 1:.N] %>%
        melt(id.vars = "id_link", value.name = "index")
        ## .[order(id_link, -index)]
    
    merge(dt_tanp_city_geo %>% copy %>% .[, index := 1:.N], dt_tanp_city_match_id, by = "index") %>%
        dcast(id_link ~ variable, value.var = c("ID", "city"))
        
}


gd_tanp_muci_ff <- function(FILE_TANP_MUCI_ID = PMDATA_LOCS$FILE_TANP_MUCI_ID) {
    fread(FILE_TANP_MUCI_ID)
}







gd_tanp_muci <- function(dt_tanp_wcid) {

    dt_tanp_muci_geo_ff <- gd_tanp_muci_ff()
    

    ## ## museum geocoding: use museum-city
    ## dt_tanp_muci_new <- dt_tanp_wcid[, .(museum, city_new, id_city)] %>% unique %>%
    ##     .[, muci := paste0("muci_", 1:.N)] %>%
    ##     .[, .(muci, museum, city_new, id_city, addr = paste0(museum, ", ",city_new))]

    ## get new IDs
    dt_tanp_muci_new <- dt_tanp_wcid[, .(museum, city_new, id_city)] %>% unique %>%
        .[!dt_tanp_muci_geo_ff, on = .(museum, city_new, id_city)] %>%
        .[, muci := paste0("muci_", (dt_tanp_muci_geo_ff[, .N]+1):(dt_tanp_muci_geo_ff[, .N] + .N))] %>%
        .[, .(museum, city_new, muci, id_city, addr = paste0(museum, ", ",city_new))]

    ## dt_tanp_muci_new[, .(museum, city_new, id_city)] %>% print(n=80)


    ## do the geocoding of new ones if neccessary
    if (nrow(dt_tanp_muci_new) > 0) {
        dt_tanp_muci_geo <- geocode(dt_tanp_muci_new, address = addr, full_results = T, method = "google") %>% adt

        
        ## manual start
        ## dt_muci_cbn <- dt_tanp_muci_geo %>% copy %>%
        ##     .[, `:=`(address_components = NULL, navigation_points = NULL, types = NULL)]


        dt_muci_cbn <- rbind(dt_tanp_muci_geo_ff, dt_tanp_muci_geo, fill = T) %>%
            .[, `:=`(address_components = NULL, navigation_points = NULL, types = NULL)]
        
        ## print(dt_tanp_muci_geo[, .(museum, city_new, muci, id_city, lat, long, addr)])

        wtf <- readline("write to file?")
        if (wtf == "y") {
            ## fwrite(dt_tanp_muci_geo2, PMDATA_LOCS$FILE_TANP_MUCI_ID, append = T)
            fwrite(dt_muci_cbn, PMDATA_LOCS$FILE_TANP_MUCI_ID, append = F)
        }
        
    }

    ## get from file again
    dt_tanp_muci_geo <- gd_tanp_muci_ff()
    ## dt_tanp_muci_geo[grepl("Metropol", museum)]
    ## dt_tanp_muci_geo[grepl("Museum of China", museum)]
    ## dt_tanp_wcid[grepl("Museum of China", museum)]
    ## dt_tanp_muci_geo[muci %in% c("muci_116","muci_112"), .(lat, long)] %>% adf
    
    dt_muci_links <- gd_muci_links(dt_tanp_muci_geo)[b_same == 1] # get links to update

    ## dt_tanp_muci_geo[grepl("Tomie", museum)]
    ## dt_tanp_muci_geo[grepl("Paul", city_new)] %>% names

    cnt_unq <- dt_tanp_muci_geo[, uniqueN(muci)]
    keep_going <- T
    cntr <- 0
    dt_tanp_muci_fixed <- dt_tanp_muci_geo %>% copy
    dt_tanp_muci_fixed[grepl("SAAM", museum)]


    ## iteratively update all muci ids
    while(keep_going) {
        
        dt_tanp_muci_fixed <- dt_tanp_muci_fixed %>% copy %>%
            .[dt_muci_links, muci := i.muci2, on = .(muci = muci1)]
        
        keep_going <- cnt_unq != dt_tanp_muci_fixed[, uniqueN(muci)]
        cnt_unq <- dt_tanp_muci_fixed[, uniqueN(muci)]
        cntr <- cntr + 1
        print(cnt_unq)
        if (cntr == 20) {stop("stop")}
    }

    dt_tanp_muci_fixed[grepl("SAAM", museum)] # seems to work

    ## dt_tanp_muci_fixed[, .N, muci][order(-N)]
    ## dt_tanp_muci_fixed[grepl("Smithsoni|National Portrait", museum) & grepl("Washington", city_new)]

    ## new names: use first occurence for consistency
    dt_tanp_muci_newname <- dt_tanp_muci_fixed[, head(.SD,1), muci]
    dt_tanp_muci_fixed[dt_tanp_muci_newname, museum_new := i.museum, on = "muci"]

    dt_tanp_muci_fixed[grepl("SAAM", museum), .(muci, museum_new, city_new, id_city)] # seems to work

    ## dt_tanp_muci_fixed[muci == "muci_216"]
    ## dt_tanp_muci_fixed[muci == "muci_21"]

    return(dt_tanp_muci_fixed)

}



gd_muci_links_geo <- function(dt_tanp_muci_geo) {
    "get muci links based on geographical distance: within 100m"

    dt_tanp_muci_vect <- terra::vect(dt_tanp_muci_geo[, .(muci, long, lat)], geom = c("long", "lat"), crs = "WGS84")
    mat_dist <- terra::distance(dt_tanp_muci_vect)

    dt_tanp_muci_match_id <- i1d2d(which(mat_dist < 100), mat_dist) %>% adt %>% 
        .[, id_link := 1:.N] %>%
        melt(id.vars = "id_link", value.name = "index") %>%
        .[order(id_link, -index)]

    merge(dt_tanp_muci_geo %>% copy %>% .[, index := 1:.N], dt_tanp_muci_match_id, by = "index") %>%
        dcast(id_link ~ variable, value.var = c("muci", "museum")) %>%
        .[, .(id_link, muci1 = muci_i, muci2 = muci_j, museum1 = museum_i, museum2 = museum_j, src = "geo")]

}
      

gd_muci_links_stringdist <- function(dt_tanp_muci_geo) {
    #' get string similarity of muci in same city
    


    ## cross-join into long
    dt_tanp_cpr <- merge(dt_tanp_muci_geo[, .(id1 = muci, museum1 = museum, id_city)],
                         dt_tanp_muci_geo[, .(id2 = muci, museum2 = museum, id_city)], by = "id_city",
                         allow.cartesian = T) %>%
        .[, `:=`(id1_int = as.integer(gsub("muci_", "", id1)), id2_int = as.integer(gsub("muci_", "", id2)))] %>%
        .[id1_int > id2_int]
    


    ## l_cols_strdist_toobig <- paste0("strdist_", c(paste0("qgram_", 1:5), "osa", "lv", "dl", "lcs"))

    ## only use standardized string distances: cosine, jaccard, jw
    dt_qmod <- expand.grid(mod = c("cosine", "jaccard"), q = 1:5, stringsAsFactors = F) %>% adt

    dt_tanp_wfeat <- gd_grid_wfeat(dt_tanp_cpr, "museum1", "museum2", dt_qmod = dt_qmod, l_mod_noq = "jw") %>% 
        .[, strdist_cosine_avg := rowMeans(.SD), .SDcols = patterns("strdist_cosine")] %>%
        .[, strdist_jaccard_avg := rowMeans(.SD), .SDcols = patterns("strdist_jaccard")] %>%
        .[, strdist_avg_all := rowMeans(.SD), .SDcols = c("strdist_cosine_avg", "strdist_jaccard_avg",
                                                          "strdist_jw")]
    ## .[, .SD := NULL,  .SDcols = patterns("strdist_cosine")]

    
    return(dt_tanp_wfeat[between(strdist_avg_all, 0, 0.3, incbounds = F),
                         .(id_link = 1:.N, muci1 = id1, muci2 = id2, museum1, museum2, src = "stringdist")])
    

}

gd_muci_links_ff <- function(FILE_TANP_MUCI_LINKS = PMDATA_LOCS$FILE_TANP_MUCI_LINKS) {
    fread(FILE_TANP_MUCI_LINKS)
}

gd_muci_links <- function(dt_tanp_muci_geo)  {
    #' get muci links
    dt_muci_links_geo <- gd_muci_links_geo(dt_tanp_muci_geo)
    dt_muci_links_stringdist <- gd_muci_links_stringdist(dt_tanp_muci_geo)
    ## dt_muci_links_stringdist %>% print(n=80)

    dt_links_muci <- rbind(dt_muci_links_geo,dt_muci_links_stringdist) %>%
        .[, .(src = paste0(src, collapse = ",")), .(muci1, muci2, museum1, museum2)]
        ## .[, b_same := 0]

    dt_muci_links_ff <- gd_muci_links_ff()

    dt_links_muci_new <- dt_links_muci[!dt_muci_links_ff, on = .(muci1, muci2)]

    if (nrow(dt_links_muci_new) > 0) {
        fwrite(dt_links_muci_new %>% copy %>% .[, b_same := -1])
        stop("check muci links")
    }
    
    ## fwrite(dt_links_muci_new, PMDATA_LOCS$FILE_TANP_MUCI_LINKS, append = T)
    
    return(dt_muci_links_ff)
}


## ** main

dt_tanp_cbn <- gd_tanp_cbn()
dt_tanp_city <- gd_tanp_city(dt_tanp_cbn)

dt_tanp_city[grepl("New York", city), .(ID, city, city_new, lat, long)]

## with city id 
dt_tanp_wcid <- merge(dt_tanp_cbn, dt_tanp_city[, .(city, id_city = ID, city_new)], by = "city")

dt_tanp_wcid[grepl("Kr|Pau|Washing", city)] %>% print(n=80)

dt_tanp_wcid[grepl("Smithsonian", museum)]
dt_tanp_wcid[grepl("National Portrait", museum) & grepl("Washington", city)]

dt_tanp_wcid[grepl("/", museum)]

dt_tanp_muci <- gd_tanp_muci(dt_tanp_wcid)
dt_tanp_wcid[grepl("museum of modern art", museum, ignore.case= T) & grepl("new york", city_new, ignore.case = T)]
dt_tanp_muci[grepl("Smithsonian",museum), .(muci, museum, id_city, museum_new)]
dt_tanp_muci[grepl("National Portrait", museum) & grepl("Washington", city_new)]


dt_tanp_muyr <- merge(dt_tanp_wcid, dt_tanp_muci[, .(muci, id_city, museum, museum_new)],
                      by = c("museum", "id_city")) %>%
    .[, year := as.integer(gsub("tanp(\\d+)_.*", "\\1", id))]

dt_tanp_muyr[grepl("SAAM", museum_new), .(muci)]
dt_tanp_muyr[grepl("National Portrait", museum) & grepl("Washington", city_new)]


dt_tanp_muyr %>% ggplot(aes(x = year, y = total, color = muci)) +
    geom_line(show.legend = F) +
    scale_y_continuous(trans = scales::log10_trans()) +
    geom_point(show.legend = F)

dt_tanp_muyr[grepl("national museum", museum, ignore.case = T)] %>% print(n=80)

dt_tanp_muyr[, .N, .(muci,year)][N > 1]
dt_tanp_muyr[muci %in% c("muci_195", "muci_248") &  year == 17]


dt_tanp_muyr[, .N, muci][order(-N)]
dt_tanp_muyr[muci %in% c("muci_216")][order(year, muci)] %>% adf

dt_tanp_muyr[grepl("Smithsonian", museum)]

dt_af_instns <- gd_af_instns()
dt_af_instns[grepl("Smithsonian", Name)]
dt_af_instns[grepl("National Portrait", Name)]


## gap stuffing

dt_tanp_muyr %>% copy %>% .[, `:=`(yrdiff = max(year) - min(year), nobs = .N), muci] %>%
    .[yrdiff > nobs, .(museum_new, muci, museum, year, yrdiff, nobs)] %>% print(n=80)




## dt_tanp_wfeat[strdist_cosine_1 < 0.1]

## dt_tanp_wfeat[strdist_cosine_1 < 0.1]

## dt_tanp_wfeat[strdist_cosine_1 == 0] %>% adf

g_tanp <- igraph::graph_from_data_frame(dt_tanp_wfeat[strdist_cosine_1 == 0, .(id1, id2)], directed = F)
clusters <- igraph::cluster_louvain(g_tanp)
dt_clusters <- data.table(id = clusters$names, cluster = as.integer(clusters$membership))


merge(dt_clusters, dt_tanp_cbn, by = "id")[order(cluster)] %>%
    .[, nbr_members := .N, cluster] %>%
    .[nbr_members > 2]


dt_tanp_wfeat[strdist_avg_all> 0][order(strdist_avg_all), .(name1, name2, strdist_avg_all)] %>% print(n=9)
    ## .[grepl("American Indian", name1)]

dt_tanp_wfeat[strdist_cosine_1> 0][order(strdist_cosine_1)][1:20, .(id1, id2, name1, name2)]


tribble(~id1, ~id2, 
"tanp22_16",   "tanp20_27", 
"tanp22_55",   "tanp20_94", 
"tanp22_40",   "tanp20_20", 
"tanp22_59",   "tanp20_51", 
"tanp20_51",   "tanp20_100",
"tanp22_30",   "tanp20_49", 
"tanp22_7",    "tanp20_68") %>% adt

library(ellmer)
Sys.setenv(OPENAI_API_KEY = show_pass_secret("gemini-api-key"))
chat_google_gemini("what is the capital of uruguay")



## check match probability fall of
dt_fallof <- dt_tanp_wfeat[grepl("tanp23", id1) & grepl("tanp22", id2)] %>%
    .[order(id1, strdist_avg_all), .SD, .SDcols = patterns("avg|id1|id2|name1|name2")] %>%
    .[, any_match := as.integer(any(strdist_cosine_avg == 0)), id1] %>%
    .[any_match == 0] %>%
    .[, nbr := 1:.N, id1]

dt_fallof %>% melt(id.vars = c("id1", "nbr"), measure.vars = patterns("strdist")) %>%
    .[nbr < 5] %>% 
    ggplot(aes(x=factor(nbr), y = value, color = id1, group = id1)) + geom_line() + geom_point() +
    facet_grid(~variable)

dt_fallof[id1=="tanp23_31"] %>% adf


dt_fallof %>% ggplot(aes(x=factor(nbr), y=strdist_avg_all, color = id1, group = id1)) + geom_line() + geom_point()

dt_fallof[nbr %in% c(1,2)][, .N, nbr]

