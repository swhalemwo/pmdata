library(data.table)
library(jtls)
library(purrr)
library(igraph)
library(terra)
library(tidygeocoder)
library(ellmer)



PMDATA_LOCS <- gc_pmdata_locs()

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))
Sys.setenv(GEMINI_API_KEY = show_pass_secret("gemini-api-key"))

gd_tanp_city_geo <- function(FILE_TANP_CITY_ID = PMDATA_LOCS$FILE_TANP_CITY_ID) {
    #' read stuff from file
    dt_tanp_city_geo <- fread(FILE_TANP_CITY_ID)
    return(dt_tanp_city_geo)
}


gd_tanp_cbn <- function(DIR_TANP_STRUC = PMDATA_LOCS$DIR_TANP_STRUC) {

    

    ## more systematic approach
    ## read in data
    dt_tanp00_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_00_struc.csv"))
    dt_tanp00 <- gd_proc_exhb(dt_tanp00_struc, "tanp00_")

    dt_tanp01_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_01_struc.csv"))
    dt_tanp01 <- gd_proc_exhb(dt_tanp01_struc, "tanp01_")


    dt_tanp02_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_02_struc.csv"))
    dt_tanp02 <- gd_proc_exhb(dt_tanp02_struc, "tanp02_")
    
    dt_tanp03_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_03_struc.csv"))
    dt_tanp03 <- gd_proc_exhb(dt_tanp03_struc, id_prefix = "tanp03_")

    
    dt_tanp04_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_04_struc.csv"))
    dt_tanp04 <- gd_proc_exhb(dt_tanp04_struc, "tanp04_")


    dt_tanp05_struc <- fread(paste0(DIR_TANP_STRUC, "tanp_05_struc.csv"))
    dt_tanp_05 <- gd_proc_exhb(dt_tanp05_struc, "tanp05_")

    dt_tanp_07 <- fread(paste0(DIR_TANP_STRUC, "tanp_07.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp07_", 1:.N), city = stringr::str_to_title(trimws(city)))]


    dt_tanp_08 <- fread(paste0(DIR_TANP_STRUC, "tanp_08.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp08_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    dt_tanp_09 <- fread(paste0(DIR_TANP_STRUC, "tanp_09.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp09_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    
    dt_tanp_10 <- fread(paste0(DIR_TANP_STRUC, "tanp_10.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp10_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    
    dt_tanp_11 <- fread(paste0(DIR_TANP_STRUC, "tanp_11.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp11_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    
    dt_tanp_12 <- fread(paste0(DIR_TANP_STRUC, "tanp_12.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp12_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    
    dt_tanp_13 <- fread(paste0(DIR_TANP_STRUC, "tanp_13.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp13_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    
    dt_tanp_14 <- fread(paste0(DIR_TANP_STRUC, "tanp_14.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp14_", 1:.N), city = stringr::str_to_title(trimws(city)))]

    dt_tanp_15 <- fread(paste0(DIR_TANP_STRUC, "tanp_15.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp15_", 1:.N), city = stringr::str_to_title(trimws(city)))]


    dt_tanp_16 <- fread(paste0(DIR_TANP_STRUC, "tanp_16.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, `:=`(id  = paste0("tanp16_", 1:.N), city = stringr::str_to_title(city))]    
    
    dt_tanp_17 <- fread(paste0(DIR_TANP_STRUC, "tanp_17.csv")) %>%
        .[, map(.SD, trimws)] %>%
        .[, id := paste0("tanp17_", 1:.N)] %>% # drop duplicate MMCA
        .[!(museum == "National Museum of Modern and Contemporary Art"  & total == "1,218,504")] %>%
        ## rename one SAAM (SAAM already there on its own)
        .[museum == "SAAM/National Portrait Gallery", museum := "National Portrait Gallery"]
          

    dt_tanp_18 <- fread(paste0(DIR_TANP_STRUC, "tanp_18.csv")) %>%
        .[, `:=`(city = stringr::str_to_title(city), id = paste0("tanp18_", 1:.N))]


    dt_tanp_19 <- fread(paste0(DIR_TANP_STRUC, "tanp_19.csv")) %>%
        .[, `:=`(city = stringr::str_to_title(city), id = paste0("tanp19_", 1:.N))]
        
    
    dt_tanp_20 <- fread(paste0(DIR_TANP_STRUC, "tanp_20.csv")) %>%
        setnames(new = c("total", "museum", "city", "perc_chng_19", "days_closed_pandemic")) %>% 
        .[, id := paste0("tanp20_", 1:.N)] 

    dt_tanp_21 <- fread(paste0(DIR_TANP_STRUC, "tanp_21.csv")) %>%
        setnames(new = c("total", "museum", "city", "perc_chng_20", "perc_chng_19")) %>% 
        .[, id := paste0("tanp21_", 1:.N)] 

    

    dt_tanp_22 <- fread(paste0(DIR_TANP_STRUC, "tanp_22.csv")) %>%
        setnames(new = c("total", "museum", "city", "perc_chng_21", "perc_chng_19")) %>%
        .[, id := paste0("tanp22_", 1:.N)]


    dt_tanp_23 <- fread(paste0(DIR_TANP_STRUC, "tanp_23.csv")) %>%
        setnames(new = c("total", "museum", "city", "perc_chng_22", "perc_chng_19")) %>%
        .[, id := paste0("tanp23_", 1:.N)]

    dt_tanp_24 <- fread(paste0(DIR_TANP_STRUC, "tanp_24.csv")) %>%
        setnames(new = c("total", "museum", "city", "perc_chng_23", "perc_chng_19")) %>%
        .[, id := paste0("tanp24_", 1:.N)]

    ## combine
    dt_tanp_cbn <- map(list(
        dt_tanp00, dt_tanp01, dt_tanp02, dt_tanp03, dt_tanp04, dt_tanp_05,
        dt_tanp_07, dt_tanp_08, dt_tanp_09, dt_tanp_10, dt_tanp_11,
        dt_tanp_12, dt_tanp_13, dt_tanp_14, dt_tanp_15, dt_tanp_16, dt_tanp_17, dt_tanp_18,
        dt_tanp_19, dt_tanp_20, dt_tanp_21, dt_tanp_22, dt_tanp_23, dt_tanp_24),
                       ~.x[, .(id, museum, city, total)]) %>%
        rbindlist %>%
        .[museum == "teamLab Borderless: MORI Building", city := "Tokyo"] %>% # manual fixes
        .[, museum := trimws(gsub("\\*|†|�", "", museum))] %>%
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
    
    max_id <- dt_tanp_city_ff[, tail(ID,1)] %>% gsub("city_", "", .) %>% as.integer

    # assign new IDs: start where we have stopped previously (ff)
    dt_tanp_city_new <- dt_tanp_city_unq[!dt_tanp_city_ff, on = "city"] %>%
        .[, ID := paste0("city_", (max_id + 1):(max_id + .N))] 

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

    ## dt_tanp_city_geo[grepl("Col", city), .(city, ID, lat, long, city_new)]

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
    ## can't use google: doesn't support city?
    ## dt_newcities_geo <- geocode(dt_tanp_city_new, city = city, full_results = T, method = "google") %>% adt

    print(dt_newcities_geo)

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

    
    max_id <- dt_tanp_muci_geo_ff[, tail(muci,1)] %>% gsub("muci_", "", .) %>% as.integer

    ## get new IDs
    dt_tanp_muci_new <- dt_tanp_wcid[, .(museum, city_new, id_city)] %>% unique %>%
        .[!dt_tanp_muci_geo_ff, on = .(museum, city_new, id_city)] %>%
        .[, muci := paste0("muci_", (max_id+1):(max_id + .N))] %>%
        .[, .(museum, city_new, muci, id_city, addr = paste0(museum, ", ",city_new))]

    ## dt_tanp_muci_new[, .(museum, city_new, id_city)] %>% print(n=80)
    ## dt_tanp_wcid[grepl("Valencia", city_new)]
    ## dt_tanp_muci_geo_ff[grepl("Valencia", city_new), .(muci, museum, city_new, id_city)]

    ## do the geocoding of new ones if neccessary
    if (nrow(dt_tanp_muci_new) > 0) {
        dt_tanp_muci_geo <- geocode(dt_tanp_muci_new, address = addr, full_results = T, method = "google") %>% adt

        
        ## manual start
        ## dt_muci_cbn <- dt_tanp_muci_geo %>% copy %>%
        ##     .[, `:=`(address_components = NULL, navigation_points = NULL, types = NULL)]


        dt_muci_cbn <- rbind(dt_tanp_muci_geo_ff, dt_tanp_muci_geo, fill = T) %>%
            .[, `:=`(address_components = NULL, navigation_points = NULL, types = NULL)]
        
        ## print(dt_tanp_muci_geo[, .(museum, city_new, muci, id_city, lat, long, addr)])

        print(dt_tanp_muci_geo)

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
    ## dt_tanp_muci_fixed[grepl("SAAM", museum)]


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

    ## dt_tanp_muci_fixed[grepl("SAAM", museum)] # seems to work
    ## dt_tanp_muci_fixed[id_city == "city_91", .(muci, museum, city_new, id_city)]

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

gd_proc_exhb <- function(dt_tanp_struc, id_prefix) {
    #' process exhibition data for integration into tanp
    #' exhb entries (museum,city) not necessarily unique for same org -> do some aggregation first
    #' kinda add tanp only later on in the end, when aggregated

    ## first aggregate to organization-year (not finally unique, need muci for that)
    dt_tanp_oyr_prep <- dt_tanp_struc[, .(total = sum(total_visitors)), .(museum = venue_name, city)] %>%
        .[, id := paste0("exhb_", 1:.N)]

    dt_tanp_city <- gd_tanp_city(dt_tanp_oyr_prep)
    ## dt_tanp_city[grepl("Louis", city), .(city, ID, lat, long, city_new)]
    ## dt_tanp_city[grepl("Col", city), .(ID, city, lat, long, city_new)]

    ## get unique city
    dt_tanp_wcid <- merge(dt_tanp_oyr_prep, dt_tanp_city[, .(city, id_city = ID, city_new)], by = "city")

    ## dt_tanp_wcid[grepl("Col", city), .(id_city, city, city_new)]
    ## dt_tanp_wcid[grepl("Louis", city), .(id_city, city, city_new)]

    ## get overall muci
    dt_tanp_muci <- gd_tanp_muci(dt_tanp_wcid)
    ## dt_tanp_muci[city_new == "St. Louis"]
    ## dt_tanp_muci[grepl("Louis", city_new), .(id_city, city_new)]
    ## dt_tanp_muci[id_city == "city_91", .(muci, museum, city_new, id_city)]

    ## add muci back to get unique institutions
    dt_tanp_exhb <- merge(dt_tanp_wcid, dt_tanp_muci[, .(muci, id_city, museum, museum_new)],
          by = c("museum", "id_city"))

    ## add stuff (museum name, city): backwards to easily integrate it: need id, museum, city, total
    dt_proc_exhb <- merge(
        dt_tanp_exhb[, .(total = sum(total)), muci],
        dt_tanp_muci[, head(.SD, 1), muci, .SDcols = c("museum", "id_city")], by = "muci") %>%
        merge(dt_tanp_city[, head(.SD,1), .(id_city = ID), .SDcols = "city"], by = "id_city") %>%
        .[order(-total)] %>% 
        .[, .(id = paste0(id_prefix, 1:.N), museum, city, total)]

    
    ## dt_tanp_exhb[, .N, id_city][!dt_tanp_wcid[, .N, id_city], on = .(id_city, N)]

    ## dt_tanp_wcid[city_new == "St. Louis"]
    ## dt_tanp_exhb[city_new == "St. Louis"]
    return(dt_proc_exhb)

}

gd_tanp05_raw <- function() {
    #' get 05 tanp exhb info


    FILE_TANP05_RAW <- "~/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_05_raw.csv"
    dt_tanp05_raw <- data.table(text = readLines(FILE_TANP05_RAW))

    dt_tanp05_raw[, `:=`(cnt_comma = str_count(text, ","), cnt_slash = str_count(text, "/"),
                         cnt_dash = str_count(text, "-"))]

    dt_tanp05_raw[, .N, cnt_comma]
    dt_tanp05_raw[, .N, cnt_slash]
    dt_tanp05_raw[cnt_slash %in% c(1,2)]
    dt_tanp05_raw[, .N, .(cnt_slash, cnt_dash)]

    dt_tanp05_raw[cnt_slash == 4 & cnt_dash != 1]
    dt_tanp05_raw[cnt_slash %!in% c(0,4)]

    ## get IDs: use date delimiters as ID, then fill up backwards
    dt_tanp05_raw[cnt_slash == 4, id_show := 1:.N] %>%
        setnafill(type = "nocb", cols = "id_show") %>%
        .[, lines := .N, id_show]

    return(dt_tanp05_raw)
}






gd_tanp05_struc <- function(dt_tanp05_raw, limit) {
    
    chat_tanp <- chat_google_gemini("you are a careful research assistant. you'll get some text summarizing an art exhibition. the first two numbers are daily visitors and total visitors. after that you have, in the following order, the show name, the venue name, the city, and the date (start/end). there may not be clear delimiters between these items, i.e. exhibition name could be followed directly by the museum name. all fields are always present. your task is to figure out all the fields (visitor daily, visitor total, show name, museum name, start and end date)")


    ## text1 <- dt_tanp05_raw[id_show == 1, paste0(text, collapse = "\n")]

    schema_tanp05 <- type_object(
        daily_visitors = type_number(),
        total_visitors = type_number(),
        show_name = type_string(),
        venue_name = type_string(),
        city = type_string(),
        start_date = type_string(),
        end_date = type_string())

    ## chat_tanp$chat_structured(text1, type = schema_tanp05)
    ## dt_text_parallel <- dt_tanp05_raw[id_show <= 40, .(test_strs = paste0(text, collapse = "\n")), id_show]
    dt_text_parallel <- dt_tanp05_raw[id_show <= limit, .(test_strs = paste0(text, collapse = "\n")), id_show]
    l_prompts <- dt_text_parallel$test_strs %>% as.list

    ## do the LLM
    dt_tanp05_struc_prep <- parallel_chat_structured(chat_tanp, l_prompts, type = schema_tanp05)

    dt_tanp05_struc <- cbind(dt_text_parallel[, .(id_show)], dt_tanp05_struc_prep) # add IDs

    ## write to file: add index
    DIR_LLM <- paste0("~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/")
    l_files <- list.files(DIR_LLM) %>% keep(~grepl("tanp05_llm_", .x))
    
    max_cntr <- gsub("tanp05_llm_(\\d+)\\.csv", "\\1", l_files) %>% as.integer %>% max
    
    fwrite(dt_tanp05_struc, paste0(DIR_LLM, sprintf("tanp05_llm_%s.csv", max_cntr + 1)))

    return(dt_tanp05_struc)
}

gd_tanp05_asses <- function(dt_tanp05_struc) {
    #' automated checks
    
    ## check whether ordering by visitor numbers makes sense
    dt_assess <- dt_tanp05_struc %>% copy %>%
        .[, daily_visitors_l1 := data.table::shift(daily_visitors)]

    ## check how many times the daily visitor order is disrupted
    dt_assess[, daily_check := daily_visitors_l1 >=daily_visitors]

    dt_assess[, `:=`(date_start = as.Date(start_date, format = "%d/%m/%y"),
                     date_end = as.Date(end_date, format = "%d/%m/%y"))] %>%
        .[, nbr_days_date := as.numeric(date_end - date_start)] %>% # days open based on dates, just range
        .[, nbr_days_cnts := total_visitors/daily_visitors] # how many days it was open based on visitor numbers

    ## dt_assess[nbr_days_cnts > nbr_days_date]
        
    avg_scalar <- dt_assess[, total_visitors/(daily_visitors * nbr_days_date)] %>% mean
    print(sprintf("avg scalar: %s", round(avg_scalar,3)))

    ## check how much predicted total numbers vary from observed, assuming avg scalar (best fit)
    ## difference by log 0.2 in either direction
    dt_assess_cnts <- dt_assess[, total_pred := nbr_days_date* avg_scalar * daily_visitors] %>%
        .[, diff_total_obs_pred := log(total_pred/total_visitors)]

    dt_assess_cnts[is.na(diff_total_obs_pred), .(total_pred, total_visitors, nbr_days_date)]
    

    
    ## dt_assess %>% ggplot(aes(x = diff_total_obs_pred)) + geom_density()

    ## dt_assess[abs(diff_total_obs_pred) > 0.2, .(id_show, nbr_days_date,
    ##                                             daily_visitors, total_pred, total_visitors)]

    ## check of those where the daily visitor numbers are not able to reach the total (total > daily * nbr_days)
    ## how many of them are substantially higher (log > 0.1)
    dt_assess_days <- dt_assess[nbr_days_cnts > nbr_days_date,
                                .(id_show, nbr_days_date, nbr_days_cnts, daily_visitors, total_pred,
                                  total_visitors, date_start, date_end)] %>%
        .[, diff_nbrdays := log(nbr_days_date/nbr_days_cnts)] %>%
        .[abs(diff_nbrdays) > 0.1]
    
    
    dt_check <- tribble(~test, ~cnt,
            "daily_order", dt_assess[daily_check == F, .N],
            "nbr_visitor", dt_assess[abs(diff_total_obs_pred) > 0.2, .N],
            "nbr_days", dt_assess_days[abs(diff_nbrdays) > 0.1, .N],
            "wrong_dates", dt_assess[nbr_days_date < 0, .N]) %>% adt %>%
        .[, prop := cnt/dt_assess[, .N]]
    

    return(dt_check)

}

gd_tanp05_mnlcheck <- function(dt_tanp05_struc) {

    ## manual check
    dt_output_mnl <- fread("~/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_05_1.csv", nrows = 113) %>%
        .[, `:=`(total_visitors = as.integer(trimws(gsub(",", "", total_visitors))),
                 daily_visitors = as.integer(trimws(gsub(",", "", daily_visitors))),
                 venue_name = trimws(venue_name),
                 id_show = 1:.N, src = "mnl")]
        

    ## dt_output_mnl[1:40, .(show_name)] %>% print(n=80)
    ## dt_output_par[1:40, .(show_name)] %>% print(n=80)

    l_relcols <- c("show_name", "venue_name", "daily_visitors", "total_visitors")

    dt_cpr_wide <- rbind(dt_output_mnl %>% copy %>% melt(id.vars = c("id_show", "src"), measure.vars = l_relcols),
                          dt_tanp05_struc %>% copy %>% .[, `:=`(src = "par")] %>%
                          melt(id.vars = c("id_show", "src"), measure.vars = l_relcols)) %>%
        dcast(id_show + variable ~src)

    dt_check_mnl <- dt_cpr_wide %>%
        .[, b_same := as.integer(mnl == par)] %>%
        .[b_same == 0]

    return(dt_check_mnl)
    
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



dt_tanp_muyr[, .N, .(muci, year)][N > 1] %>%
    merge(dt_tanp_muyr, by = c("muci", "year"))

dt_tanp_muyr[grepl("queensland", museum, ignore.case = T)]
dt_tanp_muyr[grepl("de young", museum, ignore.case = T)]

dt_tanp_muyr[grepl("SAAM", museum_new), .(muci)]
dt_tanp_muyr[grepl("National Portrait", museum) & grepl("Washington", city_new)]


dt_tanp_muyr %>% ggplot(aes(x = year, y = total, color = muci)) +
    geom_line(show.legend = F) +
    scale_y_continuous(trans = scales::log10_trans()) +
    geom_point(show.legend = F)

dt_tanp_muyr[grepl("national museum", museum, ignore.case = T)] %>% print(n=80)

dt_tanp_muyr[year == 17][order(-total)]



dt_tanp_muyr[, .N, muci][order(-N)]
dt_tanp_muyr[muci %in% c("muci_216")][order(year, muci)] %>% adf

dt_tanp_muyr[grepl("Smithsonian", museum)]

dt_af_instns <- gd_af_instns()
dt_af_instns[grepl("Smithsonian", Name)]
dt_af_instns[grepl("National Portrait", Name)]

dt_af_exhbs <- gd_af_exhbs()

dt_af_exhbs[, begin_year := year(BeginDate)] %>%
    .[begin_year > 1990, .N, begin_year] %>% ggplot(aes(x=begin_year, y = N)) + geom_line()




## gap stuffing

dt_tanp_muyr %>% copy %>% .[, `:=`(yrdiff = max(year) - min(year), nobs = .N), muci] %>%
    .[yrdiff > nobs, .(museum_new, muci, museum, year, yrdiff, nobs)] %>%
    .[, nobs_abs := yrdiff - nobs] %>% # print(n=80)
    .[, head(.SD, 1), muci] %>% ## .[, sum(nobs_abs)]
    





