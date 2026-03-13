library(data.table)
library(jtls)
library(purrr)
library(igraph)
library(terra)

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
    dt_tanp_cbn <- map(list(dt_tanp_20, dt_tanp_21, dt_tanp_22, dt_tanp_23, dt_tanp_24),
                       ~.x[, .(id, museum, city, total)]) %>%
        rbindlist %>%
        .[museum == "teamLab Borderless: MORI Building", city := "Tokyo"] # manual fixes
    
    if (dt_tanp_cbn[city == "", .N] > 0) {stop("not all museums have cities")}

    return(dt_tanp_cbn)


}


gd_tanp_city <- function(dt_tanp_cbn) {
    #' get all the cities

    dt_tanp_city <- dt_tanp_cbn[, .(city = unique(city))] # [, ID := paste0("city_", 1:.N)]

    ## need to deal with cities that have no ID yet
    dt_tanp_city_ff <- gd_tanp_city_geo()
    
    dt_tanp_city_new <- dt_tanp_city[!dt_tanp_city_ff, on = "city"] %>%
        .[, ID := paste0("city_", (dt_tanp_city[, .N]):.N)] # start where we have stopped

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
    dt_tanp_city_geo[dt_tanp_city_links_ff, ID := i.ID_i, on = .(ID = ID_j)]
    ## dt_tanp_city_geo[grepl("Kr", city), .(city, ID)]

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
        fwrite(dt_newcitie_geo %>% adt %>% copy %>% .[, boundingbox := NULL], FILE_TANP_CITY_ID, append = T)
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



dt_tanp_cbn <- gd_tanp_cbn()





## cross-join into long
dt_tanp_cpr <- merge(dt_tanp_cbn[, .(id1 = id, name1 = paste0(museum, " -- ", city), mrg = "x")],
                     dt_tanp_cbn[, .(id2 = id, name2 = paste0(museum, " -- ", city), mrg = "x")], by = "mrg",
                     allow.cartesian = T) %>% .[id1 > id2]



## l_cols_strdist_toobig <- paste0("strdist_", c(paste0("qgram_", 1:5), "osa", "lv", "dl", "lcs"))

## only use standardized string distances: cosine, jaccard, jw
dt_qmod <- expand.grid(mod = c("cosine", "jaccard"), q = 1:5, stringsAsFactors = F) %>% adt

dt_tanp_wfeat <- gd_grid_wfeat(dt_tanp_cpr, "name1", "name2", dt_qmod = dt_qmod, l_mod_noq = "jw") %>% 
    .[, strdist_cosine_avg := rowMeans(.SD), .SDcols = patterns("strdist_cosine")] %>%
    .[, strdist_jaccard_avg := rowMeans(.SD), .SDcols = patterns("strdist_jaccard")] %>%
    .[, strdist_avg_all := rowMeans(.SD), .SDcols = c("strdist_cosine_avg", "strdist_jaccard_avg", "strdist_jw")]

dt_tanp_wfeat[strdist_cosine_1 < 0.1]


dt_tanp_wfeat[strdist_cosine_1 < 0.1]

dt_tanp_wfeat[strdist_cosine_1 == 0] %>% adf

g_tanp <- igraph::graph_from_data_frame(dt_tanp_wfeat[strdist_cosine_1 == 0, .(id1, id2)], directed = F)
clusters <- igraph::cluster_louvain(g_tanp)
dt_clusters <- data.table(id = clusters$names, cluster = as.integer(clusters$membership))


merge(dt_clusters, dt_tanp_cbn, by = "id")[order(cluster)] %>%
    .[, nbr_members := .N, cluster] %>%
    .[nbr_members > 2]


dt_tanp_wfeat[strdist_avg_all> 0][order(strdist_avg_all), .(name1, name2, strdist_avg_all)][1:20]

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
