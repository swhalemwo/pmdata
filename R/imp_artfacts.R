
#' import the data.table of artfacts exhibitions from ArtFacts sqlite db
#' @param ARTFACTS_SQLITE_DB location of ArtFacts sqlite db
#' @export
gd_af_exhbs <- function(ARTFACTS_SQLITE_DB = PMDATA_LOCS$ARTFACTS_SQLITE_DB) {
    
    db_artfacts <- dbConnect(SQLite(), ARTFACTS_SQLITE_DB)

    dt_af_exhbs <- dbGetQuery(db_artfacts, "select * from exhbs") %>% adt
    return(dt_af_exhbs)
}

#' provide DT of ArtFacts institutions
#' @param ARTFACTS_SQLITE_DB location of ArtFacts sqlite db
#' @export
gd_af_instns <- function(ARTFACTS_SQLITE_DB = PMDATA_LOCS$ARTFACTS_SQLITE_DB) {
    
    db_artfacts <- dbConnect(SQLite(), ARTFACTS_SQLITE_DB)
    dt_af_instns <- dbGetQuery(db_artfacts, "select * from instns") %>% adt

    return(dt_af_instns)
}

#' provide DT of ArtFacts artists
#' @param ARTFACTS_SQLITE_DB location of ArtFacts sqlite db
#' @export
gd_af_people <- function(ARTFACTS_SQLITE_DB = PMDATA_LOCS$ARTFACTS_SQLITE_DB) {
    
    db_artfacts <- dbConnect(SQLite(), ARTFACTS_SQLITE_DB)
    dt_af_people <- dbGetQuery(db_artfacts, "select * from people") %>% adt

    return(dt_af_people)
}

#' provide links of ArtFacts links to exhibitions
#' @param ARTFACTS_SQLITE_DB location of ArtFacts sqlite db
#' @export
gd_af_links_exhb_ppl <- function(ARTFACTS_SQLITE_DB = PMDATA_LOCS$ARTFACTS_SQLITE_DB) {
    
    db_artfacts <- dbConnect(SQLite(), ARTFACTS_SQLITE_DB)
    ## FIXME: adjust sqlite table name after setting up proper import
    dt_af_links_exhb_ppl <- dbGetQuery(db_artfacts, "select * from exhb_ppl_links") %>% adt

    return(dt_af_links_exhb_ppl)
}

## gd_af_links_exhb_ppl()

#' read the file of matches between AF institutions (AF_IID) and PMDB
#' one row per PM
#' @param FILE_ARTFACTS_PMDB_MATCHES location of the AF-PMDB match results file
#' @return data.table with column PMDB_ID and AF_IID (integer where match exists, "nomatch" if no match exists)
#' @export
gd_af_pmdb_matches <- function(FILE_ARTFACTS_PMDB_MATCHES = PMDATA_LOCS$FILE_ARTFACTS_PMDB_MATCHES) {
    if (file.exists(FILE_ARTFACTS_PMDB_MATCHES)) {
        dt_res <- fread(FILE_ARTFACTS_PMDB_MATCHES)
    } else {
        dt_res <- data.table(PMDB_ID = character(), AF_IID = character())
    }

    return(dt_res)
}


#' read the file of Artfacts institution classification
#'
#' @param FILE_ARTFACTS_INST_CLSFCN csv file with classifications
#' @export
gd_af_inst_clsfcn <- function(FILE_ARTFACTS_INST_CLSFCN = PMDATA_LOCS$FILE_ARTFACTS_INST_CLSFCN) {
    fread(FILE_ARTFACTS_INST_CLSFCN)
}


#' get the AF locations (geocoded with google maps API)
#'
#' @param NODB_GEOCODE_AF sqlite DB with locations
#' @export
gd_af_inst_loc <- function(NODB_GEOCODE_AF = PMDATA_LOCS$NODB_GEOCODE_AF) {
    db_af_inst_locs <- dbConnect(SQLite(), NODB_GEOCODE_AF)
    
    dt_af_inst_loc <- dbGetQuery(db_af_inst_locs, "select * from google_flat") %>% adt
    return(dt_af_inst_loc)
}


#' see which locations are close together
#' uses base dist, rather than more accurate haversine (now should be less accurate on poles)
#' because base dist is much faster
#' but should be good enough
#'
#' @param dt_af_inst_loc data.table with columns for ID (integer), long, lat
#' @param maxsim nodes closer than this are in same place (i think this is around some meters)
#' @export
gd_af_inst_sameloc <- function(dt_af_inst_loc, maxsim = 0.00001) {
    

    t3 <- Sys.time()
    mx <- dt_af_inst_loc[, cbind(long, lat) %>% set_rownames(ID)] %>% dist
    dt_af_inst_sameloc <- i1d2d(which(mx < maxsim), mx) %>% adt %>% .[, names(.) := map(.SD, as.integer)]
    t4 <- Sys.time()
    t4-t3 # 0.11 secs %>% much faster
    return(dt_af_inst_sameloc)
}


#' use graph to see which orgs are in same place by clustering sameloc edge list
#' @param dt_af_inst_sameloc edge
#' @export
gd_af_inst_sameloc_clusters <- function(dt_af_inst_sameloc) {
    
    g_ovlp <- igraph::graph_from_data_frame(dt_af_inst_sameloc, directed = F)
    clusters <- igraph::cluster_louvain(g_ovlp)
    ## plot(clusters, g_ovlp)

    dt_clusters <- data.table(ID = as.integer(clusters$names), cluster = as.integer(clusters$membership))
    
    ## dt_clusters[, .N, cluster][order(-N)]
    ## looks good: every cluster has at least 2 (kinda has to based on edgelist)

    return(dt_clusters)
}

#' spread locations of identically located points using 2d density
#' @param dt_af_inst_loc_wclust data.table with all orgs (including overlapping ones): coordinates, cluster, ID
#' @param cluster_id cluster identifier (integer)
#' @export 
gd_ovlp_shifted_coords <- function(dt_af_inst_loc_wclust, cluster_id) {


    ## get coordinates of stuff to cluster
    dt_cluster_coords <- dt_af_inst_loc_wclust[cluster == cluster_id]

    ## get cluster location
    mat_cluster_coords <- dt_cluster_coords[, map(.SD, mean), .SDcols = c("long", "lat")]

    ## get all other instns around cluster mean; only use non-approximate
    dt_base <- dt_af_inst_loc_wclust[distHaversine(cbind(long, lat), mat_cluster_coords)<10e3] %>%
        .[geometry.location_type != "APPROXIMATE"]

    ## leaflet(dt_base) %>% addTiles %>% addCircles(lng=~long, lat=~lat)

    ## if there's nothing in environment -> just return original coords, no can do
    if (dt_base[, .N] < 2 | dt_base[, uniqueN(.SD), .SDcols = c("lat", "long")] == 1) {
        return(dt_cluster_coords)
    }

    ## calculate grid of base, assign cells and relative probablities
    dt_grid <- dt_base[, kde2d_tidy(long, lat, n = 50)][, `:=`(cell = 1:.N, zscld = z/max(z))]
    
    ## visualization: maybe move to separate plot for illustrative purposes
    ## ## addRectangles needs corners %>% needs avg step size
    ## lunqx <- dt_grid[, unique(x)] %>% sort
    ## stepx <- (lunqx - shift(lunqx)) %>% mean(na.rm = T)

    ## lunqy <- dt_grid[, unique(y)] %>% sort
    ## stepy <- (lunqy - shift(lunqy)) %>% mean(na.rm = T)

    ## dt_grid[, `:=`(xmin = x - stepx/2, xmax = x + stepx/2, ymin = y - stepy/2, ymax = y+stepy/2)]
    

    ## dt_grid %>% ggplot(aes(x=x, y=y, fill = z)) + geom_tile()

    ## leaflet(dt_grid) %>% addTiles() %>%
    ##     ## addCircles(lng = ~x, lat = ~y) %>%
    ##     addRectangles(lng1 = ~xmin, lng2 = ~xmax, lat1 = ~ymin, lat2 = ~ymax, stroke = F,
    ##                   fillOpacit = 0.5,
    ##                   color = ~colorNumeric("YlOrRd", 0:1)(zscld))
    
    # update coords
    dt_newcords <- dt_grid[cell %in% sample(cell, size = dt_cluster_coords[, .N], prob = zscld), .(x,y)]
    dt_cluster_newcords <- cbind(dt_cluster_coords, dt_newcords)[, `:=`(long = x, lat = y, x = NULL, y= NULL)] 

    ## check that coords are now spread -> looks good
    ## leaflet(dt_cluster_coords) %>% addTiles %>% addCircles(lat = ~lat, lng = ~long)
    ## leaflet(dt_cluster_newcords) %>% addTiles %>% addCircles(lat = ~lat, lng = ~long)
            
    return(dt_cluster_newcords)


}

#' de-overlap overlapping locations (approximate locations which cluster in city centers)
#' @param dt_af_inst_loc dt with AF locations
#' @param dt_af_inst_sameloc_clusters dt with AF overlap cluster information
#' @export
gd_af_loc_deovlpd <- function(dt_af_inst_loc, dt_af_inst_sameloc_clusters) {
    ## dt_af_inst_sameloc_clusters[, .N, cluster][N > 3]

    ## merge cluster info back
    dt_af_inst_loc_wclust <- merge(dt_af_inst_loc, dt_af_inst_sameloc_clusters, by = "ID", all.x = T)

    ## get all the places that have to be spread out; only keep where location is not precise
    dt_af_inst_clusters <- dt_af_inst_loc_wclust[!is.na(cluster) & geometry.location_type != "ROOFTOP"] %>%
        copy %>% 
        .[, memnbr_cluster := .N, cluster] %>% .[memnbr_cluster > 1] # only keep clusters here with at least 2

    ## 2d density
    ## need the NYC cluster: 63 (atm)
    ## dt_af_inst_loc_wclust[distHaversine(cbind(long, lat), cbind(-74.0060, 40.7128)) < 50e3, .N, cluster][order(-N)]

    ## shifing all takes around 3.5 secs %>% fine to keep like this

    dt_af_inst_clust_newcords <- map(dt_af_inst_clusters[, unique(cluster)],
                                     ~gd_ovlp_shifted_coords(dt_af_inst_loc_wclust, .x)) %>% rbindlist

    ## bring new shifted coords back in 
    dt_af_loc_deovlpd <- dt_af_inst_loc %>% copy %>%
        .[dt_af_inst_clust_newcords, `:=`(lat = i.lat, long = i.long), on = "ID"]

    ## manually inspect -> looks good
    ## dt_af_inst_loc[distHaversine(cbind(long,lat), cbind(-74.0060, 40.7128)) < 50e3] %>%
    ##     leaflet() %>% addTiles %>% addCircles(lat = ~lat, lng = ~long)

    ## dt_af_loc_deovlpd[distHaversine(cbind(long,lat), cbind(-74.0060, 40.7128)) < 50e3] %>%
    ##     leaflet() %>% addTiles %>% addCircles(lat = ~lat, lng = ~long)

    return(dt_af_loc_deovlpd)

}
