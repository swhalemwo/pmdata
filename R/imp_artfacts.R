
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
    
    g_ovlp <- igraph::graph_from_data_frame(dt_ovlp_base, directed = F)
    clusters <- igraph::cluster_louvain(g_ovlp)
    ## plot(clusters, g_ovlp)

    dt_clusters <- data.table(ID = as.integer(clusters$names), cluster = as.integer(clusters$membership))
    
    ## dt_clusters[, .N, cluster][order(-N)]
    ## looks good: every cluster has at least 2 (kinda has to based on edgelist)

    return(dt_clusters)
}
