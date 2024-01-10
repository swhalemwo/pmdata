
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
