library(tidygeocoder)
library(data.table)
library(DBI)
library(RSQLite)

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))

setup_geocode_sqlite <- function(DB_GEOCODE) {
    #' setup sqlite db for geocoding if not existing

    
    dbx <- dbConnect(SQLite(), DB_GEOCODE)

}


DB_GEOCODE <- "~/Dropbox/phd/pmdata/inst/manual_munging/db_geocode.sqlite"




jtls_geocode <- function(dtx, c_geocode) {
    #' geocode dt snippet: dtx
    #' c_geocode: how geocoding is to be done, list of mapping input arguments to geocode functionality (
    #' e.g. address, street, city, county
    #' also allows to specify method (e.g. "google")

    c_geocode_args <- c(list(.tbl = dtx), c_geocode)
    dt_geocoded <- do.call(geocode, c_geocode_args)

    #' write dt_geocoded to sqlite
    
}

dtxx <- dt_pmdb[museum_status %in% c("private museum", "closed")] %>% 
    .[10:15, .(ID, name, city, country = iso3c,
                       addr = sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name")))]


## default
dt_geocoded <- geocode(dtxx, address = addr, limit = 5, full_results = T, return_input = F)

## google
dt_geocoded2 <- geocode(dtxx, address = addr, limit = 5, full_results = T, return_input = F, method = "google") %>% adt

dt_geocoded2 %>% adt %>% .[, address_components] #


dbDataType(dbx, dt_geocoded2)

map(dt_geocoded2, class) %>% as.vector
sapply(dt_geocoded2, class) %>% keep(~.x == "list") %>% names
l_vrbls <- sapply(dt_geocoded2, class) %>% keep(~.x == "list") %>% names

dt_geocoded2[, .SD, .SDcols = l_vrbls]

dt_geocoded2[, navigation_points]


dt_geocoded_proc <- merge(adt(dt_geocoded), dtxx[, .(ID, address = addr)], by = "address")



gen_extra_col_schema <- function(col_vlu, col_prefix, id_vlu) {
    #' idea is that every non-standard column gets its own table
    #' need to see if schemas are all the same, then set up table
    
    ## check if entire col_vlu can be easily parsed: no list columns
    ## check whether column is vector
    b_vec_and_parseable <- all(unique(map(col_vlu, class)) %in% c("numeric", "logical", "character")) 
    
    b_df_parseable <- F
    l_parse_true <- NULL
    ## maybe I'll get more complicated class structures here later on
    if (any(sapply(col_vlu, class) == "data.frame")) {
        ## data.frame case: get parseable columns
        l_parse_asses <- sapply(col_vlu, \(col) sapply(col, class)) %>% apply(1, \(x) any(x == "list"))
        l_parse_true <- names(l_parse_asses)[!l_parse_asses]
        l_parse_false <- names(l_parse_asses)[l_parse_asses]
        b_df_parseable <- ifelse(len(l_parse_false) == 0, T, F)
    }
            
    if (b_vec_and_parseable) {
        ## vector case: assume all have the same structure, but have stop test
        l_schemas <- map(col_vlu, ~dbDataType(dbx, .x)[1])

        if (len(unique(l_schemas)) > 1) {stop("schemas not unique")}

        ## common_cols <- Reduce(intersect, l_schemas)
        ## check if all schemas are the same
        ## if (Reduce(equals, l_schemas))

        cmd_setup <- sprintf("CREATE TABLE %s (%s %s)", col_prefix, col_prefix,l_schemas[1])

        dbSendQuery(dbx, cmd_setup)
    }

    
    if (len(l_parse_true) > 0) {    
    
        ## data.frame case: get the parseable columns
        ## currently using data.frame (is what google api returns), but maybe should use DT since more general
        l_schemas <- map(col_vlu, ~dbDataType(dbx, .x[l_parse_true]))

        if (len(unique(l_schemas))>1) {stop("schemas not unique")}
    
        cmd_setup <- sprintf("CREATE TABLE %s (%s %s, \n%s)\n", col_prefix,
                             "ID", dbDataType(dbx, id_vlu),
                             paste0(imap(l_schemas[[1]], ~sprintf("%s %s", .y, .x)), collapse = ",\n"))
    
        dbSendQuery(dbx, cmd_setup)
    }
    
    ## dealing with non-parseable things
    ## can i use recursion? 
    # i think I have to combine them first tho: now have 7 dfs, but that should only be 1 table
    col_vlu[[1]]
    dt_unparsed <- map(col_vlu, ~.x[l_parse_false]) %>% Reduce(rbind, .) 

    



}

dt_geocoded3 <- dt_geocoded2 %>% copy %>% .[, ID := 1:7]

dt_geocoded3[, imap(.SD, ~gen_extra_col_schema(.x, .y, ID)), .SDcols = c("address_components")]

gen_extra_col_schema(dt_geocoded2[, address_components], "address_components", 1:7)
gen_extra_col_schema(dt_geocoded2[, types], "types")

gwd_geocode_dt <- function(dtx) {

    ## set up sqlite


    ## map 


}
