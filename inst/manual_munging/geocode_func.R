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



gl_assess_dt_struc <- function(col_vlu, verbose = F) {

    b_vec_and_parseable <- all(unique(map(col_vlu, class)) %in% c("numeric", "logical", "character")) 
    
    b_df_parseable <- F
    l_parse_true <- NULL
    l_parse_false <- NULL

    ## maybe I'll get more complicated class structures here later on
    if (any(sapply(col_vlu, class) == "data.frame")) {
        ## data.frame case: get parseable columns

        ## this works for address_components so I keep it here for now, but less generalizable:
        ## seems as if nested sapply transforms data differently depending on situation -> unusuable
        ## mat_parse_assess <- sapply(col_vlu[!sapply(col_vlu, is.null)],
        ##                            \(col) sapply(col, class))
        ## if (verbose) {print(mat_parse_assess)}
        ## l_parse_impob <- apply(mat_parse_assess, 1, \(x) any(x == "list"))

        ## lapply + rbindlist seems more reliable
        dt_parse_assess <- lapply(col_vlu[!sapply(col_vlu, is.null)],
                                  \(col) lapply(col, class)) %>% rbindlist(fill = T)
            
                
        if (verbose) {print(dt_parse_assess)}

        ## see which columns can't be parsed
        l_parse_impob <- dt_parse_assess[, map(.SD, ~any(.x %in% c("list", "data.frame")))] %>% unlist

        ## l_parse_impob <- sapply(col_vlu[3], class)
                
        if (verbose) {print(l_parse_impob)}
        
        l_parse_true <- names(l_parse_impob)[!l_parse_impob]
        l_parse_false <- names(l_parse_impob)[l_parse_impob]
        b_df_parseable <- ifelse(len(l_parse_false) == 0, T, F)
    }

    l_assess_dt_struc <- list(
        b_vec_and_parseable = b_vec_and_parseable,
        l_parse_true  = l_parse_true,
        l_parse_false =  l_parse_false,
        b_df_parseable  = b_df_parseable)

    return(l_assess_dt_struc)

}    

gd_rcbn <- function(v_parse_false, l_dt_parsed) {
    ## browser()
    #' separate way of recombining columns that are not directly parseable

    ## get datatypes to see whether I can use rbind or not
    ## l_datatypes <- lapply(col_vlu[l_index_nonnull[[v_parse_false]]], \(row) map(row, class)) %>% unlist

    ## l_datatypes <- lapply(l_dt_parsed, \(row) map(row, class)) %>% unlist

    l_datatypes <- lapply(l_dt_parsed[[v_parse_false]], \(row) map(row, class)) %>% unlist

    ## l_dt_parsed

    if (!any(l_datatypes == "data.frame")) {

        ## this works if there are no dfs in the columns

        dt_cbn <- Reduce(rbind, l_dt_parsed[[v_parse_false]])

    } else {

        ## if there are dfs, i can't just use rbind but need to re-assemble

        ## reassemble by changing to cols: map for each column, extract its value from all the rows
        l_cols <- lapply(c("ID", v_parse_false), \(colx) map(l_dt_parsed[[v_parse_false]], ~chuck(.x, colx)))
        
        ## l_dt_parsed[[1]]
        ## l_dt_parsed$location[[3]] %>% chuck("ID")

        

        ## this is what to get to, it seems to work 
        ## dt_cbn <- data.frame(ID = unlist(l_cols[1]),
        ##                      location = I(l_cols[[2]])) %>% adt
        
        ## do this automatically:
        ## need a do.call -> need a list of arguments
        ## i just need the number of the column, then can use setNames to add the column name

        ## first set up ID column (number 1)
        l_colargs <- list("ID" = unlist(l_cols[1]))

        ## then add the others with I()
        l_col_noparse_prep <- imap(v_parse_false, ~I(l_cols[[.y +1]])) %>% setNames(v_parse_false)

        l_col_noparse_cbn <- c(l_colargs, l_col_noparse_prep)

        dt_cbn <- do.call(data.frame, l_col_noparse_cbn) %>% adt
    }

    return(dt_cbn)
}


gen_extra_col_schema <- function(col_vlu, col_prefix, id_vlu) {
    ## browser()
    #' idea is that every non-standard column (list, data.frame) gets its own table
    #' need to see if schemas are all the same, then set up table
    #' recursively goes through all the non-standard columns and checks whether they can be parsed
    
    ## check if entire col_vlu can be easily parsed: no list columns
    ## check whether column is vector
    
    print(sprintf("working on col: %s", col_prefix))

    l_assess_dt_struc <- gl_assess_dt_struc(col_vlu)
    b_vec_and_parseable <- l_assess_dt_struc$b_vec_and_parseable
    l_parse_true <- l_assess_dt_struc$l_parse_true
    l_parse_false <- l_assess_dt_struc$l_parse_false
    b_df_parseable <- l_assess_dt_struc$b_df_parseable

            
    if (b_vec_and_parseable) {
        ## vector case: assume all have the same structure, but have stop test
        l_schemas <- map(col_vlu, ~dbDataType(dbx, .x)[1])

        if (len(unique(l_schemas)) > 1) {stop("schemas not unique")}

        ## common_cols <- Reduce(intersect, l_schemas)
        ## check if all schemas are the same
        ## if (Reduce(equals, l_schemas))

        cmd_setup <- sprintf("CREATE TABLE %s (ID %s, %s %s)",
                             col_prefix, dbDataType(dbx, id_vlu), col_prefix,l_schemas[1])
        
        cat(cmd_setup)
        cat("\n")
        dbSendQuery(dbx, cmd_setup)
    }
    
    if (len(l_parse_true) > 0) {    
    
        ## data.frame case: get the parseable columns
        ## currently using data.frame (is what google api returns), but maybe should use DT since more general
        l_schemas <- map(col_vlu, ~dbDataType(dbx, .x[l_parse_true]))

        if (len(unique(l_schemas))>1) {stop("schemas not unique")}

        ## deal with dots in schemas: need to be quoted for sqlite
        l_vrbl_schema <- imap(l_schemas[[1]],
                              ~sprintf("%s %s", ifelse(grepl("\\.", .y), sprintf("\"%s\"", .y), .y), .x))

        cmd_setup <- sprintf("CREATE TABLE %s (ID %s, \n%s)\n", col_prefix,
                             dbDataType(dbx, id_vlu),
                             paste0(l_vrbl_schema, collapse = ",\n"))

        cat(cmd_setup)
        cat("\n")
        dbSendQuery(dbx, cmd_setup)
    }
    
    ## dealing with non-parseable columns using recursion
    ## i think I have to combine them first tho: now have 7 dfs, but that should only be 1 table
    
    if (len(l_parse_false) > 0) {
    
        ## for each l_parse_false variable, get the non-null indexes
        ## need to do a looooooot of unpacking to get to the NULLs (2 nested loops not enough)
        
        l_index_nonnull <- map(l_parse_false,
                               ~!sapply(col_vlu, \(row) all(sapply(row[[.x]], \(vlus) is.null(vlus))))) %>%
            setNames(l_parse_false)

        ## get list of streamlined objects, with ID
        l_dt_parsed2 <- lapply(l_parse_false,
                              \(col) map2(col_vlu[l_index_nonnull[[col]]], id_vlu[l_index_nonnull[[col]]],
                                          ~{.x$ID <- .y; .x[c("ID", col)]})) %>% 
            setNames(l_parse_false)

        
        ## combine to new df
        l_dt_cbn <- map(l_parse_false, ~gd_rcbn(.x, l_dt_parsed2[.x])) %>%
            setNames(l_parse_false)

        ## recursively run to get new schemas
        map(l_parse_false, ~gen_extra_col_schema(l_dt_cbn[[.x]][[.x]],
                                                 col_prefix = paste0(col_prefix, "_", .x),
                                                 id_vlu = l_dt_cbn[[.x]][["ID"]]))
        
    }
    
    ## xx <- "navigation_points"
    ## gen_extra_col_schema(l_dt_cbn[[xx]][[xx]],
    ##                      col_prefix = paste0(col_prefix, "_", xx),
    ##                      id_vlu = l_dt_cbn[[xx]][["ID"]])

    
}

dt_tomap <- dt_pmdb[museum_status %in% c("private museum", "closed") & iso3c == "USA"] %>% 
    .[sample(1:.N, 5), .(ID, name, city, country = iso3c,
                         addr = sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name")))]



## set up method vectors
l_methods <- list("google" = list(mode = "single"),
                  "osm" =    list(mode = "single"),
                  "arcgis" = list(mode = "single"),
                  "census" = list(mode = "single"))

## do the actual coding
l_dt_geocoded_prep1 <- imap(l_methods, ~geocode(dt_tomap, address = addr, limit = 5, full_results = T,
                                                return_input = F, method = .y, mode = .x$mode))
## merge back IDs
l_dt_geocoded_prep2 <- map(l_dt_geocoded_prep1, ~merge(.x, dt_tomap[, .(ID, address = addr)]))
## splitting into list
l_dt_geocoded_prep3 <- imap(l_dt_geocoded_prep2,
                            ~data.table(colx = split(adf(.x[names(.x) != "ID"]), 1:nrow(.x)), ID = .x$ID) %>%
                            setnames(old = "colx", new = .y))

system("rm /home/johannes/Dropbox/phd/pmdata/inst/manual_munging/db_geocode.sqlite")
dbx <- dbConnect(SQLite(), DB_GEOCODE)

imap(l_dt_geocoded_prep3, ~gen_extra_col_schema(.x[, get(.y)], .y, .x$ID))


gen_extra_col_schema(l_dt_geocoded_prep3$osm$osm, "osm", l_dt_geocoded_prep3$osm$ID) ## works with new
gen_extra_col_schema(l_dt_geocoded_prep3$google$google, "google", l_dt_geocoded_prep3$google$ID)



## default
dt_geocoded <- geocode(dtxx, address = addr, limit = 5, full_results = T, return_input = F, flatten = F, method = "google")

## google
dt_geocoded2 <- geocode(dtxx, address = addr, limit = 5, full_results = T, return_input = F, method = "google") %>% adt




gwd_geocode_dt <- function(dtx) {

    ## set up sqlite


    ## map 


}

NODB_GEOCODE <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode.sqlite"

gc_geocode_cfg <- function() {
    #' specify the setup of different geocoding methods
    

    ## base geocode settings: if I wanna change them, I'll have to add them to all the methods
    c_base <- list(address = quote(addr), limit = 5, full_results = T,
                   return_input = F, # needed for limit > 1
                   mode = "single")
    
    
    lc_varying <- list("google" = list(method = "google"),
                       "osm" =    list(method = "osm"),
                       "arcgis" = list(method = "arcgis"),
                       "census" = list(method = "census"))

    ## combine: still gives access to all the names
    map(lc_varying, ~c(.x, c_base)) 

    

    
}



nodb_creator <- function(dbname, container_key) {
    src <- src_sqlite(dbname)

    docdb_create(src, container_key, value = NULL)
}


nodb_inserter <- function(dbname, container_key, data) {
    src <- src_sqlite(dbname)

    docdb_create(src, container_key, data)
}


gwd_geocode_chunker <- function(data_to_geocode_chunk, container_key, dbname) {
    #' geocode a chunk of data and insert it to the database
    Sys.sleep(0.5)
    ## get geocode settings and merge with actual geocoded data
    l_args <- gc_geocode_cfg() %>% chuck(container_key) %>% c(list(.tbl = data_to_geocode_chunk))
    
    ## actual geocoding
    dt_geocoded <- do.call(geocode, l_args)

    ## merge IDs back: addr gets returneda as address %>% can return it
    dt_geocoded_wid <- merge(dt_geocoded, data_to_geocode_chunk[, .(ID, address = addr)], on = "address")

    
    print(dt_geocoded_wid)
    nodb_inserter(dbname, container_key, dt_geocoded_wid)

    return(invisible(T))

}


gwd_geocode <- function(dt_to_geocode, container_key, dbname) {

    ## get existing data
    src_nosql <- src_sqlite(dbname)
    src_sql <- dbConnect(SQLite(), dbname)

    ## check which IDs are already there
    cmd_IDs_present <- sprintf("select %s.json ->> 'ID' as ID from %s", container_key, container_key)
    dt_IDs_present <- dbGetQuery(src_sql, cmd_IDs_present) %>% adt
    dt_to_geocode_filtered <- dt_to_geocode[!dt_IDs_present, on = "ID"]

    print(sprintf("data size: %s, already coded: %s, left to code: %s",
                  dt_to_geocode[, .N], dt_IDs_present[, .N], dt_to_geocode_filtered[, .N]))

    ## split into chunks
    l_dt_to_geocode <- split(dt_to_geocode_filtered, 1:dt_to_geocode[, (.N/5)])

    map(l_dt_to_geocode, gwd_geocode_chunker, container_key, dbname)
    
    ## get arguments
    

}

system("rm /home/johannes/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode.sqlite")
map(names(gc_geocode_cfg()), ~nodb_creator(NODB_GEOCODE, .x))

nodb_creator(NODB_GEOCODE, "osm")
nodb_inserter(NODB_GEOCODE, "osm", l_dt_geocoded_prep2$osm)

gwd_geocode(dt_tomap, "osm", NODB_GEOCODE)

dt_tomap <- dt_pmdb[museum_status %in% c("private museum", "closed") & iso3c == "USA"] %>% 
    .[sample(1:.N, 13), .(ID, name, city, country = iso3c,
                          addr = sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name")))]


map(names(gc_geocode_cfg()), ~gwd_geocode(dt_tomap, .x, NODB_GEOCODE))




