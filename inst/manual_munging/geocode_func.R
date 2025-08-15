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
                                   \(col) lapply(col, class)) %>% rbindlist
                
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


gen_extra_col_schema <- function(col_vlu, col_prefix, id_vlu) {
    #' idea is that every non-standard column gets its own table
    #' need to see if schemas are all the same, then set up table
    
    ## check if entire col_vlu can be easily parsed: no list columns
    ## check whether column is vector

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
        dbSendQuery(dbx, cmd_setup)
    }
    
    if (len(l_parse_true) > 0) {    
    
        ## data.frame case: get the parseable columns
        ## currently using data.frame (is what google api returns), but maybe should use DT since more general
        l_schemas <- map(col_vlu, ~dbDataType(dbx, .x[l_parse_true]))

        if (len(unique(l_schemas))>1) {stop("schemas not unique")}
    
        cmd_setup <- sprintf("CREATE TABLE %s (ID %s, \n%s)\n", col_prefix,
                             dbDataType(dbx, id_vlu),
                             paste0(imap(l_schemas[[1]], ~sprintf("%s %s", .y, .x)), collapse = ",\n"))

        cat(cmd_setup)
        dbSendQuery(dbx, cmd_setup)
    }
    
    ## dealing with non-parseable things
    ## can i use recursion? 
    # i think I have to combine them first tho: now have 7 dfs, but that should only be 1 table
    ## col_vlu[[1]]
    if (len(l_parse_false) > 0) {
    
        l_index_nonnull <- !sapply(col_vlu, is.null) # get non-null entries

        ## need to make list to assign ID to each entry
        l_dt_parsed <- map2(col_vlu[l_index_nonnull], id_vlu[l_index_nonnull],
                            ~{.x$ID <- .y;
                                ## row.names(.x) <- NULL; # yeet rownames, probably not needed
                                .x[c("ID", l_parse_false)]})

        ## get datatypes to see whether I can use rbind or not
        l_datatypes <- lapply(col_vlu[l_index_nonnull], \(row) map(row, class)) %>% unlist
        if (!any(l_datatypes == "data.frame")) {

            ## this works if there are no dfs in the columns

            dt_cbn <- Reduce(rbind, l_dt_parsed) %>% adt

        } else {

            ## if there are dfs, i can't just use rbind but need to re-assemble

            ## reassemble by changing to cols: map for each column, extract its value from all the rows
            l_cols <- lapply(c("ID", l_parse_false), \(colx) map(l_dt_parsed, ~chuck(.x, colx)))
                        
            ## this is what to get to, it seems to work 
            ## dt_cbn <- data.frame(ID = unlist(l_cols[1]),
            ##                      location = I(l_cols[[2]])) %>% adt
            
            ## do this automatically:
            ## need a do.call -> need a list of arguments
            ## i just need the number of the column, then can use setNames to add the column name

            ## first set up ID column (number 1)
            l_colargs <- list("ID" = unlist(l_cols[1]))

            ## then add the others with I()
            l_col_noparse_prep <- imap(l_parse_false, ~I(l_cols[[.y +1]])) %>% setNames(l_parse_false)

            l_col_noparse_cbn <- c(l_colargs, l_col_noparse_prep)

            dt_cbn <- do.call(data.frame, l_col_noparse_cbn) %>% adt
        }
        
        ## gen_extra_col_schema(dt_cbn[, types], colname_new, dt_cbn[, ID])

        map(l_parse_false, ~gen_extra_col_schema(dt_cbn[, get(.x)],
                                                 col_prefix = paste0(col_prefix, "_", .x),
                                                 id_vlu = dt_cbn[, ID]))
        
    }

}

dt_geocoded3 <- dt_geocoded2 %>% copy %>% .[, ID := 1:7]

system("rm /home/johannes/Dropbox/phd/pmdata/inst/manual_munging/db_geocode.sqlite")
dbx <- dbConnect(SQLite(), DB_GEOCODE)

dt_geocoded3[, imap(.SD, ~gen_extra_col_schema(.x, .y, ID)), .SDcols = l_vrbls]

gen_extra_col_schema(dt_geocoded2[, address_components], "address_components", 1:7)
gen_extra_col_schema(dt_geocoded2[, types], "types", 1:7)
gen_extra_col_schema(dt_geocoded2[, navigation_points], "navigation_points", 1:7)

gen_extra_col_schema(dt_geocoded2, "geocoded2", 1:7)

dt_geocoded2[, as.data.table(.SD), .I]

dtx <- data.table(xx = split(dt_geocoded2, 1:dt_geocoded2[, .N]))
gen_extra_col_schema(dtx, "google", 1:7)


gl_assess_dt_struc(dt_geocoded2[, address_components], verbose = T)
gl_assess_dt_struc(dt_geocoded2[, types], verbose = T)
gl_assess_dt_struc(dt_geocoded2[, navigation_points], verbose = T)



gwd_geocode_dt <- function(dtx) {

    ## set up sqlite


    ## map 


}
