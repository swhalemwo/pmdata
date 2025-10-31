library(jtls)
library(pmdata)
library(purrr)

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))
Sys.setenv("LOCATIONIQ_API_KEY" = show_pass_secret("location-iq-token"))
Sys.setenv("GEOCODIO_API_KEY" = show_pass_secret("geocodio-api-key"))
           


gc_geocode_cfg <- function() {
    #' specify the setup of different geocoding methods
    

    ## base geocode settings: if I wanna change them, I'll have to add them to all the methods
    c_base <- list(address = quote(addr), limit = 5, full_results = T,
                   return_input = F, # needed for limit > 1
                   mode = "single")
    
    
    lc_varying <- list(
        "google" =   list(method = "google")
        ## "osm" =      list(method = "osm"),
        ## "arcgis" =   list(method = "arcgis"),
        ## "census" =   list(method = "census"),
        ## "iq"     =   list(method = "iq"),
        ## "geocodio" = list(method = "geocodio")
    )

    ## combine: still gives access to all the names
    map(lc_varying, ~c(.x, c_base)) 

}


## get data to code
PMDATA_LOCS <- gc_pmdata_locs()

dt_af_instns <- gd_af_instns()
## dt_af_instns_tomap <- dt_af_instns[Country == "United States" & InstitutionType != "Private Galleries"] %>%
##     .[, .(ID, addr = sprintf("%s, %s, %s", Name, City, Country))]



## set up 
NODB_GEOCODE <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_artfacts.sqlite"
map(names(gc_geocode_cfg()), ~nodb_creator(NODB_GEOCODE, .x))

library(RSQLite)

db_af <- dbConnect(SQLite(), NODB_GEOCODE)
dt_af_already_geocoded <- dbGetQuery(db_af, "select ID, lat, long from google_flat") %>% adt

dt_af_instns_tomap <- dt_af_instns[InstitutionType %in% c("Public Institutions", "Non-profit organizations")] %>%
    .[!dt_af_already_geocoded, on = "ID"] %>% head(9800) %>%
    .[, .(ID, addr = sprintf("%s, %s, %s", Name, City, Country))]

## merge(dt_af_instns, dt_af_already_geocoded, by = "ID") %>% summary



## map(names(gc_geocode_cfg()), ~gwd_geocode(dt_af_instns_tomap[1:3], .x, NODB_GEOCODE))
map(names(gc_geocode_cfg()), ~gwd_geocode(dt_af_instns_tomap, .x, NODB_GEOCODE))
map(names(gc_geocode_cfg()), ~gwd_flat_geocode(.x, NODB_GEOCODE))


