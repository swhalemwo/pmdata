library(purrr)
library(pmdata)
library(jtls)

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))
Sys.setenv("GEOCODIO_API_KEY" = show_pass_secret("geocodio-api-key"))

gc_geocode_cfg <- function() {
    #' specify the setup of different geocoding methods    

    ## base geocode settings: if I wanna change them, I'll have to add them to all the methods

    c_base <- list(limit = 5, full_results = T,
                   return_input = F, # needed for limit > 1
                   mode = "single")
    
    
    lc_varying <- list(
        "google" =   list(method = "google", address = "addr_wname"),
        "geocodio_pobox" = list(method = "geocodio", address = "addr_pobox"))
        ## "geocodio_name" = list(method = "geocodio", address = quote(addr_wname)))
        
    map(lc_varying, ~c(.x, c_base))
}


PMDATA_LOCS <- gc_pmdata_locs()

dt_nccs_muem <- gd_nccs_muem()[nteecc == "A51"]

## check where name changes but EIN stays same
## dt_nccs_muem %>% copy %>% .[, nbr_names := uniqueN(name), ein] %>% .[nbr_names >1] %>%
##     .[order(ein)] %>% .[, .(ein, name, nbr_names)] %>% print(n=150)

## ## check where EIN changes but name stays same
## dt_nccs_muem %>% copy %>% .[, nbr_ein := uniqueN(ein), name] %>% .[nbr_ein >1] %>%
##     .[order(name)] %>% .[, .(ein, name, nbr_ein)] %>% print(n=150)

## ## get multiple IDs for moved stuff
## dt_nccs_muem_tomap <- dt_nccs_muem[, .(ein, name, address, city, state, zip)] %>% unique %>%
##     .[, addr_nbr := 1:.N, ein] %>%
##     .[, .(ID = sprintf("%s__%s", ein, addr_nbr),
##           ein,
##           addr_wname = sprintf("%s, %s, %s, USA", name, city, state),
##           addr_pobox = sprintf("%s, %s, %s, %s, USA", address, city, state, zip))]

## dt_nccs_muem_tomap[, uniqueN(addr_wname)]
## dt_nccs_muem_tomap[, uniqueN(addr_pobox)]
## dt_nccs_muem_tomap[, uniqueN(ID)]

dt_nccs_muem_tomap <- dt_nccs_muem[, tail(.SD,1), ein] %>%
    .[, .(ID = ein,
          addr_wname = sprintf("%s, %s, %s, USA", name, city, state),
          addr_pobox = sprintf("%s, %s, %s, %s, USA", address, city, state, zip))]

## dt_nccs_muem_tomap %>% copy %>% .[, nbr_ein := .N, addr_wname] %>% .[nbr_ein > 1]
    


## ## get some museum where location changed
## dt_nccs_muem_tomap %>% copy %>% .[, nbr_locs := uniqueN(addr_wname), ein] %>%
##     .[nbr_locs > 1] %>% .[order(-ein)]


NODB_GEOCODE <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_nccs.sqlite"

map(names(gc_geocode_cfg()), ~nodb_creator(NODB_GEOCODE, .x))

map(names(gc_geocode_cfg()), ~gwd_geocode(dt_nccs_muem_tomap, .x, NODB_GEOCODE))




## dt_test_zip4 <- dt_muem[nteecc == "A51", .(address, city, state, zip)] %>% unique %>% head(10)
## dt_test_zip4 %>% view_xl

## dt_test_zip4[, addr := sprintf("%s, %s, %s, %s, USA", address, city, state, zip)]

## dt_test_zip4_coded <- geocode(dt_test_zip4, address = "addr", limit = 5, full_results = T, return_input = F,
##                               mode = "single", method = "geocodio")




## dt_test_zip4_gui <- fread("~/Downloads/zip4_test_geocodio_d7072da305f2aaf6d3cced1bd8d1f213c19b91a8.csv") %>%
##     .[, .(lat = `Geocodio Latitude`, long = `Geocodio Longitude`)]
    

## cbind(dt_test_zip4_coded[, head(.SD, 1), address, .SDcols = c(lat_api = "lat", long_api = "long")],
##       dt_test_zip4_gui[, .(lat_gui = lat, long_gui = long)]) %>% 
##     .[, `:=`(diff_lat = lat_gui - lat_api, diff_long = long_gui - long_api)] %>%
##     .[, sprintf("%s %s", diff_lat, diff_long)]
