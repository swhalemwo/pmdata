## * geocoding fun  

library(pmdata)
library(countrycode)
library(collapse)
library(purrr)

options(width = 115)



PMDATA_LOCS <- gc_pmdata_locs()

dt_pmdb_prep <- gd_pmdb_excl(sel = "all") %>% gd_pmdb

dt_pmdb <- dt_pmdb_prep[, .(ID, name, museum_status, iso3c, city)] %>%
    .[museum_status %in% c("private museum", "closed")]

## ** using existing datasets

## read some more data 


DATA_PATH <- "/home/johannes/Dropbox/phd/pmdata/data_sources/geonames/"

## smaller dt: 44k
dt_worldcities <- fread(paste0(DATA_PATH, "worldcities.csv")) %>%
    .[, .(city = city_ascii, lat, lon = lng, iso3c = iso3)]

## larger dt: 146k
dt_geonames <- fread(paste0(DATA_PATH, "geonames-all-cities-with-a-population-1000.csv")) %>%
    .[, .(city = `ASCII Name`, iso2c = `Country Code`, coords = Coordinates, IDGN = `Geoname ID`)] %>%
    .[, c("lat", "lon") := map(tstrsplit(coords, ", "), as.numeric)] %>% 
    .[, `:=`(iso3c = countrycode(iso2c, "iso2c", "iso3c"), iso2c = NULL, coords = NULL)]
    


## join data
## first start with worldcities

## check with cities get matched multiple times: multiple cities exist with same name in country
dt_multmatch_worldcities <- join(dt_pmdb, dt_worldcities, on = .c(iso3c, city), how = "right") %>% na.omit() %>%
    .[, N := .N, ID] %>% .[N > 1, head(.SD, 1), ID]
## these are to be marked as manually later on
## dt_worldcities[city == "Miami" & iso3c == "USA"]

## first match with worldcities
dt_match_worldcities <- join(dt_pmdb, dt_worldcities, on = .c(iso3c, city), how = "left")
## 71.9 are matched, nice

## get those that are not matched by worldcities
dt_nomatch_worldcities <- copy(dt_match_worldcities)[is.na(lat), .(ID, name, museum_status, iso3c, city)]

## see how many of them have multiple matches in geonames
dt_multmatch_geonames <- join(dt_nomatch_worldcities, dt_geonames, on = .c(iso3c, city), how = "right") %>%
    na.omit() %>% .[, N := .N, ID] %>% .[N > 1, head(.SD, 1), ID]


dt_match_geonames <- join(dt_unmatch_worldcities,  dt_geonames, on = .c(iso3c, city))
    


## ** via API

library(tidygeocoder)

## geo-code test: once with name, once just city
dt_pmdb_gct <- dt_pmdb[1:5] %>%
    .[, `:=`(addr_name = sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name")),
             addr_city = sprintf("%s, %s", city, countrycode(iso3c, "iso3c", "country.name")))]


dt_pmdb_gct_name <- geocode(atb(dt_pmdb_gct), addr_name)
dt_pmdb_gct_city <- geocode(atb(dt_pmdb_gct), addr_city)

## seems to be possible to just pass name and get different coords
adt(dt_pmdb_gct_name)[, lat]
adt(dt_pmdb_gct_city)[, lat]

## not sure how certain that model is..
dt_pmdb_gct_dtls <- geocode(atb(dt_pmdb_gct), addr_name, limit =5, return_input = F)

dt_pmdb_gct_dtls <- geocode(atb(dt_pmdb_gct), addr_name, limit =5, full_results = T, return_input = F)


## work around nonsense limit restrictions with DT
geocode(tibble(addr = "Albert Heijn, amsterdam, the netherlands"), addr, limit = 5, return_input = F)

dt_markets <- data.table(addr = c("Albert Heijn, amsterdam, the netherlands",
                                  "Jumbo, Amsterdam, the netherlands"), id = c(1,2)) %>%
    .[, addr2 := addr] %>% atb
    

dt_markets_gcdd <- geocode(dt_markets, addr, limit = 5, return_input = F)


dt_markets %>% adt %>% .[, geocode(atb(.SD), addr, limit = 5, return_input = F), by = id]



## try OSM for closed ones
dt_pmdb_gctc <- dt_pmdb[museum_status == "closed", .SD[1:5]] %>%
    .[, addr_name := sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name"))]

## geo code test closed coded
dt_pmdb_gctcd <- geocode(atb(dt_pmdb_gctc), addr_name)
                           

## geocode argics
geocode(atb(dt_pmdb_gctc), addr_name, method = "arcgis", limit = 5, return_input = F)


## ** google api

dt_google_full <- geocode(atb(dt_pmdb_gctc), addr_name, method = "google", limit = 5, return_input = F, full_result = T)

dt_markets_gcdd <- geocode(dt_markets, method = "google", addr, limit = 5, return_input = F)

adt(dt_google_full)[4] %>% adf


## ** explore feasibility of using boundaries as proxy for size
FILE_GEOCODE_RES <- "/home/johannes/Dropbox/phd/pmdata/inst/manual_munging/google_geocode_res.Rds"
dt_pmdb_gcd <- readRDS(FILE_GEOCODE_RES)

dt_pmdb_gcd %>% adt %>% .[, .N, geometry.location_type]
## ROOFTOP                  439
## APPROXIMATE              111
## GEOMETRIC_CENTER          83
## NA                        50

## check boundaries -> very few boundaries (~120), but viewports
adt(dt_pmdb_gcd)[, map(.SD, ~sum(is.na(.x)))] %>% melt(measure.vars = names(.)) %>% print(n=300)


## get viewport 
adt(dt_pmdb_gcd)[1, .SD, .SDcols = keep(names(dt_pmdb_gcd), ~grepl("geometry.viewport", .x))] %>%
    melt(measure.vars = names(.)) %>% .[, coords := as.character(value)]


gm_viewport <- function(lat_NE, long_NE, lat_SW, long_SW) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generatei measure of size of viewport
    df_coords <- matrix(c(lat_NE, long_NE,
             lat_NE, long_SW,
             lat_SW, long_SW,
             lat_SW, long_NE,
             lat_NE, long_NE),
             ncol = 2, byrow = T) %>% adt %>% setnames(c("lat", "long")) %>% as.matrix %>% list

    st_polygon(df_coords) %>% st_sfc(crs = 4326) %>% st_area %>% as.numeric

}

## measure viewport size
dt_pmdb_size <- adt(dt_pmdb_gcd) %>%
    .[geometry.location_type == "ROOFTOP",
      size_viewport := gm_viewport(geometry.viewport.northeast.lat, geometry.viewport.northeast.lng,
                                   geometry.viewport.southwest.lat, geometry.viewport.southwest.lng), .I]
      
## evaluate viewport sizes:
## some super yuge ones, around 50% around 90k m^2

dt_pmdb_size$size_viewport %>% hist(breaks = 300)

dt_pmdb_size[size_viewport < 5e5, size_viewport] %>% hist(breaks = 100)

dt_pmdb_size[size_viewport > 500000, .(address, lat, long, size_viewport)]
dt_pmdb_size[!is.na(size_viewport), .N]
             
dt_pmdb_size[not(size_viewport > 8e4 & size_viewport < 1e5), .(address, lat, long, size_viewport)]


## recheck boundaries
dt_pmdb_size[!is.na(geometry.bounds.southwest.lat) & geometry.location_type == "ROOFTOP"]
dt_pmdb_size[!is.na(geometry.bounds.southwest.lat), .N, geometry.location_type]

dt_pmdb_size[!is.na(geometry.bounds.southwest.lat), .(formatted_address)] %>% print(n=300)
