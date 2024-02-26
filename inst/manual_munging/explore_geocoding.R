## * geocoding fun  

library(pmdata)
library(countrycode)
library(collapse)
library(purrr)

options(width = 115)



PMDATA_LOCS <- gc_pmdata_locs()

dt_pmdb_prep <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb

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
