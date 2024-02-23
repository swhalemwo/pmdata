library(pmdata)
library(countrycode)
library(collapse)
library(purrr)

options(width = 115)

PMDATA_LOCS <- gc_pmdata_locs()

dt_pmdb_prep <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb

dt_pmdb <- dt_pmdb_prep[, .(ID, name, museum_status, iso3c, city)] %>%
    .[museum_status %in% c("private museum", "closed")]

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
    


## ** MOW FUN
