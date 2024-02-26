## * PMDB geocoding and merging back script
## needs manual copying with view_xl


## ** libs, data


library(pmdata)
library(countrycode)
library(collapse)
library(purrr)
library(jtls)
library(countrycode)
library(tidygeocoder)
library(collapse)

options(width = 115)



PMDATA_LOCS <- gc_pmdata_locs()

dt_pmdb_prep <- gd_pmdb_excl(only_pms = F)

## dt pmdb to geo-code
dt_pmdb_tgc <- dt_pmdb_prep[, .(ID, name, iso3c, city = Museum_city)] %>%
    .[, addr := sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name"))]

## ** actual geocoding

## dt pmdb geocoded
## naive method, doesn't work well since ID not retained
dt_pmdb_gcd <- geocode(atb(dt_pmdb_tgc), addr, method = "google", limit = 5,
                       full_results = T, return_input = F)


FILE_GEOCODE_RES <- "/home/johannes/Dropbox/phd/pmdata/inst/manual_munging/google_geocode_res.Rds"

saveRDS(dt_pmdb_gcd, FILE_GEOCODE_RES)




join(dt_pmdb_gcd, dt_pmdb_tgc, on = c(address = "addr")) %>% adt %>% .[, .N, ID]

## dt_pmdb_gcd2 <- dt_pmdb_tgc[1:5, geocode(atb(.SD), addr, limit = 5, full_results = T, return_input = F), .I]




## ** copying back
