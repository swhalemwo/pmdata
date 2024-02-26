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
stop("don't accidentally run stuff that might cost moneyz")
FILE_GEOCODE_RES <- "/home/johannes/Dropbox/phd/pmdata/inst/manual_munging/google_geocode_res.Rds"
dt_pmdb_gcd <- geocode(atb(dt_pmdb_tgc), addr, method = "google", limit = 5,
                       full_results = T, return_input = F)
saveRDS(dt_pmdb_gcd, FILE_GEOCODE_RES)


## ** post-processing

## get IDs back, start dealing with multiple matches
dt_pmdb_gcd2 <- join(dt_pmdb_gcd, dt_pmdb_tgc, on = c(address = "addr")) %>% adt %>%
    .[, nbr_addr := .N, ID]

## deal with multiple matches
dt_pmdb_gcd2[, .N, nbr_addr] # check how many affected: 73

## small deviation: just take first one
dt_pmdb_small_dev <- dt_pmdb_gcd2[nbr_addr > 1, .(lat_sd = sd(lat), long_sd = sd(long), .N), ID] %>% # print(n=300)
    .[lat_sd < 0.01 & long_sd < 0.01]

dt_pmdb_large_dev <- dt_pmdb_gcd2[nbr_addr > 1, .(lat_sd = sd(lat), long_sd = sd(long), .N), ID] %>% # print(n=300)
    .[not(lat_sd < 0.01 & long_sd < 0.01)]

## dt_pmdb_prep[dt_pmdb_large_dev, on = "ID"][, .N, museum_status]


## check errors: only one PM -> add to manual exceptions
join(dt_pmdb_gcd2[is.na(lat)], dt_pmdb_prep[, .(ID, museum_status)]) %>% # .[, .N, museum_status]
    .[museum_status == "private museum"]


## merge together different sets into google geocoded
dt_pmdb_ggcd <- rbind(
    dt_pmdb_gcd2[nbr_addr == 1 & !is.na(lat)],
    dt_pmdb_gcd2[dt_pmdb_small_dev, on = "ID", .SD[1], ID]) 

## ** testing
library(plotly)
library(sf)
library(ggplot2)
library(maps)


plot <- figure(width = 1600, height = 900, padding_factor = 0) %>%
     ly_map("world", col = "gray") %>%
     ly_points(long, lat, dt_pmdb_gs, size = 5,
               hover = name)

widgetframe::frameWidget(plot,width=1600,height=900)

    
## ** back to google sheets
dt_pmdb_gs <- join(dt_pmdb_prep[, .(ID, name)],
                   dt_pmdb_ggcd[, .(ID, lat, long, formatted_address)], on = "ID") %>%
    .[order(ID)]
view_xl(dt_pmdb_gs)






## messy cases: manually look them up








## dt_pmdb_gcd2 <- dt_pmdb_tgc[1:5, geocode(atb(.SD), addr, limit = 5, full_results = T, return_input = F), .I]




## ** copying back
