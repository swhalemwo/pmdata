library(data.table)
library(jtls)
library(purrr)
library(terra)
library(tidygeocoder)
library(ellmer)


## overall file to check that gw_tanp_muyr is working
## some purrr functions are assumed to be global, oh well that'd probably break CRAN


## general idea: written down here to not forget it
## records are tanp id, visitors (daily, tota),museum, city, dates (start, end)
## overarching goal: find which records belong to same entity: assign them a id,
## id here is called muci (museum-city, since that's the most detailed info available)
## goal is also to change inputs (raw/struc as little as possible), and do changes with code whereever possible

## process
## first checks cities, if not checked yet, geocode and check overlap, reduce to common spelling
## (esp relevant for washington dc)
## then check muci with same process: see which don't exist, geocode them, check overlap based on geographical and string distance (get candidates and check them manually)
## particularities: muci_id includes city_new, so when new spelling of city is added, it has to be the spelling (and id) of the first muci_id entry, otherwise entries that should be combined will stay unique

## for years 2000-2005 museum numbers are not available, so exhibition numbers are aggregated to museum
## processing of exhibition tables into structured (struc) files also done with gemini

## notes for improvements, in order of importance
## 1. data structure of museum overlap check: would make more sense with separate entity, as currently with pairwise checks for 5 different spelling 5*4/2 = 10 links have to be checked; also leads to spread across files
## 2. use dolt for tracking updates when re-running entire process, now kinda sketchy 
## 3. maybe use some embeddings for names, that's what google geocoding does anyways to identify names
## 4. geocoding function could be memoised, at least within project

## things that worked well
## using city + geocoding to limit candidates helps a lot, geodistance much better than pure string distance
## overall output of museum-year (gd_tanp_muyr) is nice flow, clean output, very shiny demon trap crystal

library(pmdata)
PMDATA_LOCS <- gc_pmdata_locs()


dtx <- gw_tanp_muyr()
dtx[, .N, year] %>% print(n=80)

pmdata:::gw_tanp_af_matches()

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))
Sys.setenv(GEMINI_API_KEY = show_pass_secret("gemini-api-key"))
