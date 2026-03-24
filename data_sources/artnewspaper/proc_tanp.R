library(data.table)
library(jtls)
library(purrr)
library(terra)
library(tidygeocoder)
library(ellmer)





## library(pmdata)
PMDATA_LOCS <- gc_pmdata_locs()

Sys.setenv("GOOGLEGEOCODE_API_KEY" = show_pass_secret("google-geocode-api-key"))
Sys.setenv(GEMINI_API_KEY = show_pass_secret("gemini-api-key"))
