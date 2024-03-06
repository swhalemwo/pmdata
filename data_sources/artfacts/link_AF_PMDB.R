

library(pmdata)
library(countrycode)
library(collapse)
library(purrr)

options(width = 115)

PMDATA_LOCS <- gc_pmdata_locs()
dt_df_instns <- pmdata::gd_af_instns()
dt_pdmb <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb






