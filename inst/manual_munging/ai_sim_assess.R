library(tidygeocoder)
library(data.table)
library(DBI)
library(RSQLite)
library(sf)
library(units)
library(ggridges)
library(pmdata)
library(jtls)
library(purrr)
library(texreg)
library(gbm)
library(xgboost)
library(fixest)
library(ellmer)
library(caret)

NODB_GEOCODE_NCCS <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_nccs.sqlite"
NODB_GEOCODE_AF <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_artfacts.sqlite"



gd_coords <- function(db_name, container, dt_info) {
    
    src2 <- dbConnect(SQLite(), db_name)

    dt_coords <- dbGetQuery(src2, sprintf("select ID, lat, long from %s_flat", container)) %>% adt %>% na.omit %>% 
        st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>% adt

    merge(dt_info, dt_coords, by = "ID")
}

PMDATA_LOCS <- gc_pmdata_locs()

gd_grid_new <- function() {
    #' generate data for which I want to get matches
    #' here it's AF and NCCS 

    dt_nccs_artmuem_full <- gd_nccs_muem()[nteecc == "A51"]
    dt_nccs_artmuem <- gd_nccs_muem()[nteecc == "A51", tail(.SD, 1), ein]

    dt_nccs_artmuem_min <- dt_nccs_artmuem %>% copy %>% .[, .(ID = ein, name)] # minimal 


    dt_nccs_info <- gd_coords(NODB_GEOCODE_NCCS, "google_waddr", dt_nccs_artmuem_min) %>%
        setnames(old = names(.), new = paste0(names(.), "_nccs"))

    dt_af_instns_us <- gd_af_instns() %>% .[Country == "United States" & InstitutionType != "Private Galleries"] %>%
        .[, .(ID, name = Name)]

    dt_af_info <- gd_coords(NODB_GEOCODE_AF, "google", dt_af_instns_us) %>%
        setnames(old = names(.), new = paste0(names(.), "_af")) %>% .[]



    ## set up grid
    lenx <- 5e3
    ## lenx <- max(
    dt_grid <- CJ(ID_af = dt_af_info[1:lenx, ID_af], ID_nccs = dt_nccs_info[1:lenx, ID_nccs]) %>%
        merge(dt_af_info, by = "ID_af") %>%
        merge(dt_nccs_info, by = "ID_nccs")

    return(dt_grid)
}


## ** AI matching


gwd_aisimas <- function(dt, prompt) {
    #' ai similarity assessment

    ## options(width = 500)
    ## dt_to_attach <- capture.output(print(adf(dt)), sep = "\n")
    str_dt <- paste(capture.output(fwrite(dt, file = "")), collapse = "\n")
    ## write.csv(dt_t

    prompt <- "you are a very hard-working and capable research assistant. you're goal is to match organizations from two different datasets. you will see a table, which includes information on the candidates. in particular, the first column is the same for every row, this is the organization name in dataset AF, for which we want to find the equivalent in dataset NCCS, which is in the second column. your task is to see which rows are most similar (i.e. which organization name in the second column is the match for the name in the first column). Names will often not be identical strings, but have some differences, so keep that in mind. Column 3 has an assessment of string distance (smaller values means more similar strings, and column 4 has a measure of geographical distance). they can help you as well. column 5 and 6 have the IDs of the organizations. Return only the ID from the 5th column that corresponds to the row which matches the first column. if there is no clear match, return the string 'no match'. The table follows now:"

    
    
}


gwd_aisimas(dt_example)


## ** training for matching
## combine MOW and PMDB, and AF and PMDB


## first PMDB





## ** xgboost





## collecting new data to be matched
dt_grid_new <- gd_grid_new()

## add distances based on coordinates--
dt_grid[, geodist := st_distance(geometry_af, geometry_nccs, by_element = T) %>% set_units("km") %>% drop_units]




## training model
dt_grid_blank <- gd_grid_train(size_nomatch = 1e3)
dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank, "name_pmdb", "name_tgt")

## dt_grid_wfeat[, .SD[sample(1:.N, size = 5)], match] %>% write.csv(file = "")
    
## dt_grid_blank[tolower(name_pmdb) == tolower(name_tgt) & match == 0]
dt_grid_blank[, .N, match]







# Train the model



## dt_grid_test[match_pred_num > 0.5, .(name_pmdb, name_tgt, match)] %>% print(n=80)
# howy smokes

dt_grid_test[match == 1 & match_pred_num < 0.5, .(name_pmdb, name_tgt, match, match_pred)] %>% print(n=80)



    


r_xgb_smol <- gr_xgb_smol(r_xgb)


dt_grid_bu <- dt_grid %>% copy %>% .[1:1e4]

dt_grid_smol <- gd_dt_smol(dt_grid_blank = dt_grid, r_xgb_smol = r_xgb_smol, "name_af", "name_nccs", thld = 0.0001)

dt_grid_fullfeat <- gd_dt_smol(dt_grid_smol, r_xgb, "name_af", "name_nccs", thld = 0.01)



dt_grid_fullfeat[match_pred > 0.1][, .(name_af, name_nccs, match_pred)] %>% print(n=80)



dt_grid_fullfeat[tolower(name_af) == tolower(name_nccs), .(name_af, name_nccs, match_pred)]

dt_grid_fullfeat[match_pred > 0.5]
dt_grid[tolower(name_af) == tolower(name_nccs)]
