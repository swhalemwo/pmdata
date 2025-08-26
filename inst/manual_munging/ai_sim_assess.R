NODB_GEOCODE_NCCS <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_nccs.sqlite"
NODB_GEOCODE_AF <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_artfacts.sqlite"



gd_coords <- function(db_name, container, dt_info) {
    
    src2 <- dbConnect(SQLite(), db_name)

    dt_coords <- dbGetQuery(src2, sprintf("select ID, lat, long from %s_flat", container)) %>% adt %>% na.omit %>% 
        st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>% adt

    merge(dt_info, dt_coords, by = "ID")
}

PMDATA_LOCS <- gc_pmdata_locs()
    
dt_nccs_artmuem_full <- gd_nccs_muem()[nteecc == "A51"]
dt_nccs_artmuem <- gd_nccs_muem()[nteecc == "A51", tail(.SD, 1), ein]

dt_nccs_artmuem_min <- dt_nccs_artmuem %>% copy %>% .[, .(ID = ein, name)] # minimal 


dt_nccs_info <- gd_coords(NODB_GEOCODE_NCCS, "google_waddr", dt_nccs_artmuem_min) %>%
    setnames(old = names(.), new = paste0(names(.), "_nccs"))

dt_af_instns_us <- gd_af_instns() %>% .[Country == "United States" & InstitutionType != "Private Galleries"] %>%
    .[, .(ID, Name)]

dt_af_info <- gd_coords(NODB_GEOCODE_AF, "google", dt_af_instns_us) %>%
    setnames(old = names(.), new = paste0(names(.), "_af")) %>% .[]

library(gtools, include.only = "permutations")

## old permutations: too expensive
## dt_af_cpnts <- dt_af_info %>% head(10) %>% 
##     .[, .(pob_name  = strsplit(Name_af, " ")[[1]] %>%
##               permutations(n=uniqueN(.), r= uniqueN(.), v = .) %>%
##               apply(1, paste0, collapse = " ")), ID_af]


library(stringdist)
l_stringdist_methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex") %>%
    setNames(.,.)

imap(l_stringdist_methods, ~stringdist("some test here", "test some", method = .x))
imap(l_stringdist_methods, ~stringdist("test some", "some test here", method = .x))

merge(dt_af_info[1:10], dt_nccs_info[1:10], by = 1)

## set up grid
lenx <- 5e3
## lenx <- max(
dt_grid <- CJ(ID_af = dt_af_info[1:lenx, ID_af], ID_nccs = dt_nccs_info[1:lenx, ID_nccs]) %>%
    merge(dt_af_info, by = "ID_af") %>%
    merge(dt_nccs_info, by = "ID_nccs")




l_qs <- 1:5

t1 = Sys.time()
dt_grid[, paste0("strdist_jac", l_qs)
        := (map(l_qs, ~stringdist(tolower(Name_af), tolower(name_nccs), method = "jaccard", q = .x))), ID_nccs]
t2 = Sys.time()
dt_grid[, .N*5]/as.numeric(t2-t1)

dt_grid[, geodist := st_distance(geometry_af, geometry_nccs, by_element = T) %>% set_units("km") %>% drop_units]


dt_grid %>% head %>% adf



dt_grid_subset <- dt_grid[strdist_jac1 < 0.3 & geodist < 5] %>% copy


dt_grid_subset[, nbr_candidates := .N, .(ID_af)]

dt_grid_subset[nbr_candidates == 1]
dt_grid_subset[nbr_candidates > 5]


dt_grid_subset[nbr_candidates == 1, .(name_nccs, Name_af, strdist_jac1, geodist)]

## hermitage
dt_grid_subset[ID_nccs == 10769997, .(name_nccs, Name_af, strdist_jac1, geodist)]


dt_grid_subset[grepl("joslyn", ignore.case = T, Name_af), .(ein, Name_af, name_nccs)]

dt_nccs_artmuem_full[ein %in% c(470384577, 470835035), .(year, styear, name, ein)] %>% print(n=80)


dt_grid_subset[ID_af == 6805] ## single

dt_grid_subset[ID_af == 7345, .(Name_af, name_nccs, strdist_jac1, geodist, ID_nccs, ID_af)] %>% print(n=80)## many

dt_grid_subset[ID_af == 7345, .(Name_af, name_nccs, strdist_jac1, geodist, ID_nccs, ID_af)] %>% view_xl

dt_example <- dt_grid_subset[ID_af == 7345, .(name_nccs, strdist_jac1, geodist, ID_nccs)]

library(ellmer)




gwd_aisimas <- function(dt, prompt) {
    #' ai similarity assessment

    ## options(width = 500)
    ## dt_to_attach <- capture.output(print(adf(dt)), sep = "\n")
    str_dt <- paste(capture.output(fwrite(dt, file = "")), collapse = "\n")
    ## write.csv(dt_t

    prompt <- "you are a very hard-working and capable research assistant. you're goal is to match organizations from two different datasets. you will see a table, which includes information on the candidates. in particular, the first column is the same for every row, this is the organization name in dataset AF, for which we want to find the equivalent in dataset NCCS, which is in the second column. your task is to see which rows are most similar (i.e. which organization name in the second column is the match for the name in the first column). Names will often not be identical strings, but have some differences, so keep that in mind. Column 3 has an assessment of string distance (smaller values means more similar strings, and column 4 has a measure of geographical distance). they can help you as well. column 5 and 6 have the IDs of the organizations. Return only the ID from the 5th column that corresponds to the row which matches the first column. if there is no clear match, return the string 'no match'. The table follows now:"

    
    
}


gwd_aisimas(dt_example)
