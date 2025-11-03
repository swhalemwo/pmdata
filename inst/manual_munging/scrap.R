Reduce(\(x) rbind(I(x)), l_dt_parsed)
Reduce(\(x, y) rbind(I(x), I(y)), l_dt_parsed)

Reduce(\(x, y) rbind(adt(I(x)), adt(I(y))), l_dt_parsed)



map(l_dt_parsed, adt)
adt(l_dt_parsed[[1]])

l_dt_parsed[[1]] %>% ncol
l_dt_parsed[[1]] %>% str

nested_df1 <- data.frame(latitude = 43.8, longitude = 10.5)
nested_df2 <- data.frame(latitude = 46.4, longitude = 13)

                                        # Create a list containing the nested data frames
nested_list <- list(nested_df1, nested_df2)

                                        # Create the main data frame with an ID column and a list column
main_df <- data.frame(
    ID = c(3, 4),
    location = I(nested_list)  # Use I() to inhibit unlisting
)


rbind(data.frame(a = 1, b = list(1,2)), data.frame(a = 3, b = list(4,5)))

data.table(a = 3, b = c(4,5))
data.table(a=1:2, b=list(1:5,1:10))

rbind(data.table(a=1, b=list(1:5)), data.table(a=1, b=list(1:5)))


## * old sqlite geocoding prep

map(dt_geocoded2, class) %>% as.vector
sapply(dt_geocoded2, class) %>% keep(~.x == "list") %>% names
l_vrbls <- sapply(dt_geocoded2, class) %>% keep(~.x == "list") %>% names

dt_geocoded2[, .SD, .SDcols = l_vrbls]

dt_geocoded2[, navigation_points]


dt_geocoded_proc <- merge(adt(dt_geocoded), dtxx[, .(ID, address = addr)], by = "address")







dt_geocoded3 <- dt_geocoded2 %>% copy %>% .[, ID := 1:7]



dt_geocoded3[, imap(.SD, ~gen_extra_col_schema(.x, .y, ID)), .SDcols = l_vrbls]


## testing of geocoding subcomponents
gen_extra_col_schema(dt_geocoded2[, address_components], "address_components", 1:7)
gen_extra_col_schema(dt_geocoded2[, types], "types", 1:7)
gen_extra_col_schema(dt_geocoded2[, navigation_points], "navigation_points", 1:7)

gen_extra_col_schema(dt_geocoded2, "geocoded2", 1:7)

dt_geocoded2[, as.data.table(.SD), .I]

dtx <- data.table(xx = split(dt_geocoded2, 1:dt_geocoded2[, .N]))
dtx <- data.table(xx = split(adf(dt_geocoded2), 1:dt_geocoded2[, .N]))
gen_extra_col_schema(dtx[, xx], "google", 1:7)


## testing structure generation
gl_assess_dt_struc(dt_geocoded2[, address_components], verbose = T)
gl_assess_dt_struc(dt_geocoded2[, types], verbose = T)
gl_assess_dt_struc(dt_geocoded2[, navigation_points], verbose = T)




## ** nosql exploration


library(nodbi)
NODB_GEOCODE <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode.sqlite"

src <- src_sqlite(NODB_GEOCODE)

docdb_create(src = src, key = "test", value = l_dt_geocoded_prep2$google)

docdb_query(src, "test", '{"lat" : {"$gt" :30}}')

docdb_query(src, "test", '{"lat" : {"$gt" :30}}', fields = '{"ID" :1, "lat": 1}')

docdb_query(src, "test", '{"lat" : {"$gt" :30}}', fields = '{"ID" :1, "navigation_points": 1}') %>% adt

docdb_create(src = src, key = "test", value = l_dt_geocoded_prep2$osm)

docdb


docdb_get(src, "test")[1:8, c("ID", "address", "lat", "long")]


select test.json ->> 'address' as address
from test, json_each(test.json, '$.ID')


select test.json ->> 'address' as address1, test.json ->> 'lat' as lat
from test

src2 <- dbConnect(SQLite(), NODB_GEOCODE)
dbGetQuery(src2, "select test.json ->> 'address' as address, test.json ->> 'lat' as lat from test")

dbGetQuery(src2, "select test.json ->> 'address' as address, test.json ->> 'lat' as lat from test, json_each(test.json, '$.lat')
where json_each.value > 30")


dbGetQuery(src2, "    PRAGMA table_info(test);")

## get the number of keys in the schema
dbGetQuery(src2, "
    SELECT key, COUNT(*) AS frequency
    FROM (
        SELECT json_each.key
        FROM test
        CROSS JOIN json_each(test.json)  -- Specify the table and column explicitly
    ) AS keys
    GROUP BY key
    ORDER BY frequency DESC;
")

dbGetQuery(src2, "select test.json ->> 'navigation_points' as nav_points from test")

dbGetQuery(src2, "select test.json ->> 'types' as types from test")



## ** geocoding exploration

system("rm /home/johannes/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode.sqlite")
map(names(gc_geocode_cfg()), ~nodb_creator(NODB_GEOCODE, .x))

nodb_creator(NODB_GEOCODE, "osm")
nodb_inserter(NODB_GEOCODE, "osm", l_dt_geocoded_prep2$osm)

gwd_geocode(dt_tomap, "osm", NODB_GEOCODE)

dt_tomap <- dt_pmdb[museum_status %in% c("private museum", "closed") & iso3c == "USA"] %>% 
    .[sample(1:.N, 13), .(ID, name, city, country = iso3c,
                          addr = sprintf("%s, %s, %s", name, city, countrycode(iso3c, "iso3c", "country.name")))]


map(names(gc_geocode_cfg()), ~gwd_geocode(dt_tomap, .x, NODB_GEOCODE))

src <- src_sqlite(NODB_GEOCODE)
src2 <- dbConnect(SQLite(), NODB_GEOCODE)
dbGetQuery(src2, "select google.json ->> 'address' as address, google.json ->> 'lat' as lat from google")

docdb_get(src, key = "arcgis") %>% adt %>% .[ID ==142, .SD, .SDcols = c("lat", "long", "arcgis_address", "address", "score",
                                                                        keep(names(.), ~grepl("name", .x, ignore.case = T)))] %>% adf

                                                                        patterns("name"))]



docdb_get(src, key = "google") %>% adt %>% .[ID == 142] %>% names


## ** geocoding eval

library(nodbi)
library(RSQLite)
library(pmdata)
library(jtls)
library(magrittr)
library(purrr)

PMDATA_LOCS <- gc_pmdata_locs()

dt_af_inst_loc <- gd_af_inst_loc()

dt_af_inst_loc_wnoise <- dt_af_instns_geocoded_google_flat %>% copy %>% 
    .[, c("lat", "long") := map(.SD, ~.x + runif(.N, min = -0.0005, max = 0.0005)), .SDcols = c("lat", "long")]


dt_af_inst_sameloc <- gd_af_inst_sameloc(dt_af_inst_loc)
dt_af_inst_sameloc_clusters <- gd_af_inst_sameloc_clusters(dt_af_inst_sameloc)
    

dt_af_loc_deovlpd <- gd_af_loc_deovlpd(dt_af_inst_loc, dt_af_inst_sameloc_clusters)



## gd_ovlp_shifted_coords(dt_af_inst_loc_wclust, 44)


## *** ugly code below, clean above




src <- src_sqlite(NODB_GEOCODE_AF)



dt_af_instns_tomap <- gd_af_instns()[Country == "United States" & InstitutionType != "Private Galleries"] %>%
    .[, .(ID, addr = sprintf("%s, %s, %s", Name, City, Country))]


## ok this is really fucking slow %>% needs to be flattened
dt_af_instns_geocoded_google <- docdb_get(src, key = "google") %>% adt

dt_af_instns <- gd_af_instns()

dt_af_instns_geocoded_google_flat <- merge(dt_af_inst_loc, dt_af_instns, by = "ID")


dt_dists <- gd_distmat(dt_af_instns_geocoded_google_flat[, .(src = ID, long, lat)])
dt_dists[out != incom & dist < 0.1, .(unq_out = uniqueN(out), unq_incom = uniqueN(incom))]


## check what hasn't been mapped

l_methods <- c("google", "arcgis" ,  "iq", "geocodio") # "census",  "osm")


gd_compare_coords()

## ** leaflet interactive map
library(leaflet)
library(purrr)
options(browser = "surf") # make it use surf

m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer

m = m %>% setView(-93.65, 42.0285, zoom = 17)
m

m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')

m %>% addPopups(-93.60, 42.0285, '==========')


## example of quickly visualizing stuff
dt_af_inst_loc_wnoise <- dt_af_instns_geocoded_google_flat %>% copy %>% 
    ## .[grepl("New York", address)] %>% # .[sample(1:.N, size = 200)] %>%
    ## add some random noise
    .[, c("lat", "long") := map(.SD, ~.x + runif(.N, min = -0.0005, max = 0.0005)), .SDcols = c("lat", "long")]


leaflet(dt_af_inst_loc_wnoise) %>% addTiles() %>% addCircles(lat = ~lat, lng = ~long, label = ~Name) 
# huh looks quite good for quick plotting


## fitBounds # fits rectangle lng1/lat1, lng2/lat2
library(geosphere)
## filtering based on distance

dt_af_inst_loc_wnoise[distHaversine(cbind(long, lat), cbind(-74.0060, 40.7128)) < 50000] %>% # NYC coordinates
    leaflet()  %>% addTiles() %>% addCircles(lat = ~lat, lng = ~long, label = ~Name)
# huh that also works quite well


## graph component identification for non-identified stuff

t1 = Sys.time()
dt_dists <- gd_distmat(dt_af_instns_geocoded_google_flat[1:3000, .(src = ID, long, lat)])
dt_ovlp_distmat <- dt_dists[dist < 1, .(out = as.integer(out), incom = as.integer(incom))][out > incom]
t2 = Sys.time()
t2-t1 # 3.8 secs for 3k entries/9m dists: 2.3m dists/sec

## mx <- dist(dt_af_instns_geocoded_google_flat[1:5, .(long, lat)])



## checks look good
dt_ovlp_base[!dt_ovlp_distmat, on = .(V1 = out, V2 = incom)]
dt_ovlp_distmat[!dt_ovlp_base, on = .(out = V1, incom = V2)]

library(igraph)


dt_af_inst_sameloc <- gd_af_inst_sameloc(dt_af_inst_loc)
dt_af_inst_sameloc_clusters <- gd_af_inst_sameloc_clusters(dt_af_inst_sameloc)
dt_af_inst_sameloc_clusters[, .N, cluster][N > 3]

library(geosphere)

dt_cluster_nyc <- merge(dt_af_inst_loc_wnoise[distHaversine(cbind(long, lat), cbind(-74.0060, 40.7128)) < 50000],
      dt_af_inst_sameloc_clusters, by = "ID", all.x = T) %>%
    setnafill(fill = 0, cols = "cluster") %>%
    .[cluster !=0] %>%
    .[, cluster_memnbr := .N, cluster] %>% 
    ## .[cluster != 0, .(format(lat), format(long), cluster, address, Name)] %>%
    .[order(cluster)]

## dt_cluster_nyc[cluster_memnbr >3, .N, cluster]

library(leaflet)
leaflet(dt_cluster_nyc[cluster_memnbr >3]) %>% addTiles() %>% addCircles(lat = ~lat, lng = ~long, label = ~Name)

dt_cluster_nyc[, .N, .(geometry.location_type, cluster)] %>% print(n=80)

dt_af_inst_loc_wnoise[, .N, geometry.location_type]


## hmm not sure if NY clusters are such an issue, check largest cluster %>% berlin lul
dt_berlin <- merge(dt_af_inst_loc_wnoise, dt_af_inst_sameloc_clusters[cluster==2], by = "ID", all.x = T) %>% 
    .[, .SD[distHaversine(cbind(long,lat), .[cluster==2, cbind(mean(long), mean(lat))]) < 70e3]] %>%
    setnafill(fill = 0, cols = "cluster") %>% # .[, .N, cluster] 
    .[, .(Name, lat, long, cluster)]


leaflet(dt_berlin) %>% # .[, head(.SD,10), cluster] %>% 
    addTiles() %>%
    addCircles(lat = ~lat, lng = ~long, label = ~Name,
        color = ~colorFactor(palette = "RdYlBu", domain = unique(cluster))(cluster),
        opacity = 1, fillOpacity = 0.9, radius = 20)



# dt_ovlp_distmat[, .(node = unique(c(out, incom)))] # looks good


## ** old geocode eval funcs


gd_distmat <- function(dt_pts) {
    ## from set of points, calculate all pairwise distances, put into long dt
    
    l_src <- dt_pts[, src]
    mat_dist <- distm(dt_pts[, .(long, lat)], fun = distHaversine)
    
    colnames(mat_dist) <- l_src
    rownames(mat_dist) <- l_src

    mat_dist %>% adt(keep.rownames = "out") %>%
        melt(id.vars = "out", variable.name = "incom", variable.factor = F, value.name = "dist")
        ## .[, ID := dt_pts[, ID[1]]]
}


distm
    distHaversine

    
xy <- rbind(c(0,0),c(90,90),c(10,10),c(-120,-45))
distm(xy)
xy2 <- rbind(c(0,0),c(10,-10))
distm(xy, xy2)
distHaversine(xy, xy2)
distHaversine(xy)

xy3 <- rbind(c(0,0),c(10,10), c(20,20), c(10,10))
distHaversine(xy, xy3)

sapply(1:4, \(x) distHaversine(xy[x,], xy3[x,])) ## looks good

terra::vect(dt_pmdb[, .(ID, long, lat)], geom = c("long", "lat"), crs = "WGS84")
library(sf)


dt_pt[ID == 1203] %>% .[., on = 'ID', allow.cartesian = T]

st_point

dt_cbn %>% copy %>% .[, st_point(c(long, lat)), .I]


dt_cbn %>% na.omit %>% .[ID == 1203] %>% gd_distmat

dt_distmat <- dt_cbn %>% na.omit %>% .[, gd_distmat(.SD), ID]

dt_distmat[, .(mean_dist_oneway = mean(dist), .N), out]

dt_distmat[, .(mean_dist_twoway = mean(dist)/1e3, .N), .(incom, out)] %>%
    .[out != "iq" & incom != "iq"] %>%
    .[out != incom] %>% 
    ggplot(aes(x=incom, y = out, fill =mean_dist_twoway, label = round(mean_dist_twoway,1))) +
    geom_tile() + geom_text()

dt_distmat %>% # [, .(mean_dist_twoway = mean(dist)/1e3, .N), .(incom, out)] %>%
    .[out != "iq" & incom != "iq"] %>%
    .[out != incom] %>%
    .[incom > out] %>%
    .[dist < 3e4] %>% 
    ggplot(aes(x=dist/1e3, y = interaction(incom, out), fill = interaction(incom, out))) +
    geom_density_ridges(alpha = 0.5, bandwidth = 0.2)
## coord_cartesian(xlim = c(0, 30))


## ** geocoding check mismatches (in gd_compare_coords)

dt_grid_full[dist_km < 5, uniqueN(ID)]
dt_grid_full[dist_km > 100, .N, .(src, src2)]


dt_distmat[dist < 5000 & out != incom, uniqueN(ID)]

dt_distmat[out == "google" & incom == "arcgis", .N, dist < 1e3]

dt_af_instns <- gd_af_instns()


merge(
    dt_grid_full[src == "google" & src2 == "arcgis" & dist_km > 10],
    dt_af_instns, by = "ID") %>% .[, .(ID, Name, geometry, geometry2, dist_km, City)] %>% print(n=80)

                                        # %>% .[, .(Name, City, Country)] 

dt_test <- data.table(addr = "George Eastman Museum, Rochester, NY, United States")
dt_test_coded <- geocode(dt_test, address = "addr", method = "google") %>% adt
dt_test_coded2 <- geocode(dt_test, address = "addr", method = "arcgis", limit = 5, return_input = F,
                          full_results = T) %>% adt
dt_test_coded[, sprintf("%s %s", lat, long)]
dt_test_coded2[, .(xx = sprintf("%s %s", lat, long), score, attributes.Score)]


src2

dbGetQuery(src2, "select google.json ->> 'lat' as lat, google.json ->> 'long' as long from google, json_each(google.json, '$.ID')
where json_each.value = 2452")



dbGetQuery(src2, "select google.json ->> 'lat' as lat from google,
json_each(google.json, '$.lat') wehre json_each.value > 30")

## ** logistic regression approach -> bye bye


r_log1 <- glm(match ~ strdist_jac1, family = binomial, dt_grid)
r_log12 <- glm(match ~ strdist_jac2, family = binomial, dt_grid)
r_log2 <- glm(match ~ strdist_jac1 + strdist_jac2, family = binomial, dt_grid)
r_log3 <- glm(match ~ strdist_jac1 + strdist_jac2 + strdist_jac3, family = binomial, dt_grid)
r_log4 <- glm(match ~ strdist_jac1 + strdist_jac2 + strdist_jac3 + strdist_jac4, family = binomial, dt_grid)
r_log5 <- glm(match ~ strdist_jac1 + strdist_jac2 + strdist_jac3 + strdist_jac4 + strdist_jac5, family = binomial, dt_grid)

dt_grid[, map(.SD, mean), match, .SDcols = patterns("strdist_jac")]

## see how distinctive they are: matches clearly distinct.. but is that enough?
## will see when I have figured out the boosting.. 

melt.data.table(dt_grid,  id.vars = c("ID_pmdb", "ID_tgt", "match"), measure.vars = paste0("strdist_jac", l_qs)) %>%
    ggplot(aes(x = value, color = factor(match))) +
    geom_density() + 
    facet_grid(variable+match~., scales = "free") 

dt_grid %>% ggplot(aes (x = strdist_jac5, color= factor(match))) + geom_density()

screenreg(list(r_log1,r_log12,r_log2, r_log3, r_log4, r_log5))

## ** mboost package

r_boost <- glmboost(match ~ strdist_jac1 + strdist_jac2 + strdist_jac3 + strdist_jac5, dt_grid, family = Binomial(type = "glm", link = "logit"))
AIC(r_boost, method = "corrected")


cars.gb <- gamboost(dist ~ speed, data = cars, dfbase = 4,
                    control = boost_control(mstop = 50))
cars.gb

AIC(cars.gb, method = "corrected")

### plot fit for mstop = 1, ..., 50
plot(dist ~ speed, data = cars)

tmp <- sapply(1:mstop(AIC(cars.gb)), function(i)
    lines(cars$speed, predict(cars.gb[i]), col = "red"))

lines(cars$speed, predict(smooth.spline(cars$speed, cars$dist),
                          cars$speed)$y, col = "green")

### artificial example: sinus transformation
x <- sort(runif(100)) * 10
y <- sin(x) + rnorm(length(x), sd = 0.25)
plot(x, y)
### linear model
lines(x, fitted(lm(y ~ sin(x) - 1)), col = "red")
### GAM
lines(x, fitted(gamboost(y ~ x,
                         control = boost_control(mstop = 500))),
      col = "green")





## ** gbm


set.seed(123)


l_cols_feat <- keep(names(dt_grid_wfeat), ~grepl("strdist", .x))
f_gbm <- l_cols_feat  %>% paste0(collapse = "+") %>%
    sprintf("match ~ %s", .) %>% as.formula
              

r_gbm <- gbm(
  formula = f_gbm,
  distribution = "bernoulli",    # Binary classification
  data = dt_grid_wfeat,          
  n.trees = 100,                 # Number of trees
  interaction.depth = 4,         # Depth of trees
  n.minobsinnode = 10,           # Minimum number of observations in the tree node
  shrinkage = 0.01,              # Learning rate
  verbose = TRUE                 # Show progress
)


gbm.perf(r_gbm)

confusionMatrix


## ** exploring match data
dt_grid %>% head %>% adf



dt_grid_subset <- dt_grid[strdist_jac1 < 0.3 & geodist < 5] %>% copy


dt_grid_subset[, nbr_candidates := .N, .(ID_af)]

dt_grid_subset[nbr_candidates == 1]
dt_grid_subset[nbr_candidates > 5]


dt_grid_subset[nbr_candidates == 1, .(name_nccs, name_af, strdist_jac1, geodist)]

## hermitage
dt_grid_subset[ID_nccs == 10769997, .(name_nccs, name_af, strdist_jac1, geodist)]


dt_grid_subset[grepl("joslyn", ignore.case = T, name_af), .(ein, name_af, name_nccs)]

dt_nccs_artmuem_full[ein %in% c(470384577, 470835035), .(year, styear, name, ein)] %>% print(n=80)


dt_grid_subset[ID_af == 6805] ## single

dt_grid_subset[ID_af == 7345, .(name_af, name_nccs, strdist_jac1, geodist, ID_nccs, ID_af)] %>% print(n=80)## many

dt_grid_subset[ID_af == 7345, .(name_af, name_nccs, strdist_jac1, geodist, ID_nccs, ID_af)] %>% view_xl

dt_example <- dt_grid_subset[ID_af == 7345, .(name_nccs, strdist_jac1, geodist, ID_nccs)]


## ** old AI sim code, now hopefullly better fucntionalized

## training model
dt_grid_blank <- gd_grid_train_mon(size_nomatch = 1e3)
dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank, "name_pmdb", "name_tgt")

## dt_grid_wfeat[, .SD[sample(1:.N, size = 5)], match] %>% write.csv(file = "")
    
## dt_grid_blank[tolower(name_pmdb) == tolower(name_tgt) & match == 0]
dt_grid_blank[, .N, match]


# Train the model


## dt_grid_test[match_pred_num > 0.5, .(name_pmdb, name_tgt, match)] %>% print(n=80)
# howy smokes

dt_grid_test[match == 1 & match_pred_num < 0.5, .(name_pmdb, name_tgt, match, match_pred)] %>% print(n=80)



r_xgb_mon <- gr_xgb_mon()

r_xgb_mon_ff <- pmdata:::rr_xgb_mon("full")

r_xgb_mon_ff <- readRDS(PMDATA_LOCS$R_XGB_MON)

gd_xgb_topfeat(r_xgb_mon)
gd_xgb_topfeat(r_xgb_mon_ff, 0.8)

setinfo(r_xgb_mon_ff, "label", 1)

pmdata:::gr_xgb_mon_smol()

r_xgb_smol <- gr_xgb_smol(r_xgb_mon, dt_grid_blank, c_params)


dt_grid_bu <- dt_grid %>% copy %>% .[1:1e4]

dt_grid_smol <- gd_dt_smol(dt_grid_blank = dt_grid, r_xgb_smol = r_xgb_smol, "name_af", "name_nccs", thld = 0.0001)

dt_grid_fullfeat <- gd_dt_smol(dt_grid_smol, r_xgb, "name_af", "name_nccs", thld = 0.01)



dt_grid_fullfeat[match_pred > 0.1][, .(name_af, name_nccs, match_pred)] %>% print(n=80)



dt_grid_fullfeat[tolower(name_af) == tolower(name_nccs), .(name_af, name_nccs, match_pred)]

dt_grid_fullfeat[match_pred > 0.5]
dt_grid[tolower(name_af) == tolower(name_nccs)]


library(yaml)
read_yaml("~/Dropbox/phd/pmdata/inst/manual_munging/params_mon.yaml") %>% chuck("c_params_xgb") %>% str
