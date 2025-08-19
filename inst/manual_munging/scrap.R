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


