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
