## * main



dt_tanp_cbn <- gd_tanp_cbn()
dt_tanp_city <- gd_tanp_city(dt_tanp_cbn)

dt_tanp_city[grepl("New York", city), .(ID, city, city_new, lat, long)]

## with city id 
dt_tanp_wcid <- merge(dt_tanp_cbn, dt_tanp_city[, .(city, id_city = ID, city_new)], by = "city")

dt_tanp_wcid[grepl("Kr|Pau|Washing", city)] %>% print(n=80)

dt_tanp_wcid[grepl("Smithsonian", museum)]
dt_tanp_wcid[grepl("National Portrait", museum) & grepl("Washington", city)]

dt_tanp_wcid[grepl("/", museum)]

dt_tanp_muci <- gd_tanp_muci(dt_tanp_wcid)
dt_tanp_wcid[grepl("museum of modern art", museum, ignore.case= T) & grepl("new york", city_new, ignore.case = T)]
dt_tanp_muci[grepl("Smithsonian",museum), .(muci, museum, id_city, museum_new)]
dt_tanp_muci[grepl("National Portrait", museum) & grepl("Washington", city_new)]


dt_tanp_muyr <- merge(dt_tanp_wcid, dt_tanp_muci[, .(muci, id_city, museum, museum_new)],
                      by = c("museum", "id_city")) %>%
    .[, year := as.integer(gsub("tanp(\\d+)_.*", "\\1", id))]



dt_tanp_muyr[, .N, .(muci, year)][N > 1] %>%
    merge(dt_tanp_muyr, by = c("muci", "year"))

dt_tanp_muyr[grepl("queensland", museum, ignore.case = T)]
dt_tanp_muyr[grepl("de young", museum, ignore.case = T)]

dt_tanp_muyr[grepl("SAAM", museum_new), .(muci)]
dt_tanp_muyr[grepl("National Portrait", museum) & grepl("Washington", city_new)]

## plot total over time
dt_tanp_muyr %>%
    ## .[, head(.SD, 100), year] %>% 
    ggplot(aes(x = year, y = total, color = muci_id)) +
    geom_line(show.legend = F) +
    scale_y_continuous(trans = scales::log10_trans()) +
    geom_point(show.legend = F)


dt_tanp_muyr[, head(.SD, 100), year] %>%
    xtsum(log(total), muci_id)

dt_tanp_muyr[ year <= 2005, head(.SD, 100), year] %>%
    xtsum(total, muci_id)

dt_tanp_muyr[ year > 2005, head(.SD, 100), year] %>%
    xtsum(total, muci_id)

dt_tanp_muyr[grepl("national museum", museum, ignore.case = T)] %>% print(n=80)

dt_tanp_muyr[year == 17][order(-total)]



dt_tanp_muyr[, .N, muci][order(-N)]
dt_tanp_muyr[muci %in% c("muci_216")][order(year, muci)] %>% adf

dt_tanp_muyr[grepl("Smithsonian", museum)]

dt_af_instns <- gd_af_instns()
dt_af_instns[grepl("Smithsonian", Name)]
dt_af_instns[grepl("National Portrait", Name)]

dt_af_exhbs <- gd_af_exhbs()

dt_af_exhbs[, begin_year := year(BeginDate)] %>%
    .[begin_year > 1990, .N, begin_year] %>% ggplot(aes(x=begin_year, y = N)) + geom_line()




## ** gap stuffing

dt_tanp_muyr %>% copy %>% .[, `:=`(yrdiff = max(year) - min(year), nobs = .N), muci_id] %>%
    .[yrdiff > nobs, .(muci_name, muci_id, old_museum, year, yrdiff, nobs)] %>%
    .[, nobs_abs := yrdiff - nobs] %>% # print(n=80)
    .[, head(.SD, 1), muci_id]## .[, sum(nobs_abs)]


## * wayback searching
dt_wayback <- fread("~/Dropbox/phd/pmdata/test")

dt_wayback[, date_str := as.character(V2)] %>%
    .[, date := as.Date(date_str, format = "%Y%m%d%H%M%S")]

dt_wayback[date < "2000-01-01"]

dt_wayback_fltr <- dt_wayback[date %between% c("2007-02-01", "2014-02-01")]

fwrite(dt_wayback_fltr, "~/Dropbox/phd/pmdata/filtered")

dt_spring_2007 <- dt_wayback[date %between% c("2007-03-01", "2007-06-30")]



dt_spring_2007[grepl("articlelist", V3)]
dt_spring_2007[grepl("jpg", V3)]
dt_spring_2007 %>% print(n=800)
dt_spring_2007[grepl("pdf", V1)]

dt_2017 <- dt_wayback[date > "2018-02-01"]
fwrite(dt_2017, "~/Dropbox/phd/pmdata/wayback17")


## * artfacts matching


dt_tanp_muci_fixed[, uniqueN(muci)]

dt_tanp_muci_fixed[, .N, .(muci, museum)][order(muci, -N)] %>% print(n=80)

dt_tanp_muyr[, .N, .(muci_id, old_museum)][order(muci_id, -N)][, head(.SD, 1), muci_id]




## gets muci, then coordinates, then old name
dt_tanp_muci <- merge(gd_tanp_muyr()[, .(muci = unique(muci_id))],
                      pmdata:::gd_tanp_muci_ff(), by = "muci") %>% .[, nbr := 1:.N] %>% # %>% .[1:40]
    merge(gd_tanp_muyr()[, .N, .(muci_id, old_museum)][order(muci_id, -N)][, head(.SD, 1), muci_id],
          by.x = "muci", by.y = "muci_id")
    

dt_af_inst_loc <- merge(gd_af_instns(), gd_af_inst_loc(), by = "ID") %>%
    .[, .(ID, name_af = Name, long, lat, nbr = 1:.N)] # %>% .[1:60]
dt_muci_pts <- terra::vect(dt_tanp_muci[, .(muci, museum = old_museum, long, lat)], geom = c("long", "lat"))
dt_af_pts <- terra::vect(dt_af_inst_loc[, .(id_af = ID, name_af, long, lat)], geom = c("long", "lat"))

mat_dist <- terra::distance(dt_muci_pts, dt_af_pts)
mat_dist[1:10, 1:10]

## lame indexes: get all within 1km
mat_indx <- which(mat_dist < 1000, arr.ind = T)
dt_dist <- mat_indx %>% adt %>% .[, dist := mat_dist[mat_indx]]




mat_dist2 <- mat_dist[1:20, 1:40]
sapply(1:4, \(x) which(mat_dist2 == sort(mat_dist2)[x]))

## better dist matrix: get closest 5 for each tanp

## set up col names
lv_nbraf <- paste0("nbraf", 1:5)
lv_dist <- paste0("dist", 1:5)

## actual calcs: get both ids (for merging later) and dists, then join and order later
dt_clos5 <- apply(mat_dist, 1, \(x) c(order(x)[1:5], sort(x)[1:5])) %>% t %>% adt %>% 
    .[, nbr_tanp := 1:.N] %>%
    setnames(new = c(lv_nbraf, lv_dist, "nbr_tanp")) %>% 
    melt(id.vars = "nbr_tanp", measure.vars = list(lv_nbraf, lv_dist), value.name = c("nbr_af", "dist")) %>%
    merge(dt_af_inst_loc[, .(name_af, id_af = ID, nbr_af = nbr)], by = "nbr_af") %>%
    .[, variable := NULL] %>%
    merge(dt_tanp_muci[, .(muci, name_tanp = old_museum, nbr_tanp = nbr)], by = "nbr_tanp") %>%
    .[order(nbr_tanp, dist)]

## get string features
dt_qmod <- expand.grid(mod = c("cosine", "jaccard"), q = 1:5, stringsAsFactors = F) %>% adt

dt_tanp_wfeat <- gd_grid_wfeat(dt_clos5, "name_af", "name_tanp", dt_qmod = dt_qmod, l_mod_noq = "jw") %>% 
        .[, strdist_cosine_avg := rowMeans(.SD, na.rm = T), .SDcols = patterns("strdist_cosine")] %>%
        .[, strdist_jaccard_avg := rowMeans(.SD, na.rm = T), .SDcols = patterns("strdist_jaccard")] %>%
        .[, strdist_avg_all := rowMeans(.SD, na.rm = T), .SDcols = c("strdist_cosine_avg", "strdist_jaccard_avg",
                                                          "strdist_jw")]
    ## .[, .SD := NULL,  .SDcols = patterns("strdist_cosine")]


dt_clos_cbn <- merge(dt_clos5, dt_tanp_wfeat[, .(nbr_tanp, nbr_af, strdist_avg_all)], by = c("nbr_tanp", "nbr_af"))
    
dt_clos_cbn[, .SD[min(dist) > 0.1], nbr_tanp][order(nbr_tanp, dist), .(name_af, name_tanp, dist)]



dt_clos_cbn[dist < 1 & strdist_avg_all < 0.05, .(name_af, name_tanp)] %>% print(n=800)



## get some initial link
fwrite(dt_clos_cbn[dist < 1 & strdist_avg_all < 0.05, .(muci_id = muci, id_af)],
       PMDATA_LOCS$FILE_LINKS_AF_TANP)

dt_af_instns <- gd_af_instns()

dt_links_tanp_af <- gd_links_tanp_af()

dt_clos_cbn[!dt_links_tanp_af, on = .(muci = muci_id)] %>%
    .[dist < 2, .(muci, id_af, name_tanp, name_af)] %>% fwrite



pmdata:::gw_tanp_af_matches()

dt_af_exhbs <- gd_af_exhbs()
dt_af_exhbs_agg <- dt_af_exhbs[, .N, .(ID = InstitutionID)]


merge(dt_af_instns[City == "Prague" & grepl("national gallery", Name, ignore.case = T)], dt_af_exhbs_agg,
      by = "ID")

 
dt_tanp_muci[grepl("MMCA|National Museum of Modern and Contemporary", museum)]

leaflet(dt_tanp_muci[grepl("MMCA|National Museum of Modern and Contemporary", museum)]) %>% addTiles() %>%
    addCircles(lat = ~lat, lng = ~long, label = ~museum)

dt_tanp_muyr[grepl("MMCA", muci_name)]
dt_tanp_muyr[, .N, .(muci_id, year)][N > 1]


dt_tanp_muyr <- gw_tanp_muyr()
## dt_tanp_muyr <- gd_tanp_muyr()
dt_tanp_muyr[, .N, .(muci_id, year)][N > 1]
dt_tanp_muyr[muci_id == "muci_505"]
dt_tanp_muyr[, uniqueN(muci_id)]

## 67 and 313 are Gwacheon
## l_mmca_int <- c(197, 196, 272, 195, 288, 272, 336, 194)
## l_mmca <- paste0("muci_", c(197, 196, 272, 195, 288, 272, 336, 194))
## gd_tanp_muci_ff()[muci %in% l_mmca, .(muci, museum, city_new)]

## dt_mmca_links <- expand.grid(muci1 = l_mmca_int, muci2 = l_mmca_int) %>% adt %>%
##     .[muci1 > muci2] %>%
##     .[, map(.SD, ~paste0("muci_", .x))]


## dt_mmca_links[!gd_muci_links_ff(), on = .(muci1, muci2)] %>% cat

## dt_tanp_muci_newname[grepl("MMCA", museum)]




## check matching: have things been matched that no longer exist in dt_tanp_muyr
dt_links_tanp_af <- gd_links_tanp_af()
dt_links_tanp_af[!dt_tanp_muyr, on = "muci_id"]
## -> cleaned some vatican match away

dt_links_tanp_af[id_af != "nomatch"]



dt_tanp_linkcvrg <- merge(dt_tanp_muyr, dt_links_tanp_af[, .(muci_id, match = as.integer(!(id_af == "nomatch")))],
      by  = "muci_id")
    
## coverage over time
dt_tanp_linkcvrg[, .(total_yr = sum(total), N = .N), .(match, year)] %>% 
    dcast(year ~ match, value.var = c("total_yr", "N")) %>%
    .[, `:=`(total_yr = total_yr_0 + total_yr_1, N_yr = N_0 + N_1)] %>%
    .[, `:=`(prop_total_match = total_yr_1/total_yr, prop_N_match = N_1/N_yr)] %>%
    melt(id.vars = "year", measure.vars = patterns("prop")) %>% 
    ggplot(aes(x=year, y = value, color = variable)) + geom_point() + geom_line()


## find largest non-matched ones, looks kinda ok
dt_tanp_linkcvrg[match == 0][order(year, -total)][, head(.SD,10), year] %>%
    .[, .(total = sum(total)), .(muci_id, muci_name, city_name)] %>%
    .[order(-total)] %>% print(n=80)


