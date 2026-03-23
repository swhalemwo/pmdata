## * main
## ** main

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


dt_tanp_muyr %>% ggplot(aes(x = year, y = total, color = muci)) +
    geom_line(show.legend = F) +
    scale_y_continuous(trans = scales::log10_trans()) +
    geom_point(show.legend = F)

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




## gap stuffing

dt_tanp_muyr %>% copy %>% .[, `:=`(yrdiff = max(year) - min(year), nobs = .N), muci] %>%
    .[yrdiff > nobs, .(museum_new, muci, museum, year, yrdiff, nobs)] %>%
    .[, nobs_abs := yrdiff - nobs] %>% # print(n=80)
    .[, head(.SD, 1), muci] %>% ## .[, sum(nobs_abs)]


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


