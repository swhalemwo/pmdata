
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
