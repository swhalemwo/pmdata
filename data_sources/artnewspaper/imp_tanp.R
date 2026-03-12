library(data.table)
library(jtls)

dt_tanp <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/museum_visitors24.csv",
      header = F, sep = ";") %>%
    .[, col := paste0("col", rep(1:5, 100))] %>%
    .[, id := rep(1:100, each = 5)]


dt_tanp %>% dcast(id ~ col, value.var = "V1") %>% print(n=800)



## more systematic approach
dt_tanp_20 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_20.csv") %>%
    setnames(new = c("total", "museum", "city", "perc_chng", "days_closed")) %>% 
    .[, id := paste0("tanp20_", 1:.N)] 
    

dt_tanp_22 <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_22.csv") %>%
    setnames(new = c("total", "museum", "city", "perc_chng_21", "perc_chng_19")) %>%
    .[, id := paste0("tanp22_", 1:.N)]


## combine 
dt_tanp_cbn <- rbind(dt_tanp_20[, .(id, museum, city)], dt_tanp_22[, .(id, museum, city)])


## cross-join into long
dt_tanp_cpr <- merge(dt_tanp_cbn[, .(id1 = id, museum1 = museum, mrg = "x")],
                     dt_tanp_cbn[, .(id2 = id, museum2 = museum, mrg = "x")], by = "mrg", allow.cartesian = T)




gd_grid_wfeat(dt_tanp_cpr
