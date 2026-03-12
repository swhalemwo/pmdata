library(data.table)
library(jtls)
library(purrr)
library(igraph)

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
dt_tanp_cbn <- rbind(dt_tanp_20[, .(id, museum, city, total)], dt_tanp_22[, .(id, museum, city, total)])


## cross-join into long
dt_tanp_cpr <- merge(dt_tanp_cbn[, .(id1 = id, name1 = paste0(museum, " -- ", city), mrg = "x")],
                     dt_tanp_cbn[, .(id2 = id, name2 = paste0(museum, " -- ", city), mrg = "x")], by = "mrg", allow.cartesian = T) %>%
    .[id1 > id2]



## l_cols_strdist_toobig <- paste0("strdist_", c(paste0("qgram_", 1:5), "osa", "lv", "dl", "lcs"))

## only use standardized string distances: cosine, jaccard, jw


dt_qmod <- expand.grid(mod = c("cosine", "jaccard"), q = 1:5, stringsAsFactors = F) %>% adt

dt_tanp_wfeat <- gd_grid_wfeat(dt_tanp_cpr, "name1", "name2", dt_qmod = dt_qmod, l_mod_noq = "jw") %>% 
    .[, strdist_cosine_avg := rowMeans(.SD), .SDcols = patterns("strdist_cosine")] %>%
    .[, strdist_jaccard_avg := rowMeans(.SD), .SDcols = patterns("strdist_jaccard")] %>%
    .[, strdist_avg_all := rowMeans(.SD), .SDcols = c("strdist_cosine_avg", "strdist_jaccard_avg", "strdist_jw")]

dt_tanp_wfeat[strdist_cosine_1 < 0.1]


dt_tanp_wfeat[strdist_cosine_1 < 0.1]

dt_tanp_wfeat[strdist_cosine_1 == 0] %>% adf

g_tanp <- igraph::graph_from_data_frame(dt_tanp_wfeat[strdist_cosine_1 == 0, .(id1, id2)], directed = F)
clusters <- igraph::cluster_louvain(g_tanp)
dt_clusters <- data.table(id = clusters$names, cluster = as.integer(clusters$membership))


merge(dt_clusters, dt_tanp_cbn, by = "id")[order(cluster)] %>%
    .[, nbr_members := .N, cluster] %>%
    .[nbr_members > 2]


dt_tanp_wfeat[strdist_avg_all> 0][order(strdist_avg_all), .(name1, name2, strdist_avg_all)][1:20]

dt_tanp_wfeat[strdist_cosine_1> 0][order(strdist_cosine_1)][1:20] %>% adf
