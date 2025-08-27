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
    .[, .(ID, name = Name)]

dt_af_info <- gd_coords(NODB_GEOCODE_AF, "google", dt_af_instns_us) %>%
    setnames(old = names(.), new = paste0(names(.), "_af")) %>% .[]


## old permutations: too expensive
## dt_af_cpnts <- dt_af_info %>% head(10) %>% 
##     .[, .(pob_name  = strsplit(name_af, " ")[[1]] %>%
##               permutations(n=uniqueN(.), r= uniqueN(.), v = .) %>%
##               apply(1, paste0, collapse = " ")), ID_af]


library(stringdist)
l_stringdist_methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex") %>%
    setNames(.,.)

imap(l_stringdist_methods, ~stringdist("some test here", "test some", method = .x))
imap(l_stringdist_methods, ~stringdist("test some", "some test here", method = .x))



## set up grid
lenx <- 5e3
## lenx <- max(
dt_grid <- CJ(ID_af = dt_af_info[1:lenx, ID_af], ID_nccs = dt_nccs_info[1:lenx, ID_nccs]) %>%
    merge(dt_af_info, by = "ID_af") %>%
    merge(dt_nccs_info, by = "ID_nccs")



## test jaccard distances with varying qgrams
l_qs <- 1:5

t1 = Sys.time()
dt_grid[, paste0("strdist_jac", l_qs)
        := (map(l_qs, ~stringdist(tolower(name_af), tolower(name_nccs), method = "jaccard", q = .x))), ID_nccs]
t2 = Sys.time()
dt_grid[, .N*5]/as.numeric(t2-t1)

## add distances based on coordinates--
dt_grid[, geodist := st_distance(geometry_af, geometry_nccs, by_element = T) %>% set_units("km") %>% drop_units]

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

library(ellmer)

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
gd_grid_train <- function(size_nomatch = 2e3) {

    #' generate training data to see how similar matched museums are:
    #' find out which metrics are best for automated merging

    dt_pmdb <- gd_pmdb_excl(sel = "all") %>% gd_pmdb %>% .[, .(ID_pmdb = ID, name_pmdb = name)]

    dt_mow_prep <- gd_mow_info() %>% .[, .(ID_mow = MOW_ID, name_mow = name, ID_PMDB_mow = PMDB_ID)]
    dt_mow <- rbind(dt_mow_prep[!is.na(ID_PMDB_mow)],
                    dt_mow_prep[is.na(ID_PMDB_mow), .SD[sample(1:.N, size = size_nomatch)]]) 

    dt_grid_mow <- CJ(ID_mow = dt_mow[, ID_mow], ID_pmdb = dt_pmdb[, ID_pmdb]) %>%
        merge(dt_pmdb, by =  "ID_pmdb") %>%
        merge(dt_mow, by = "ID_mow") %>%
        .[, match := (ID_PMDB_mow == ID_pmdb)*1] %>% setnafill(fill = 0, cols = "match")

    ## then AF
    dt_af_instns_prep <- gd_af_instns()[, .(ID_af = ID, name_af = Name)]
    dt_af_pmdb_matches <- gd_af_pmdb_matches()[AF_IID != "nomatch"][, AF_IID := as.integer(AF_IID)] %>%
        .[, .(ID_af = AF_IID, ID_PMDB_af = PMDB_ID)]
    dt_af_instns_prep2 <- merge(dt_af_instns_prep, dt_af_pmdb_matches, by = "ID_af", all.x = T)
    dt_af_instns <- rbind(dt_af_instns_prep2[!is.na(ID_PMDB_af)],
                          dt_af_instns_prep2[is.na(ID_PMDB_af), .SD[sample(1:.N, size = size_nomatch)]])

    dt_grid_af <- CJ(ID_af = dt_af_instns[, ID_af], ID_pmdb = dt_pmdb[, ID_pmdb]) %>%
        merge(dt_pmdb, by = "ID_pmdb") %>%
        merge(dt_af_instns, by = "ID_af") %>%
        .[, match := (ID_PMDB_af == ID_pmdb)*1] %>% setnafill(fill = 0, cols = "match")

    dt_grid <- rbind(dt_grid_mow[, .(ID_pmdb, ID_tgt = ID_mow, name_pmdb, name_tgt = name_mow, match)],
                     dt_grid_af[, .(ID_pmdb, ID_tgt = ID_af, name_pmdb, name_tgt = name_af, match)])

    return(dt_grid)
}




gd_grid_wfeat <- function(dt_grid_blank) {

    l_qs <- 1:5 # qgrams for qmethods
    l_qmods <- c("qgram", "jaccard", "cosine") # methods using Q

    ## "hamming" creates infinite dist
    l_mod_noq <- c("osa", "lv", "dl", "lcs", "jw")

    dt_qmod <- CJ(q = l_qs, mod = l_qmods)## [, sprintf("%s_%s", mod, q)]

    ## map2(dt_qmod[, mod], dt_qmod[, q], ~sprintf("%s%s", .x, .y))
    
    ## mapply(\(x,y) sprintf("%s%s", x,y), dt_qmod %>% as.list)
    

    ## dt_grid_blank <- dt_grid_blank[1:1e4]

    ## generate string similarities for q mods
    dt_grid_blank[, paste0(dt_qmod[, sprintf("strdist_%s_%s", mod, q)]) :=
                  (map2(dt_qmod[, mod], dt_qmod[, q],
                        ~stringdist(tolower(name_pmdb), tolower(name_tgt), method = .x, q = .y))), ID_pmdb]

    
    dt_grid_blank[, paste0("strdist_", l_mod_noq) :=
                        map(l_mod_noq, ~stringdist(tolower(name_pmdb),
                                                   tolower(name_tgt), method = .x)), ID_pmdb] %>%
        names

    ## fill up missing columns
    l_cols_feat <- paste0("strdist_", c(dt_qmod[, sprintf("%s_%s", mod, q)], l_mod_noq))
    setnafill(dt_grid_blank, fill = NA, nan = NA, cols = l_cols_feat)

    dt_grid_wfeat <- dt_grid_blank[sample(1:.N, size = .N)]

    return(dt_grid_wfeat) # shuffle

}


dt_grid_blank <- gd_grid_train(size_nomatch = 1e2)
dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank)

dt_grid_wfeat[, .SD[sample(1:.N, size = 5)], match] %>% write.csv(file = "")
    



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




## ** xgboost


gd_xgb_assess <- function(r_xgb, dt_grid_test, mat_test, return_data = F) {

    
    dt_grid_test[, match_pred_num := predict(r_xgb, mat_test)][, match_pred := 1*(match_pred_num > 0.5)]
    
    mat_confuse <- dt_grid_test[, table(Pred = match_pred, actual = match)]

    accuracy <- sum(diag(mat_confuse)) / sum(mat_confuse)
    precision <- mat_confuse[2, 2] / sum(mat_confuse[2, ])
    recall <- mat_confuse[2, 2] / sum(mat_confuse[, 2])
    f1_score <- 2 * ((precision * recall) / (precision + recall))

    dt_assess <- data.table(
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1_score = f1_score)

    
    if (return_data) {
        ret_obj <- list(dt_grid_test = dt_grid_test, dt_assess = dt_assess)
    } else {
        ret_obj <- dt_assess
    }
    


    return(ret_obj)
}


library(caret)
train_index <- createDataPartition(dt_grid_wfeat$match, p = 0.8, list = FALSE)

dt_grid_train <- dt_grid_wfeat[train_index, ]
dt_grid_test <- dt_grid_wfeat[-train_index, ]


mat_train <- xgb.DMatrix(dt_grid_train[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                         label = dt_grid_train$match)

mat_test <- xgb.DMatrix(dt_grid_test[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                        label = dt_grid_test$match)

params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "logloss",         # Evaluation metric
  eta = 0.1,                       # Learning rate
  max_depth = 6                    # Maximum depth of trees
)

l_watch <- list(train = mat_train, test = mat_test)

# Train the model
nrounds <- 100                       # Number of boosting rounds, 100 seems to be good
r_xgb <- xgb.train(params = params, data = mat_train, nrounds =nrounds, verbose  = 1, watchlist = l_watch)

gd_xgb_assess(r_xgb,  dt_grid_test, mat_test)



## dt_grid_test[match_pred_num > 0.5, .(name_pmdb, name_tgt, match)] %>% print(n=80)
# howy smokes

dt_grid_test[match == 1 & match_pred_num < 0.5, .(name_pmdb, name_tgt, match, match_pred)] %>% print(n=80)


dt_boost_paramcbns <- expand.grid(
    eta = c(0.01, 0.05, 0.2, 0.5),
    max_depth = c(2,4,6,8),
    n_rounds = c(20,50,100),
    subsample = c(0.5,1)) %>% adt

l_boost_paramcbns <- dt_boost_paramcbns %>% split(1:nrow(.))
                             
assess_xgb_params <- function(eta, max_depth, n_rounds, subsample) {

    c_params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = eta,
        max_depth = max_depth,
        subsample = subsample)

    r_xgb <- xgb.train(params = c_params, data = mat_train, nrounds =n_rounds, verbose  = 1, watchlist = l_watch)
    gd_xgb_assess(r_xgb, dt_grid_test, mat_test)
}

l_res <- map(l_boost_paramcbns, ~do.call(assess_xgb_params, .x))

l_res %>% rbindlist %>% str









