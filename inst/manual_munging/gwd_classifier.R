gr_xgb_smol <- function(r_xgb) {
    #' generate a smaller model from r_xgb with only most influential predictors
    ## FIXME: data dependencies still rely on globals    

    dt_topfeat <- gd_xgb_topfeat(r_xgb)

    ## mat_train <- xgb.DMatrix(dt_grid_train[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
    ##                      label = dt_grid_train$match)

    mat_train_smol <- dt_grid_train[, .SD, .SDcols = dt_topfeat[, Feature]] %>% as.matrix %>%
        xgb.DMatrix(label = dt_grid_train$match)

    mat_test_smol <- dt_grid_test[, .SD, .SDcols = dt_topfeat[, Feature]] %>% as.matrix %>%
        xgb.DMatrix(label = dt_grid_test$match)
    

    l_watch_smol <- list(train = mat_train_smol, test = mat_test_smol)

    r_xgb_smol <- xgb.train(params = params, data = mat_train_smol,
                            nrounds =nrounds, verbose  = 1, watchlist = l_watch_smol)

    dt_assess <- gd_xgb_assess(r_xgb_smol, dt_grid_test = dt_grid_test, mat_test = mat_test_smol)
    print(dt_assess)
    return(r_xgb_smol)
}



gd_xgb_topfeat <- function(r_xgb) {
    #' use smaller, faster model: only take most influential predictors
    #' feature construction is expensive so can use that to filter down number of cases to check

    dt_topfeat <- xgb.importance(model = r_xgb) %>% .[, cum_gain := cumsum(Gain)] %>% .[cum_gain < 0.8]

    return(dt_topfeat)
}



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




gd_strfeat_wq <- function(dtx, dt_qmod) {
    #' generate string features for string similarities with q

    dt_strfeat_wq <- dtx %>% copy %>%
        .[, paste0(dt_qmod[, sprintf("strdist_%s_%s", mod, q)]) :=
                (map2(dt_qmod[, mod], dt_qmod[, q],
                      ~stringdist(tolower(name1), tolower(name2), method = .x, q = .y))), name1]

    return(dt_strfeat_wq)
}

gd_strfeat_noq <- function(dtx, l_mod_noq) {
    #' generate string similarity features for which there is no q (only single input)
    dt_strfeat_noq <- dtx %>% copy %>%
        .[, paste0("strdist_", l_mod_noq) :=
                map(l_mod_noq, ~stringdist(tolower(name1),
                                           tolower(name2), method = .x)), name1]
        

    return(dt_strfeat_noq)
}
    



gd_grid_wfeat <- function(dt_grid_blank, name1, name2, dt_qmod = NULL, l_mod_noq = NULL) {
    #' construct features for similarity matching
    
    dt_grid_blank %>% setnames(old = c(name1, name2), new = c("name1", "name2"))
    

    if (is.null(dt_qmod)) {
        l_qs <- 1:5 # qgrams for qmethods
        l_qmods <- c("qgram", "jaccard", "cosine") # methods using Q

        dt_qmod <- CJ(q = l_qs, mod = l_qmods)## [, sprintf("%s_%s", mod, q)]
    }

    if (is.null(l_mod_noq)) {
        ## "hamming" creates infinite dist -> yeet
        l_mod_noq <- c("osa", "lv", "dl", "lcs", "jw")
    }

    
    dt_grid_wfeat_q <- gd_strfeat_wq(dt_grid_blank, dt_qmod)

    dt_grid_wfeat_both <- gd_strfeat_noq(dt_grid_wfeat_q, l_mod_noq)
    
    ## dt_grid_blank <- dt_grid_blank[1:1e4]

    ## generate string similarities for q mods
    
    dt_grid_wfeat_both %>% setnames(old = c("name1", "name2"), new = c(name1, name2))

    ## fill up missing columns
    l_cols_feat <- paste0("strdist_", c(dt_qmod[, sprintf("%s_%s", mod, q)], l_mod_noq))
    setnafill(dt_grid_wfeat_both, fill = NA, nan = NA, cols = l_cols_feat)

    dt_grid_wfeat_both <- dt_grid_wfeat_both[sample(1:.N, size = .N)] # shuffle

    return(dt_grid_wfeat_both)

}



gd_xgb_assess <- function(r_xgb, dt_grid_test, mat_test, thld = 0.5, return_data = F) {

    
    dt_grid_test[, match_pred_num := predict(r_xgb, mat_test)][, match_pred := 1*(match_pred_num > thld)]
    
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


gd_dt_smol <- function(dt_grid_blank, r_xgb_smol, name1, name2, thld = 0.0001) {
    #' idea: apply a smaller model first (less time spent constructing features, which is expensive)

    ## first see which columns are needed
    # l_topfeat <- gd_xgb_topfeat(r_xgb)[, Feature]
    # l_topfeat <- xgb.importance(model = r_xgb_smol)[, Feature]
    l_topfeat <- r_xgb_smol$feature_names

    ## sort into q and noq
    l_mod_qindex <- grepl("\\d", l_topfeat)
    l_mod_noq <- l_topfeat[!l_mod_qindex] %>% gsub("strdist_", "", .)

    dt_qmod <- data.table(mod = sub("strdist_([a-z_]+)_\\d+$", "\\1", l_topfeat[l_mod_qindex]),
                          q = sub(".*_(\\d+)$", "\\1", l_topfeat[l_mod_qindex]))

    ## construct feature dt
    dt_feat_smol <- gd_grid_wfeat(copy(dt_grid_blank), name1, name2, dt_qmod = dt_qmod, l_mod_noq = l_mod_noq)

    mat_pred_smol <- xgb.DMatrix(dt_feat_smol[, .SD, .SDcols = l_topfeat] %>% as.matrix)

    dt_feat_smol[, match_pred := predict(r_xgb_smol, mat_pred_smol)]

    ## gd_xgb_assess(r_xgb_smol, dt_feat_smol, mat_pred_smol, thld = 0.0001) # gives recall of 0.994, not ideal but hopefully good enough
    ## and results in 10x data decrease
    
    dt_feat_smol[match_pred >= thld]
    

    ## then construct those features for dt_grid_blank
    
}


assess_xgb_params <- function(eta, max_depth, n_rounds, subsample) {
    # FIXME: data requirements not clear

    c_params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = eta,
        max_depth = max_depth,
        subsample = subsample)

    r_xgb <- xgb.train(params = c_params, data = mat_train, nrounds =n_rounds, verbose  = 1, watchlist = l_watch)
    gd_xgb_assess(r_xgb, dt_grid_test, mat_test)
}


gd_hyperparam_tuning <- function() {

    ## somewhat systematic parameter exploration
    ## set up params
    dt_boost_paramcbns <- expand.grid(
        eta = c(0.01, 0.05, 0.2, 0.5),
        max_depth = c(2,4,6,8),
        n_rounds = c(20,50,100),
        subsample = c(0.5,1)) %>% adt

    l_boost_paramcbns <- dt_boost_paramcbns %>% split(1:nrow(.))


    l_res <- map(l_boost_paramcbns, ~do.call(assess_xgb_params, .x))

    dt_paramres <- l_res %>% rbindlist %>%
        cbind(dt_boost_paramcbns)

    return(dt_paramres)
}

gr_xgb_mon <- function() {
    #' generates and writes model for matching organization names (MON)
    #' goes to pmdata

    dt_grid_blank <- gd_grid_train(size_nomatch = 1e3)

    c_params_mon <- list(
        n_rounds = 100,
        name1 = "name_pmdb",
        name2 = "name_tgt",

        ## parameters for model
        c_params_xgb = list(
            objective = "binary:logistic",  # Binary classification
            eval_metric = "logloss",         # Evaluation metric
            eta = 0.2,                       # Learning rate
            max_depth = 8,                    # Maximum depth of trees
            subsample = 0.5)
    )

    gr_xgb(dt_grid_blank, c_params_mon)
 
    
   
}



gr_xgb <- function(dt_grid_blank, c_params) {

    
    ## FEATURE: will have to be generalized to accomodate more than string similarity features
    ## can't assume binary classification
    dt_grid_wfeat <- gd_grid_wfeat(dt_grid_blank,
                                   chuck(c_params, "name1"),
                                   chuck(c_params, "name2"))

    ## setup training/test data
    train_index <- createDataPartition(dt_grid_wfeat$match, p = 0.8, list = FALSE)

    dt_grid_train <- dt_grid_wfeat[train_index, ]
    dt_grid_test <- dt_grid_wfeat[-train_index, ]


    mat_train <- xgb.DMatrix(dt_grid_train[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                             label = dt_grid_train$match)

    mat_test <- xgb.DMatrix(dt_grid_test[, .SD, .SDcols = l_cols_feat] %>% as.matrix,
                            label = dt_grid_test$match)


    l_watch <- list(train = mat_train, test = mat_test)

    nrounds <- 100                       # Number of boosting rounds, 100 seems to be good
    r_xgb <- xgb.train(params = c_params %>% chuck("c_params_xgb"),
                       data = mat_train,
                       nrounds =c_params %>% chuck("n_rounds"),
                       verbose  = 1, watchlist = l_watch)

    gd_xgb_assess(r_xgb,  dt_grid_test, mat_test) %>% print

    xgb.save(r_xgb, R_XGB_namematch)
    

}


rr_xgb <- function(R_XGB_namematch) {

    xgb.load(R_XGB_namematch)
}


## ** main

## hyperparameter tuning exploration
## FIXME should be more systematic
## dt_paramres_long <- dt_paramres %>% melt(id.vars = names(dt_boost_paramcbns))
    

## fx <- sprintf("value ~ mvsw(%s)", paste0(names(dt_boost_paramcbns), collapse = ",")) %>% as.formula
## feols(fx, dt_paramres_long[variable == "recall"])

## dt_paramres_long[variable == "recall"] %>% 
##     ggplot(aes(x = factor(n_rounds), y = value, color = factor(eta))) + geom_point() +
##     facet_grid(max_depth~subsample)

## dt_paramres[order(-recall)]

## ## dt_paramres %>% ggplot(aes(x = n_rounds, 
