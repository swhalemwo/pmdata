
#' generate data for training MON models
#' so far uses PMDB-MOW, and PMDB-AF links (manually constructed)
#' future: add more links, based on AI results
#'
#'@param size_nomatch how many non-matched organizations to get from each dataset
gd_grid_train_mon <- function(size_nomatch = 2e3) {

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

#' generate hyperparameters for MON models

gc_params_mon <- function() {
    c_params_mon <- list(
        n_rounds = 100,
        ## n_rounds = 10, # shorter model for debugging
        name1 = "name_pmdb",
        name2 = "name_tgt",
        size_nomatch = 1e3,
        ## parameters for model
        c_params_xgb = list(
            objective = "binary:logistic",  # Binary classification
            eval_metric = "logloss",         # Evaluation metric
            eta = 0.2,                       # Learning rate
            max_depth = 8,                    # Maximum depth of trees
            subsample = 0.5)
    )
    return(c_params_mon)
}

#' generate and write full MON (matching organization name) model
#'
#' @param R_XGB_MON file to store full model
gwr_xgb_mon <- function(R_XGB_MON = PMDATA_LOCS$R_XGB_MON) {
    #' generates and writes model for matching organization names (MON)
    #' goes to pmdata

    c_params_mon <- gc_params_mon()

    ## dt_grid_blank <- gd_grid_train_mon(size_nomatch = 1e3)
    dt_grid_blank <- gd_grid_train_mon(size_nomatch = c_params_mon %>% chuck("size_nomatch"))

    r_xgb_mon <- gr_xgb(dt_grid_blank, c_params_mon)
    
    saveRDS(r_xgb_mon, R_XGB_MON)
}

#' generate and write smol XGB MON model: less string similarity variable construction needed
#'
#' @param R_XGB_MON_SMOL file to store model in
gwr_xgb_mon_smol <- function(R_XGB_MON_SMOL = PMDATA_LOCS$R_XGB_MON_SMOL) {
    #' generate a particular smol XGB model for organization name matching
    c_params_mon <- gc_params_mon()

    ## dt_grid_blank <- gd_grid_train_mon(size_nomatch = 1e3)
    dt_grid_blank <- gd_grid_train_mon(size_nomatch = c_params_mon %>% chuck("size_nomatch"))

    r_xgb_mon <- rr_xgb_mon()

    r_xgb_mon_smol <- gr_xgb_smol(r_xgb_mon, dt_grid_blank, c_params_mon)
    saveRDS(r_xgb_mon_smol, R_XGB_MON_SMOL)
}

#' read trained MON (match organization name) models, either use large or smol
#'
#' @param model_size one of full or smol, specifying which model to load
#' @param R_XGB_MON file with full model
#' @param R_XGB_MON_SMOL file with smol model
#' @export
rr_xgb_mon <- function(model_size = "full", R_XGB_MON = PMDATA_LOCS$R_XGB_MON,
                       R_XGB_MON_SMOL = PMDATA_LOCS$R_XGB_MON_SMOL) {

    if (model_size == "full") {
        readRDS(R_XGB_MON)
    } else if (model_size == "smol") {
        readRDS(R_XGB_MON_SMOL)
    }
}



## FIXME: eventually add function that retrains full and smol models in one go 
## - do hyper parameter optimization for both
