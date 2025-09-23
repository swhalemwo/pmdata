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
library(fixest)
library(ellmer)
library(caret)
library(stringdist)

NODB_GEOCODE_NCCS <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_nccs.sqlite"
NODB_GEOCODE_AF <- "~/Dropbox/phd/pmdata/inst/manual_munging/nodb_geocode_artfacts.sqlite"

Sys.setenv(OPENAI_API_KEY = show_pass_secret("llm-openai-key"))




gd_coords <- function(db_name, container, dt_info) {
    
    src2 <- dbConnect(SQLite(), db_name)

    dt_coords <- dbGetQuery(src2, sprintf("select ID, lat, long from %s_flat", container)) %>% adt %>% na.omit %>% 
        st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>% adt

    merge(dt_info, dt_coords, by = "ID")
}

PMDATA_LOCS <- gc_pmdata_locs()

gd_grid_new <- function() {
    #' generate data for which I want to get matches
    #' here it's AF and NCCS 

    ## dt_nccs_artmuem_full <- gd_nccs_muem()[nteecc == "A51"]
    dt_nccs_artmuem <- gd_nccs_muem()[nteecc == "A51", tail(.SD, 1), ein]

    dt_nccs_artmuem_min <- dt_nccs_artmuem %>% copy %>% .[, .(ID = ein, name)] # minimal 


    dt_nccs_info <- gd_coords(NODB_GEOCODE_NCCS, "google_waddr", dt_nccs_artmuem_min) %>%
        setnames(old = names(.), new = paste0(names(.), "_nccs"))

    dt_af_instns_us <- gd_af_instns() %>% .[Country == "United States" & InstitutionType != "Private Galleries"] %>%
        .[, .(ID, name = Name)]

    dt_af_info <- gd_coords(NODB_GEOCODE_AF, "google", dt_af_instns_us) %>%
        setnames(old = names(.), new = paste0(names(.), "_af")) %>% .[]



    ## set up grid
    lenx <- 5e3
    ## lenx <- max(
    dt_grid <- CJ(ID_af = dt_af_info[1:lenx, ID_af], ID_nccs = dt_nccs_info[1:lenx, ID_nccs]) %>%
        merge(dt_af_info, by = "ID_af") %>%
        merge(dt_nccs_info, by = "ID_nccs")

    return(dt_grid)
}


## ** AI matching

prompt_sys <- "you are a very hard-working and capable organizational studies research assistant. you're goal is to match organizations from two different datasets, AF and NCCS. you will get a single name from dataset NCCS, and a table of candidates from dataset AF. Your task is to select the candidate from the AF dataset that corresponds to the single name from the NCCS dataset. in the datasets the names are often spelled differently, but often there also might be no match, so it's your task to only select the candidate from dataset AF if you're sure it matches the name given by NCCS.

There is some information to help you with this task. For the single name from NCCS (for which we want to find the corresponding organization from AF), you are given the name of the organization and its city and state (all organizations are in the USA). The list of candidates from the AF dataset is a csv-like table which includes the column 'ID_af', 'name_af', 'city_af'. It also includes the as column 'geodist' (which describes the geographical distance in kilometers between the AF row  and the NCCS candidate) and column 'match_pred', which is the predicted probability of matching the NCCS organization based on an XGB model which in turn is based on string similarity models (ranges from 0 to 1). These metrics can be helpful, but they might also be wrong; the geolocation API might have confused organizations, and actually matching organizations might not always have similar string distance (or different organizations might have similar strings) ; the task is thus to use all the information to obtain an informed decision. The set of candidates is based on the 10 most likely predictions, and the 10 closest organizations. In particular you should base your evaluation on common name features, such as names of places, personal names or artifical words that are in both the NCCS term and the candidate, rather than general terms like 'museum' or 'gallery'.

Your task is to find the organization from the set of candidates that matches the NCCS organization, using all the information described above (organization name, city, state, predicted match based on string similarity, geographical distance). Rather err on the side of caution (finding no match) than risking to return a wrong match. return answer as a json dictionary with the following keys:
- ID_af: the ID of the entry from the AF candidate set that matches the NCCS organization. if there is no match in the set of candidates, return 0 here.
- gpt_conf: your confidence that this is the right choice, on a scale from 0 to 1.
- gpt_elab: a short justification of your choice (no more than 100 words)

here are some negative examples, i.e. no match:
NCCS name: MU GALLERY INC, NCCS city: BOSTON, NCCS state: MA to '808 Gallery Boston University Art Gallery, Boston, MA'
"


gwd_aisimas <- function(dtx, prompt_sys) {
    #' ai similarity assessment: takes a data frame of possible candidates, returns the matching one

    ## options(width = 500)
    ## dt_to_attach <- capture.output(print(adf(dtx)), sep = "\n")

    ## set up data
    dt_cand <- dtx[, .(ID_af, name_af, city_af, geodist, match_pred)]
    str_dt <- paste(capture.output(fwrite(dt_cand, , file = "")), collapse = "\n")
    ID_nccs <- dtx[1, ID_nccs]
    str_nccs <- dtx[1, sprintf("NCCS name: %s, NCCS city: %s, NCCS state: %s", name_nccs, city_nccs, state_nccs)]
    prompt_user <- sprintf("%s\n\nAF candidate info:\n%s", str_nccs, str_dt)


    print(ID_nccs)
    
    cntr <- 0
    while (T) {
        
        l_resp <- call_openai_api_structured(prompt_sys, prompt_user)

        ## run model as long as necessary to get the response columns
        b_cols_there <- len(setdiff(names(l_resp), c("ID_af", "gpt_conf", "gpt_elab"))) == 0

        ## make sure returned ID actually is in candidates (or 0)
        b_af_id_there <- l_resp$ID_af %in% c(dt_cand[, ID_af],0)

        if (b_cols_there & b_af_id_there) break
        
        
        print(cntr)
        if (cntr > 10) {stop("model failed 10 times to get right columns -> check logic")}
        cntr <- cntr +1
    }


    ## add ID_nccs to response
    l_resp_wid <- c(l_resp, list(ID_nccs = ID_nccs)) %>% unname

    dbExecute(db_match_nccs_af, "UPDATE match_nccs_af SET ID_af = ?, gpt_conf = ?, gpt_elab = ?, nbr_checked = nbr_checked +1 WHERE ID_nccs = ?",
              params =l_resp_wid)


    if (l_resp %>% chuck("ID_af") == 0) {
        ## failure case: also update log

        dbAppendTable(db_match_nccs_af, "nccs_fail_log", dt_cand[, .(ID_nccs, ID_af, match_pred, geodist)])

    }
}




## gwd_aisimas(dt_cands_gpt[ID_nccs == 43145215], prompt_sys)





## output_structure <- type_object(
##     color = type_string("the word describing the color in the text snippet"))

## system_prompt <- "find the word which is a color in this list of words"

## user_prompt <- "car apple orange lamp sun"





call_openai_api_structured <- function(system_prompt, user_prompt) {
    
    c_params_ellmer <- ellmer::params(temperature = 0,
                                      max_tokens = 1000,
                                      seed = 42)
    
    c_llm_output <- type_object(
        ID_af = type_integer(paste0("The ID the entry from the AF candidate set that matches the NCCS organization. ",
                                    "if there is no match in the set of candidates, return 0")),
        gpt_conf = type_number("your confidence that the chosen ID is the right one, on a scale from 0 to 1"),
        gpt_elab = type_string("a short justification of your choice (no more than 100 words)"))



    chat <- chat_openai(
        model = "gpt-4o-mini",
        system_prompt = system_prompt,
        params = c_params_ellmer,
        echo = "all" #suppress the output from being printed
    )
    
    response <- chat$chat_structured(user_prompt, type = c_llm_output)
    return(response)
}

call_openai_api_structured(system_prompt, user_prompt)





gwd_aisimas(dt_example)


gd_cands_gpt <- function() {
    #' generate candidate pairs for AI matching
    ## FIXME: this should probably be generalized later on
    ## FIXME: this is leaking memory like crazy, probably through xgb.DMatrix not releasing?
    
    ## collecting new data to be matched
    dt_grid_new <- gd_grid_new()

    ## add distances based on coordinates--
    dt_grid_new[, geodist := st_distance(geometry_af, geometry_nccs, by_element = T) %>%
                      set_units("km") %>% drop_units]

    ## get smol and full models to train match
    r_xgb_mon_smol <- rr_xgb_mon(model_size = "smol")
    r_xgb_mon_full <- rr_xgb_mon(model_size = "full")

    ## use smol model for basic filtering
    dt_grid_smol <- gd_dt_smol(dt_grid_new, r_xgb_mon_smol, "name_af", "name_nccs", thld = 0)

    
    ## get 10 closest stringmatch and 10 closest locations
    dt_grid_smol2 <- rbind(dt_grid_smol[, head(.SD[order(-match_pred)], 10), ID_nccs],
                           dt_grid_smol[, head(.SD[order(geodist)], 10), ID_nccs]) %>% 
        .[, head(.SD,1), .(ID_nccs, ID_af)] # remove duplicates

    ## use full model to predict matches with hopefully more accuracy
    dt_grid_smol3 <- gd_dt_smol(dt_grid_smol2, r_xgb_mon_full, "name_af", "name_nccs", thld = 0)

    ## merge location info back for AI
    dt_nccs_locinfo <- gd_nccs_muem()[nteecc == "A51", tail(.SD, 1), ein] %>%
        .[, .(ID_nccs = ein, city_nccs = city, state_nccs = state)]

    dt_af_locinfo <- gd_af_instns()[Country == "United States" & InstitutionType != "Private Galleries"] %>%
        .[, .(ID_af = ID, city_af = City)]

    dt_grid_smol4 <- merge(dt_grid_smol3, dt_nccs_locinfo, by = "ID_nccs") %>%
        merge(dt_af_locinfo, by = "ID_af")
    
    

    return(dt_grid_smol4)
}

dt_cands_gpt <- gd_cands_gpt()



DB_MATCH_NCCS_AF <- "~/Dropbox/phd/pmdata/inst/manual_munging/DB_MATCH_NCCS_AF.sqlite"
db_match_nccs_af <- dbConnect(SQLite(), DB_MATCH_NCCS_AF)


dbCreateTable(db_match_nccs_af, name = "match_nccs_af",
              fields  = c(ID_nccs = "INTEGER",
                          ID_af = "INTEGER",
                          gpt_conf = "NUMERIC",
                          gpt_elab = "STRING",
                          check_man = "INTEGER",
                          nbr_checked = "INTEGER"))

dbRemoveTable(db_match_nccs_af, "nccs_fail_log")

dbCreateTable(db_match_nccs_af, name = "nccs_fail_log",
              fields = c(ID_nccs = "INTEGER",
                         ID_af = "INTEGER",
                         match_pred = "NUMERIC",
                         geodist = "NUMERIC"))


## generate and insert data: only do this in beginning

## dbRemoveTable(db_match_nccs_af, "match_nccs_af")

## dt_toinsert <- dt_cands_gpt[, .(ID_nccs = unique(ID_nccs))] %>%
##     .[, `:=`(ID_af = character(), gpt_conf = numeric(), gpt_elab = character(), check_man = integer(), nbr_checked = 0)]


## dbAppendTable(db_match_nccs_af, "match_nccs_af", dt_toinsert)


gw_nccs_af_matches <- function(dt_cands_gpt, limit = NULL) {
    
    
    ## get the ID_nccs left to check
    dt_ID_nccs <- dbGetQuery(db_match_nccs_af, "select ID_nccs from match_nccs_af where ID_af IS NULL ") %>% adt

    if (!is.null(limit)) {
        dt_ID_nccs <- head(dt_ID_nccs, n = limit)
    }

    dt_cands_fltrd <- dt_cands_gpt[dt_ID_nccs, on = "ID_nccs"]

    l_cands_fltrd <- split(dt_cands_fltrd, ~ID_nccs)

    map(l_cands_fltrd, ~gwd_aisimas(.x, prompt_sys))

    

}

gw_nccs_af_matches(dt_cands_gpt, limit =30)

dt_nccs_muem[ein == 942744357, .(name)]
dt_cands_gpt[ID_nccs == 942744357, .(ID_af, ID_nccs, name_af, name_nccs)]


dt_matches <- dbGetQuery(db_match_nccs_af, "select ID_nccs, ID_af from match_nccs_af where ID_af IS NOT NULL") %>% adt

dt_matches[, .N, ID_af == 0] ## 70% matched

dt_cands_gpt[dt_matches[ID_af != 0], on = c("ID_nccs", "ID_af")][, .(name_nccs, name_af, geodist, match_pred)] %>%
    .[geodist < 1] %>% print(n=80)



dt_cands_gpt[match_pred > 0.01]

## just predict with where I have reasonable certainty

dt_minmatch <- dt_cands_gpt[, .SD[which.max(match_pred)], ID_nccs] %>% .[match_pred > 0.2 & geodist < 5, .(ID_af, ID_nccs)]


dt_cbn_af_nccs <- merge(
    dt_nccs_muem[dt_minmatch, on = .(ein = ID_nccs)][year >= 2000],
    dt_insyr_obs, by.x = c("ID_af", "year"), by.y = c("InstitutionID", "begin_year"), all.x = T) %>%
    .[!is.na(cnt_prevexhb)] %>%
    .[, match := exps]


dt_cbn_af_nccs


c_ivs <- c("cnt_prevexhb", "aucturn_cum", "career_year", "cnt_prevexhb_log", "aucturn_cum_log","nbr_at_exhbtd_ttl", "nbr_at_exhbtd_unq",
           "nbr_shows", "nbr_artists_pshow", "qntl_cpe", "year")

c_ivs <- c("career_year", "aucturn_cum_log","nbr_at_exhbtd_ttl", "nbr_at_exhbtd_unq",
           "nbr_shows", "nbr_artists_pshow", "qntl_cpe", "year")


## dt_cbn_af_nccs %>% ggplot(aes(x = exps)) + geom_density()


l_mat_tt <- gl_mat_train_test(dt_cbn_af_nccs, l_cols_feat = c_ivs)
mat_train <- l_mat_tt$mat_train
mat_test <- l_mat_tt$mat_test

l_watch <- list(train = mat_train, test = mat_test)

c_params_exps <- list(
    ## objective = "count:poisson",
    objective = "reg:squarederror",
    eval_metric = "logloss",
    eta = 0.1,
    max_depth = 8,
    subsample = 0.5)

r_xgb_exps <- xgb.train(params = c_params_exps, data = mat_train, nrounds = 50, verbose = 1, watchlist = l_watch)
dt_porf <- data.table(match = getinfo(mat_test, "label"), match_pred = predict(r_xgb_exps, mat_test))
dt_porf %>% head %>% adf

xgb.importance(model = r_xgb_exps)

ggplot(dt_porf, aes(x = log10(match), y = log10(match_pred))) + geom_point() + geom_smooth()
