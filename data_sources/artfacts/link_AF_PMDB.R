library(pmdata)
library(countrycode)
library(collapse)
library(purrr)
library(jtls)


options(width = 115)


gw_af_instns_el <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get AF institions data
    dt_af_instns <- gd_af_instns()    

    ## generate the alist cells
    af_segs <- dt_af_instns[, sprintf("(\"%s - %s\" . ((id . \"%s\") (info . \"%s-%s--%s\")))",
                                      Name, City, ID, City, Country, FoundationYear)]

    ## generate the string for the entire a list
    af_alist_str <- c("(setq af-instns '(", af_segs, "))")

    ## write alist to file
    af_instns_file <- paste0(dirname(gc_pmdata_locs()$ARTFACTS_SQLITE_DB), "/af-instns.el")

    fileconn <- file(af_instns_file)
    writeLines(af_alist_str, fileconn)
    close(fileconn)

}





gw_AF_PMDB_match <- function(dt_pmdb_matchy, pmdb_ID) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' check an individual museum from PMDB for match with Artfacts

    ## print the PM to 
    do.call(sprintf, c(list(fmt = "%s, %s-%s--%s"),
                       as.list(dt_pmdb_matchy[ID==pmdb_ID, .(name, city, country, year_opened)]))) %>% print

    ## use artfacts-pmdb-match (C-c m) to fuzzy search whether PMDB museum is in Artfacts
    AF_IID <- readline("AF-IID: ") # AF-IID: Artfacts institution ID

    ## if entry is not integer (AF IIDs are integers), assume missing data (just type "j" or whatever)
    if (is.na(suppressWarnings(as.integer(AF_IID)))) AF_IID <- "nomatch"

    dt_res <- data.table(PMDB_ID = pmdb_ID, AF_IID = AF_IID)
    fwrite(dt_res, PMDATA_LOCS$FILE_ARTFACTS_PMDB_MATCHES, append = T)

}

gw_AF_PMDB_matches <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    library(countrycode)

    dt_pmdb_tomatch <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb %>%
        .[, .(ID, name, museum_status, year_opened, city,
              country = countrycode(iso3c, "iso3c", "country.name"))] %>%
        .[museum_status %!in% c("no private museum", "duplicate")]

    dt_af_pmdb_matches <- gd_af_pmdb_matches()

    pmdb_IDs_tocheck <- setdiff(dt_pmdb_tomatch$ID, dt_af_pmdb_matches$PMDB_ID)

    map(pmdb_IDs_tocheck, ~gw_AF_PMDB_match(dt_pmdb_tomatch, .x))

}

    
PMDATA_LOCS <- gc_pmdata_locs()


gw_AF_PMDB_matches()
    
dt_af_exhbs <- gd_af_exhbs()

## dtx
## dt_af_exhbs[InstitutionID == 12706, .(start = as.Date(BeginDate))] 
## dt_af_exhbs[InstitutionID == 29210, .(start = as.Date(BeginDate))] 

## dt_af_exhbs[InstitutionID == 11 | InstitutionID == 30136, .(InstitutionID, Title)] %>% print(n=80)

dt_pmdb <- gd_pmdb_excl(only_pms = F) %>% gd_pmdb %>% .[museum_status %in% c("private museum", "closed")]

dt_matches_pmdb_af <- gd_af_pmdb_matches()

## compare coverage: 
dt_pmdb_af <- join(dt_matches_pmdb_af, dt_pmdb[, .(ID, name, iso3c, museum_status)],
                   on = c(PMDB_ID = "ID"), how = "inner")

## .[grepl("carmignac", name, ignore.case = T)]
dt_pmdb_af[, .N, .(museum_status, AF_IID != "nomatch")] %>%
    .[, prop := N/sum(N), museum_status]
## huh actually not so much worse, 65% coverage for open, 57% for closed

## coverage of matching by region and country
dt_pmdb_af_cvrgvis <- dt_pmdb_af[, .N, .(museum_status, iso3c, AF_IID != "nomatch")] %>%
    .[CJ(AF_IID = funique(AF_IID), iso3c = funique(iso3c), museum_status = funique(museum_status)),
      on = .(iso3c, AF_IID, museum_status)] %>% # expand DT by all possible combinations
    replace_NA(value = 0) %>%  # make values of combination not covered in the data 0
    .[, N_cry_ms := sum(N), .(museum_status, iso3c)] %>% # count per country and museum status (ms)
    .[, prop_covered := N/N_cry_ms] %>% # prop with AF_IID (by country and museum_status)
    .[AF_IID == TRUE] %>% # only focus on covered percentages (not covered is 1- covered)
    .[, museum_status := factor(museum_status, levels = c("private museum", "closed"))] %>% 
    .[order(museum_status, prop_covered)] %>%  # reorder in for ggplot
    .[, iso3c := factor(iso3c, funique(iso3c))] %>%
    .[, reg6 := rcd_iso3c_reg6(iso3c)] %>%
    .[!is.nan(prop_covered)] # %>% .[iso3c == "DEU"]
                  
dt_pmdb_af_cvrgvis %>% 
    ggplot(aes(x = prop_covered, y = iso3c, color = museum_status, group = museum_status, size = N_cry_ms)) +
    geom_point(position = position_dodge(width = 0.8)) +
    geom_text(mapping = aes(label = N_cry_ms), position = position_dodge(width = 0.8),
              color = "black",
              ## manually scale font size to make it fit in the bubbles
              size = dt_pmdb_af_cvrgvis[order(reg6), rescale(N_cry_ms, to = c(2, 4))]) + 
    facet_grid(reg6 ~ ., scales = "free_y", space = "free_y") +
    scale_size_continuous(range = c(2,6))
    

## coverage by region
dt_pmdb_af %>% copy() %>% 
    .[, reg_sub := countrycode(iso3c, "iso3c", "un.regionsub.name",
                               custom_match = c(TWN = "South-eastern Asia"))] %>%
    .[, .N, .(museum_status, reg_sub, AF_IID != "nomatch")] %>%
    .[, prop_covered := N/sum(N), .(museum_status, reg_sub)] %>% 
    .[AF_IID == TRUE] %>%
    .[, museum_status := factor(museum_status, levels = c("private museum", "closed"))] %>%
    .[order(museum_status, prop_covered)] %>% .[, reg_sub := factor(reg_sub, funique(reg_sub))] %>% 
    ggplot(aes(x=prop_covered, y=reg_sub, group = museum_status, color = museum_status, fill = museum_status)) +
    geom_col(position = position_dodge2(preserve = "single"))
    
## hmm i think the 12 regions are still too many.. it's about similarity/structure
## the ~30 europe countries are not great either, but europe is already narrowing it down

.[, .N, .(museum_status, reg_sub)] %>%
    


    
    
    




## merge AF exhibitions to PMDB
dt_pmdb_exhbs <- join(dt_af_exhbs,
     dt_pmdb_af[AF_IID != "nomatch", .(InstitutionID = as.integer(AF_IID), PMDB_ID, name)], 
     on = "InstitutionID", how = 'inner') %>%
    .[, begin_year := year(BeginDate)]

dt_pmdb_exhbs[, .N, .(PMDB_ID, begin_year)]

## around 1% of exhibitions are in private museums...

## percentage of exhibition in PMs over time: increases from 0.5 in 1995 to ~1.9 in 2023
dt_af_exhbs_pmdb <- join(dt_af_exhbs,
                         dt_pmdb_af[AF_IID != "nomatch", .(InstitutionID = as.integer(AF_IID), PMDB_ID, name)],
     on = "InstitutionID", how = 'left')

class(dt_af_exhbs_pmdb$BeginDate) <- "Date"
class(dt_af_exhbs_pmdb$EndDate) <- "Date"

dt_af_exhbs_pmdb[, begin_year := year(BeginDate)]

dt_af_exhbs_pmdb[, .(prop_pmdb = sum(100*!is.na(PMDB_ID))/.N), begin_year] %>%
    .[begin_year %between% c(1990, 2023)] %>% 
    ggplot(aes(x=begin_year, y = prop_pmdb)) + geom_line()

gd_af_instns()[, .N, InstitutionType][order(-N)] %>% print(n=80)
