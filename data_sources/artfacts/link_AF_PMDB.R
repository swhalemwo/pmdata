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
