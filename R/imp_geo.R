## ## read in Twan's dataset,

## FILE_URED <- "/home/johannes/ownCloud/pmdata/twan_municipalities/Final datasets/URED_regions.csv"
## dt_ured <- fread(FILE_URED)

## dt_ured2 <- read.csv(FILE_URED) %>% adt


## ## yeet all the party stuff
## ## this could be much more compact if they'd know how to RDBMS
## dt_ured_reg <- dt_ured[, .(country, country_code, region_code, year, regionname, electorate, totalvote)] %>% funique


## ## number of geo units per country-year
## dt_ured_reg[, .N, .(year, country_code)] %>%
##     ggplot(aes(x=year, y=N)) + geom_point() +
##     facet_wrap(~country_code, scales = "free_y")


## ## coverage of electorate and totalvote
## summary(dt_ured_reg)
## dt_ured_reg[, mean(is.na(electorate))] # 5% missing, but maybe not? is maybe issue due missing data for some party
## dt_ured_reg[, mean(is.na(totalvote))] # 1% missing

## ## debug missings
## copy(dt_ured_reg)[, N := .N, .(country_code, region_code, year)] %>%
##     .[N > 1]


## copy(dt_ured_reg)[, .(nbr_dist_vlus = fnunique(electorate)), .(country_code, region_code, year)] %>%
##     ## .[, .N, nbr_dist_vlus]
##     .[nbr_dist_vlus > 1 & region_code != ""]

## dt_ured_reg[region_code == "06088" & year == 2002 & country_code == "FR"]
## dt_ured[region_code == "06088" & year == 2002 & country_code == "FR"] %>% adf

## dt_ured[region_code == "06088" & year == 2002 & country_code == "FR", .N, validvote]



## ## might be issue due to some electorate/totalvote missing for some parties, but still there for other parties
## dt_ured[region_code == "196648013" & year == 1972] %>% adf


## ## regionname and regioncode is not the same
## dt_ured[, .(country, country_code, region_code, year, regionname)] %>% funique
## dt_ured[, .(country, country_code, region_code, year)] %>% funique

## ## could be that 

## fnunique(dt_ured[, .(country_code, region_code)])
## fnunique(dt_ured[, .(country_code, region_code, regionname)])

## dt_ured_reg[, .N, .(country_code, region_code, year)][, .(N2 = .N), region_code] %>% .[, .N, N2] %>% .[order(N2)]

## dt_ured_reg[region_code == "10301"] %>% print(n=50)

## ## dt_ured[, .(country

#' generate dt of country borders
#'
#' based on https://github.com/geodatasource/country-borders
#' 
#' @param COUNTRY_BOUNDARIES_FILE file with country borders
#' @export
gd_crybndrs <- function(COUNTRY_BOUNDARIES_FILE = PMDATA_LOCS$COUNTRY_BOUNDARIES_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    country_code <- country_border_code <- iso3c <- iso3c_border <- country_name <- country_border_name <- NULL

    dt_crybndrs <- fread(COUNTRY_BOUNDARIES_FILE) %>% 
        .[, `:=`(iso3c = countrycode(country_code, "iso2c", "iso3c"),
                 iso3c_border = countrycode(country_border_code, "iso2c", "iso3c"))] %>% 
        .[, `:=`(country_code = NULL, country_border_code = NULL)] %>%
        .[, .(iso3c, iso3c_border, country_name, country_border_name)]
        ## .[is.na(iso3c_border)] %>% print(n=300)

    return(dt_crybndrs)


}


## gd_crybndrs()[iso3c == "IDN"]

## gd_marbndrs <- function(MARITIME_BOUNDARIES_FILE = PMDATA_LOCS$MARITIME_BOUNDARIES_FILE) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
##     1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

##     read.rdbf

## }


## gd_marbndrs()
