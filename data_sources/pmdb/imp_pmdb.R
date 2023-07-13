## * import PMDB

args <- commandArgs(trailingOnly = T)
options(width = 110)


library(readxl)
library(purrr)
library(countrycode)
## library(pryr)
library(lobstr)
## library(xlsx)
library(googlesheets4)

vrbl_fndr <- function(df, penl_vrbls) {
    #' find the variable referred by by potential variables (penl_vrbls)

    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (len(vrbl) == 0 ) {
        stop("no variable found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else {
        vrbl
    }
}


gendt_pmdb_excl <- function(PMDB_FILE, only_pms=T, rff) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' just read in the excel file, add a few auxiliary columns 

    if (rff == "xlsx") {    
        dt_pmdb_excl <- read_excel(PMDB_FILE, sheet = "Blad1") %>% adt() %>% .[2:.N]
    } else if (rff == "copy") {
        dt_pmdb_excl <- copy(DT_EXCL)
    } else if (rff == "csv") {
        dt_pmdb_excl <- fread(paste0(PMDB_DIR, "dt_pmdb_excl.csv"))
    } else if (rff == "sheets") {
        dt_pmdb_excl <- adt(read_sheet(PMDB_URL))[2:.N]
        tb_pmdb_excl <- read_sheet(PMDB_URL)
    }
    
    ## dtx <- read.xlsx2(PMDB_FILE, sheetName = "Blad1") %>% adt() %>% .[2:.N]
    ## dtx[653:.N] %>% melt(id.vars = "ID") %>% .[value != ""]


    ## some additional basic aux columns, no transformation
    basic_cols <- list(
        country = c("Country where museum is located", "Museum_country"),
        name = c("Museum_name", "Name of museum"),
        year_opened_str =  c("Opening year", "Museum_opening year"),
        year_closed_str = c("Closing year", "Museum_closing year"),
        museum_status = c("Museum status", "Museum_status"))

    ## throw error if any of the aux columns are already there
    if (len(setdiff(names(basic_cols), names(dt_pmdb_excl))) != len(names(basic_cols))) {
        stop("new cols already there")}

    xx <- dt_pmdb_excl[, .SD, .SDcols = unlist(map(basic_cols, ~vrbl_fndr(dt_pmdb_excl, .x)))] %>%
        setnames(new = names(basic_cols))
   
    as.character(xx$year_closed_str)
    xx[, ID_len := len(ID), .(name)][ID_len > 1]
    class(xx$year_closed_str)
    apply(dt_pmdb_excl, 2, typeof)

    dt_pmdb_excl[, `:=`(ID2 = paste0(ID), IDl = ID[[1]]), Museum_name][, .(ID, ID2, IDl)]
    select(tb_pmdb_excl, ID) %>% print(n=50)
    
    library(tidyr)
    unnest(tb_pmdb_excl, ID)

    list_cols <- sapply(dt_pmdb_excl, class) %>% imap(~list(col = .y, type = .x)) %>% rbindlist() %>%
        .[type == "list", col]
    
    dt_pmdb_excl[, .SD, .SDcols = list_cols]

    dt_pmdb_excl[, (list_cols) := map(list_cols, ~get(paste0(.x)))]
    dt_pmdb_excl[, (list_cols) := map(list_cols, ~get(paste0(.x))), Museum_name]

    xx <- dt_pmdb_excl %>% copy() %>% .[, (list_cols) := map(list_cols, ~paste0(get(.x))), Museum_name] %>%
        .[, (list_cols) := map(list_cols, ~unlist(get(.x)))]
    ## using get(.x)[[1]] causes some memory crash 

    ## xx$ID


    v_idtcl <- Vectorize(identical)

    dt_pmdb_excl[, .(ID, Museum_name)] %>% copy() %>%
        .[, `:=`(ID2 = paste0(ID), ID3 = paste0(ID[[1]])), Museum_name] %>% .[!v_idtcl(ID2, ID3)]
    
    dt_pmdb_excl[, (list_cols) := map(list_cols, ~unlist(get(.x))), Museum_name]
    dt_pmdb_excl$ID

    ## select basic_cols subset of columns, rename it
    dt_pmdb_excl2 <- dt_pmdb_excl[, .SD, .SDcols = unlist(map(basic_cols, ~vrbl_fndr(dt_pmdb_excl, .x)))] %>% 
        setnames(new = names(basic_cols)) %>% # update names 
        cbind(dt_pmdb_excl) %>% # combine with old data
        .[, `:=`(ID =as.integer(ID), # edit ID columns 
                 iso3c = countrycode(country, "country.name", "iso3c"), # add iso3c column
                 year_opened = as.integer(year_opened_str),
                 year_closed = as.integer(year_closed_str))] 

    return(dt_pmdb_excl2)
        
}


PMDATA_DIR <- "/home/johannes/Dropbox/phd/pmdata/"
PMDB_DIR <- paste0(PMDATA_DIR, "data_sources/pmdb/")
PMDB_FILE <- paste0(PMDB_DIR, "Private museum database_v1.xlsx")
PMDB_URL <- "https://docs.google.com/spreadsheets/d/1WO3FRh0mbUYi_jgTvD1u3zExg0DH17awqG7NVrfYsKQ/edit#gid=550520953"
## maybe add version number later


DT_EXCL <- read_excel(PMDB_FILE) %>% adt() %>% .[2:.N]

fwrite(DT_EXCL, paste0(PMDB_DIR, "dt_pmdb_excl.csv"))
dtx2 <- fread(paste0(PMDB_DIR, "dt_pmdb_excl.csv"))

gendt_pmdb_excl(PMDB_FILE, only_pms = F, rff = "sheets")





x <- read_sheet(PMDB_URL)





for(i in 1:10) {
    ## dtx <- leaker_test()
    dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F, rff = "sheets")
    rm(dtx)    
    gc()
    print(as.integer(mem_used()))
}




dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F, rff = T)

fwrite(
object_size(dtx)

## memory profiling

mem_change(dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F))
object_size(dtx)


for(i in 1:40) {
    dtx <- gendt_pmdb_excl(PMDB_FILE, only_pms = F)
    rm(dtx)    
    gc()
    print(pryr::mem_used())
}



