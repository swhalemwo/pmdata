#' downloads World Bank data using wbstats API
#'
#' @param indx character of WB variable names, or vector thereof
download_WB_data <- function(indx){
    #' downloads data with the WB API, can take single WB variable codes or vectors thereof
    print(paste0("now downloading: ", paste0(indx, collapse = ", ")))
    indx_data <- wb_data(indicator = indx) # , start_date = STARTING_YEAR, end_date = ENDING_YEAR)
    
    date_pos <- which(names(indx_data)=="date")
    names(indx_data)[date_pos] <- "year"

    return(indx_data[,c("iso3c", "country", "year", indx)])
}



gd_WBdata_fromfile <- function() {
    dt_wb <- fread(paste0(c_dirs$data, "WB_data_proc.csv"))
    return(dt_wb)
}



get_WB_data <- function(indx, refresh_all=FALSE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' get WB variables, download all new if asked to do so, else read from file

    ## download everything if no file is there
    if ("WB_data_proc.csv" %in% list.files(c_dirs$data)) {
        df_wb_local <- read_WB_data()
        wb_vars_there <- names(df_wb_local)[4:len(names(df_wb_local))]
        print(paste0("wb_vars_there: ", paste0(wb_vars_there, collapse = ", ")))
        nothing_there <- FALSE

    } else {
        nothing_there <- TRUE
        refresh_all <- TRUE
        wb_vars_there <- c()
    }
        

    if (refresh_all) {
        print(paste0("download all indicators anew: ", paste0(indx, collapse = ", ")))
        ## indx <- c(indx,  "NY.GDP.PCAP.KD.ZG")
        df_wb_api <- download_WB_data(indx)

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% indx)]

        ## if (nothing_there) {
            ## assign result of API call to have something to be able to not change merge syntax below
            df_wb_local <- df_wb_api[,c("iso3c", "country", "year")]
        ## }
        
    } else {
        wb_vars_to_download <- indx[which(indx %!in% wb_vars_there)]
        print(paste0("number of vars to download: ", length(wb_vars_to_download)))
        print(paste0("download selected indicator(s) anew: ", wb_vars_to_download))
        
        if (length(wb_vars_to_download) > 0) {
            df_wb_api <- download_WB_data(wb_vars_to_download)
        } else {
            df_wb_api <- df_wb_local[,c("iso3c", "country", "year")]
        }

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% wb_vars_to_download)]
        print(paste0("unchanged_vars: ", paste0(unchanged_vars, collapse = ", ")))
        ## need to this out: how to get the variables which shouldn't be changed; think i got it now
    }

    df_wb_new <- merge(df_wb_local[,c("iso3c", "country", "year", unchanged_vars)], df_wb_api) %>% adt %>% 
        .[!is.na(countrycode(iso3c, "iso3c", "country.name"))] %>%
        atb() # filter out countries that don't conform to iso3c standard (for now CHI)

    

    ## inspect coverage 
    ## colSums(is.na(select(df_wb_new, NY.GDP.PCAP.CD, NY.GDP.MKTP.CN)))

    ## convert to constant USD, using WID data
    ## actually also needs NY.GDP.MKTP.CN to be present for comparison.. could be yeeted tho
    if ("NY.GDP.PCAP.CD" %in% indx) {
        df_wb_new <- cvrt_ny.gdp.pcap.cd(df_wb_new)
    }

    save_WB_data(df_wb_new)

    return(df_wb_new[,c("iso3c", "country", "year", indx)])
    
}


get_WB_data("SP.POP.TOTL")
