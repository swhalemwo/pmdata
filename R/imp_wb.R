#' downloads World Bank data using wbstats API
#'
#' @param indx character of WB variable names, or vector thereof
gd_WB_download <- function(indx){
    
    print(paste0("now downloading: ", paste0(indx, collapse = ", ")))
    indx_data <- wb_data(indicator = indx)  # , start_date = STARTING_YEAR, end_date = ENDING_YEAR)

    ## rename date to year
    dt_indx_data_proc <- as.data.table(indx_data) %>% setnames(old = "date", new = "year") %>%
        .[, .SD, .SDcols = c("iso3c", "country", "year", indx)]
    ## date_pos <- which(names(indx_data)=="date")
    ## names(indx_data)[date_pos] <- "year"

    ## return(indx_data[,c("iso3c", "country", "year", indx)])
    return(dt_indx_data_proc)
}


#' generates World bank data
#' to not requery on every run, it writes requested data to WB_data_proc.csv
#'
#' column format for written data is iso3c, country.name, year, and then the indexes requested
#' 
#' the overall idea is that WB data is project-specific:
#' intended to pass c_dirs$data (c_dirs from jtls) as DIR_WBproc
#' 
#' @param indx character of WB variable names, or vector thereof
#' @param DIR_WBproc directory where to save the data
#' @param refresh_all if T, download all variables agains
#' @export
gd_WB <- function(indx, DIR_WBproc, refresh_all=FALSE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## get WB variables, download all new if asked to do so, else read from file

    ## check what is there from previous runs
    ## if something is there, use it (only download new ones, unless refresh_all = T)
    if ("WB_data_proc.csv" %in% list.files(DIR_WBproc)) {
        df_wb_local <- fread(paste0(DIR_WBproc, "WB_data_proc.csv"))
        wb_vars_there <- names(df_wb_local)[4:length(names(df_wb_local))]
        print(paste0("wb_vars_there: ", paste0(wb_vars_there, collapse = ", ")))
        nothing_there <- FALSE

        ## download everything if no file is there
    } else {
        nothing_there <- TRUE
        refresh_all <- TRUE
        wb_vars_there <- c()
    }
        
    downloaded_something <- F

    if (refresh_all) {
        print(paste0("download all indicators anew: ", paste0(indx, collapse = ", ")))
        ## indx <- c(indx,  "NY.GDP.PCAP.KD.ZG")
        downloaded_something <- T
        df_wb_api <- gd_WB_download(indx)

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
            df_wb_api <- gd_WB_download(wb_vars_to_download)
            downloaded_something <- T
        } else {
            df_wb_api <- df_wb_local[,c("iso3c", "country", "year")]
        }

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% wb_vars_to_download)]
        print(paste0("unchanged_vars: ", paste0(unchanged_vars, collapse = ", ")))
        ## need to this out: how to get the variables which shouldn't be changed; think i got it now
    }

    df_wb_new <- merge.data.table(df_wb_local[,c("iso3c", "country", "year", unchanged_vars), with = F],
                                  df_wb_api,
                                  by = c("iso3c", "country", "year"))
        ## .[!is.na(countrycode(iso3c, "iso3c", "country.name"))]
    ## filter out countries that don't conform to iso3c standard (for now CHI)

    
    ## inspect coverage 
    ## colSums(is.na(select(df_wb_new, NY.GDP.PCAP.CD, NY.GDP.MKTP.CN)))

    ## write to 
    if (downloaded_something) {
        fwrite(df_wb_new, file = paste0(DIR_WBproc, "WB_data_proc.csv"))
    }

    return(df_wb_new[,c("iso3c", "country", "year", indx), with = F])
    
}


## gd_WB("SP.POP.TOTL", DIR_WBproc = "/home/johannes/Dropbox/phd/papers/closing/data/", refresh_all = T)
