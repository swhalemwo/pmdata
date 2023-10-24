## * MOW import


gd_mow_info <- function(MOW_INFO_FILE = PMDATA_LOCS$MOW_INFO_FILE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    dt_mow_info <- fread(MOW_INFO_FILE) %>%
        .[, iso3c := countrycode(country, "country.name", "iso3c")]
    
    


    fileConn<-file("/home/johannes/Dropbox/phd/pmdata/data_sources/mow/output.txt")
    writeLines(c("Hello","World"), fileConn)
    close(fileConn)
        

    ## read in the matches to PMDB
    


    ## hmm there are a bunch whose opening date is stuff like 20th century, which gets parsed as 20
    ## dt_mow_info[founding_date1 < 100, .(name, founding_date1)] %>% print(n=300)

    ## dt_mow_info[founding_date1 < 1700, .(name, founding_date1)] %>% print(n=300)

    ## less than 1% (366) are before 1800, only 71 before 1500
    return(dt_mow_info)

    ## dt_mow_info[, .N, .(iso3c, founding_date1)] %>% na.omit %>%
    ##     .[, Nsum := sum(N), iso3c] %>%
    ##     .[, Nprop := N/Nsum] %>% 
    ##     .[Nsum > 500] %>% 
    ##     ggplot(aes(x=founding_date1, y=Nprop, group = iso3c)) + geom_line() +
    ##     xlim(1900,max(dt_mow_info$founding_date1, na.rm = T))
    
}

gd_mow_info(gc_pmdata_locs()$MOW_INFO_FILE)


gd_mow_tags <- function() {
}
    

    
gd_mow_dupl <- function() {
    dupl_res_file <- paste0(


}
                        
