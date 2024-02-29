## * import GHSL data



#' generate population amounts around coordinates using GHSL 
#' @param dt_coords data.table with lat, long (in degrees) and numeric ID
#' @param id_vrbl variable in dt_coords to identify rows, must be unique
#' @param year year of GHSL
#' @param radius_km radius around coordinates in meters
#' @param DIR_GHSL directory with the GHSL files, to be provided by PMDATA_LOCS
#' @return data.table with population numbers
#' @export 
gd_popcircle <- function(dt_coords, id_vrbl, year, radius_km, DIR_GHSL = PMDATA_LOCS$DIR_GHSL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    id_num <- lat <- long <- value <- weight <- NULL
    
    dt_coords <- copy(dt_coords) # copy coordinates to not overwrite input (for non-int IDs)

    ## check that input data is correct
    if (year %!in% seq(1975, 2030, 5)) {stop("year not available")}
    if (dt_coords[, fnunique(get(id_vrbl))] != fnrow(dt_coords)) {stop("IDs not unique")}

    ## create backup IDs to be able to rename dt_coords vrbl
    dt_id <- dt_coords[, .SD, .SDcols = id_vrbl] %>%
        .[, id_num := as.integer(factor(get(id_vrbl), levels = get(id_vrbl)))]

    ## overwrite IDs to integer IDs
    dt_coords[, (id_vrbl) := as.integer(factor(get(id_vrbl), levels = get(id_vrbl)))]

    file_raster_pop <- sprintf("%sGHS_POP_E%s_GLOBE_R2023A_54009_1000_V1_0.tif", DIR_GHSL, year)
    ## describe(file_raster_pop)

    ## read raster
    dr_pop <- terra::rast(file_raster_pop)
    
    ## generate SpatVector circles
    SV_circles <- terra::vect(dt_coords, crs = "WGS84", geom = c("long", "lat")) %>%  # df always type = "points"
        terra::buffer(width = radius_km * 1000) %>%
        terra::project(dr_pop)
    
    ## extract raster cells matched by circle polygons, weigh by overlap
    dt_popres <- terra::extract(dr_pop, SV_circles, weights = T) %>% adt %>%
        setnames(new = c(id_vrbl,  "value", "weight")) %>% na.omit %>%
        .[, .(pop = sum(value * weight), year = year), id_vrbl]
    
    ## put original IDs back
    dt_popres <- join(dt_id, dt_popres,  on = c("id_num" = id_vrbl)) %>%
        .[, .SD, .SDcols  = c(id_vrbl, "pop", "year")]


    return(dt_popres)
    
}


#' test that imp_ghsl works
#' @param PMDATA_LOCS list of file locations (documented to make R CMD check happy)
test_gd_popcircle <- function(PMDATA_LOCS) {

    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' it's testing time
    ## lat_NY <- 40.7128  # Latitude of New York City
    ## long_NY <- -74.0060 # Longitude of New York City

    ## vx_ny <- gd_circle(lat_NY, long_NY, 10000) %>% as.matrix %>% 
    ##     vect(type= "polygons", crs = "WGS84")

    ## plot(vx_ny)
    ## extract(dr_pop2k, vx_ny, weights = T) %>% adt %>% .[, #  %>% adt %>% names

    ## with actual locations

    ID <- NULL

    dt_centers <- data.table(city = c("NY", "HH", "BJ"),
                             lat = c(40.7128, 53.5488, 39.9042),
                             long = c(-74.0060, 9.9872, 116.4074))

    dt_centers_numid <- copy(dt_centers)[, ID := 1:3]


    dt_res <- gd_popcircle(dt_centers, id_vrbl = "city", year=2000, radius_km = 30,
                           DIR_GHSL = PMDATA_LOCS$DIR_GHSL)

    dt_res_previously <- data.table(city = c("NY", "HH", "BJ"),
                                    ## pop = c(11698576, 2506907, 10781604),
                                    pop = c(11637116, 2494977, 10728866),
                                    year = 2000)

    isTRUE(all.equal(dt_res, dt_res_previously,  tolerance = 0.01))

        

}




## PMDATA_LOCS <- gc_pmdata_locs()
## test_imp_ghsl(PMDATA_LOCS)

## imp_ghsl(PMDATA_LOCS$DIR_GHSL)



    
    
    
    
    
