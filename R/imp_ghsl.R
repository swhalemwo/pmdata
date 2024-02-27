## * import GHSL data


# Function to create a circle polygon
## gd_circle <- function(lat, lon, radius_meters, num_points = 100) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
##     ## Convert radius from meters to degrees (approximation)
##     radius_degrees <- radius_meters / 111000
    
##     ## Generate angles
##     angles <- seq(0, 2 * pi, length.out = num_points)
    
##     ## Generate coordinates of circle vertices
##     circle_lat <- lat + radius_degrees * cos(angles)
##     circle_lon <- lon + radius_degrees * sin(angles)
    
##     ## Create polygon
##     dt_circle_poly <- data.table(lon = circle_lon, lat = circle_lat)

    

##     return(dt_circle_poly)
## }


gd_circle <- function(lat, lon, radius_meters, num_points = 100) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    X <- Y <- NULL

    ## Create a point object
    center_point <- sf::st_as_sf(data.frame(lon = lon, lat=lat), coords = c("lon", "lat"), crs = 4326)

    ## Create a buffer around the point (with a projected CRS)
    buffer <- sf::st_buffer(center_point, dist = radius_meters)

    ## extract coordinates as DT
    dt_buffer <- sf::st_coordinates(buffer) %>% adt %>%
        .[, .(lon = X, lat = Y)]

    
    return(dt_buffer)
}


test_gd_circle <- function()  {
    #' test that circles are properly generated (taking curvature into account)

    lat <- lon <- id <- vlu <- vrbl <- pole <- eq <- ID <- NULL


    dt_centers_test <- data.table(id = c("eq","pole"), lat = c(10,80), lon = c(0,0))

    dt_circres <- dt_centers_test[, gd_circle(lat, lon, 10e3), id]
    ## dt_circres3 <- dt_centers_test[, gd_circle3(lat, lon, 10e3), id]

    dt_circres %>% melt(id.vars = "id", variable.name = "vrbl", value.name = "vlu") %>%
        .[, .(min = min(vlu), max = max(vlu)), .(id, vrbl)] %>%
        .[, range := max - min] %>%
        .[vrbl == "lon"] %>% dcast(vrbl ~ id, value.var = "range") %>%
        .[, pole > eq]

}

#' generate population amounts around coordinates using GHSL
#' @param dt_coords data.table with lat, lon (in degrees) and numeric ID
#' @param id_vrbl variable in dt_coords to identify rows, must be unique
#' @param year year of GHSL
#' @param radius radius around coordinates in meters
#' @param DIR_GHSL directory with the GHSL files, to be provided by PMDATA_LOCS
#' @return data.table with population numbers
#' @export 
imp_ghsl <- function(dt_coords, id_vrbl, year, radius, DIR_GHSL = PMDATA_LOCS$DIR_GHSL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    id_num <- lat <- lon <- value <- weight <- NULL
    
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
    
    ## generate circles for Spatvectors as matrix
    mat_coord_circles <- dt_coords[, gd_circle(lat, lon, radius), id_vrbl] %>%
        .[, .(object = get(id_vrbl), part = 1, x= lon, y=lat)] %>%
        as.matrix # somehow matrix works best to convert to SpatVector later on
    
    ## generate SpatVector circles
    SV_circles <- terra::vect(mat_coord_circles, crs = "WGS84", type = "polygons") %>%
        terra::project(dr_pop) # align CRS
    
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
test_imp_ghsl <- function(PMDATA_LOCS) {
    
    ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' it's testing time
    ## lat_NY <- 40.7128  # Latitude of New York City
    ## lon_NY <- -74.0060 # Longitude of New York City

    ## vx_ny <- gd_circle(lat_NY, lon_NY, 10000) %>% as.matrix %>% 
    ##     vect(type= "polygons", crs = "WGS84")

    ## plot(vx_ny)
    ## extract(dr_pop2k, vx_ny, weights = T) %>% adt %>% .[, #  %>% adt %>% names

    ## with actual locations

    ID <- NULL

    dt_centers <- data.table(city = c("NY", "HH", "BJ"),
                             lat = c(40.7128, 53.5488, 39.9042),
                             lon = c(-74.0060, 9.9872, 116.4074))

    dt_centers_numid <- copy(dt_centers)[, ID := 1:3]


    dt_res <- imp_ghsl(dt_centers, id_vrbl = "city", year=2000, radius = 3e4,
                       DIR_GHSL = PMDATA_LOCS$DIR_GHSL)

    dt_res_previously <- data.table(city = c("NY", "HH", "BJ"),
                                    pop = c(11698576, 2506907, 10781604),
                                    year = 2000)

    ## compare: somehow default tolerance sqrt(.Machine$double.eps), which is 1.490116e-08 finds difference
    all.equal(dt_res, dt_res_previously, tolerance = 0000000000000000000000000000000000.1)
    

}



## PMDATA_LOCS <- gc_pmdata_locs()
## test_imp_ghsl(PMDATA_LOCS)

## imp_ghsl(PMDATA_LOCS$DIR_GHSL)



    
    
    
    
    
