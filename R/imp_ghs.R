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
    
    ## Create a point object
    center_point <- st_as_sf(data.frame(lon = lon, lat=lat), coords = c("lon", "lat"), crs = 4326)

    ## Create a buffer around the point (with a projected CRS)
    buffer <- st_buffer(center_point, dist = radius_meters)

    ## extract coordinates as DT
    dt_buffer <- st_coordinates(buffer) %>% adt %>%
        .[, .(lon = X, lat = Y)]

    
    return(dt_buffer)
}


test_gd_circle <- function()  {
    #' test that circles are properly generated (taking curvature into account)

    dt_centers_test <- data.table(id = c("eq","pole"), lat = c(10,80), lon = c(0,0))

    dt_circres <- dt_centers_test[, gd_circle(lat, lon, 10e3), id]
    ## dt_circres3 <- dt_centers_test[, gd_circle3(lat, lon, 10e3), id]

    dt_circres %>% melt(id.vars = "id", variable.name = "vrbl", value.name = "vlu") %>%
        .[, .(min = min(vlu), max = max(vlu)), .(id, vrbl)] %>%
        .[, range := max - min] %>%
        .[vrbl == "lon"] %>% dcast(vrbl ~ id, value.var = "range") %>%
        .[, pole > eq]

}



imp_ghsl <- function(DIR_GHSL = PMDATA_LOCS$DIR_GHSL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
        
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    DIR_GHSL

    file2k <- paste0(DIR_GHSL, "GHS_POP_E2000_GLOBE_R2023A_54009_1000_V1_0.tif")
    library(terra)
    library(sf)

    describe(file2k)


    ## read raster
    dr_pop2k <- rast(file2k)
    ## dsf_pop2k <- st_read(file2k)

    ## values(dr_pop2k)[1:10]

    ## extract supports Spatvecotr -> maybe can generate circle
    ## example with NY
    
    lat_NY <- 40.7128  # Latitude of New York City
    lon_NY <- -74.0060 # Longitude of New York City

    vx_ny <- gd_circle(lat_NY, lon_NY, 10000) %>% as.matrix %>% 
        vect(type= "polygons", crs = "WGS84")

    ## plot(vx_ny)
    ## extract(dr_pop2k, vx_ny, weights = T) %>% adt %>% .[, #  %>% adt %>% names

    ## with actual locations

    dt_centers <- data.table(city = c("NY", "HH", "BJ"),
                             lat = c(40.7128, 53.5488, 39.9042),
                             lon = c(-74.0060, 9.9872, 116.4074))
    
    
    dt_city_circles <- dt_centers[, gd_circle(lat, lon, 20e3), city] %>%
        .[, .(object = as.numeric(factor(city)), part = 1, x= lon, y=lat)] %>%
        as.matrix # somehow matrix works best to convert to SpatVector later on
    
        
    ## plot(x = dt_city_circles$circle_lon, y=dt_city_circles$circle_lat)

    v_ccls <- vect(dt_city_circles, crs = "WGS84", type = "polygons") %>% # geom = c("x", "y"))
        project(dr_pop2k)
    

    extract(dr_pop2k, v_ccls, weights = T) %>% adt %>%
        setnames(new = .c(ID,  value, weight)) %>% na.omit %>%
        .[, sum(value * weight), ID]
        

    ## try another way of getting circles with st_buffer
    
    ## check if it works with extreme values

    

        


}

PMDATA_LOCS <- gc_pmdata_locs()

## imp_ghsl(PMDATA_LOCS$DIR_GHSL)


    
    
    
    
    
    

    }
