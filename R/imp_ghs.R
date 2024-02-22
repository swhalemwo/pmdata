## * import GHSL data


# Function to create a circle polygon
gv_circle <- function(lat, lon, radius_meters, num_points = 100) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## Convert radius from meters to degrees (approximation)
    radius_degrees <- radius_meters / 111000
    
    ## Generate angles
    angles <- seq(0, 2 * pi, length.out = num_points)
    
    ## Generate coordinates of circle vertices
    circle_lat <- lat + radius_degrees * cos(angles)
    circle_lon <- lon + radius_degrees * sin(angles)
    
    ## Create polygon
    dt_circle_poly <- data.table(lat = circle_lat, lon = circle_lon)

    

    return(dt_circle_poly)
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

    ## convert to DT
    dt_pop2k <- adf(dr_pop2k)
    
    ## subset and plot some section

    dr_pop2k[dr_pop2k$x < 100] 

    ## values(dr_pop2k)[1:10]
    hasValues(dr_pop2k)
    max(dr_pop2k)

    values(dr_pop2k, row = 5000 , nrows = 3, col=9000, ncols = 6, mat = T)
    extract(dr_pop2k)
    ## extract supports Spatvecotr -> maybe can generate circle

    
    subset(dr_pop2k, subset = T, select = 9000:9010)

    
    vx <- data.table(x=c(1,2000,2000,1,1), y=c(1,1,2000,2000,1)) %>% as.matrix %>% 
        vect(type = "polygons")
    
    
    extract(dr_pop2k, vx)

    lat_NY <- 40.7128  # Latitude of New York City
    lon_NY <- -74.0060 # Longitude of New York City
    radius_meters <- 10000 # 10 kilometers

    lat_HH <- 53.5488
    lon_HH <- 9.9872
    
    vx_ny <- gv_circle(center_lat, center_lon, 10000) %>%
        vect(type= "polygons", crs = "WGS84")

    plot(vx_ny)
    extract(dr_pop2k, vx_ny, weights = T) %>% adt %>% names

    dt_centers <- data.table(city = c("NY", "HH"), lat = c(40.7128, 53.5488), lon = c(-74.0060, 9.9872))

    dt_city_circles <- dt_centers[, gv_circle(lat, lon, 50e3), city] %>%
        .[, .(object = as.numeric(factor(city)), part = 1, x= lon, y=lat)] %>% as.matrix
        
        

    ## plot(x = dt_city_circles$circle_lon, y=dt_city_circles$circle_lat)

    v_ccls <- vect(dt_city_circles, crs = "WGS84", type = "polygons") %>% # geom = c("x", "y"))
        project(dr_pop2k)
    

    extract(dr_pop2k, v_ccls, weights = T) %>% adt %>%
        setnames(new = .c(ID,  value, weight)) %>% na.omit %>%
        .[, sum(value * weight), ID]
        
        

}

PMDATA_LOCS <- gc_pmdata_locs()

## imp_ghsl(PMDATA_LOCS$DIR_GHSL)
