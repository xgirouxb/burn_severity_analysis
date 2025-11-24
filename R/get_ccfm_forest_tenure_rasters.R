get_ccfm_forest_tenure_rasters <- function(
    study_fire_sampling_polygons
){
  # Raster attribute table for Canadian Council of Forest Minister's tenure map
  tenure_rat <- data.frame(
    value = c(11, 12, 13, 20, 31, 32, 33, 40, 50, 100),
    class =  c('long_term', 'short_term', 'other', 'protected', 
               'fed_reserve', 'fn_reserve', 'restricted',
               'fn_treaty', 'private', 'water')
  )
  
  # Download/read 2017 CCFM forest tenure archived raster
  ccfm_tenure_2017 <- get_archive_from_url(url_ccfm_forest_tenure_2017) %>% 
    fs::dir_ls() %>% 
    stringr::str_subset("\\.tif$") %>% 
    terra::rast()
  names(ccfm_tenure_2017) <- "ccfm_tenure"
  
  # Download/read 2020 CCFM forest tenure archived raster
  ccfm_tenure_2020 <- get_archive_from_url(url_ccfm_forest_tenure_2020) %>% 
    fs::dir_ls() %>% 
    stringr::str_subset("\\.tif$") %>% 
    terra::rast()
  names(ccfm_tenure_2020) <- "ccfm_tenure"
  
  # Define/create local cache directory for CCFM forest tenure rasters
  tenure_cache <- fs::path("data/_cache/ccfm_forest_tenure")
  if (!fs::dir_exists(tenure_cache)) { fs::dir_create(tenure_cache) }
  
  # Create list of CCFM forest tenure for each study fire
  ccfm_tenure_files <- study_fire_sampling_polygons %>% 
    # Nest by study fire
    dplyr::group_nest(fire_id) %>% 
    # Get tenure from 2017 or 2020 CCFM forest tenure maps
    dplyr::mutate(
      tenure_file_path = purrr::map2(
        .x = data,
        .y = fire_id,
        .f = ~ {
          # Study fire year
          fire_year <- max(.x$fire_year)
          
          # Choose 2017 or 2020 forest tenure map based on study fire year
          map_years <- c(2017, 2020) 
          closest_map_year <- map_years[which.min(abs(map_years - fire_year))]
          if(closest_map_year == 2017) { ccfm_tenure <- ccfm_tenure_2017 }
          if(closest_map_year == 2020) { ccfm_tenure <- ccfm_tenure_2020 }
          
          # Reproject study fire polygon to forest tenure map projection
          fire_lambert <- terra::project(
            x = terra::vect(.x),
            y = terra::crs(ccfm_tenure)
          )
          
          # Crop/mask the forest tenure layers to study fire area
          study_fire_ccfm_tenure <- terra::crop(
            x = ccfm_tenure,
            y = fire_lambert,
            mask = TRUE
          )
          
          # Add raster attribute table
          levels(study_fire_ccfm_tenure) <- tenure_rat
          
          # File name and path for writing to file
          tenure_file_name <- fs::path(tenure_cache, paste0(.y, ".tif"))
          
          # Write fire and harvest disturbance years to file  
          terra::writeRaster(
            # Reproject in study proj
            x = terra::project(
              x = study_fire_ccfm_tenure,
              y = study_proj,
              res = 30
            ), 
            filename = tenure_file_name,
            datatype = "INT1U",
            overwrite = TRUE
          )
          
          # Return file name
          tenure_file_name
        }
      )
    ) %>%
    # Clean-up
    dplyr::select(-data)
  
  # Return list of CCFM forest tenure raster file names
  return(ccfm_tenure_files)
}