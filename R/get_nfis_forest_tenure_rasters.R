get_nfis_forest_tenure_rasters <- function(
    study_fire_sampling_polygons
){
  
  # Download/read 2017 NFIS forest management asrchived raster
  nfis_tenure_2017 <- get_archive_from_url(url_forest_management_2017) %>% 
    fs::dir_ls() %>% 
    stringr::str_subset("\\.tif$") %>% 
    terra::rast()
  names(nfis_tenure_2017) <- "nfis_tenure"
  
  # Download/read 2020 NFIS forest management asrchived raster
  nfis_tenure_2020 <- get_archive_from_url(url_forest_management_2020) %>% 
    fs::dir_ls() %>% 
    stringr::str_subset("\\.tif$") %>% 
    terra::rast()
  names(nfis_tenure_2020) <- "nfis_tenure"
  
  # Define/create local cache directory for NFIS forest management rasters
  tenure_cache <- fs::path("data/_cache/forest_management")
  if (!fs::dir_exists(tenure_cache)) { fs::dir_create(tenure_cache) }
  
  # Create list of NFIS forest management for each study fire
  nfis_tenure_files <- study_fire_sampling_polygons %>% 
    # Nest by study fire
    dplyr::group_nest(fire_id) %>% 
    # Get tenure from 2017 or 2020 NFIS forest management maps
    dplyr::mutate(
      tenure_file_path = purrr::map2(
        data,
        fire_id,
        ~{
          # Study fire year
          fire_year <- max(.x$fire_year)
          
          # Choose 2017 or 2020 forest management map based on study fire year
          map_years <- c(2017, 2020) 
          closest_map_year <- map_years[which.min(abs(map_years - fire_year))]
          if(closest_map_year == 2017) { nfis_tenure <- nfis_tenure_2017}
          if(closest_map_year == 2020) { nfis_tenure <- nfis_tenure_2020}
          
          # Reproject study fire polygon to forest map projection
          fire_lambert <- terra::project(
            x = terra::vect(.x),
            y = terra::crs(nfis_tenure)
          )
          
          # Crop/mask the forest tenure layers closest to fire year to study fire area
          study_fire_nfis_tenure <- terra::crop(
            x = nfis_tenure,
            y = fire_lambert,
            mask = TRUE
          )
          
          # File name and path for writing to file
          tenure_file_name <- fs::path(tenure_cache, paste0(.y, ".tif"))
          
          # Write fire and harvest disturbance years to file  
          terra::writeRaster(
            # Reproject in study proj
            x = terra::project(
              x = study_fire_nfis_tenure,
              y = study_proj,
              res = 30
            ), 
            filename = tenure_file_name,
            datatype = "INT2U",
            overwrite = TRUE
          )
          
          # Return file name
          tenure_file_name
        }
      )
    ) %>%
    # Clean-up
    dplyr::select(-data)
}