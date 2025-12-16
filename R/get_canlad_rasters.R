get_canlad_disturbance_rasters <- function(study_fire_sampling_polygons) {
  
  # Define CanLaD years to download (year preceding most recent study fire)
  canlad_years <- 1985:(max(study_years) - 1)
  
  # Define local cache directory for CanLaD rasters, create if it doesn't exist
  canlad_cache <- fs::path("data/_cache/canlad")
  if (!fs::dir_exists(canlad_cache)) { fs::dir_create(canlad_cache) }
  raw_canlad_cache <- fs::path("data/_cache/canlad/raw")
  if (!fs::dir_exists(raw_canlad_cache)) { fs::dir_create(raw_canlad_cache) }
  
  # Get URLs for CanLaD rasters to cache locally
  raster_urls <- get_url_list(url_canlad_1985_2024) %>%
    stringr::str_subset(paste(canlad_years, collapse = "|"))
  
  # Create CanLaD raster file paths for local cache
  raster_paths <- fs::path(raw_canlad_cache, fs::path_file(raster_urls))
  
  # Check for uncached CanLaD rasters
  uncached_raster_urls <- raster_urls[!fs::file_exists(raster_paths)]
  
  # If a raster is missing from local cache, download and cache it
  if (length(uncached_raster_urls) > 0) {
    purrr::walk(
      uncached_raster_urls,
      function(url) { get_tif_from_url(url, output_dir = raw_canlad_cache) }
    )
  }
  
  # Sanity check, stop if cache does not contain all the CanLaD rasters
  if (!setequal(raster_paths, fs::dir_ls(raw_canlad_cache))) {
    stop("There are uncached CanLaD rasters.")
  }
  
  # Get CanLaD projection (Canada Lambert)
  canlad_proj <- terra::crs(terra::rast(raster_paths[1]))
  
  # Create list of CanLaD most recent harvest and fire years for each study fire
  canlad_disturbance_paths <- study_fire_sampling_polygons %>% 
    # Split by study fire
    dplyr::group_split(fire_id) %>% 
    # Get year of most recent harvest and most recent wildfire in CanLaD stack
    purrr::map(
      function(study_fire) {
        
        # Study fire attributes
        fire_year <- study_fire$fire_year
        fire_id <- study_fire$fire_id
        
        # Define year range for CanLaD stack (1985 to 1 year before study fire)
        years <- 1985:(max(fire_year) - 1)
        
        # Subset CanLaD raster stack paths
        stack_paths <- stringr::str_subset(
          raster_paths,
          paste(years, collapse = "|")
        )
        
        # Load, and crop/mask each CanLaD annual raster to study fire area
        list_canlad_annual_rasters <- purrr::map(
          stack_paths,
          function(path) {
            terra::crop(
              x = terra::rast(path),
              # Crop to reprojected study fires to CanLaD Lambert proj
              y = terra::project(terra::vect(study_fire), canlad_proj),
              mask = TRUE
            )
          }
        )
        
        # Get the year of most recent CanLaD fire disturbance in stack
        canlad_fire_year <- purrr::map(
          list_canlad_annual_rasters,
          function(r) { terra::ifel(r == 1, readr::parse_number(names(r)), NA) }
        ) %>%
          # Stack annual rasters, then compute pixel-wise max of wildfire years
          terra::rast() %>%
          terra::app(fun = "max", na.rm = TRUE)
        names(canlad_fire_year) <- "canlad_fire_year"
        
        # Get the year of most recent CanLaD harvest disturbance in stack
        canlad_harvest_year <- purrr::map(
          list_canlad_annual_rasters,
          # Substitute annual harvest pixels (2) with year
          function(r) { terra::ifel(r == 2, readr::parse_number(names(r)), NA) }
        ) %>%
          # Stack annual rasters, then compute pixel-wise max of harvest years
          terra::rast() %>%
          terra::app(fun = "max", na.rm = TRUE)
        names(canlad_harvest_year) <- "canlad_harvest_year"
        
        # File name and path for writing raster to file
        raster_file_path <- fs::path(canlad_cache, paste0(fire_id, ".tif"))
        
        # Write fire and harvest disturbance years to file  
        terra::writeRaster(
          # Create 2 layer raster and reproject in study proj
          x = terra::project(
            x = c(canlad_fire_year, canlad_harvest_year),
            y = study_proj,
            res = 30
          ), 
          filename = raster_file_path,
          datatype = "INT2U",
          overwrite = TRUE
        )
        
        # Return tbl of raster file path
        tibble::tibble(
          fire_id = fire_id,
          raster_file_path = raster_file_path
        )
      }
    )
  
  # Return list of CanLaD disturbance raster file names
  return(canlad_disturbance_paths)
}

get_precanlad_disturbance_rasters <- function(study_fire_sampling_polygons) {
  
  # Define local cache directory for pre-CanLaD rasters
  precanlad_cache <- fs::path("data/_cache/precanlad")
  if (!fs::dir_exists(precanlad_cache)) { fs::dir_create(precanlad_cache) }
  raw_precanlad_cache <- fs::path("data/_cache/precanlad/raw")
  if (!fs::dir_exists(raw_precanlad_cache)) { 
    fs::dir_create(raw_precanlad_cache) 
  }
  
  # Get URLs for CanLaD rasters to cache locally
  raster_urls <- get_url_list(url_precanlad_1965_1984) %>%
    # Only disturbance type and year tif files
    stringr::str_subset("disturbanceType|disturbanceYear") %>% 
    stringr::str_subset("\\.tif$")
  
  # Create pre-CanLaD raster file paths for local cache
  raster_paths <- fs::path(raw_precanlad_cache, fs::path_file(raster_urls))
  
  # Check for uncached pre-CanLaD rasters
  uncached_raster_urls <- raster_urls[!fs::file_exists(raster_paths)]
  
  # If a raster is missing from local cache, download and cache it
  if (length(uncached_raster_urls) > 0) {
    purrr::walk(
      uncached_raster_urls,
      function(url) { get_tif_from_url(url, output_dir = raw_precanlad_cache) }
    )
  }
  
  # Sanity check, stop if cache does not contain all the pre-CanLaD rasters
  if (!setequal(raster_paths, fs::dir_ls(raw_precanlad_cache))) {
    stop("There are uncached pre-CanLaD rasters.")
  }
  
  # Get pre-CanLaD projection (Canada Lambert)
  precanlad_proj <- terra::crs(terra::rast(raster_paths[1]))
  
  # Create list of pre-CanLaD harvest and fire years for each study fire
  precanlad_disturbance_paths <- study_fire_sampling_polygons %>% 
    # Split by study fire
    dplyr::group_split(fire_id) %>% 
    # Get year of harvest and wildfire disturbances in pre-CanLaD image
    purrr::map(
      function(study_fire) {
        
        # Study fire attributes
        fire_year <- study_fire$fire_year
        fire_id <- study_fire$fire_id
        
        # Reproject fire to CanLaD projection
        fire_lambert <- terra::project(terra::vect(study_fire), precanlad_proj)
        
        # Mask pre-CanLaD disturbance years to study fire
        disturbance_year <- stringr::str_subset(raster_paths, "Year") %>% 
          terra::rast() %>%
          terra::crop(y = fire_lambert, mask = TRUE)
        
        # Mask pre-CanLaD disturbance type to study fire
        disturbance_type <- stringr::str_subset(raster_paths, "Type") %>% 
          terra::rast() %>%
          terra::crop(y = fire_lambert, mask = TRUE)
        
        # Create raster with years of fire disturbances pixels  (2)
        precanlad_fire_year <- terra::mask(
          x = disturbance_year,
          mask = terra::ifel(disturbance_type == 2, 1, NA)
        )
        names(precanlad_fire_year) <- "precanlad_fire_year"
        
        # Create raster with years of harvest disturbances pixels  (3)
        precanlad_harvest_year <- terra::mask(
          x = disturbance_year,
          mask = terra::ifel(disturbance_type == 3, 1, NA)
        )
        names(precanlad_harvest_year) <- "precanlad_harvest_year"
        
        # File name and path for writing to file
        raster_file_path <- fs::path(precanlad_cache, paste0(fire_id, ".tif"))
        
        # Write fire and harvest disturbance years to file  
        terra::writeRaster(
          # Create 2 layer raster and reproject to study proj
          x = terra::project(
            x = c(precanlad_fire_year, precanlad_harvest_year),
            y = study_proj,
            res = 30
          ),
          filename = raster_file_path,
          datatype = "INT2U",
          overwrite = TRUE
        )
        
        # Return tbl of raster file path
        tibble::tibble(
          fire_id = fire_id,
          raster_file_path = raster_file_path
        )
      }
    )
  
  # Return list of pre-CanLaD disturbance raster file names
  return(precanlad_disturbance_paths)
}