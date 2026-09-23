prep_cef_road_rasters <- function(
    cef_road_lines,
    sampling_polygons,
    n_workers = NULL
) {
  
  # Create local cache for CEF road rasters
  roads_cache <- fs::dir_create("data/_cache/cef_roads")
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Create list of CEF road raster file paths for each study fire
  cef_road_raster_paths <- sampling_polygons %>% 
    # Split by study fire
    dplyr::group_split(fire_id) %>% 
    # Create list of road density and distance rasters written to cache
    furrr::future_map(
      function(study_fire) {
        
        # Study fire id
        study_fire_id <- unique(study_fire$fire_id)
        
        # Max distance to road
        max_dist <- 10000
        
        # Buffer study fire polygon, 10 km + 3*30m pixels
        study_fire_buffer <- sf::st_buffer(study_fire, dist = max_dist + 3*30)
        
        # Filter CEF roads within study fire polygon
        study_fire_roads <- cef_road_lines %>% 
          dplyr::filter(fire_id == study_fire_id)
        
        # Create template raster for buffered study fire area
        fire_rast <- terra::rast(
          x = terra::vect(study_fire_buffer),
          resolution = 30,
          vals = 1
        )
        
        # Mask template raster values outside AOI buffer
        fire_rast <- terra::crop(
          x = fire_rast,
          y = terra::vect(study_fire_buffer),
          mask = TRUE
        )
        
        # Compute road density (m/km^2) in 1 km neighbourhood window
        road_density_raster <- line_density(
          sf_lines = study_fire_roads,
          sf_aoi = study_fire,
          radius = 1000,
          template = fire_rast
        )
        
        # Name road density band
        names(road_density_raster) <- "cef_road_density_1000m"
        
        # Compute distance to nearest road, capped at 10 km
        if (nrow(study_fire_roads) == 0) {
          
          # If no roads, assign maximum distance
          road_distance_raster <- terra::subst(
            x = fire_rast,
            from = 1,
            to = max_dist
          )
          
        } else {
          
          # Compute distance to nearest road
          road_distance_raster <- terra::distance(
            x = fire_rast,
            y = terra::vect(study_fire_roads),
            unit = "m",
            rasterize = TRUE
          )
          
          # Cap distances at 10 km
          road_distance_raster <- terra::classify(
            x = road_distance_raster,
            rcl = rbind(c(max_dist, Inf, max_dist)),
            include.lowest = FALSE
          )
        }
        
        # Crop and mask distance raster to study fire area
        road_distance_raster <- terra::crop(
          x = road_distance_raster,
          y = terra::vect(study_fire),
          mask = TRUE
        )
        
        # Name road distance band
        names(road_distance_raster) <- "cef_road_distance"
        
        # Combine density and distance rasters
        cef_road_raster <- c(road_density_raster, road_distance_raster)
        
        # File name and path for writing raster to file
        raster_file_path <- fs::path(roads_cache, paste0(study_fire_id, ".tif"))
        
        # Write road density and distance raster to file  
        terra::writeRaster(
          x = cef_road_raster, 
          filename = raster_file_path,
          datatype = "INT2U",
          overwrite = TRUE
        )
        
        # Return tbl of raster file path
        tibble::tibble(
          fire_id = study_fire_id,
          raster_file_path = raster_file_path
        )
      },
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = 42)
    ) %>% 
    # Convert list to tibble
    purrr::list_rbind()
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Add the time this target completed as an attribute
  # NB Ensures downstream targets are invalidated whenever the CEF road
  # rasters are regenerated, even if file paths are unchanged
  attr(cef_road_raster_paths, "road_rasters_updated_at") <- Sys.time()
  
  # Return list of road raster file names
  return(cef_road_raster_paths)
  
}