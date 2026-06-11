get_fire_weather_rasters <- function(
    study_fire_sampling_polygons
) {
  
  # -------------------------------------------------------------------------- #
  # Step 0: Setup ####
  
  # Point to CFSDS on OSF repository
  cfsds_repo <- osfr::osf_retrieve_node(id = guid_cfsds_fire_weather_repository)
  
  # Define local cache directory for CFSDS csv's, create if it doesn't exist
  cfsds_cache <- fs::path("data/_cache/fire_weather")
  fs::dir_create(cfsds_cache, recurse = TRUE)
  raw_cfsds_cache <- fs::path("data/_cache/fire_weather/raw")
  fs::dir_create(raw_cfsds_cache, recurse = TRUE)
  
  # -------------------------------------------------------------------------- #
  # Step 1: Download and unzip fire weather point data from CFSDS ####
  
  # Fetch list of files on OSF repository with names that match study years
  osf_files <- osfr::osf_ls_files(
    x = cfsds_repo,
    path = "Fire growth points",
    n_max = Inf
  ) %>% 
    dplyr::filter(stringr::str_extract(name, "\\d{4}") %in% study_years)
  
  # Build a table of csv paths
  csv_tbl <- tibble::tibble(
    year = as.integer(stringr::str_extract(osf_files$name, "\\d{4}")),
    csv_path = fs::path(
      raw_cfsds_cache,
      stringr::str_replace(osf_files$name, "\\.zip$", ".csv")
    )
  )
  
  # If unzipped csv files are not all locally cached
  if (any(!fs::file_exists(csv_tbl$csv_path))) {
    # Download and unzip fire weather CSVs
    osf_files %>% 
      osfr::osf_download(path = raw_cfsds_cache, conflicts = "overwrite") %>% 
      dplyr::pull(local_path) %>% 
      purrr::walk(~archive::archive_extract(.x, dir = raw_cfsds_cache))
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Setup parameters for reading big CSV files with {arrow} ####
  
  # Define {arrow} data types to avoid data reading errors 
  arrow_types <- csv_tbl$csv_path[1] %>% 
    readr::read_csv(guess_max = 1e6, n_max = 1, show_col_types = FALSE) %>% 
    readr::spec() %>%
    purrr::pluck("cols") %>% 
    purrr::map(
      function(col) {
        switch(
          class(col)[1],
          "collector_character"   = arrow::string(),
          "collector_double"   = arrow::float64(),
          arrow::string() # Default fallback
        )
      }
    )
  
  # Run the arrow::schema function over each element of the name arrow_type list
  # NB: the big bang operator (!!!) makes it go by element 
  arrow_schema <- rlang::exec(arrow::schema, !!!arrow_types)
  
  # -------------------------------------------------------------------------- #
  # Step 3: Rasterize fire weather data for each study fire ####
  
  # Create list of fire weather raster file paths for each study fire
  fire_weather_raster_paths <- study_fire_sampling_polygons %>% 
    dplyr::group_split(fire_id) %>%
    purrr::map(
      function(study_fire) {
        
        # Use NRCAN projection for native CFSDS dataset operations
        nrcan_proj <- 'EPSG:3979'
        
        # -------------------------------------------------------------------- #
        ## Step 3.1: Read the massive CSVs with {arrow} to save memory ####
        
        # Read csv 
        fire_weather_sf <- arrow::open_csv_dataset(
          # Provide path to csv for corresponding study year
          sources = csv_tbl %>% 
            dplyr::filter(year == study_fire$fire_year) %>% 
            dplyr::pull(csv_path),
          # Use custom arrow schema to avoid data type errors
          schema = arrow_schema,
          skip = 1
        ) %>% 
          # Collect fire weather points for study fire
          dplyr::filter(ID == study_fire$fire_id) %>%
          dplyr::collect() %>% 
          # Make a unique row ID and clean up names
          dplyr::mutate(id = dplyr::row_number()) %>% 
          dplyr::select(id, fire_id = ID, dob = DOB, dplyr::everything()) %>% 
          # Cast to sf
          sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>% 
          sf::st_transform(nrcan_proj)
        
        # -------------------------------------------------------------------- #
        ## Step 3.2: Rasterize the fire weather points in native projection ####
        
        # Reproject study fire to CFSDS native projection
        study_fire_lambert <- study_fire %>% 
          sf::st_transform(nrcan_proj) %>% 
          terra::vect()
        
        # Create template raster at 30-m resolution 
        template_raster <- terra::rast(
          x = study_fire_lambert,
          resolution = 90,
          vals = 1
        ) %>% 
          # Mask to buffered study fire area
          terra::crop(y = study_fire_lambert, mask = TRUE)
        
        # Rasterize unique IDs of fire weather points 
        r_ids <- terra::rasterize(
          x = terra::vect(fire_weather_sf),
          y = template_raster,
          field = "id" 
        ) %>% 
          # Fill in NA pixels with ID of nearest pixel
          terra::cover(x = ., y = terra::distance(x = ., values = TRUE))
        
        # List of variables to add as raster bands
        var_list <- sf::st_drop_geometry(fire_weather_sf) %>% 
          dplyr::select(-id, -fire_id, -year) %>% 
          names()
        
        # Value lookup table
        val_tbl <- fire_weather_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::select(id, dplyr::all_of(var_list))
        
        # Map over each variable and add it as a band to the raster
        fire_weather_bands <- purrr::map(
          var_list,
          function(var) {
            # Substitute ID values for their corresponding variable values
            r <- terra::subst(x = r_ids, from = val_tbl$id, to = val_tbl[[var]]) 
            
            # Assign the attribute name
            names(r) <- var
            
            # Return one-banded raster
            return(r)
          }
        )
        
        # Combine bands into one raster, project, and crop
        fire_weather_raster <- terra::rast(fire_weather_bands) %>% 
          # Reproject to study projection
          terra::project(
            y = study_proj,
            method = "near",
            res = 90
          ) %>% 
          # Mask to study fire area
          terra::crop(y = terra::vect(study_fire), mask = TRUE)
        
        # -------------------------------------------------------------------- #
        ## Step 3.3: Write fire weather rasters to local cache ####
        
        # File path for writing raster to file
        raster_file_path <- fs::path(
          cfsds_cache,
          paste0(study_fire$fire_id, ".tif")
        )
        
        # Write fire weather raster to file
        terra::writeRaster(
          x = fire_weather_raster,
          filename = raster_file_path,
          datatype = "FLT4S",
          overwrite = TRUE
        )
        
        # Return tbl of file path
        tibble::tibble(
          fire_id = study_fire$fire_id,
          raster_file_path = raster_file_path
        )  
      }
    ) %>% 
    # Combine
    dplyr::bind_rows()
  
  # Return list of fire weather raster file paths
  return(fire_weather_raster_paths)
}
