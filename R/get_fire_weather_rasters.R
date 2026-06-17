get_fire_weather_rasters <- function(
    sampling_polygons,
    n_workers = NULL
) {
  
  # -------------------------------------------------------------------------- #
  # Step 0: Setup ####
  
  # Point to CFSDS on OSF repository
  cfsds_repo <- osfr::osf_retrieve_node(id = guid_cfsds_fire_weather_repository)
  
  # Define local cache directory for CFSDS tables, create if it doesn't exist
  cfsds_cache <- fs::dir_create("data/_cache/fire_weather")
  raw_cfsds_cache <- fs::dir_create("data/_cache/fire_weather/raw")
  
  # -------------------------------------------------------------------------- #
  # Step 1: Download and unzip fire weather point data from CFSDS ####
  
  # Fetch list of files on OSF repository with names that match study years
  osf_files <- osfr::osf_ls_files(
    x = cfsds_repo,
    path = "Fire growth points",
    n_max = Inf
  ) %>% 
    dplyr::filter(stringr::str_extract(name, "\\d{4}") %in% study_years)
  
  # Build a table of zip paths
  zip_tbl <- tibble::tibble(
    year = as.integer(stringr::str_extract(osf_files$name, "\\d{4}")),
    zip_path = fs::path(raw_cfsds_cache, osf_files$name)
  )
  
  # Download zip files if not already cached
  if (any(!fs::file_exists(zip_tbl$zip_path))) {
    osfr::osf_download(osf_files, path = raw_cfsds_cache, conflicts = "overwrite")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Pull fire specific data from zip files with {arrow} ####
  
  # Create a local parquet cache for stashing {arrow} tables
  parquet_cache <- fs::dir_create("data/_cache/fire_weather/parquet")
  
  # Define {arrow} data types to avoid data reading errors 
  arrow_types <- archive::archive_read(zip_tbl$zip_path[1]) %>% 
    readr::read_csv(guess_max = 1e6, n_max = 1, show_col_types = FALSE) %>% 
    readr::spec() %>%
    purrr::pluck("cols") %>% 
    purrr::map(
      function(col) {
        switch(
          class(col)[1],
          "collector_character" = arrow::string(),
          "collector_double"    = arrow::float64(),
          arrow::string()
        )
      }
    )
  
  # Run the arrow::schema function over each element of the name arrow_type list
  # NB: the big bang operator (!!!) makes it go by element 
  arrow_schema <- rlang::exec(arrow::schema, !!!arrow_types)
  
  # Read each year's zip once, extract points for each fire, cache as parquet
  zip_tbl %>%
    dplyr::group_split(year) %>%
    purrr::walk(
      function(year) {
        # Pull year data with arrow
        arrow::read_csv_arrow(
          file = archive::archive_read(year$zip_path),
          schema = arrow_schema,
          skip = 1
        ) %>%
          # Limit query to study fires
          dplyr::filter(ID %in% sampling_polygons$fire_id) %>%
          dplyr::collect() %>%
          # Map over each fire and write to parquet cache
          dplyr::group_split(ID) %>%
          purrr::walk(
            function(fire_data) {
              
              arrow::write_parquet(
                x = fire_data,
                fs::path(parquet_cache, paste0(unique(fire_data$ID), ".parquet"))
              )
          }
        )
      }
    )
  
  # Collect garbage before parallel processing
  gc()
  
  # -------------------------------------------------------------------------- #
  # Step 3: Rasterize fire weather data for each study fire ####
  
  # Setup parallel processing if n_workers is supplied
  if(!is.null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Create list of fire weather raster file paths for each study fire
  fire_weather_raster_paths <- sampling_polygons %>% 
    dplyr::group_split(fire_id) %>%
    furrr::future_map(
      function(study_fire) {
        
        # Use native CFSDS projection for operations
        nrcan_proj <- 'EPSG:3979'
        
        # -------------------------------------------------------------------- #
        ## Step 3.1: Read the massive tables with {arrow} to save memory ####

        # Read fire weather points 
        fire_weather_sf <- arrow::read_parquet(
          fs::path(parquet_cache, paste0(study_fire$fire_id, ".parquet"))
        ) %>%
          # Remove duplicates, keep row where sprdistm has a value
          dplyr::group_by(lon, lat) %>%
          dplyr::slice_max(order_by = sprdistm, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::distinct(.keep_all = TRUE) %>% 
          # Make a unique row ID for rasterization lookup table (Section 3.2)
          dplyr::mutate(id = dplyr::row_number()) %>% 
          # Clean up, cast to sf, project
          dplyr::select(id, fire_id = ID, dob = DOB, dplyr::everything()) %>%
          sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>% 
          sf::st_transform(nrcan_proj)
 
        # Cast sf points to terra vect points
        fire_weather_pts <- terra::vect(fire_weather_sf)
        
        # -------------------------------------------------------------------- #
        ## Step 3.2: Rasterize the fire weather points ####
        
        # Cast reprojected fire polygon to terra vect
        study_fire_poly <- terra::vect(sf::st_transform(study_fire, nrcan_proj))
        
        # Create template raster at 90-m resolution 
        template_raster <- terra::rast(
          x = terra::buffer(study_fire_poly, 300),
          resolution = 90,
          vals = 1
        ) %>% 
          # Mask to buffered study fire area
          terra::crop(y = terra::buffer(study_fire_poly, 300), mask = TRUE)
        
        # Rasterize unique IDs of fire weather points 
        r_ids <- terra::rasterize(
          x = fire_weather_pts,
          y = template_raster,
          field = "id" 
        )
        
        # Fill in NA pixels with ID from the nearest pixel
        r_ids <- terra::cover(
          x = r_ids,
          y = terra::distance(x = r_ids, values = TRUE)
        ) %>% 
          # Reproject single band of IDs to study projection
          terra::project(y = study_proj, method = "near", res = 90) %>% 
          # Mask to study fire
          terra::crop(y = terra::vect(study_fire), mask = TRUE)
        
        # List of fire weather variables to be rasterized
        var_list <- fire_weather_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::select(-id, -fire_id, -year) %>%
          names()
        
        # Build a lookup table of row ID → variable values
        val_tbl <- fire_weather_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::select(id, dplyr::all_of(var_list))
        
        # Cast value lookup table to matrix for speed
        val_mat <- as.matrix(val_tbl[, var_list])
        
        # Extract vector of rasterized ids, one id per pixel
        id_vals <- terra::values(r_ids, mat = FALSE)
        
        # Matching each pixel's id to the corresponding row in val_mat
        row_idx <- base::match(id_vals, val_tbl$id)
        
        # Sanity check: all rasterized pixel IDs should match a row in the fire
        #               weather lookup table — if not, rasterizing ids 
        #               produced unexpected values.
        if (anyNA(row_idx[!is.na(id_vals)])) {
          stop(
            "⚠️ Fire weather raster contains pixel IDs with no match in the lookup table!\n",
            "   fire_id: ", study_fire$fire_id, "\n",
            "   Unmatched pixel IDs: ", paste(unique(id_vals[is.na(row_idx) & !is.na(id_vals)]), collapse = ", ")
          )
        }

        # Look up each pixel's weather values by row index — produces an 
        # (n_pixels × n_vars) matrix in raster pixel order
        out_mat <- val_mat[row_idx, , drop = FALSE]
        
        # Create an empty multi-band raster (one band per weather variable)
        # Inherits extent, resolution, and CRS from the reprojected ID raster
        fire_weather_raster <- terra::rast(x = r_ids, nlyrs = length(var_list))
        
        # Populate raster bands from the output matrix and name them
        terra::values(fire_weather_raster) <- out_mat
        names(fire_weather_raster) <- var_list
        
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
      },
      # Pass seed to {future} to avoid complaints, schedule one task at a time
      .options = furrr::furrr_options(seed = TRUE, scheduling = Inf)
    ) %>% 
    # Combine
    dplyr::bind_rows()
  
  # Close parallel processing if n_workers is supplied
  if(!is.null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # Return list of fire weather raster file paths
  return(fire_weather_raster_paths)
}
