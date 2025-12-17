get_vri_polygons <- function(sf_aoi, year_offset = -1, vri_lyr_name) {
  
  # Import VRI polygons for layer of interest in sample areas and study years
  vri_polygons <- purrr::map(
    study_years,
    function(fire_year) {
      
      # VRI year to sample, default is year preceding a fire
      vri_year <- fire_year + year_offset
      
      # Define cache directory for VRI archive, create if it doesn't exist
      vri_cache <- fs::path("data/_cache/vri", vri_year)
      if (!fs::dir_exists(vri_cache)) { fs::dir_create(vri_cache) }
      
      # If VRI cache contains no gdb matching vri_lyr_name...
      if (!any(stringr::str_detect(fs::dir_ls(vri_cache), vri_lyr_name))) {
        
        # Get VRI historical archive subdirectories for VRI year of interest
        vri_year_subdir <- get_url_list(
          url = url_vri_archive,
          match_string = paste0(vri_year)
        )
        
        # Get URL for VRI gdb of interest
        url_vri_year_gdb <- get_url_list(
          url = vri_year_subdir,
          match_string = vri_lyr_name
        )
        
        # Download VRI zipped geodatabase to cache and unzip
        get_archive_from_url(
          archive_url = url_vri_year_gdb,
          output_dir = vri_cache
        )
        
        # Rename the unzipped VRI geodatabase with consistent naming scheme
        # (VRI gdb names are sometimes upper, lower, or sentence case)
        
        # Fetch list of gdbs currently in cache
        cached_gdbs <- fs::dir_ls(vri_cache, type = "directory")
        
        # Fetch messy VRI naming scheme that matches supplied layer name
        messy_gdb_name <- cached_gdbs[
          stringr::str_detect(
            string = stringr::str_to_lower(fs::path_file(cached_gdbs)),
            pattern = stringr::str_to_lower(vri_lyr_name)
          )
        ]
        
        # Sanity check
        if (length(messy_gdb_name) > 1) { 
          stop("> 1 matching gdb matches `vri_lyr_name`.") 
        }
        
        # Clean gdb naming scheme
        clean_gdb_name <- fs::path(
          vri_cache,
          paste0(vri_lyr_name, "_", vri_year, ".gdb")
        )
        
        # Rename messy VRI gdb with standard naming scheme
        file.rename(messy_gdb_name, clean_gdb_name)
        
        # Sanity check
        if (!fs::dir_exists(clean_gdb_name)) { stop("Bad VRI gdb names.") }
        if (fs::dir_exists(messy_gdb_name)) { stop("Bad VRI gdb names.") }
      }
      
      # Query cached VRI gdb for polygons in sample area
      sf_poly_year <- dplyr::group_split(sf_aoi, fire_id) %>% 
        # Map across each fire
        purrr::map(
          function(study_fire) {
            sf::st_read(
              # Get the VRI gdb in cache that matches layer name
              dsn = stringr::str_subset(fs::dir_ls(vri_cache), vri_lyr_name),
              # Spatial query using WKT 
              wkt_filter = sf::st_as_text(sf::st_geometry(study_fire)),
              # Silence outputs
              quiet = TRUE,
              # Output tibble instead of data.frame
              as_tibble = TRUE
            ) %>% 
              # Add fire_id as field
              dplyr::mutate(fire_id = study_fire$fire_id)
          }
        ) %>% 
        # Bind rows for single output sf
        dplyr::bind_rows() %>% 
        # Add fire year 
        dplyr::mutate(fire_year = fire_year) %>% 
        # VRI has slop names for geometries, standardize to "geometry"
        dplyr::rename(geometry = !!attr(., "sf_column")) %>% 
        sf::st_set_geometry("geometry")
      
      # Return polygons
      return(sf_poly_year)
    }
  ) %>% 
    # Convert list to sf
    dplyr::bind_rows() %>% 
    # Cast dttm columns to UTC
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ inherits(.x, "POSIXt")),
        ~ lubridate::with_tz(.x, tzone = "UTC")
      )
    ) %>% 
    # Clean up ESRI slop
    dplyr::select(
      -dplyr::starts_with("Shape"), 
      -dplyr::starts_with("SE_ANNO_CAD"),
      -dplyr::starts_with("GEOMETRY_"), 
      -dplyr::starts_with("OBJECTID")
    ) %>% 
    # Cast columns names to lowercase to match metadata (except for geometry)
    dplyr::rename_with(stringr::str_to_lower, .cols = -geometry)
  
  # Return
  return(vri_polygons)
}