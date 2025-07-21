get_vri_polygons <- function(sf_aoi, year_offset = -1, vri_lyr_name) {
  
  # Import VRI polygons for layer of interest in sample areas and study years
  vri_polygons <- sf_aoi %>% 
    tidyr::nest(.by = fire_year) %>%
    dplyr::mutate(
      sf_poly = purrr::map2(
        .x = fire_year,
        .y = data,
        .f = ~ {
          
          # VRI year to sample, default is year preceding a fire
          vri_year <- .x + year_offset
          
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
          }
          
          # Query cached VRI gdb for polygons in sample area
          sf_poly_year <- dplyr::group_split(.y, fire_id) %>% 
            # Map across each fire
            purrr::map(
              .f = ~{
                sf::st_read(
                  # Get the VRI gdb in cache that matches layer name
                  dsn = stringr::str_subset(fs::dir_ls(vri_cache), vri_lyr_name),
                  # Spatial query using WKT 
                  wkt_filter = sf::st_as_text(sf::st_geometry(.x)),
                  # Silence outputs
                  quiet = TRUE
                ) %>% 
                  # Add fire_id as field
                  dplyr::mutate(fire_id = .x$fire_id)
              }
            ) %>% 
            # Bind rows for single output sf
            dplyr::bind_rows() %>% 
            # Add fire year 
            dplyr::mutate(fire_year = .x) %>% 
            # VRI has slop names for geometries, standardize to "geometry"
            dplyr::rename(geometry = !!attr(., "sf_column")) %>% 
            sf::st_set_geometry("geometry")
          
          # Return polygons
          sf_poly_year
        }
      )
    ) %>% 
    # Get list of fire polygons
    dplyr::pull(sf_poly) %>% 
    # Bind_rows
    dplyr::bind_rows() %>% 
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