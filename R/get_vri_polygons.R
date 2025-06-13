get_vri_polygons <- function(study_fire_polygons, vri_lyr_name) {
  
  # Define burn sample area to include outer buffer and all inner skips
  sample_area <- study_fire_polygons %>%
    # Delete inner holes (skips/refugia) and islands
    delete_holes() %>% 
    # Make a 1000 m buffer
    sf::st_buffer(1000)
  
  # Import VRI polygons for layer of interest in sample areas and study years
  vri_polygons <- sample_area %>% 
    tidyr::nest(.by = fire_year) %>%
    dplyr::mutate(
      sf_poly = purrr::map2(
        .x = fire_year,
        .y = data,
        .f = ~ {
          
          # VRI year to sample is year preceding a fire
          vri_year <- .x - 1
          
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
      -dplyr::starts_with("Shape"), -SE_ANNO_CAD_DATA,
      -GEOMETRY_AREA, -GEOMETRY_LEN, -OBJECTID
    )
  
  # Return
  return(vri_polygons)
}
