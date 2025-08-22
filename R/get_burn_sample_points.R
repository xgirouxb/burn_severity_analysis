get_bc_burn_sample_points <- function(
    path_fire_data, study_fire_sampling_polygons
){
  
  # Read in the raw MGH fire data
  raw_data <- readr::read_csv(path_fire_data, show_col_types = FALSE) %>% 
    # Clean up 
    janitor::clean_names() %>% 
    # Parse old and new fire IDs (MGH has trailing 0s that are removed in NBAC)
    dplyr::mutate(
      old_fire_id = fire_id,
      fire_id = paste0(
        # Keep original year suffix
        fire_year, "_",
        # Remove leading 0s from NBAC fire id
        readr::parse_number(stringr::str_replace(old_fire_id, ".*_", ""))
      )
    ) %>% 
    # Select columns of interest for downstream sampling
    dplyr::select(id = oid, fire_id, old_fire_id, fire_year, x_3005, y_3005) %>%
    # Purge duplicated sample ids (due to error in DOB2 values)
    dplyr::add_count(id) %>%
    dplyr::filter(n == 1) %>%
    dplyr::select(-n) %>% 
    # Remove samples without fire_ids included in study fire sampling polygons
    dplyr::filter(fire_id %in% unique(study_fire_sampling_polygons$fire_id))
  
  # Convert to simple feature
  burn_sample_points <- raw_data %>% 
    sf::st_as_sf(coords = c("x_3005", "y_3005"), crs = "EPSG:3005") %>% 
    # Remove sampling points that do not fall within fire sampling polygons
    sf::st_filter(study_fire_sampling_polygons)
  
  # Return
  return(burn_sample_points)

}