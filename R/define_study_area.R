define_study_area <- function() {
  
  # Import StatCan admin boundaries
  statcan_admin_bounds <- get_sf_from_source(
    sf_source = url_statcan_admin_bounds
  )
  
  # Build BC provincial bounds for study area
  study_area <- statcan_admin_bounds %>%
    # Filter British Columbia
    dplyr::filter(PRUID == 59) %>% 
    # Cast MULTIPOLYGON to POLYGON
    sf::st_cast("POLYGON", warn = FALSE) %>%
    # Fix topology errors
    sf::st_make_valid() %>%
    # Combine into single layer
    sf::st_union() 
  
  # Return
  return(study_area)
}