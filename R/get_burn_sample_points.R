get_bc_burn_sample_points <- function(path_fire_data){
  
  # Read in the raw data
  raw_data <- readr::read_csv(path_fire_data, show_col_types = FALSE) %>% 
    # Clean up 
    janitor::clean_names() %>% 
    # Select columns of interest for downstream sampling
    dplyr::select(id = oid, fire_id, fire_year, x_3005, y_3005)
  
  # Convert to simple feature
  burn_sample_points <- raw_data %>% 
    sf::st_as_sf(coords = c("x_3005", "y_3005"), crs = "EPSG:3005")
  
  # Return
  return(burn_sample_points)

}