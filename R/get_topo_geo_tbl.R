get_topo_geo_tbl <- function(
    path_fire_data,
    burn_sample_points
){
  
  # Read in the raw MGH fire data
  topo_geo_tbl <- readr::read_csv(path_fire_data, show_col_types = FALSE) %>%
    # Clean up
    janitor::clean_names() %>%
    # Select topogeo variables for downstream sampling
    dplyr::select(
      # Sample id
      id = oid,
      # Heat load index
      hli, 
      # Elevation
      dem,
      # Biogeoclimatic zones of BC
      bgz
    )
  # Return
  return(topo_geo_tbl)
}