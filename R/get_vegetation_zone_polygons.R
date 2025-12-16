get_vegetation_zone_polygons <- function(study_fire_sampling_polygons) {
  
  # Get Canadian Vegetation Zone (CVZ) polygons that intersect study area
  vegetation_zone_polygons <- get_sf_from_source(
    sf_source = url_nrcan_vegetation_zones,
    sf_aoi = study_fire_sampling_polygons
  ) %>% 
    # Group by Level 1 vegetation zone
    dplyr::group_by(id_1) %>% 
    # Dissolve into single MULTIPOLYGON for each vegetation zone
    dplyr::summarize(
      cvz = dplyr::first(id_1),
      cvz_name = dplyr::first(level_1),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Remove id_1
    dplyr::select(-id_1) %>% 
    # Fix topology errors
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")
  
  # Return polygons
  return(vegetation_zone_polygons)
}