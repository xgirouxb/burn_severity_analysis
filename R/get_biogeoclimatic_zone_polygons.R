get_biogeoclimatic_zone_polygons <- function(study_fire_sampling_polygons) {
  
  # Get BC Biogeoclimatic Zone (BGZ) polygons that intersect study fires
  biogeoclimatic_zone_polygons <- bcdata::bcdc_query_geodata(
    uuid_bc_beogeoclimatic_zones
  ) %>% 
    bcdata::collect() %>% 
    sf::st_filter(study_fire_sampling_polygons) %>% 
    # Dissolve into single MULTIPOLYGON for each BGZ
    dplyr::group_by(ZONE_NAME) %>% 
    dplyr::summarise(
      bgz = unique(ZONE),
      bgz_name = unique(ZONE_NAME),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Add a unique numerical code
    dplyr::mutate(bgz_code = as.numeric(as.factor(bgz))) %>% 
    # Clean-up
    dplyr::select(bgz, bgz_code, bgz_name)
  
  # Return polygons
  return(biogeoclimatic_zone_polygons)
}