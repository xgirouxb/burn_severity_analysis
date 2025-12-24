get_biogeoclimatic_zone_polygons <- function(study_fire_sampling_polygons) {
  
  # Get BC Biogeoclimatic Ecological Classification (BEC) polygons
  # that intersect study fires
  biogeoclimatic_zone_polygons <- bcdata::bcdc_query_geodata(
    uuid_bc_beogeoclimatic_zones
  ) %>% 
    bcdata::collect() %>% 
    sf::st_filter(study_fire_sampling_polygons) %>% 
    # Dissolve into single MULTIPOLYGON for each BEC
    dplyr::group_by(ZONE_NAME) %>% 
    dplyr::summarise(
      bec_short = unique(ZONE),
      bec_name = unique(ZONE_NAME),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Add a unique numerical code
    dplyr::mutate(bec = as.numeric(as.factor(bec_short))) %>% 
    # Clean-up
    dplyr::select(bec, bec_short, bec_name)
  
  # Return polygons
  return(biogeoclimatic_zone_polygons)
}