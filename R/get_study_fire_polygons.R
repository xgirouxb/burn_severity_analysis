get_study_fire_polygons <- function(study_area, study_years) {
  
  # Get NBAC archive URLs for study years
  list_nbac_subdir <- purrr::map(
      .x = study_years,
      .f = ~{get_url_list(url = url_nbac_archive, match_string = paste0(.x))}
  )
  
  # Get NBAC fire polygons that intersect with study area
  list_nbac_fire_polygons <- purrr::map(
    .x = list_nbac_subdir,
    .f = ~{get_sf_from_source(sf_source = .x, sf_aoi = study_area)}
  )
  
  # Bind list to single sf obj of fire polygons
  nbac_fire_polygons <- dplyr::bind_rows(list_nbac_fire_polygons) %>%
    # Retain only fires > 1000 hectares
    dplyr::filter(POLY_HA > 1000) %>% 
    # Make a unique fire_id
    dplyr::mutate(fire_id = paste0(YEAR, "_", NFIREID)) %>% 
    # Remove 2 fires that are not in MGH samples
    dplyr::filter(!(fire_id %in% c("2017_1002", "2018_1291")))
  
  # Return 
  return(nbac_fire_polygons)
}