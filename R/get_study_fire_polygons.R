get_study_fire_polygons <- function(study_area, study_years) {
  
  # Get NBAC archive URLs for study years
  list_nbac_subdir <- purrr::map(
    study_years,
    function(study_year) { 
      get_url_list(
        url = url_nbac_archive,
        match_string = paste0("NBAC_", study_year)) 
    }
  )
  
  # Get NBAC fire polygons that intersect with study area
  list_nbac_fire_polygons <- purrr::map(
    list_nbac_subdir,
    function(nbac_subdir) { 
      get_sf_from_source(
        sf_source = nbac_subdir,
        sf_aoi = study_area
      ) 
    }
  )
  
  # Bind list to single sf obj of fire polygons
  study_fire_polygons <- dplyr::bind_rows(list_nbac_fire_polygons) %>%
    # Make a unique fire_id
    dplyr::mutate(fire_id = paste0(YEAR, "_", NFIREID)) %>%
    # Remove prescribed burns (e.g., "2017_2418")
    dplyr::filter(PRESCRIBED != "true" | is.na(PRESCRIBED)) %>%
    # Remove fires not within BC (e.g., "2018_35")
    sf::st_filter(
      # Use a 3-km inner buffer so BC datasets have coverage of fire buffer area
      y = sf::st_buffer(study_area, -3000),
      .predicate = sf::st_within
    ) %>%
    # Dissolve into single MULTIPOLYGON for each fire
    dplyr::group_by(fire_id) %>%
    dplyr::summarize(
      fire_id = unique(fire_id),
      fire_year = unique(YEAR),
      fire_start_date = unique(HS_SDATE),
      fire_end_date = unique(HS_EDATE),
      fire_burn_area_ha = sum(POLY_HA),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Retain only fires > 1000 hectares
    dplyr::filter(fire_burn_area_ha > 1000) %>%
    # Fix topology errors, if any
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")
  
  # Return 
  return(study_fire_polygons)
}
