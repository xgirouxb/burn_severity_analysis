get_study_fire_polygons <- function(study_area, study_years) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Import and wrangle fire polygons from NBAC archive              ####
  
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
  nbac_fire_polygons <- dplyr::bind_rows(list_nbac_fire_polygons) %>%
    # Make a unique fire_id
    dplyr::mutate(fire_id = paste0(YEAR, "_", NFIREID)) %>%
    # Remove prescribed burns ("2017_2418)
    dplyr::filter(PRESCRIBED != "true" | is.na(PRESCRIBED)) %>%
    # Remove fires not within BC ("2018_35")
    sf::st_filter(study_area, .predicate = sf::st_within) %>%
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
    # Fix topology errors
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")
  
  # -------------------------------------------------------------------------- #
  # Step 2: Remove fires without samples in MGH dataset                     ####
  #         NB This step may eventually be removed if all sampling is 
  #            integrated into this {targets} pipeline
  
  # Get and parse fire_ids in MGH
  mgh_fire_ids <- readr::read_csv(path_fire_data, show_col_types = FALSE) %>%
    # Split string, remove leading 0s, then combine
    dplyr::distinct(fireID) %>% 
    tidyr::separate(fireID, into = c("year", "id"), sep = "_") %>%
    dplyr::mutate(id = as.integer(id)) %>%  
    tidyr::unite("fire_id", year, id, sep = "_") %>% 
    # Grab vector
    dplyr::pull(fire_id)
  
  # Remove fires that don't have corresponding samples in MGH dataset
  # "2017_1002", "2018_1291", and "2018_194"
  study_fire_polygons <- nbac_fire_polygons %>% 
    dplyr::filter(!(fire_id %in% setdiff(.$fire_id, mgh_fire_ids)))
  
  # Return 
  return(study_fire_polygons)
}
