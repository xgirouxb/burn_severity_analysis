get_historical_fire_polygons <- function(study_fire_sampling_polygons) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Import and wrangle NBAC archive                                 ####
  
  # Get NBAC archive URLs for all years between 1973 (start of archive)
  # and the year preceding the most recent study year
  list_nbac_subdir <- purrr::map(
    .x = 1973:(max(study_fire_sampling_polygons$fire_year)-1),
    .f = ~{ 
      get_url_list(url = url_nbac_archive, match_string = paste0("NBAC_", .x)) 
    }
  )
  
  # Import NBAC fire polygons for British Columbia
  hist_nbac_fire_polygons <- purrr::map(
    .x = list_nbac_subdir,
    .f = ~{ get_sf_from_source(sf_source = .x) }
  ) %>% 
    # Bind rows
    dplyr::bind_rows() %>% 
    # Filter for fires in British Columbia
    dplyr::filter(ADMIN_AREA == "BC") %>% 
    # Add some unique historical fire attributes 
    dplyr::mutate(
      hist_fire_src = "nbac",
      hist_fire_id = paste0(YEAR, "_", NFIREID),
      hist_fire_year = YEAR,
    ) %>% 
    # Spatial inner join (many to many) fires that intersect study fire polygons
    sf::st_join(y = study_fire_sampling_polygons, left = FALSE) %>% 
    # Filter for historical fires that predate study fires
    dplyr::filter(hist_fire_year < fire_year) %>% 
    # Dissolve into single MULTIPOLYGON for each historical fire
    dplyr::group_by(
      # Study fire attributes
      fire_id, fire_year,
      # Historical fire attributes
      hist_fire_src, hist_fire_id, hist_fire_year
    ) %>%
    dplyr::summarize(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Fix topology errors
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")
  
  # -------------------------------------------------------------------------- #
  # Step 2: Import and wrangle BC historical fire archive                   ####
  
  # Import BC historical fire archive polygons                                    
  hist_bc_fire_polygons <- bcdata::bcdc_query_geodata(
    uuid_bc_historical_fires
  ) %>% 
    # Limit query to years prior to NBAC archive 
    dplyr::filter(FIRE_YEAR < 1973) %>%
    # Fetch from BC data archives
    bcdata::collect() %>%
    # Add some unique historical fire attributes 
    dplyr::mutate(
      hist_fire_src = "bc",
      hist_fire_id = stringr::str_extract(id, "\\d+$"),
      hist_fire_year = FIRE_YEAR,
    ) %>%
    # Spatial inner join (many to many) fires that intersect study fire polygons
    sf::st_join(y = study_fire_sampling_polygons, left = FALSE) %>% 
    # Dissolve into single MULTIPOLYGON for each historical fire
    dplyr::group_by(
      # Study fire attributes
      fire_id, fire_year,
      # Historical fire attributes
      hist_fire_src, hist_fire_id, hist_fire_year
    ) %>%
    dplyr::summarize(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Fix topology errors
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")

  # -------------------------------------------------------------------------- #
  # Step 3: Combine NBAC and BC historical fire archives                    ####
  
  # Combine historical fire datasets
  historical_fire_polygons <- dplyr::bind_rows(
    # 1973 to study fire year (2017, 2018, or 2021)
    hist_nbac_fire_polygons,
    # 1919 to 1972 --> BC historical fires (less precise than NBAC)
    hist_bc_fire_polygons
  )
  
  # Return
  return(historical_fire_polygons)
}