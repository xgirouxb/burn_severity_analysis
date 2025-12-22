get_historical_fire_polygons <- function(study_fire_sampling_polygons) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Import and wrangle NBAC archive                                 ####
  
  # Get NBAC archive URLs for all years between 1973 (start of archive)
  # and the year after the last study year
  list_nbac_subdir <- purrr::map(
    1973:(max(study_years) + 1),
    function(year) { 
      get_url_list(
        url = url_nbac_archive,
        match_string = paste0("NBAC_", year)
      ) 
    }
  )

  # Import NBAC fire polygons for British Columbia
  hist_nbac_fire_polygons <- purrr::map(
    list_nbac_subdir,
    function(nbac_subdir) { get_sf_from_source(sf_source = nbac_subdir) }
  ) %>% 
    # Bind rows
    dplyr::bind_rows() %>% 
    # Filter for fires in British Columbia
    dplyr::filter(ADMIN_AREA == "BC") %>% 
    # Add some unique historical fire attributes 
    dplyr::mutate(
      hf_fire_src = "nbac",
      hf_fire_id = paste0(YEAR, "_", NFIREID),
      hf_fire_year = YEAR,
    ) %>% 
    # Spatial inner join to filter historical fires that intersect study fires
    sf::st_join(
      # Add 10-km buffer for downstream neighbourhood variables
      y = sf::st_buffer(study_fire_sampling_polygons, dist = 10000),
      left = FALSE
    ) %>% 
    # Retain unique historical fires (some fires may intersect > 1 study fire)
    dplyr::distinct(hf_fire_id, .keep_all = TRUE) %>% 
    # Dissolve into single MULTIPOLYGON for each historical fire
    dplyr::group_by(hf_fire_src, hf_fire_id, hf_fire_year) %>%
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop") %>%
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
      hf_fire_src = "bc",
      hf_fire_id = stringr::str_extract(id, "\\d+$"),
      hf_fire_year = FIRE_YEAR,
    ) %>%
    # Spatial inner join to filter historical fires that intersect study fires
    sf::st_join(
      # Add 10-km buffer for downstream neighbourhood variables
      y = sf::st_buffer(study_fire_sampling_polygons, dist = 10000),
      left = FALSE
    ) %>% 
    # Retain unique historical fires (some fires may intersect > 1 study fire)
    dplyr::distinct(hf_fire_id, .keep_all = TRUE) %>%  
    # Dissolve into single MULTIPOLYGON for each historical fire
    dplyr::group_by(hf_fire_src, hf_fire_id, hf_fire_year) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
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