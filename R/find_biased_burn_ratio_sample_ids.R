find_biased_burn_ratio_sample_ids <- function(
    burn_sample_points,
    cutblock_polygons,
    historical_fire_polygons,
    results_polygons
){
  
  # -------------------------------------------------------------------------- #
  # Step 1: Consolidated cutblocks harvest data                             ####
  
  # Identify samples with cutblocks in 1 year window before and after fire
  cutblock_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting cutblock polygons
    sf::st_join(cutblock_polygons, left = FALSE) %>% 
    # Filter within 1-year window before and after fire
    dplyr::filter(
      # Harvest starts before end of window, AND...
      cc_harvest_start_year <= (fire_year + 1) &
      # ... ends (or starts if end is NA) after start of window
      dplyr::coalesce(cc_harvest_end_year, cc_harvest_start_year) >= (fire_year - 1)
    ) %>%
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 2: BC historical fire data                                         ####
  
  # Identify samples with fires in 1-year window before and after study fire
  historical_fire_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting historical fire polygons
    sf::st_join(historical_fire_polygons, left = FALSE) %>%
    ## Filter non-study fires within 1-year window before and after fire
    dplyr::filter(
      hf_fire_year == (fire_year - 1) | 
        hf_fire_year == (fire_year + 1)
    ) %>% 
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 3: BC RESULTS harvest and fire data                                ####
  
  # Identify samples with fires in 1-year window before and after study fire
  results_fire_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting RESULTS fire polygons
    sf::st_join(
      y = dplyr::select(results_polygons, res_fire1_year, res_fire2_year),
      left = FALSE
    ) %>% 
    # Filter non-study fires within 1-year window before and after study fire
    dplyr::filter(
      # First fire recorded in RESULTS
      res_fire1_year == (fire_year - 1) | res_fire1_year == (fire_year + 1) |
      # Second fire recorded in RESULTS
      res_fire2_year == (fire_year - 1) | res_fire2_year == (fire_year + 1)
    ) %>% 
    # Get vector of ids
    dplyr::pull(id)
  
  # Identify samples with harvest in 1-year window before and after study fire
  results_harvest_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting RESULTS fire polygons
    sf::st_join(
      y = results_polygons %>% 
        dplyr::select(res_harvest_start_year, res_harvest_end_year),
      left = FALSE
    ) %>% 
    # Filter for harvest within 1-year window before and after fire
    dplyr::filter(
      # Harvest starts before end of window, AND
      res_harvest_start_year <= (fire_year + 1) &
        # ... ends after start of window
        res_harvest_end_year >= (fire_year - 1)
    ) %>%
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 4: Combine and sort unique id vector                               ####
  biased_burn_ratio_sample_ids <- c(
    cutblock_disturbed_sample_ids,
    historical_fire_disturbed_sample_ids,
    results_fire_disturbed_sample_ids,
    results_harvest_disturbed_sample_ids
  ) %>% 
    unique() %>% 
    sort()
  
  # Return
  return(biased_burn_ratio_sample_ids)
}
  