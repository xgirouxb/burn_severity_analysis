find_biased_burn_ratio_sample_ids <- function(
    burn_sample_points,
    cutblock_polygons,
    historical_fire_polygons
){
  
  # Identify burn samples with cutblocks in 1 year window before and after fire
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
  
  # Identify burn samples with fires in 1-year window before and after study fire
  historical_fire_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting historical fire polygons
    sf::st_join(historical_fire_polygons, left = FALSE) %>%
    # Filter for historical fires that aren't the study fire
    dplyr::filter(fire_id != hf_fire_id) %>% 
    # Filter within 1-year window before and after fire
    dplyr::filter(
      hf_fire_year <= (fire_year + 1) & 
        hf_fire_year >= (fire_year - 1)
    ) %>% 
    # Get vector of ids
    dplyr::pull(id)
  
  # Combine and sort unique id vector
  biased_burn_ratio_sample_ids <- c(
    cutblock_disturbed_sample_ids,
    historical_fire_disturbed_sample_ids
  ) %>% 
    unique() %>% 
    sort()
  
  # Return
  return(biased_burn_ratio_sample_ids)
}
  