find_biased_burn_ratio_sample_ids <- function(
    burn_sample_points,
    cutblock_polygons,
    historical_fire_polygons,
    results_polygons,
    forestry_disturbance_rasters
){
  
  # -------------------------------------------------------------------------- #
  # Step 1: Consolidated cutblocks harvest polygon data ####
  
  # Identify samples with cutblocks in 1 year window before and after fire
  cc_disturbed_sample_ids <- burn_sample_points %>% 
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
  # Step 2: BC historical fire polygon data ####
  
  # Identify samples with fires in 1-year window before and after study fire
 hf_disturbed_sample_ids <- burn_sample_points %>% 
    # Join intersecting historical fire polygons
    sf::st_join(historical_fire_polygons, left = FALSE) %>%
    # Filter non-study fires within 1-year window before and after fire
    dplyr::filter(
      hf_fire_year == (fire_year - 1) | 
        hf_fire_year == (fire_year + 1)
    ) %>% 
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 3: BC RESULTS harvest and fire polygon data ####
  
  # Identify RESULTS harvest/fires in 1-year window before and after study fire
  res_harvest_fire_disturbed_sample_ids <- burn_sample_points %>% 
    # Nest samples by fire id
    # (avoid sampling if point intersects mismatched overlapping fires)
    dplyr::group_split(fire_id) %>% 
    # Spatially join RESULTS attributes to sample points within each study fire
    purrr::map(
      function(study_fire_samples) {
        # Only sample RESULTS polygons with matching fire ids
        sf::st_join(
          x = study_fire_samples,
          y = results_polygons %>% 
            dplyr::filter(fire_id == unique(study_fire_samples$fire_id)) %>%
            # Select variables of interest
            dplyr::select(
              res_harvest_start_year, res_harvest_end_year,
              res_fire1_year, res_fire2_year
            ),
          # Inner join, remove samples that don't intersect RESULTS polygons
          left = FALSE
        ) %>% 
          # Drop geometries
          sf::st_drop_geometry()
      }
    ) %>% 
    # Coerce list to tibble
    dplyr::bind_rows() %>% 
    # Filter for harvest/fires within 1-year window before and after fire
    dplyr::filter(
      # Harvest starts before end of window, AND
      res_harvest_start_year <= (fire_year + 1) &
        # ... ends after start of window
        res_harvest_end_year >= (fire_year - 1) |
        # OR first fire in 1-year window
        res_fire1_year == (fire_year - 1) | res_fire1_year == (fire_year + 1) |
        # OR second fire 1-year window
        res_fire2_year == (fire_year - 1) | res_fire2_year == (fire_year + 1)
    ) %>%
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 4: Forestry disturbance harvest and planting raster data ####
  
  # N.B.: These data are derived from above polygon datasets, but rasterization
  #       leads to edge errors, additional filtering with rasters is required       
  
  # Identify harvest/plantings in 1-year window before and after study fire
  fd_harvest_planting_disturbed_sample_ids <- burn_sample_points %>% 
    # Nest by study fire and year
    dplyr::group_nest(fire_id, fire_year) %>% 
    # Join table of forest management raster file names
    dplyr::left_join(forestry_disturbance_rasters, by = "fire_id") %>% 
    # Sample rasters
    dplyr::mutate(
      forestry_disturbance_samples = purrr::map2(
        raster_file_path,
        data,
        ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
          tibble::as_tibble() %>% 
          dplyr::mutate(id = .y$id, .before = dplyr::everything()) %>% 
          dplyr::select(-ID) 
      )
    ) %>% 
    # Unnest and clean-up
    dplyr::select(fire_id, fire_year, forestry_disturbance_samples) %>%
    tidyr::unnest(cols = c(forestry_disturbance_samples)) %>% 
    # Filter harvest or planting within 1-year window before and after study fire
    dplyr::filter(
      # Harvest in 1-year window
      harvest_year == (fire_year - 1) |
        harvest_year == (fire_year + 1) |
        # Planting in 1-year window
        res_planting_year == (fire_year - 1) |
        res_planting_year == (fire_year + 1)
    ) %>% 
    # Get vector of ids
    dplyr::pull(id)
  
  # -------------------------------------------------------------------------- #
  # Step 4: Combine and sort unique id vector                               ####
  biased_burn_ratio_sample_ids <- c(
    cc_disturbed_sample_ids,
    hf_disturbed_sample_ids,
    res_harvest_fire_disturbed_sample_ids,
    fd_harvest_planting_disturbed_sample_ids
  ) %>% 
    unique() %>% 
    sort()
  
  # Return
  return(biased_burn_ratio_sample_ids)
}
  