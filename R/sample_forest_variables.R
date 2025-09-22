sample_forest_variables <- function(
    burn_sample_points,
    biased_burn_ratio_sample_ids,
    vri_polygons,
    land_cover_class_tbl,
    cutblock_polygons,
    historical_fire_polygons,
    canlad_disturbance_rasters,
    precanlad_disturbance_rasters,
    bc_results_tbl,
    topo_geo_tbl
) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Sample forest vegetation: VRI archive + land cover images       ####
  
  # Sample vegetation attributes (VRI and Land cover)
  sampled_vegetation <- burn_sample_points %>%
    # Nest samples by fire id and year
    # (as some VRI polygons might intersect points with mismatched years)
    dplyr::group_split(fire_id, fire_year) %>%
    # Spatially join VRI attributes to sample points within each study fire
    purrr::map(
      ~ sf::st_join(
        x = .x,
        # Only sample VRI polygons with matching fire ids
        y = vri_polygons %>%
          dplyr::filter(fire_id == unique(.x$fire_id)) %>%
          dplyr::select(-fire_id, -fire_year),
        # Inner join in case some samples don't have 
        left = FALSE
      )
    ) %>%
    # Convert list to sf
    dplyr::bind_rows() %>%
    # If >1 VRI polygon intersects point, retain the most recent one
    dplyr::group_by(id) %>% 
    dplyr::slice_max(
      order_by = vri_reference_year,
      n = 1,
      # Pick only one if there are ties
      with_ties = FALSE
    ) %>% 
    dplyr::ungroup() %>% 
    # Join land cover classes and composition
    dplyr::left_join(
      land_cover_class_tbl,
      by = c("id", "fire_id", "fire_year")
    ) %>% 
    # Clean up
    dplyr::select(-vri_feature_id)
  
  # -------------------------------------------------------------------------- #
  # Step 2: Sample forest disturbances                                      ####
  
  ## 2.1 Sample cutblock polygon attributes ####
  sampled_cutblocks <- burn_sample_points %>%
    # Spatial inner join cutblocks that intersect sample points
    sf::st_join(
      y = cutblock_polygons,
      left = FALSE
    ) %>%
    # Filter cutblocks that predate study fires
    dplyr::filter(cc_harvest_start_year < fire_year) %>%
    # Drop geometries prior to summarise
    sf::st_drop_geometry() %>%
    # If >1 cutblock polygon intersects point, retain the most recent one
    dplyr::group_by(id) %>%
    dplyr::slice_max(
      order_by = cc_harvest_start_year,
      n = 1,
      # Pick only one if there are ties
      with_ties = FALSE
    ) %>% 
    # Compute years since most recent harvest
    dplyr::mutate(
      cc_years_since_harvest = unique(fire_year) - cc_harvest_start_year
    ) %>%
    # Clean up
    dplyr::select(
      id, cc_harvest_start_year, cc_harvest_end_year,
      cc_years_since_harvest, cc_percent_clearcut
    )
  
  ## 2.2 Sample historical fire polygon attributes ####
  sampled_historical_fires <- burn_sample_points %>%
    # Spatial inner join fires that intersect sample points
    sf::st_join(y = historical_fire_polygons, left = FALSE) %>%
    # Filter for historical fires that predate study fires
    dplyr::filter(hf_fire_year < fire_year) %>%
    # Drop geometries prior to summarise
    sf::st_drop_geometry() %>%
    # Compute years since most recent historical fire
    dplyr::group_by(id, fire_id, fire_year) %>%
    dplyr::summarise(
      hf_fire_year = max(hf_fire_year),
      hf_years_since_fire = unique(fire_year) - hf_fire_year,
      .groups = "drop"
    ) %>%
    # Clean-up
    dplyr::select(id, hf_fire_year, hf_years_since_fire)
  
  ## 2.3 Sample CanLaD harvest (1985-2020) and fire disturbances ####
  sampled_canlad_disturbances <- burn_sample_points %>%
    # Nest by study fire and year
    dplyr::group_nest(fire_id, fire_year) %>% 
    # Join table of CanLaD raster file names
    dplyr::left_join(canlad_disturbance_rasters, by = "fire_id") %>% 
    # Sample rasters
    dplyr::mutate(
      canlad_samples = purrr::map2(
        canlad_file_path,
        data,
        ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
            tibble::as_tibble() %>% 
            dplyr::select(-ID) %>% 
            dplyr::bind_cols(tibble::tibble(id = .y$id))
      )
    ) %>% 
    # Clean-up and unnest
    dplyr::select(fire_id, fire_year, canlad_samples) %>%
    tidyr::unnest(cols = c(canlad_samples))

    ## 2.4 Sample pre-CanLaD (1964-1984) harvest and fire disturbances ####
    sampled_precanlad_disturbances <- burn_sample_points %>%
      # Nest by study fire and year
      dplyr::group_nest(fire_id, fire_year) %>% 
      # Join table of pre-CanLaD raster file names
      dplyr::left_join(precanlad_disturbance_rasters, by = "fire_id") %>% 
      # Sample rasters
      dplyr::mutate(
        precanlad_samples = purrr::map2(
          precanlad_file_path,
          data,
          ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
              tibble::as_tibble() %>% 
              dplyr::select(-ID) %>% 
              dplyr::bind_cols(tibble::tibble(id = .y$id))
        )
      ) %>% 
      # Clean-up and unnest
      dplyr::select(fire_id, fire_year, precanlad_samples) %>%
      tidyr::unnest(cols = c(precanlad_samples))
    
    ## 2.5 Combined CanLaD and pre-CanLaD disturbance years
    sampled_combined_canlad_disturbances <- dplyr::left_join(
      sampled_canlad_disturbances,
      sampled_precanlad_disturbances,
      by = c("fire_id", "fire_year", "id")
    ) %>% 
      # Get most recent disturbance years from CanLaD and pre-CanLaD
      dplyr::mutate(
        canlad_fire_year = pmax(
          precanlad_fire_year,
          canlad_fire_year,
          na.rm = TRUE
        ),
        canlad_years_since_fire = fire_year - canlad_fire_year,
        canlad_harvest_year = pmax(
          precanlad_harvest_year,
          canlad_harvest_year,
          na.rm = TRUE
        ),
        canlad_years_since_harvest = fire_year - canlad_harvest_year
      ) %>% 
      # Clean up
      dplyr::select(
        id, canlad_fire_year, canlad_years_since_fire,
        canlad_harvest_year, canlad_years_since_harvest
      )
  
  # -------------------------------------------------------------------------- #
  # Step 3: Join forest vegetation/disturbances and biogeo                  ####
  
  # Join sampled vegetation and disturbance data
  joined_forest_variables <- sampled_vegetation %>% 
    # Left-join consolidated cutblock attributes
    dplyr::left_join(sampled_cutblocks, by = "id") %>% 
    # Left-join historical fire attributes
    dplyr::left_join(sampled_historical_fires, by = "id") %>% 
    # Left-join CanLaD disturbance years
    dplyr::left_join(sampled_combined_canlad_disturbances, by = "id") %>% 
    # Left-join BC RESULTS disturbance attributes
    dplyr::left_join(bc_results_tbl, by = "id") %>% 
    # Left-join topo and biogeo attributes
    dplyr::left_join(topo_geo_tbl, by = "id")
  
  # -------------------------------------------------------------------------- #
  # Step 4: Remove samples with potentially biased burn ratios, return      ####
  
  # Samples with disturbances 1 year prior, 1 year after or same year as fire
  # will have biased burn ratios because it may accentuate image differencing
  forest_variables <- joined_forest_variables %>% 
    # Remove observations where cutblocks/burns occurred in 1-year window around 
    # study fire (e.g. salvage logging)
    dplyr::filter(!(id %in% biased_burn_ratio_sample_ids))
    
  # Return
  return(forest_variables)
}