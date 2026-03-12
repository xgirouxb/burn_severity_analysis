sample_forest_variables <- function(
    burn_sample_points,
    biased_burn_ratio_sample_ids,
    vri_polygons,
    land_cover_class_tbl,
    cutblock_polygons,
    historical_fire_polygons,
    results_polygons,
    canlad_disturbance_rasters,
    precanlad_disturbance_rasters,
    forestry_disturbance_rasters,
    ccfm_tenure_rasters,
    topography_rasters,
    vegetation_zone_polygons,
    biogeoclimatic_zone_polygons,
    firezone_polygons
) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Sample forest vegetation: VRI archive + land cover images       ####
  
  # Sample vegetation attributes (VRI and Land cover)
  sampled_vegetation <- burn_sample_points %>%
    # Nest samples by fire id
    # (as some VRI polygons might intersect points with mismatched years)
    dplyr::group_split(fire_id) %>%
    # Spatially join VRI attributes to sample points within each study fire
    purrr::map(
      function(study_fire) {
        sf::st_join(
          x = study_fire,
          # Only sample VRI polygons with matching fire ids
          y = vri_polygons %>%
            dplyr::filter(fire_id == unique(study_fire$fire_id)) %>%
            dplyr::select(-fire_id, -fire_year),
          # Inner join in case some samples don't have 
          left = FALSE
        ) 
      }
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
    sf::st_join(y = cutblock_polygons, left = FALSE) %>%
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
    # Compute year of and years since most recent harvest
    dplyr::mutate(
      cc_harvest_year = pmax(
        cc_harvest_start_year,
        cc_harvest_end_year,
        na.rm = TRUE
      ),
      cc_years_since_harvest = unique(fire_year) - cc_harvest_year
    ) %>%
    # Clean up
    dplyr::select(id, cc_harvest_year, cc_harvest_start_year,
                  cc_harvest_end_year, cc_years_since_harvest)
  
  ## 2.2 Sample historical fire polygon attributes ####
  sampled_historical_fires <- burn_sample_points %>%
    # Spatial inner join fires that intersect sample points
    sf::st_join(y = historical_fire_polygons, left = FALSE) %>%
    # Filter for historical fires that predate study fires
    dplyr::filter(hf_fire_year < fire_year) %>%
    # Drop geometries prior to summarise
    sf::st_drop_geometry() %>%
    # Compute year of and years since most recent historical fire
    dplyr::group_by(id, fire_id, fire_year) %>%
    dplyr::summarise(
      hf_fire_year = max(hf_fire_year),
      hf_years_since_fire = unique(fire_year) - hf_fire_year,
      .groups = "drop"
    ) %>%
    # Clean-up
    dplyr::select(id, hf_fire_year, hf_years_since_fire)
  
  ## 2.3 Sample harvest/fire/plantations in RESULTS polygons ####
  sampled_results <- burn_sample_points %>%
    # Nest samples by fire id and year
    # (as some RESULTS polygons might intersect points with mismatched years)
    dplyr::group_split(fire_id) %>%
    # Spatially join RESULTS attributes to sample points within each study fire
    purrr::map(
      function(study_fire_samples) {
        
        # Get study fire attributes
        study_fire_id <- unique(study_fire_samples$fire_id)
        study_fire_year <- unique(study_fire_samples$fire_year)
        
        # Only sample RESULTS polygons with matching fire ids
        sf::st_join(
          x = study_fire_samples,
          y = dplyr::filter(results_polygons, fire_id == study_fire_id) %>%
            # Select variables of interest
            dplyr::select(
              res_harvest_start_year, res_harvest_end_year, res_fire_year,
              res_planting_year, res_lead_spp, res_opening_id
            ) %>% 
            # Only sample polygons with disturbances that predate study fire
            dplyr::mutate(
              dplyr::across(
                dplyr::ends_with("_year"),
                ~ dplyr::if_else(.x < study_fire_year, .x, NA_real_)
              )
            ) %>% 
            dplyr::filter(!dplyr::if_all(dplyr::ends_with("_year"), is.na))
          ,
          # Inner join, remove samples that don't intersect RESULTS polygons
          left = FALSE
        )  
      }
    ) %>%
    # Convert list to sf
    dplyr::bind_rows() %>%
    # Drop geometries
    sf::st_drop_geometry() %>%
    # If >1 RESULTS polygon intersects point (approx 0.5% of samples)
    dplyr::group_by(id) %>%
    # Get most recent disturbance year
    dplyr::arrange(desc(res_planting_year), .by_group = TRUE) %>% 
    dplyr::summarise(
      id = dplyr::first(id),
      fire_year = unique(fire_year), 
      # res_harvest_year = maximum(res_harvest_end_year),
      res_harvest_start_year = maximum(res_harvest_start_year),
      res_harvest_end_year = maximum(res_harvest_end_year),
      res_fire_year = maximum(res_fire_year),
      res_planting_year = maximum(res_planting_year),
      # Get first non-NA species (sorted by planting year)
      res_lead_spp = dplyr::first(res_lead_spp, na_rm = TRUE),
      .groups = "drop"
    ) %>% 
    # Compute years since most recent harvest/fire/plantation
    dplyr::mutate(
      res_harvest_year = pmax(
        res_harvest_start_year,
        res_harvest_end_year,
        na.rm = TRUE
      ),
      res_years_since_harvest = fire_year - res_harvest_year,
      res_years_since_fire = fire_year - res_fire_year,
      res_years_since_planting = fire_year - res_planting_year
    ) %>%
    # Clean up
    dplyr::select(-fire_year) %>% 
    {.}
  
  ## 2.4 Sample CanLaD harvest (1985-2020) and fire disturbances ####
  sampled_canlad_disturbances <- burn_sample_points %>%
    # Nest by study fire and year
    dplyr::group_nest(fire_id, fire_year) %>% 
    # Join table of CanLaD raster file names
    dplyr::left_join(canlad_disturbance_rasters, by = "fire_id") %>% 
    # Sample rasters
    dplyr::mutate(
      canlad_samples = purrr::map2(
        raster_file_path,
        data,
        ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
          tibble::as_tibble() %>% 
          dplyr::mutate(id = .y$id, .before = dplyr::everything()) %>% 
          dplyr::select(-ID)
      )
    ) %>% 
    # Clean up and unnest
    dplyr::select(fire_id, fire_year, canlad_samples) %>%
    tidyr::unnest(cols = c(canlad_samples))

    ## 2.5 Sample pre-CanLaD (1964-1984) harvest and fire disturbances ####
    sampled_precanlad_disturbances <- burn_sample_points %>%
      # Nest by study fire and year
      dplyr::group_nest(fire_id, fire_year) %>% 
      # Join table of pre-CanLaD raster file names
      dplyr::left_join(precanlad_disturbance_rasters, by = "fire_id") %>% 
      # Sample rasters
      dplyr::mutate(
        precanlad_samples = purrr::map2(
          raster_file_path,
          data,
          ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
            tibble::as_tibble() %>% 
            dplyr::mutate(id = .y$id, .before = dplyr::everything()) %>% 
            dplyr::select(-ID)
        )
      ) %>% 
      # Clean-up and unnest
      dplyr::select(fire_id, fire_year, precanlad_samples) %>%
      tidyr::unnest(cols = c(precanlad_samples))
    
    ## 2.6 Combined CanLaD and pre-CanLaD disturbance years
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
  # Step 3: Sample CCFM forest tenure classes from rasters                  ####
  
  # Sample CCFM forest tenure types
  sampled_ccfm_forest_tenure <- burn_sample_points %>%
    # Nest by study fire and year
    dplyr::group_nest(fire_id, fire_year) %>% 
    # Join table of CCFM forest tenure classes raster file names
    dplyr::left_join(ccfm_tenure_rasters, by = "fire_id") %>% 
    # Sample rasters
    dplyr::mutate(
      ccfm_tenure_samples = purrr::map2(
        raster_file_path,
        data,
        ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
          tibble::as_tibble() %>% 
          dplyr::mutate(id = .y$id, .before = dplyr::everything()) %>% 
          dplyr::select(-ID)
      )
    ) %>% 
    # Clean-up and unnest
    dplyr::select(ccfm_tenure_samples) %>%
    tidyr::unnest(cols = c(ccfm_tenure_samples))
  
  # -------------------------------------------------------------------------- #
  # Step 4: Sample forestry disturbances from rasters                       ####  
  
  # Sample forestry disturbances
  sampled_forestry_disturbances <- burn_sample_points %>%
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
    dplyr::mutate(
      fd_years_since_harvest = fire_year - harvest_year,
      fd_years_since_planting = fire_year - res_planting_year
    ) %>% 
    dplyr::select(
      id, fd_harvest_year = harvest_year, fd_years_since_harvest,
      fd_planting_year = res_planting_year, fd_years_since_planting,
      fd_planted_spp = res_planted_spp, dplyr::everything(),
      -fire_year, -fire_id
    )
  
  # -------------------------------------------------------------------------- #
  # Step 5: Sample topographic metrics ####
  
  # Sample topographic metrics 
  sampled_topo_metrics <- burn_sample_points %>%
    # Nest by study fire and year
    dplyr::group_nest(fire_id, fire_year) %>% 
    # Join table of forest management raster file names
    dplyr::left_join(topography_rasters, by = "fire_id") %>% 
    # Sample rasters
    dplyr::mutate(
      topography_samples = purrr::map2(
        raster_file_path,
        data,
        ~ terra::extract(x = terra::rast(paste(.x)), y = terra::vect(.y)) %>% 
          tibble::as_tibble() %>% 
          dplyr::mutate(id = .y$id, .before = dplyr::everything()) %>% 
          dplyr::select(-ID) 
      )
    ) %>% 
    # Unnest and clean-up
    dplyr::select(topography_samples) %>%
    tidyr::unnest(cols = c(topography_samples))
  
  # -------------------------------------------------------------------------- #
  # Step 6: Sample biogeo/vegetation/fire zones  ####
  
  sampled_biogeo_veg_zones <- burn_sample_points %>% 
    # Spatial join vegetation zone polygons that intersect sample points
    sf::st_join(y = vegetation_zone_polygons) %>%
    # Spatial join biogeoclimatic zone polygons that intersect sample points
    sf::st_join(y = biogeoclimatic_zone_polygons) %>%
    # Spatial join fire regime types polygons that intersect sample points
    sf::st_join(y = firezone_polygons) %>%
    # Remove duplicates when a sample point intersects two polygons
    dplyr::slice(1, .by = id) %>% 
    # Assign representative FRT (most common) to each study fire
    dplyr::group_by(fire_id) %>%
    dplyr::mutate(
      frt = as.numeric(names(which.max(table(frt)))),
      frt_name = names(which.max(table(frt_name)))
    ) %>%
    dplyr::ungroup() %>% 
    # Clean up
    sf::st_drop_geometry() %>% 
    dplyr::select(id, cvz, cvz_name, bec, bec_name, frt, frt_name)

  # -------------------------------------------------------------------------- #
  # Step 7: Join forest vegetation/disturbances and biogeo ####
  
  # Join sampled vegetation and disturbance data
  joined_forest_variables <- sampled_vegetation %>% 
    # Left-join consolidated cutblock attributes
    dplyr::left_join(sampled_cutblocks, by = "id") %>% 
    # Left-join historical fire attributes
    dplyr::left_join(sampled_historical_fires, by = "id") %>% 
    # Left-join RESULTS polygon attributes
    dplyr::left_join(sampled_results, by = "id") %>%
    # Left-join CanLaD disturbance years
    dplyr::left_join(sampled_combined_canlad_disturbances, by = "id") %>% 
    # Left-join CCFM forest tenure 
    dplyr::left_join(sampled_ccfm_forest_tenure, by = "id") %>% 
    # Left-join forestry disturbances
    dplyr::left_join(sampled_forestry_disturbances, by = "id") %>% 
    # Left-join topographic data 
    dplyr::left_join(sampled_topo_metrics, by = "id") %>% 
    # Left-join biogeo attributes
    dplyr::left_join(sampled_biogeo_veg_zones, by = "id")
  
  # -------------------------------------------------------------------------- #
  # Step 8: Remove samples with potentially biased burn ratios, return      ####
  
  # Samples with disturbances 1 year prior, 1 year after or same year as fire
  # will have biased burn ratios because it may accentuate image differencing
  forest_variables <- joined_forest_variables %>% 
    # Remove observations where cutblocks/burns occurred in 1-year window around 
    # study fire (e.g. salvage logging)
    dplyr::filter(!(id %in% biased_burn_ratio_sample_ids)) %>% 
    # Clean up
    dplyr::select(id, dplyr::everything(), geometry, -old_fire_id)
    
  # Return
  return(forest_variables)
}