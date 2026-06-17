define_sampling_points <- function(
    study_fire_polygons,
    sampling_polygons,
    n_workers = NULL
) {
  
  # Setup parallel processing if n_workers is supplied
  if(!is.null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Randomly generate sampling points within each study fire sampling polygon
  study_sampling_points <- sampling_polygons %>% 
    # Stop sampling when there is approximately 1 sample per 8 ha 
    dplyr::mutate(
      area_ha = units::drop_units(units::set_units(sf::st_area(.), ha)),
      max_samples = round(area_ha/8)
    ) %>% 
    # Make list of fire sf polygons
    group_split(fire_id) %>% 
    # Map Sequential Sampling Inhibition algorithm 
    furrr::future_map(
      function(fire_polygon) {
        
        # Set random seed
        set.seed(42)
        
        # Get NBAC polygon that only includes burned areas
        burned_polygon <- study_fire_polygons %>% 
          dplyr::filter(fire_id == unique(fire_polygon$fire_id)) %>% 
          dplyr::mutate(burned = 1) %>% 
          dplyr::select(burned)
        
        # Create sample points in fires and surrounding areas (buffer, skips)
        sample_points <- sf::st_sample(
          x = fire_polygon, 
          type = "SSI",  # see ?spatstat.random::rSSI
          # Enforce 100-m minimum distance between samples
          r = 100,
          # Stop sampling once max number of samples is reached
          n = unique(fire_polygon$max_samples)
        ) %>%
          # Project and cast to sf, add fire_id and fire_year
          sf::st_set_crs(study_proj) %>% 
          sf::st_as_sf() %>%
          dplyr::mutate(
            fire_id = unique(fire_polygon$fire_id),
            fire_year = unique(fire_polygon$fire_year)
          ) %>% 
          # Clean up, rename geometry column
          dplyr::select(fire_id, fire_year, geometry = geom) %>% 
          # Classify the samples as burned (1) or not burned (0) based on 
          # their intersection with the NBAC polygon
          sf::st_join(y = burned_polygon) %>% 
          dplyr::mutate(burned = tidyr::replace_na(burned, 0))
        
        # Return sample points 
        return(sample_points)
      },
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = TRUE)
    ) %>% 
    # Bind rows
    dplyr::bind_rows() %>% 
    # Add unique sample id
    dplyr::mutate(id = dplyr::row_number()) %>% 
    dplyr::relocate(id, .before = dplyr::everything())
  
  # Close parallel processing if n_workers is supplied
  if(!is.null(n_workers)) { future::plan(strategy = "future::sequential") }
    
  # Return sampling points
  return(study_sampling_points)
}