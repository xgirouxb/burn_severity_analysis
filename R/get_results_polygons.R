get_results_openings_polygons <- function(study_fire_sampling_polygons) {
  
  # Import BC RESULTS openings polygons that intersect study fires
  results_openings_polygons <- study_fire_sampling_polygons %>%
    # Add 10-km buffer for downstream neighbourhood variables
    sf::st_buffer(dist = 10000) %>% 
    # Nest by fire (to respect {bcdata} spatial query size limits)
    dplyr::group_split(fire_id) %>%
    # Import polygons that intersect with each study fire's sampling area
    purrr::map(
      function(study_fire) {
        
        # Spatially filter RESULTS openings that intersect fire polygons
        openings_retrieved <- bcdata::bcdc_query_geodata(
          uuid_results_openings
        ) %>%
          dplyr::filter(bcdata::INTERSECTS(study_fire)) %>%
          # Retrieved from database
          bcdata::collect()
        
        # If no intersecting polygons, return NULL
        if (nrow(openings_retrieved) == 0) { return(NULL) }
        
        # Else, return parsed openings
        openings_retrieved <- openings_retrieved %>%
          # Add fire id and year
          dplyr::mutate(
            fire_id = unique(study_fire$fire_id),
            fire_year = unique(study_fire$fire_year)
          ) %>%
          # Parse column data types
          dplyr::mutate(
            # Parse id
            OPENING_ID = as.integer(OPENING_ID),
            # Parse dates
            dplyr::across(dplyr::ends_with("_DATE"), lubridate::ymd),
            # Parse codes
            dplyr::across(dplyr::ends_with("_CODE"), as.character),
            # Parse counts
            dplyr::across(dplyr::ends_with("_COUNT"), as.numeric)
          ) %>% 
          # Clean up 
          dplyr::select(
            fire_id, fire_year, OPENING_ID,
            APPROVE_DATE, DISTURBANCE_START_DATE, DISTURBANCE_END_DATE,
            DENUDATION_1_DISTURBANCE_CODE, DENUDATION_1_COMPLETION_DATE,
            DENUDATION_2_DISTURBANCE_CODE, DENUDATION_2_COMPLETION_DATE,
            SITE_PREP_1_TECHNIQUE_CODE, SITE_PREP_1_COMPLETION_DATE,
            SITE_PREP_2_TECHNIQUE_CODE, SITE_PREP_2_COMPLETION_DATE, 
            PLANTING_1_TECHNIQUE_CODE, PLANTING_1_COMPLETION_DATE,
            PLANTING_2_TECHNIQUE_CODE, PLANTING_2_COMPLETION_DATE,
            PLANTING_COUNT
          )
        return(openings_retrieved)
      }
    ) %>%
    # Convert list to sf
    dplyr::bind_rows()
  
  # Return
  return(results_openings_polygons)
}

get_results_plantings_polygons <- function(results_openings_polygons) {
  
  # Import BC RESULTS forest cover polygons that intersect with planted openings
  results_planting_polygons <- results_openings_polygons %>%
    # Only retain openings with plantings
    dplyr::filter(PLANTING_COUNT > 0) %>% 
    # Split into chunks of with max 1000 openings within each fire
    # (to respect {bcdata} query size limits)
    dplyr::group_by(fire_id) %>%
    dplyr::mutate(chunk = dplyr::ntile(n = ceiling(dplyr::n()/1000))) %>%
    dplyr::ungroup() %>% 
    dplyr::group_split(fire_id, chunk) %>% 
    # Import polygons that intersect with each study fire's sampling area
    purrr::map(
      function(study_fire_openings_chunk) {
        
        # Get opening ids for database query
        opening_ids <- dplyr::pull(study_fire_openings_chunk, OPENING_ID)
        
        # Filter RESULTS plantings by OPENING_ID
        plantings_retrieved <- bcdata::bcdc_query_geodata(
          uuid_results_plantings
        ) %>%
          dplyr::filter(OPENING_ID %in% opening_ids) %>% 
          # Retrieve from database
          bcdata::collect()
        
        # If no intersecting polygons, return NULL
        if (nrow(plantings_retrieved) == 0) { return(NULL) }
        
        # Else, return parsed plantings
        plantings_retrieved <- plantings_retrieved %>%
          # Add fire id and year
          dplyr::mutate(
            fire_id = unique(study_fire_openings_chunk$fire_id),
            fire_year = unique(study_fire_openings_chunk$fire_year)
          ) %>%
          # Parse column data types
          dplyr::mutate(
            OPENING_ID = as.integer(OPENING_ID),
            SILV_TREE_SPECIES_CODE = as.character(SILV_TREE_SPECIES_CODE),
            NUMBER_PLANTED = as.integer(NUMBER_PLANTED)
          ) %>% 
          # Select attributes of interest
          dplyr::select(
            fire_id, fire_year,
            OPENING_ID, SILV_TREE_SPECIES_CODE, NUMBER_PLANTED
          )
        return(plantings_retrieved)
      }
    ) %>% 
    # Convert list to sf
    dplyr::bind_rows()
  
  # Return
  return(results_planting_polygons)
}