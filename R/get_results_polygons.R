get_results_openings_polygons <- function(study_fire_sampling_polygons) {
  
  # Import BC RESULTS openings polygons that intersect study fires
  results_openings_polygons <- study_fire_sampling_polygons %>%
    # Nest by fire (to respect {bcdata} spatial query size limits)
    dplyr::group_split(fire_id) %>%
    # Import polygons that intersect with each study fire's sampling area
    purrr::map(
      .f = ~{
        
        # Spatially filter RESULTS openings that intersect fire polygons
        openings_retrieved <- bcdata::bcdc_query_geodata(uuid_results_openings) %>%
          dplyr::filter(bcdata::INTERSECTS(.x)) %>%
          # Retrieved from database
          bcdata::collect()
        
        # If no intersecting polygons, return NULL
        if(nrow(openings_retrieved) == 0) return(NULL)
        
        # Else, return parsed
        openings_retrieved %>%
          # Add fire id and year
          dplyr::mutate(
            fire_id = dplyr::first(.x$fire_id),
            fire_year = dplyr::first(.x$fire_year)
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
      }
    ) %>%
    dplyr::bind_rows()
  
  # Return
  return(results_openings_polygons)
}