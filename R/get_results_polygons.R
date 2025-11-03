get_results_openings_polygons <- function(study_fire_sampling_polygons) {
  
  # Import BC RESULTS openings polygons that intersect study fires
  results_openings_polygons <- study_fire_sampling_polygons %>%
    # Nest by fire (to respect {bcdata} spatial query size limits)
    dplyr::group_split(fire_id) %>%
    # Import polygons that intersect with each study fire's sampling area
    purrr::map(
      .f = ~{
        # Spatially filter RESULTS openings that intersect fire polygons
        results_retrieved <- bcdata::bcdc_query_geodata(uuid_results_openings) %>%
          dplyr::filter(bcdata::INTERSECTS(.x)) %>%
          # Retrieved from database
          bcdata::collect()
        
        # If no intersecting polygons, return NULL
        if(nrow(results_retrieved) == 0) return(NULL)
        
        # Else, return parsed
        results_retrieved %>%
          # Add fire id and year
          dplyr::mutate(
            fire_id = dplyr::first(.x$fire_id),
            fire_year = dplyr::first(.x$fire_year)
          ) %>%
          # Parse column data types
          dplyr::mutate(
            # Parse dates
            dplyr::across(dplyr::ends_with("_DATE"), lubridate::ymd),
            # Parse characters
            dplyr::across(dplyr::ends_with("_CODE"), as.character),
            dplyr::across(dplyr::ends_with("_ID"), as.character),
            dplyr::across(dplyr::ends_with("_INDEX"), as.character),
            # Parse numeric
            dplyr::across(dplyr::ends_with("_COUNT"), as.numeric),
            dplyr::across(dplyr::ends_with("_AREA"), as.numeric),
            dplyr::across(dplyr::ends_with("_PCT"), as.numeric)
          )
      }
    ) %>%
    dplyr::bind_rows()
  
  # Return
  return(results_openings_polygons)
}