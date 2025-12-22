get_cutblock_polygons <- function(study_fire_sampling_polygons){

  # Import BC consolidated cutblocks that intersect study fires
  cutblock_polygons <- study_fire_sampling_polygons %>%
    # Add 10-km buffer for downstream neighbourhood variables
    sf::st_buffer(dist = 10000) %>% 
    # Nest by fire (to respect {bcdata} spatial query size limits)
    dplyr::group_split(fire_id) %>%
    # Import cutblocks that intersect with each study fire's sampling area
    purrr::map(
      function(study_fire) {
        # Spatially filter cutblocks that intersect fire polygons
        cutblocks_retrieved <- bcdata::bcdc_query_geodata(uuid_bc_cutblocks) %>%
          dplyr::filter(bcdata::INTERSECTS(study_fire)) %>%
          bcdata::collect() 
        
        # If there are no cutblocks intersecting fire, return NULL
        if (nrow(cutblocks_retrieved) == 0 ) { return(NULL) }
        
        # Else clean up attributes of interest
        cutblocks_retrieved <- cutblocks_retrieved %>%
          # Clean names
          janitor::clean_names() %>% 
          # Coerce datatypes to fix errors when some tibble columns are all NAs
          dplyr::mutate(
            vccb_sysid = as.integer(vccb_sysid),
            opening_id = as.integer(opening_id),
            harvest_start_date = lubridate::as_date(harvest_start_date),
            harvest_end_date = lubridate::as_date(harvest_end_date),
            harvest_start_year = as.integer(harvest_start_year_calendar),
            harvest_end_year = as.integer(lubridate::year(harvest_end_date)),
            percent_clearcut = as.integer(percent_clearcut),
            percent_partial_cut = as.integer(percent_partial_cut),
            area_ha = as.integer(area_ha)
          ) %>%            
          # Select cutblock attributes of interest
          dplyr::select(
            # Cutblock IDs
            vccb_sysid, opening_id,
            # Time
            harvest_start_date, harvest_end_date,
            harvest_start_year, harvest_end_year,
            # Cutblock attributes
            percent_clearcut, percent_partial_cut, area_ha,
            # Source
            data_source, data_source_date,
            # Geometry
            geometry
          )
        return(cutblocks_retrieved)
      }
    ) %>%
    # Unnest
    dplyr::bind_rows() %>% 
    # Remove duplicates (some cutblocks may intersect >1 study fire polygon)
    dplyr::distinct(vccb_sysid, opening_id, .keep_all = TRUE) %>% 
    # Filter cutblocks in study period (+ 1 year)
    dplyr::filter(harvest_start_year <= (max(study_years) + 1)) %>% 
    # Add consolidated cutblock prefix to variable names (exclude geometry)
    dplyr::rename_with(~ paste0("cc_", .x), .cols = !geometry)
  
  # Return
  return(cutblock_polygons)
}