get_cutblock_polygons <- function(study_fire_sampling_polygons){
  
  # Define consolidated cutblock archive
  cutblock_polygons_archive <- bcdata::bcdc_query_geodata(uuid_bc_cutblocks)
  
  # Import intersections of BC consolidated cutblocks and study fires
  cutblock_polygons <- study_fire_sampling_polygons %>%
    # Nest by fire
    dplyr::group_split(fire_id, fire_year, fire_start_date, fire_end_date) %>%
    # Import cutblocks that intersect with individual fires
    purrr::map(
      .f = ~{
        # Spatial intersection filter of fire sampling polygons
        cutblocks_retrieved <- cutblock_polygons_archive %>%
          dplyr::filter(bcdata::INTERSECTS(.x)) %>%
          bcdata::collect() 
        
        # If there are no cutblocks intersecting fire, return NULL
        if (nrow(cutblocks_retrieved) == 0 ) return(NULL)
        
        # Else add fire IDs and clean up
        cutblocks_retrieved %>%
          # Clean names
          janitor::clean_names() %>% 
          # Add fire attributes
          dplyr::mutate(
            fire_id = .x$fire_id,
            fire_year = .x$fire_year,
            fire_start_date = .x$fire_start_date,
            fire_end_date = .x$fire_end_date,
          ) %>% 
          # Select cutblock attributes of interest
          dplyr::select(
            # Fire IDs
            dplyr::starts_with("fire_"),
            # Cutblock IDs
            vccb_sysid, opening_id,
            # Time
            harvest_start_date, harvest_end_date,
            harvest_start_year = harvest_start_year_calendar,
            # Cutblock attributes
            percent_clearcut, percent_partial_cut, area_ha,
            # Source
            data_source, data_source_date,
            # Geometry
            geometry
          ) %>% 
          # Coerce datatypes to fix errors when some tibble columns are all NAs
          dplyr::mutate(
            vccb_sysid = as.integer(vccb_sysid),
            opening_id = as.integer(opening_id),
            harvest_start_date = lubridate::as_date(harvest_start_date),
            harvest_end_date = lubridate::as_date(harvest_end_date),
            harvest_start_year = as.integer(harvest_start_year),
            percent_clearcut = as.integer(percent_clearcut),
            percent_partial_cut = as.integer(percent_partial_cut),
            area_ha = as.integer(area_ha)
          )
      }
    ) %>%
    # Unnest
    dplyr::bind_rows()
}
