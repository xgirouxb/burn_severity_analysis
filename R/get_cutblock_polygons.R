get_cutblock_polygons <- function(study_fire_sampling_polygons){

  # Import BC consolidated cutblocks that intersect and predate study fires
  cutblock_polygons <- study_fire_sampling_polygons %>%
    # Nest by fire
    dplyr::group_split(fire_id, fire_year) %>%
    # Import cutblocks that intersect with individual fires
    purrr::map(
      .f = ~{
        # Spatial intersection filter of fire sampling polygons
        cutblocks_retrieved <- bcdata::bcdc_query_geodata(uuid_bc_cutblocks) %>%
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
            fire_year = .x$fire_year
          ) %>%
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
            # Fire IDs
            dplyr::starts_with("fire_"),
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
      }
    ) %>%
    # Unnest
    dplyr::bind_rows() %>% 
    # Add consolidated cutblock prefix to variable names
    dplyr::rename_with(
      ~ paste0("cc_", .x),
      # Exclude IDs, geometry, and variables that already have the prefix
      !(fire_year | fire_id | geometry)
    )
  
  # Return
  return(cutblock_polygons)
}
