define_study_area <- function(){
  
  # Import StatCan admin boundaries
  statcan_admin_bounds <- get_sf_from_source(
    sf_source = url_statcan_admin_bounds
  )
  
  # Build BC provincial bounds for study area
  study_area <- statcan_admin_bounds %>%
    # Filter British Columbia
    dplyr::filter(PRUID == 59) %>% 
    # Cast MULTIPOLYGON to POLYGON
    sf::st_cast("POLYGON", warn = FALSE) %>%
    # Fix topology errors
    sf::st_make_valid() %>%
    # Combine into single layer
    sf::st_union() 
  
  # Return
  return(study_area)
}

define_study_fires <- function(study_area, study_years){
  
  # Get NBAC archive URLs for study years
  list_nbac_subdir <- purrr::map(
    .x = study_years,
    .f = ~{get_url_list(url = url_nbac_archive, match_string = paste0(.x))
    }
  )
  
  # Get NBAC fire polygons that intersect with study area
  list_nbac_fire_polygons <- purrr::map(
    .x = list_nbac_subdir,
    .f = ~{
      get_sf_from_source(
        sf_source = .x,
        sf_aoi = study_area
      )
    }
  )
    
  # Bind list to single sf obj of fire polygons
  nbac_fire_polygons <- dplyr::bind_rows(list_nbac_fire_polygons) %>%
    # Retain only fires > 1000 hectares
    dplyr::filter(POLY_HA > 1000) %>% 
    # Make a unique fire_id
    dplyr::mutate(fire_id = paste0(YEAR, "_", NFIREID)) %>% 
    # Remove 2 fires that are not in MGH samples
    dplyr::filter(!(fire_id %in% c("2017_1002", "2018_1291")))
  
  # Return 
  return(nbac_fire_polygons)
  
}