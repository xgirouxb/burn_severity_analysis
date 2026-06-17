define_sampling_polygons <- function(study_fire_polygons){
  
  # Buffer fire polygons and fill in holes to allow sampling of unburned areas
  # in surrounding landscape and skips/refugia within the burn area
  sampling_polygons <- study_fire_polygons %>% 
    # Split into list of study fires
    dplyr::group_split(fire_id) %>% 
    # Double the area and fill in the holes of each fire polygon
    purrr::map(
      function(study_fire) {
        
        # Compute the required buffer distance to double the polygon area
        dbl_buffer <- find_buffer_distance(study_fire)
        
        # Delete holes 
        filled_fire_polygon <- delete_holes(study_fire)
        
        # Apply buffer to double polygon area
        buffered_fire_polygon <- sf::st_buffer(
          filled_fire_polygon,
          dist = dbl_buffer
        )
        
        # Return
        return(buffered_fire_polygon)
      }
    ) %>% 
    # Combine
    dplyr::bind_rows()
  
  # Return
  return(sampling_polygons)
}
