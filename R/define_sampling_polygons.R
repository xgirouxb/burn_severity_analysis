define_sampling_polygons <- function(
    study_fire_polygons,
    n_workers = NULL
) {
  
  # Setup parallel processing if n_workers is supplied
  if (!is.null(n_workers)) {
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  # Expand each fire polygon to twice its original area. Filling interior holes
  # contributes toward the area increase; any remaining increase is obtained by
  # buffering outward into the surrounding landscape.
  sampling_polygons <- study_fire_polygons %>%
    # Split into list of study fires
    dplyr::group_split(fire_id) %>%
    # Map over each fire
    furrr::future_map(
      function(study_fire) {
        
        # Target area is twice the area of the original fire polygon
        target_area <- 2 * as.numeric(sf::st_area(study_fire))
        
        # Fill holes to include skips/refugia within the fire perimeter
        filled_fire_polygon <- delete_holes(study_fire)
        
        # Compute additional outward buffer required to reach twice the
        # original fire area
        buffer_distance <- find_buffer_distance(
          filled_fire_polygon,
          target_area = target_area
        )
        
        # Apply buffer
        sampling_polygon <- sf::st_buffer(
          filled_fire_polygon,
          dist = buffer_distance
        )
        
        # Return
        return(sampling_polygon)
      },
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = 42)
    ) %>%
    # Combine
    dplyr::bind_rows()
  
  # Close parallel processing if n_workers is supplied
  if (!is.null(n_workers)) {
    future::plan(strategy = "future::sequential")
  }
  
  # Return
  return(sampling_polygons)
}