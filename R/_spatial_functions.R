#' Delete holes and islands inside sf polygons
#' 
#' Removes interior holes and islands (e.g., polygons nested within holes)
#' from an \code{sf} polygon. Only the outermost rings of each polygon are
#' conserved and returned as a single-part geometry.
#'
#' @param sf_poly An \code{sf} "POLYGON" or "MULTIPOLYGON" object.
#'
#' @return An \code{sf} "MULTIPOLYGON" object outermost polygon rings only.
#'
delete_holes <- function(sf_poly) {
  
  # Cast to MULTIPOLYGON 
  if (!all(sf::st_is(sf_poly, "MULTIPOLYGON"))) {
    sf::st_cast(sf_poly, "MULTIPOLYGON")
  }
  
  # Split into list of polygons
  list_sf_poly <- dplyr::group_split(sf_poly, row_id = dplyr::row_number())
  
  # Map over each polygon
  list_sf_poly_outer <- purrr::map(
    .x = list_sf_poly,
    .f = ~{
      
      # Map over each element of geometry and select outer ring
      outer_rings <- purrr::map(
        sf::st_geometry(.x),
        ~ sf::st_multipolygon(purrr::map(.x, ~ list(.x[[1]])))
      )
      
      # Replace input geometries with outer rings
      output_sf <- .x %>% 
        # Remove current geometry
        dplyr::select(-geometry) %>% 
        # Replace with sfc of outer rings
        dplyr::mutate(
          geometry = sf::st_sfc(outer_rings, crs = sf::st_crs(.x))
        ) %>% 
        # Set geometry column
        sf::st_set_geometry("geometry") %>% 
        # Dissolve island vertices
        sf::st_make_valid() %>%
        sf::st_buffer(0)
      
      # Return
      output_sf
    }
  )
  
  # Clean up
  sf_poly_outer <- dplyr::bind_rows(list_sf_poly_outer) %>% 
    # Remove row_id
    dplyr::select(-row_id) %>% 
    # Set crs
    sf::st_set_crs(sf::st_crs(sf_poly))
  
  # Return
  return(sf_poly_outer)
}