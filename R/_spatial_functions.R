#' Find buffer distance that doubles a polygon's area
#'
#' @description
#' This function calculates the approximate buffer distance required to 
#' double the surface area of a given `sf` geometry. Starts with a reasonable
#' ballpark based on relative size of input polygon uses `uniroot()` to find the
#' buffer distance that minimizes the difference between geometry area and
#' doubled area. 
#'
#' @param sf_poly An \code{sf} "POLYGON" or "MULTIPOLYGON" object.
#' @param tolerance Numeric. The tolerance, or allowed difference, between input
#'                           geometry area and the target area (i.e., double).
#'
#' @return A numeric value, the buffer distance required to double the . Returns 0 if 
#' the input geometry has an area of 0.
find_buffer_distance <- function(sf_poly, tolerance = 0.01) {
  
  # Input geometry area, return 0 if area is 0
  geom_area <- as.numeric(sf::st_area(sf_poly))
  if (geom_area == 0) return(0)
  
  # Target is doubling the area of the polygon
  target_area <- geom_area * 2
  
  # Get the diagonal of the bounding box as a proxy for polygon size
  bbox <- sf::st_bbox(sf_poly)
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]
  diag <- as.numeric(sqrt(width^2 + height^2))
  
  # Fast guess upper bound as 1/20 of bbox diagonal
  upper_bound <- diag * 0.05
  
  # Simplify polygon based on size of input geometry to speed up sf::st_buffer
  geom_fast <- sf::st_simplify(sf_poly, dTolerance = diag * 0.01) %>% 
    sf::st_geometry()
  
  # Define function to minimize: difference between current and target area
  area_diff <- function(dist) {
    as.numeric(sf::st_area(sf::st_buffer(geom_fast, dist = dist))) - target_area
  }
  
  # Expand upper bound if buffer distance is too small to double area 
  while (area_diff(upper_bound) < 0) { upper_bound <- upper_bound * 2 }
  
  # Search for buffer distance that doubles geometry area
  result <- uniroot(area_diff, lower = 0, upper = upper_bound, tol = tolerance)
  
  # Return buffer distance that is within tolerance
  return(result$root)
}

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
    list_sf_poly,
    function(sf_poly) {
      
      # Map over each element of geometry and select outer ring
      outer_rings <- purrr::map(
        sf::st_geometry(sf_poly),
        function(geoms) {
          sf::st_multipolygon(purrr::map(geoms, function(x) { list(x[[1]]) }))
        }
      )
      
      # Replace input geometries with outer rings
      output_sf <- sf_poly %>% 
        # Remove current geometry
        dplyr::select(-geometry) %>% 
        # Replace with sfc of outer rings
        dplyr::mutate(
          geometry = sf::st_sfc(outer_rings, crs = sf::st_crs(sf_poly))
        ) %>% 
        # Set geometry column
        sf::st_set_geometry("geometry") %>% 
        # Dissolve island vertices
        sf::st_make_valid() %>%
        sf::st_buffer(0)
      
      # Return
      return(output_sf)
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

#' Na-proof maximum  
#'
#' This function calculates the maximum value of a vector while ignoring `NA`
#' values. Unlike the standard [base::max()], this function returns `NA` 
#' instead of `-Inf` when the input vector contains only `NA` values or is empty.
#'
#' @param x A numeric value or vector.
#'
#' @return 
#' A single numeric value representing the maximum, or `NA` if no 
#' non-missing values are present.
#' 
#' @examples
#' maximum(c(1, 5, 2, NA))
#' # [1] 5
#' 
#' maximum(c(NA, NA))
#' # [1] NA
maximum <- function(x) { ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)) }

#' Compute the Terrain Ruggedness Index (TRI) within a neighborhood window
#'
#' This function calculates the Terrain Ruggedness Index (TRI) in a neighboorhood
#' window from elevation values in a \code{terra} SpatRaster object.
#'
#' The TRI measures the total change in elevation or value across a cell's
#' neighborhood by summing the squared differences in value between the central
#' cell and its surrounding neighbours, then taking the square root of that sum.
#' For memory efficiency, the version used here is an algebraic expansion 
#' of this formula that solely relies on terra functions implemented in C++.
#'
#' @param x A terra `SpatRaster` object with elevation data (e.g., DEM, DSM).
#' @param dist Numeric. The distance from the focal cell to the window edge (radius 
#' for "circle", or half-edge of a "rectangle"), expressed in units of the 
#' raster's Coordinate Reference System (CRS).
#' @param window Character. The shape of the neihbourhood window; must be either 
#' `"circle"` or `"rectangle"`.
#' @param ... Additional arguments passed to `terra::focal()`.
#'
#' @return 
#' A `SpatRaster` object containing the computed TRI values.
#'
#' @references
#' Riley, S. J., DeGloria, S. D., & Elliot, R. (1999).
#' A terrain ruggedness index that quantifies topographic heterogeneity.
#' Intermountain Journal of Sciences, 5(1-4), 23-27.
#'
compute_tri <- function(
    x,
    dist,
    window = c("circle", "rectangle"),
    ...
) {
  
  # Check window type 
  if (!window %in% c("circle", "rectangle")) {
    stop("`window` must be either 'circle' or 'rectangle'.")
  }
  
  # Define window, set weights to 1 and NA
  window <- terra::focalMat(x, d = dist, type = window)
  window <- ifelse(window > 0, 1, 0)
  
  # Set window centre cell to NA
  window[((length(window) + 1)/2)] <- 0
  
  # Sum of neighbours
  sum_neigh   <- terra::focal(x, window, fun = "sum", ...)
  
  # Sum of squared neighbours
  sum_sq_neigh  <- terra::focal(x^2, window, fun = "sum", ...)
  
  # Number of neighbours 
  n_neigh <- sum(window)
  
  # TRI formula (expanded):
  tri <- sqrt(sum_sq_neigh - 2 * x * sum_neigh + n_neigh * x^2)
  
  # Return
  return(tri)
}

#' Compute the Topographic Position Index (TPI) for a neighborhood window
#'
#' This function calculates the Topographic Position Index (TPI) in a neighboorhood
#' window using elevation values in a ' \code{terra} SpatRaster object.
#' 
#' TPI measures the relative elevation of a central cell compared to the mean
#' elevation of its surrounding neighbors. A positive TPI indicates a ridge or peak,
#' a negative TPI indicates a valley or depression, and a value near zero indicates
#' a flat area or constant slope.
#'
#' @param x A terra `SpatRaster` object with elevation data (e.g., DEM, DSM).
#' @param dist Numeric. The distance from the focal cell to the window edge (radius 
#' for "circle", or half-edge of a "rectangle"), expressed in units of the 
#' raster's Coordinate Reference System (CRS).
#' @param window Character. The shape of the neihbourhood window; must be either 
#' `"circle"` or `"rectangle"`.
#' @param ... Additional arguments passed to `terra::focal()`.
#' 
#' @return 
#' A `SpatRaster` object containing the computed TRI values.
#'
#' @references
#' Weiss, A. D. (2001).
#' Topographic Position and Landforms Analysis.
#' ESRI user conference.
#'
compute_tpi <- function(
    x,
    dist, 
    window = c("circle", "rectangle"),
    ...
) {
  
  # Check window type 
  if (!window %in% c("circle", "rectangle")) {
    stop("`window` must be either 'circle' or 'rectangle'.")
  }
  
  # Define neighbourhood window, set weights to 1 and 0
  window <- terra::focalMat(x, d = dist, type = window)
  window <- ifelse(window > 0, 1, 0)
  
  # Set window centre cell to NA
  window[((length(window) + 1)/2)] <- 0
  
  # Compute neighbourhood mean
  mean <- terra::focal(x, w = window, fun = "mean", ...)
  
  # Compute TPI
  tpi <- x - mean
  
  # Return
  return(tpi)
}

#' Compute the potential direct incident radiation (PDIR) on topographical features.
#' 
#' This function calculates the potential direct incident radiation (PDIR)
#' using elevation values in a ' \code{terra} SpatRaster object using equation 3
#' from McCune & Keon (2002), a more precise estimation that is restricted to a
#' latitude of 30-60 degrees North.
#'
#' @param x A terra `SpatRaster` object with elevation data (e.g., DEM, DSM).
#'
#' @return
#' A terra `SpatRaster` object containing the computed potential direct incident
#' radiation(PDIR) values, expressed in Rad (\eqn{MJ \cdot cm^{-2} \cdot year^{-1}})
#'
#' @references
#' McCune, B., & Keon, D. (2002). Equations for potential annual direct incident
#' radiation and heat load. *Journal of Vegetation Science*, 13(4): 603-606
#' McCune, B. (2007). Equations for potential annual direct incident radiation and heat load.
#' *Journal of Vegetation Science*, 18: 751-754. 
#'
compute_pdir <- function(x) {
  
  # Get DEM mask
  mask <- terra::ifel(is.na(x), NA, 1)
  
  # Compute aspect
  aspect <- terra::terrain(x = x, v = "aspect", unit = "radians")
  
  # Fold aspect around N-S axis for computing PDIR in Northern hemisphere
  aspect <- pi - abs(aspect - pi)
  
  # Compute slope
  slope <- terra::terrain(x = x, v = "slope", unit = "radians")
  
  # Compute latitude in degrees (agnostic to input CRS)
  latitude_deg <- terra::setValues(
    x = x,
    values = terra::project(
      x = terra::crds(x, na.rm = FALSE),
      from = terra::crs(x),
      to = "EPSG:4326"
    )[,2]
  ) * mask
  
  # Sanity check, is latitude within acceptable range for equation
  lat_range <- terra::global(latitude_deg, "range", na.rm = TRUE)
  if (lat_range["min"] < 30 | lat_range["max"] > 60) {
    stop("Outside latitude range for solar incidence radiation equation.")
  }  
  
  # Convert latitude to radians
  latitude <- (latitude_deg * pi) / (180)
  
  # Compute PDIR using McCune & Keon (2002) equation 3
  pdir <- 0.339 +
    0.808 * cos(latitude) * cos(slope) +
    -0.196 * sin(latitude) * sin(slope) +
    -0.482 * cos(aspect) * sin(slope)
  
  # Return in Rad (MJ * (cm^–2) * (year^–1)) 
  return(pdir)
}

#' Sample a raster at point locations.
#'
#' A helper function that extracts raster values at sf point locations,
#' returning a tibble with the original point attributes bound to the
#' extracted raster values.
#'
#' @param raster_file_path A character string giving the file path to a raster
#'   readable by \code{terra::rast()}.
#' @param sf_points An \code{sf} POINTS object containing point geometries at 
#'                  which raster values will be extracted.
#'
#' @return
#' A \code{tibble} containing the attributes of \code{sf_points} bound to the
#' extracted raster values, with one row per point.
#'
sample_raster <- function(raster_file_path, sf_points) {
  terra::extract(
    x = terra::rast(raster_file_path),
    y = terra::vect(sf_points),
    bind = TRUE
  ) %>%
    tibble::as_tibble()
}
