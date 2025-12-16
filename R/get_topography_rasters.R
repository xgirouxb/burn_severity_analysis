get_topography_rasters <- function(
    study_fire_sampling_polygons
){
  
  # Define local cache directory for output rasters, create if it doesn't exist
  topo_cache <- fs::path("data/_cache/topographic_metrics")
  if (!fs::dir_exists(topo_cache)) { fs::dir_create(topo_cache) }
  
  # Connect to NRCAN Digital Surface Model COG
  topo_cog <- terra::rast(paste0("/vsicurl/", url_nrcan_elevation))
  
  # Get NRCAN DSM projection (Canada Lambert)
  nrcan_proj <- terra::crs(topo_cog)
  
  # Create list of topography metrics raster file paths for each study fire
  topography_paths <- study_fire_sampling_polygons %>% 
    # Split by study fire
    dplyr::group_split(fire_id) %>% 
    # Extract digital surface model for each study fire and compute topo metrics
    purrr::map(
      ~{
        # Reproject study fire polygon and buffer to eliminate edge effects
        aoi_buf <- terra::project(x = terra::vect(.x), y = nrcan_proj) %>% 
          terra::buffer(1060)

        # Crop NRCAN DSM to buffered study fire area, reproject to study proj
        dem <- terra::crop(x = topo_cog, y = aoi_buf, mask = TRUE) %>% 
          terra::project(study_proj, res = 30)
        names(dem) <- "dem"
        
        # Compute slope
        slope <- terra::terrain(dem)
        names(slope) <- "slope"
        
        # Compute potential direct incident radiation
        pdir <- compute_pdir(dem)
        names(pdir) <- "pdir"

        # Landscape neighbourhood sizes (in metres)
        neighbourhood_radii <- c(100, 500, 1000)
        
        # Compute topographical metrics in landscape neighbourhoods 
        topo_metrics <- purrr::map(
          neighbourhood_radii,
          function(radius) {
            
            # Compute Terrain Ruggedness Index 
            tri <- compute_tri(x = dem, dist = radius, window = "circle")
            names(tri) <- paste0("tri_", radius, "m")
            
            # Compute Topographic Position Index 
            tpi <- compute_tpi(x = dem, dist = radius, window = "circle")
            names(tpi) <- paste0("tpi_", radius, "m")
            
            # Return
            return(c(tri, tpi))
          }
        ) %>% 
          # Combine into single SpatRast
          terra::rast()

        # Combine topographic layers into SpatRast
        topography_raster <- c(dem, slope, pdir, topo_metrics) %>% 
          # Mask to study fire area
          terra::crop(y = .x, mask = TRUE)

        # File path for writing raster to file
        raster_file_path <- fs::path(topo_cache, paste0(.x$fire_id, ".tif"))
        
        # Write topography metrics raster to file  
        terra::writeRaster(
          x = topography_raster, 
          filename = raster_file_path,
          overwrite = TRUE,
          datatype = "FLT4S"
        )
        
        # Return tbl of raster file path
        tibble::tibble(
          fire_id = .x$fire_id,
          raster_file_path = raster_file_path
        )
      }
    ) %>%
    # Combine
    dplyr::bind_rows()
  
  # Return list of topography raster file paths
  return(topography_paths)
}
