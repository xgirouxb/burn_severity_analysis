launch_ntems_land_cover_gee_tasks <- function(
    sampling_points,
    neighbourhood_radius = c(100, 500, 1000)
) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment and import modules ####
  
  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee$Initialize(project = EARTH_ENGINE_PROJECT_ID)
  
  # Import required module
  get_lc <- reticulate::import_from_path("get_ntems_lc_classes", "py")
  
  # Feature collection of burn sample points
  ee_sample_points <- ee$FeatureCollection(gee_assetid_sample_points)
  
  # Image collection of forest land cover
  # doi.org/10.1016/j.rse.2021.112780
  ee_forest_land_cover <- ee$ImageCollection(gee_assetid_land_cover)
  
  # Sanity check: `sampling_points` in GEE assets should have same number
  # of observations as local `sampling_points` target.
  if (ee_sample_points$size()$getInfo() != nrow(sampling_points)) {
    stop(
      "⚠️ `sampling_points` in GEE assets does not match local copy, upload latest version!"
    )
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Launch land cover sampling tasks ####
  
  # List of radii to compute land cover class proportions
  ee_radius_list <- ee$List(neighbourhood_radius)
  
  # Get list of unique fire_ids in local `sampling_points` target
  fire_id_list <- unique(sampling_points$fire_id)
  
  # Launch one Earth Engine task per fire and retain its task ID
  ntems_land_cover_gee_tasks <- purrr::map(
    fire_id_list,
    function(fire_id) {
    
      ee_task <- get_lc$sample_ntems_lc_classes(
        sample_pts = ee_sample_points$filter(ee$Filter$eq("fire_id", fire_id)),
        forest_land_cover = ee_forest_land_cover,
        radius_list = ee_radius_list,
        export_filename = paste0("ntems_", fire_id)
      )
      
      return(tibble::tibble(fire_id = fire_id, ee_task_id = ee_task$id))
    }
  ) %>%
    dplyr::bind_rows()
  
  return(ntems_land_cover_gee_tasks)
}