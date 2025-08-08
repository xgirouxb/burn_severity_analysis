get_land_cover_class_tbl <- function(
    burn_sample_points,
    neighbourhood_radius = c(100, 500, 1000)
){
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment and import modules                             ####

  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee_credentials <- ee$ServiceAccountCredentials(
    email = GEE_SERVICE_ACCOUNT,
    key_file = GOOGLE_API_KEY
  )
  ee$Initialize(ee_credentials)

  # Import required modules
  get_lc <- reticulate::import_from_path("get_land_cover_classes", "py")
  ee_task <- reticulate::import_from_path("monitor_ee_task", "py")

  # Feature collection of burn sample points
  ee_burn_sample_points <- ee$FeatureCollection(
    'projects/ee-bc-burn-severity/assets/burn_sample_points'
  )

  # Sanity check: `burn_sample_points` in GEE assets should have same number of
  #                sample points as local `burn_sample_points` target.
  if (ee_burn_sample_points$size()$getInfo() != nrow(burn_sample_points)) {
    # ...if it does not stop and print error
    stop(
      "`burn_sample points` in GEE assets does not match local copy, upload latest version!",
    )
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Sample land cover classes at burn sample points                 ####
  
  # List of radii to compute land cover class proportions
  ee_radius_list <- ee$List(neighbourhood_radius)
  
  # Launch task on EE
  sample_task <- get_lc$sample_lc_classes(
    sample_pts = ee_burn_sample_points,
    radius_list = ee_radius_list
  )
  
  # -------------------------------------------------------------------------- #
  # Step 3: Monitor task on Earth Engine server                             ####
  
  # Monitor task until it is complete
  task_status <- ee_task$monitor_gee_task(sample_task)
  
  # Sanity check: If task did not complete successfully...
  if (task_status != "Task completed successfully.") {
    # ...stop and print error
    stop(task_status)
  }
  
  # -------------------------------------------------------------------------- #
  # Step 4: Get output table from google drive                              ####
  
  # Authenticate with service account JSON key
  googledrive::drive_auth(path = GOOGLE_API_KEY)
  
  # Download the file to local _cache
  googledrive::drive_download(
    file = "ee_bc_burn_severity/landcover_proportion_samples.csv",
    path = "data/_cache/landcover/landcover_proportion_samples.csv",
    overwrite = TRUE
  )
  
  # Create column selectors to organize columns by neighbourhood size
  radius_selectors <- purrr::map(
    radii_list,
    ~ rlang::expr(dplyr::ends_with(!!paste0(.x, "m")))
  )
  
  # Read in landcover samples
  land_cover_classes <- readr::read_csv(
    file = "data/_cache/landcover/landcover_proportion_samples.csv"
  ) %>% 
    # Reorder columns
    dplyr::select(id, fire_id, fire_year, landcover, !!!radius_selectors)
  
  # Return
  return(land_cover_classes)
}
