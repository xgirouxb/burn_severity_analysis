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
  ee_burn_sample_points <- ee$FeatureCollection(gee_assetid_land_cover)

  # Sanity check: `burn_sample_points` in GEE assets should have same number of
  #                sample points as local `burn_sample_points` target.
  if (ee_burn_sample_points$size()$getInfo() != nrow(burn_sample_points)) {
    # ...if it does not stop and print error
    stop(
      "`burn_sample_points` in GEE assets does not match local copy, upload latest version!",
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
  
  # List all files in the folder that match the name pattern
  matched_files <- googledrive::drive_ls(path = "ee_bc_burn_severity/") %>%
    dplyr::filter(name == "land_cover_proportion_samples.csv") %>%
    # Parse `drive_resource` list to extract file timestamp
    dplyr::mutate(created_time = purrr::map_chr(drive_resource, ~ .x$createdTime)) %>%
    # Sort by most recent to oldest file
    dplyr::arrange(dplyr::desc(created_time))
  
  # Sanity check: Was a file was found?
  if (nrow(matched_files) == 0) {
    stop("No file named 'land_cover_proportion_samples.csv' found in the folder.")
  }
  
  # Download the most recent matching file
  googledrive::drive_download(
    file = dplyr::slice(matched_files, 1),
    path = "data/_cache/land_cover/land_cover_proportion_samples.csv",
    overwrite = TRUE,
  )
  
  # Create column selectors to organize columns by neighbourhood size
  radius_selectors <- purrr::map(
    # Build strings for all lc variables
    tidyr::crossing(
      type = c("coniferous", "deciduous", "wetland", "nonfuel"),
      radius = neighbourhood_radius
    ) %>% 
      dplyr::arrange(radius) %>% 
      dplyr::mutate(lc_var_name = paste0(type, "_", radius, "m")) %>% 
      dplyr::pull(lc_var_name),
    # Transform them into a <tidy-select> expression
    ~ rlang::expr(dplyr::ends_with(!!.x))
  )
  
  # Read in land cover samples
  land_cover_classes <- readr::read_csv(
    file = "data/_cache/land_cover/land_cover_proportion_samples.csv",
    show_col_types = FALSE
  ) %>% 
    # Reorder columns
    dplyr::select(id, fire_id, fire_year, lc_land_cover, !!!radius_selectors)
  
  # Return
  return(land_cover_classes)
}
