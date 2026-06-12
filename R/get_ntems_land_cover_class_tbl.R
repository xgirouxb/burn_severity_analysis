get_ntems_land_cover_class_tbl <- function(
    study_sampling_points,
    neighbourhood_radius = c(100, 500, 1000)
){
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment and import modules                             ####

  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee$Initialize(project = EARTH_ENGINE_PROJECT_ID)

  # Import required module
  get_lc <- reticulate::import_from_path("get_ntems_land_cover_classes", "py")

  # Feature collection of burn sample points
  ee_burn_sample_points <- ee$FeatureCollection(gee_assetid_sample_points)
  
  # Image collection of forest land cover (doi.org/10.1016/j.rse.2021.112780)
  ee_forest_land_cover <- ee$ImageCollection(gee_assetid_land_cover)

  # Sanity check: `study_sampling_points` in GEE assets should have same number 
  #                of sample points as local `study_sampling_points` target.
  if (ee_burn_sample_points$size()$getInfo() != nrow(study_sampling_points)) {
    # ...if it does not stop and print error
    stop(
      "⚠️`study_sampling_points` in GEE assets does not match local copy, upload latest version!",
    )
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Sample land cover classes at burn sample points                 ####
  
  # List of radii to compute land cover class proportions
  ee_radius_list <- ee$List(neighbourhood_radius)
  
  # Launch task on EE
  sample_task <- get_lc$sample_ntems_lc_classes(
    sample_pts = ee_burn_sample_points,
    forest_land_cover = ee_forest_land_cover,
    radius_list = ee_radius_list
  )
  
  # -------------------------------------------------------------------------- #
  # Step 3: Monitor task on Earth Engine server                             ####
  
  # Monitor task until it is inactive
  task_status <- monitor_gee_tasks(sample_task$id, check_interval_minutes = 5)
  
  # Sanity check: If task did not complete successfully...
  if (task_status$ee_task_status != "COMPLETED") {
    # ...stop and print error
    stop("⚠️ NTEMS sampling failed, see failed Earth Engine tasks.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 4: Get output table from google drive                              ####
  
  # List all files in the folder that match the name pattern
  matched_files <- googledrive::drive_ls(path = "ee_bc_burn_severity/") %>%
    dplyr::filter(name == "ntems_land_cover_proportion_samples.csv") %>%
    # Parse `drive_resource` list to extract file timestamp
    dplyr::mutate(created_time = purrr::map_chr(drive_resource, ~ .x$createdTime)) %>%
    # Sort by most recent to oldest file
    dplyr::arrange(dplyr::desc(created_time))
  
  # Sanity check: Was a matching file found?
  if (nrow(matched_files) == 0) {
    stop("⚠️ No file named 'ntems_land_cover_proportion_samples.csv' found in the folder.")
  }
  
  # Create local cache for NTEMS samples
  ntems_cache <- fs::path('data/_cache/ntems_land_cover')
  fs::dir_create(ntems_cache)
  
  # Download the most recent matching file
  googledrive::drive_download(
    file = dplyr::slice(matched_files, 1),
    path = fs::path(ntems_cache, "ntems_land_cover_proportion_samples.csv"),
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
  ntems_land_cover_classes <- readr::read_csv(
    file = fs::path(ntems_cache, "ntems_land_cover_proportion_samples.csv"),
    show_col_types = FALSE
  ) %>% 
    # Reorder columns
    dplyr::select(id, fire_id, fire_year, ntems_land_cover, !!!radius_selectors)
  
  # Return
  return(ntems_land_cover_classes)
}
