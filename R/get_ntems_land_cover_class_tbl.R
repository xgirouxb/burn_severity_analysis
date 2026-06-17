get_ntems_land_cover_class_tbl <- function(
    sampling_points,
    neighbourhood_radius = c(100, 500, 1000)
){
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment and import modules                             ####
  
  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee$Initialize(project = EARTH_ENGINE_PROJECT_ID)
  
  # Import required module
  get_lc <- reticulate::import_from_path("get_ntems_lc_classes", "py")
  
  # Feature collection of burn sample points
  ee_sample_points <- ee$FeatureCollection(gee_assetid_sample_points)
  
  # Image collection of forest land cover (doi.org/10.1016/j.rse.2021.112780)
  ee_forest_land_cover <- ee$ImageCollection(gee_assetid_land_cover)
  
  # Sanity check: `sampling_points` in GEE assets should have same number
  #                of observations as local `sampling_points` target.
  if (ee_sample_points$size()$getInfo() != nrow(sampling_points)) {
    stop(
      "⚠️ `sampling_points` in GEE assets does not match local copy, upload latest version!"
    )
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Sample land cover classes at burn sample points                 ####
  
  # List of radii to compute land cover class proportions
  ee_radius_list <- ee$List(neighbourhood_radius)
  
  # Get list of unqiue fire_id's in local `sampling_points` target
  fire_id_list <- unique(sampling_points$fire_id)
  
  # Launch task on EE for each fire, return table of fire_ids with EE task ids
  ntems_tasks <- purrr::map(
    fire_id_list,
    function(fire_id) {
      ee_task_id <- get_lc$sample_ntems_lc_classes(
        sample_pts = ee_sample_points$filter(ee$Filter$eq("fire_id", fire_id)),
        forest_land_cover = ee_forest_land_cover,
        radius_list = ee_radius_list,
        export_filename = paste0("ntems_", fire_id)
      )
      return(tibble::tibble(fire_id = fire_id, ee_task_id = ee_task_id$id))
    }
  ) %>% 
    # Bind rows
    dplyr::bind_rows()
  
  # -------------------------------------------------------------------------- #
  # Step 3: Monitor tasks on Earth Engine server ####
  
  # Monitor tasks until all are inactive
  task_status <- monitor_gee_tasks(ee_task_id = ntems_tasks$ee_task_id)
  
  # Join task status to table with fire_id
  ntems_tasks <- dplyr::left_join(ntems_tasks, task_status, by = "ee_task_id")
  
  # Sanity check: all tasks should be COMPLETED
  incomplete_task <- ntems_tasks %>% 
    dplyr::filter(ee_task_status != "COMPLETED")
  if (nrow(incomplete_task) > 0) {
    # ...if there are incomplete tasks, stop and print fire_ids
    cat("\n⚠️ NTEMS land cover sampling failed for some study fires:\n\n")
    print(incomplete_task)
    stop("Targets pipeline interrupted, see failed Earth Engine tasks.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 4: Download land cover sample tables from Drive to local cache ####
  
  # List files in Google Drive project folder
  list_drive_file_names <- googledrive::drive_ls(path = "ee_bc_burn_severity/")
  
  # Get list of matching csv tables
  matched_ntems_tbls <- purrr::map(
    ntems_tasks$fire_id,
    function(fire_id) {
      list_drive_file_names %>% 
        dplyr::filter(name == paste0("ntems_", fire_id, ".csv")) %>%
        # Parse the Drive metadata list to extract the file creation timestamp
        dplyr::mutate(
          fire_id = fire_id,
          timestamp = purrr::map(
            drive_resource,
            function(x) { lubridate::ymd_hms(x$createdTime) }
          )
        ) %>% 
        dplyr::select(fire_id, name, id, timestamp) %>% 
        tidyr::unnest(timestamp) %>% 
        # Get only most recent file if multiple copies
        dplyr::slice_max(order_by = timestamp, n = 1, with_ties = FALSE)
    }
  ) %>% 
    # Bind rows
    dplyr::bind_rows()
  
  # Sanity check: all fire ids should have corresponding CSV on Drive
  missing_ntems_csvs <- setdiff(ntems_tasks$fire_id, matched_ntems_tbls$fire_id)
  if (length(missing_ntems_csvs) > 0) {
    cat("\n⚠️ The following RBR raster files are missing from Google Drive:\n")
    cat(paste0("\t* ntems_", missing_ntems_csvs, ".csv"), sep = "\n")
    cat("\n")
    stop("Pipeline halted: Missing expected NTEMS CSV tables on Google Drive.")
  }
  
  # Create local cache for NTEMS samples
  ntems_cache <- fs::dir_create('data/_cache/ntems_land_cover')
  
  # Download NTEMS land cover sample tables to local cache
  ntems_csv_paths <- matched_ntems_tbls %>% 
    dplyr::group_split(fire_id) %>% 
    purrr::map(
      function(ntems_tbl) {
        googledrive::drive_download(
          file = googledrive::as_id(ntems_tbl$id),
          path = fs::path(ntems_cache, paste0(ntems_tbl$fire_id, ".csv")),
          overwrite = TRUE,
        )
      }
    ) %>% 
    # Bind rows and return only fire_id and path to local cache
    dplyr::bind_rows() %>% 
    dplyr::mutate(fire_id = fs::path_ext_remove(fs::path_file(local_path))) %>% 
    dplyr::select(fire_id, csv_file_path = local_path)
  
  # -------------------------------------------------------------------------- #
  # Step 5: Read all tables and export as target ####
  
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
  ntems_land_cover_class_tbl <- readr::read_csv(
    file = ntems_csv_paths$csv_file_path,
    show_col_types = FALSE
  ) %>% 
    # Reorder columns
    dplyr::select(id, fire_id, fire_year, ntems_land_cover, !!!radius_selectors)
  
  # Return
  return(ntems_land_cover_class_tbl)
}
