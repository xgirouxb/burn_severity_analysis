get_ntems_land_cover_class_tbl <- function(ntems_land_cover_gee_tasks) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment                                                ####
  
  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee$Initialize(project = EARTH_ENGINE_PROJECT_ID)
  
  # -------------------------------------------------------------------------- #
  # Step 2: Monitor tasks on Earth Engine server                            ####
  
  # Monitor tasks until all are inactive
  task_status <- monitor_gee_tasks(
    ee_task_id = ntems_land_cover_gee_tasks$ee_task_id
  )
  
  # Join task status to table with fire_id
  ntems_tasks <- ntems_land_cover_gee_tasks %>%
    dplyr::left_join(task_status, by = "ee_task_id")
  
  # Sanity check: all tasks should be COMPLETED
  incomplete_tasks <- dplyr::filter(ntems_tasks, ee_task_status != "COMPLETED")
  
  # If there are incomplete tasks, stop and print fire_ids
  if (nrow(incomplete_tasks) > 0) {
    cat("\n⚠️ NTEMS land cover sampling failed for some study fires:\n\n")
    print(incomplete_tasks)
    stop("Targets pipeline interrupted, see failed Earth Engine tasks.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 3: Match exported tables on Google Drive                           ####
  
  # List files in Google Drive project folder
  list_drive_file_names <- googledrive::drive_ls(path = "ee_bc_burn_severity/")
  
  # Get matching CSV table for each fire
  matched_ntems_tbls <- purrr::map(
    ntems_tasks$fire_id,
    function(fire_id) {
      list_drive_file_names %>%
        dplyr::filter(name == paste0("ntems_", fire_id, ".csv")) %>%
        # Parse Drive metadata to extract file creation timestamp
        dplyr::mutate(
          fire_id = fire_id,
          timestamp = purrr::map(
            drive_resource,
            function(x) { lubridate::ymd_hms(x$createdTime) }
          )
        ) %>%
        dplyr::select(fire_id, name, id, timestamp) %>%
        tidyr::unnest(timestamp) %>%
        # Get only most recent file if multiple copies exist
        dplyr::slice_max(order_by = timestamp, n = 1, with_ties = FALSE)
    }
  ) %>%
    dplyr::bind_rows()
  
  # Sanity check: all fire ids should have corresponding CSV on Drive
  missing_ntems_csvs <- setdiff(ntems_tasks$fire_id, matched_ntems_tbls$fire_id)
  
  # If there are missing CSVs, stop and print
  if (length(missing_ntems_csvs) > 0) {
    cat("\n⚠️ The following NTEMS CSV files are missing from Google Drive:\n")
    cat(paste0("\t* ntems_", missing_ntems_csvs, ".csv"), sep = "\n")
    cat("\n")
    stop("Pipeline halted: Missing expected NTEMS CSV tables on Google Drive.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 4: Download land cover sample tables                               ####
  
  # Create local cache for NTEMS samples
  ntems_cache <- fs::dir_create("data/_cache/ntems_land_cover")
  
  # Download NTEMS land cover sample tables to local cache
  ntems_csv_paths <- matched_ntems_tbls %>%
    dplyr::group_split(fire_id) %>%
    purrr::map(
      function(ntems_tbl) {
        googledrive::drive_download(
          file = googledrive::as_id(ntems_tbl$id),
          path = fs::path(ntems_cache, paste0(ntems_tbl$fire_id, ".csv")),
          overwrite = TRUE
        )
      }
    ) %>%
    # Bind rows and return only fire_id and path to local cache
    dplyr::bind_rows() %>%
    dplyr::mutate(fire_id = fs::path_ext_remove(fs::path_file(local_path))) %>%
    dplyr::select(fire_id, csv_file_path = local_path)
  
  # -------------------------------------------------------------------------- #
  # Step 5: Read and combine land cover sample tables                       ####
  
  # Read all land cover samples
  ntems_land_cover_class_tbl <- readr::read_csv(
    file = ntems_csv_paths$csv_file_path,
    show_col_types = FALSE
  ) %>%
    # Keep identifiers, NTEMS land cover class, and all neighbourhood
    # land cover proportion variables regardless of sampled radius
    dplyr::select(
      id, fire_id, fire_year, ntems_land_cover, 
      dplyr::starts_with("ntems_prop_")
    )
  
  # Return
  return(ntems_land_cover_class_tbl)
}