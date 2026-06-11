get_burn_severity_rasters <- function(study_fire_sampling_polygons) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Prep environment and import modules                             ####
  
  # Import and initialize Earth Engine API
  ee <- reticulate::import("ee")
  ee$Initialize(project = EARTH_ENGINE_PROJECT_ID)
  
  # Import required modules
  get_rbr <- reticulate::import_from_path("get_rbr_img", "py")
  
  # Feature collection of study fire sampling polygons
  ee_study_fire_sampling_polygons <- ee$FeatureCollection(gee_assetid_study_fire_sampling_polygons)
  
  # Get list of fire_id's in ee asset
  ee_fire_id_list <- ee_study_fire_sampling_polygons$aggregate_array('fire_id')$getInfo()
  
  # Get list of fire_id's in local `study_fire_sampling_polygons` target
  fire_id_list <- unique(study_fire_sampling_polygons$fire_id)
  
  # Sanity check: `study_fire_sampling_polygons` in GEE assets should have same 
  #                fire_id's as those in local target.
  if (!identical(sort(ee_fire_id_list), sort(fire_id_list))) {
    # ...if it does not stop and print out warning
    stop(
      "⚠️`study_fire_sampling_polygons` in GEE assets does not match local copy, upload latest version!",
    )
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Compute RBR images for each study fire sampling polygon         ####
  
  # Launch task on EE for each fire, return table of fire_ids with 
  rbr_tasks <- purrr::map(
    fire_id_list,
    function(fire_id) {
      ee_task_id <- get_rbr$get_rbr_img(
        fire_polygon = ee$Feature(ee_study_fire_sampling_polygons$filter(ee$Filter$eq('fire_id', fire_id))$first())
      )
      return(tibble::tibble(fire_id = fire_id, ee_task_id = ee_task_id$id))
    }
  ) %>% 
    # Bind rows
    dplyr::bind_rows()
  
  # -------------------------------------------------------------------------- #
  # Step 3: Monitor task on Earth Engine server                             ####
  
  # Monitor tasks until all are inactive
  task_status <- monitor_gee_tasks(ee_task_id = rbr_tasks$ee_task_id)
  
  # Join task status to table with fire_id
  rbr_tasks <- dplyr::left_join(rbr_tasks, task_status, by = "ee_task_id")
  
  # Sanity check: all tasks should be COMPLETED
  incomplete_task <- rbr_tasks %>% 
    dplyr::filter(ee_task_status != "COMPLETED")
  if (nrow(incomplete_task) > 0) {
    # ...if there are incomplete tasks, stop and print fire_ids
    cat("\n⚠️ RBR computation failed for some study fires:\n\n")
    print(incomplete_task)
    stop("Targets pipeline interupted, see failed Earth Engine tasks.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 4: Download RBR images from google drive to local cache            ####
  
  # List files in Google Drive project folder
  list_drive_file_names <- googledrive::drive_ls(path = "ee_bc_burn_severity/")
  
  # Get list of matching RBR raster file names
  matched_rbr_img_names <- purrr::map(
    rbr_tasks$fire_id,
    function(fire_id) {
      list_drive_file_names %>% 
        dplyr::filter(name == paste0("rbr_", fire_id, ".tif")) %>%
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
  
  # Sanity check: all fire ids should have corresponding RBR image on Drive
  missing_rbr_imgs <- setdiff(rbr_tasks$fire_id, matched_rbr_img_names$fire_id)
  if (length(missing_rbr_imgs) > 0) {
    cat("\n⚠️ The following RBR raster files are missing from Google Drive:\n")
    cat(paste0("\t* rbr_", missing_rbr_imgs, ".tif"), sep = "\n")
    cat("\n")
    stop("Pipeline halted: Missing expected RBR images on Google Drive.")
  }
  
  # Create local cache for RBR imgs
  rbr_cache <- fs::dir_create("data/_cache/rbr")
  
  # Download RBR images to local cache
  rbr_raster_paths <- matched_rbr_img_names %>% 
    dplyr::group_split(fire_id) %>% 
    purrr::map(
      function(rbr_img) {
        googledrive::drive_download(
          file = rbr_img$name,
          path = fs::path(rbr_cache, paste0(rbr_img$fire_id, ".tif")),
          overwrite = TRUE,
        )
      }
    ) %>% 
    # Bind rows and return only fire_id and path to local cache
    dplyr::bind_rows() %>% 
    dplyr::mutate(fire_id = fs::path_ext_remove(fs::path_file(local_path))) %>% 
    dplyr::select(fire_id, raster_file_path = local_path)
  
  # Return list of RBR raster file paths
  return(rbr_raster_paths)
}