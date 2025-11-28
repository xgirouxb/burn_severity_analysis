prep_forest_management_rasters <- function(
    study_fire_sampling_polygons,
    cutblock_polygons,
    historical_fire_polygons,
    results_polygons
) {
  
  # Define local cache directory for output rasters, create if it doesn't exist
  management_cache <- fs::path("data/_cache/forest_management")
  if (!fs::dir_exists(management_cache)) { fs::dir_create(management_cache) }
  
  # Create list of forest management raster files for each study fire
  forest_management_files <- study_fire_sampling_polygons %>%
    # Nest by fire id
    dplyr::group_nest(fire_id) %>%
    # Create forest management rasters
    dplyr::mutate(
      rast_path = purrr::map2(
        .x = data,
        .y = fire_id,
        .f = ~ {
          
          # ------------------------------------------------------------------ #
          # Step 0: Setup ####
          
          # Define maximum of 10 km for distance/neighbourhood functions
          max_dist <- 10000
          
          # Get study fire variables
          study_fire <- .x
          study_fire_buffer <- sf::st_buffer(.x, max_dist)
          study_fire_year <- .x$fire_year

          # Create template raster at 30-m resolution 
          template_raster <- terra::rast(
            # Use 10-km buffer for distance/neighbourhood functions
            x = terra::vect(study_fire_buffer),
            resolution = 30,
            vals = 1
          ) %>% 
            # Mask to buffered study fire area
            terra::crop(y = terra::vect(study_fire_buffer), mask = TRUE)
          
          # ------------------------------------------------------------------ #
          # Step 1: Subset disturbance datasets ####
          
          # Import consolidated cutblocks that intersect/predate study fire
          cc_sub <- cutblock_polygons %>% 
            # Use buffered study fire area         
            sf::st_filter(study_fire_buffer) %>%                
            dplyr::filter(cc_harvest_start_year < study_fire_year) %>%
            # Select variables of interest
            dplyr::select(cc_harvest_start_year, cc_harvest_end_year)
          
          # Import historical fire polygons that intersect/predate study fire
          hf_sub <- historical_fire_polygons %>% 
            # Use buffered study fire area
            sf::st_filter(study_fire_buffer) %>%
            dplyr::filter(hf_fire_year < study_fire_year) %>%
            # Select variables of interest
            dplyr::select(hf_fire_year)
          
          # Import RESULTS openings/plantations for study fire
          res_sub <- results_polygons %>% 
            dplyr::filter(fire_id == .y) %>%
            # Select variables of interest
            dplyr::select(
              res_harvest_start_year, res_harvest_end_year, res_fire_year,
              res_planting_year, res_lead_spp
            ) %>% 
            # Only retain years that predate fire
            dplyr::mutate(
              dplyr::across(
                dplyr::ends_with("_year"),
                ~ dplyr::if_else(.x < study_fire_year, .x, NA_real_)
              )
            ) %>% 
            # Retain rows with disturbances
            dplyr::filter(!dplyr::if_all(dplyr::ends_with("_year"), is.na))
          
          # ------------------------------------------------------------------ #
          # Step 2: Convert harvested, burned, and planted areas to raster ####
          
          ## 2.1 Rasterize harvested areas ####
          
          # Get polygons of harvested areas
          harvested_polygons <- dplyr::bind_rows(cc_sub, res_sub) %>% 
            # Get most recent harvest disturbance year
            dplyr::mutate(
              harvest_year = pmax(
                cc_harvest_start_year, cc_harvest_end_year, 
                res_harvest_start_year, res_harvest_end_year,
                na.rm = TRUE
              )
            ) %>% 
            # Retain rows with harvest year
            dplyr::filter(!is.na(harvest_year))
          
          # If harvested polygons are absent in study area
          if (nrow(harvested_polygons) == 0) {
            # Create empty raster 
            harvested_raster <- terra::subst(template_raster, 1, NA)
            # If harvested polygons are present in study area  
          } else {
            # Create raster of most recently harvested areas
            harvested_raster <- terra::rasterize(
              x = harvested_polygons,
              y = template_raster,
              field = "harvest_year",
              fun = "max"
            )
          }
        
          ## 2.2 Rasterize burned areas ####
          
          # Get polygons of historical fires
          burned_polygons <- dplyr::bind_rows(hf_sub, res_sub) %>% 
            # Get most recent fire disturbance year
            dplyr::mutate(
              hist_fire_year = pmax(hf_fire_year, res_fire_year, na.rm = TRUE)
            ) %>% 
            # Remove rows with no harvest
            dplyr::filter(!is.na(hist_fire_year))
          
          # If burned polygons are absent in study area
          if (nrow(burned_polygons) == 0) {
            # Create empty raster 
            burned_raster <- terra::subst(template_raster, 1, NA)
          # If burned polygons are present in study area  
          } else {
            # Create raster of most recently burned areas
            burned_raster <- terra::rasterize(
              x = burned_polygons,
              y = template_raster,
              field = "hist_fire_year",
              fun = "max"
            )
          }
          
          ## 2.3 Rasterize planted areas ####
          
          # Get polygons of plantations
          planted_polygons <- dplyr::filter(res_sub, !is.na(res_planting_year))
          
          # If planted polygons are absent in study area
          if (nrow(planted_polygons) == 0) {
            # Create empty raster 
            planted_raster <- terra::subst(template_raster, 1, NA)
          # If planted polygons are present in study area  
          } else {
            # Create raster of most recently planted areas
            planted_raster <- terra::rasterize(
              x = planted_polygons,
              y = template_raster,
              field = "res_planting_year",
              fun = "max"
            )
          }
          
          # ------------------------------------------------------------------ #
          # Step 3: Update harvested and planted areas ####
          
          # Update planted areas if most recent disturbance is fire or harvest
          updated_planted_raster <- terra::ifel(
            # Mask where fire occurred after planting OR 
            test = (planted_raster < burned_raster) | 
              # Mask where harvest occurred more than 5 years after planting
              (harvested_raster > (planted_raster + 5)),
            yes = NA,
            no = planted_raster
          )
          
          # Update harvest where a plantation or fire follows
          updated_harvested_raster <- terra::ifel(
            # Mask where fire occurred after harvest OR 
            test = (harvested_raster < burned_raster) | 
              # Mask where planting occurred after harvest
              (harvested_raster < (planted_raster + 5)),
            yes = NA,
            no = harvested_raster
          )
          
          # ------------------------------------------------------------------ #
          # Step 4: Compute distance to managed/planted/harvested areas ####
          
          # Helper function: test if raster is empty (all NA values)
          is_raster_empty <- function(r) { 
            # Return TRUE or FALSE
            terra::global(r, fun = "notNA") == 0 
          }
          
          ## 4.1 Compute distance to managed forests ####
          
          # Create managed forest by combining planted and harvested areas
          managed_raster <- terra::ifel(
            test = is.na(updated_planted_raster) & 
              is.na(updated_harvested_raster),
            yes = NA,
            no = 1
          )
          
          # If managed forest raster is empty (all values are NA) ...
          if (is_raster_empty(managed_raster)) {
            # ... create raster of max distance.
            managed_distance <- terra::subst(template_raster, 1, max_dist)
          } else {
            # Else, compute distance (in metres) to managed forest
            managed_distance <- terra::distance(managed_raster) %>% 
              # Clamp to max distance
              terra::clamp(upper = max_dist)
          }
          # Rename layer
          names(managed_distance) <- "managed_distance"
          
          ## 4.2 Compute distance to planted forests ####
          
          # If updated planted raster is empty (all values are NA)
          if (is_raster_empty(updated_planted_raster)) {
            # Create raster of max distance
            planted_distance <- terra::subst(template_raster, 1, max_dist)
          } else {
            # Else, compute distance to planted areas
            planted_distance <- terra::distance(updated_planted_raster) %>% 
              # Clamp to max distance
              terra::clamp(upper = max_dist)
          }
          # Rename layer
          names(planted_distance) <- "planted_distance"
          
          ## 4.3 Compute distance to harvested forests ####
          
          # If updated harvested raster is empty (all values are NA)
          if (is_raster_empty(updated_harvested_raster)) {
            # Create raster of max distance
            harvested_distance <- terra::subst(template_raster, 1, max_dist)
          } else {
            # Else, compute distance to harvested areas
            harvested_distance <- terra::distance(updated_harvested_raster) %>% 
              # Clamp to max distance
              terra::clamp(upper = max_dist)
          }
          # Rename layer
          names(harvested_distance) <- "harvested_distance"
          
          # ------------------------------------------------------------------ #
          # Step 5: Compute proportion of managed forests in landscape
          
          # Create managed forest presence(1)/absence(0) raster
          managed_raster01 <- terra::subst(managed_raster, NA, 0)
          
          # Landscape neighbourhood sizes (in metres)
          neighbourhood_radii <- c(100, 500, 1000)
          
          # Compute percentage of managed forests in landscape neighbourhoods 
          pct_managed_raster <- purrr::map(
            .x = neighbourhood_radii,
            .f = function(radius) {
              
              # Define a circular neighbourhood window with supplied radius
              neighbourhood_window <- terra::focalMat(
                x = managed_raster01,
                d = radius,
                type = "circle"
              )
              
              # Compute percentage of managed forests, rename layer
              # NB: percentage = mean(presence/absence)*100
              pct_managed <- terra::focal(
                x = managed_raster01, 
                w = neighbourhood_window, 
                fun = "mean",
                na.rm = TRUE
              ) * 100
              names(pct_managed) <- paste0("pct_managed_", radius, "m")
              
              # Return
              return(pct_managed)
            }
          ) %>% 
            # Combine into single SpatRast
            terra::rast()
          
          # ------------------------------------------------------------------ #
          # Step 6: Combine layers, write raster to cache 
          
          # Combine distance and proportion layers into SpatRast
          output_rast <- c(
            managed_distance, planted_distance,
            harvested_distance, pct_managed_raster
          )  %>% 
            # Mask to study fire area
            terra::crop(y = terra::vect(study_fire), mask = TRUE)
          
          # File name and path for writing raster to file
          raster_file_name <- fs::path(management_cache, paste0(.y, ".tif"))
          
          # Write fire and harvest disturbance years to file  
          terra::writeRaster(
            x = output_rast, 
            filename = raster_file_name,
            datatype = "INT2U",
            overwrite = TRUE
          )
          
          # Return file name
          raster_file_name
        }
      )
    ) %>%
    # Clean-up
    dplyr::select(-data)
  
  # Return list of forest management raster file names
  return(forest_management_files)
}