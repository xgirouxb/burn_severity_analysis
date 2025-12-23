prep_forestry_disturbance_rasters <- function(
    study_fire_sampling_polygons,
    cutblock_polygons,
    historical_fire_polygons,
    results_polygons
) {
  
  # Define local cache directory for output rasters, create if it doesn't exist
  disturbance_cache <- fs::path("data/_cache/forestry_disturbance")
  if (!fs::dir_exists(disturbance_cache)) { fs::dir_create(disturbance_cache) }
  
  # Create list of forestry disturbance raster file paths for each study fire
  forestry_disturbance_paths <- study_fire_sampling_polygons %>%
    # Split by fire id
    dplyr::group_split(fire_id) %>%
    # Create list of forestry disturbance rasters written to cache
    purrr::map(
      function(study_fire) {
        
        # -------------------------------------------------------------------- #
        # Step 0: Setup ####
        
        # Define maximum of 10 km for distance/neighbourhood functions
        max_dist <- 10000
        
        # Get study fire attributes for clearer syntax below
        aoi_buf <- terra::buffer(x = terra::vect(study_fire), width = max_dist)
        
        # Create template raster at 30-m resolution 
        template_raster <- terra::rast(aoi_buf, resolution = 30, vals = 1) %>% 
          # Mask to buffered study fire area
          terra::crop(aoi_buf, mask = TRUE)
        
        # -------------------------------------------------------------------- #
        # Step 1: Subset disturbance datasets ####
        
        # Import consolidated cutblocks that intersect/predate study fire buffer
        cc_sub <- sf::st_filter(cutblock_polygons, study_fire) %>% 
          dplyr::filter(cc_harvest_start_year < study_fire$fire_year) %>%
          # Select variables of interest
          dplyr::select(cc_harvest_start_year, cc_harvest_end_year)
        
        # Import historical fires that intersect/predate study fire buffer
        hf_sub <- sf::st_filter(historical_fire_polygons, study_fire) %>% 
          dplyr::filter(hf_fire_year < study_fire$fire_year) %>%
          # Select variables of interest
          dplyr::select(hf_fire_year)
        
        # Import RESULTS openings/plantations for study fire
        res_sub <- results_polygons %>% 
          dplyr::filter(fire_id == study_fire$fire_id) %>%
          # Select variables of interest
          dplyr::select(
            res_harvest_start_year, res_harvest_end_year, res_fire_year,
            res_planting_year, res_lead_spp
          ) %>% 
          # Only retain years that predate fire
          dplyr::mutate(
            dplyr::across(
              dplyr::ends_with("_year"),
              ~ dplyr::if_else(.x < study_fire$fire_year, .x, NA_real_)
            )
          ) %>% 
          # Retain rows with disturbances
          dplyr::filter(!dplyr::if_all(dplyr::ends_with("_year"), is.na))
        
        # -------------------------------------------------------------------- #
        # Step 2: Convert harvested, burned, and planted areas to raster ####
        
        ## 2.1 Rasterize most recent harvest year ####
        
        # Get polygons of harvested areas
        harvested_polygons <- dplyr::bind_rows(cc_sub, res_sub) %>% 
          # Get most recent harvest year
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
          # Create raster of most recent harvest years
          harvested_raster <- terra::rasterize(
            x = terra::vect(harvested_polygons),
            y = template_raster,
            field = "harvest_year",
            fun = "max",
            touches = TRUE
          )
        }
        
        ## 2.2 Rasterize most recent burn year ####
        
        # Get burned polygons
        burned_polygons <- hf_sub %>% 
          # Remove rows with no historical fire
          dplyr::filter(!is.na(hf_fire_year))
        
        # If burned polygons are absent in study area
        if (nrow(burned_polygons) == 0) {
          # Create empty raster 
          burned_raster <- terra::subst(template_raster, 1, NA)
          # If burned polygons are present in study area  
        } else {
          # Create raster of most recent burn year
          burned_raster <- terra::rasterize(
            x = terra::vect(burned_polygons),
            y = template_raster,
            field = "hf_fire_year",
            fun = "max",
            touches = TRUE
          )
        }
        
        ## 2.3 Rasterize most recent planting year ####
        
        # Get planted polygons
        planted_polygons <- dplyr::filter(res_sub, !is.na(res_planting_year))
        
        # If planted polygons are absent in study area
        if (nrow(planted_polygons) == 0) {
          # Create empty raster 
          planted_raster <- terra::subst(template_raster, 1, NA)
          # If planted polygons are present in study area  
        } else {
          # Create raster of most recent planting year
          planted_raster <- terra::rasterize(
            x = terra::vect(planted_polygons),
            y = template_raster,
            field = "res_planting_year",
            fun = "max",
            touches = TRUE
          )
        }
        
        ## 2.4 Rasterize leading planted species ####
        
        # List of species and associated codes for raster attribute table
        spp_rat <- tibble::tibble(
          value = 1:7,
          # Extra levels included here for consistency with VRI lead spp
          res_lead_spp = c("douglas-fir", "fir", "other_coniferous",
                           "other_deciduous", "pine", "poplar", "spruce")
        ) 
        
        # Get polygons of planted species
        planted_spp_polygons <- planted_polygons %>%
          # Retain plantings with recorded species
          dplyr::filter(!is.na(res_lead_spp)) %>% 
          # Arrange by planting year (most recent at bottom)
          dplyr::arrange(res_planting_year) %>% 
          # Add species code
          dplyr::left_join(y = spp_rat, by = "res_lead_spp")
        
        # If planted species polygons are absent in study area
        if (nrow(planted_spp_polygons) == 0) {
          # Create empty raster 
          planted_spp_raster <- terra::subst(template_raster, 1, NA)
          # If planted species polygons are present in study area  
        } else {
          # Create raster of most recently planted species
          planted_spp_raster <- terra::rasterize(
            x = terra::vect(planted_spp_polygons),
            y = template_raster,
            field = "value",
            touches = TRUE
          )
        }
        
        # Add raster attribute table
        levels(planted_spp_raster) <- spp_rat
        
        # -------------------------------------------------------------------- #
        # Step 3: Update harvested and planted areas ####
        
        # Update harvest where a plantation or fire follows
        updated_harvested_raster <- terra::ifel(
          # Mask where fire occurred after harvest OR 
          test = (harvested_raster < burned_raster) | 
            # Mask where planting occurred after harvest
            (harvested_raster < (planted_raster + 5)),
          yes = NA,
          no = harvested_raster
        )
        names(updated_harvested_raster) <- "harvest_year"
        
        # Update planted areas if most recent disturbance is fire or harvest
        updated_planted_raster <- terra::ifel(
          # Mask where fire occurred after planting OR 
          test = (planted_raster < burned_raster) | 
            # Mask where harvest occurred more than 5 years after planting
            (harvested_raster > (planted_raster + 5)),
          yes = NA,
          no = planted_raster
        )
        names(updated_planted_raster) <- "res_planting_year"
        
        # Update planted species if most recent disturbance is fire or harvest
        updated_planted_spp_raster <- terra::ifel(
          # Mask where fire occurred after planting OR 
          test = (planted_raster < burned_raster) | 
            # Mask where harvest occurred more than 5 years after planting
            (harvested_raster > (planted_raster + 5)),
          yes = NA,
          no = planted_spp_raster
        )
        names(updated_planted_spp_raster) <- "res_planted_spp"
        
        # -------------------------------------------------------------------- #
        # Step 4: Compute distance to managed/planted/harvested areas ####
        
        # Helper function: test if raster is empty (all NA values)
        is_raster_empty <- function(r) { 
          # Return TRUE or FALSE
          terra::global(r, fun = "notNA") == 0 
        }
        
        ## 4.1 Compute distance to managed forest stand ####
        
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
        
        # -------------------------------------------------------------------- #
        # Step 5: Compute proportion of managed forests in landscape
        
        # Create managed forest presence(1)/absence(0) raster
        managed_raster01 <- terra::subst(managed_raster, NA, 0)
        
        # Landscape neighbourhood sizes (in metres)
        neighbourhood_radii <- c(100, 500, 1000)
        
        # Compute percentage of managed forests in landscape neighbourhoods 
        pct_managed_raster <- purrr::map(
          neighbourhood_radii,
          function(radius) {
            
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
        
        # Combine year, distance, and percent layers into SpatRast
        forestry_disturbance_raster <- c(
          # Years
          updated_harvested_raster, updated_planted_raster,
          updated_planted_spp_raster,
          # Distance to 
          managed_distance, planted_distance, harvested_distance,
          # Percent of neighbourhood
          pct_managed_raster
        )  %>% 
          # Mask to study fire area
          terra::crop(y = terra::vect(study_fire), mask = TRUE)
        
        # File path for writing raster to file
        raster_file_path <- fs::path(
          disturbance_cache,
          paste0(study_fire$fire_id, ".tif")
        )
        
        # Write forestry disturbance years to file  
        terra::writeRaster(
          x = forestry_disturbance_raster, 
          filename = raster_file_path,
          datatype = "INT2U",
          overwrite = TRUE
        )
        
        # Return tbl of file path
        tibble::tibble(
          fire_id = study_fire$fire_id,
          raster_file_path = raster_file_path
        )
        
      }
    ) %>%
    # Combine
    dplyr::bind_rows()
  
  # Return list of forestry disturbance raster file paths
  return(forestry_disturbance_paths)
}