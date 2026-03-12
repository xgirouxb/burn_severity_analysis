impute_forest_variables <- function(forest_variables){

  # -------------------------------------------------------------------------- #
  # Step 1: Consolidate stand age/spp from VRI, CC, HF, CanLaD and RESULTS ####
  
  # Consolidate stand age/spp from VRI, CC, HF, CanLaD and RESULTS
  est_age_forest_variables <- forest_variables %>%
    # Clean-up samples not within rasters ( n = 1)
    dplyr::filter(!is.na(planted_distance)) %>% 
    # Clean-up water land cover class
    dplyr::filter(vri_bclcs_level_4 != "WA", lc_land_cover != "water") %>% 
    dplyr::mutate(
      # Consolidate years since historical fires and CanLaD-detected fires
      years_since_fire_disturbance = dplyr::if_else(
        condition = abs(hf_years_since_fire - canlad_years_since_fire) <= 2,
        true = hf_years_since_fire,
        false = NA_integer_
      ),
      # Compute years since forestry disturbance
      years_since_forestry_disturbance = pmin(
        fd_years_since_harvest,
        fd_years_since_planting,
        na.rm = TRUE
      ),
      # Compute years since disturbance
      years_since_disturbance = pmin(
        years_since_fire_disturbance,
        years_since_forestry_disturbance,
        na.rm = TRUE
      ),
      # Assign stand age as years since most recent disturbance
      # (fire, harvest, planting) or, when undisturbed, the age in VRI + 1
      stand_age = dplyr::coalesce(
        years_since_disturbance,
        (vri_mean_proj_age + 1)
      )
    )
  
  # Sanity check: there should be no stand ages < 2 years old
  if (any(est_age_forest_variables$stand_age < 2, na.rm = TRUE)) {
    stop("Detected stand ages < 2 years old, revise bias sample id criteria.")
  }
  
  # -------------------------------------------------------------------------- #
  # Step 2: Flag and reset outdated VRI attributes ####
  
  # List of VRI treed stand numerical variables of interest (excluding age)
  vri_treed_num_vars <- c(
    "vri_mean_proj_height", "vri_quad_diam_125",
    "vri_live_stand_volume_125", "vri_live_stems_per_ha", "vri_basal_area",
    "vri_total_biomass_per_ha", "vri_crown_closure"
  ) 
  
  # Flag outdated samples, reset variables of interest to NA if outdated
  reset_forest_variables <- est_age_forest_variables %>% 
    # Flag VRI as outdated if projected age is more than a decade older than
    # than stand age derived from VRI and disturbances (RESULTS/CC/HF/CanLaD)
    dplyr::mutate(
      is_outdated = !is.na(stand_age) &
        !is.na(vri_mean_proj_age) &
        (vri_mean_proj_age - stand_age) > 10,
    ) %>%
    # Reset outdated observations (9.38%)
    dplyr::mutate(
      # Reset VRI numerical attributes
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        ~ dplyr::if_else(is_outdated, NA_real_, .)
      ),
      # Reset VRI lead spp
      vri_lead_spp = dplyr::if_else(is_outdated, NA_character_, vri_lead_spp)
    )
  
  # -------------------------------------------------------------------------- #
  # Step 3: Reclassify false NAs and false 0s in VRI treed stand attributes ####
  
  # When this step is complete:
  # - NAs are strictly for missing values that will be imputed downstream
  # - 0s are true values for VRI treed attributes in non-treed or non-veg stands
  #   or placeholders for stand age in non-treed stands 
  # - "none" is placeholder for stand spp in non-treed or non-veg stands
  
  reclass_forest_variables <- reset_forest_variables %>% 
    # ------------------------------------------------------------------------ #
    # 3.1 Create additional flags for downstream reclassification rules
    dplyr::mutate(
      # Flag planted stands
      is_planted = planted_distance == 0,
      # Flag where all VRI treed stand variables are NA or 0
      is_all_na = dplyr::if_all(
        dplyr::all_of(vri_treed_num_vars),
        ~ is.na(.) | . == 0
      ),
      # Flag where VRI treed stand variables have false zeros 
      # (non-zero values in some attributes and zeros in others)
      is_false_zero = dplyr::if_any(dplyr::all_of(vri_treed_num_vars), ~ . == 0) 
        & !dplyr::if_all(dplyr::all_of(vri_treed_num_vars), ~ . == 0),
      # Flag where VRI BCLCS level 4 is non-treed class
      is_non_treed = !(vri_bclcs_level_4 %in% c("TC", "TB", "TM"))
    ) %>% 
    # ------------------------------------------------------------------------ #
    # 3.2 Assign stand species
    dplyr::mutate(
      # If stand is planted, use spp from RESULTS (even if NA), else use VRI
      stand_spp = dplyr::if_else(is_planted, fd_planted_spp, vri_lead_spp)
    ) %>% 
    # ------------------------------------------------------------------------ #
    # 3.3 Replace false 0s that should be missing data
    dplyr::mutate(
      # A 0 is false where other VRI treed attributes that are NOT 0
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        # If false 0s detected and a value is 0, set value to NA to impute
        ~ dplyr::if_else(is_false_zero & . == 0, NA_real_, .)
      )
    ) %>%
    # ------------------------------------------------------------------------ #
    # 3.4 Assign true 0s to non-treed stands where all VRI attributes are NA
    dplyr::mutate(
      # Reclass all VRI treed numerical attributes to true 0s
      # Reclass stand age to placeholder 0
      dplyr::across(
        dplyr::all_of(c(vri_treed_num_vars, "stand_age")),
        ~ dplyr::if_else(!is_planted & is_all_na & is_non_treed, 0, .)
      ),
      # Reclass stand species to none
      stand_spp = dplyr::if_else(
        !is_planted & is_all_na & is_non_treed,
        "none",
        stand_spp
      )
    ) %>% 
    # ------------------------------------------------------------------------ #
    # 3.5 Clean-up
    dplyr::select(-dplyr::starts_with("is_"))
  
  # -------------------------------------------------------------------------- #
  # Step 4: Get spatial coordinates to spatialize imputation ####
  forest_variables_tbl <- reclass_forest_variables %>% 
    dplyr::mutate(
      x = sf::st_coordinates(geometry)[, 1],
      y = sf::st_coordinates(geometry)[, 2]
    ) %>% 
    sf::st_drop_geometry()

  # -------------------------------------------------------------------------- #
  # Step 5: Impute with MICE                                                ####
  
  # List ID variables to exclude from prediction/imputation
  vri_id_vars <- c("fire_id", "fire_year", "id")
  
  # List spatial coordinates
  coords <- c("x", "y")
  
  # List landscape class categorical grouping variable, only for prediction
  lc_class_cat_vars <- c("vri_bclcs_level_4", "lc_land_cover")
  
  # List land cover proportion (within 100m only) variables, only for prediction 
  lc_prop_vars <- c(
    "lc_prop_coniferous_100m", "lc_prop_deciduous_100m",
    "lc_prop_nonfuel_100m", "lc_prop_wetland_100m"
  )
  
  # List distance to managed landscapes, only for prediction
  managed_distance <- c("planted_distance", "harvested_distance")
  
  # List biogeographical categorical variables we want to use for prediction
  biogeo_cat_vars <- c("cvz", "bec", "frt")
  
  # List topographic numerical variables we want to use for prediction
  topo_num_vars <- c(
    "dem", "slope", "pdir",
    "tri_100m", "tpi_100m", "tri_500m", "tpi_500m"
  )
  
  # List VRI categorical variables we want to impute
  spp_cat_vars <- c("stand_spp")
  
  # List VRI treed numerical variables we want to impute
  # NB stand age NAs total only 1480
  vri_treed_num_vars <- c(
    "stand_age", "vri_mean_proj_height", "vri_quad_diam_125",
    "vri_live_stand_volume_125", "vri_live_stems_per_ha", "vri_basal_area",
    "vri_total_biomass_per_ha", "vri_crown_closure"
  )
  
  # Make table for imputation
  imputation_tbl <- forest_variables_tbl %>% 
    # Select columns to include in mice
    dplyr::select(
      dplyr::all_of(
        x = c(
          vri_id_vars,
          coords,
          lc_class_cat_vars,
          lc_prop_vars,
          managed_distance,
          biogeo_cat_vars,
          topo_num_vars,
          spp_cat_vars,
          vri_treed_num_vars
        )
      )
    ) %>% 
    # Assign proper data types for mice
    dplyr::mutate(
      dplyr::across(dplyr::all_of(coords), as.numeric),
      dplyr::across(dplyr::all_of(lc_class_cat_vars), as.factor),
      dplyr::across(dplyr::all_of(lc_prop_vars), as.numeric),
      dplyr::across(dplyr::all_of(managed_distance), as.numeric), 
      dplyr::across(dplyr::all_of(biogeo_cat_vars), as.factor), 
      dplyr::across(dplyr::all_of(topo_num_vars), as.numeric),
      dplyr::across(dplyr::all_of(spp_cat_vars), as.factor),
      dplyr::across(dplyr::all_of(vri_treed_num_vars), as.numeric)
    )
  
  # Create predictor matrix
  mice_pred_mat <- mice::make.predictorMatrix(imputation_tbl)
  
  # Remove ID variables from the predictor matrix (not to be used for imputation)
  mice_pred_mat[, vri_id_vars] <- 0  
  
  # Imputation
  imputed_forest_variables <- mice::mice(
    data = imputation_tbl,
    m = 5,
    method = "rf",
    rfPackage = "literanger",
    predictorMatrix = mice_pred_mat,
    ntree = 10,
    maxit = 50,
    seed = 42,
  )
  
  # Return mice object
  return(imputed_forest_variables)
}