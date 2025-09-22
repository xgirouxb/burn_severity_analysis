impute_forest_variables <- function(
    forest_variables
){

  # -------------------------------------------------------------------------- #
  # Step 1: Consolidate stand age from VRI, CC, HF, CanLaD and RESULTS      ####
  
 est_age_forest_variables <- forest_variables %>%
  dplyr::mutate(
    # Consolidate years since historical fires and CanLaD-detected fires
    years_since_fire = dplyr::if_else(
      condition = abs(hf_years_since_fire - canlad_years_since_fire) <= 2,
      true = hf_years_since_fire,
      false = NA_integer_
    ),
    # Compute years since disturbance
    years_since_disturbance = dplyr::coalesce(
      # First prioritize years since harvest or since fire
      pmin(cc_years_since_harvest, years_since_fire, na.rm = TRUE),
      # If that is NA then use age from RESULTS sylvicultural database
      res_age
    ),
    # The age parsed from RESULTS records are a bit rougher than CC
    years_since_disturbance = dplyr::if_else(
      # If res_age suggests stand age is > 10 years younger that CC
      condition = !is.na(res_age) & 
        (years_since_disturbance - res_age) > 10 &
        # AND if CC and VRI disagree on stand age
        abs(years_since_disturbance - vri_min_proj_age) > 5,
      # Use res_age instead
      true = res_age,
      false = years_since_disturbance
    ), 
    # Finally, if there are no disturbances, use the age listed in VRI + 1,
    # (for stand age the year of study fire instead of VRI archive year)
    stand_age = dplyr::coalesce(
      years_since_disturbance,
      (vri_mean_proj_age + 1)
    )
  ) %>% 
  # Remove samples (n = 2489) where stand age is 1 year or younger, 
  # disturbances may bias burn ratio estimation when image differencing
  dplyr::filter(stand_age > 1 | is.na(stand_age))
  
  # -------------------------------------------------------------------------- #
  # Step 2: Flag and reset outdated VRI attributes                          ####
  
  # List of VRI treed stand numerical variables of interest (excluding age)
  vri_treed_num_vars <- c(
    "vri_mean_proj_height", "vri_quad_diam_125",
    "vri_live_stand_volume_125", "vri_live_stems_per_ha", "vri_basal_area",
    "vri_total_biomass_per_ha", "vri_crown_closure"
  ) 
  
  # Flag outdated samples then reset variables of interest
  reset_forest_variables <- est_age_forest_variables %>% 
    # Flag where VRI needs to be updated (hang on to your hats)
    dplyr::mutate(
      flag_vri_outdated = dplyr::case_when(
        # If stand age is NA, set to FALSE (shrubs, herbs, water, etc...)
        is.na(stand_age) ~ FALSE,
        # If the stand age is older than 60 years, the VRI attributes are 
        # likely up to date even if VRI/CC/HF/RESULTS discrepancies exist
        # (~80% have been photo-surveyed in previous 20 years)
        stand_age > 25 ~ FALSE, 
        # If VRI mean age is smaller than 10, reset/impute will not improve data
        vri_mean_proj_age <= 10 ~ FALSE,
        # If the discrepancy between disturbances and VRI age attributes are
        # within a tolerance of 10 years, reset/impute will not improve data
        abs(stand_age - vri_mean_proj_age) <= 10 |
          abs(stand_age - vri_min_proj_age) <= 10 |
          abs(stand_age - vri_max_proj_age) <= 10 ~ FALSE,
        # If VRI min age is greater than 10 AND it's discrepancy with stand age
        # is greater than 10 years, flag as outdated
        vri_mean_proj_age > 10 & abs(stand_age - vri_mean_proj_age) > 10 ~ TRUE,
        # Everything else set to FALSE 
        TRUE ~ FALSE
      )
    ) %>%
    # Reset outdated observations
    dplyr::mutate(
      # Reset VRI numerical attributes
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        ~ dplyr::if_else(
          condition = flag_vri_outdated,
          true = NA_real_,
          false = .
        )
      ),
      # Reset VRI lead spp
      vri_lead_spp = dplyr::if_else(
        condition = flag_vri_outdated,
        true = NA_character_,
        false = vri_lead_spp
      )
    ) %>% 
    # Clean up
    dplyr::select(-flag_vri_outdated)
  
  # -------------------------------------------------------------------------- #
  # Step 3: Reclassify false NAs and false 0s in VRI treed stand variables  ####

  # When this operation is complete:
  # - NAs are for missing values that will be imputed downstream
  # - 0s are true values for VRI attributes in non-treed or non-veg stands
  
  reclass_forest_variables <- reset_forest_variables %>% 
    # ------------------------------------------------------------------------ #
    # 1. Missing data is 0 across ALL treed stand variables and stand age
    #    where if_all(vri_treed_num_vars, ~ is.na(.) | . == 0)
    dplyr::mutate(
      # If all VRI treed stand variables are NA or 0, set flag TRUE
      vri_treed_all_na_flag = dplyr::if_all(
        dplyr::all_of(vri_treed_num_vars),
        ~ is.na(.) | . == 0
      ),
      # If BCLCS level 4 is untreed class, set flag to TRUE
      vri_bclcs_non_treed_flag = !(vri_bclcs_level_4 %in% c("TC", "TB", "TM")),
      # Set all VRI tree stand variables to 0 
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        ~ dplyr::if_else(vri_treed_all_na_flag & vri_bclcs_non_treed_flag, 0, .)
      ),
      # Set stand age to 0 as placeholder
      stand_age = dplyr::if_else(
        vri_treed_all_na_flag & vri_bclcs_non_treed_flag,
        0,
        stand_age
      ),
      # Set leading species to "none"
      vri_lead_spp = dplyr::if_else(
        vri_treed_all_na_flag & vri_bclcs_non_treed_flag,
        "none",
        vri_lead_spp
      )
    ) %>% 
    # ------------------------------------------------------------------------ #
    # 2. Set ALL treed stand variables and stand age to 0 in non-veg lc classes
    #    where vri_bclcs_level_4 %in% c("WA", "EL", "RO", "SI", "UNK") 
    dplyr::mutate(
      # If BCLCS level 4 land cover is non-vegetated, set flag TRUE
      vri_nonveg_flag = vri_bclcs_level_4 %in% c("WA", "EL", "RO", "SI", "UNK"),
      # Set all VRI tree stand variables to 0 
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        ~ dplyr::if_else(vri_nonveg_flag, 0, .)
      ),
      # Set stand age to 0 as placeholder
      stand_age = dplyr::if_else(vri_nonveg_flag, 0, stand_age),
      # Set leading species to "none"
      vri_lead_spp = dplyr::if_else(vri_nonveg_flag, "none", vri_lead_spp)
    ) %>% 
    # ------------------------------------------------------------------------ #
    # 3. False 0s that should be missing data
    dplyr::mutate(
      # False zeros if there is at least one 0...
      false_zero = dplyr::if_any(dplyr::all_of(vri_treed_num_vars), ~ . == 0) &
        # But there are values that are NOT 0
        !dplyr::if_all(dplyr::all_of(vri_treed_num_vars), ~ . == 0),
      # Replace false 0s with NA
      dplyr::across(
        dplyr::all_of(vri_treed_num_vars),
        # If false 0s detected and a value is 0, set value to NA to impute
        ~ dplyr::if_else(false_zero & . == 0, NA_real_, .)
      )
    ) %>%
    # Clean up
    dplyr::select(
      -vri_treed_all_na_flag, -vri_bclcs_non_treed_flag,
      -false_zero, -vri_nonveg_flag
    )
  
  # -------------------------------------------------------------------------- #
  # Step 4: Impute with MICE                                                ####
  
  # List ID variables to exclude from prediction/imputation
  vri_id_vars <- c("fire_id", "fire_year", "id")
  
  # List landscape class categorical grouping variable, only for prediction
  lc_class_cat_vars <- c("vri_bclcs_level_4", "lc_land_cover")
  
  # List land cover proportion (within 100m only) variables, only for prediction 
  lc_prop_vars <- c(
    "lc_prop_coniferous_100m", "lc_prop_deciduous_100m",
    "lc_prop_nonfuel_100m", "lc_prop_wetland_100m"
  )
  
  # List biogeo categorical variables we want to use for prediction
  biogeo_cat_vars <- c("bgz")
  
  # List topo numerical variables we want to use for prediction
  topo_num_vars <- c("hli", "dem")
  
  # List VRI categorical variables we want to impute
  vri_treed_cat_vars <- c("vri_lead_spp")
  
  # List VRI treed numerical variables we want to impute
  # NB stand age NAs total only 1480
  vri_treed_num_vars <- c(
    "stand_age", "vri_mean_proj_height", "vri_quad_diam_125",
    "vri_live_stand_volume_125", "vri_live_stems_per_ha", "vri_basal_area",
    "vri_total_biomass_per_ha", "vri_crown_closure"
  )
  
  # Make table for imputation
  imputation_tbl <- reclass_forest_variables %>% 
    sf::st_drop_geometry() %>% 
    # Select columns to include in mice
    dplyr::select(
      dplyr::all_of(
        x = c(
          vri_id_vars,
          lc_class_cat_vars,
          lc_prop_vars,
          biogeo_cat_vars,
          topo_num_vars,
          vri_treed_cat_vars,
          vri_treed_num_vars
        )
      )
    ) %>% 
    # Assign proper data types for mice
    dplyr::mutate(
      dplyr::across(dplyr::all_of(lc_class_cat_vars), as.factor),
      dplyr::across(dplyr::all_of(lc_prop_vars), as.numeric),
      dplyr::across(dplyr::all_of(biogeo_cat_vars), as.factor),
      dplyr::across(dplyr::all_of(topo_num_vars), as.numeric),
      dplyr::across(dplyr::all_of(vri_treed_cat_vars), as.factor),
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