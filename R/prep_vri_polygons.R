prep_vri_polygons <- function(
    vri_r1_polygons,
    vri_d_polygons,
    vri_species_key
) {
  
  # -------------------------------------------------------------------------- #
  # Step 1: Retain leading and second species                               ####
  #         - Get the two leading species listed in VRI treed stand
  #         - Pool lead species into common genus
  
  # Parse leading species
  vri_leading_species <- vri_r1_polygons %>%
    # Clean up
    sf::st_drop_geometry() %>%
    janitor::clean_names() %>%
    # Select ID variables and leading species code
    dplyr::select(
      fire_id, fire_year, feature_id,
      species_cd_1, species_cd_2
    ) %>% 
    # Substitute deleted hybrids (in 2019) for genus code (see p. 217)
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("species_cd_"),
        ~ dplyr::case_when(
          .x %in% c("SXE", "SXB", "SXX")  ~ "SX", # Spruce hybrids
          .x == "EXW"                     ~ "E",  # Birch hybrid 
          TRUE                            ~ .x
        )
      )
    ) %>% 
    # Join by leading species code in VRI species key
    dplyr::left_join(
      y = vri_species_key,
      by = dplyr::join_by(species_cd_1 == vri_species_code)
    ) %>%
    # Parse leading species names and pool rarer species into genera
    dplyr::mutate(
      common_genus = stringr::str_to_lower(common_genus),
      common_name = stringr::str_to_lower(common_name),
      vri_lead_genus = dplyr::case_when(
        # All broadleaf together
        common_genus %in% c("alder", "birch", "maple", "poplar") |
          common_name == "unknown broadleaf"
        ~ "broadleaf",
        # Other coniferous
        common_genus %in% c("larch", "hemlock", "cedar", "cypress", "juniper") |
          common_name == "unknown conifer"
        ~ "other_coniferous",
        # Unknown species set to NA
        common_name == "unknown" ~ NA_character_,
        # Leave remainder unchanged
        TRUE ~ common_genus
      )
    ) %>%
    # Clean up
    dplyr::select(
      fire_id, fire_year, feature_id,
      vri_lead_genus, vri_lead_spp = common_name, species_cd_2
    ) %>%
    # Join by secondary species code in VRI species key
    dplyr::left_join(
      y = vri_species_key,
      by = dplyr::join_by(species_cd_2 == vri_species_code)
    ) %>%
    # Parse secondary species names
    dplyr::mutate(vri_second_spp = stringr::str_to_lower(common_name)) %>% 
    # Clean up
    dplyr::select(
      fire_id, fire_year, feature_id,
      vri_lead_spp = common_name, vri_lead_genus, vri_second_spp
    )
  
  # -------------------------------------------------------------------------- #
  # Step 2: Select VRI R1 layer attributes of interest, join species data   ####
  
  vri_r1 <- vri_r1_polygons %>% 
    # Clean names
    janitor::clean_names() %>%
    # Reset geometry column with new name
    sf::st_set_geometry("geometry") %>% 
    # Join two leading species
    dplyr::left_join(
      y = vri_leading_species,
      by = c("fire_year", "fire_id", "feature_id")
    ) %>%
    # Compute some new attributes
    dplyr::mutate(
      # VRI archive year
      projected_year = lubridate::year(projected_date),
      # VRI year source data was collected, interpreted, and input in dbase  
      reference_year, # Collected
      interpretation_year = lubridate::year(interpretation_date), # Interpreted
      input_year = lubridate::year(input_date), # Input
      # Harvest year
      harvest_year = lubridate::year(harvest_date),
      # Stand total biomass (whole_stem, branch, foliage, bark)
      total_biomass_per_ha = dplyr::if_else(
        # If all VRI biomass variables are NA
        dplyr::if_all(dplyr::contains("_biomass_per_ha"), ~ is.na(.x)),
        # Return NA
        NA_real_,
        # Else return their sum 
        rowSums(dplyr::across(dplyr::contains("_biomass_per_ha")), na.rm = TRUE)
      ),
      # Weighted mean, min, and max age of two leading species
      mean_proj_age = dplyr::case_when(
        # If second species has no age, only use species 1
        !is.na(proj_age_1) & is.na(proj_age_2) ~ proj_age_1,
        # If both leading species have age data, use mean weighted by % cover
        !is.na(proj_age_1) & !is.na(proj_age_2) ~
          proj_age_1*(species_pct_1/(species_pct_1 + species_pct_2)) +
          proj_age_2*(species_pct_2/(species_pct_1 + species_pct_2)),
        # Else, return NA
        TRUE ~ NA_real_
      ),
      min_proj_age = pmin(proj_age_1, proj_age_2, na.rm = TRUE),
      max_proj_age = pmax(proj_age_1, proj_age_2, na.rm = TRUE),
      # Weighted mean height of two leading species
      mean_proj_height = dplyr::case_when(
        # If second species has no height, only use species 1
        !is.na(proj_height_1) & is.na(proj_height_2) ~ proj_height_1,
        # If both leading species have age data, use mean weighted by % cover
        !is.na(proj_height_1) & !is.na(proj_height_2) ~
          proj_height_1*(species_pct_1/(species_pct_1 + species_pct_2)) +
          proj_height_2*(species_pct_2/(species_pct_1 + species_pct_2)),
        # Else, return NA
        TRUE ~ NA_real_
      ),
      # Set BCLCS level 4 to "WA" when water in level 2 (it is currently NA)
      bclcs_level_4 = dplyr::if_else(bclcs_level_2 == "W", "WA", bclcs_level_4),
      # Set pther BCLCS level 4 NAs to "UNK"
      bclcs_level_4 = dplyr::if_else(is.na(bclcs_level_4), "UNK", bclcs_level_4)
    ) %>%
    # Select attributes of interest ------------------------------------------ #
    dplyr::select(
      # Fire and VRI polygon IDs
      fire_year, fire_id, feature_id,
      # VRI archive dates
      projected_year, # Archive year
      reference_year, # Attribute data collected
      interpretation_year, # Attribute data interpreted
      # Disturbance date
      harvest_year,
      # Stand composition
      vri_lead_genus, # Lead genus
      vri_lead_spp, # Lead species
      vri_second_spp, # Seconday species
      # Stand attributes
      mean_proj_height, # Mean projected height for 2 leading species
      mean_proj_age, # Weighted mean projected age for 2 leading species
      min_proj_age, # Minimum projected age of 2 leading species
      max_proj_age, # Maximum projected age of 2 leading species
      quad_diam_125, # Quadratic mean stand diameter (breast height)
      live_stand_volume_125, # Stand volume
      vri_live_stems_per_ha, # Stand density
      basal_area, # Stand total cross sectional area of all living trees (m2/ha)
      crown_closure, # Stand percentage of ground area covered by tree canopy
      total_biomass_per_ha, # Stand total biomass (stem, branch, foliage, bark)
      # Understory
      shrub_height, # Avg height of shrubs (m)
      shrub_crown_closure, # Percent ground covered by shrubs (%)
      bryoid_cover_pct, # Percent ground covered by bryoids (%)
      herb_cover_pct, # Percent ground covered by graminoids (%)
      # British Columbia land cover classification scheme level 2 and 4
      bclcs_level_2,
      bclcs_level_4,
      # Stand dead attributes
      stand_percentage_dead, # Percent of stand dead following epidemic
      dead_stand_volume_125, # Stand snag volume
      vri_dead_stems_per_ha # Stand snag density
    ) %>%
    # Add vri prefix to all variables ---------------------------------------- #
    dplyr::rename_with(
      ~ paste0("vri_", .x),
      # Exclude IDs, geometry, and variables that already have the prefix
      !(fire_year | fire_id | feature_id | dplyr::starts_with("vri") | geometry)
    )
  
  # -------------------------------------------------------------------------- #
  # Step 3: Select VRI attributes only found in the D layer (dead)          ####
  
  # Prep DEAD layer columns 
  vri_d <- vri_d_polygons %>% 
    # Clean names
    janitor::clean_names() %>%
    sf::st_drop_geometry() %>% 
    # Compute some new attributes
    dplyr::mutate(
      # Stand total dead biomass (whole_stem, branch, foliage, bark)
      total_biomass_per_ha = rowSums(
        dplyr::across(dplyr::contains("_biomass_per_ha")),
        na.rm = TRUE
      )
    ) %>%
    # Select attributes of interest
    dplyr::select(
      # Fire and VRI polygon IDs
      fire_year, fire_id, feature_id,
      # Stand dead attributes only found in D layer
      proj_height = proj_height_1, # Projected height of leading dead species
      proj_age = proj_age_1, # Projected age of leading dead species
      quad_diam_125, # Quadratic mean stand diameter of dead trees
      basal_area, # Stand total cross sectional area of dead trees (m2/ha)
      total_biomass_per_ha # Stand total dead biomass 
    ) %>%
    # Add vri_d prefix to all variables only found in the D layer
    dplyr::rename_with(
      ~ paste0("vri_d_", .x),
      # Exclude ID columns
      !(fire_year | fire_id | feature_id)
    )
  
  # -------------------------------------------------------------------------- #
  # Step 4: Join R1 and D layers, return                                    ####
  
  vri_out <- dplyr::left_join(
    x = vri_r1,
    y = vri_d,
    by = c("fire_year", "fire_id", "feature_id")
  ) %>% 
    # Add vri_ prefix to VRI feature id
    dplyr::rename(vri_feature_id = feature_id)
  
  # Return
  return(vri_out)
}