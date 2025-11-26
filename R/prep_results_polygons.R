prep_results_polygons <- function(
    results_openings_polygons,
    results_plantings_polygons,
    vri_species_key
) {
  
  # -------------------------------------------------------------------------- #
  # Step 1. Parse harvest dates from RESULTS openings ####
  results_harvest <- results_openings_polygons %>% 
    sf::st_drop_geometry() %>% 
    ## 1.1 Parse RESULTS harvest start date ####
    dplyr::mutate(
      # Compute discrepancy between start date and 1st denudation completion
      harvest_start_diff = as.numeric(
        (DENUDATION_1_COMPLETION_DATE - DISTURBANCE_START_DATE)/365.25
      ),
      # Assign harvest start date
      res_harvest_start_date = dplyr::case_when(
        # IF denudation 1 is logged (L) or salvage logged (S),
        DENUDATION_1_DISTURBANCE_CODE %in% c("L", "S") &
          # AND the difference between start and completion > 2 years
          abs(harvest_start_diff) >= 2 ~
          # ASSIGN denudation 1 completion date
          DENUDATION_1_COMPLETION_DATE,
        # ELSE IF denudation 1 is logged (L) or salvage logged (S), 
        DENUDATION_1_DISTURBANCE_CODE %in% c("L", "S") ~
          # ASSIGN earliest from start date or denudation completion date
          pmin(
            DISTURBANCE_START_DATE,
            DENUDATION_1_COMPLETION_DATE,
            na.rm = TRUE
          ),
        # ELSE IF denudation 2 is logged (L) or salvage logged (S) (and 1 is not)
        DENUDATION_2_DISTURBANCE_CODE %in% c("L", "S") ~
          # ASSIGN denudation 2 completion, if NA use denudation 1 completion
          # (These are identical dates except in n = 13 stands)
          dplyr::coalesce(
            DENUDATION_2_COMPLETION_DATE,
            DENUDATION_1_COMPLETION_DATE
          ),
        # ELSE return NA
        TRUE ~ NA
      )
    ) %>% 
    ## 1.2 Parse RESULTS harvest end date ####
    dplyr::mutate(
      # Assign harvest end date
      res_harvest_end_date = dplyr::case_when(
        # IF denudation 1 is logged (L) or salvage (S)
        DENUDATION_1_DISTURBANCE_CODE %in% c("L", "S") &
          # AND denudation 2 is logged (L) or salvage (S) or NA,
          DENUDATION_2_DISTURBANCE_CODE %in% c("L", "S", NA) ~
          # ASSIGN the latest between denudation 1, 2, and res_harvest_start_date
          pmax(
            DENUDATION_1_COMPLETION_DATE,
            DENUDATION_2_COMPLETION_DATE,
            res_harvest_start_date,
            na.rm = TRUE
          ),
        # ELSE IF denudation 1 is logged (L) or salvage logged (S),
        DENUDATION_1_DISTURBANCE_CODE %in% c("L", "S") &
          # AND denudation 2 IS NOT logged (L) or salvage (S) or NA,
          !(DENUDATION_2_DISTURBANCE_CODE %in% c("L", "S", NA)) ~
          # ASSIGN the latest between denudation 1 and res_harvest_start_date
          pmax(
            DENUDATION_1_COMPLETION_DATE,
            res_harvest_start_date,
            na.rm = TRUE
          ),
        # ELSE IF denudation 1 IS NOT logged (L) or salvage logged (S),
        !(DENUDATION_1_DISTURBANCE_CODE %in% c("L", "S")) &
          # AND denudation 2 IS logged (L) or salvage (S),
          DENUDATION_2_DISTURBANCE_CODE %in% c("L", "S") ~
          # ASSIGN the latest between denudation 2 and res_harvest_start_date
          pmax(
            DENUDATION_2_COMPLETION_DATE,
            res_harvest_start_date,
            na.rm = TRUE
          ),
        # ELSE return NA
        TRUE ~ NA
      ),
      # Convert harvest start/end dates to years
      res_harvest_start_year = lubridate::year(res_harvest_start_date),
      res_harvest_end_year = lubridate::year(res_harvest_end_date)
    ) %>% 
    # Select attributes of interest
    dplyr::select(
      # IDs 
      fire_id, fire_year, OPENING_ID,
      # Harvest start/end years
      res_harvest_start_year, res_harvest_end_year
    )
  
  # -------------------------------------------------------------------------- #
  # Step 2. Parse fire years from RESULTS openings ####
  results_fire <- results_openings_polygons %>% 
    sf::st_drop_geometry() %>% 
    # Parse 1st and 2nd fire completion dates
    dplyr::mutate(
      # 1st fire completion date
      res_fire1_date = dplyr::case_when(
        # IF denudation 1 is burned (B),
        DENUDATION_1_DISTURBANCE_CODE == "B" ~
          # ASSIGN denudation 1, if it is NA use disturbance start
          dplyr::coalesce(DENUDATION_1_COMPLETION_DATE, DISTURBANCE_START_DATE),
        # ELSE IF denudation 2 IS burned and denudation 1 IS NOT burned
        DENUDATION_2_DISTURBANCE_CODE == "B" &
          DENUDATION_1_DISTURBANCE_CODE != "B" ~
          # ASSIGN denudation 2 completion date, if NA use study fire date
          dplyr::coalesce(
            DENUDATION_2_COMPLETION_DATE, 
            lubridate::make_date(fire_year, 1, 1)
          ),
        # ELSE return NA
        TRUE ~ NA
      ),
      # 2nd fire completion date
      res_fire2_date = dplyr::if_else(
        # IF both denudation 1 and 2 are burned (B)
        DENUDATION_2_DISTURBANCE_CODE == "B" &
          DENUDATION_1_DISTURBANCE_CODE == "B",
        # ASSIGN denudation 2 completion date
        DENUDATION_2_COMPLETION_DATE,
        # ELSE return NA
        NA
      ),
      # Convert completion dates to years
      # NB: Keep all fire years to identify potential bias in burn ratios
      res_fire1_year = lubridate::year(res_fire1_date),
      res_fire2_year = lubridate::year(res_fire2_date),
      # Retain year of most recent fire that occurred prior to study fire
      res_fire_year = pmax(
        dplyr::if_else(res_fire1_year < fire_year, res_fire1_year, NA_integer_),
        dplyr::if_else(res_fire2_year < fire_year, res_fire2_year, NA_integer_),
        na.rm = TRUE
      )
    ) %>% 
    # Select attributes of interest
    dplyr::select(
      # IDs 
      fire_id, fire_year, OPENING_ID,
      # Fire years
      res_fire1_year, res_fire2_year, res_fire_year
    )
  
  # -------------------------------------------------------------------------- #
  # Step 3. Parse planting years from RESULTS openings ####
  results_plantings <- results_openings_polygons %>% 
    sf::st_drop_geometry() %>% 
    # Add parsed harvest dates
    dplyr::left_join(
      y = results_harvest,
      by = c("fire_id", "fire_year", "OPENING_ID")
    ) %>% 
    # Add some attributes to reduce verbose expressions downstream
    dplyr::mutate(
      # Convert dates of interest to years (shown here in order of priority)
      planting1_year = lubridate::year(PLANTING_1_COMPLETION_DATE),
      planting2_year = lubridate::year(PLANTING_2_COMPLETION_DATE),
      siteprep1_year = lubridate::year(SITE_PREP_1_COMPLETION_DATE),
      siteprep2_year = lubridate::year(SITE_PREP_2_COMPLETION_DATE),
      denudation1_year = lubridate::year(DENUDATION_1_COMPLETION_DATE),
      denudation2_year = lubridate::year(DENUDATION_2_COMPLETION_DATE),
      disturbance_start_year = lubridate::year(DISTURBANCE_START_DATE),
      approve_year = lubridate::year(APPROVE_DATE),
      harvest_year = res_harvest_end_year,
      # Create planted logical flag
      is_planted = dplyr::if_else(
        PLANTING_COUNT == 0 | is.na(PLANTING_COUNT),
        FALSE,
        TRUE
      )
    ) %>% 
    # Parse planting years
    dplyr::mutate(
      ## 3.1 Extract earliest planting year ####
      planting_year = pmin(planting1_year, planting2_year, na.rm = TRUE),
      ## 3.2 Extract latest site preparation year ####
      # NB: Site preparations (burn, mechanical, manual, grass seeding, etc...)
      #     occur before planting. If there is no planting completion date,
      #     use latest site prep year.
      site_prep_year = pmax(siteprep1_year, siteprep2_year, na.rm = TRUE),
      ## 3.3 Extract latest denudation year
      # NB: Denudations (burn, logging, pest, etc..) occur before planting.
      #     If there are no planting completion or site prep years,
      #     use latest denudation date (excluding burns corresponding to study
      #     or post-study fires)
      denudation_year = pmax(
        # 1st denudation year (excluding burns for study or post-study fires)
        dplyr::if_else(
          DENUDATION_1_DISTURBANCE_CODE == "B" & denudation1_year >= fire_year,
          NA,
          denudation1_year
        ),
        # 2nd denudation year (excluding burns for study or post-study fires)
        dplyr::if_else(
          DENUDATION_2_DISTURBANCE_CODE == "B" & denudation2_year >= fire_year,
          NA,
          denudation2_year
        ),
        # Remove NAs
        na.rm = TRUE
      ),
      ## 3.4 Edge cases: extract latest of disturbance start or approve year
      # NB: In some edge cases (for the plantation subset), denudation dates for 
      #     relevant disturbances are missing (n = 1 logging, n = 2 pest). 
      #     Typical chronology where "L": approval --> logging --> planting 
      #     Typical chronology where "P": pest --> approval --> planting
      #     Typical chronology where "R": approval --> rehab --> planting 
      edge_case_year = dplyr::case_when(
        # IF denudation 1 is logging (L) and there is no completion date
        DENUDATION_1_DISTURBANCE_CODE == "L" & is.na(denudation1_year) ~
          # ASSIGN previously extracted harvest end year 
          harvest_year,
        # ELSE IF denudation 1 is pest (P) and there is no completion date
        DENUDATION_1_DISTURBANCE_CODE == "P" & is.na(denudation1_year) ~
          # ASSIGN latest between approval and disturbance start
          pmax(disturbance_start_year, approve_year, na.rm = TRUE),
        # ELSE IF denudation 1 is rehab (R) and there is no completion date
        DENUDATION_1_DISTURBANCE_CODE == "R" & is.na(denudation1_year) ~
          # ASSIGN latest between approval and disturbance start
          pmax(disturbance_start_year, approve_year, na.rm = TRUE),
        # ELSE
        TRUE ~ NA_integer_
      ),
      ## 3.5 Plantation year ####
      # Combine in order of priority
      res_planting_year = dplyr::coalesce(
        planting_year,
        site_prep_year,
        denudation_year,
        edge_case_year,
        NA_integer_
      ),
      # Flag remaining NAs, everything that is TRUE needs sanity check
      inspect_na_flag = if_else(is.na(res_planting_year), TRUE, FALSE),
      # Remove if plantation year is same year as or after study fire year
      # (these NAs do not require inspection sanity check)
      res_planting_year = dplyr::if_else(
        res_planting_year >= fire_year,
        NA_integer_,
        res_planting_year
      )
    )
  
  ## 3.6 Sanity check: missing planting years ####
  
  # Check for any planted polygons with missing years
  planting_sanity_check <- results_plantings %>% 
    # Planting years with NAs to inspect
    dplyr::filter(is_planted, inspect_na_flag) %>% 
    # Exclude plantings where the sole disturbance is associated to study
    # fire or post study fire burns
    dplyr::filter(
      !(((DENUDATION_1_DISTURBANCE_CODE == "B") &
           (DENUDATION_2_DISTURBANCE_CODE %in% c("B", NA))) &
          ((is.na(denudation1_year) | denudation1_year >= fire_year) &
             (is.na(denudation2_year) | denudation2_year >= fire_year)))
    )
  
  # If there are any edge cases remaining...
  if (nrow(planting_sanity_check) > 0) {
    # ...stop and print OPENING_IDs with missing planting years.
    stop(
      "Review planting dates for edge cases, inspect following OPENING_IDs: ",
      paste(planting_sanity_check$OPENING_ID, collapse = ", ")
    )
  }
  
  ## 3.7 Select attributes of interest ####
  results_plantings <- results_plantings %>% 
    # Select attributes of interest
    dplyr::select(fire_id, fire_year, OPENING_ID, res_planting_year)
  
  # -------------------------------------------------------------------------- #
  # Step 4. Parse planted species from RESULTS plantings ####
  results_planted_species <- results_plantings_polygons %>%
    sf::st_drop_geometry() %>% 
    # Only retain if there is a planting date (removes post-study plantings)
    dplyr::filter(
      OPENING_ID %in% (results_plantings %>% 
        dplyr::filter(!is.na(res_planting_year)) %>% 
        dplyr::pull(OPENING_ID))
    ) %>% 
    # Join VRI species key by species code
    dplyr::left_join(
      y = vri_species_key,
      by = c("SILV_TREE_SPECIES_CODE" = "vri_species_code")
    ) %>% 
    # Common genus name to lower case
    dplyr::mutate(common_genus = stringr::str_to_lower(common_genus)) %>% 
    # Sum of number planted for each species in each opening
    dplyr::group_by(fire_id, fire_year, OPENING_ID, common_genus) %>% 
    dplyr::summarise(n = sum(NUMBER_PLANTED), .groups = "drop_last") %>% 
    # Reverse sort by number of planted trees
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>%
    # Get leading planted species, ungroup
    dplyr::summarise(
      res_lead_spp = dplyr::first(common_genus),
      .groups = "drop"
    ) %>% 
    # Pool rare species
    # NB: Not all these are planted species, they are included here for  
    #     consistency with VRI pooling method
    dplyr::mutate(
      res_lead_spp = dplyr::case_when(
        # Other deciduous
        res_lead_spp %in% c("alder", "birch", "maple") | 
          res_lead_spp == "unknown deciduous" 
        ~ "other_deciduous",
        # Other coniferous
        res_lead_spp %in% c("larch", "hemlock", "cedar", "cypress", "juniper") |
          res_lead_spp == "unknown conifer"
        ~ "other_coniferous",
        # Unknown species set to NA
        res_lead_spp == "unknown" ~ NA_character_,
        # Leave remainder unchanged
        TRUE ~ res_lead_spp
      )
    )
  
  # -------------------------------------------------------------------------- #
  # Step 5. Combine RESULTS data and return polygons ####
  results_polygons <- results_openings_polygons %>% 
    # Join harvest dates
    dplyr::left_join(
      y = results_harvest,
      by = c("fire_id", "fire_year", "OPENING_ID")
    ) %>% 
    # Join fire years
    dplyr::left_join(
      y = results_fire,
      by = c("fire_id", "fire_year", "OPENING_ID")
    ) %>%
    # Join planting years and planted species
    dplyr::left_join(
      y = results_plantings,
      by = c("fire_id", "fire_year", "OPENING_ID")
    ) %>%
    dplyr::left_join(
      y = results_planted_species,
      by = c("fire_id", "fire_year", "OPENING_ID")
    ) %>% 
    # Select attributes of interest
    dplyr::select(
      # ID
      fire_id, fire_year, res_opening_id = OPENING_ID,
      # Harvest
      res_harvest_start_year, res_harvest_end_year,
      # Fire
      res_fire1_year, res_fire2_year, res_fire_year,
      # Planting
      res_planting_year, res_lead_spp
    )
  
  # Return
  return(results_polygons)
}