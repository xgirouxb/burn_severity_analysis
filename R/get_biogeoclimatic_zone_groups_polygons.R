get_biogeoclimatic_zone_groups_polygons <- function() {
  
  # Get BC Biogeoclimatic Ecological Classification (BEC) polygons
  biogeoclimatic_zone_groups_polygons <- bcdata::bcdc_query_geodata(
    uuid_bc_beogeoclimatic_zones
  ) %>% 
    bcdata::collect() %>% 
    # Define BEC groups
    dplyr::mutate(
      bec_group_short = dplyr::case_when(
        # Dry interior forest
        ZONE %in% c("BG", "IDF", "PP") ~ "DIF",
        # Sub-boreal forest
        ZONE %in% c("SBPS", "SBS") ~ "SBF",
        # Boreal forest
        ZONE %in% c("BWBS", "SWB") ~ "BF",
        # Coastal mountain forest
        ZONE %in% c("CDF", "CWH", "MH") ~ "CMF", 
        # Interior montane forest
        ZONE %in% c("ESSF", "ICH", "MS") ~ "IMF"
      ),
      bec_group_name = dplyr::case_when(
        bec_group_short == "DIF" ~ "Dry Interior Forest",
        bec_group_short == "SBF" ~ "Sub-Boreal Forest",
        bec_group_short == "BF" ~ "Boreal Forest",
        bec_group_short == "CMF" ~ "Coastal Mountain Forest",
        bec_group_short == "IMF" ~ "Interior Montane Forest"
      )
    ) %>% 
    # Dissolve into single MULTIPOLYGON for each BEC group
    dplyr::group_by(bec_group_short, bec_group_name) %>% 
    dplyr::summarise(
      bec_group_short = unique(bec_group_short),
      bec_group_name = unique(bec_group_name),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    # Add a unique numerical code
    dplyr::mutate(bec_group = as.numeric(as.factor(bec_group_short))) %>% 
    # Clean-up
    dplyr::select(bec_group, bec_group_short, bec_group_name)
  
  # Return polygons
  return(biogeoclimatic_zone_groups_polygons)
}