get_firezone_polygons <- function(study_fire_sampling_polygons) {
  
  # Fire regime types (FRT) legend (see Figure 4, doi 10.1139/cjfr-2019-0191)
  # m : Mountains
  # FA : Fire activity -> low (FA1) to extreme (FA5) size/frequency/burn rate
  # SE : Seasonality -> spring (sp) or summer (su) dominated, or mixed (mx)
  # IC : Ignition cause -> human (hm), lightning (lt), or mixed (mx)
  # frt_combo -> ignition doesnt influence fire behaviour, pool into less FRTs
  frt_legend <- tibble::tribble(
    ~frt,             ~frt_name,     ~frt_combo,
       1,   "FA1 - SEsp - IChm",   "FA1 - SEsp",
       2,   "FA2 - SEsu - IClt",   "FA2 - SEsu",
       3,   "FA4 - SEsu - IClt",   "FA4 - SEsu",
       4,   "FA4 - SEmx - IClt",   "FA4 - SEmx",
       5,   "FA1 - SEmx - IClt",   "FA1 - SEmx",
       6,   "FA3 - SEmx - ICmx",   "FA3 - SEmx",
       7,   "FA2 - SEsp - IChm",   "FA2 - SEsp",
       8,   "FA5 - SEsu - IClt",   "FA5 - SEsu",
       9, "m FA2 - SEmx - IClt", "m FA2 - SEmx",
      10, "m FA2 - SEsu - IClt", "m FA2 - SEsu",
      11, "m FA4 - SEmx - IClt", "m FA4 - SEmx",
      12, "m FA1 - SEmx - IChm", "m FA1 - SEmx",
      13, "m FA2 - SEsu - ICmx", "m FA2 - SEsu",
      14, "m FA2 - SEsu - IChm", "m FA2 - SEsu",
      15, "m FA1 - SEsu - IChm", "m FA1 - SEsu"
  )
  
  # Get Canada fire zone polygons that intersect study fires
  firezone_polygons <- get_sf_from_source(
    sf_source = url_frt_firezones,
    sf_aoi = study_fire_sampling_polygons
  ) %>% 
    # Join FRT names
    dplyr::left_join(frt_legend, by = c("Cluster" = "frt")) %>% 
    # Clean up
    dplyr::select(frt = Cluster, frt_name = frt_combo, geometry)
  
  # Return
  return(firezone_polygons)
}