get_cef_integrated_roads <- function(sampling_polygons, n_workers = NULL) {

  # -------------------------------------------------------------------------- #
  # 1. Download and cache Integrated Roads FGDB ####
  
  # Create local cache for cef roads gdb
  roads_cache <- fs::dir_create("data/_cache/cef_roads/gdb_cache")
  
  # Get path of integrated roads geodatabase currently in cache
  cached_gdb <- fs::dir_ls(
    path = fs::dir_ls(roads_cache),
    regexp = "\\.gdb$",
    ignore.case = TRUE
  )
  
  # If there is no geodatabase in the roads cache...
  if (length(cached_gdb) == 0L) {
    
    # Get the {bcdata} resource for integrated roads geodatabase
    bcdata_roads_resource <- bcdata::bcdc_tidy_resources(uuid_cef_roads) %>%
      dplyr::filter(format == "fgdb")
    
    # Sanity check: There should only be one resource in {bcdata} database
    if(nrow(bcdata_roads_resource) != 1L) { 
      stop("⚠️ Number of road GDBs listed on {bcdata} for UUID should be 1!") 
    }
    
    # Download zipped integrated roads geodatabase to local cache and unzip
    get_archive_from_url(
      archive_url = dplyr::pull(bcdata_roads_resource, url),
      output_dir = roads_cache
    )
    
    # Get path of integrated roads geodatabase currently in cache
    cached_gdb <- fs::dir_ls(
      path = fs::dir_ls(roads_cache),
      regexp = "\\.gdb$",
      ignore.case = TRUE
    )
  }
  
  # Sanity check: There should only be one locally cached roads geodatabase
  if(length(cached_gdb) != 1L) { stop("⚠️ Only 1 roads GDB should be cached!") }
  
  # -------------------------------------------------------------------------- #
  # 2. Parse CEF Integrated Roads for study fires ####
  
  # Setup parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { 
    future::plan(
      strategy = "future::multisession",
      workers = n_workers,
      gc = TRUE
    )
  }
  
  all_cef_roads <- sampling_polygons %>%
    # Add 10 km buffer (+ 3*30m pixel buffer)
    sf::st_buffer(dist = 10000 + 3*30) %>%
    dplyr::group_split(fire_id) %>%
    # Map across each fire
    furrr::future_map(
      function(study_fire) {
        
        # Read the cached integrated roads geodatabase
        sf_roads <- sf::st_read(
          dsn = cached_gdb,
          # Spatial query using WKT
          wkt_filter = sf::st_as_text(sf::st_geometry(study_fire)),
          # Silence outputs
          quiet = TRUE,
          # Output tibble instead of data.frame
          as_tibble = TRUE
        ) %>% 
          # Add fire_id and fire_year as fields
          dplyr::mutate(
            fire_id = unique(study_fire$fire_id),
            fire_year = unique(study_fire$fire_year)
          ) %>% 
          # Rename geometry
          dplyr::rename(geometry = Shape)
        
        # Constant attribute-geometry relationship (silence warnings)
        sf::st_agr(sf_roads) <- "constant"
        
        # Clip roads to the buffered sampling polygon
        sf::st_intersection(x = sf_roads, y = sf::st_geometry(study_fire)) %>% 
          # Compute road length
          dplyr::mutate(
            road_length_km = as.numeric(sf::st_length(geometry))/1000
          )
      },
      # Pass seed to {future} to avoid complaints
      .options = furrr::furrr_options(seed = 42)
    ) %>%
    # Combine and clean up
    dplyr::bind_rows() %>% 
    sf::st_make_valid()
  
  # Close parallel processing if n_workers is supplied
  if(!is_null(n_workers)) { future::plan(strategy = "future::sequential") }
  
  # -------------------------------------------------------------------------- #
  # 3. Classify road presence: DRA, then RESULTS, then O&G evidence ####
  cef_road_lines <- all_cef_roads %>%
    # Parse years
    dplyr::mutate(
      # Digital Road Atlas data capture year
      dra_year = lubridate::year(DRA_DATA_CAPTURE_DATE),
      # RESULTS reference year
      results_year = as.numeric(RESULTS_REFERENCE_YEAR),
      # Oil and gas land-stage effective year
      oil_gas_year = lubridate::year(
        lubridate::ymd(OG_PERMITS_ROW_LAND_STAGE_EFF_DATE, quiet = TRUE)
      )
    ) %>%
    # Classify the road as present or absent relative to fire_year
    dplyr::mutate(
      # Conservative road presence hierarchy:
      # DRA     | Pre-fire capture date + not proposed | Present
      # RESULTS | Pre-fire reference year              | Present
      # O&G     | Pre-fire effective year + Constructed| Present
      # Other   | FTEN-only / undated / post-fire      | Absent
      road_present = dplyr::case_when(
        
        # If DRA date predates fire ...
        !is.na(dra_year) & dra_year < fire_year &
          # ...and it is not a proposed road
          dplyr::coalesce(DRA_ROAD_CLASS != "Road Proposed", TRUE)
        # Classify as present
        ~ TRUE,
        
        # If RESULTS reference data predates fire
        !is.na(results_year) & results_year < fire_year
        # Classify as present
        ~ TRUE,
        
        # If Oil & gas permit year predates fire ...
        !is.na(oil_gas_year) & oil_gas_year < fire_year &
          # ...and is an actual constructed road record
          OG_PERMITS_ROW_CONSTRUCTION_DESC == "Constructed"
        # Classify as present
        ~ TRUE,
        
        # Otherwise classify as absent (FTEN low confidence)
        TRUE ~ FALSE
      )
    ) %>% 
    # Remove roads absent prior to wildfire
    dplyr::filter(road_present)
  
  # Return road lines present before wildfire
  return(cef_road_lines)
}
