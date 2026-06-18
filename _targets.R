# Load packages required to define the pipeline
library(targets)
library(tarchetypes)

# Load environmental variables (in .Renviron file) for Google APIs
GOOGLE_ACCOUNT_EMAIL <- Sys.getenv("GOOGLE_ACCOUNT_EMAIL")
EARTH_ENGINE_PROJECT_ID <- Sys.getenv("EARTH_ENGINE_PROJECT_ID")

# Refresh if OAuth token is stale to avoid interactive stall of pipeline
googledrive::drive_auth(email = GOOGLE_ACCOUNT_EMAIL)

# Set target options
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    # File/folder control tools
    "fs", "readr", "janitor",
    # Data manipulation tools
    "dplyr", "tidyr", "magrittr", "tibble", "purrr", "furrr", "forcats",
    # Spatial data tools
    "sf", "terra",
    # Data visualization tools
    "ggplot2"
  ),
  # Default data format
  format = "rds",
  # Run garbage collection before launching a target
  garbage_collection = TRUE
)

# Run all the R scripts in the R/ folder
tar_source()

# ---------------------------------------------------------------------------- #
# Target list

list(
  
  # -------------------------------------------------------------------------- #
  # 1. Define study area fires and sampling points for analysis
  # -------------------------------------------------------------------------- #
  
  # Define study area as BC admin boundaries
  tar_target(
    name = study_area,
    command = define_study_area()
  ),
  # Import BC fire larger than 1000 hectares in study years from NBAC archive
  tar_target(
    name = study_fire_polygons,
    command = get_study_fire_polygons(study_area, study_years)
  ),
  # Define sampling area for study fires (include buffer and skips/refugia)
  tar_target(
    name = sampling_polygons,
    command = define_sampling_polygons(study_fire_polygons)
  ),
  # Define study fire sampling points
  tar_target(
    name = sampling_points,
    command = define_sampling_points(
      study_fire_polygons,
      sampling_polygons,
      n_workers = round(parallelly::availableCores()*0.5)
    )
  ),
  
  # -------------------------------------------------------------------------- #
  # 2. Import descriptive bio-geographical and fire regime layers for mapping
  # -------------------------------------------------------------------------- #

  # Import NRCAN Canada Vegetation Zone polygons
  tar_target(
    name = vegetation_zone_polygons,
    command = get_vegetation_zone_polygons(sampling_polygons)
  ),
  # Import BC Biogeoclimatic Zone polygons
  tar_target(
    name = biogeoclimatic_zone_polygons,
    command = get_biogeoclimatic_zone_polygons(sampling_polygons)
  ),
  # Import BC Biogeoclimatic Zone groups polygons
  tar_target(
    name = biogeoclimatic_zone_groups_polygons,
    command = get_biogeoclimatic_zone_groups_polygons()
  ),
  # Import Fire Regime Types polygons
  tar_target(
    name = firezone_polygons,
    command = get_firezone_polygons(sampling_polygons)
  ),

  # -------------------------------------------------------------------------- #
  # 3. Prepare response variable - burn severity
  # -------------------------------------------------------------------------- #

  #  Import Relativized Burn Ratio images computed via Google Earth Engine API
  tar_target(
    name = burn_severity_rasters,
    command = get_burn_severity_rasters(sampling_polygons)
  ),

  # -------------------------------------------------------------------------- #
  # 4. Top-down fire weather covariates
  # -------------------------------------------------------------------------- #

  # Import and rasterize fire weather points data from the Canadian Fire Spread
  # Dataset (CFSDS) repository on OSF
  tar_target(
    name = fire_weather_rasters,
    command = get_fire_weather_rasters(
      sampling_polygons,
      n_workers = round(parallelly::availableCores()*0.2)
    )
  ),

  # -------------------------------------------------------------------------- #
  # 5. Bottom-up topography covariates
  # -------------------------------------------------------------------------- #

  # Import topography metric rasters
  tar_target(
    name = topography_rasters,
    command = get_topography_rasters(
      sampling_polygons,
      n_workers = round(parallelly::availableCores()*0.2)
    )
  ),

  # -------------------------------------------------------------------------- #
  # 6. Bottom-up vegetation covariates
  # -------------------------------------------------------------------------- #

  # Track VRI leading species key for any updates on disk
  tar_target(
    name = tracked_vri_species_key,
    command = path_vri_species_key,
    format = "file"
  ),
  # Import VRI leading species key
  tar_target(
    name = vri_species_key,
    command = readr::read_csv(tracked_vri_species_key, show_col_types = FALSE)
  ),
  # Import VRI Rank 1 layer polygons for study years and fires
  tar_target(
    name = vri_r1_polygons,
    command = get_vri_polygons(
      sf_aoi = sampling_polygons,
      vri_lyr_name = "LYR_R1"
    )
  ),
  # Import VRI D layer (dead) polygons for study years and fires
  tar_target(
    name = vri_d_polygons,
    command = get_vri_polygons(
      sf_aoi = sampling_polygons,
      vri_lyr_name = "LYR_D"
    )
  ),
  # Import land cover classes sampled via Google Earth Engine Python API
  # see JavaScript equivalent:
  # https://code.earthengine.google.com/d78997162f4707f78ba8ad0f36572e31
  tar_target(
    name = ntems_land_cover_class_tbl,
    command = get_ntems_land_cover_class_tbl(
      sampling_points,
      # Supply vector of neighbourhood radii to compute land cover proportions
      neighbourhood_radius = c(100, 500, 1000)
    )
  ),

  # -------------------------------------------------------------------------- #
  # 7. Bottom-up vegetation disturbance covariates
  # -------------------------------------------------------------------------- #

  # Import BC consolidated cutblocks that intersect study fire sampling polygons
  tar_target(
    name = cutblock_polygons,
    command = get_cutblock_polygons(sampling_polygons)
  ),
  # Import historical fires that intersect/predate study fire sampling polygons
  tar_target(
    name = historical_fire_polygons,
    command = get_historical_fire_polygons(sampling_polygons)
  ),
  # Import CanLaD harvest and fire disturbance rasters for study fire areas
  tar_target(
    name = canlad_disturbance_rasters,
    command = get_canlad_disturbance_rasters(sampling_polygons)
  ),
  # Import pre-CanLaD harvest and fire disturbance rasters for study fire areas
  tar_target(
    name = precanlad_disturbance_rasters,
    command = get_precanlad_disturbance_rasters(sampling_polygons)
  ),
  # Import BC RESULTS openings polygons for study years and fires
  tar_target(
    name = results_openings_polygons,
    command = get_results_openings_polygons(sampling_polygons)
  ),
  # Import BC RESULTS plantings polygons for planted openings
  tar_target(
    name = results_plantings_polygons,
    command = get_results_plantings_polygons(results_openings_polygons)
  ),
  # Import CCFM forest tenure rasters
  tar_target(
    name = ccfm_tenure_rasters,
    command = get_ccfm_forest_tenure_rasters(sampling_polygons)
  ),

  # -------------------------------------------------------------------------- #
  # 8. Prepare input data sets for modelling
  # -------------------------------------------------------------------------- #

  # Prepare and merge VRI R1 and D polygon layers
  tar_target(
    name = vri_polygons,
    command = prep_vri_polygons(
      vri_r1_polygons,
      vri_d_polygons,
      vri_species_key
    )
  ),
  # Prepare RESULTS openings and plantings polygons
  tar_target(
    name = results_polygons,
    command = prep_results_polygons(
      results_openings_polygons,
      results_plantings_polygons,
      vri_species_key
    )
  ),
  # Prepare forestry disturbance rasters
  tar_target(
    name = forestry_disturbance_rasters,
    command = prep_forestry_disturbance_rasters(
      sampling_polygons,
      cutblock_polygons,
      historical_fire_polygons,
      results_polygons,
      vri_species_key
    )
  ),
  # Find samples with disturbances that may interfere with burn ratios
  # (i.e., harvest/fire in 1 year window around study fire)
  tar_target(
    name = biased_burn_ratio_sample_ids,
    command = find_biased_burn_ratio_sample_ids(
      sampling_points,
      cutblock_polygons,
      historical_fire_polygons,
      results_polygons,
      forestry_disturbance_rasters
    )
  ),
  
  # -------------------------------------------------------------------------- #
  # 8. Sample and impute input data sets for modelling
  # -------------------------------------------------------------------------- #
  
  # Sample forest variables: vegetation, land cover, and harvest/fire
  tar_target(
    name = forest_variables,
    command = sample_forest_variables(
      sampling_points,
      biased_burn_ratio_sample_ids,
      burn_severity_rasters,
      fire_weather_rasters,
      vri_polygons,
      ntems_land_cover_class_tbl,
      cutblock_polygons,
      historical_fire_polygons,
      results_polygons,
      canlad_disturbance_rasters,
      precanlad_disturbance_rasters,
      forestry_disturbance_rasters,
      ccfm_tenure_rasters,
      topography_rasters,
      vegetation_zone_polygons,
      biogeoclimatic_zone_polygons,
      biogeoclimatic_zone_groups_polygons,
      firezone_polygons
    )
  ),
  # Reclassify/impute VRI variables
  tar_target(
    name = imputed_forest_variables,
    command = impute_forest_variables(forest_variables, vri_species_key)
  )
)
