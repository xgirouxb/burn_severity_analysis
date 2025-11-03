# Load packages required to define the pipeline
library(targets)
library(tarchetypes)

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
  # Import study area fires, samples for pipeline, and input data sets
  
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
  # Define sampling area for study fires (include 1km and skips/refugia)
  tar_target(
    name = study_fire_sampling_polygons,
    command = sf::st_buffer(delete_holes(study_fire_polygons), 1000)
  ),
  # Import burn sample points from raw BC fire dataset (MGH)
  tar_target(
    name = burn_sample_points,
    command = get_bc_burn_sample_points(
      path_fire_data, 
      study_fire_sampling_polygons
    )
  ),
  # Import BC consolidated cutblocks that intersect study fire sampling polygons
  tar_target(
    name = cutblock_polygons,
    command = get_cutblock_polygons(study_fire_sampling_polygons)
  ),
  # Import historical fires that intersect/predate study fire sampling polygons
  tar_target(
    name = historical_fire_polygons,
    command = get_historical_fire_polygons(study_fire_sampling_polygons)
  ),
  # Import CanLaD harvest and fire disturbance rasters for study fire areas
  tar_target(
    name = canlad_disturbance_rasters,
    command = get_canlad_disturbance_rasters(study_fire_sampling_polygons)
  ),
  # Import pre-CanLaD harvest and fire disturbance rasters for study fire areas
  tar_target(
    name = precanlad_disturbance_rasters,
    command = get_precanlad_disturbance_rasters(study_fire_sampling_polygons)
  ),
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
      sf_aoi = study_fire_sampling_polygons,
      vri_lyr_name = "LYR_R1"
    )
  ),
  # Import VRI D layer (dead) polygons for study years and fires
  tar_target(
    name = vri_d_polygons,
    command = get_vri_polygons(
      sf_aoi = study_fire_sampling_polygons,
      vri_lyr_name = "LYR_D"
    )
  ),
  # Import BC RESULTS openings polygons for study years and fires
  tar_target(
    name = results_openings_polygons,
    command = get_results_openings_polygons(study_fire_sampling_polygons)
  ),
  # Import land cover classes sampled via Google Earth Engine
  # e.g. https://code.earthengine.google.com/d78997162f4707f78ba8ad0f36572e31
  tar_target(
    name = land_cover_class_tbl,
    command = get_land_cover_class_tbl(
      burn_sample_points,
      # Supply vector of radii within which to evaluate land cover proportions
      neighbourhood_radius = c(100, 500, 1000)
    )
  ),
  # Import RESULTS data related to post-disturbance plantations and treatments
  tar_target(
    name = bc_results_tbl,
    command = get_bc_results_tbl(
      path_fire_data,
      burn_sample_points
    )
  ),
  # Import NFIS forest tenure rasters
  tar_target(
    name = nfis_tenure_rasters,
    command = get_nfis_forest_tenure_rasters(
      study_fire_sampling_polygons
    )
  ),
  # Import topographic and biogeo variables
  tar_target(
    name = topo_geo_tbl,
    command = get_topo_geo_tbl(
      path_fire_data,
      burn_sample_points
    )
  ),
  
  # -------------------------------------------------------------------------- #
  # Prepare input data sets for modelling

  # Prepare and merge VRI R1 and D polygon layers
  tar_target(
    name = vri_polygons,
    command = prep_vri_polygons(
      vri_r1_polygons,
      vri_d_polygons,
      vri_species_key
    )
  ),
  # Find samples with disturbances that interfere with burn ratios
  # (i.e., harvest/fire in 1 year window around study fire)
  tar_target(
    name = biased_burn_ratio_sample_ids,
    command = find_biased_burn_ratio_sample_ids(
      burn_sample_points,
      cutblock_polygons,
      historical_fire_polygons
    )
  ),
  # Sample forest variables: vegetation, land cover, and harvest/fire
  tar_target(
    name = forest_variables,
    command = sample_forest_variables(
      burn_sample_points,
      biased_burn_ratio_sample_ids,
      vri_polygons,
      land_cover_class_tbl,
      cutblock_polygons,
      historical_fire_polygons,
      canlad_disturbance_rasters,
      precanlad_disturbance_rasters,
      bc_results_tbl,
      topo_geo_tbl
    )
  ),
  # Reclassify/impute VRI variables
  tar_target(
    name = imputed_forest_variables,
    command = impute_forest_variables(forest_variables)
  )
)
