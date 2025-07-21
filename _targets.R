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
  # Import BC consolidated cutblocks that intersect study fire sampling polygons
  tar_target(
    name = cutblock_polygons,
    command = get_cutblock_polygons(study_fire_sampling_polygons)
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
      vri_lyr_name = "VEG_COMP_LYR_R1_POLY"
    )
  ),
  # Import VRI D layer (dead) polygons for study years and fires
  tar_target(
    name = vri_d_polygons,
    command = get_vri_polygons(
      sf_aoi = study_fire_sampling_polygons,
      vri_lyr_name = "VEG_COMP_LYR_D_POLY"
    )
  ),
  # Import burn sample points from raw BC fire dataset (MGH)
  tar_target(
    name = burn_sample_points,
    command = get_bc_burn_sample_points(path_fire_data)
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
  )
  
)
