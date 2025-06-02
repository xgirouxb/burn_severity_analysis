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
  # Import study area fires and samples for pipeline 
  
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
  # Import burn sample points from raw BC fire dataset (MGH)
  tar_target(
    name = burn_sample_points,
    command = get_bc_burn_sample_points(path_fire_data)
  )
)
