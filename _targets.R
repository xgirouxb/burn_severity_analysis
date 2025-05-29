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
  # Importing data required for pipeline
  
  # Import burn sample points from raw BC fire dataset (MGH)
  tar_target(
    name = burn_sample_points,
    command = get_bc_burn_sample_points(path_fire_data)
  )
)
