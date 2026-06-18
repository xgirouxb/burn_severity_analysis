# Activate renv environment management
source("renv/activate.R")

# Fix permission errors when installing igraph in renv
Sys.setenv(R_INSTALL_STAGED = FALSE)

# Fix "SSL connect error" when working within networks
Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT = TRUE)

# Set longer timeout for download.file()
options(timeout = max(1000, getOption("timeout")))

# Raise {future} serialization warning threshold to 3GB
# (suppresses warnings when passing large objects to parallel workers)
options('future.globals.maxSize' = 3 * 1024^3)

# Define and set to miniforge conda env for reticulate
# see https://rstudio.github.io/reticulate/articles/versions.html
env_name <- "python-gee"
Sys.setenv(RETICULATE_PYTHON = fs::path_home("AppData", "Local", "miniforge3", "envs", env_name, "python.exe"))
options('reticulate.conda_binary' = fs::path_home("AppData", "Local", "miniforge3", "condabin", "conda.bat"))
reticulate::use_condaenv(
  condaenv = env_name,
  conda = fs::path_home("AppData", "Local", "miniforge3", "condabin", "conda.bat")
)

# Silence {googledrive} functions
options(googledrive_quiet = TRUE)
