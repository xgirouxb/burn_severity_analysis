#' Check if provided string is a URL
#'
#' @param string A string
#'
#' @return Logical TRUE or FALSE
#'
is_url <- function(string) {
  stringr::str_detect(string, "https|http|ftp")
}

#' List URL subdirectories available in URL directory
#'
#' @param url A URL string
#' @param match_string A string to subset matching directories
#'
#' @return A list of subdirectory URLs
#'
get_url_list <- function(url, match_string = NULL){
  
  # Check if supplied URL string is valid
  if (!is_url(url)) { stop("Invalid URL") }
  
  # List URL subdirectories available in URL directory
  list_subdir <- rvest::read_html(url) %>%
    # Parse subdirectory URLS
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href")
  
  # Subset subdirectories of interest 
  if (!is.null(match_string)) {
    list_subdir <- stringr::str_subset(list_subdir, pattern = match_string)
  }
  
  # Construct subdirectory URL
  url_list <- rvest::url_absolute(list_subdir, url)
  
  # Return
  return(url_list)
}

#' Download and unzip archives from a URL to local temporary directory
#'
#' @param archive_url A URL string with the address of the archived file.
#' @param output_dir An output directory where the zip is extracted. It can be
#'                   useful to supply this for large files so they avoid
#'                   garbage collection in temp folders.
#'
#' @return A string with the local directory where the unzipped file is cached.
#'
get_archive_from_url <- function(archive_url, output_dir = NULL) {
  
  # Check if supplied URL string is valid
  if (!is_url(archive_url)) { stop("Invalid URL") }
  
  # Create temp folder for download
  temp_zip <- tempfile()
  
  # Create output directory, use temp folder if output_dir is not supplied
  fs::dir_create(output_dir <- output_dir %||% tempfile())
  
  # Download zipped archive from URL
  download.file(archive_url, destfile = temp_zip, quiet = TRUE, mode = "wb")
  
  # Unzip to second temp directory
  archive::archive_extract(temp_zip, dir = output_dir)
  
  # Return temp directory or supplied output directory 
  return(output_dir)
}

#' Read and filter simple features layers from a local temp directory or URL
#'
#' @param sf_source A URL or local directory where the sf is stored.
#' @param proj An object of class crs with the target projection (see ?sf::st_crs).
#' @param sf_aoi An sf object defining an area of interest to filter the output sf. 
#' @param sf_glob A wildcard passed on to grep() to filter paths (see ?fs::dir_ls).
#' @param file File to write to.
#' 
#' @return An sf object, projected and filtered to aoi if specified.
#'
get_sf_from_source <- function(
    sf_source,
    proj = study_proj,
    sf_aoi = NULL,
    sf_glob = "*.shp",
    file = NULL
) {
  
  # If the provided source is a URL, download and unzip to local temp directory
  if (is_url(sf_source)) { sf_source <- get_archive_from_url(sf_source) }
  
  # Get local path to vector layer of interest
  sf_path <- sf_source %>% 
    # Check all subfolders in sf_source for path to simple feature of interest
    fs::dir_ls(recurse = TRUE, type = "file", glob = sf_glob)
  
  # Read simple feature object
  sf_obj <- sf::read_sf(sf_path) %>%
    # Reproject if sf crs doesn't match with study projection
    { if (sf::st_crs(.) != sf::st_crs(proj)) sf::st_transform(., proj) else . }
  
  # Filter to area of interest bounds
  if (!is.null(sf_aoi)) {
    sf_obj <- sf_obj %>% 
      # Remove M in XYM geometries if present (avoids sf complaints)
      sf::st_zm() %>%
      # Filter to area of interest
      sf::st_filter(sf_aoi)
  }
  
  # Write to file if argument is provided
  if (!is.null(file)) {
    
    # Create directory if it doesn't exist
    if (!fs::dir_exists(fs::path_dir(file))) { 
      fs::dir_create(fs::path_dir(file))
    }
    
    # Write to file
    suppressWarnings(sf::write_sf(obj = sf_obj, dsn = file, quiet = TRUE))
  }
  
  # Return 
  return(sf_obj)
}
