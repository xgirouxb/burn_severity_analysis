get_bc_results_tbl <- function(
    path_fire_data,
    burn_sample_points
){

  # Read in the raw MGH fire data
  bc_results_tbl <- readr::read_csv(path_fire_data, show_col_types = FALSE) %>%
    # Clean up
    janitor::clean_names() %>%
    # Select RESULTS variables for downstream sampling
    dplyr::select(id = oid, dplyr::starts_with("res_")) %>%
    dplyr::rename(res_age = res_re_sage) %>%
    # Fix bad RESULTS (if res_age is NA, all RESULTS variables should be 0)
    dplyr::mutate(
      dplyr::across(
        # Apply to all res_* columns except res_age
        dplyr::starts_with("res_") & !dplyr::matches("res_age"),
        ~ ifelse(is.na(res_age), 0, .)
      )
    )

  # Return
  return(bc_results_tbl)
}