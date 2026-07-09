# Utility functions for handling input dataframes and aligning them to JSON schema

#' Read an object from the global environment by name
#'
#' Supports plain names (`my_df`), list element access (`obj$element`),
#' and bracket notation (`obj[['element']]`).
#'
#' @param var_name Character string identifying the object
#' @param envir Environment of the object (default: .GlobalEnv)
#' @return The object itself
get_from_env <- function(var_name, envir = .GlobalEnv) {
  var_name <- trimws(var_name)

  # plain name
  # may contain letters, numbers, . and _, but not starting with _ or number
  if (grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", var_name)) {
    return(get(var_name, envir = envir))
  }

  # obj$element format
  # valid name + '$' + valid name (same rules as above)
  pattern1 <- "^([a-zA-Z.][a-zA-Z0-9._]*)\\$([a-zA-Z.][a-zA-Z0-9._]*)$"
  if (grepl(pattern1, var_name)) {
    obj_name <- sub(pattern1, "\\1", var_name)
    element_name <- sub(pattern1, "\\2", var_name)
    return(get(obj_name, envir = envir)[[element_name]])
  }

  # obj[['element']] or obj[["element"]] format
  # valid name + '[[' + quote (single or double) + valid name + same quote + ']]'
  # \\2 is backreference to the quote type (second group)
  pattern2 <- "^([a-zA-Z.][a-zA-Z0-9._]*)\\[\\[(['\"])([a-zA-Z.][a-zA-Z0-9._]*)\\2\\]\\]$"
  if (grepl(pattern2, var_name)) {
    obj_name <- sub(pattern2, "\\1", var_name)
    element_name <- sub(pattern2, "\\3", var_name)
    return(get(obj_name, envir = envir)[[element_name]])
  }

  stop("Unrecognized variable name format: ", var_name)
}

generate_desc_template <- function(df_rxsmeta){
  site_species_counts <- df_rxsmeta |>
    dplyr::group_by(site_label, species_code) |>
    dplyr::summarise(n = dplyr::n_distinct(tree_label), .groups = "drop") |>
    dplyr::mutate(n_species = glue::glue("{n} {species_code} tree{ifelse(n>1,'s','')}")) |>
    dplyr::group_by(site_label) |>
    dplyr::summarise(species_counts = and_collapse(n_species)) |>
    glue::glue_data("{species_counts} collected at site {site_label}") |>
    and_collapse(oxford_comma = TRUE)

  desc <- glue::glue("
  This dataset contains data from {site_species_counts}, 
  covering the period from YYYY to YYYY [ADD INFO].

  [ADD INFO on site locations, characteristics].
  
  Study purpose: [ADD INFO on research questions and goals]
  
  Study design: [ADD INFO on sampling design, methods, etc.]
  ")
  desc
}

