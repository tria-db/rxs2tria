# Utility functions for handling input dataframes and aligning them to JSON schema

#' Create an empty dataframe based on the provided JSON schema
#' @param tbl_name The JSON schema defining the structure of the table.
#' @param nrows Number of rows to create (default 0).
#' @returns An "empty" tibble with the correct columns and types as defined in the schema.
create_empty_df <- function(tbl_name, nrows = 0){
  obj <- get_schema(tbl_name)
  tbl_schema <- jsonlite::fromJSON(obj$schema$schema)
  # Mapping JSON Schema types to R types
  json_to_r_types <- c(
    string    = "c",
    integer   = "i",
    boolean   = "l",
    number    = "d",
    date      = "D",
    datetime  = "T",
    null      = "c" # or "NULL"
  )

  # extract column names and types from schema
  cols_info <- tbl_schema$items$properties
  col_names <- names(cols_info)
  col_types <- sapply(cols_info, function(x) x$type[1]) # NOTE: leverages that default type is in first position in schema!

  # special cases:
  col_formats <- sapply(cols_info, function(x) if (!is.null(x$format)) x$format else "none")
  col_types[which(col_formats == "date-time")] <- "datetime"
  col_types[which(col_formats == "date")] <- "date"
  col_types[which(col_names == "embargoed_until")] <- "date" # special special case because of dependency on ds_access in schema

  r_col_types <- paste(json_to_r_types[col_types], collapse ="")
  empty_df <- readr::read_csv("\n", col_names = col_names, col_types = r_col_types)

  # add dummy rows if nrows > 0
  if (nrows > 0){
    char_cols <- col_names[col_types == "string"] # NOTE: we have at least one char column in all our tables
    bool_cols <- col_names[col_types == "boolean"]
    empty_df[1:nrows, char_cols] <- "" # dummy value to add char rows
    empty_df[1:nrows, bool_cols] <- FALSE # default value FALSE for logical cols bc of checkboxes in rhandsontable
  }

  empty_df
}

#' Helper to convert a column to the target class
#' @param x The column to convert.
#' @param target_class The target class to convert to
convert_column <- function(x, target_class) {
  target_class <- target_class[1]  # Use just the first class name
  switch(target_class,
         "character" = as.character(x),
         "factor" = as.factor(x),
         "numeric" = as.numeric(x),
         "integer" = as.integer(x),
         "logical" = as.logical(x),
         "Date" = as.Date(x),
         "POSIXct" = as.POSIXct(x),
         x  # fallback: no conversion
  )
}

#' Align input dataframes for individual tables to the correct structure
#' @param tbl_name The name of the table, used to get the schema.
#' @param source_data The input dataframe to be aligned.
#' @param force_required If TRUE, missing required columns will raise an error; if FALSE, a warning only.
#' @param ignore_colnames If TRUE, the input df column names will be ignored and replaced with the target structure names by position.
#' @returns The aligned dataframe if successful, otherwise raises an error/warnings.
align_to_schema <- function(source_data, tbl_name,
                            force_required = TRUE,
                            ignore_colnames = FALSE) {
  obj <- get_schema(tbl_name)
  # convert schema to list so we can access properties
  tbl_schema <- jsonlite::fromJSON(obj$schema$schema)

  target_structure <- create_empty_df(tbl_name, nrows = 0)
  target_cols <- names(target_structure)

  aligned_data <- source_data

  # Force target_cols as names for source_data if ignore_colnames is TRUE
  if (ignore_colnames){
    names(aligned_data)[1:length(target_cols)] <- target_cols
  }

  # Check for existence, missing and extra columns
  req_cols <- tbl_schema$items$required
  source_cols <- names(aligned_data)
  missing_cols <- setdiff(req_cols, source_cols)
  extra_cols <- setdiff(source_cols, target_cols)
  common_cols <- intersect(target_cols, source_cols)

  if (length(common_cols) == 0) {
    msg <- paste("No valid columns found in table", tbl_name)
    if (force_required) {
      stop(msg)
    } else {
      warning(msg)
      return(create_empty_df(tbl_name, nrows = 1))
    }
  }

  if (length(missing_cols) > 0) {
    msg <- paste("Missing required columns in table", tbl_name, ":", paste(missing_cols, collapse = ", "))
    if (force_required) {
      stop(msg)
    } else {
      warning(msg)
    }
  }

  if (length(extra_cols) > 0) {
    warning(paste("Extra columns ignored in table", tbl_name))
  }

  # Enforce the correct class for all columns in source_data
  # TODO: use compare_df_cols from janitor to subset only mismatching cols?
  for (col in common_cols) {
    aligned_data[[col]] <- convert_column(aligned_data[[col]], class(target_structure[[col]]))
  }

  # bind to target_structure to ensure all columns are present
  aligned_data <- dplyr::bind_rows(target_structure, aligned_data[common_cols])

  # TODO:
  # # if there are ISO country codes, convert to full name if possible
  # country_cols <- c("org_country", "inst_country", "country")
  # for (col in intersect(country_cols, names(aligned))){
  #   is_iso_code <- aligned_data[[col]] %in% countries_info$country_iso_code
  #   df_lookup <- tibble::tibble(value = aligned[[col]])
  #   df_lookup <- df_lookup |>
  #     dplyr::left_join(countries_info, by = c("value" = "country_iso_code"))
  #   aligned[[col]] <- ifelse(is_iso_code, df_lookup$combined, aligned[[col]])
  # }
  #
  # # for the tree table, lookup species information
  # if (tbl_name == "tree_data" && "species_code" %in% names(aligned)) {
  #   df_lookup <- aligned |>
  #     dplyr::left_join(
  #       species_info, by = c("species_code" = "itrdb_species_code"),
  #       suffix = c("", "_lookup")
  #     ) %>%
  #     dplyr::mutate(
  #       species_name = ifelse(is.na(species_name) | species_name == "",
  #                             species_name_lookup, species_name),
  #       phylogenetic_group = ifelse(is.na(phylogenetic_group) | phylogenetic_group == "",
  #                                   phylogenetic_group_lookup, phylogenetic_group),
  #       leaf_habit = ifelse(is.na(leaf_habit) | leaf_habit == "",
  #                           leaf_habit_lookup, leaf_habit),
  #       tree_ring_structure = ifelse(is.na(tree_ring_structure) | tree_ring_structure == "",
  #                                    tree_ring_structure_lookup, tree_ring_structure)
  #     ) |> dplyr::select(-dplyr::ends_with("_lookup"))
  #   aligned <- df_lookup
  # }

  aligned_data
}

#' Validate an input dataframe against a given schema
#' Wrapper which tries to align the input dataframe to the schema and
#' catches errors and warnings to provide user feedback.
#' @param df_raw The input dataframe to validate.
#' @param tbl_name The name of the table, used to get the schema.
#' @param force_required If TRUE, missing required columns will raise an error; if FALSE, a warning only.
#' @param ignore_colnames If TRUE, the input df column names will be ignored and replaced with the target structure names by position.
#' @returns The aligned dataframe if successful, otherwise raises an error.
validate_df <- function(df_raw, tbl_name,
                        force_required = FALSE,
                        ignore_colnames = FALSE) {
  safe_block({
    tbl_data <- tibble::as_tibble(df_raw, .name_repair = janitor::make_clean_names)
    align_to_schema(tbl_data, tbl_name,
                    force_required = force_required,
                    ignore_colnames = ignore_colnames)
  },
  err_title = "Error loading table",
  err_message = paste("Table", tbl_name, "could not be aligned with required structure:"),
  propagate_err = TRUE,
  warn_message = paste("Warning while loading table", tbl_name, ":")
  )
}

#' Validate a list of input dataframes against the complete JSON schema
#' The input is expected to be a named list of dataframes, where the names correspond
#' to the table names defined in the schema (usually read directly from a json file).
#' @param raw_json_data A named list of dataframes to validate.
#' @returns A named list of validated and aligned dataframes if successful, otherwise raises an error.
validate_json <- function(raw_json_data){
  safe_block({
    obj <- get_schema("metadata")
    # convert schema to list so we can access properties
    complete_schema <- jsonlite::fromJSON(obj$schema$schema)

    all_tbls <- names(complete_schema$properties)
    input_tbls <- names(raw_json_data)
    input_tbls <- input_tbls[!grepl("^\\$", input_tbls)] # ignore any schema keywords
    common_tbls <- intersect(all_tbls, input_tbls)

    if (!'roxas_data' %in% common_tbls) {
      stop("The required 'roxas_data' table is missing in the input file.")
      return(NULL)
    }
    if (length(common_tbls) < length(all_tbls)) {
      missing_tbls <- setdiff(all_tbls, common_tbls)
      warning(paste("The following tables are missing in the input file:", paste(missing_tbls, collapse = ", ")))
    }

    # Validate all imported tables against the schema
    converted <- list()
    for (tbl_name in common_tbls) {
      tbl_data <- raw_json_data[[tbl_name]]
      force_required <- tbl_name == "roxas_data" # only force required for the roxas data table
      converted[[tbl_name]] <- validate_df(tbl_data, tbl_name, force_required = force_required)
    }

    converted
  })
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
     This dataset contains data from {site_species_counts}, covering the period [ADD INFO].
     [ADD INFO on site locations, characteristics].

     Study purpose: [ADD INFO on research questions and goals]

     Study design: [ADD INFO on sampling design, methods, etc.]
   ")
   desc
}
