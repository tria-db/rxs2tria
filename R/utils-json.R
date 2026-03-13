#' Read and resolve JSON Schema from file
#'
#' @param schema_path Path to the JSON schema file
#' @return A list containing the fully resolved schema
#' @export
resolve_schema <- function(schema_path) {

  # Determine base path for resolving relative references
  base_path <- dirname(schema_path)

  # load schema from file
  obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  schema <- jsonlite::fromJSON(obj$schema$schema, simplifyDataFrame = FALSE)

  # helper function to extract table properties from base table schema
  extract_tbl_props <- function(tbl_schema){
    resolved_schema <- list()
    stopifnot(tbl_schema$type == "array")
    resolved_schema$minItems <- tbl_schema$minItems
    resolved_schema$maxItems <- tbl_schema$maxItems
    resolved_schema$additionalProperties <- tbl_schema$additionalProperties %||% TRUE
    resolved_schema$unevaluatedProperties <- tbl_schema$unevaluatedProperties %||% TRUE
    resolved_schema$tbl_props <- tbl_schema$items$properties
    req_props <- tbl_schema$items$required %||% character(0)
    for (rp in req_props) {
      resolved_schema$tbl_props[[rp]][['required']] <- TRUE
    }
    resolved_schema
  }  

  # if we are reading a table schema directly: extract info
  if (!is.null(schema$type) && schema$type == "array") {
    resolved_schema <- extract_tbl_props(schema)
    return(resolved_schema)
  }

  # if we are reading an extended schema: iterate over allOf
  if (!is.null(schema$allOf)) {
    resolved_schema <- list()
    for (k in seq_along(schema$allOf)) {
      subschema <- schema$allOf[[k]]
      if (is.null(subschema[['$ref']])) { # if no ref, extract directly
        res_subschema <- extract_tbl_props(subschema)
      } else { # else resolve ref
        ref_path <- subschema[["$ref"]]
        ref_file <- fs::path_real(file.path(base_path, ref_path))
        res_subschema <- resolve_schema(ref_file)
      }
      # combine the resolved subschemata into one
      resolved_schema <- modifyList(resolved_schema, res_subschema)
    }
    return(resolved_schema)
  }

  # if we are reading a combined schema: iterate over the properties (subschema refs)
  if (!is.null(schema$type) && schema$type == "object"){
    resolved_schema <- list()
    for (k in seq_along(schema$properties)) {
      subschema <- schema$properties[[k]]
      if (is.null(subschema[['$ref']])) { # not tested, current structure has only refs
        res_subschema <- extract_tbl_props(subschema)
      } else { # resolve the refs
        ref_path <- subschema[["$ref"]]
        ref_file <- fs::path_real(file.path(base_path, ref_path))
        res_subschema <- resolve_schema(ref_file)
      }
      # add subschema to main schema
      subschema_name <- names(schema$properties)[[k]]
      resolved_schema[[subschema_name]] <- res_subschema
    }
    # main schema props
    resolved_schema$additionalProperties <- schema$additionalProperties %||% TRUE
    req_tbls <- schema$required %||% character(0)
    for (rt in req_tbls) {
      resolved_schema[[rt]][['required']] <- TRUE
    }
    return(resolved_schema)
  }
  }

#' Validate a dataframe against json schema
#' @param df the data frame to validate.
#' @param schema the name of the QWAmetadata element under question
#' @param warn_only warn or abort on errors
#' @param greedy stop on first error for faster eval
#' @return passes if validation ok
validate_schema <- function(df, schema = c("dataset", "authors", "funding", "related",
                                           "sites", "trees", "woodpieces", "slides", "images"),
                            roxas_version = NULL,
                            warn_only = TRUE, greedy = TRUE){
  schema <- match.arg(schema)
  schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  df_na <- df |> dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x,"")))
  result <- schema_obj$validate(
    jsonlite::toJSON(df_na, na = "null", POSIXt = "ISO8601"),
    verbose = TRUE,
    greedy = greedy
  )
  if (!result) {
    errors <- attr(result, "errors")
    msg <- errors |>
      tibble::as_tibble() |> 
      dplyr::select(instancePath, keyword, params, message) |> 
      dplyr::mutate(
        last_seg = sub("^.*/", "", instancePath), 
        full_message = dplyr::case_when(
          keyword == "type" ~ paste0(message, ": `", last_seg, "`"),
          keyword == "additionalProperties" ~ paste0(message, ": `", params$additionalProperty, "`"),
          keyword == "unevaluatedProperties" ~ paste0(message, ": `", params$unevaluatedProperty, "`"),
          keyword == "enum" ~ paste0(message, ": `", last_seg, "` (", params$allowedValues, ")"),
          .default = message
        )
      ) |> dplyr::pull(full_message) |> unique()
    names(msg) <- rep("*", length(msg))
    if (warn_only) cli::cli_warn(c("!" = glue::glue("QWAmetadata${schema} validation checks failed:"), msg))
    else cli::cli_abort(c("x" = glue::glue("QWAmetadata${schema} validation checks failed:"), msg))
  }
  invisible(result)
}



#' Helper to check the structure columns define a sound hierarchy
#' @param df data frame containing the structure columns image_label, slide_label,
#'   woodpiece_label, tree_label and site_label
#' @keywords internal
check_structure <- function(df){
  valid <- (
    stringr::str_starts(df$image_label, df$slide_label) &
      stringr::str_starts(df$slide_label, df$woodpiece_label) &
      stringr::str_starts(df$woodpiece_label, df$tree_label) &
      stringr::str_starts(df$tree_label, df$site_label)
  )
  if (any(!valid)){
      cli::cli_abort(c(
      "x" = "Invalid data structure for:",
      df$image_label[!valid]
      ))
  }
  invisible(TRUE)
}


#' Helper to find the correct schema files, mapping QWAmetadata slot names 
#' to the package-relative schema file path
#' @param schema QWAmetadata slot name.
#' @param roxas_version Needs to be specified in for slot images only.
#' @return relative path of the schema file.
#' @keywords internal
schema_rel_path <- function(schema, roxas_version = NULL) {
  schema_clean <- schema
  if (schema == "images" && roxas_version == "roxas"){
    schema_clean <- "roxas"
  } else if (schema == "images" && roxas_version == "roxas_ai"){
    schema_clean <- "roxasai"
  }
  switch(schema_clean,
    "dataset" = "extdata/json_schema/base_schema/20251007_tria_ds_data_schema.json",
    "authors" = "extdata/json_schema/base_schema/20251007_tria_author_data_schema.json",
    "funding" = "extdata/json_schema/base_schema/20251007_tria_funding_data_schema.json",
    "related" = "extdata/json_schema/base_schema/20251007_tria_relresource_data_schema.json",
    "sites" = "extdata/json_schema/base_schema/20251007_tria_site_data_schema.json",
    "trees" = "extdata/json_schema/base_schema/20251007_tria_tree_data_schema.json",
    "woodpieces" = "extdata/json_schema/base_schema/20251007_tria_woodpiece_data_schema.json",
    "slides" = "extdata/json_schema/base_schema/20251007_tria_slide_data_schema.json",
    "roxas" = "extdata/json_schema/base_schema/20260313_tria_roxas_data_schema.json",
    "roxasai" = "extdata/json_schema/base_schema/20260313_tria_roxasai_data_schema.json"
  )
}

#' Helper to resolve references to other schemata within a JSON schema
#' @param x a schema (as list)
#' @returns A copy of x where any `$ref` fields have been replaced with the 
#'   actual schema read from the corresponding files
#' @keywords internal
resolve_refs <- function(x, base_path){
  if (!is.list(x)) return(x)
  if (!is.null(x[["$ref"]])) {
    ref_file <- fs::path_real(fs::path(base_path, x[["$ref"]]))
    ref_raw  <- jsonlite::fromJSON(readLines(ref_file, warn = FALSE), simplifyDataFrame = FALSE)
    return(resolve_refs(ref_raw, fs::path_dir(ref_file)))   # recurse with new base_path
  }
  purrr::map(x, resolve_refs, base_path = base_path)
}

#' Helper to resolve (merge) "allOf" entries in JSON schema
#' @param x a schema (as list)
#' @returns A copy of x where all allOf elements have been merged with `modifyList` or
#'   `union` (in the case of `required` keys)
#' @keywords internal
resolve_allOfs <- function(x){
  if (!is.list(x)) return(x)
  if (!is.null(x[["allOf"]])) {
    combined <- x$allOf[[1]]
    for (k in seq_along(x[["allOf"]])[-1]){
      sublist <- x$allOf[[k]]
      combined$required <- union(combined$required %||% character(0),
                                 sublist$required   %||% character(0))
      sublist$required <- NULL
      combined <- modifyList(combined, sublist)
    }
    return(resolve_allOfs(combined))
  }
  purrr::map(x, resolve_allOfs)
}

#' Create an empty dataframe based on the provided JSON schema
#' @param tbl_schema The (resolved) JSON schema defining the structure of the table.
#' @param nrows Number of rows to create (default 0).
#' @returns An "empty" tibble with the correct columns and types as defined in the schema.
#' @keywords internal
create_empty_df <- function(tbl_schema, nrows = 0){
  # Mapping JSON Schema types to R types
  json_to_r_types <- c(
    string = "c",
    integer = "i",
    boolean = "l",
    number = "d",
    date = "D",
    datetime = "T",
    null = "c" # or "NULL"?
  )

  # extract column names and types from schema
  cols_info <- tbl_schema$items$properties
  col_names <- names(cols_info)
  col_types <- sapply(cols_info, function(x) x$type[1]) # NOTE: leverages that default type is in first position in schema!

  # special cases:
  col_formats <- sapply(cols_info, function(x) if (!is.null(x$format)) x$format else "none")
  col_types[which(col_formats == "date-time")] <- "datetime"
  col_types[which(col_formats == "date")] <- "date"

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
#' @keywords internal
convert_column <- function(x, target_class) {
  target_class <- target_class[1]  # use the first class name
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

#' Align dataframes for individual QWAmetadata components to the correct schema
#' @param df The dataframe to be aligned.
#' @param schema the name of the QWAmetadata element under question.
#' @param force_required If TRUE, missing required columns will raise an error; if FALSE, a warning only.
#' @param ignore_colnames If TRUE, the input df column names will be ignored 
#' and replaced with the target structure names by position. DANGER!
#' @returns The (possibly aligned) data frame.
#' @export
align_df_to_schema <- function(df,
                               schema = c("dataset", "authors", "funding", "related",
                                          "sites", "trees", "woodpieces", "slides", "images"),
                               roxas_version = NULL,
                               force_required = TRUE,
                               ignore_colnames = FALSE) {
  checkmate::assert_data_frame(df)
  schema <- match.arg(schema)
  if (schema == "images"){
    checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = FALSE)
  } else {
    checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = TRUE)
  }
  checkmate::assert_logical(c(force_required, ignore_colnames))
  
  schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  # convert the table schema to list and resolve any $refs or allOfs
  tbl_schema <- jsonlite::fromJSON(schema_obj$schema$schema, simplifyDataFrame = FALSE)
  tbl_schema <- tbl_schema |> 
    resolve_refs(fs::path_dir(schema_path)) |> 
    resolve_allOfs()

  target_structure <- create_empty_df(tbl_schema, nrows = 0)
  target_cols <- names(target_structure)

  aligned_data <- df
  tbl_name <- schema

  # force target_cols as names for source_data if ignore_colnames is TRUE
  if (ignore_colnames){
    names(aligned_data)[1:length(target_cols)] <- target_cols
  }

  # check for existence, missing (required, optional) and extra columns
  req_cols <- tbl_schema$items$required
  source_cols <- names(aligned_data)
  missing_cols <- setdiff(req_cols, source_cols)
  missing_opt_cols <- setdiff(target_cols, c(req_cols, source_cols))
  extra_cols <- setdiff(source_cols, target_cols)
  common_cols <- intersect(target_cols, source_cols)

  if (length(common_cols) == 0) {
    msg <- paste("No valid columns found in table", tbl_name)
    if (force_required) {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
      return(create_empty_df(tbl_name, nrows = 1))
    }
  }

  if (length(missing_cols) > 0) {
    msg <- c("x" = glue::glue("Missing required columns in table {schema}:"), missing_cols)
    if (force_required) {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
    }
  }

  if (length(missing_opt_cols) > 0) {
    #msg <- paste("Missing optional columns added to table", tbl_name, ":", paste(missing_opt_cols, collapse = ", "))
    msg <- c("!" = glue::glue("Missing optional columns added to table {schema}:"), missing_opt_cols)
    cli::cli_warn(msg)
  }

  if (length(extra_cols) > 0) {
    #msg <- paste("Extra columns ignored in table", tbl_name)
    msg <- c("!" = glue::glue("Extra columns ignored in table {schema}:"), extra_cols)
    cli::cli_warn(msg)
  }

  # enforce the correct class for all columns in df
  for (col in common_cols) {
    aligned_data[[col]] <- convert_column(aligned_data[[col]], class(target_structure[[col]]))
  }
  # compare NA counts after conversion
  na_counts_org <- colSums(is.na(df[common_cols]))
  na_counts_aligned <- colSums(is.na(aligned_data[common_cols]))
  if (any(na_counts_aligned > na_counts_org)){
    NA_cols <- names(which(na_counts_aligned>na_counts_org))
    #msg <- paste("Forced type conversions led to new NA values in", tbl_name, ":", paste(NA_cols, collapse = ", "))
    msg <- c("!" = glue::glue("Forced type conversions led to new NA values in table {schema}:"), NA_cols)
    cli::cli_warn(msg)
  }

  # bind to target_structure to ensure all columns are present
  aligned_data <- dplyr::bind_rows(target_structure, aligned_data[common_cols])

  aligned_data
}

