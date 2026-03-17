#' Map a QWAmetadata slot name to its package-relative schema file path
#'
#' @param schema QWAmetadata slot name.
#' @param roxas_version Required when `schema = "images"`: `"roxas"` selects
#'   the classic ROXAS schema, `"roxas_ai"` the ROXAS AI schema.
#'   Ignored for all other schema names.
#' @return Package-relative path to the JSON schema file.
#' @keywords internal
schema_rel_path <- function(schema, roxas_version = NULL) {
  schema_clean <- ifelse(schema == "images", roxas_version, schema)
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
    "roxas_ai" = "extdata/json_schema/base_schema/20260313_tria_roxasai_data_schema.json"
  )
}

#' Inline all references in a JSON schema list
#'
#' Recursively walks the schema list and replaces every node of the form
#' `{"$ref": "path/to/schema.json"}` with the content of the referenced file.
#' Relative paths are resolved against `base_path`; the base path is updated
#' when descending into a referenced file so nested refs resolve correctly.
#'
#' @param x A schema as a nested R list.
#' @param base_path Directory used to resolve relative `$ref` paths (from where `x` was loaded).
#' @returns A copy of `x` with all `$ref` nodes replaced by their referenced content.
#' @keywords internal
resolve_refs <- function(x, base_path){
  if (!is.list(x)) return(x)
  if (!is.null(x[["$ref"]])) {
    ref_file <- fs::path_real(fs::path(base_path, x[["$ref"]]))
    ref_raw  <- jsonlite::fromJSON(readLines(ref_file, warn = FALSE), simplifyDataFrame = FALSE)
    return(resolve_refs(ref_raw, fs::path_dir(ref_file))) # recurse with new base_path
  }
  purrr::map(x, resolve_refs, base_path = base_path)
}

#' Merge all `allOf` compositions in a JSON schema list
#'
#' Recursively walks the schema list and merges every `allOf` array into a
#' single object. Properties are merged with [modifyList()] (i.e., later entries
#' override earlier ones - but in the `rxs2tria` schemata, components should be distinct by definition).
#' `required` arrays are combined so that required fields from all subschemas are preserved.
#'
#' @param x A schema as a nested R list, typically after [resolve_refs()].
#' @returns A copy of `x` with all `allOf` arrays merged into their parent objects.
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
      combined <- modifyList(combined, sublist) # TODO: better merge logic?
    }
    return(resolve_allOfs(combined))
  }
  purrr::map(x, resolve_allOfs)
}

#' Create an empty dataframe conforming to a JSON schema
#'
#' @param tbl_schema A fully resolved table schema list (i.e. after
#'   [resolve_refs()] and [resolve_allOfs()]), with `$items$properties`
#'   containing the column definitions.
#' @param nrows Number of rows to create. If `> 0`, character columns are
#'   filled with `""` and logical columns with `FALSE`.
#' @returns A tibble with columns and R types matching the schema, with
#'   `nrows` rows.
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

#' Align a data frame to a QWAmetadata component schema
#'
#' Coerces column types, drops extra columns, and optionally adds missing
#' columns to make a data frame conform to the expected schema structure.
#'
#' @param df Data frame to align.
#' @param schema Name of the QWAmetadata component. For `"images"`,
#'   `roxas_version` must be provided.
#' @param roxas_version ROXAS software version: `"roxas"` or `"roxas_ai"`.
#'   Required when `schema = "images"`, ignored otherwise.
#' @param allow_missing_req If `TRUE` (default), missing required columns
#'   trigger a warning and are added as `NA`; if `FALSE`, raises an error.
#' @param add_missing_opt If `TRUE` (default), missing optional columns are
#'   added as `NA`.
#' @param mute_info If `TRUE`, suppresses informational messages about missing
#'   optional columns and extra columns. Warnings about required columns or
#'   type coercions are always shown.
#' @param ignore_colnames If `TRUE`, replaces column names with the schema's
#'   target names by position. Use with caution.
#' @returns The aligned data frame with schema-compliant column order and types.
#' @seealso [validate_schema()]
#' @export
align_df_to_schema <- function(df,
                               schema,
                               roxas_version = NULL,
                               allow_missing_req = TRUE,
                               add_missing_opt = TRUE,
                               mute_info = FALSE,
                               ignore_colnames = FALSE
                              ) {
  checkmate::assert_data_frame(df)
  checkmate::assert_choice(schema, c("dataset", "authors", "funding", "related",
                                     "sites", "trees", "woodpieces", "slides", "images"))
  if (schema == "images"){
    checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = FALSE)
  } 
  checkmate::assert_logical(c(allow_missing_req, add_missing_opt, mute_info, ignore_colnames))
  
  # load the table's JSON schema and resolve any $refs or allOfs
  schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- jsonlite::fromJSON(schema_obj$schema$schema, simplifyDataFrame = FALSE)
  tbl_schema <- tbl_schema |> 
    resolve_refs(fs::path_dir(schema_path)) |> 
    resolve_allOfs()

  target_structure <- create_empty_df(tbl_schema, nrows = 0)
  target_cols <- names(target_structure)

  aligned_data <- df

  # force target_cols as names for source_data if ignore_colnames is TRUE
  if (ignore_colnames){
    names(aligned_data)[1:min(length(aligned_data),length(target_cols))] <- target_cols
  }

  # check for existence, missing (required, optional) and extra columns
  req_cols <- tbl_schema$items$required
  source_cols <- names(aligned_data)
  missing_req_cols <- setdiff(req_cols, source_cols)
  missing_opt_cols <- setdiff(target_cols, c(req_cols, source_cols))
  extra_cols <- setdiff(source_cols, target_cols)
  common_cols <- intersect(target_cols, source_cols)

  # TODO: this is a special case for the shiny only? so might call create_empty directly for null components
  # if (length(common_cols) == 0) {
  #   msg <- c("x" = glue::glue("No valid columns found in {schema}:"), common_cols)
  #   if (allow_missing_req) {
  #     cli::cli_warn(msg)
  #     return(create_empty_df(tbl_name, nrows = 1))
  #   } else {
  #     cli::cli_warn(msg)
  #   }
  # }
  msg <- c()

  if (length(missing_req_cols) > 0) {
    msg <- c(msg, 
      "i" = glue::glue("Missing required columns{ifelse(allow_missing_req,' added','')}:"), 
      " " = glue::glue_collapse(missing_req_cols, sep = ", ")
    )
    if (!allow_missing_req) {
      cli::cli_abort(c("x" = "{.field {schema}} data frame could not be aligned to schema.", msg))
    }
  }

  if (length(missing_opt_cols) > 0 && !mute_info) {
    msg <- c(msg,
      "i" = glue::glue("Missing optional columns{ifelse(add_missing_opt,' added','')}:"),
      " " = glue::glue_collapse(missing_opt_cols, sep = ", ")
    )
  }

  if (length(extra_cols) > 0) {
    msg <- c(msg,
      "i" = glue::glue("Extra columns ignored:"), 
      " " = glue::glue_collapse(extra_cols, sep = ", ")
    )
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
    msg <- c(msg,
      "i" = glue::glue("Type converted columns with new NAs:"), 
     " " = glue::glue_collapse(NA_cols, sep = ", ")
    )
  }

  if (length(msg)>0){
    cli::cli_warn(c("!" = "{.field {schema}} data frame aligned to schema with {length(msg)%/%2} warning{?s}", msg))
  } else if (!mute_info) {
    cli::cli_inform(c("i" = "{.field {schema}} date frame in line with  schema."))
  }
  
  # bind to target_structure to ensure all / required+common columns are present
  if (add_missing_opt){
    res_cols <- target_cols
  } else{
    res_cols <- intersect(target_cols, c(req_cols, common_cols)) # ensures correct order
  }
  aligned_data <- dplyr::bind_rows(target_structure[res_cols], aligned_data[common_cols])

  aligned_data
}

#' Validate a data frame against its JSON schema
#'
#' @param df Data frame to validate (slot of [QWAmetadata()] object).
#' @param schema Name of the QWAmetadata component. For `"images"`,
#'   `roxas_version` must be provided to select the correct schema.
#' @param roxas_version ROXAS software version: `"roxas"` or `"roxas_ai"`.
#'   Required when `schema = "images"`, ignored otherwise.
#' @param warn_only If `TRUE` (default), validation failures raise a warning;
#'   if `FALSE`, they raise an error.
#' @param greedy If `TRUE` (default), stops on the first validation error;
#'   if `FALSE`, collects all errors.
#' @return Invisibly returns the validation result.
#' @seealso [align_df_to_schema()]
validate_schema <- function(df, schema,
                            roxas_version = NULL,
                            warn_only = TRUE, greedy = TRUE){
  checkmate::assert_data_frame(df)
  checkmate::assert_choice(schema, c("dataset", "authors", "funding", "related",
                                     "sites", "trees", "woodpieces", "slides", "images"))
  if (schema == "images"){
    checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = FALSE)
  } 
  checkmate::assert_logical(c(warn_only, greedy))

  schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  # replace NA_character_ in df with "" for compliance with jsonvalidate logic
  df_na <- df |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x,"")))
  result <- schema_obj$validate(
    jsonlite::toJSON(df_na, na = "null", POSIXt = "ISO8601"),
    verbose = TRUE,
    greedy = greedy
  )

  # if validation resulted in any errors, format the issues into a single message
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
          .default = paste0(message, ": `", last_seg, "`")
        )
      ) |> dplyr::pull(full_message) |> unique()
    names(msg) <- rep("*", length(msg))
    if (warn_only) {
      cli::cli_warn(c("!" = "{.field {schema}} data frame failed validation checks:", msg))
    } else {
     cli::cli_abort(c("x" = "{.field {schema}} data frame failed validation checks:", msg))
    }
  }
  invisible(result)
}

#' Helper to check the structure columns define a sound hierarchy
#' @param df data frame containing the structure columns image_label, slide_label,
#'   woodpiece_label, tree_label and site_label
#' @keywords internal
check_structure <- function(df){
  checkmate::assert_character(df$image_label, unique = TRUE)
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

# TODO: other additional validation checks beyond the base schema



# #' Read and resolve JSON schema from file
# #'
# #' The correct structure for [QWAdata()] and [QWAmetadata()] objects are defined in
# #' JSON schema files attached to the `rxs2tria` package. This function reads a
# #' JSON schema file and resolves it into a structured list.
# #' For table schemas (i.e., the dataframe in the individual slots, `"type": "array"`),
# #' returns a list with `$tbl_props` (named list of column definitions plus `required` flag).
# #' For combined schemas (`"type": "object"` where each property is a `$ref`
# #' to a table schema), returns a named list of per-component resolved schemas.
# #'
# #' @param schema_path Path to the JSON schema file.
# #' @return For table schemas: a list with `$tbl_props`, `$minItems`,
# #'   `$maxItems`, `$additionalProperties`, `$unevaluatedProperties`.
# #'   For combined schemas: a named list of per-table resolved schemas.
# #' @seealso [resolve_refs()], [resolve_allOfs()], [align_df_to_schema()]
# #' @internal
# resolve_schema <- function(schema_path) {
#   # Determine base path for resolving relative references
#   base_path <- dirname(schema_path)

#   # load schema from file
#   obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
#   schema <- jsonlite::fromJSON(obj$schema$schema, simplifyDataFrame = FALSE)

#   # helper function to extract table properties from base table schema
#   extract_tbl_props <- function(tbl_schema){
#     resolved_schema <- list()
#     stopifnot(tbl_schema$type == "array")
#     resolved_schema$minItems <- tbl_schema$minItems
#     resolved_schema$maxItems <- tbl_schema$maxItems
#     resolved_schema$additionalProperties <- tbl_schema$additionalProperties %||% TRUE
#     resolved_schema$unevaluatedProperties <- tbl_schema$unevaluatedProperties %||% TRUE
#     resolved_schema$tbl_props <- tbl_schema$items$properties
#     req_props <- tbl_schema$items$required %||% character(0)
#     for (rp in req_props) {
#       resolved_schema$tbl_props[[rp]][['required']] <- TRUE
#     }
#     resolved_schema
#   }  

#   # if we are reading a table schema directly: extract info
#   if (!is.null(schema$type) && schema$type == "array") {
#     resolved_schema <- extract_tbl_props(schema)
#     return(resolved_schema)
#   }

#   # if we are reading an extended schema: iterate over allOf
#   if (!is.null(schema$allOf)) {
#     resolved_schema <- list()
#     for (k in seq_along(schema$allOf)) {
#       subschema <- schema$allOf[[k]]
#       if (is.null(subschema[['$ref']])) { # if no ref, extract directly
#         res_subschema <- extract_tbl_props(subschema)
#       } else { # else resolve ref
#         ref_path <- subschema[["$ref"]]
#         ref_file <- fs::path_real(file.path(base_path, ref_path))
#         res_subschema <- resolve_schema(ref_file)
#       }
#       # combine the resolved subschemata into one
#       resolved_schema <- modifyList(resolved_schema, res_subschema)
#     }
#     return(resolved_schema)
#   }

#   # if we are reading a combined schema: iterate over the properties (subschema refs)
#   if (!is.null(schema$type) && schema$type == "object"){
#     resolved_schema <- list()
#     for (k in seq_along(schema$properties)) {
#       subschema <- schema$properties[[k]]
#       if (is.null(subschema[['$ref']])) { # not tested, current structure has only refs
#         res_subschema <- extract_tbl_props(subschema)
#       } else { # resolve the refs
#         ref_path <- subschema[["$ref"]]
#         ref_file <- fs::path_real(file.path(base_path, ref_path))
#         res_subschema <- resolve_schema(ref_file)
#       }
#       # add subschema to main schema
#       subschema_name <- names(schema$properties)[[k]]
#       resolved_schema[[subschema_name]] <- res_subschema
#     }
#     # main schema props
#     resolved_schema$additionalProperties <- schema$additionalProperties %||% TRUE
#     req_tbls <- schema$required %||% character(0)
#     for (rt in req_tbls) {
#       resolved_schema[[rt]][['required']] <- TRUE
#     }
#     return(resolved_schema)
#   }
# }