

#' Map a QWAmetadata slot name to its package-relative schema file path
#'
#' @param schema QWAmetadata slot name.
#' @param roxas_version Required when `schema = "images"`: `"roxas"` selects
#'   the classic ROXAS schema, `"roxas_ai"` the ROXAS AI schema.
#'   Ignored for all other schema names.
#' @return Package-relative path to the JSON schema file.
#' @noRd
schema_rel_path <- function(schema, roxas_version = NULL) {
  schema_clean <- ifelse(schema == "images", roxas_version, schema)
  switch(schema_clean,
    "QWAmetadata" = "extdata/json_schema/base_schema/20260313_tria_metadata_schema.json",
    "shiny_meta" = "extdata/json_schema/20260402_tria_shinyext_schema.json",
    "dataset" = "extdata/json_schema/base_schema/20251007_tria_ds_data_schema.json",
    "authors" = "extdata/json_schema/base_schema/20251007_tria_author_data_schema.json",
    "funding" = "extdata/json_schema/base_schema/20251007_tria_funding_data_schema.json",
    "related" = "extdata/json_schema/base_schema/20251007_tria_relresource_data_schema.json",
    "resources" = "extdata/json_schema/base_schema/20260331_tria_resource_data_schema.json",
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
#' @noRd
resolve_refs <- function(x, base_path){
  if (!is.list(x)) return(x)
  if (!is.null(x[["$ref"]])) {
    ref_file <- fs::path_real(fs::path(base_path, x[["$ref"]]))
    ref_raw  <- jsonlite::fromJSON(readLines(ref_file, warn = FALSE), simplifyDataFrame = FALSE)
    return(resolve_refs(ref_raw, fs::path_dir(ref_file))) # recurse with new base_path
  }
  purrr::map(x, resolve_refs, base_path = base_path)
}

#' @noRd
resolve_schema <- function(schema_obj, schema_path) {
  tbl_schema  <- jsonlite::fromJSON(schema_obj$schema$schema, simplifyDataFrame = FALSE)
  resolve_refs(tbl_schema, fs::path_dir(schema_path))
}

#' @noRd
extract_required <- function(x) {
  if (!is.list(x)) return(NULL)
  # collect "required" at this level
  current <- if (!is.null(x$required)) list(x$required) else list()
  # recurse into all children and combine
  children <- purrr::map(x, extract_required) 
  c(current, children) |> purrr::list_flatten() |> unname()
}
#' @noRd
merge_props <- function(a, b) {
  new_keys  <- setdiff(names(b), names(a))
  both_keys <- intersect(names(b), names(a))
  
  a[new_keys] <- b[new_keys]
  
  for (key in both_keys) {
    if (is.list(a[[key]]) && is.list(b[[key]])) {
      a[[key]] <- merge_props(a[[key]], b[[key]])  # recurse for sub-lists
    } else {
      a[[key]] <- c(a[[key]], b[[key]])  # concatenate atomics
    }
  }
  a
}
#' @noRd
extract_properties <- function(x) {
  if (!is.list(x)) return(NULL)

  current <- if (!is.null(x$properties)) x$properties else list()
  children <- purrr::map(x, extract_properties) |> purrr::compact()
  all_props <- c(list(current), children)
  purrr::reduce(all_props, merge_props, .init = list())
}

#' Helper to extract specific column definitions from a imported schema
#' @noRd
get_tbl_props <- function(tbl_schema) {
  required <- tbl_schema |> extract_required() |> unlist()
  properties <- tbl_schema |> extract_properties()
  prop_names <- names(properties)
  properties <- lapply(prop_names, function(nm) {
    c(properties[[nm]], list(required = nm %in% required))
  })
  names(properties) <- prop_names
  list(properties = properties, required = required)
}

# tbl_props <- get_tbl_props(full_schema$properties$tbl_name)
# tbl_props <- get_tbl_props(tbl_schema)


#' Create an empty dataframe conforming to a JSON schema
#'
#' @param tbl_props The properties from table schema (i.e. after
#'   resolve_refs() and get_tbl_props(), with `$properties`
#'   containing the column definitions, `$required` the required columns.
#' @param nrows Number of rows to create. If `> 0`, character columns are
#'   filled with `""` and logical columns with `FALSE`.
#' @returns A tibble with columns and R types matching the schema, with
#'   `nrows` rows.
#' @noRd
create_empty_df <- function(tbl_props, nrows = 0) {
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
  cols_info <- tbl_props$properties
  col_names <- names(cols_info)
  col_types <- sapply(cols_info, function(x) x$type[1]) # NOTE: leverages that default type is in first position in schema!

  # special cases:
  col_formats <- sapply(cols_info, function(x) if (!is.null(x$format)) x$format else "none")
  col_types[which(col_formats == "date-time")] <- "datetime"
  col_types[which(col_formats == "date")] <- "date"

  r_col_types <- paste(json_to_r_types[col_types], collapse ="")
  empty_df <- vroom::vroom(I("\n"), col_names = col_names, col_types = r_col_types)

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
#' @noRd
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

#' Helper to force a given df into alignment with the base schema
#' ensure it has required columns, remove any extra columns
#' force all columns to have the correct type
#' @param df  The data frame to be aligned
#' @param tbl_props  Table properties read from schema (output of get_tbl_props()) 
#' @param schema  Schema name, only used for warning messages
#' @param add_opt  Should missing optional columns be added as well, or only required?
#' @param mute_info  Should warning messages (missing req columns, 
#'   dropped extra cols, NAs due to type conversions) be displayed
#' @param ignore_colnames  Should the column names of df be ignored
#'   in favor of position?
#' @noRd
align_to_schema <- function(df, tbl_props, schema = NULL, add_opt = FALSE,
                            mute_info = FALSE, ignore_colnames = FALSE
                            ) {
  # get target empty df to align data to
  target_structure <- create_empty_df(tbl_props, nrows = 0)
  target_cols <- names(target_structure)

  aligned_data <- df

  # force target_cols as names for source_data if ignore_colnames is TRUE
  if (ignore_colnames){
    names(aligned_data)[1:min(length(aligned_data),length(target_cols))] <- target_cols
  }

  req_cols <- if (add_opt) target_cols else tbl_props$required
  source_cols <- names(aligned_data)
  common_cols <- intersect(target_cols, source_cols)

  # enforce the correct class for all columns in df
  for (col in common_cols) {
    aligned_data[[col]] <- convert_column(aligned_data[[col]], class(target_structure[[col]]))
  }
  
  # bind to target_structure to ensure required+common columns are present
  res_cols <- intersect(target_cols, c(req_cols, common_cols)) # ensures correct order
  aligned_data <- dplyr::bind_rows(target_structure[res_cols], aligned_data[common_cols])

  # if not muted, collect messages on changes made
  if (!mute_info) {
    msg <- c()
    
    missing_cols <- setdiff(req_cols, source_cols)
    extra_cols <- setdiff(source_cols, target_cols)

    if (length(missing_cols) > 0) {
      msg <- c(msg, 
        "i" = glue::glue("Missing columns added:"), 
        " " = glue::glue_collapse(missing_cols, sep = ", ")
      )
    }

    if (length(extra_cols) > 0) {
      msg <- c(msg,
        "i" = glue::glue("Extra columns ignored:"), 
        " " = glue::glue_collapse(extra_cols, sep = ", ")
      )
    }

    if (!is.null(df)) {
      # compare NA counts after conversion
      na_counts_org <- colSums(is.na(df[common_cols]))
      na_counts_aligned <- colSums(is.na(aligned_data[common_cols]))
      if (any(na_counts_aligned > na_counts_org)){
        NA_cols <- names(which(na_counts_aligned>na_counts_org))
        msg <- c(msg,
          "i" = glue::glue("Type converted columns caused new NAs:"), 
          " " = glue::glue_collapse(NA_cols, sep = ", ")
        )
      }
    }
    
    if (is.null(df)) {
      cli::cli_warn(c("!" = "{.field {schema}} empty data frame created from schema"))
    } else if (length(msg)>0) {
      cli::cli_warn(c("!" = "{.field {schema}} data frame aligned to schema with {length(msg)%/%2} warning{?s}", msg))
    } else {
      cli::cli_inform(c("i" = "{.field {schema}} data frame aligned with schema"))
    }
  }

  aligned_data
}

# #' Align a data frame to a QWAmetadata component schema
# #'
# #' Coerces column types, drops extra columns, and optionally adds missing
# #' columns to make a data frame conform to the expected schema structure.
# #'
# #' @param df Data frame to align.
# #' @param schema Name of the QWAmetadata component. For `"images"`,
# #'   `roxas_version` must be provided.
# #' @param roxas_version ROXAS software version: `"roxas"` or `"roxas_ai"`.
# #'   Required when `schema = "images"`, ignored otherwise.
# #' @param allow_missing_req If `TRUE` (default), missing required columns
# #'   trigger a warning and are added as `NA`; if `FALSE`, raises an error.
# #' @param add_missing_opt If `TRUE` (default), missing optional columns are
# #'   added as `NA`.
# #' @param mute_info If `TRUE`, suppresses informational messages about missing
# #'   optional columns and extra columns. Warnings about required columns or
# #'   type coercions are always shown.
# #' @param ignore_colnames If `TRUE`, replaces column names with the schema's
# #'   target names by position. Use with caution.
# #' @returns The aligned data frame with schema-compliant column order and types.
# #' @seealso [validate_schema()]
# #' @export
# align_df_to_schema <- function(df,
#                                schema,
#                                roxas_version = NULL,
#                                allow_missing_req = TRUE,
#                                add_missing_opt = TRUE,
#                                mute_info = FALSE,
#                                ignore_colnames = FALSE
#                               ) {
#   checkmate::assert_data_frame(df)
#   checkmate::assert_choice(schema, c("dataset", "authors", "funding", "related", "resources",
#                                      "sites", "trees", "woodpieces", "slides", "images"))
#   if (schema == "images"){
#     checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = FALSE)
#   } 
#   checkmate::assert_logical(c(allow_missing_req, add_missing_opt, mute_info, ignore_colnames))
  
#   # load the table's JSON schema and resolve any $refs or allOfs
#   schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
#   schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
#   tbl_schema <- jsonlite::fromJSON(schema_obj$schema$schema, simplifyDataFrame = FALSE)
#   tbl_schema <- tbl_schema |> 
#     resolve_refs(fs::path_dir(schema_path))
#   tbl_props <- get_tbl_props(tbl_schema)

#   target_structure <- create_empty_df(tbl_props, nrows = 0)
#   target_cols <- names(target_structure)

#   aligned_data <- df

#   # force target_cols as names for source_data if ignore_colnames is TRUE
#   if (ignore_colnames){
#     names(aligned_data)[1:min(length(aligned_data),length(target_cols))] <- target_cols
#   }

#   # check for existence, missing (required, optional) and extra columns
#   req_cols <- tbl_props$required
#   source_cols <- names(aligned_data)
#   missing_req_cols <- setdiff(req_cols, source_cols)
#   missing_opt_cols <- setdiff(target_cols, c(req_cols, source_cols))
#   extra_cols <- setdiff(source_cols, target_cols)
#   common_cols <- intersect(target_cols, source_cols)

#   msg <- c()

#   if (length(missing_req_cols) > 0) {
#     msg <- c(msg, 
#       "i" = glue::glue("Missing required columns{ifelse(allow_missing_req,' added','')}:"), 
#       " " = glue::glue_collapse(missing_req_cols, sep = ", ")
#     )
#     if (!allow_missing_req) {
#       cli::cli_abort(c("x" = "{.field {schema}} data frame could not be aligned to schema.", msg))
#     }
#   }

#   if (length(missing_opt_cols) > 0 && !mute_info) {
#     msg <- c(msg,
#       "i" = glue::glue("Missing optional columns{ifelse(add_missing_opt,' added','')}:"),
#       " " = glue::glue_collapse(missing_opt_cols, sep = ", ")
#     )
#   }

#   if (length(extra_cols) > 0) {
#     msg <- c(msg,
#       "i" = glue::glue("Extra columns ignored:"), 
#       " " = glue::glue_collapse(extra_cols, sep = ", ")
#     )
#   }

#   # enforce the correct class for all columns in df
#   for (col in common_cols) {
#     aligned_data[[col]] <- convert_column(aligned_data[[col]], class(target_structure[[col]]))
#   }
#   # compare NA counts after conversion
#   na_counts_org <- colSums(is.na(df[common_cols]))
#   na_counts_aligned <- colSums(is.na(aligned_data[common_cols]))
#   if (any(na_counts_aligned > na_counts_org)){
#     NA_cols <- names(which(na_counts_aligned>na_counts_org))
#     msg <- c(msg,
#       "i" = glue::glue("Type converted columns with new NAs:"), 
#      " " = glue::glue_collapse(NA_cols, sep = ", ")
#     )
#   }

#   if (length(msg)>0){
#     cli::cli_warn(c("!" = "{.field {schema}} data frame aligned to schema with {length(msg)%/%2} warning{?s}", msg))
#   } else if (!mute_info) {
#     cli::cli_inform(c("i" = "{.field {schema}} data frame in line with  schema."))
#   }
  
#   # bind to target_structure to ensure all / required+common columns are present
#   if (add_missing_opt){
#     res_cols <- target_cols
#   } else{
#     res_cols <- intersect(target_cols, c(req_cols, common_cols)) # ensures correct order
#   }
#   aligned_data <- dplyr::bind_rows(target_structure[res_cols], aligned_data[common_cols])

#   aligned_data
# }

# #' Validate a data frame against its JSON schema
# #'
# #' @param df Data frame to validate (slot of [QWAmetadata()] object).
# #' @param schema Name of the QWAmetadata component. For `"images"`,
# #'   `roxas_version` must be provided to select the correct schema.
# #' @param roxas_version ROXAS software version: `"roxas"` or `"roxas_ai"`.
# #'   Required when `schema = "images"`, ignored otherwise.
# #' @param warn_only If `TRUE` (default), validation failures raise a warning;
# #'   if `FALSE`, they raise an error.
# #' @param greedy If `TRUE` (default), stops on the first validation error;
# #'   if `FALSE`, collects all errors.
# #' @return Invisibly returns the validation result.
# #' @seealso [align_df_to_schema()]
# validate_schema <- function(df, schema,
#                             roxas_version = NULL,
#                             warn_only = TRUE, greedy = TRUE){
#   checkmate::assert_data_frame(df)
#   checkmate::assert_choice(schema, c("dataset", "authors", "funding", "related", "resources",
#                                      "sites", "trees", "woodpieces", "slides", "images"))
#   if (schema == "images"){
#     checkmate::assert_subset(roxas_version, c("roxas", "roxas_ai"), empty.ok = FALSE)
#   } 
#   checkmate::assert_flag(warn_only)
#   checkmate::assert_flag(greedy)

#   schema_path <- system.file(schema_rel_path(schema, roxas_version), package = "rxs2tria")
#   schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
#   # replace NA_character_ in df with "" for compliance with jsonvalidate logic
#   df_na <- df |> 
#     dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x,"")))
#   result <- schema_obj$validate(
#     jsonlite::toJSON(df_na, na = "null", POSIXt = "ISO8601"),
#     verbose = TRUE,
#     greedy = greedy
#   )

#   # if validation resulted in any errors, format the issues into a single message
#   if (!result) {
#     errors <- attr(result, "errors")
#     msg <- errors |>
#       tibble::as_tibble() |> 
#       dplyr::select(instancePath, keyword, params, message) |> 
#       dplyr::mutate(
#         last_seg = sub("^.*/", "", instancePath),
#         full_message = dplyr::case_when(
#           keyword == "type" ~ paste0(message, ": `", last_seg, "`"),
#           keyword == "additionalProperties" ~ paste0(message, ": `", params$additionalProperty, "`"),
#           keyword == "unevaluatedProperties" ~ paste0(message, ": `", params$unevaluatedProperty, "`"),
#           keyword == "enum" ~ paste0(message, ": `", last_seg, "` (", params$allowedValues, ")"),
#           .default = paste0(message, ": `", last_seg, "`")
#         )
#       ) |> dplyr::pull(full_message) |> unique()
#     names(msg) <- rep("*", length(msg))
#     if (warn_only) {
#       cli::cli_warn(c("!" = "{.field {schema}} data frame failed validation checks:", msg))
#     } else {
#      cli::cli_abort(c("x" = "{.field {schema}} data frame failed validation checks:", msg))
#     }
#   }
#   invisible(result)
# }


#' Helper to check df against schema
#' @noRd
check_schema <- function(df, schema_obj, schema = NULL, warn_only = TRUE, greedy = FALSE) {
  # convert NA_character to empty strings for $validate to work correctly
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
#' @noRd
check_structure <- function(df) {
  checkmate::assert_character(df$image_label, unique = TRUE)
  valid <- (
    stringr::str_starts(df$image_label, df$slide_label) &
      stringr::str_starts(df$slide_label, df$woodpiece_label) &
      stringr::str_starts(df$woodpiece_label, df$tree_label) &
      stringr::str_starts(df$tree_label, df$site_label)
  )
  if (any(!valid)) {
    cli::cli_abort(c(
      "x" = "Invalid data structure for:",
      df$image_label[!valid]
    ))
  }
  invisible(TRUE)
}

#' @noRd
check_missing_opt <- function(df, tbl_props, schema = NULL) {
  missing_opt_cols <- setdiff(
    names(tbl_props$properties), 
    c(tbl_props$required, names(df))
  )
  if (length(missing_opt_cols)>0) {
    cli::cli_warn(c(
      "!" = "{.field {schema}} missing optional columns:",
      "i" = "{missing_opt_cols}"
    ))
  }
  invisible(TRUE)
}





# TODO: other additional validation checks beyond the base schema
