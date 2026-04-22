# Helpers ----
# Get roxas version from images data frame ($software column)
#' @noRd
infer_roxas_version <- function(data, warn_only = FALSE) {
  valid <- c("roxas", "roxas_ai")
  if (!("software" %in% names(data))){
    msg <- "Cannot infer {.arg roxas_version}: {.var data} has no {.var software} column."
    if (warn_only) cli::cli_warn(msg)
    else cli::cli_abort(msg)
  }
  vals <- unique(stats::na.omit(data$software))
  ok <- intersect(vals, valid)
  if (length(ok) != 1) {
    msg <- c("Cannot infer {.arg roxas_version} from {.var data$software}.",
            "i" = "Found: {.val {if (length(vals) == 0) NA else vals}}",
            "i" = "Expected exactly one of: {.val {valid}}")
    if (warn_only) cli::cli_warn(msg)
    else cli::cli_abort(msg)
  }
  ok
}

# Internal S3 constructor ----
#' @noRd
new_QWAimages <- function(data, roxas_version) {
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  structure(
    data,
    class = c("QWAimages", class(data)),
    roxas_version = roxas_version
  )
}

# Public constructor -----
#' Create a QWAimages object
#'
#' `QWAimages` is a subclass of `data.frame` that holds image-level metadata
#' for a QWA dataset. It is the required component of [QWAmetadata] and can be
#' used as input for [collect_raw_data()].
#'
#' Each row represents one image with its associated ROXAS (AI) output file paths,
#' hierarchical labels (site, tree, woodpiece, slide, image), EXIF metadata,
#' and ROXAS (AI) settings. It is typically constructed by [build_QWAimages()].
#'
#' The `roxas_version` attribute records whether the data were produced by
#' classic ROXAS (`"roxas"`) or ROXAS AI (`"roxas_ai"`).
#'
#' @param data A data frame with image-level metadata. Is aligned with the required
#'   structure for QWAimages objects and validated against the corresponding JSON schema. 
#' @param roxas_version The ROXAS software version: `"roxas"` or `"roxas_ai"`.
#'   If `NULL`, inferred from the `$software` column of `data`.
#'
#' @returns An `QWAimages` object with a `roxas_version` attribute.
#'
#' @seealso [QWAmetadata()], [build_QWAimages()], [read_QWAimages()]
#' @export
QWAimages <- function(data, roxas_version = NULL) {
  # ensure valid roxas_version can be established:
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"), null.ok = TRUE)
  rv_param <- roxas_version
  rv_attr <- attr(data, "roxas_version")
  checkmate::assert_choice(rv_attr, c("roxas","roxas_ai"), null.ok = TRUE)
  rv_data <- if (is.null(rv_param) && is.null(rv_attr)) infer_roxas_version(data) else NULL
  if (length(unique(c(rv_param,rv_attr,rv_data))) > 1) {
    cli::cli_abort(c(
      "x" = "Conflicting {.arg roxas_version} values. Check input data."
    ))
  }
  roxas_version <- rv_param %||% rv_attr %||% rv_data 

  # prep data:
  data <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)

  schema_path <- system.file(schema_rel_path(roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  # align to schema: add missing req cols, force correct type, remove extra cols
  # warn if this causes changes to data
  data <- align_to_schema(data, tbl_props, roxas_version)

  # minimal validation checks:
  # abort if structure invalid
  check_structure(data)
 
  # construct object:
  new_QWAimages(data, roxas_version)
}

# Methods (general and specific) ------
#' Print summary of a QWAimages object
#'
#' Displays a compact overview of a [QWAimages] object: the ROXAS software
#' version, dimensions, and coverage (woodpieces, slides, images, year range).
#'
#' @param x A `QWAimages` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAimages()]
#' @export
summary.QWAimages <- function(x, ...) {
  rv <- attr(x, "roxas_version")
  n_imgs <- nrow(x)
  n_slides <- length(unique(x$slide_label))
  n_wps <- length(unique(x$woodpiece_label))
  n_trees <- length(unique(x$tree_label))
    sites <- unique(x$site_label)
  species <- unique(x$species_code)
  yrs <- range(x$outmost_year, na.rm = TRUE)
  
  cli::cli_h3("{.cls QWAimages}")
  cli::cli_bullets(c(
    "*" = "software: {.val {rv}}",
    "*" = "{n_imgs} images from {n_slides} slides > {n_wps} woodpieces > {n_trees} trees",
    "*" = "{length(sites)} site{?s}: {.val {sites}}",
    "*" = "{length(species)} species: {.val {species}}",
    "*" = "outmost years coverage: {yrs[1]}\u2013{yrs[2]}"
  ))

  invisible(x)
}

#' Validate a QWAimages object
#'
#' Runs a series of checks on a [QWAimages] object and warns about any issues
#' found. Checks include: compliance with the base JSON schema, consistency of
#' the `roxas_version` attribute with the `$software` column, missing optional
#' columns, and invalid data structure.
#'
#' @param x A [QWAimages] object.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found;
#'   an error is thrown if the object structure is invalid.
#' @seealso [QWAimages()], [check_QWAmetadata()]
#' @export
check_QWAimages <- function(x) {
  checkmate::assert_class(x, c("QWAimages", "data.frame"))
  roxas_version <- attr(x, "roxas_version")
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))

  schema_path <- system.file(schema_rel_path(roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  
  # extended validation checks:
  # compliance with base schema
  check_schema(x, schema_obj, roxas_version, warn_only = TRUE, greedy = FALSE)
  # warn if roxas_version mismatch
  check_roxas_version(x)
  # warn if missing optional columns
  check_missing_opt(x, tbl_props, roxas_version)

  # abort if structure invalid
  check_structure(x)

  # TODO: any additional checks?

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}


#' Complete a QWAimages object
#'
#' Add any missing columns to a [QWAimages] object, including optional columns.
#' New columns are intitialized as `NA`. 
#'
#' @param x A [QWAimages] object.
#' @returns A [QWAimages] object extended with all possible columns.
#' @seealso [complete_QWAmetadata()], [QWAimages()]
#' @export
complete_QWAimages <- function(x) {
  checkmate::assert_class(x, "QWAimages")
  roxas_version <- attr(x, "roxas_version")
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))

  schema_path <- system.file(schema_rel_path(roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  df <- align_to_schema(x, tbl_props, roxas_version, add_opt = TRUE)

  new_QWAimages(df, roxas_version)
}



#' Write a QWAimages object to a CSV file. 
#'
#' @param x A [QWAimages] object.
#' @param file Path to the output file.
#' @param compress If `TRUE`, write compressed `.gz` file (default `FALSE`). 
#'   Note that if you add an compression-related extension to the file name, 
#'   vroom will automatically compress the output regardless of this parameter.
#' @param overwrite Allow to overwrite existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAimages()], [QWAimages]
#' @export
write_QWAimages <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAimages")

  # if compress, enforce .gz extension
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")
  
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  vroom::vroom_write(x, file, delim = ",")
  cli::cli_inform(c("v" = "QWAimages written to {.file {file}}"))
  invisible(file)
}


#' Read a QWAimages object from a CSV file
#'
#' Reads image-level metadata from a (compressed) CSV file written by
#' [write_QWAimages()] and returns a [QWAimages] object. The `roxas_version`
#' is inferred from the `software` column of the data.
#'
#' @param file Path to a `.csv` or `.csv.gz` file.
#' @returns A [QWAimages] object.
#' @seealso [write_QWAimages()]
#' @export
read_QWAimages <- function(file) {
  checkmate::assert_file_exists(file)

  df <- vroom::vroom(file, show_col_types = FALSE)
  aligned_data <- QWAimages(df)
  cli::cli_inform(c("v" = "QWAimages read from {.file {file}}"))
  aligned_data
}