# Helpers ----
#' @noRd
# infer roxas_version from $software column; returns NULL with warning if ambiguous
infer_rv_from_data <- function(x) {
  valid <- c("roxas", "roxas_ai")
  if (!("software" %in% names(x))) return(NULL)
  vals <- unique(stats::na.omit(tolower(x$software)))
  ok <- intersect(vals, valid)
  if (length(ok) == 1) return(ok)
  if (length(ok) > 1)
    cli::cli_warn(c(
      "!" = "Mixed {.var software} values in data: {.val {ok}}.",
      "i" = "Data should contain only one ROXAS software version."
    ))
  NULL
}

#' @noRd
# resolve roxas_version from attr > param > data for a QWAimages object or data.frame
# warn if: 
#  - rv_infer cannot be uniquely inferred from data,
#  - rv_attr and rv_infer do not match (attr wins),
# error if:
#  - rv_attr and rv_param mismatch
#  - rv_param and rv_infer mismatch
#  - all are NULL
resolve_roxas_version <- function(x, rv_param = NULL) {
  valid <- c("roxas", "roxas_ai")
  checkmate::assert_choice(rv_param, valid, null.ok = TRUE)
  rv_attr <- attr(x, "roxas_version")
  checkmate::assert_choice(rv_attr, valid, null.ok = TRUE)
  rv_infer <- infer_rv_from_data(x)

  if (!is.null(rv_attr)) {
    if (!is.null(rv_param) && rv_param != rv_attr)
      cli::cli_abort(c(
        "Supplied {.arg roxas_version} ({.val {rv_param}}) conflicts with
         the object's {.arg roxas_version} attribute ({.val {rv_attr}}).",
        "i" = "Rebuild the  {.val QWAimages} object or correct the input data."
      ))
    if (!is.null(rv_infer) && rv_infer != rv_attr)
      cli::cli_warn(c(
        "!" = "{.arg roxas_version} attribute ({.val {rv_attr}}) does not match
               the {.var software} column ({.val {rv_infer}}).",
        "i" = "Check your data for mixed or incorrect software values."
      ))
    return(rv_attr)
  }

  if (!is.null(rv_param) && !is.null(rv_infer) && rv_param != rv_infer)
    cli::cli_abort(c(
      "Supplied {.arg roxas_version} ({.val {rv_param}}) conflicts with
       the value inferred from {.field $software} ({.val {rv_infer}}).",
      "i" = "Check that {.arg roxas_version} matches the {.field software} column."
    ))

  rv <- rv_param %||% rv_infer
  if (is.null(rv))
    cli::cli_abort(c(
      "Cannot determine {.arg roxas_version}.",
      "i" = "Supply it explicitly or ensure the data has a valid {.field software} column."
    ))
  rv
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

# Internal S3 validator ----
#' Abort if a QWAimages object violates the class invariants
#'
#' Valid `roxas_version` attribute, equal to `$software` in all rows; structure
#' labels present, character and not `NA`; `image_label` unique.
#' @returns `x`, invisibly.
#' @noRd
validate_QWAimages <- function(x) {
  checkmate::assert_class(x, c("QWAimages", "data.frame"))
  rv <- attr(x, "roxas_version")
  if (!checkmate::test_choice(rv, c("roxas", "roxas_ai"))) {
    cli::cli_abort(c(
      "x" = "{.cls QWAimages}: invalid {.arg roxas_version} attribute {.val {rv}}.",
      "i" = "Rebuild the object with {.fn QWAimages}."
    ))
  }
  validate_keys(x, key_props(rv), "QWAimages", unique_by = "image_label")
  if (!"software" %in% names(x) || !all(x$software %in% rv)) {
    cli::cli_abort(c(
      "x" = "{.cls QWAimages}: {.field software} must be {.val {rv}} (the {.arg roxas_version} attribute) in all rows.",
      "i" = "Rebuild the object with {.fn QWAimages}."
    ))
  }
  invisible(x)
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
#' @returns A `QWAimages` object with a `roxas_version` attribute.
#'
#' @seealso [QWAmetadata()], [build_QWAimages()], [read_QWAimages()]
#' @export
QWAimages <- function(data, roxas_version = NULL) {
  roxas_version <- resolve_roxas_version(data, rv_param = roxas_version)

  # prep data:
  data <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)

  # software always equals roxas_version (case changes are not reported)
  n_changed <- sum(!is.na(data$software) & tolower(data$software) != roxas_version)
  if (n_changed > 0) {
    cli::cli_warn(c("!" = "{.field software} set to {.val {roxas_version}} in {n_changed} row{?s}."))
  }
  data$software <- roxas_version

  schema_path <- system.file(schema_rel_path(roxas_version), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  # align to schema: add missing req cols, force correct type, remove extra cols
  # warn if this causes changes to data
  data <- align_to_schema(data, tbl_props, roxas_version)

  x <- validate_QWAimages(new_QWAimages(data, roxas_version))
  # warn if label prefixes do not match
  check_structure(x)
  x
}

# Methods (general and specific) ------
#' @param x A `QWAimages` object.
#' @export
#' @rdname summary.QWAimages
print.QWAimages <- function(x, ...) {
  NextMethod()
  invisible(x)
}


#' Print summary of a QWAimages object
#'
#' Displays a compact overview of a [QWAimages] object: the ROXAS software
#' version, dimensions, and coverage (woodpieces, slides, images, year range).
#'
#' @param object A `QWAimages` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `object`, invisibly.
#'
#' @seealso [QWAimages()]
#' @export
summary.QWAimages <- function(object, ...) {
  rv <- attr(object, "roxas_version")
  n_imgs <- nrow(object)
  n_slides <- length(unique(object$slide_label))
  n_wps <- length(unique(object$woodpiece_label))
  n_trees <- length(unique(object$tree_label))
  sites <- unique(object$site_label)
  species <- unique(object$species_code)
  yrs <- range(object$outmost_year, na.rm = TRUE)
  
  cli::cli_h3("{.cls QWAimages}")
  cli::cli_bullets(c(
    "*" = "software: {.val {rv}}",
    "*" = "{n_imgs} images from {n_slides} slides > {n_wps} woodpieces > {n_trees} trees",
    "*" = "{length(sites)} site{?s}: {.val {sites}}",
    "*" = "{length(species)} species: {.val {species}}",
    "*" = "outmost years coverage: {yrs[1]}\u2013{yrs[2]}"
  ))

  invisible(object)
}

#' Validate a QWAimages object
#'
#' Runs a series of checks on a [QWAimages] object and warns about any issues
#' found. Aborts if the object is invalid (structure labels missing or `NA`,
#' duplicate `image_label`s, `$software` not matching the `roxas_version`
#' attribute). Checks include: compliance with the JSON schema of the given
#' stage and a valid data structure. Missing optional columns are reported as
#' information.
#'
#' @param x A [QWAimages] object.
#' @param submission Check against the schema of submission files (`TRUE`)
#'   or TRIA dataset files (`FALSE`)? Set by [check_QWAmetadata()].
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found;
#'   an error is thrown if the object structure is invalid.
#' @seealso [QWAimages()], [check_QWAmetadata()]
#' @export
check_QWAimages <- function(x, submission = TRUE) {
  validate_QWAimages(x)
  checkmate::assert_flag(submission)
  roxas_version <- attr(x, "roxas_version")

  schema_path <- system.file(schema_rel_path(roxas_version, submission), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  
  # extended validation checks:
  # compliance with the schema of the stage
  check_schema(x, schema_obj, roxas_version, greedy = FALSE)
  # report missing optional columns
  check_missing_opt(x, tbl_props, roxas_version)

  # check if structure invalid / labels start_with logic
  check_structure(x)

  # TODO: any additional checks?
  # e.g.: check outmost_year is valid

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}


#' Complete a QWAimages object
#'
#' Add any missing columns to a [QWAimages] object, including optional columns,
#' except the quality control columns set in the flags app (`colType`
#' `"qc_input"`, e.g. `comment_handled`). New columns are initialised as `NA`.
#'
#' @param x A [QWAimages] object.
#' @returns A [QWAimages] object extended with all optional columns except
#'   `qc_input` ones.
#' @seealso [complete_QWAmetadata()], [QWAimages()]
#' @export
complete_QWAimages <- function(x) {
  validate_QWAimages(x)
  roxas_version <- attr(x, "roxas_version")

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
#' @param drop_empty If `TRUE` (default), optional columns set in the Shiny
#'   apps (`colType` `"meta_input"` or `"qc_input"`, e.g. `img_comment`) that
#'   are empty in all rows are not written.
#' @returns The output file path, invisibly.
#' @note Empty values are written as empty fields. The `roxas_version`
#'   attribute is not written to the CSV file. It is re-derived from the
#'   `$software` column when the file is read back with [read_QWAimages()].
#' @seealso [read_QWAimages()], [QWAimages]
#' @export
write_QWAimages <- function(x, file, compress = FALSE, overwrite = TRUE,
                            drop_empty = TRUE) {
  validate_QWAimages(x)
  checkmate::assert_flag(drop_empty)

  # if compress, enforce .gz extension
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")
  
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  x_out <- prepare_write(x, attr(x, "roxas_version"), drop_empty)
  vroom::vroom_write(x_out, file, delim = ",", na = "")
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
  # renamed attributes backcompatibility
  df <- df |> 
    dplyr::rename(dplyr::any_of(c(
      # current_name = 'old_name'
      cluster_dbl_cwt_threshold = 'dbl_cwt_threshold',
      opposite_cwt_ratio_limit = 'maxrel_opp_cwt',
      relwidth_cwt_integration = 'relwidth_cwt_window',
      meas_created_at = 'rxs_created_at',
      img_comment = 'comment'
    )))

  aligned_data <- QWAimages(df)
  cli::cli_inform(c("v" = "QWAimages read from {.file {file}}"))
  aligned_data
}