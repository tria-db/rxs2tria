# Internal S3 constructor ----
#' @noRd
new_QWAprofile <- function(data, profile_type) {
  checkmate::assert_class(data, "data.frame")
  checkmate::assert_choice(profile_type,  c("sector","band"))
  
  structure(
    data,
    class = c("QWAprofile", class(data)),
    profile_type = profile_type
  )
}

# Public constructor ----
#' Create a QWAprofile object
#'
#' `QWAprofile` is a subclass of `data.frame` that holds profiles of QWA
#' measurements aggregated along the annual rings. Usually elements of the 
#' `$profiles` slot of a `QWAdata` object.
#'
#' Two profile types are supported, set via the `profile_type` attribute:
#' - **`"sector"`**: bins defined by equal relative position sectors across the
#'   ring width (0–100%). [calculate_sector_profiles()]
#' - **`"band"`**: bins defined by fixed-width distance bands in microns.
#'   [calculate_band_profiles()]
#'
#' @param data A data frame containing the profile data. Must include at minimum
#'   columns identifying the image/ring (`image_label`, `year`) and the bin
#'   position.
#' @param profile_type The binning scheme used: `"sector"` (relative position,
#'   default) or `"band"` (fixed-width distance bands).
#'
#' @returns An object of class `c("QWAprofile", "data.frame")` with a
#'   `profile_type` attribute.
#'
#' @seealso [QWAdata], [calculate_sector_profiles()], [calculate_band_profiles()]
#' @export
QWAprofile <- function(data, profile_type = c("sector", "band")) {
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(profile_type, c("sector","band"))
  
  # TODO: validation checks

  new_QWAprofile(data, profile_type)
}

# Methods (general and specific) ----
#' @export
#' @rdname summary.QWAprofile
print.QWAprofile <- function(x, ...) {
  NextMethod()
  invisible(x)
}

#' Summarise a QWAprofile object
#'
#' Displays a compact overview of a [QWAprofile] object: the profile type,
#' the number of position bins, and the coverage (images, rings, year range).
#'
#' @param object A `QWAprofile` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `object`, invisibly.
#'
#' @seealso [QWAprofile()]
#' @export
summary.QWAprofile <- function(object, ...) {
  profile_type <- attr(object, "profile_type")
  cli::cli_h3("{.cls QWAprofile}")
  cli::cli_bullets(c(
    " " = "type:  {.val {profile_type}}",
    " " = "size:  {nrow(object)} rows \u00d7 {ncol(object)} cols"
  ))
  if ("image_label" %in% names(object)) {
    bullets <- c(" " = "{length(unique(object$image_label))} image{?s}")
    if ("year" %in% names(object)) {
      yr <- range(object$year, na.rm = TRUE)
      bullets <- c(bullets, " " = "years: {yr[1]}\u2013{yr[2]}")
    }
    cli::cli_text("{.strong Coverage}")
    cli::cli_bullets(bullets)
  }
  invisible(object)
}

#' Write a QWAprofile object to a CSV file
#'
#' @param x A [QWAprofile] object.
#' @param file Path to the output `.csv` or `.csv.gz` file.
#' @param compress If `TRUE`, write a compressed `.csv.gz` file (default `FALSE`).
#' @param overwrite Allow overwriting existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @note The `profile_type` attribute is not written to the CSV file. It is
#'   re-derived from the column names when the file is read back with
#'   [read_QWAprofile()].
#' @seealso [read_QWAprofile()], [QWAprofile()]
#' @export
write_QWAprofile <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAprofile")
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  vroom::vroom_write(x, file, delim = ",")
  cli::cli_inform(c("v" = "QWAprofile written to {.file {file}}"))
  
  invisible(file)
}


#' Read a QWAprofile object from a CSV file
#'
#' Reads a profile written by [write_QWAprofile()] and reconstructs the
#' [QWAprofile] object. The `profile_type` is inferred from the available columns.
#'
#' @param file Path to a `.csv` or `.csv.gz` file.
#' @returns A [QWAprofile] object.
#' @seealso [write_QWAprofile()]
#' @export
read_QWAprofile <- function(file) {
  checkmate::assert_file_exists(file)
  df <- vroom::vroom(file, show_col_types = FALSE)

  if ("sector_n" %in% names(df)) {
    pt <- "sector"
  } else if (all(c("start","end") %in% names(df))) {
    pt <- "band"
  } else { 
    cli::cli_abort("Profile type could not be established in {.file {file}}. Is it a {.val QWAprofile}?")
  }

  # TODO: validate profile data
  # - image_label, year available and complete,
  # - valid measurement and count cols
  # - sectors: n sectors no NA, consistent
  # - bands: start, end, bandwith no NA, consistent
  # use QWAprofile?
  cli::cli_inform(c("v" = "QWAprofile read from {.file {file}}"))
  new_QWAprofile(df, profile_type = pt)
}

#' Validate a QWAprofile object
#'
#' Runs a series of checks on a [QWAprofile] object and warns about any issues
#' found. Checks include: required identifier columns present and non-`NA`,
#' consistent bin definitions (`sector_n` for sector profiles, `start`/`end`
#' for band profiles), and at least one measurement column present.
#'
#' @param x A [QWAprofile] object.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found.
#' @seealso [QWAprofile()], [check_QWAdata()]
#' @export
check_QWAprofile <- function(x) {
  checkmate::assert_class(x, "QWAprofile")
  profile_type <- attr(x, "profile_type")
  checkmate::assert_choice(profile_type, c("sector", "band"))

  # required identifier columns
  id_cols <- c("image_label", "year")
  missing_id <- setdiff(id_cols, names(x))
  if (length(missing_id) > 0)
    cli::cli_warn(c("!" = "Missing required columns: {.field {missing_id}}"))

  if (all(id_cols %in% names(x))) {
    if (anyNA(x$image_label) || anyNA(x$year))
      cli::cli_warn(c("!" = "{.field image_label} and {.field year} must not contain NA values."))
  }

  # bin-specific checks
  if (profile_type == "sector") {
    if (!"sector_n" %in% names(x))
      cli::cli_warn(c("!" = "Missing required column {.field sector_n} for sector profiles."))
    else if (anyNA(x$sector_n) || length(unique(x$sector_n)) < 1)
      cli::cli_warn(c("!" = "{.field sector_n} contains NA or is empty."))
  } else {
    bin_cols <- c("start", "end")
    missing_bin <- setdiff(bin_cols, names(x))
    if (length(missing_bin) > 0)
      cli::cli_warn(c("!" = "Missing required columns for band profiles: {.field {missing_bin}}"))
    else if (any(x$start >= x$end, na.rm = TRUE))
      cli::cli_warn(c("!" = "Some bands have {.field start} >= {.field end}."))
  }

  # at least one measurement column
  non_meta <- setdiff(names(x), c("image_label", "year", "sector_n", "start", "end",
                                   "mrw", "eww", "ew_band"))
  if (length(non_meta) == 0)
    cli::cli_warn(c("!" = "No measurement columns found in profile."))

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}


#' Add cell parameters to an existing QWAprofile object
#'
#' Computes profiles for additional cell parameters from `QWA_data` and merges
#' them into an existing [QWAprofile] object. Only parameters not already
#' present in `x` are computed. The bin definitions (sectors or bands) and
#' aggregation settings from the original profile are reused.
#'
#' @param x A [QWAprofile] object to extend.
#' @param QWA_data A [QWAdata] object (source of cell measurements).
#' @param add_params Character vector of additional cell parameter column names
#'   to compute and add.
#' @param quant_probs Numeric vector of quantile probabilities. Should match
#'   those used when computing `x`. `NULL` adds means only.
#' @returns A [QWAprofile] object with the new parameter columns appended.
#' @seealso [calculate_sector_profiles()], [calculate_band_profiles()],
#'   [QWAprofile()]
#' @export
complete_QWAprofile <- function(x, QWA_data, add_params, quant_probs = NULL) {
  checkmate::assert_class(x, "QWAprofile")
  checkmate::assert_class(QWA_data, "QWAdata")
  checkmate::assert_character(add_params, min.len = 1)
  profile_type <- attr(x, "profile_type")

  # skip params already present (match by base name, ignoring _mean/_N/_qXX suffixes)
  existing_base <- unique(sub("_(mean|N|q\\d+)$", "", names(x)))
  new_params <- setdiff(add_params, existing_base)
  if (length(new_params) == 0) {
    cli::cli_inform(c("i" = "All requested parameters are already present in the profile."))
    return(x)
  }

  if (profile_type == "sector") {
    n_sectors <- max(x$sector_n, na.rm = TRUE)
    prf_new <- calculate_sector_profiles(QWA_data, n_sectors, new_params, quant_probs)
    join_cols <- c("image_label", "year", "sector_n")
  } else {
    # infer bandwidth and stepsize from existing band definitions
    bandwidth <- round(mean(x$end - x$start, na.rm = TRUE))
    stepsize  <- round(min(diff(sort(unique(x$start))), na.rm = TRUE))
    prf_new <- calculate_band_profiles(QWA_data, bandwidth, stepsize, new_params, quant_probs)
    join_cols <- c("image_label", "year", "start", "end")
  }

  # keep only the new parameter columns from prf_new
  new_cols <- setdiff(names(prf_new), c(names(x), join_cols))
  merged <- dplyr::left_join(x, dplyr::select(prf_new, dplyr::all_of(c(join_cols, new_cols))),
                             by = join_cols)

  cli::cli_inform(c("v" = "Added {length(new_cols)} column{?s} to profile: {.field {new_cols}}"))
  new_QWAprofile(merged, profile_type)
}
