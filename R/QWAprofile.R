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

# Internal S3 validator ----
#' Abort if a QWAprofile object violates the class invariants
#'
#' Valid `profile_type` attribute; key columns (`image_label`, `year` and
#' `sector_n` or `start`, `end`) present, not `NA` and unique; `start < end`.
#' @returns `x`, invisibly.
#' @noRd
validate_QWAprofile <- function(x) {
  checkmate::assert_class(x, c("QWAprofile", "data.frame"))
  pt <- attr(x, "profile_type")
  if (!checkmate::test_choice(pt, c("sector", "band"))) {
    cli::cli_abort(c(
      "x" = "{.cls QWAprofile}: invalid {.arg profile_type} attribute {.val {pt}}.",
      "i" = "Rebuild the object with {.fn QWAprofile}."
    ))
  }
  # no schema for profiles; sector_n is a factor after calculate_sector_profiles()
  keys <- c(image_label = "string", year = "integer",
            switch(pt, sector = c(sector_n = NA), band = c(start = "number", end = "number")))
  validate_keys(x, keys, "QWAprofile", unique_by = names(keys))
  if (pt == "band" && any(x$start >= x$end)) {
    cli::cli_abort(c("x" = "{.cls QWAprofile}: {sum(x$start >= x$end)} band{?s} with {.field start} >= {.field end}."))
  }
  invisible(x)
}

# Public constructor ----
#' Create a QWAprofile object
#'
#' `QWAprofile` is a subclass of `data.frame` that holds profiles of QWA
#' measurements aggregated along the annual rings, computed from a [QWAdata]
#' object with [calculate_sector_profiles()] or [calculate_band_profiles()].
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
  profile_type <- rlang::arg_match(profile_type)

  x <- new_QWAprofile(data, profile_type)
  validate_QWAprofile(x)
  x
}

# Methods (general and specific) ----
#' @param x A `QWAprofile` object.
#' @export
#' @rdname summary.QWAprofile
print.QWAprofile <- function(x, ...) {
  NextMethod()
  invisible(x)
}

#' Summarise a QWAprofile object
#'
#' Displays a compact overview of a [QWAprofile] object: the profile type,
#' its dimensions, and the coverage (images, year range).
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
#' Empty values are written as empty fields.
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
  validate_QWAprofile(x)
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  vroom::vroom_write(x, file, delim = ",", na = "")
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
  # - valid measurement and count cols
  # - sectors: n sectors consistent
  # - bands: bandwidth consistent
  x <- QWAprofile(df, profile_type = pt)
  cli::cli_inform(c("v" = "QWAprofile read from {.file {file}}"))
  x
}

#' Validate a QWAprofile object
#'
#' Runs a series of checks on a [QWAprofile] object and warns about any issues
#' found. Aborts if the object is invalid (key columns `image_label`, `year`
#' and `sector_n` for sector profiles or `start`/`end` for band profiles
#' missing, `NA` or not unique; bands with `start >= end`). Warns if no
#' measurement column is present.
#'
#' @param x A [QWAprofile] object.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found.
#' @seealso [QWAprofile()], [check_QWAdata()]
#' @export
check_QWAprofile <- function(x) {
  validate_QWAprofile(x)

  # at least one measurement column
  non_meta <- setdiff(names(x), c("image_label", "year", "sector_n", "start", "end",
                                   "mrw", "eww", "ew_band"))
  if (length(non_meta) == 0)
    cli::cli_warn(c("!" = "No measurement columns found in profile."))

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}
