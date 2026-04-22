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

# Methods (general and specifi) ----
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
  cli::cli_h2("<QWAprofile>")
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
#' @seealso [read_QWAprofile()], [QWAprofile()]
#' @export
write_QWAprofile <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAprofile")
  ext <- if (compress) ".csv.gz" else ".csv"
  file_fixed <- paste0(sub("\\.csv(\\.gz)?$", "", file), ext)
  if (file_fixed != file)
    cli::cli_warn("Adjusted {.arg file} extension to {.val {ext}}: {.file {file_fixed}}")
  file <- file_fixed
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
  cli::cli_inform(c("v" = "QWAprofile read from {.file {file}}"))
  new_QWAprofile(df, profile_type = pt)
}
