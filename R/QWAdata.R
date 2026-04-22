# Helpers ----

# Internal S3 constructor ----
#' @noRd
new_QWAdata <- function(cells = NULL,
                        rings = NULL) {
  checkmate::assert_data_frame(cells, null.ok = TRUE)
  checkmate::assert_data_frame(rings, null.ok = TRUE) 

  structure(
    list(
      cells = cells,
      rings = rings
    ),
    class = c("QWAdata", "list")
  )
}

# Public constructor ----
#' Create a QWAdata object
#'
#' The S3 class `QWAdata` holds the quantitative wood anatomy (QWA) measurements
#' data produced from ROXAS output files, organised into two components:
#'
#' - **`$cells`**: cell-level measurements (one row per cell).
#' - **`$rings`**: ring-level measurements, and possibly also quality flags
#'   (one row per annual ring per image).
#'
#' At least one component must be provided. The typical workflow builds a
#' `QWAdata` object incrementally using [collect_raw_data()],
#' [remove_outliers()], [complete_cell_measures()], and [validate_QWA_data()].
#' Metadata is kept separately as a [QWAmetadata] or [QWAimages] object.
#' Radial profiles are computed separately as [QWAprofile] objects using
#' [calculate_sector_profiles()] or [calculate_band_profiles()].
#'
#' @param cells Data frame with cell-level measurements (optional).
#' @param rings Data frame with ring-level measurements and flags (optional).
#'
#' @returns An object of class `QWAdata`.
#'
#' @seealso [QWAmetadata()], [QWAimages()], [QWAprofile()], [collect_raw_data()],
#'   [validate_QWA_data()], [print.QWAdata()]
#' @export
QWAdata <- function(cells = NULL,
                    rings = NULL) {

  if (is.null(cells) && is.null(rings))
    cli::cli_abort("At least one of {.arg cells} or {.arg rings} must be provided.")

  checkmate::assert_data_frame(cells, null.ok = TRUE)
  checkmate::assert_data_frame(rings, null.ok = TRUE)

  if (!is.null(cells) && !is.null(rings)) {
    df_rings_log <- complete_rings_log(new_QWAdata(cells, rings))
    check_cwt_estimates(df_rings_log)

    # dating check without outmost_year (pass dummy meta with NA)
    dummy_meta <- data.frame(
      image_label = unique(df_rings_log$image_label),
      outmost_year = Inf
    )
    check_dating(df_rings_log, dummy_meta)
  }

  new_QWAdata(cells, rings)
}

# Methods (general and specific) ------
#' Print a QWAdata object
#'
#' Displays a compact overview of a [QWAdata] object: which components are
#' present, the spatial and temporal coverage of the data, and (if the data
#' have been validated) a summary of ring quality flags.
#'
#' @param x A `QWAdata` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAdata()], [summary.QWAdata()]
#' @export
print.QWAdata <- function(x, ...) {
  cli::cli_h3("{.cls QWAdata}")

  # --- Components ---
  fmt_comp <- function(label, val) {
    if (is.null(val)) {
      paste0(format(label, width = 9), ": {.emph none}")
    } else if (is.data.frame(val)) {
      paste0(format(label, width = 9), ": ", nrow(val), " \u00d7 ", ncol(val))
    } else {
      paste0(format(label, width = 9), ": present")
    }
  }
  cli::cli_text("{.strong Components}")
  cli::cli_bullets(c(
    " " = fmt_comp("cells", x$cells),
    " " = fmt_comp("rings", x$rings)
  ))

  # --- Coverage ---
  ref <- if (!is.null(x$rings)) x$rings else x$cells
  if (!is.null(ref)) {
    cli::cli_text("{.strong Coverage}")
    bullets <- character(0)
    if ("woodpiece_label" %in% names(ref))
      bullets <- c(bullets, " " = "{length(unique(ref$woodpiece_label))} woodpiece{?s}")
    if ("slide_label" %in% names(ref))
      bullets <- c(bullets, " " = "{length(unique(ref$slide_label))} slide{?s}")
    bullets <- c(bullets, " " = "{length(unique(ref$image_label))} image{?s}")
    if ("year" %in% names(ref)) {
      yr <- range(ref$year, na.rm = TRUE)
      bullets <- c(bullets, " " = "years: {yr[1]}\u2013{yr[2]}")
    }
    cli::cli_bullets(bullets)
  }

  # --- Flags (only after validate_QWA_data) ---
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring")
  if (!is.null(x$rings) && all(flag_cols %in% names(x$rings))) {
    r <- x$rings
    cli::cli_text("{.strong Ring flags}")
    cli::cli_bullets(c(
      " " = "incomplete: {sum(r$incomplete_ring, na.rm = TRUE)}",
      " " = "missing:    {sum(r$missing_ring,    na.rm = TRUE)}",
      " " = "duplicate:  {sum(r$duplicate_ring,  na.rm = TRUE)}"
    ))
  }

  invisible(x)
}

#' @rdname print.QWAdata
#' @param object A `QWAdata` object.
#' @export
summary.QWAdata <- function(object, ...) {
  print(object, ...)
}


#' Complete a QWAdata object
#'
#' Runs the full preprocessing pipeline on a [QWAdata] object:
#' 1. Adds derived cell measures (see [complete_cell_measures()]).
#' 2. Extends the rings data frame to cover all years present in cells,
#'    with cell counts and mean CWT per ring (see `complete_rings_log()`).
#' 3. Flags incomplete, missing, and duplicate rings and initialises
#'    the `exclude_issues` column (see [validate_QWA_data()]).
#'
#' @param x A [QWAdata] object with both `$cells` and `$rings`.
#' @param meta A [QWAimages] object providing `spatial_resolution` and
#'   `outmost_year`, required for the incomplete-innermost and dating checks.
#' @param exclude_mode Passed to [validate_QWA_data()]: `"either"` (default)
#'   excludes incomplete and missing rings; `"incomplete_only"` excludes only
#'   incomplete rings.
#' @returns A [QWAdata] object with all derived columns and flag columns populated.
#' @seealso [QWAdata()], [check_QWAdata()], [validate_QWA_data()],
#'   [complete_cell_measures()]
#' @export
complete_QWAdata <- function(x, meta,
                             exclude_mode = c("either", "incomplete_only")) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(
    c("image_label", "spatial_resolution", "outmost_year"), names(meta)
  )

  x <- complete_cell_measures(x)
  x <- validate_QWA_data(x, meta, exclude_mode = exclude_mode)
  x
}


#' Validate a QWAdata object
#'
#' Re-runs data quality checks on a [QWAdata] object and warns about any
#' issues found. Checks include:
#' - CWT estimates present for all images.
#' - Dating validity (undated, duplicate, future years; and if `meta` is
#'   provided, years after `outmost_year`).
#' - Flag columns (`incomplete_ring`, `missing_ring`, `duplicate_ring`,
#'   `exclude_issues`) are present, non-`NA`, and internally consistent
#'   (e.g. `exclude_issues` agrees with the flag columns given `exclude_mode`).
#'
#' @param x A [QWAdata] object.
#' @param meta Optional [QWAimages] object. If provided, also checks that no
#'   ring years exceed `outmost_year`.
#' @param exclude_mode Which flag combination is expected in `exclude_issues`:
#'   `"either"` (default) or `"incomplete_only"`. Used to check consistency.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found;
#'   errors are thrown for structural issues.
#' @seealso [QWAdata()], [complete_QWAdata()]
#' @export
check_QWAdata <- function(x, meta = NULL,
                          exclude_mode = c("either", "incomplete_only")) {
  checkmate::assert_class(x, "QWAdata")
  if (!is.null(meta)) {
    checkmate::assert_class(meta, "QWAimages")
    checkmate::assert_subset(
      c("image_label", "outmost_year"), names(meta)
    )
  }
  mode <- match.arg(exclude_mode)

  # --- CWT and dating checks ---
  if (!is.null(x$cells) && !is.null(x$rings)) {
    df_rings_log <- complete_rings_log(x)
    check_cwt_estimates(df_rings_log)

    df_meta <- if (!is.null(meta)) {
      meta[c("image_label", "outmost_year")]
    } else {
      data.frame(image_label = unique(df_rings_log$image_label), outmost_year = Inf)
    }
    check_dating(df_rings_log, df_meta)
  }

  # --- Flag column checks ---
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_issues")
  r <- x$rings
  if (is.null(r)) {
    cli::cli_inform(c("i" = "No rings data: skipping flag checks."))
    cli::cli_inform(c("v" = "All checks completed."))
    return(invisible(TRUE))
  }

  missing_flags <- setdiff(flag_cols, names(r))
  if (length(missing_flags) > 0)
    cli::cli_warn(c(
      "!" = "Flag column{?s} missing from {.var $rings}: {.var {missing_flags}}.",
      "i" = "Run {.fn complete_QWAdata} to initialise all flag columns."
    ))

  present_flags <- intersect(flag_cols, names(r))

  # non-NA check
  for (col in present_flags) {
    n_na <- sum(is.na(r[[col]]))
    if (n_na > 0)
      cli::cli_warn("!" = "{n_na} {.code NA} value{?s} in {.var $rings${col}}.")
  }

  # consistency: exclude_issues should match flag columns given exclude_mode
  if (all(c("incomplete_ring", "missing_ring", "exclude_issues") %in% names(r))) {
    expected_excl <- switch(mode,
      "either"          = r$incomplete_ring | r$missing_ring,
      "incomplete_only" = r$incomplete_ring
    )
    n_mismatch <- sum(r$exclude_issues != expected_excl, na.rm = TRUE)
    if (n_mismatch > 0)
      cli::cli_warn(c(
        "!" = "{n_mismatch} row{?s} where {.var exclude_issues} is inconsistent with",
        " " = "{.var incomplete_ring} / {.var missing_ring} under {.val {mode}} mode."
      ))
  }

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}


#' Write a QWAdata object to files
#'
#' Cells and rings are written as (compressed) CSV files.
#'
#' @param x A [QWAdata] object.
#' @param dir Directory to write to. Files are auto-named using `dataset_name`.
#'   Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells, file_rings Explicit output paths for the cells and rings
#'   CSV files. Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Name prefix for auto-generated filenames when using
#'   `dir`. Defaults to `"QWAdata"`.
#' @param compress If `TRUE` (default), write `.csv.gz` files.
#' @param overwrite Allow to overwrite existing files? (default `FALSE`).
#' @returns A named list of written file paths, invisibly.
#' @seealso [read_QWAdata()], [write_QWAprofile()]
#' @export
write_QWAdata <- function(x, dir = NULL,
                          file_cells = NULL, file_rings = NULL,
                          dataset_name = NULL,
                          compress = TRUE, overwrite = FALSE) {
  checkmate::assert_class(x, "QWAdata")
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }

  ext <- if (compress) ".csv.gz" else ".csv"
  fix_ext <- function(path) paste0(sub("\\.csv(\\.gz)?$", "", path), ext)

  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    prefix <- dataset_name %||% "QWAdata"
    prefix <- gsub("[^[:alnum:]_-]", "_", prefix)
    file_cells <- file.path(dir, paste0(prefix, "_cells", ext))
    file_rings <- file.path(dir, paste0(prefix, "_rings", ext))
  } else {
    file_cells_fixed <- fix_ext(file_cells)
    file_rings_fixed <- fix_ext(file_rings)
    if (file_cells_fixed != file_cells) {
      cli::cli_warn("Adjusted {.arg file_cells} extension to {.val {ext}}: {.file {file_cells_fixed}}")
      file_cells <- file_cells_fixed
    }
    if (file_rings_fixed != file_rings) {
      cli::cli_warn("Adjusted {.arg file_rings} extension to {.val {ext}}: {.file {file_rings_fixed}}")
      file_rings <- file_rings_fixed
    }
  }
  checkmate::assert_path_for_output(file_cells, overwrite = overwrite)
  checkmate::assert_path_for_output(file_rings, overwrite = overwrite)

  vroom::vroom_write(x$cells, file_cells, delim = ",")
  vroom::vroom_write(x$rings, file_rings, delim = ",")

  cli::cli_inform(c(
    "v" = "Cells written to {.file {file_cells}}",
    "v" = "Rings written to {.file {file_rings}}"
  ))

  invisible(list(file_cells = file_cells, file_rings = file_rings))
}

#' Read a QWAdata object from CSV files
#'
#' Reads cells and rings from (compressed) CSV files.
#' Use the `components` argument to load only a subset, e.g. to avoid reading
#' a large cells file when only rings are needed.
#'
#' @param dir Directory to search for cells and rings files.
#'   Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit paths to the cells and rings CSV files.
#'   Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Optional string to disambiguate when multiple matching
#'   files are found in `dir`.
#' @param components Character vector of components to read. Any subset of
#'   `c("cells", "rings")`. Defaults to `c("cells", "rings")`.
#'   Omitted components are `NULL` in the returned [QWAdata] object.
#' @returns A [QWAdata] object.
#' @seealso [write_QWAdata()], [read_QWAprofile()]
#' @export
read_QWAdata <- function(dir = NULL, file_cells = NULL, file_rings = NULL,
                         dataset_name = NULL,
                         components = c("cells", "rings")) {
  components <- match.arg(components, c("cells", "rings"), several.ok = TRUE)
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }
  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    csv_files <- fs::dir_ls(fs::path_abs(dir), type = "file",
                            regexp = "\\.csv(\\.gz)?$")

    filter_candidates <- function(pattern) {
      cands <- grep(pattern, csv_files, value = TRUE)
      if (!is.null(dataset_name)) cands <- grep(dataset_name, cands, value = TRUE)
      cands
    }

    if ("cells" %in% components) {
      cell_candidates <- filter_candidates("cells")
      if (length(cell_candidates) != 1)
        cli::cli_abort("Could not uniquely identify a cells file in {.path {dir}} ({length(cell_candidates)} matches).")
      file_cells <- cell_candidates
    }
    if ("rings" %in% components) {
      ring_candidates <- filter_candidates("rings")
      if (length(ring_candidates) != 1)
        cli::cli_abort("Could not uniquely identify a rings file in {.path {dir}} ({length(ring_candidates)} matches).")
      file_rings <- ring_candidates
    }
  }

  rings <- NULL
  cells <- NULL

  if ("rings" %in% components) {
    cli::cli_inform(c(" " = "Reading rings data from {.file {file_rings}}..."))
    rings <- vroom::vroom(file_rings, show_col_types = FALSE)
    cli::cli_inform(c("v" = "{nrow(rings)} rings read from file"))
  }

  if ("cells" %in% components) {
    cli::cli_inform(c(" " = "Reading cells data from {.file {file_cells}}..."))
    cells <- vroom::vroom(file_cells, show_col_types = FALSE)
    cli::cli_inform(c("v" = "{nrow(cells)} cells read from file"))
  }

  QWAdata(cells = cells, rings = rings)
}