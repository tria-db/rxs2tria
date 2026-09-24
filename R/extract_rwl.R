#' Extract an rwl series from ring or sector profile data
#'
#' @description
#' This function builds a dendrochronological \pkg{dplR} `rwl` object (a data
#' frame with years as row names and series IDs as column names) from
#' annually resolved QWA data, ready for scaling (see [scale_for_tucson()])
#' and writing to `.rwl` with `dplR::write.tucson()`. Depending on the selected
#' parameter, the function either uses ring-level measurements (e.g. mean
#' ring width, `mrw`) or profile-level measurements at a given sector
#' (aggregated cell parameters such as the 90th percentile lumen area,
#' `la_q90`).
#'
#' Duplicate rings (`exclude_dupl`) and user-defined ring exclusions
#' (`exclude_issues`) are filtered out before constructing the final time
#' series. These flag columns are read from `df_rings`, making it a required
#' parameter, while `prf_data` (with the selected `sector`) is only required
#' to extract a `param` from the profile data.
#'
#' Series are grouped by **woodpiece** (each core/sample yields one series);
#' thus the `woodpiece_label`s become the column names.
#'
#' @param df_rings A data frame containing ROXAS ring-level measurements and
#'   logical flag columns (the `$rings` component of a `QWAdata` object, after
#'   calling `complete_QWAdata()` to instantiate the flag columns).
#' @param param Character string specifying the parameter to export: either
#'   a measurement column in `df_rings`, or an aggregated cell measurement in
#'   `prf_data` (e.g. `"la_mean"`, `"la_q90"`).
#' @param prf_data A data frame containing ROXAS profile-level measurements
#'   (aggregated cell parameters via `calculate_sector_profiles()`). Only 
#'   required when `param` is not a `df_rings` column.
#' @param sector Integer specifying which sector to use when exporting a
#'   `prf_data` parameter. Required in that case, otherwise ignored.
#'
#' @return A \pkg{dplR} `rwl` object with the selected `param` data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Build an rwl object from mean ring width
#' extract_rwl(df_rings = QWA_data$rings,
#'            param = "mrw")
#'
#' # Build an rwl object from a profile-level parameter
#' extract_rwl(df_rings = QWA_data$rings,
#'            param = "cwtrad_mean",
#'            prf_data = prf_sector,
#'            sector = 5)
#' }
extract_rwl <- function(df_rings, param, prf_data = NULL, sector = NULL) {
  checkmate::assert_data_frame(df_rings)
  checkmate::assert_names(names(df_rings),
    must.include = c("year", "image_label", "slide_label", "woodpiece_label"))
  required_flags <- c("exclude_issues", "exclude_dupl")
  missing_flags <- setdiff(required_flags, names(df_rings))
  if (length(missing_flags)>0) {
    cli::cli_abort(c(
      "Missing required column{?s} {.field {missing_flags}} in {.arg df_rings}",
      "i" = "Hint: Have you run {.fn complete_QWA} first?"
    ))
  }
  checkmate::assert_string(param, min.chars = 1)
  # TODO: check rings df valid?

  # possible measurement params for the rings data:
  schema_path <- system.file(schema_rel_path("rings"), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  measure_cols_rings <- tbl_props$properties |>
    purrr::keep(\(x) x$colType %in% c("measure","derived")) |> names()

  meas_cols <- intersect(names(df_rings), measure_cols_rings)
  is_prf_param <- FALSE

  if (!param %in% meas_cols) {
    if (is.null(prf_data)) {
      cli::cli_abort("{.val {param}} is not a measurements column in {.arg df_rings}, but no {.arg prf_data} supplied.")
    }
    checkmate::assert_data_frame(prf_data)
    checkmate::assert_names(names(prf_data),
      must.include = c("year", "image_label", "sector_n"))
    checkmate::assert_integerish(sector)

    # TODO: check valid, matching df_rings?

    # possible measurement params for the cells data:
    schema_path <- system.file(schema_rel_path("cells"), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    measure_cols_cells <- tbl_props$properties |>
      purrr::keep(\(x) x$colType %in% c("measure","derived")) |> names()
    measure_cols_cells <- setdiff(measure_cols_cells, c("sector100", "ew_lw")) # not actually valid params

    in_prf_data <- param %in% names(prf_data)
    is_agg <- stringr::str_detect(param, "_(mean|q\\d+)$")
    is_measure <- sub("_(mean|q\\d+)$", "", param) %in% measure_cols_cells
    if (!in_prf_data || !is_agg || !is_measure) {
      cli::cli_abort("{.val {param}} is not a measurements column in {.arg prf_data} or {.arg df_rings}.")
    }

    if (!sector %in% unique(prf_data$sector_n)) {
      cli::cli_abort( "Sector {.val {sector}} is not present in {.arg prf_data$sector_n}")
    }

    is_prf_param <- TRUE
  }

  # extract selected and prepare for rwl
  df_data <- df_rings |>
    dplyr::select(dplyr::any_of(c("woodpiece_label", "image_label", "year",
                                   "exclude_dupl", "exclude_issues", param)))

  if (is_prf_param) {
    df_data <- prf_data |>
      dplyr::filter(.data$sector_n == sector) |>
      dplyr::select(dplyr::all_of(c("image_label", "year", param))) |>
      dplyr::right_join(df_data, by = c("image_label", "year"))
  }

  pivot_rwl(df_data, param)
}

# build a dplR rwl object from a long-format df with woodpiece_label, year,
# exclude_dupl, exclude_issues and value_col columns (one row per
# woodpiece_label/year after excluding flagged rows)
pivot_rwl <- function(df, value_col) {
  df |>
    dplyr::filter(!.data$exclude_dupl, !.data$exclude_issues) |>
    dplyr::select(dplyr::all_of(c("woodpiece_label", "year", value_col))) |>
    tidyr::pivot_wider(names_from = "woodpiece_label", values_from = !!value_col) |>
    dplyr::arrange(year) |>
    tidyr::complete(year = seq(min(year), max(year), by = 1)) |>
    tibble::column_to_rownames("year") |>
    dplR::as.rwl()
}

#' Scale an rwl object for Tucson-format writing
#'
#' @description
#' `dplR::write.tucson()` stores values as integers at a fixed precision,
#' giving 5 digits of usable range. Since QWA parameters can be on very
#' different scales and units (e.g. μm²) to the mm ring widths \pkg{dplR}
#' expects, this function scales an `rwl` object (as returned by
#' [extract_rwl()]) by a power-of-ten factor. By default, the factor is chosen
#' automatically to make full use of that range, for a given `prec`.
#'
#' @details
#' For ring-width parameters in μm (e.g. `mrw`, `eww`, `lww`), use
#' `scaling = 0.001` to convert the values to mm, as conventionally expected for
#' the input object of `dplR::write.tucson()`. Auto-scaling would instead pick 
#' the factor maximising the represented digits at the selected `prec`, which
#'  does not generally correspond to mm.
#'
#' A manual `scaling` must be a power of ten, and must not push any value
#' beyond the Tucson range at the given `prec`; otherwise the function aborts
#' and suggests the largest factor that fits.
#'
#' The returned `scaling` factor recovers the original values: dividing the
#' scaled `rwl` object, or values re-read with `dplR::read.tucson()`, by
#' `scaling` gives back the original values. The *raw* integers stored in
#' an `.rwl` file created with `dplR::write.tucson()` are additionally scaled by
#' `1 / prec`, so to recover the original values directly from the raw digits, 
#' apply `* prec / scaling`.
#'
#' @param rwl A \pkg{dplR} `rwl` object, e.g. as returned by [extract_rwl()].
#' @param prec Numeric, the precision `dplR::write.tucson()` will be called
#'   with: either `0.001` (default) or `0.01`.
#' @param scaling Numeric power of ten to scale by, or `NULL` (default) to
#'   determine the factor automatically.
#'
#' @return A list with:
#'   - `rwl`: the scaled `rwl` object.
#'   - `scaling`: the scaling factor applied.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ring widths in mm
#' rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
#' scaled <- scale_for_tucson(rwl, scaling = 0.001)
#' dplR::write.tucson(scaled$rwl, fname = "mrw.rwl", prec = 0.001)
#'
#' # Other parameters, auto-scaled
#' rwl <- extract_rwl(df_rings = QWA_data$rings, param = "la_q90",
#'                    prf_data = prf_sector, sector = 5)
#' scaled <- scale_for_tucson(rwl, prec = 0.001)
#' dplR::write.tucson(scaled$rwl, fname = "la_q90.rwl", prec = 0.001)
#' }
scale_for_tucson <- function(rwl, prec = 0.001, scaling = NULL) {
  checkmate::assert_class(rwl, "rwl")
  checkmate::assert_choice(prec, c(0.001, 0.01))
  if (!is.null(scaling)) {
    checkmate::assert_number(scaling, finite = TRUE)
    if (scaling <= 0 || abs(log10(scaling) - round(log10(scaling))) > 1e-8) {
      cli::cli_abort("{.arg scaling} must be a power of ten (e.g. {.val {0.001}}).")
    }
    scaling <- 10^round(log10(scaling)) # snap floating-point error to exact power of ten
  }

  max_digits <- 5 # Tucson format field width, independent of prec
  max_representable <- (10^max_digits - 1) * prec
  vals <- unlist(rwl, use.names = FALSE)
  vals <- vals[!is.na(vals)]
  max_val <- max(c(vals, 0))
  auto_scaling <- if (max_val == 0) 1 else
    10^floor(log10(max_representable / max_val)) # largest power of 10 that fits

  if (is.null(scaling)) {
    scaling <- auto_scaling
    if (max_val == 0) {
      cli::cli_warn(c(
        "Cannot determine an auto-scaling factor: no valid values in {.arg rwl}.",
        "i" = "Returning {.arg rwl} unscaled ({.code scaling = 1})."
      ))
    } else {
      cli::cli_inform("Auto-scaled by a factor of {.val {scaling}} for optimal range at {.code prec = {prec}}.")
    }
  } else if (max_val * scaling > max_representable) {
    cli::cli_abort(c(
      "Scaling by {.val {scaling}} exceeds the Tucson range at {.code prec = {prec}}.",
      "i" = "Largest scaled value would be {.val {max_val * scaling}}, maximum is {.val {max_representable}}.",
      "i" = "Use {.code scaling = {auto_scaling}} or smaller."
    ))
  }

  list(rwl = rwl * scaling, scaling = scaling)
}

#' Legacy: Create a Tucson (.rwl) file from ROXAS ring-width or profile data
#'
#' @description
#' This function creates a dendrochronological Tucson-format (\code{.rwl})
#' file from ROXAS output data. Depending on the selected parameter, the
#' function either uses ring-level measurements (e.g. mean ring width, \code{mrw})
#' or profile-level measurements aggregated at a given sector.
#'
#' Duplicate rings and user-defined exclusions are automatically removed
#' before constructing the final time series.
#' 
#' NOTE: assumes that there is always only one core per tree, i.e. 
#' woodpiece_label and tree_label are exactly the same. Filtering might not work
#' properly, duplicates are not removed, expects a `exclude_scope` column and
#' filtering join on profile data seems off. Scaling may be off for non-μm 
#' parameters.
#'
#' @details
#' The function operates in two modes:
#'
#' \itemize{
#'   \item{\strong{Ring-level mode} (e.g. \code{PAR == "mrw"}):}
#'   Ring-width values are taken from \code{df_rings}. Rings flagged as
#'   duplicated (\code{exclude_issues == TRUE}) are removed. If multiple
#'   measurements exist for the same year and image, values are averaged.
#'   Ring widths are converted from micrometers to millimeters before export.
#'
#'   \item{\strong{Profile-level mode} (e.g. \code{PAR == "la_mean"}):}
#'   Measurements are taken from \code{prf_data} for the selected sector.
#'   Years flagged either as duplicated rings or excluded via
#'   \code{exclude_scope} are removed prior to aggregation.
#' }
#'
#' Image identifiers are simplified to tree-level series IDs before exporting,
#' and the resulting data are written to a Tucson \code{.rwl} file using
#' \code{dplR::write.tucson()}.
#'
#' @param prf_data A data frame containing ROXAS profile-level measurements
#'   (e.g. sector-based anatomical parameters).
#' @param df_rings A data frame containing ROXAS ring-level measurements and
#'   logical flag columns.
#' @param PAR Character string specifying the parameter to export.
#'   Use \code{"mrw"} for mean ring width, or any valid column name present
#'   in \code{prf_data} for profile-level parameters.
#' @param SECTOR Integer specifying which sector to use when exporting
#'   profile-level parameters.
#' @param path_out Character string specifying the output directory where
#'   the \code{.rwl} file and mapping information will be written.
#'
#' @return
#' The function is called for its side effects. It writes a Tucson-format
#' \code{.rwl} file to disk and returns the final data frame invisibly.
#'
#' @examples
#' \dontrun{
#' # Create an RWL file from mean ring width
#' create_rwl(prf_data = prf_data,
#'            df_rings = QWA_data$rings,
#'            PAR = "mrw",
#'            path_out = tempdir())
#'
#' # Create an RWL file from a profile-level parameter
#' create_rwl(prf_data = prf_data,
#'            PAR = "cwtrad_mean",
#'            df_rings = QWA_data$rings, #df_rings = read.csv(paste0(path_out, "/", "20251230_TRIA_YAM_AI_1880_rings_edited.csv"))
#'            SECTOR = 5,
#'            path_out = tempdir())
#' }
#'
#' @export
create_rwl <- function(prf_data, df_rings, PAR = "mrw", SECTOR = NULL, path_out, remove_excluded = TRUE) {

  # ----------------------------
  # 1. Safety checks
  # ----------------------------
  stopifnot(is.character(PAR), length(PAR) == 1)

  if (!PAR %in% c(names(df_rings), names(prf_data))) {
    stop("Parameter ", PAR, " not found in rings or profiles data.")
  }

  # ----------------------------
  # 2. Helper: standardize series ID
  # ----------------------------
  make_series_id <- function(x) {
    paste0(
      stringr::str_split(x, "_") |> sapply(`[`, 1),
      stringr::str_split(x, "_") |> sapply(`[`, 3)
    )
  }

  # ----------------------------
  # 3. RING-LEVEL PARAMETERS
  # ----------------------------
  if (PAR %in% names(df_rings)) {

    rings_df <- df_rings |>
      tibble::as_tibble()

    if (remove_excluded && "exclude_issues" %in% names(rings_df)) {
      rings_df <- rings_df |> dplyr::filter(!exclude_issues)
    }

    rwl <- rings_df |>
      dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
      dplyr::filter(!is.na(.data[[PAR]])) |>
      dplyr::mutate(
        series = make_series_id(image_label),
        value  = .data[[PAR]] / 1000
      ) |>
      dplyr::group_by(year, series) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      dplyr::arrange(dplyr::desc(year)) |>
      tibble::column_to_rownames("year")

  } else {

    # ----------------------------
    # 4. PROFILE-LEVEL PARAMETERS
    # ----------------------------
    if (is.null(SECTOR)) {
      stop("SECTOR must be provided for profile-level parameters.")
    }

    prf_df <- prf_data |>
      tibble::as_tibble() |>
      dplyr::filter(sector_n == SECTOR)

    # Ensure 'exclude_scope' exists and is character
    if (!"exclude_scope" %in% names(df_rings)) {
      df_rings$exclude_scope <- NA_character_
    }

    if (remove_excluded && all(c("image_label", "year") %in% names(df_rings))) {

      to_remove <- df_rings |>
        dplyr::filter(
          (exclude_issues %||% FALSE) |
            !is.na(exclude_scope)
        ) |>
        dplyr::select(image_label, year) |>
        dplyr::distinct()

      prf_df <- prf_df |>
        dplyr::mutate(
          image_label = stringr::str_replace(image_label, "_[0-9]+$", "")
        ) |>
        dplyr::anti_join(to_remove, by = c("image_label", "year"))
    }

    rwl <- prf_df |>
      dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
      dplyr::filter(!is.na(.data[[PAR]])) |>
      dplyr::mutate(series = make_series_id(image_label)) |>
      dplyr::group_by(year, series) |>
      dplyr::summarise(value = mean(.data[[PAR]], na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      dplyr::arrange(desc(year)) |>
      tibble::column_to_rownames("year")
  }

  # ----------------------------
  # 5. Write RWL
  # ----------------------------
  out_file <- file.path(path_out, paste0(PAR, "_roxas.rwl"))

  dplR::write.tucson(
    rwl,
    fname = out_file,
    prec = 0.01,
    header = NULL,
    append = FALSE,
    long.names = TRUE,
    mapping.fname = file.path(path_out, "ROXAS.RWL_new.IDs")
  )

  invisible(rwl)
}
