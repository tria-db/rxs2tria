#' Build a dplR rwl object from ring or sector profile data
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
#' create_rwl(df_rings = QWA_data$rings,
#'            param = "mrw")
#'
#' # Build an rwl object from a profile-level parameter
#' create_rwl(df_rings = QWA_data$rings,
#'            param = "cwtrad_mean",
#'            prf_data = prf_sector,
#'            sector = 5)
#' }
create_rwl <- function(df_rings, param, prf_data = NULL, sector = NULL) {
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
#' different scales and units (e.g. μm, μm²) to the mm ring widths \pkg{dplR}
#' expects, this function auto-scales an `rwl` object (as returned by
#' [create_rwl()]) by a power-of-ten factor chosen to make full use of that
#' range, for a given `prec`.
#'
#' @details
#' The returned `scaling` factor recovers the original values: dividing the
#' scaled `rwl` object, or values re-read with `dplR::read.tucson()`, by
#' `scaling` gives back the original values. The *raw* integers stored in
#' the `.rwl` file are additionally scaled by `1 / prec`, so to recover the
#' original values directly from the raw digits, apply `* prec / scaling`.
#'
#' @param rwl A \pkg{dplR} `rwl` object, e.g. as returned by [create_rwl()].
#' @param prec Numeric, the precision `dplR::write.tucson()` will be called
#'   with: either `0.001` (default) or `0.01`.
#'
#' @return A list with:
#'   - `rwl`: the scaled `rwl` object.
#'   - `scaling`: the scaling factor applied.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rwl <- create_rwl(df_rings = QWA_data$rings, param = "mrw")
#' scaled <- scale_for_tucson(rwl)
#' dplR::write.tucson(scaled$rwl, fname = "mrw.rwl", prec = 0.001)
#' }
scale_for_tucson <- function(rwl, prec = 0.001) {
  checkmate::assert_class(rwl, "rwl")
  checkmate::assert_choice(prec, c(0.001, 0.01))

  max_digits <- 5 # Tucson format field width, independent of prec
  vals <- unlist(rwl, use.names = FALSE)
  vals <- vals[!is.na(vals)]
  max_val <- max(c(vals, 0))

  if (max_val == 0) {
    cli::cli_warn("Cannot determine an auto-scaling factor: no valid values in {.arg rwl}.")
    scaling <- 1
  } else {
    max_representable <- (10^max_digits - 1) * prec
    optimal_scale <- max_representable / max_val
    scaling <- 10^floor(log10(optimal_scale)) # find power of 10 scale that fits
  }

  if (scaling != 1) {
    cli::cli_inform("Scaled by a factor of {.val {scaling}} to for optimal range at {.code prec = {prec}}.")
  }

  list(rwl = rwl * scaling, scaling = scaling)
}
