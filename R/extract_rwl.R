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
      "i" = "Hint: Have you run {.fn complete_QWAdata} first?"
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
    checkmate::assert_int(sector)

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
  df <- df |>
    dplyr::filter(!.data$exclude_dupl, !.data$exclude_issues) |>
    dplyr::select(dplyr::all_of(c("woodpiece_label", "year", value_col)))

  if (nrow(df) == 0) {
    cli::cli_abort("No rings left after removing excluded rings ({.field exclude_dupl}, {.field exclude_issues}).")
  }
  dupl <- duplicated(df[, c("woodpiece_label", "year")])
  if (any(dupl)) {
    cli::cli_abort(c(
      "Multiple rings per woodpiece and year after removing excluded rings.",
      "i" = "Check the {.field exclude_dupl} flags of woodpiece{?s} {.val {unique(df$woodpiece_label[dupl])}}."
    ))
  }

  df |>
    tidyr::pivot_wider(names_from = "woodpiece_label", values_from = !!value_col) |>
    dplyr::arrange(.data$year) |>
    tidyr::complete(year = seq(min(.data$year), max(.data$year), by = 1)) |>
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
#' The applied factor is stored in the `"scaling"` attribute of the returned
#' `rwl` object, and recovers the original values: dividing the scaled `rwl`
#' object, or values re-read with `dplR::read.tucson()`, by `scaling` gives
#' back the original values. The *raw* integers stored in an `.rwl` file
#' created with `dplR::write.tucson()` are additionally scaled by `1 / prec`,
#' so to recover the original values directly from the raw digits, apply
#' `* prec / scaling`. An `rwl` object that already has a `"scaling"`
#' attribute is not scaled again.
#'
#' The attribute is kept by [rename_for_tucson()], so both functions can be
#' applied in either order, but is dropped by most other operations (e.g.
#' subsetting, arithmetic, \pkg{dplR} functions). Apply them as the last steps
#' before writing.
#'
#' Note that `dplR::write.tucson()` writes negative values, and at
#' `prec = 0.001` also values rounding to zero, as missing values.
#'
#' @param rwl A \pkg{dplR} `rwl` object, e.g. as returned by [extract_rwl()].
#' @param prec Numeric, the precision `dplR::write.tucson()` will be called
#'   with: either `0.001` (default) or `0.01`.
#' @param scaling Numeric power of ten to scale by, or `NULL` (default) to
#'   determine the factor automatically.
#'
#' @return The scaled `rwl` object, with the applied factor as attribute
#'   `"scaling"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ring widths in mm
#' rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
#' scaled <- scale_for_tucson(rwl, scaling = 0.001)
#' dplR::write.tucson(scaled, fname = "mrw.rwl", prec = 0.001)
#'
#' # Other parameters, auto-scaled
#' rwl <- extract_rwl(df_rings = QWA_data$rings, param = "la_q90",
#'                    prf_data = prf_sector, sector = 5)
#' scaled <- scale_for_tucson(rwl, prec = 0.001)
#' attr(scaled, "scaling")
#' dplR::write.tucson(scaled, fname = "la_q90.rwl", prec = 0.001)
#' }
scale_for_tucson <- function(rwl, prec = 0.001, scaling = NULL) {
  rwl <- dplR::as.rwl(rwl)
  checkmate::assert_choice(prec, c(0.001, 0.01))
  if (!is.null(attr(rwl, "scaling"))) {
    cli::cli_abort("{.arg rwl} is already scaled (factor {.val {attr(rwl, 'scaling')}}).")
  }
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

  rwl[] <- lapply(rwl, `*`, scaling) # to ensure it keeps class 'rwl'
  attr(rwl, "scaling") <- scaling
  rwl
}


#' Rename rwl series to short Tucson-compatible series IDs
#'
#' @description
#' The Tucson format limits series IDs to 6--8 characters out of `A-Z`, `a-z`
#' and `0-9` (see `long.names` in `dplR::write.tucson()`), which the
#' `woodpiece_label`s used as column names by [extract_rwl()] usually exceed.
#' `rename_for_tucson()` replaces them by short series IDs derived from the
#' data structure, instead of the generic truncation applied by
#' `dplR::write.tucson()`.
#'
#' `make_short_series_ids()` derives the underlying mapping from
#' `woodpiece_label` to short series ID.
#'
#' @details
#' The base ID of a woodpiece is its `woodpiece_label` without the site and
#' species prefixes, reduced to the allowed characters (e.g. `YAM_LASI_122_a`
#' becomes `122a`). The first of the following variants that yields unique IDs
#' of at most `max_chars` characters is used for all series:
#' 1. site label + base ID (e.g. `YAM122a`),
#' 2. base ID only (e.g. `122a`),
#' 3. species code + base ID (e.g. `LASI122a`).
#'
#' If none of them does, the function aborts.
#'
#' The mapping is stored in the `"mapping"` attribute of the returned `rwl`
#' object. The attribute is kept by [scale_for_tucson()], so both functions
#' can be applied in either order, but is dropped by most other operations
#' (e.g. subsetting, arithmetic, \pkg{dplR} functions). Apply them as the last
#' steps before writing. An `rwl` object that already has a `"mapping"`
#' attribute is not renamed again.
#'
#' @param rwl A \pkg{dplR} `rwl` object with `woodpiece_label`s as column
#'   names, e.g. as returned by [extract_rwl()].
#' @param df_structure A data frame with the data structure columns
#'   `woodpiece_label`, `site_label` and optionally `species_code`, e.g. a
#'   [QWAimages] object.
#' @param long.names Logical, the value `dplR::write.tucson()` will be called
#'   with: `FALSE` (default) allows 6 characters, `TRUE` allows 8 characters
#'   (7 if any year is before -999 or after 9999).
#' @param max_chars Integer, the maximum number of characters per series ID.
#'
#' @returns
#' - `rename_for_tucson()`: the `rwl` object with short series IDs as column
#'   names, and a data frame with columns `woodpiece_label` and `series_id`
#'   (in column order) as attribute `"mapping"`.
#' - `make_short_series_ids()`: a tibble with columns `woodpiece_label` and
#'   `series_id`, one row per woodpiece in `df_structure`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
#' rwl_out <- rwl |>
#'   rename_for_tucson(QWA_images, long.names = TRUE) |>
#'   scale_for_tucson(scaling = 0.001)
#' attr(rwl_out, "mapping")
#' dplR::write.tucson(rwl_out, fname = "mrw.rwl", prec = 0.001,
#'                    long.names = TRUE)
#' }
rename_for_tucson <- function(rwl, df_structure, long.names = FALSE) {
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_flag(long.names)
  rwl <- dplR::as.rwl(rwl)
  if (!is.null(attr(rwl, "mapping"))) {
    cli::cli_abort("{.arg rwl} is already renamed (see {.code attr(rwl, 'mapping')}).")
  }

  # name width limits as in dplR::write.tucson()
  yrs <- as.numeric(row.names(rwl))
  long_years <- min(yrs) < -999 || max(yrs) > 9999
  max_chars <- if (!long.names) 6 else if (long_years) 7 else 8

  df_map <- make_short_series_ids(df_structure, max_chars)
  idx <- match(names(rwl), df_map$woodpiece_label)
  if (anyNA(idx)) {
    cli::cli_abort(c(
      "No short series ID found for some {.cls rwl} columns. Do {.arg rwl} and {.arg df_structure} match?",
      cli_truncated_list(names(rwl)[is.na(idx)])
    ))
  }
  names(rwl) <- df_map$series_id[idx]
  attr(rwl, "mapping") <- df_map[idx, c("woodpiece_label", "series_id")]
  rwl
}

#' @rdname rename_for_tucson
#' @export
make_short_series_ids <- function(df_structure, max_chars = 8) {
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_names(names(df_structure),
    must.include = c("woodpiece_label", "site_label"))
  checkmate::assert_count(max_chars, positive = TRUE)
  
  alnum <- function(x) gsub("[^A-Za-z0-9]", "", x) # charset of dplR::write.tucson()
  
  if (!"species_code" %in% names(df_structure)) {
    df_structure$species_code <- NA_character_
  }

  wp <- df_structure |> 
    dplyr::distinct(
      .data$site_label, .data$species_code, .data$woodpiece_label
    ) |> 
    dplyr::mutate(
      prefix = dplyr::if_else(is.na(.data$species_code), .data$site_label,
        paste0(.data$site_label, "_", .data$species_code)),
      base_series_id = alnum(stringr::str_remove(.data$woodpiece_label,
          paste0("^", stringr::str_escape(.data$prefix)))),
      site_series_id = paste0(alnum(.data$site_label), .data$base_series_id),
      species_code = dplyr::coalesce(.data$species_code, ""),
      species_series_id = paste0(alnum(.data$species_code), .data$base_series_id)
    )
  
  # in order of preference: SITE+WP, WP, SPECIES+WP
  for (variant in c("site_series_id", "base_series_id", "species_series_id")) {
    ids <- wp[[variant]]
    if (all(nchar(ids) <= max_chars) && !anyDuplicated(ids)) {
      return(tibble::tibble(woodpiece_label = wp$woodpiece_label, series_id = ids))
    }
  }

  cli::cli_abort("Could not derive unique series IDs of at most {max_chars} characters.")
}

