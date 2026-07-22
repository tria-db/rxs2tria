# Helpers ----

#' Complete the rings df with all years present in the cell data
#'
#' This function takes the raw QWA data (cells and rings dataframes) and returns an
#' extended version of the rings data frame, with additional rows for years that
#' were only present in the cells data / missing altogether, and an additional 
#' column for the number of cells per ring (cno, derived from cells data).
#'
#' @param QWA_data a list containing the cells and rings dataframes
#' @return an extended data frame of rings data
#' @noRd
complete_rings <- function(QWA_data) {
  df_rings <- QWA_data$rings
  logical_cols <- names(df_rings)[sapply(df_rings, is.logical)]
  fill_logical <- setNames(rep(list(FALSE), length(logical_cols)), logical_cols)

  if (!is.null(QWA_data$cells)) {
    # remove and recalculate cno if already present
    df_rings <- df_rings |> 
      dplyr::select(!dplyr::any_of("cno"))
    QWA_data$cells |>
      dplyr::group_by(.data$image_label, .data$year) |>
      dplyr::summarise(cno = dplyr::n(), .groups = 'drop') |>
      dplyr::full_join(df_rings, by = c('image_label', 'year')) |>
      dplyr::arrange(.data$image_label, .data$year) |>
      dplyr::group_by(.data$image_label) |>
      tidyr::complete(year = tidyr::full_seq(.data$year, 1),
                      fill = c(list(cno = 0), fill_logical)) |>
      tidyr::fill("woodpiece_label", "slide_label", .direction = 'downup') |>
      dplyr::ungroup() |>
      dplyr::select("woodpiece_label", "slide_label", "image_label", "year", "cno", dplyr::everything())
  } else {
    if ("cno" %in% names(df_rings)) {
      fill_logical <- c(list(cno = 0), fill_logical)
    }
    df_rings |>
      dplyr::arrange(.data$image_label, .data$year) |>
      dplyr::group_by(.data$image_label) |>
      tidyr::complete(year = tidyr::full_seq(.data$year, 1),
                      fill = fill_logical) |>
      tidyr::fill("woodpiece_label", "slide_label", .direction = 'downup') |>
      dplyr::ungroup() |>
      dplyr::select("woodpiece_label", "slide_label", "image_label", "year", dplyr::everything())
  }
}

#' @noRd
check_cwt <- function(df_cells, warn_only = FALSE) {
  no_cwt_imgs <- df_cells |>
    dplyr::select("image_label","cwttan") |> 
    dplyr::summarise(all_na = all(is.na(.data$cwttan)), .by = "image_label") |>
    dplyr::filter(.data$all_na) |> 
    dplyr::pull("image_label")
  if (length(no_cwt_imgs) > 0) {
    msg <- c(
      "!" = "Missing cell wall thickness estimates detected",
      "i" = "For conifer (but not angiosperm) data, CWT analysis is expected for TRIA submissions.",
      " " = "{length(no_cwt_imgs)} image{?s} without cwttan found:",
      cli_truncated_list(no_cwt_imgs)
    )
    if (warn_only) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(msg)
    }
  }
  invisible(TRUE)
}

#' @noRd
check_ring_years <- function(df_rings, df_meta = NULL, warn_only = FALSE) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # if df_meta (resp. $outmost_year) is not given, create dummy
  if (is.null(df_meta$outmost_year)) {
    df_meta <- data.frame(
      image_label = unique(df_rings$image_label),
      outmost_year = Inf
    )
  }

  df <- df_rings |> dplyr::select("image_label", "year") |> 
    dplyr::left_join(
      df_meta |> dplyr::select("image_label", "outmost_year"), 
      by = "image_label"
  )

  issues <- df |>
    dplyr::summarise(
      undated = any(is.na(.data$year)),
      duplicated = any(duplicated(.data$year)),
      gaps = dplyr::n_distinct(.data$year, na.rm=TRUE) != (max(.data$year, na.rm=TRUE) - min(.data$year, na.rm=TRUE) + 1),
      in_future = any(.data$year > current_year, na.rm=TRUE),
      after_outmost = any(.data$year > .data$outmost_year + 1, na.rm=TRUE), # add 1 year for incomplete rings
      .by = "image_label"
    )
  issue_counts <- colSums(issues[-1])

  if (any(issue_counts>0)) {
    msg <- c(
      " " = "Undated year in {issue_counts[1]} image{?s} {.val {issues$image_label[issues$undated]}}",
      " " = "Duplicated years in {issue_counts[2]} image{?s} {.val {issues$image_label[issues$duplicated]}}",
      " " = "Gaps in dating in {issue_counts[3]} image{?s} {.val {issues$image_label[issues$gaps]}}",
      " " = "Invalid future years in {issue_counts[4]} image{?s} {.val {issues$image_label[issues$in_future]}}",
      " " = "After outmost years in {issue_counts[5]} image{?s} {.val {issues$image_label[issues$after_outmost]}}"
    )
    msg <- msg[issue_counts >0]
    msg <- c("x" = "The data has dating problems. Fix these issues before continuing!", msg)
    
    if (warn_only) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(msg)
    }
  }

  invisible(TRUE)
}

#' @noRd
check_cell_years <- function(df_cells, warn_only = FALSE) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  in_future <- df_cells |> 
    dplyr::distinct(.data$image_label, .data$year) |> 
    dplyr::filter(.data$year > current_year) |> 
    dplyr::pull("image_label") |> unique()
  if (length(in_future)>0) {
    if (warn_only) {
      cli::cli_warn(c(
        "!" = "The data has dating problems. Fix these issues before continuing!", 
        "i" = "Invalid future years in {length(in_future)} image{?s}:",
        cli_truncated_list(in_future)
      ))
    } else {
      cli::cli_abort(c(
        "The data has dating problems. Fix these issues before continuing!", 
        "i" = "Invalid future years in {length(in_future)} image{?s}:",
        cli_truncated_list(in_future)
      ))
    }
  }
  invisible(TRUE)
}

# Helper to avoid -inf warning on all NA vectors
#' @noRd
max_na_inf <- function(x) {
  x_na <- is.na(x)
  if(all(x_na)) -Inf else max(x[!x_na])
}

#' Complete QWA measures
#'
#' Some additional cell measures are calculated based on the existing data:
#' - tca: la + cwa
#' - rwd2: cwtrad/drad
#' - dcwt
#' - cwtall_adj
#' - cdrad, cdtan, cdratio
#' - sector100
#' - ew_lw: indicates if it is an EW or LW cell (based on Mork index <1 for EW)
#' And for the rings, we add
#' - eww and lww estimates  (based on Mork index of 1)
#' If any of these columns already exist in the data they are NOT recalculated, only
#' the missing measures are added.
#'
#' @param QWA_data a `QWAdata` object containing the cells and rings data frames
#' @param only optional character vector of measure names to calculate. When
#'   supplied, only measures listed here are eligible for calculation (derived measures
#'   not in `only` are skipped). `NULL` (default) retains the existing behaviour: 
#'   all missing measures are added.
#' @return a `QWAdata` object with the updated cells and rings dataframes with the new measures
#' @export
complete_measures <- function(QWA_data, only = NULL) {
  checkmate::assert_class(QWA_data, "QWAdata")
  checkmate::assert_character(only, null.ok = TRUE)

  df_cells <- QWA_data$cells |> 
    # join mrw may needed for some calculations
    dplyr::left_join(QWA_data$rings |> 
                      dplyr::select("image_label", "year", "mrw"),
                      by=c('image_label', 'year'))
  df_rings <- QWA_data$rings

  # definitions of the cell measures to calculate
  all_exprs <- list(
    tca = rlang::quo(.data$la + .data$cwa),
    rwd2 = rlang::quo(.data$cwtrad / .data$drad),
    lr = rlang::quo(sqrt(.data$la / pi)),
    wa = rlang::quo(dplyr::if_else(.data$rtsr < 1, 
                                   (.data$lr + .data$cwttan)^2 * pi - .data$la,
                                   (.data$lr + .data$cwtall)^2 * pi - .data$la)),
    dcwt = rlang::quo(.data$wa / (.data$la + .data$wa)),
    # standardized raddistr (by mrw):
    # add mean cwt: mean of radial and tangential cwt if Mork index latewood-like,
    # in earlywood-like cells take cwttan
    cwtall_adj = rlang::quo(dplyr::if_else(.data$rtsr < 1, .data$cwttan, .data$cwtall)),
    cdrad = rlang::quo(.data$drad + 2*.data$cwttan),
    cdtan = rlang::quo(.data$dtan + 2*.data$cwtrad),
    cdratio = rlang::quo(.data$cdrad / .data$cdtan),
    # add sector100 in 2 steps
    sector100 = rlang::quo(as.numeric(cut(.data$rraddistr,
                               b = seq(from=0, to=100, by= 1),
                               labels = 1:100,
                               include.lowest = TRUE))),
    # round for data with rraddistr just above 100, otherwise leave NA
    sector100 = rlang::quo(dplyr::if_else(.data$rraddistr > 100 & .data$rraddistr <= 101, 
                                100, .data$sector100))
  )

  # find which ones actually need to be recalculated
  cell_meas_all <- c("tca", "rwd2", "dcwt", "cwtall_adj",
                     "cdrad", "cdtan", "cdratio", "sector100", "ew_lw")
  cell_meas_missing <- setdiff(cell_meas_all, names(QWA_data$cells))
  if (!is.null(only)) cell_meas_missing <- intersect(cell_meas_missing, only)
  to_calculate <- if ("dcwt" %in% cell_meas_missing) {
    c(cell_meas_missing, c("lr", "wa"))
  } else {
    cell_meas_missing
  }
  ring_meas_missing <- c()
  
  # recalculate the missing cell measures
  run_exprs <- all_exprs[names(all_exprs) %in% to_calculate] 
  df_cells <- df_cells |> 
    dplyr::mutate(!!!run_exprs) |> 
    dplyr::select(!dplyr::any_of(c("lr","wa")))

  # do we need to recalculate max_EW_sector for cells$ew_lw or rings$eww?
  ew_lw_needed <- "ew_lw" %in% cell_meas_missing
  eww_needed   <- !"eww" %in% names(df_rings) && (is.null(only) || "eww" %in% only)
  if (ew_lw_needed || eww_needed) {
    mork <- 1
    df_ewlw <- df_cells |> 
      dplyr::filter(!is.na(.data$rtsr), !is.na(.data$mrw)) |> # remove cells that do not have a measured CWT or MRW
      # TODO: check grouping - could also group by slide_label to treat overlaps / incomplete rings together?
      dplyr::group_by(.data$image_label, .data$year, .data$sector100) |> 
      dplyr::summarise(rtsr.mean = mean(.data$rtsr),
                       mrw = mean(.data$mrw), .groups = "drop_last") |> 
      # TODO: fill is 10 so the last 4 sectors are always LW -> very narrow incomplete rings seen as LW only
      dplyr::mutate(rollmean = zoo::rollmean(.data$rtsr.mean , 9, fill = c(NA, NA, 10))) |> 
      dplyr::summarise(
        # the boundary is set at the highest sector with a rolling mean <= mork
        # TODO: check edge cases
        max_EW_sector = max_na_inf(.data$sector100[.data$rollmean <= mork]),
        .groups = "drop")
    
    if (ew_lw_needed) {
      df_cells <- df_cells |>
        dplyr::left_join(df_ewlw, by = c('image_label', 'year')) |>
        dplyr::mutate(ew_lw = dplyr::if_else(.data$sector100 <= .data$max_EW_sector, "EW", "LW")) |>
        dplyr::select(-dplyr::any_of(c("mrw", "max_EW_sector")))
    }

    if (eww_needed) {
      df_rings <- df_rings |> 
        dplyr::left_join(df_ewlw, by = c("image_label", "year")) |> 
        dplyr::mutate(eww = dplyr::if_else(.data$max_EW_sector >= 0,
          .data$max_EW_sector*.data$mrw/100, 0)) |> 
        dplyr::select(!"max_EW_sector")
      ring_meas_missing <- c(ring_meas_missing, "eww")
    }
  }

  if (!"lww" %in% names(df_rings) && (is.null(only) || "lww" %in% only)) {
    df_rings <- df_rings |>
      dplyr::mutate(lww = .data$mrw - .data$eww)
    ring_meas_missing <- c(ring_meas_missing, "lww")
  }

  # reorder: derived columns go to the end
  ring_meas_all <- c("eww", "lww")
  df_cells <- df_cells |>
    dplyr::select(-dplyr::any_of(c("mrw",cell_meas_all)),
                  dplyr::any_of(cell_meas_all))
  df_rings <- df_rings |>
    dplyr::select(-dplyr::any_of(ring_meas_all),
                  dplyr::any_of(ring_meas_all))

  cell_meas_existing <- setdiff(cell_meas_all, cell_meas_missing)
  ring_meas_existing <- setdiff(ring_meas_all, ring_meas_missing)

  cli::cli_inform(c(
    "v" = "Cell and ring measures completed",
    "i" = if (length(cell_meas_missing) > 0) 
            "Derived cell measures: {.field {cell_meas_missing}}", 
    # "i" = if (length(cell_meas_existing) > 0)
    #         "derived cell measures already present: {.field {cell_meas_existing}}",
    "i" = if (length(ring_meas_missing) > 0) 
            "Derived ring measures: {.field {ring_meas_missing}}"
    # "i" = if (length(ring_meas_existing) > 0)
    #         "derived ring measures already present: {.field {ring_meas_existing}}"
  ))

  new_QWAdata(cells = df_cells, rings = df_rings)
}

#' Add the initial flags columns to QWAdata
#'
#' The function identifies the rings with the following issues:
#' - (radially) incomplete rings
#' - missing / wedging rings
#' - duplicate / overlapping rings
#'
#' Here, by **incomplete** rings we mean those at the inner (pith) and outer (bark)
#' boundaries of an images, which are cut-off by the image or slide border.
#' For these rings, some cells are usually recognized but the MRW can NOT
#' (outer) or NOT ACCURATELY (inner) be estimated. In some cases, the user may
#' have manually deleted the incomplete rings within ROXAS already, so it is not
#' a priori clear that all inner- and outermost rings per image are incomplete.
#'
#' NOTE: Because ROXAS uses the outer ring boundary to estimate MRW, the
#' innermost ring generally has an MRW estimate (that is not based on the true
#' ring boundary but rather the image border), while the outermost ring has no
#' MRW value (except if it is actually complete either because it is at the
#' at the bark or because the user removed the incomplete ring manually in ROXAS).
#' Therefore, we perform an additional check on the border shape and position to
#' check if an innermost ring is incomplete,  while the outermost ring is flagged
#' as incomplete if and only if it has no MRW.
#' TODO: The correct behaviour for rings derived with ROXAS AI still needs to be determined
#'
#' **Missing** rings are for years that have no discernible ring in the image, but
#' have been manually added in ROXAS during cross-dating, leading to an entry
#' in the rings data but no corresponding no entries (cells) in the cells data.
#' This is usually the case with wedging rings.
#'
#' **Duplicate** rings are those that are present in multiple images due to them
#' overlapping. All years which have cells in more than one image are flagged
#' and ranked by their number of cells. The (complete) year with the highest
#' number of cells for each overlap is the one that would then usually be
#' selected for further analysis when building chronologies.
#'
#' @param x a `QWAdata` object containing the cells and rings dataframes
#' @param meta a [QWAimages] object or data.frame providing image-level
#'   information on `spatial_resolution` (required for the incomplete
#'   innermost ring check) and `outmost_year`.
#' @param exclude_mode how should the `$exclude_issues` flag column be initialized, to
#'   exclude any incomplete or missing rings (`"either"`, default) or only the 
#'   incomplete rings (i.e., not flagging missing/wedging rings for exclusion
#'   from analyses, `"incomplete_only"`).
#' @returns A `QWAdata` object with the validated data: cells unchanged, rings
#'   with added flag columns.
#' @export
#'
complete_flags <- function(x, meta, exclude_mode = c("either","incomplete_only")) {
  all_flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")
  flag_cols_missing <- setdiff(all_flag_cols, names(x$rings))
  flag_cols_existing <- intersect(all_flag_cols, names(x$rings))

  df_rings_log <- x$rings

  if ("incomplete_ring" %in% flag_cols_missing) {
    df_rings_log <- flag_incomplete_rings(df_rings_log, x$cells, meta)
  }

  if ("missing_ring" %in% flag_cols_missing) {
    df_rings_log <- df_rings_log |>
      dplyr::mutate(missing_ring = is.na(.data$cno) | (.data$cno < 5) | dplyr::coalesce(.data$mrw < 10, FALSE), # TODO: (should never have NA cno anymore because we replace with 0), but mrw might be NA for incomplete rings -> coalesce. make thresholds function params?
                    no_MRW_other = is.na(.data$mrw) & !(.data$outermost_ring | .data$innermost_ring)) # TODO: check if this ever occurs and for what reason
    # for missing rings, we want some measures set to 0
    missing_to_zero <- c("mrw","ra","eww","lww")
    df_rings_log <- df_rings_log |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(missing_to_zero),
          \(x) dplyr::if_else(.data$missing_ring & is.na(x) & .data$cno < 5, 0, x))
      )
  }

  if (any(c("duplicate_ring", "exclude_dupl") %in% flag_cols_missing)) {
    df_rings_log <- flag_duplicate_rings(df_rings_log) # replace with duplicate_sel?
  }

  mode <- match.arg(exclude_mode)
  if ("exclude_issues" %in% flag_cols_missing) {
    df_rings_log <- df_rings_log |>
      dplyr::mutate(
        exclude_issues = switch(
          mode,
          "either" = incomplete_ring | missing_ring,
          "incomplete_only" = incomplete_ring
        )
      )
  }

  # remove any of the helper cols
  df_rings_log <- df_rings_log |>
    dplyr::select(-dplyr::any_of(c(
        'innermost_ring','outermost_ring',
        'mae','medYleft','medYright','mindist',
        'incomplete_inner', 'incomplete_innerv2', 'incomplete_fct_check',
        'no_MRW_other')))

  cli::cli_inform(c(
    "v" = "Ring flag columns completed",
    "i" = "Derived flag columns: {.field {flag_cols_missing}}"
    # "i" = if (length(flag_cols_existing) > 0)
    #         "Flag columns already present (not overwritten): {.field {flag_cols_existing}}"
  ))

  new_QWAdata(cells = x$cells, rings = df_rings_log)
}

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
#' `QWAdata` object incrementally using [collect_raw_data()], [complete_QWAdata()],
#' and [check_QWAdata()].
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
#'   [complete_QWAdata()], [check_QWAdata()], [print.QWAdata()]
#' @export
QWAdata <- function(cells = NULL,
                    rings = NULL) {
  if (is.null(cells) && is.null(rings))
    cli::cli_abort("At least one of {.arg cells} or {.arg rings} must be provided.")

  checkmate::assert_data_frame(cells, null.ok = TRUE)
  checkmate::assert_data_frame(rings, null.ok = TRUE)

  # minimal requirements: cells - align cols, cwt
  if (!is.null(cells)) {
    # get schema from file
    schema_path <- system.file(schema_rel_path("cells"), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    # required cols
    checkmate::assert_data_frame(
      cells[tbl_props$required], any.missing = FALSE, 
      .var.name = "Missing required columns in cells component")
    # expected raw measurement cols
    measure_cols <- tbl_props$properties |> purrr::keep(\(x) x$colType == "measure") |> names()
    missing_meas_cols <- setdiff(measure_cols, names(cells))
    if (length(missing_meas_cols)>0) {
      cli::cli_warn(c(
        "!" = "{.field cells} has missing expected raw measurement columns:",
        "i" = "{missing_meas_cols}"
      ))
    }
    # align: all measurements numeric (TODO: distinguish between numeric and integer?)
    cells <- cells |> 
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("image_label", "ew_lw")), as.character),
        dplyr::across(-dplyr::any_of(c("image_label", "ew_lw")), as.numeric)
    )
    # check for cwt estimates (expected for conifers)
    check_cwt(cells, warn_only = TRUE)
    if (is.null(rings)) { # if no rings, check dating in cells (just no in-future years)
      check_cell_years(cells, warn_only = FALSE)
    }
  }
   
  # rings - align cols, complete years, check dating
  if (!is.null(rings)) {
    # get schema from file
    schema_path <- system.file(schema_rel_path("rings"), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    # required columns
    checkmate::assert_data_frame(
      rings[tbl_props$required], any.missing = FALSE, 
      .var.name = "Missing required columns in rings component")
    # expected measurement cols
    measure_cols <- tbl_props$properties |> purrr::keep(\(x) x$colType == "measure") |> names()
    missing_meas_cols <- setdiff(measure_cols, names(rings))
    if (length(missing_meas_cols)>0) {
      cli::cli_warn(c(
        "!" = "{.field rings} is missing expected raw measurement columns:",
        "i" = "{missing_meas_cols}"
      ))
    }
    # align types
    char_cols <- tbl_props$properties |> purrr::keep(\(x) "string" %in% x$type) |> names()
    flag_cols <- tbl_props$properties |> purrr::keep(\(x) "boolean" %in% x$type) |> names()
    num_cols <- tbl_props$properties |> purrr::keep(\(x) "number" %in% x$type) |> names()
    int_cols <- tbl_props$properties |> purrr::keep(\(x) "integer" %in% x$type) |> names()
    rings <- rings |> 
      dplyr::mutate(
        dplyr::across(dplyr::any_of(char_cols), as.character),
        dplyr::across(dplyr::any_of(flag_cols), as.logical),
        dplyr::across(dplyr::any_of(num_cols), as.numeric),
        dplyr::across(dplyr::any_of(int_cols), as.integer)
      )
    # ensure we have complete year sequences for each image (re-adds cno)
    rings <- complete_rings(new_QWAdata(cells, rings))
    # check dating
    check_ring_years(rings, warn_only = FALSE)
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

#' @rdname QWAdata
#' @param x A named list with any subset of the [QWAdata] components.
#' @export
as_QWAdata <- function(x) {
  checkmate::assert_list(x, names = "named")
  valid_args <- names(formals(QWAdata))
  extra <- setdiff(names(x), valid_args)
  if (length(extra) > 0)
    cli::cli_warn("Ignoring unknown components: {.val {extra}}")
  do.call(QWAdata, x[intersect(names(x), valid_args)])
}


#' Complete a QWAdata object
#'
#' Runs the full preprocessing pipeline on a [QWAdata] object:
#' 1. Adds derived cell measures (see [complete_measures()]).
#' 2. Extends the rings data frame to cover all years present in cells,
#'    with cell counts and mean CWT per ring (see `complete_rings_log()`).
#' 3. Flags incomplete, missing, and duplicate rings and initialises
#'    the `exclude_issues` column (see [complete_flags()]).
#'
#' @param x A [QWAdata] object with both `$cells` and `$rings`.
#' @param meta A [QWAimages] object providing `spatial_resolution` and
#'   `outmost_year`, required for the incomplete-innermost and dating checks.
#' @param exclude_mode Passed to [complete_flags()]: `"either"` (default)
#'   excludes incomplete and missing rings; `"incomplete_only"` excludes only
#'   incomplete rings.
#' @returns A [QWAdata] object with all derived columns and flag columns populated.
#' @seealso [QWAdata()], [check_QWAdata()], [complete_flags()],
#'   [complete_measures()]
#' @export
complete_QWAdata <- function(x, meta,
                             exclude_mode = c("either", "incomplete_only")) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(
    c("image_label", "spatial_resolution", "outmost_year"), names(meta)
  )

  # align cells
  if (!is.null(x$cells)) {
    x$cells <- x$cells |> 
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("image_label", "ew_lw")), as.character),
        dplyr::across(-dplyr::any_of(c("image_label", "ew_lw")), as.numeric)
    )
  }

  # align rings
  if (!is.null(x$rings)) {
    char_cols <- c("image_label", "slide_label", "woodpiece_label", "affected_tissue")
    flag_cols <- names(x$rings)[sapply(x$rings, is.logical)] # TODO: get from schema
    x$rings <- x$rings |> 
      dplyr::mutate(
        dplyr::across(dplyr::any_of(char_cols), as.character),
        dplyr::across(-dplyr::any_of(c(char_cols, flag_cols)), as.numeric)
      )
  } 
  
  # ensure complete sequences of years (note: creates rings df from cells if null)
  x$rings <- complete_rings(x)
  check_ring_years(x$rings, meta, warn_only = FALSE)

  # complete measures (derived cell measures and ew/lw estimates)
  x <- complete_measures(x)

  # complete flags columns if not yet initialized
  # TODO: option to initialise complete set of flags incl the manual ones?
  x <- complete_flags(x, meta, exclude_mode = exclude_mode)

  cli::cli_alert_success("Added derived measures and flags to `QWAdata` object")
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
  }
  mode <- match.arg(exclude_mode)

  if (is.null(x$cells) && is.null(x$rings))
    cli::cli_abort("{.var QWAdata} requires at least one of {.field cells} or {.field rings}.")
  
  if (!is.null(x$cells)) {
    # TODO: check against schema for cells
    check_missing <- checkmate::test_data_frame(
      x$cells[c("image_label","year","xpix","ypix")], any.missing = FALSE)
    if (!check_missing) {
      cli::cli_warn("Missing required detected in {.field cells} component")
    }
    # TODO: complete the checks, see QWAdata
    # check_missing_opt(cells, tbl_props)? or separate for measures and derived
    # extra_cols <- names(x$cells) %in% all_cols
    # char_cols <- x$cells |> dplyr::select(dplyr::where(is.character)) |> names()
    # checkmate::assert_subset(char_cols, ...)
    # missing (incl. optional) columns
    check_cwt(x$cells, warn_only = TRUE)
  }
    

  if (!is.null(x$rings)) {
    # TODO: check against schema for rings
    # as for cells...
    # check dating
    check_ring_years(x$rings, meta, warn_only = TRUE)
  } else { # check dating of cells component
    check_cell_years(x$cells, warn_only = TRUE)
  }
  
  # check for negative values / outliers, ranges as defined in schemata
  # plus check structure against each other, cno
  # plus re-check calculated measures / flags are valid?

  cli::cli_inform(c("v" = "All checks completed."))
  invisible(TRUE)
}


#' Write a QWAdata object to files
#'
#' Cells and rings are written as (compressed) CSV files.
#'
#' @param x A [QWAdata] object.
#' @param dir Directory to write to. Files are auto-named
#'   `{dataset_name}_QWAdata_cells.csv(.gz)` and
#'   `{dataset_name}_QWAdata_rings.csv(.gz)`. Mutually exclusive with
#'   `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit output paths for the cells and rings
#'   CSV files. Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Name prefix for auto-generated filenames when using
#'   `dir`. If omitted, files are named `QWAdata_cells.csv(.gz)` and
#'   `QWAdata_rings.csv(.gz)`.
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

  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    ext <- if (compress) ".csv.gz" else ".csv"
    if (is.null(dataset_name)) {
      base_cells <- "QWAdata_cells"
      base_rings <- "QWAdata_rings"
    } else {
      prefix <- gsub("[^[:alnum:]_-]", "_", dataset_name)
      base_cells <- paste0(prefix, "_QWAdata_cells")
      base_rings <- paste0(prefix, "_QWAdata_rings")
    }
    file_cells <- file.path(dir, paste0(base_cells, ext))
    file_rings <- file.path(dir, paste0(base_rings, ext))
  } else {
    if (compress && fs::path_ext(file_cells) != "gz") {
      file_cells <- paste0(file_cells, ".gz")
    }
    if (compress && fs::path_ext(file_rings) != "gz") {
      file_rings <- paste0(file_rings, ".gz")
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
      basenames <- fs::path_file(csv_files)
      cands <- csv_files[grep(pattern, basenames)]
      if (!is.null(dataset_name)) cands <- cands[grep(dataset_name, fs::path_file(cands))]
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
    # back-compatibility: older files used dh_w / dh_m for what are now dhw / dhm
    legacy <- c(dhw = "dh_w", dhm = "dh_m") # new = old
    rings <- rings |> 
      dplyr::rename(dplyr::any_of(legacy))
    cli::cli_inform(c("v" = "{nrow(rings)} rings read from file"))
  }

  if ("cells" %in% components) {
    cli::cli_inform(c(" " = "Reading cells data from {.file {file_cells}}..."))
    cells <- vroom::vroom(file_cells, show_col_types = FALSE)
    cli::cli_inform(c("v" = "{nrow(cells)} cells read from file"))
  }

  QWAdata(cells = cells, rings = rings)
}