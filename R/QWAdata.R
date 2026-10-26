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
check_cwt <- function(df_cells) {
  no_cwt_imgs <- df_cells |>
    dplyr::select("image_label","cwttan") |> 
    dplyr::summarise(all_na = all(is.na(.data$cwttan)), .by = "image_label") |>
    dplyr::filter(.data$all_na) |> 
    dplyr::pull("image_label")
  if (length(no_cwt_imgs) > 0) {
    cli::cli_inform(c(
      "i" = "Missing cell wall thickness estimates detected",
      "i" = "For conifer (but not angiosperm) data, CWT analysis is expected for TRIA submissions.",
      " " = "{length(no_cwt_imgs)} image{?s} without cwttan found:",
      cli_truncated_list(no_cwt_imgs)
    ))
  }
  invisible(TRUE)
}

# deprecated exclude_mode: warn if supplied, return the rule for initialising
# exclude_issues ("incomplete_only" unless "either" is given explicitly)
#' @noRd
resolve_exclude_mode <- function(exclude_mode) {
  if (is.null(exclude_mode)) return("incomplete_only")
  cli::cli_warn(c(
    "!" = "{.arg exclude_mode} is deprecated and will be removed in a future version.",
    "i" = "{.field exclude_issues} is now initialised from {.field incomplete_ring}."
  ), .frequency = "once", .frequency_id = "rxs2tria_exclude_mode")
  match.arg(exclude_mode, c("either", "incomplete_only"))
}

# gaps and future years warn (abort with strict); undated and duplicated years
# abort in validate_QWAdata(), years after outmost_year: check_outmost_year()
#' @noRd
check_ring_years <- function(df_rings, strict = FALSE) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  issues <- df_rings |>
    dplyr::summarise(
      gaps = dplyr::n_distinct(.data$year, na.rm=TRUE) != (max(.data$year, na.rm=TRUE) - min(.data$year, na.rm=TRUE) + 1),
      in_future = any(.data$year > current_year, na.rm=TRUE),
      .by = "image_label"
    )
  issue_counts <- colSums(issues[-1])

  msg <- c(
    " " = "Gaps in dating in {issue_counts[1]} image{?s} {.val {issues$image_label[issues$gaps]}}",
    " " = "Invalid future years in {issue_counts[2]} image{?s} {.val {issues$image_label[issues$in_future]}}"
  )

  if (any(issue_counts > 0)) {
    return(report_issue(c(
      "!" = "The data has dating problems:",
      msg[issue_counts > 0]
    ), strict))
  }

  invisible(TRUE)
}

# ring years after outmost_year + 1 (incomplete outermost ring) warn (abort
# with strict); images without outmost_year are reported (I), images not in
# df_meta are skipped
#' @noRd
check_outmost_year <- function(df_rings, df_meta, strict = FALSE) {
  outmost <- data.frame(
    image_label = df_meta$image_label,
    outmost_year = df_meta[["outmost_year"]] %||% rep(NA_real_, nrow(df_meta))
  )
  df <- df_rings |>
    dplyr::summarise(last_year = max(.data$year), .by = "image_label") |>
    dplyr::inner_join(outmost, by = "image_label")

  unchecked <- unique(df$image_label[is.na(df$outmost_year)])
  if (length(unchecked) > 0) {
    cli::cli_inform(c(
      "i" = "{.field outmost_year} missing for {length(unchecked)} image{?s}, ring years not checked against it:",
      cli_truncated_list(unchecked)
    ))
  }

  after <- unique(df$image_label[which(df$last_year > df$outmost_year + 1)])
  if (length(after) > 0) {
    return(report_issue(c(
      "!" = "Ring years after {.field outmost_year} (+ 1 for an incomplete ring) in {length(after)} image{?s}:",
      cli_truncated_list(after),
      "i" = "Check the dating of these images or {.field outmost_year} in the {.cls QWAimages} object."
    ), strict))
  }
  invisible(TRUE)
}

#' @noRd
check_cell_years <- function(df_cells, strict = FALSE) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  in_future <- df_cells |>
    dplyr::distinct(.data$image_label, .data$year) |>
    dplyr::filter(.data$year > current_year) |>
    dplyr::pull("image_label") |> unique()
  if (length(in_future)>0) {
    return(report_issue(c(
      "!" = "The data has dating problems:",
      "i" = "Invalid future years in {length(in_future)} image{?s}:",
      cli_truncated_list(in_future)
    ), strict))
  }
  invisible(TRUE)
}

# align a cells or rings table to its schema, extra columns kept; abort on
# missing key columns instead of adding them empty
#' @noRd
align_data <- function(df, schema) {
  missing_keys <- setdiff(names(key_props(schema)), names(df))
  if (length(missing_keys) > 0) {
    cli::cli_abort(c("x" = "{.field {schema}}: missing key column{?s} {.field {missing_keys}}."))
  }
  align_to_schema(df, schema_tbl_props(schema), schema, keep_extra = TRUE)
}

# duplicate cells key (image, year, xpix, ypix): no class invariant, as some
# ROXAS versions produce them, but the key of the cells in TRIA
#' @noRd
check_cells_key <- function(df_cells, strict = FALSE) {
  key <- c("image_label", "year", "xpix", "ypix")
  dt <- data.table::as.data.table(df_cells[key])
  dupl <- duplicated(dt) | duplicated(dt, fromLast = TRUE)
  if (any(dupl)) {
    imgs <- unique(df_cells$image_label[dupl])
    return(report_issue(c(
      "!" = "{.field cells}: {sum(dupl)} row{?s} with duplicate {.field {key}} (the cells key in TRIA) in {length(imgs)} image{?s}:",
      cli_truncated_list(imgs)
    ), strict))
  }
  invisible(TRUE)
}

# (image, year) of df without a rings row; warns (aborts with strict) and
# returns the missing (image, year) invisibly
#' @noRd
check_rows_in_rings <- function(df, rings, what, strict = FALSE, consequence = NULL,
                                hint = "Run {.fn complete_QWAdata} to add the rings rows.") {
  missing <- data.frame(image_label = df$image_label, year = df$year) |>
    dplyr::distinct() |>
    dplyr::anti_join(rings, by = c("image_label", "year"))
  if (nrow(missing) > 0) {
    imgs <- unique(missing$image_label)
    report_issue(c(
      "!" = "{.field rings}: no row for {nrow(missing)} year{?s} with {what} in {length(imgs)} image{?s}:",
      cli_truncated_list(imgs),
      "i" = consequence,
      "i" = hint
    ), strict)
  }
  invisible(missing)
}

# cells vs. rings: shared images, a rings row for each (image, year) with
# cells, and cno (if present) equal to the number of cells
#' @noRd
check_cells_rings <- function(x, strict = FALSE) {
  if (is.null(x$cells) || is.null(x$rings)) return(invisible(TRUE))
  imgs_cells <- unique(x$cells$image_label)
  imgs_rings <- unique(x$rings$image_label)
  if (length(intersect(imgs_cells, imgs_rings)) == 0) {
    return(report_issue(c("!" = "{.field cells} and {.field rings} have no images in common."), strict))
  }

  ok <- TRUE
  only_cells <- setdiff(imgs_cells, imgs_rings)
  if (length(only_cells) > 0) {
    ok <- report_issue(c(
      "!" = "{length(only_cells)} image{?s} in {.field cells} but not in {.field rings}:",
      cli_truncated_list(only_cells)
    ), strict)
  }
  only_rings <- setdiff(imgs_rings, imgs_cells)
  if (length(only_rings) > 0) {
    ok <- report_issue(c(
      "!" = "{length(only_rings)} image{?s} in {.field rings} but not in {.field cells}:",
      cli_truncated_list(only_rings)
    ), strict)
  }

  # images in both only
  hint <- "Run {.fn complete_QWAdata} to add the rings rows and recount {.field cno}."
  n_cells <- x$cells |>
    dplyr::filter(.data$image_label %in% imgs_rings) |>
    dplyr::count(.data$image_label, .data$year, name = "n_cells")
  no_ring <- check_rows_in_rings(n_cells, x$rings, "cells", strict, hint = hint)
  if (nrow(no_ring) > 0) ok <- FALSE
  if ("cno" %in% names(x$rings)) {
    cmp <- x$rings |>
      dplyr::filter(.data$image_label %in% imgs_cells) |>
      dplyr::select("image_label", "year", "cno") |>
      dplyr::left_join(n_cells, by = c("image_label", "year")) |>
      dplyr::mutate(n_cells = dplyr::coalesce(.data$n_cells, 0L))
    differs <- dplyr::coalesce(cmp$cno != cmp$n_cells, TRUE)
    if (any(differs)) {
      imgs <- unique(cmp$image_label[differs])
      ok <- report_issue(c(
        "!" = "{.field rings}: {.field cno} differs from the number of cells in {sum(differs)} ring{?s} of {length(imgs)} image{?s}:",
        cli_truncated_list(imgs), "i" = hint
      ), strict)
    }
  }
  invisible(ok)
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
#' - ew_lw: indicates if it is an earlywood (`"ew"`) or latewood (`"lw"`) cell
#'   (based on Mork index <1 for EW)
#' And for the rings, we add
#' - eww and lww estimates  (based on Mork index of 1)
#' If any of these columns already exist in the data they are NOT recalculated, only
#' the missing measures are added.
#'
#' @param QWA_data a `QWAdata` object containing the cells and rings data frames
#' @param only optional character vector of measure names to calculate. When
#'   supplied, only measures listed here are eligible for calculation (derived measures
#'   not in `only` are skipped). `NULL` (default) adds all missing measures.
#' @return a `QWAdata` object with the updated cells and rings dataframes with the new measures
#' @export
complete_measures <- function(QWA_data, only = NULL) {
  validate_QWAdata(QWA_data)
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
        dplyr::mutate(ew_lw = dplyr::if_else(.data$sector100 <= .data$max_EW_sector, "ew", "lw")) |>
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
#' check if an innermost ring is incomplete, while the outermost ring is flagged
#' as incomplete if it has no MRW, an MRW of zero, or fewer than five cells.
#' The same rules are applied to ROXAS AI data.
#'
#' **Missing** rings are for years that have no discernible ring in the image, but
#' have been manually added in ROXAS during cross-dating, leading to an entry
#' in the rings data but no corresponding no entries (cells) in the cells data.
#' This is usually the case with wedging rings.
#'
#' **Duplicate** rings are those that are present in multiple images due to them
#' overlapping. All years which have cells in more than one image are flagged
#' and ranked by their number of cells, with missing and incomplete rings ranked
#' last. `exclude_dupl` is `TRUE` for all but the best-ranked ring of each
#' overlap, which is the one usually selected for further analysis when
#' building chronologies.
#'
#' The `exclude_issues` column is initialised from `incomplete_ring`. Missing
#' rings are not excluded, as their zero width is a valid measurement. To use a
#' different rule, set `exclude_issues` afterwards, e.g.
#' `x$rings$exclude_issues <- x$rings$incomplete_ring | x$rings$missing_ring`.
#'
#' @param x a `QWAdata` object containing the cells and rings dataframes
#' @param meta a [QWAimages] object or data.frame providing image-level
#'   information on `spatial_resolution` (required for the incomplete
#'   innermost ring check) and `outmost_year`.
#' @param exclude_mode Deprecated. `"either"` initialises `exclude_issues`
#'   from incomplete or missing rings, `"incomplete_only"` from incomplete
#'   rings only (now the default behaviour).
#' @returns A `QWAdata` object with the validated data: cells unchanged, rings
#'   with added flag columns.
#' @export
#'
complete_flags <- function(x, meta, exclude_mode = NULL) {
  validate_QWAdata(x)
  all_flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")
  flag_cols_missing <- setdiff(all_flag_cols, names(x$rings))
  flag_cols_existing <- intersect(all_flag_cols, names(x$rings))

  df_rings_log <- x$rings

  if ("incomplete_ring" %in% flag_cols_missing) {
    consequence <- "Their innermost rings are flagged as incomplete."
    check_images_in_meta(
      unique(df_rings_log$image_label), meta, "rings",
      consequence = consequence,
      hint = "Pass the {.cls QWAimages} object the data were read with, or run {.fn update_QWAdata}."
    )
    check_spatial_resolution(
      meta[meta$image_label %in% df_rings_log$image_label, ],
      consequence = consequence
    )
    df_rings_log <- flag_incomplete_rings(df_rings_log, x$cells, meta)
  }

  if ("missing_ring" %in% flag_cols_missing) {
    df_rings_log <- df_rings_log |>
      dplyr::mutate(missing_ring = !.data$incomplete_ring &                  # wedging ring: cannot assess for incomplete in image 
                                    dplyr::coalesce(.data$mrw < 10, FALSE) | # close to 0 mrw, or close to 0 cno if mrw is na
                                    (.data$cno < 5 & is.na(.data$mrw)),      # a wide ring with no cells usually has other issues
                    no_MRW_other = is.na(.data$mrw) & !(.data$outermost_ring | .data$innermost_ring)) # TODO: check if this ever occurs and for what reason
    # for missing rings, we want area and width set to 0
    missing_to_zero <- c("mrw","ra")
    df_rings_log <- df_rings_log |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(missing_to_zero),
          \(x) dplyr::if_else(.data$missing_ring & is.na(x) & .data$cno < 5, 0, x)),
        dplyr::across(dplyr::any_of(c("eww","lww")), # if mrw is 0 -> eww and lww also 0
         \(x) dplyr::if_else(is.na(x) & dplyr::coalesce(.data$mrw == 0, FALSE), 0, x))
      )
  }

  if (any(c("duplicate_ring", "exclude_dupl") %in% flag_cols_missing)) {
    df_rings_log <- flag_duplicate_rings(df_rings_log) # replace with duplicate_sel?
  }

  mode <- resolve_exclude_mode(exclude_mode)
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

# Internal S3 validator ----
#' Abort if a QWAdata object violates the class invariants
#'
#' At least one of `$cells` and `$rings`, each a data frame or `NULL`; key
#' columns present, typed and not `NA`; (image, year) unique in `$rings`.
#' @param hint Optional hint added to the messages on `NA` and duplicate keys.
#' @returns `x`, invisibly.
#' @noRd
validate_QWAdata <- function(x, hint = NULL) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_data_frame(x$cells, null.ok = TRUE, .var.name = "cells")
  checkmate::assert_data_frame(x$rings, null.ok = TRUE, .var.name = "rings")
  if (is.null(x$cells) && is.null(x$rings)) {
    cli::cli_abort("{.cls QWAdata} requires at least one of {.field cells} or {.field rings}.")
  }
  if (!is.null(x$cells)) {
    validate_keys(x$cells, key_props("cells"), "cells", hint = hint)
  }
  if (!is.null(x$rings)) {
    validate_keys(x$rings, key_props("rings"), "rings",
                  unique_by = c("image_label", "year"), hint = hint)
  }
  invisible(x)
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
#' At least one component must be provided. The components are aligned with
#' their schema (column types and order); columns not in the schema are kept
#' after the schema columns. Aborts on missing, `NA` or (in `$rings`) duplicate
#' keys. Warns on missing measurement columns, duplicate cell keys (image,
#' year, `xpix`, `ypix`), dating problems, images only in `$cells` or only in
#' `$rings`, years with cells but without a rings row, and `cno` not matching
#' the number of cells. The rings are not completed here: [complete_QWAdata()]
#' adds the missing rings rows and `cno`.
#'
#' The typical workflow builds a
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

  # align to schema (extra columns kept), expected raw measurement columns
  comps <- list(cells = cells, rings = rings)
  for (comp in names(purrr::compact(comps))) {
    missing_meas_cols <- setdiff(schema_cols(comp, "measure"), names(comps[[comp]]))
    comps[[comp]] <- align_data(comps[[comp]], comp)
    if (length(missing_meas_cols) > 0) {
      cli::cli_warn(c(
        "!" = "{.field {comp}} is missing expected raw measurement columns:",
        "i" = "{missing_meas_cols}"
      ))
    }
  }
  x <- validate_QWAdata(new_QWAdata(comps$cells, comps$rings))

  # data quality (rings rows and cno are completed by complete_QWAdata())
  if (!is.null(x$cells)) {
    check_cwt(x$cells) # cwt estimates expected for conifers
    check_cells_key(x$cells)
    # with rings, future years in cells are reported with the rings (or as missing rings rows)
    if (is.null(x$rings)) check_cell_years(x$cells)
  }
  if (!is.null(x$rings)) check_ring_years(x$rings)
  check_cells_rings(x)

  x
}


# Methods (general and specific) ------
#' Print a QWAdata object
#'
#' Displays a compact overview of a [QWAdata] object: which components are
#' present, the spatial and temporal coverage of the data, and (if the flag
#' columns are present, see [complete_QWAdata()]) a summary of the automatic
#' ring flags.
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

  # --- Flags (only after complete_QWAdata) ---
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
#' 1. Aligns column types and extends `$rings` to complete year sequences per
#'    image, with the cell count `cno` from `$cells`; warns on dating problems.
#' 2. Adds derived cell measures and the earlywood/latewood estimates (see
#'    [complete_measures()]).
#' 3. Flags incomplete, missing, and duplicate rings and initialises
#'    `exclude_dupl` and `exclude_issues` (see [complete_flags()]).
#'
#' @param x A [QWAdata] object with both `$cells` and `$rings`.
#' @param meta A [QWAimages] object providing `spatial_resolution` and
#'   `outmost_year`, required for the incomplete-innermost and dating checks.
#' @param exclude_mode Deprecated, see [complete_flags()].
#' @returns A [QWAdata] object with all derived columns and flag columns populated.
#' @seealso [QWAdata()], [check_QWAdata()], [complete_flags()],
#'   [complete_measures()]
#' @export
complete_QWAdata <- function(x, meta, exclude_mode = NULL) {
  validate_QWAdata(x)
  validate_QWAimages(meta)
  checkmate::assert_subset(c("spatial_resolution", "outmost_year"), names(meta))

  # align to schema (extra columns kept)
  if (!is.null(x$cells)) x$cells <- align_data(x$cells, "cells")
  if (!is.null(x$rings)) x$rings <- align_data(x$rings, "rings")

  # ensure complete sequences of years (note: creates rings df from cells if null)
  x$rings <- complete_rings(x)
  check_ring_years(x$rings)
  check_outmost_year(x$rings, meta)

  # complete measures (derived cell measures and ew/lw estimates)
  x <- complete_measures(x)

  # complete flags columns if not yet initialized
  # TODO: option to initialise complete set of flags incl the manual ones?
  x <- complete_flags(x, meta, exclude_mode = exclude_mode)

  cli::cli_alert_success("Added derived measures and flags to `QWAdata` object")
  x

}


#' Check a QWAdata object
#'
#' Checks a [QWAdata] object before submission. Aborts if the object is not a
#' valid `QWAdata` (missing, mistyped or `NA` keys, duplicate (image, year) in
#' `$rings`). Warns (or, with `strict = TRUE`, aborts) on:
#' - values violating the cells or rings schema (types, ranges, allowed values);
#' - duplicate cell keys (image, year, `xpix`, `ypix`), the key of the cells
#'   in TRIA;
#' - dating problems: gaps and future years;
#' - images only in `$cells` or only in `$rings`, years with cells but without
#'   a rings row, and `cno` not matching the number of cells (re-run
#'   [complete_QWAdata()]).
#'
#' Also informs about missing optional columns, extra columns (kept, but not
#' imported into TRIA by default) and images without CWT estimates.
#'
#' @param x A [QWAdata] object.
#' @param strict If `TRUE`, abort on the first issue instead of warning.
#' @param exclude_mode Deprecated and ignored.
#' @returns `TRUE` if no issue was found, `FALSE` otherwise (invisibly).
#' @seealso [QWAdata()], [complete_QWAdata()], [check_consistency()]
#' @export
check_QWAdata <- function(x, strict = FALSE, exclude_mode = NULL) {
  validate_QWAdata(x)
  checkmate::assert_flag(strict)
  resolve_exclude_mode(exclude_mode)

  ok <- TRUE
  for (comp in c("cells", "rings")) {
    if (is.null(x[[comp]])) next
    tbl_props <- schema_tbl_props(comp)
    ok <- c(ok, check_data_schema(x[[comp]], tbl_props, comp, strict))
    check_missing_opt(x[[comp]], tbl_props, comp)
  }

  if (!is.null(x$cells)) {
    check_cwt(x$cells)
    ok <- c(ok, check_cells_key(x$cells, strict))
    # with rings, future years in cells are reported with the rings (or as missing rings rows)
    if (is.null(x$rings)) ok <- c(ok, check_cell_years(x$cells, strict))
  }
  if (!is.null(x$rings)) ok <- c(ok, check_ring_years(x$rings, strict = strict))
  ok <- c(ok, check_cells_rings(x, strict))

  if (all(ok)) {
    cli::cli_inform(c("v" = "{.cls QWAdata}: all checks passed."))
  } else {
    cli::cli_inform(c("x" = "{.cls QWAdata}: issues found, see warnings."))
  }
  invisible(all(ok))
}


#' Write a QWAdata object to files
#'
#' Cells and rings are written as (compressed) CSV files. Empty values are
#' written as empty fields.
#'
#' @param x A [QWAdata] object.
#' @param dir Directory to write to. Files are auto-named
#'   `{dataset_name}_QWAcells.csv(.gz)` and
#'   `{dataset_name}_QWArings.csv(.gz)`. Mutually exclusive with
#'   `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit output paths for the cells and rings
#'   CSV files. Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Name prefix for auto-generated filenames when using
#'   `dir`. If omitted, files are named `QWAcells.csv(.gz)` and
#'   `QWArings.csv(.gz)`.
#' @param compress If `TRUE` (default), write `.csv.gz` files.
#' @param overwrite Allow to overwrite existing files? (default `FALSE`).
#' @param drop_empty If `TRUE` (default), optional columns set in the flags app
#'   (`colType` `"qc_input"`, e.g. ring quality flags and comments) that are
#'   empty (`NA`, or `NA`/`FALSE` for flags) in all rows are not written.
#' @param keep_derived If `FALSE`, derived columns (`colType` `"derived"`, e.g.
#'   cell measures computed from other columns) are not written. They can be
#'   recomputed with [complete_QWAdata()]. Default `TRUE`.
#' @returns A named list of written file paths, invisibly.
#' @seealso [read_QWAdata()], [write_QWAprofile()]
#' @export
write_QWAdata <- function(x, dir = NULL,
                          file_cells = NULL, file_rings = NULL,
                          dataset_name = NULL,
                          compress = TRUE, overwrite = FALSE,
                          drop_empty = TRUE, keep_derived = TRUE) {
  validate_QWAdata(x)
  checkmate::assert_flag(drop_empty)
  checkmate::assert_flag(keep_derived)
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
      base_cells <- "QWAcells"
      base_rings <- "QWArings"
    } else {
      prefix <- gsub("[^[:alnum:]_-]", "_", dataset_name)
      base_cells <- paste0(prefix, "_QWAcells")
      base_rings <- paste0(prefix, "_QWArings")
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

  if (!keep_derived) {
    x$cells <- x$cells[setdiff(names(x$cells), schema_cols("cells", "derived"))]
    x$rings <- x$rings[setdiff(names(x$rings), schema_cols("rings", "derived"))]
  }

  vroom::vroom_write(prepare_write(x$cells, "cells", drop_empty), file_cells,
                     delim = ",", na = "")
  vroom::vroom_write(prepare_write(x$rings, "rings", drop_empty), file_rings,
                     delim = ",", na = "")

  cli::cli_inform(c(
    "v" = "Cells written to {.file {file_cells}}",
    "v" = "Rings written to {.file {file_rings}}"
  ))

  invisible(list(file_cells = file_cells, file_rings = file_rings))
}

#' Read a QWAdata object from CSV files
#'
#' Reads cells and/or rings from (compressed) CSV files. To load only one
#' component (e.g. to avoid reading a large cells file when only rings are
#' needed), provide only the corresponding `file_cells`/`file_rings` argument.
#'
#' @param dir Directory to search for cells and rings files. Both are read if
#'   found; if one is missing, a warning is issued and that component is
#'   omitted. Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit paths to the cells and rings CSV
#'   files. Either or both may be given; the omitted component is `NULL` in
#'   the returned [QWAdata] object. Mutually exclusive with `dir`.
#' @param dataset_name Optional string to disambiguate when multiple matching
#'   files are found in `dir`.
#' @returns A [QWAdata] object.
#' @seealso [write_QWAdata()], [read_QWAprofile()]
#' @export
read_QWAdata <- function(dir = NULL, file_cells = NULL, file_rings = NULL,
                         dataset_name = NULL) {
  use_dir <- !is.null(dir)
  any_files <- !is.null(file_cells) || !is.null(file_rings)
  if (use_dir == any_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or {.arg file_cells}/{.arg file_rings}, not both or neither.")
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

    resolve <- function(component, pattern) {
      cands <- filter_candidates(pattern)
      if (length(cands) > 1)
        cli::cli_abort("Could not uniquely identify a {component} file in {.path {dir}} ({length(cands)} matches).")
      if (length(cands) == 0) {
        cli::cli_warn("No {component} file found in {.path {dir}}, skipping.")
        return(NULL)
      }
      cands
    }

    file_cells <- resolve("cells", "cells")
    file_rings <- resolve("rings", "rings")
  }

  rings <- NULL
  cells <- NULL

  if (!is.null(file_rings)) {
    cli::cli_inform(c(" " = "Reading rings data from {.file {file_rings}}..."))
    rings <- vroom::vroom(file_rings, show_col_types = FALSE)
    # back-compatibility: older files used dh_w / dh_m for what are now dhw / dhm
    legacy <- c(dhw = "dh_w", dhm = "dh_m") # new = old
    rings <- rings |>
      dplyr::rename(dplyr::any_of(legacy))
    cli::cli_inform(c("v" = "{nrow(rings)} rings read from file"))
  }

  if (!is.null(file_cells)) {
    cli::cli_inform(c(" " = "Reading cells data from {.file {file_cells}}..."))
    cells <- vroom::vroom(file_cells, show_col_types = FALSE)
    # back-compatibility: older files used "EW" / "LW" for ew_lw
    if ("ew_lw" %in% names(cells)) {
      cells <- cells |>
        dplyr::mutate(ew_lw = tolower(.data$ew_lw))
    }
    cli::cli_inform(c("v" = "{nrow(cells)} cells read from file"))
  }

  QWAdata(cells = cells, rings = rings)
}
