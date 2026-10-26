#' Calculate radial profiles
#'
#' @description
#' `calculate_sector_profiles()` divides the cells of each ring into
#' `n_sectors` equal-width sectors (by relative radial position) and computes
#' per-sector means and optionally quantiles of the selected cell parameters.
#' The returned `QWAprofile` object includes a column `sector_n` with values in
#' \[1,..., `n_sectors`\] for each `image_label` and `year`.
#'
#' `calculate_band_profiles()` computes moving-band profiles of selected cell
#' parameters across the ring width. Each band is a window of `bandwidth`
#' microns that shifts in steps of `stepsize` microns from the innner to the
#' outer ring boundary. The returned `QWAprofile` object includes columns
#' `start` and `end` with the band limits in microns (with the `mrw` value for 
#' context) for each `image_label` and `year`.
#'
#' For each selected parameter, the returned columns include number of valid 
#' cells used in the aggregation (`<param>_N`), the mean (`<param>_mean`) for 
#' each sector or band, and the quantile columns for the given probabilities
#' (named by percentage, e.g. `la_q25`, `la_q05`, `la_q25.7`).
#'
#' @details
#' In order to use `apply_exclusions = TRUE`, the `QWA_data$rings` component
#' must be provided and include the flag columns `exclude_dupl` and
#' `exclude_issues`.
#' In addition, to exclude only the cells from affected tissue (via
#' `tissue = "affected"`), the `ew_lw` assignment in `$cells` must exist (see
#' [complete_QWAdata()]).
#' The exclusion flag and `affected_tissue` columns can be edited via
#' [launch_flags_app()] before the profile calculation, if you want a 'clean'
#' profile object that does not include rows from overlapping or problematic
#' rings (or tissue).
#'
#' To return a `ew_band` column in a band-type `QWAprofile` (indicating if a
#' band falls entirely within the earlywood), the `$rings` component must
#' include an `eww` column, which can be added via [complete_QWAdata()].
#'
#' @param QWA_data A [QWAdata] object with a non-`NULL` `$cells` component
#'   (and `$rings` component for `calculate_band_profiles()`).
#' @param n_sectors Number of equal-width sectors to divide each ring into.
#' @param sel_cell_params Character vector of cell parameters to include in 
#'   the profiles (either a subset of the measurement column names of
#'   `QWA_data$cells`, e.g. `c("la", "cwtrad")`, or `"all"`). NOTE: using 
#'   `"all"` results in a large number of columns in the returned `QWAprofile`.
#' @param quant_probs Numeric vector of distinct quantile probabilities in
#'   \[0, 1\] (e.g. `c(0.25, 0.5, 0.75)`). Set `NULL` or empty to derive only
#'  means.
#' @param apply_exclusions If `TRUE`, the cells of excluded rings
#'   (`exclude_dupl`, `exclude_issues` in `$rings`) are filtered out, returning
#'   'clean' measurement series ready for analyses. Default `FALSE`.
#' @param tissue Only used with `apply_exclusions = TRUE`. `"all"` (default)
#'   removes all cells of excluded rings; `"affected"` removes only the
#'   earlywood or latewood cells (by `ew_lw`) of rings with `exclude_issues`
#'   and `affected_tissue` `"ew"` or `"lw"`, respectively.
#' 
#' @returns A [QWAprofile] object with `profile_type = "sector"` or
#'   `profile_type = "band"`.
#'
#' @seealso [QWAprofile()]
#' @export
calculate_sector_profiles <- function(QWA_data, n_sectors, 
                                      sel_cell_params, quant_probs = NULL,
                                      apply_exclusions = FALSE,
                                      tissue = c("all", "affected")) {
  tissue <- rlang::arg_match(tissue)
  sel_cell_params <- check_profile_args(QWA_data, sel_cell_params, quant_probs,
                                        apply_exclusions, tissue)
  checkmate::assert_count(n_sectors, positive = TRUE)

  # NOTE: use data.table with collapse for speed on large dataframes
  cells_dt <- data.table::as.data.table(QWA_data$cells)

  if (apply_exclusions) {
    cells_dt <- exclude_cells(cells_dt, QWA_data$rings, tissue)
  }

  # filter out cells without valid rraddistr, subset to relevant columns only
  cells_dt <- cells_dt[!is.na(rraddistr),
                       c("image_label", "year", "rraddistr", sel_cell_params),
                       with = FALSE]
  
  # cut cells into n sectors based on their relative radial position
  cells_dt[,
           sector_n := as.numeric(
             cut(rraddistr, breaks = seq(from = 0, to = 100, by = 100/n_sectors),
                 labels = 1:n_sectors, include.lowest = TRUE))]
  cells_dt[rraddistr > 100 & rraddistr <= 101, # allow for rounding errors
           sector_n := n_sectors]
  cells_dt <- cells_dt[!is.na(sector_n)] # remove cells outside of sectors
  cells_dt[, rraddistr := NULL]

  # aggregate over each sector
  cli::cli_inform(c("i"= "Calculating sector counts and means..."))
  prf_data_agg <- cells_dt |>
    collapse::fgroup_by(c("image_label", "year", "sector_n")) |>
    collapse::fsummarise(across(sel_cell_params,
                                list(N = collapse::fnobs,
                                     mean = collapse::fmean)))

  if (!is.null(quant_probs) && length(quant_probs) > 0) {
    cli::cli_inform(c("i"= "Calculating sector quantiles..."))
    group_cols <- c("image_label", "year", "sector_n")
    prf_data_agg <- prf_data_agg[
      calc_quantiles(cells_dt, group_cols, sel_cell_params, quant_probs),
      on = group_cols]
  }

  cli::cli_inform(c("v"= "All done!"))
  new_QWAprofile(tibble::as_tibble(prf_data_agg), profile_type = "sector")
}

#' @param bandwidth Width of each band in microns.
#' @param stepsize Step size between band starts in microns.
#' @param band_rebound If `TRUE` (default), the last band is shifted so its end
#'   coincides exactly with the ring width (`mrw`).
#' @rdname calculate_sector_profiles
#' @export
calculate_band_profiles <- function(QWA_data,
                                    bandwidth, stepsize,
                                    sel_cell_params,
                                    quant_probs = NULL,
                                    band_rebound = TRUE,
                                    apply_exclusions = FALSE,
                                    tissue = c("all", "affected")) {
  tissue <- rlang::arg_match(tissue)
  sel_cell_params <- check_profile_args(QWA_data, sel_cell_params, quant_probs,
                                        apply_exclusions, tissue)
  checkmate::assert_data_frame(QWA_data$rings, null.ok = FALSE)
  checkmate::assert_names(names(QWA_data$rings),
    must.include = c("image_label", "year", "mrw"))
  # with apply_exclusions, check_profile_args() already aborts on these
  if (!apply_exclusions) {
    check_rows_in_rings(QWA_data$cells, QWA_data$rings, "cells",
      consequence = "Band positions are computed from {.field mrw} in {.field rings}, so these cells are left out of the band profiles.")
  }
  checkmate::assert_number(bandwidth, lower = 1)
  checkmate::assert_number(stepsize, lower = 1, upper = bandwidth)
  checkmate::assert_flag(band_rebound)

  has_eww <- "eww" %in% names(QWA_data$rings)

  # NOTE: use data.table with collapse for speed on large dataframes
  cells_dt <- data.table::as.data.table(QWA_data$cells)
  
  if (apply_exclusions) {
    cells_dt <- exclude_cells(cells_dt, QWA_data$rings, tissue)
  }

  # raddistr.st (rraddistr * mrw / 100) positions each cell in absolute microns
  # within its ring. It is NA for cells without an mrw (missing / incomplete rings).
  mrw_dt <- data.table::as.data.table(QWA_data$rings)[, c("image_label", "year", "mrw")]
  cells_dt <- mrw_dt[cells_dt, on = c("image_label", "year")]
  cells_dt[, raddistr.st := rraddistr * mrw / 100]
  cells_dt[, mrw := NULL]
  # filter out cells without valid raddistr.st, subset to relevant columns only
  cells_dt <- cells_dt[!is.na(raddistr.st),
                       c("image_label", "year", "raddistr.st", sel_cell_params),
                       with = FALSE]


  cli::cli_inform(c("i"= "Creating band definitions..."))
  # get the ring widths and ew widths (if available)
  ring_cols <- c("image_label", "year", "mrw", if (has_eww) "eww")
  ring_widths <- data.table::as.data.table(QWA_data$rings)[
    !is.na(mrw) & mrw > bandwidth, ring_cols, with = FALSE]
  # create band definitions for each ring
  band_defs <- ring_widths[,
                           create_bands_dt(mrw[1], bandwidth, stepsize, band_rebound),
                           by = .(image_label, year)
  ]
  data.table::setkey(band_defs, image_label, year, start, end)

  # fast way to find the band(s) which each cell is in using foverlaps:
  # it joins cells_dt with band_defs for all rows where the raddistr.st based
  # (start-end) 'interval' is within the band (start-end) interval
  # Note: we add a small tolerance to the 'end' of the cell position 'interval'
  # so that cells exactly on the edge between to bands are included only in one
  cells_dt[, c("start", "end") := list(raddistr.st, raddistr.st+0.01)]
  cells_dt[, raddistr.st := NULL]
  cli::cli_inform(c("i"= "Assigning cells to bands..."))
  cell_bands <- data.table::foverlaps(cells_dt, band_defs, type = "within",
                                      mult = "all", nomatch = NULL)
  cell_bands[, c("i.start", "i.end") := NULL] # remove the dt position columns
  rm(cells_dt) # free memory

  # aggregate over each band
  cli::cli_inform(c("i"= "Calculating band counts and means..."))
  prf_data_agg <- cell_bands |>
    collapse::fgroup_by(c("image_label", "year", "start", "end")) |>
    collapse::fsummarise(across(sel_cell_params,
                                list(N = collapse::fnobs,
                                     mean = collapse::fmean)))

  if (!is.null(quant_probs) && length(quant_probs) > 0){
    cli::cli_inform(c("i"= "Calculating band quantiles..."))
    group_cols <- c("image_label", "year", "start", "end")
    prf_data_agg <- prf_data_agg[
      calc_quantiles(cell_bands, group_cols, sel_cell_params, quant_probs),
      on = group_cols]
  }

  prf_data_agg <- ring_widths[prf_data_agg, on = c("image_label", "year")]
  prf_data_agg[, end := start + bandwidth] # restore original end values of last bands (without tolerance)

  if (has_eww) {
    cli::cli_inform(c("i"= "Adding EW indicator..."))
    # add a ew_band column to indicate if band is in EW or LW
    prf_data_agg[, ew_band := data.table::fifelse(end <= eww, TRUE, FALSE)]
    data.table::setcolorder(prf_data_agg, "ew_band", after="eww")
  }
  
  cli::cli_inform(c("v"= "All done!"))
  new_QWAprofile(tibble::as_tibble(prf_data_agg), profile_type = "band")
}

#' Check the common arguments of the profile functions
#'
#' @inheritParams calculate_sector_profiles
#' @param tissue `"all"` or `"affected"` (already matched)
#' @return `sel_cell_params`, with `"all"` resolved to the cell measurement
#'   columns present in `QWA_data$cells`
#' @noRd
check_profile_args <- function(QWA_data, sel_cell_params, quant_probs,
                               apply_exclusions, tissue) {
  validate_QWAdata(QWA_data)
  checkmate::assert_data_frame(QWA_data$cells, null.ok = FALSE)
  checkmate::assert_names(names(QWA_data$cells),
    must.include = c("image_label", "year", "rraddistr"))

  valid_params <- intersect(cell_param_cols(), names(QWA_data$cells))
  checkmate::assert_character(sel_cell_params, any.missing = FALSE,
                              min.len = 1, unique = TRUE)
  if (identical(sel_cell_params, "all")) sel_cell_params <- valid_params
  checkmate::assert_subset(sel_cell_params, valid_params, empty.ok = FALSE)

  checkmate::assert_numeric(quant_probs, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE, null.ok = TRUE)

  checkmate::assert_flag(apply_exclusions)
  if (apply_exclusions) {
    check_rings_excl_cols(QWA_data$rings)
    check_rows_in_rings(QWA_data$cells, QWA_data$rings, "cells", strict = TRUE,
      consequence = "Exclusions are applied by rings row, so these cells cannot be excluded.")
    if (tissue == "affected" && !"ew_lw" %in% names(QWA_data$cells)) {
      cli::cli_abort(c(
        "{.code tissue = \"affected\"} requires the {.field ew_lw} column in {.arg QWA_data$cells}.",
        "i" = "Hint: Have you run {.fn complete_QWAdata} first?"
      ))
    }
  }

  sel_cell_params
}

#' Remove the cells of excluded rings
#'
#' Drops all cells of rings with `exclude_dupl` or `exclude_issues`. With
#' `tissue = "affected"`, rings with `exclude_issues` and `affected_tissue`
#' `"ew"` or `"lw"` only lose their cells of that tissue (by `ew_lw`).
#'
#' @param cells_dt a data.table of cell data
#' @param df_rings the rings data frame with the exclusion flag columns
#' @param tissue `"all"` or `"affected"`
#' @return `cells_dt` without the excluded cells
#' @details Assumes the inputs have been checked by `check_profile_args()`.
#' @noRd
exclude_cells <- function(cells_dt, df_rings, tissue) {
  rings_dt <- data.table::as.data.table(df_rings)
  if (!"affected_tissue" %in% names(rings_dt)) rings_dt[, affected_tissue := NA_character_]
  # NA flags count as FALSE
  rings_dt[, c("exclude_dupl", "exclude_issues") := list(
    data.table::fcoalesce(exclude_dupl, FALSE),
    data.table::fcoalesce(exclude_issues, FALSE))]
  rings_dt <- rings_dt[exclude_dupl | exclude_issues,
                       c("image_label", "year", "exclude_dupl", "affected_tissue"),
                       with = FALSE]
  # tissue to drop per excluded ring: "ew", "lw" or "all"
  rings_dt[, drop_tissue := data.table::fifelse(
    tissue == "affected" & !exclude_dupl & affected_tissue %in% c("ew", "lw"),
    affected_tissue, "all")]
  rings_dt <- rings_dt[, c("image_label", "year", "drop_tissue"), with = FALSE]

  # keep cells of non-excluded rings, and of partially excluded rings the
  # cells of the other tissue (cells without ew_lw are dropped)
  cells_dt <- rings_dt[cells_dt, on = c("image_label", "year")]
  if (tissue == "all") {
    cells_dt <- cells_dt[is.na(drop_tissue)]
  } else {
    cells_dt <- cells_dt[is.na(drop_tissue) |
                           (drop_tissue != "all" & 
                             !is.na(ew_lw) & ew_lw != drop_tissue)]
  }
  cells_dt[, drop_tissue := NULL]
  cells_dt
}

#' Quantiles of the selected cell parameters per group
#'
#' Quantile columns are named by percentage, padded
#' to two integer digits, up to 4 decimals, without trailing zeros (e.g.
#' `la_q05`, `la_q25`, `la_q25.7`, `la_q100`).
#' @param cells_dt data.table with the group and parameter columns only
#' @param group_cols character vector of grouping columns
#' @inheritParams calculate_sector_profiles
#' @return data.table with the group columns and one column per parameter and
#'   probability
#' @noRd
calc_quantiles <- function(cells_dt, group_cols, sel_cell_params, quant_probs) {
  prf_data_quant <- cells_dt |>
    collapse::fgroup_by(group_cols) |>
    collapse::BY(collapse::.quantile, probs = quant_probs, expand.wide = TRUE)

  pct <- sub("\\.?0+$", "", sprintf("%07.4f", round(quant_probs * 100, 4)))
  params <- rep(sel_cell_params, each = length(quant_probs))
  data.table::setnames(prf_data_quant,
                       old = paste0(params, ".V", seq_along(quant_probs)),
                       new = paste0(params, "_q", pct))
  prf_data_quant
}

#' Create the moving band definitions for given a mrw, bandwidth and stepsize
#'
#' @param mrw_val numeric, the mrw value to create bands for
#' @param bandwidth numeric, the bandwidth of each band
#' @param stepsize numeric, the step size between band starts
#' @param band_rebound logical, if TRUE, the last band is rebounded to end exactly at mrw_val
#' @return a data.table with start and end positions of each band for the given mrw_val
#' @noRd
create_bands_dt <- function(mrw_val, bandwidth, stepsize, band_rebound = TRUE) {
  band_starts <- seq(0, mrw_val - bandwidth + stepsize, by = stepsize)
  # in seq, the last element + by is <= the to value, thus this definition
  # means the last band's end will always exceed mrw_val, but the second last
  # band will end at <= mrw_val

  # if last band should be rebounded, shift it back so its end coincides with mrw_val
  if (band_rebound) {
    rebounded <- mrw_val - bandwidth
    band_starts[length(band_starts)] <- rebounded
    # avoid duplicate band from rebounded band (iff mrw %% stepsize == 0)
    if (abs(band_starts[length(band_starts)-1] - rebounded) < 1e-10) {
      band_starts <- band_starts[-length(band_starts)]
    }
  }

  band_ends <- band_starts + bandwidth
  band_ends[length(band_ends)] <- band_ends[length(band_ends)] + 1 # add tolerance to last band

  data.table::data.table(start = band_starts, end = band_ends)
}
