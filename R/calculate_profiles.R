#' Calculate sector-wise radial profiles
#'
#' Divides each ring into `n_sectors` equal-width sectors (by relative radial
#' position) and computes per-sector means and optionally quantiles of the
#' selected cell parameters.
#'
#' @param QWA_data A [QWAdata] object with a non-`NULL` `$cells` component.
#' @param n_sectors Number of equal-width sectors to divide each ring into.
#' @param sel_cell_params Character vector of cell parameter column names to
#'   include in the profiles (e.g. `c("la", "cwttan")`).
#' @param quant_probs Numeric vector of quantile probabilities to calculate
#'   (e.g. `c(0.25, 0.5, 0.75)`). `NULL` or empty -> means only.
#' @returns A [QWAprofile] object with `profile_type = "sector"`.
#' @seealso [calculate_band_profiles()], [QWAprofile()]
#' @export
calculate_sector_profiles <- function(QWA_data, n_sectors, sel_cell_params, quant_probs = NULL){
  checkmate::assert_class(QWA_data, "QWAdata")
  checkmate::assert_data_frame(QWA_data$cells, null.ok = FALSE)

  # TODO: allow to select all params?
  # if (length(sel_cell_params) == 1 && sel_cell_params == "all"){
  #   sel_cell_params <- setdiff(
  #     colnames(QWA_data$cells),
  #     c("image_label", "year", "rraddistr")
  #   )
  # }
  # use data.table for speed on large dataframes
  cells_dt <- data.table::as.data.table(QWA_data$cells)
  # filter out cells without valid rraddistr, subset to relevant columns only
  cells_dt <- cells_dt[!is.na(rraddistr),
                       c("image_label", "year", "rraddistr", sel_cell_params),
                       with = FALSE]
  # cut cells into n sectors based on their relative radial position
  cells_dt[,
           sector_n := as.numeric(
             cut(rraddistr, breaks = seq(from = 0, to = 100, by = 100/n_sectors),
                 labels = 1:n_sectors, include.lowest = TRUE))]
  cells_dt[rraddistr > 100 & rraddistr <= 101,
           sector_n := n_sectors] # allow for rounding errors
  cells_dt <- cells_dt[!is.na(sector_n)] # if we still have some cells outside -> remove
  cells_dt[, rraddistr := NULL] # remove superfluous columns

  # now we can aggregate over each sector
  cli::cli_inform(c("i"= "Calculating sector counts and means..."))
  prf_data_agg <- cells_dt |>
    collapse::fgroup_by(image_label, year, sector_n) |>
    collapse::fsummarise(across(sel_cell_params,
                                list(N = collapse::fnobs,
                                     mean = collapse::fmean)))

  if (!is.null(quant_probs) && length(quant_probs) > 0){
    cli::cli_inform(c("i"= "Calculating sector quantiles..."))
    prf_data_quant <- cells_dt |>
      collapse::fgroup_by(image_label, year, sector_n) |>
      collapse::BY(collapse::.quantile,
                   probs = quant_probs,
                   expand.wide = TRUE)

    old_col_names <- unlist(lapply(sel_cell_params, function(param) {
      paste0(param, ".V", seq(length(quant_probs)))
    }))
    new_col_names <- unlist(lapply(sel_cell_params, function(param) {
      paste0(param, "_q", sprintf("%02d", round(quant_probs*100)))
    }))
    data.table::setnames(prf_data_quant, old = old_col_names, new = new_col_names)

    prf_data_agg <- prf_data_agg[prf_data_quant,
                                 on = c("image_label", "year", "sector_n")]
    rm(prf_data_quant)
  }

  cli::cli_inform(c("v"= "All done!"))
  new_QWAprofile(tibble::as_tibble(prf_data_agg), profile_type = "sector")
}


#' Helper function to create the moving band definitions for given a mrw,
#' bandwidth and stepsize
#'
#' @param mrw_val numeric, the mrw value to create bands for
#' @param bandwidth numeric, the bandwidth of each band
#' @param stepsize numeric, the step size between band starts
#' @param band_rebound logical, if TRUE, the last band is rebounded to end exactly at mrw_val
#' @return a data.table with start and end positions of each band for the given mrw_val
#' @keywords internal
create_bands_dt <- function(mrw_val, bandwidth, stepsize, band_rebound = TRUE) {
  band_starts <- seq(0, mrw_val - bandwidth + stepsize, by = stepsize)
  # in seq, the last element + by is <= the to value, thus this definition
  # means the last band's end will always exceed mrw_val, but the second last
  # band will end at <= mrw_val

  # if last band should be rebounded, shift it back so its end coincides with mrw_val
  if (band_rebound) {
    rebounded <- mrw_val - bandwidth
    band_starts[length(band_starts)] <- rebounded
    # avoid duplicate band from rebounded band (iff mrw %% stepzise == 0)
    if (abs(band_starts[length(band_starts)-1] - rebounded) < 1e-10) {
      band_starts <- band_starts[-length(band_starts)]
    }
  }

  band_ends <- band_starts + bandwidth
  band_ends[length(band_ends)] <- band_ends[length(band_ends)] + 1 # add tolerance to last band

  data.table::data.table(start = band_starts, end = band_ends)
}


#' Calculate band-wise radial profiles
#'
#' Computes moving-band profiles of selected cell parameters across the ring
#' width. Each band is a window of `bandwidth` microns that shifts in steps of
#' `stepsize` microns from the cambial side to the lumen side of the ring.
#'
#' @param QWA_data A [QWAdata] object with non-`NULL` `$cells` and `$rings`
#'   components.
#' @param bandwidth Width of each band in microns.
#' @param stepsize Step size between band starts in microns.
#' @param sel_cell_params Character vector of cell parameter column names to
#'   include in the profiles (e.g. `c("la", "cwttan")`).
#' @param quant_probs Numeric vector of quantile probabilities to calculate
#'   (e.g. `c(0.25, 0.5, 0.75)`). `NULL` or empty → means only.
#' @param band_rebound If `TRUE` (default), the last band is shifted so its end
#'   coincides exactly with the ring width (`mrw`).
#' @returns A [QWAprofile] object with `profile_type = "band"`.
#' @details Requires that [complete_QWAdata()] has been run, as `$rings` must
#'   contain `mrw` and `eww`. `raddistr.st` is computed internally from
#'   `rraddistr` and `mrw`.
#' @seealso [calculate_sector_profiles()], [QWAprofile()]
#' @export
calculate_band_profiles <- function(QWA_data,
                                    bandwidth, stepsize,
                                    sel_cell_params,
                                    quant_probs = NULL,
                                    band_rebound = TRUE) {
  checkmate::assert_class(QWA_data, "QWAdata")
  checkmate::assert_data_frame(QWA_data$cells, null.ok = FALSE)
  checkmate::assert_data_frame(QWA_data$rings, null.ok = FALSE)
  checkmate::assert_subset(c("mrw", "eww"), names(QWA_data$rings),
    .var.name = "QWA_data$rings — run complete_QWAdata() first")
  checkmate::assert_subset(c("rraddistr", "mrw"), names(QWA_data$cells),
    .var.name = "QWA_data$cells — run complete_QWAdata() first")
  # we use data.table for fast operations on the large cells table
  # raddistr.st (rraddistr * mrw / 100) positions each cell in absolute microns
  # within its ring. It is NA for cells without an mrw (missing / incomplete rings).
  cells_dt <- data.table::as.data.table(QWA_data$cells)
  mrw_dt <- data.table::as.data.table(QWA_data$rings)[, c("image_label", "year", "mrw")]
  cells_dt <- mrw_dt[cells_dt, on = c("image_label", "year")]
  cells_dt[, raddistr.st := rraddistr * mrw / 100]
  cells_dt[, mrw := NULL]
  # filter out cells without valid raddistr.st, subset to relevant columns only
  cells_dt <- cells_dt[!is.na(raddistr.st),
                       c("image_label", "year", "raddistr.st", sel_cell_params),
                       with = FALSE]


  cli::cli_inform(c("i"= "Creating band definitions..."))
  # get the ring widths and ew widths
  ring_widths <- data.table::as.data.table(QWA_data$rings)[
    !is.na(mrw) & mrw > bandwidth,
    c("image_label", "year", "mrw", "eww")]
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
  cells_dt[, c("start", "end") := list(raddistr.st, raddistr.st+0.1)]
  cells_dt[, raddistr.st := NULL]
  cli::cli_inform(c("i"= "Assigning cells to bands..."))
  cell_bands <- data.table::foverlaps(cells_dt, band_defs, type = "within",
                                      mult = "all", nomatch = NULL)
  cell_bands[, c("i.start", "i.end") := NULL] # remove the dt position columns
  rm(cells_dt) # free memory

  # now we can aggregate over each band
  cli::cli_inform(c("i"= "Calculating band counts and means..."))
  prf_data_agg <- cell_bands |>
    collapse::fgroup_by(image_label, year, start, end) |>
    collapse::fsummarise(across(sel_cell_params,
                                list(N = collapse::fnobs,
                                     mean = collapse::fmean)))

  if (!is.null(quant_probs) && length(quant_probs) > 0){
    cli::cli_inform(c("i"= "Calculating band quantiles..."))
    prf_data_quant <- cell_bands |>
      collapse::fgroup_by(image_label, year, start, end) |>
      collapse::BY(collapse::.quantile,
                   probs = quant_probs,
                   expand.wide = TRUE)

    old_col_names <- unlist(lapply(sel_cell_params, function(param) {
      paste0(param, ".V", seq(length(quant_probs)))
    }
    ))
    new_col_names <- unlist(lapply(sel_cell_params, function(param) {
      paste0(param, "_q", sprintf("%02d", round(quant_probs*100)))
    }
    ))
    data.table::setnames(prf_data_quant, old = old_col_names, new = new_col_names)

    prf_data_agg <- prf_data_agg[prf_data_quant,
                                 on = c("image_label", "year", "start", "end")]
    rm(prf_data_quant)
  }

  cli::cli_inform(c("i"= "Adding EW indicator..."))
  # add a ew_band column to indicate if band is in EW or LW
  prf_data_agg <- ring_widths[prf_data_agg, on = c("image_label", "year")]
  prf_data_agg[, ew_band := data.table::fifelse(end <= eww, TRUE, FALSE)]
  prf_data_agg[, end := start + bandwidth] # restore original end values of last bands (without tolerance)
  data.table::setcolorder(prf_data_agg, "ew_band", after="eww")

  cli::cli_inform(c("v"= "All done!"))
  new_QWAprofile(tibble::as_tibble(prf_data_agg), profile_type = "band")
}
