#' Calculate sector-wise cell parameter profiles
#'
#' This function calculates sector-wise profiles of selected cell parameters
#' (means and quantiles) from the cells data.
#'
#' @param df_cells dataframe containing the cells data (e.g. `QWA_data$cells`)
#' @param n_sectors number of radial sectors to divide the ring into (e.g 5)
#' @param sel_cell_params character vector with the names of the cell parameters
#' to include in the profiles (e.g. c("la", "cwttan"))
#' @param quant_probs numeric vector with the quantile probabilities to
#' calculate (e.g. c(0.25, 0.5, 0.75)). If NULL or empty, no quantiles are
#' calculated (i.e. only the mean).
#' @return a dataframe with the sector-wise profiles of the selected cell
#' parameters
#' @export
#'
calculate_profiles <- function(df_cells, n_sectors, sel_cell_params, quant_probs){
  prf_data <- df_cells |>
    # create sector number based on rraddistr (relative position within ring)
    dplyr::mutate(
      sector_n = as.numeric(cut(rraddistr,
                                b = seq(from=0, to=100, by = 100/n_sectors),
                                labels = 1:n_sectors,
                                include.lowest = T))) %>%
    # allow for rounding errors for data with rraddistr just above 100, otherwise leave NA
    dplyr::mutate(sector_n = dplyr::if_else(rraddistr > 100 & rraddistr <= 101, n_sectors, sector_n)) |>
    dplyr::filter(!is.na(sector_n)) |> # only use cells with valid sector as basis for calculation
    dplyr::select(image_label, year, sector_n, dplyr::all_of(sel_cell_params))

  # use the collapse package for fast groupwise aggregations
  prf_data_agg <- prf_data |>
    collapse::fgroup_by(image_label, year, sector_n) |>
    collapse::fmean() |>
    dplyr::rename_with(
      \(x) paste0(x, "_mean"),
      dplyr::all_of(sel_cell_params)
    )

  if (!is.null(quant_probs) && length(quant_probs) > 0){
    prf_data_quant <- prf_data |>
      collapse::fgroup_by(image_label, year, sector_n) |>
      collapse::BY(collapse::.quantile,
                   probs = quant_probs,
                   expand.wide = TRUE) |>
      dplyr::rename_with(
        \(x) {
          for (i in seq_along(quant_probs)){
            x <- stringr::str_replace_all(x,
                                          paste0(".V", i),
                                          paste0("_q", quant_probs[i]*100))
          }
          x
        }
      )
    prf_data_agg <- prf_data_agg |>
      dplyr::full_join(prf_data_quant,
                       by = c("image_label", "year", "sector_n"))
  }

  prf_data_agg
}

#' Helper function to create the moving band definitions for given a mrw,
#' andwidth and stepsize
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


#' Calculate band-wise cell parameter profiles
#'
#' This function calculates band-wise profiles of selected cell parameters
#' (means and quantiles) from the cells data. A band is a moving window of
#' given bandwidth that moves from the start to the end of the ring at the given
#' stepsize.
#'
#' @param QWA_data list containing the rings and cells dataframes
#' @param bandwidth the width of the band in microns
#' @param stepsize the step size with which the band is shifted, in microns
#' @param sel_cell_params character vector with the names of the cell parameters
#' to include in the profiles (e.g. c("la", "cwttan"))
#' @param quant_probs numeric vector with the quantile probabilities to
#' calculate (e.g. c(0.25, 0.5, 0.75)). If NULL or empty, no quantiles are
#' calculated (i.e. only the mean).
#' @return a dataframe with the sector-wise profiles of the selected cell
#' parameters
#' @export
calculate_band_profiles <- function(QWA_data,
                                    bandwidth, stepsize,
                                    sel_cell_params,
                                    quant_probs = NULL,
                                    band_rebound = TRUE) {
  # we use data.table for fast operations on the large cells table
  # the standardized radial distance raddistr.st is used to to position cells
  # within bands (since the ring is not the same width everywhere, so the bands
  # should reflect such fluctuations as well). Note that raddistr.st is <= mrw
  # (except for rounding errors), and raddistr.st is na if we don't have an mrw
  # (e.g. for missing / incomplete rings)
  cells_dt <- data.table::as.data.table(QWA_data$cells)
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
  prf_data_agg[, ew_band := ifelse(end <= eww, TRUE, FALSE)]
  prf_data_agg[, end := start + bandwidth] # restore original end values of last bands (without tolerance)
  data.table::setcolorder(prf_data_agg, "ew_band", after="eww")

  cli::cli_inform(c("v"= "All done!"))
  tibble::as_tibble(prf_data_agg)
}
