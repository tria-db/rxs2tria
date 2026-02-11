
#' Calculate sector-wise chronologies from quantitative wood anatomy data
#'
#' This function summarizes sector-wise cell-level parameters (means and
#' optionally quantiles) per tree and per year, producing long-format timeseries,
#' Tukey biweight mean chronologies, and inter-tree correlations.
#'
#' @param filtered_sector_data Data frame or tibble containing cell-level sector
#'   summaries with columns including `tree_id`, `image_label`, `year`, `sector_n`,
#'   and selected cell parameters (e.g., "la_mean", "cwttan_q25").
#' @param sel_params Optional character vector of parameter names to include.
#'   If NULL, automatically detects all `_mean` and `_qXX` columns.
#' @param quant_probs Optional numeric vector of quantile probabilities (e.g., c(0.25,0.5,0.75)).
#'   Used to identify corresponding `_qXX` columns.
#' @param SECTOR Optional integer vector of sector numbers to include. If NULL,
#'   all sectors are used.
#' @param min_year_cor Minimum year to include when computing inter-tree
#'   correlations (default = 1900).
#' @param write_csv Logical; if TRUE, writes CSVs of the tree-year table, chronology,
#'   and correlation results.
#' @param fname_out Base name for CSV output files (default = "chronology").
#'
#' @return A named list containing:
#'   \describe{
#'     \item{timeseries}{long-format per-tree × year × sector × param × stat values.}
#'     \item{chronology}{per-year Tukey biweight mean per sector, param, and stat.}
#'     \item{correlation}{mean inter-tree correlation per sector, param, and stat.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects mean and quantile parameters automatically if not provided.
#'   \item Filters data by sector if requested.
#'   \item Aggregates per-tree × year × sector means for each parameter/stat.
#'   \item Creates `tree_year` table in long format.
#'   \item Computes per-year Tukey biweight mean chronologies (`dplR::tbrm`).
#'   \item Computes mean inter-tree correlations per sector, parameter, and statistic.
#' }
#'
#' @export
summary_sector_chronologies <- function(
    filtered_sector_data,
    sel_params = NULL,
    quant_probs = NULL,
    SECTOR = NULL,
    min_year_cor = 1900,
    write_csv = FALSE,
    fname_out = "chronology"
) {

  # --- 0️⃣ Detect parameters and stats automatically -------------------------
  mean_cols <- names(filtered_sector_data)[stringr::str_detect(names(filtered_sector_data), "_mean$")]
  detected_params <- stringr::str_remove(mean_cols, "_mean$")

  quant_cols <- names(filtered_sector_data)[stringr::str_detect(names(filtered_sector_data), "_q\\d+$")]
  detected_probs <- quant_cols |>
    stringr::str_extract("(?<=_q)\\d+$") |>
    as.numeric() |>
    unique() / 100

  if (is.null(sel_params)) sel_params <- detected_params
  if (is.null(quant_probs))     quant_probs     <- detected_probs

  stats <- c("mean", paste0("q", sprintf("%02d", quant_probs * 100)))

  # --- 1️⃣ Filter sectors ----------------------------------------------------
  dat <- if (is.null(SECTOR)) {
    filtered_sector_data
  } else {
    dplyr::filter(filtered_sector_data, sector_n %in% SECTOR)
  }

  # Define the measurement columns
  measure_cols <- setdiff(names(dat), c("tree_id", "image_label", "year", "sector_n"))

  tree_year <- dat %>%
    tidyr::pivot_longer(
      cols = all_of(measure_cols),
      names_to = "param_stat",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      # Handle mrw separately
      param = ifelse(param_stat == "mrw", "mrw", sub("_.*$", "", param_stat)),
      stat  = ifelse(param_stat == "mrw", "mean", sub(".*_", "", param_stat))
    ) %>%
    dplyr::select(tree_id, image_label, year, sector_n, param, stat, value) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::arrange(-year, tree_id, image_label, sector_n, stat, param)


  # --- 4️⃣ Compute yearly chronologies (mean over trees) ----------------------
  chronology <- tree_year |>
    dplyr::group_by(sector_n, param, stat, year) |>
    dplyr::summarise(
      value = dplR::tbrm(value),
      .groups = "drop"
    )

  # --- 5️⃣ Compute inter-tree correlations -----------------------------------
  # --- 5️⃣ Compute inter-tree correlations + diagnostics ------------------------
  diagnostics <- tree_year %>%
    dplyr::filter(year >= min_year_cor) %>%
    tidyr::pivot_wider(
      id_cols   = c("year", "sector_n", "param", "stat"),
      names_from  = "tree_id",
      values_from = "value"
    ) %>%
    dplyr::group_by(sector_n, param, stat) %>%
    dplyr::group_modify(~ {

      tree_cols <- setdiff(names(.x), c("year", "sector_n", "param", "stat"))

      # keep trees with signal
      tree_cols <- tree_cols[sapply(.x[, tree_cols, drop = FALSE], function(x) {
        length(unique(na.omit(x))) >= 2
      })]

      n <- length(tree_cols)

      if (n < 2) {
        return(tibble::tibble(
          Rbar    = NA_real_,
          EPS     = NA_real_,
          n_trees = n
        ))
      }

      cor_mat <- suppressWarnings(
        cor(.x[, tree_cols, drop = FALSE], use = "pairwise.complete.obs")
      )

      rbar <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
      eps  <- (n * rbar) / (n * rbar + (1 - rbar))

      tibble::tibble(
        Rbar    = rbar,
        EPS     = eps,
        n_trees = n
      )
    }) %>%
    dplyr::ungroup()


  # --- 6️⃣ Optional CSV output ------------------------------------------------
  if (write_csv) {
    chronology |>
      dplyr::group_by(sector_n, param, stat) |>
      dplyr::group_walk(~{
        readr::write_csv(
          .x,
          paste0(fname_out, "_", unique(.x$param), "_", unique(.x$stat),
                 "_sector", unique(.x$sector_n), ".csv")
        )
      })
  }

  # --- 7️⃣ Return tidy results ------------------------------------------------
  list(
    chronology  = chronology,
    timeseries   = tree_year,
    diagnostics = diagnostics
  )
}








#' Calculate band-wise chronologies from quantitative wood anatomy data
#'
#' This function summarizes band-wise cell-level parameters (means, min, max,
#' delta, Tukey biweight mean) per tree and per year. Bands are assumed to be
#' already defined (with `start` and `end`) and optionally classified as
#' earlywood or latewood (`ew_band`).
#'
#' @param band_data A data frame or tibble containing band-level cell parameters,
#'   including `tree_id`, `image_label`, `year`, `start`, `end`, and selected
#'   cell parameters (e.g., "la_mean", "cwttan_q25").
#' @param sel_params Optional character vector of column names of the parameters
#'   to summarize. If NULL, all columns ending in `_mean` or `_qXX` are used.
#' @param min_year_cor Minimum year to include when computing inter-tree
#'   correlations (default = 1900).
#' @param write_csv Logical; if TRUE, writes summary, tree-year, chronology,
#'   and correlation tables to CSV files.
#' @param fname_out Base name for CSV output files (default = "band_chronology").
#'
#' @return A named list containing:
#'   \describe{
#'     \item{summary_data}{per-tree min, max, delta, and mean per band and param.}
#'     \item{timeseries}{long-format per-tree × year × param × stat values.}
#'     \item{chronology}{per-year Tukey biweight mean per param, stat, and EW/LW type.}
#'     \item{correlation}{mean inter-tree correlation per param, stat, and EW/LW type.}
#'   }
#'
#' @details
#' The function computes:
#' \enumerate{
#'   \item Full-ring summaries (min, max, delta, mean) per tree.
#'   \item Earlywood/Latewood summaries if `ew_band` is available.
#'   \item Long-format `tree_year` table for downstream analysis.
#'   \item Tukey biweight mean (`dplR::tbrm`) per year for chronology.
#'   \item Mean inter-tree correlation for each combination of parameter,
#'         statistic, and EW/LW type.
#' }
#'
#' @export
summary_band_chronologies <- function(
    band_data,
    sel_params = NULL,
    min_year_cor = 1900,
    write_csv = FALSE,
    fname_out = "band_chronology"
) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(dplR) # for tbrm()

  # --- 0️⃣ Detect parameters automatically if not provided ---
  if(is.null(sel_params)){
    mean_cols <- names(band_data)[str_detect(names(band_data), "_mean$")]
    quant_cols <- names(band_data)[str_detect(names(band_data), "_q\\d+$")]
    sel_params <- c(mean_cols, quant_cols)
  }

  # --- 1️⃣ Create a unique band identifier ---
  band_data <- band_data %>%
    dplyr::mutate(band_id = paste0(start, "-", end))

  # --- 2️⃣ Full-ring summaries ---
  full_ring <- band_data %>%
    tidyr::pivot_longer(
      cols = all_of(sel_params),
      names_to = "param",
      values_to = "value"
    ) %>%
    dplyr::group_by(tree_id, image_label, year, param) %>%
    dplyr::summarise(
      min_value  = if(all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      max_value  = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      delta      = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      mean_value = if(all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(ew_lw = "all")

  # --- 3️⃣ EW/LW summaries ---
  ew_lw_summary <- band_data %>%
    tidyr::pivot_longer(
      cols = all_of(sel_params),
      names_to = "param",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(ew_band)) %>%
    dplyr::group_by(tree_id, image_label, year, ew_band, param) %>%
    dplyr::summarise(
      min_value  = if(all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      max_value  = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      delta      = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      mean_value = if(all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(ew_lw = if_else(ew_band, "EW", "LW")) %>%
    dplyr::select(-ew_band)

  summary_data <- bind_rows(full_ring, ew_lw_summary)

  # --- 4️⃣ Prepare tree_year (per-tree × year × param/stat) ---
  tree_year <- summary_data %>%
    tidyr::pivot_longer(
      cols = c(min_value, max_value, delta, mean_value),
      names_to = "stat",
      values_to = "value"
    ) %>%
    dplyr::select(tree_id, image_label, year, ew_lw, param, stat, value)

  # --- 5️⃣ Compute chronology (Tukey biweight mean) ---
  chronology <- tree_year %>%
    dplyr::group_by(ew_lw, param, year, stat) %>%
    dplyr::summarise(
      value = dplR::tbrm(value),
      .groups = "drop"
    )

  # --- 6️⃣ Compute Rbar, EPS, and n_trees per param/stat/ew_lw ---
  # --- 6️⃣ Inter-tree correlation / diagnostics ---
  # --- 6️⃣ Inter-tree correlation + EPS diagnostics ---
  diagnostics <- purrr::map_dfr(c("all", "EW", "LW"), function(ew_type) {

    # Filter by EW/LW type if needed
    df <- tree_year
    if (ew_type != "all") df <- df %>% dplyr::filter(ew_lw == ew_type)

    df %>%
      dplyr::group_by(param, stat) %>%
      dplyr::group_modify(~{

        tmp <- .x %>%
          dplyr::filter(year >= min_year_cor) %>%
          dplyr::select(tree_id, year, value) %>%  # keep only numeric columns
          tidyr::pivot_wider(
            names_from = tree_id,
            values_from = value,
            values_fn = mean
          )

        tree_cols <- setdiff(names(tmp), "year")
        n <- length(tree_cols)

        if(n < 2){
          return(tibble::tibble(Rbar = NA_real_, EPS = NA_real_, n_trees = n))
        }

        # safe correlation
        cor_mat <- suppressWarnings(
          cor(tmp[, tree_cols], use = "pairwise.complete.obs")
        )

        rbar <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
        eps  <- (n * rbar) / (n * rbar + (1 - rbar))

        tibble::tibble(Rbar = rbar, EPS = eps, n_trees = n)
      }) %>%
      dplyr::mutate(ew_lw = ew_type)

  }) %>%
    dplyr::select(ew_lw, param, stat, Rbar, EPS, n_trees)


  # --- 7️⃣ Optional CSV export ---
  if(write_csv){
    params <- unique(summary_data$param)
    for(p in params){
      readr::write_csv(summary_data %>% dplyr::filter(param == p),
                       paste0(fname_out, "_", p, "_summary.csv"))
    }
    readr::write_csv(tree_year, paste0(fname_out, "_tree_year.csv"))
    readr::write_csv(chronology, paste0(fname_out, "_chronology.csv"))
    readr::write_csv(diagnostics, paste0(fname_out, "_diagnostics.csv"))
  }

  # --- 8️⃣ Return tidy results ---
  list(
    summary_data = summary_data,   # per-tree min/max/delta/mean
    timeseries    = tree_year,     # per-tree × year × param/stat
    chronology   = chronology,     # per-year Tukey biweight mean
    diagnostics  = diagnostics     # Rbar, EPS, n_trees
  )
}



