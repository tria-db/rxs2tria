
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

  if (is.null(sel_cell_params)) sel_cell_params <- detected_params
  if (is.null(quant_probs))     quant_probs     <- detected_probs

  stats <- c("mean", paste0("q", sprintf("%02d", quant_probs * 100)))

  # --- 1️⃣ Filter sectors ----------------------------------------------------
  dat <- if (is.null(SECTOR)) {
    filtered_sector_data
  } else {
    dplyr::filter(filtered_sector_data, sector_n %in% SECTOR)
  }

  # --- 2️⃣ Prepare param × stat grid -----------------------------------------
  grid <- tidyr::crossing(
    param = sel_cell_params,
    stat  = stats
  ) |>
    dplyr::mutate(
      col = ifelse(stat == "mean",
                   paste0(param, "_mean"),
                   paste0(param, "_", stat))
    ) |>
    dplyr::filter(col %in% names(dat))

  # --- 3️⃣ Compute tree × year × sector means --------------------------------
  tree_year <- purrr::map_dfr(seq_len(nrow(grid)), function(i) {
    param <- grid$param[i]
    stat  <- grid$stat[i]
    col   <- grid$col[i]

    # Aggregate mean per tree_id × year × sector_n
    agg <- dat |>
      dplyr::group_by(tree_id, image_label, year, sector_n) |>
      dplyr::summarise(value = mean(.data[[col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(param = param, stat = stat)

    agg
  }) %>%
    dplyr::select(tree_id, image_label, year, sector_n, param, stat, value)

  # --- 4️⃣ Compute yearly chronologies (mean over trees) ----------------------
  chronology <- tree_year |>
    dplyr::group_by(sector_n, param, stat, year) |>
    summarise(
      value = dplR::tbrm(value),
      .groups = "drop"
    )

  # --- 5️⃣ Compute inter-tree correlations -----------------------------------
  correlation <- tree_year %>%
    dplyr::filter(year >= min_year_cor) %>%
    tidyr::pivot_wider(
      id_cols = c("year", "sector_n", "param", "stat"),
      names_from = "tree_id",
      values_from = "value"
    ) %>%
    dplyr::group_by(sector_n, param, stat) %>%
    dplyr::group_modify(~{
      # Keep only tree columns
      tree_cols <- setdiff(names(.x), c("year", "sector_n", "param", "stat"))

      # Keep only columns with ≥ 2 unique, non-NA values
      tree_cols <- tree_cols[sapply(.x[, tree_cols, drop = FALSE], function(x) {
        length(unique(na.omit(x))) >= 2
      })]

      if(length(tree_cols) < 2){
        return(tibble::tibble(mean_cor = NA_real_))
      }

      # Compute correlation safely
      mat <- suppressWarnings(
        cor(.x[, tree_cols, drop = FALSE], use = "pairwise.complete.obs")
      )

      tibble::tibble(mean_cor = mean(mat[upper.tri(mat)], na.rm = TRUE))
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
    correlation = correlation
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
  library(dplR) # for mean.biweight()

  # --- 0️⃣ Detect parameters automatically if not provided ---
  if(is.null(sel_params)){
    mean_cols <- names(band_data)[str_detect(names(band_data), "_mean$")]
    quant_cols <- names(band_data)[str_detect(names(band_data), "_q\\d+$")]
    sel_params <- c(mean_cols, quant_cols)
  }

  # --- 1️⃣ Create a unique band identifier ---
  band_data <- band_data %>%
    mutate(band_id = paste0(start, "-", end))

  # --- 2️⃣ Full-ring summaries ---
  full_ring <- band_data %>%
    pivot_longer(
      cols = all_of(sel_params),
      names_to = "param",
      values_to = "value"
    ) %>%
    group_by(tree_id, image_label, year, param) %>%
    summarise(
      min_value  = if(all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      max_value  = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      delta      = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      mean_value = if(all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ew_lw = "all")

  # --- 3️⃣ EW/LW summaries ---
  ew_lw_summary <- band_data %>%
    pivot_longer(
      cols = all_of(sel_params),
      names_to = "param",
      values_to = "value"
    ) %>%
    filter(!is.na(ew_band)) %>%
    group_by(tree_id, image_label, year, ew_band, param) %>%
    summarise(
      min_value  = if(all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      max_value  = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      delta      = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      mean_value = if(all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ew_lw = if_else(ew_band, "EW", "LW")) %>%
    select(-ew_band)

  summary_data <- bind_rows(full_ring, ew_lw_summary)

  # --- 4️⃣ Prepare tree_year (per-tree, per-year values) ---
  tree_year <- summary_data %>%
    pivot_longer(
      cols = c(min_value, max_value, delta, mean_value),
      names_to = "stat",
      values_to = "value"
    ) %>%
    select(tree_id, image_label, year, ew_lw, param, stat, value)

  # --- 5️⃣ Compute chronology (Tukey biweight mean) ---
  chronology <- tree_year %>%
    group_by(ew_lw, param, year, stat) %>%
    summarise(
      value = dplR::tbrm(value),
      .groups = "drop"
    )


  # --- 6️⃣ Inter-tree correlation ---
  correlation <- map_dfr(c("all", "EW", "LW"), function(ew_type) {

    # Filter by EW/LW if needed
    df <- tree_year
    if (ew_type != "all") {
      df <- df %>% filter(ew_lw == ew_type)
    }

    df %>%
      group_by(param, stat) %>%
      group_modify(~{
        # Pivot to wide: columns = tree_id, rows = year
        mat_df <- .x %>%
          filter(year >= min_year_cor) %>%
          group_by(tree_id, year, ew_lw) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = tree_id, values_from = value)

        tree_cols <- setdiff(names(mat_df), "year")
        tree_cols <- tree_cols[sapply(mat_df[, tree_cols, drop = FALSE], function(x) {
          is.numeric(x) && length(unique(na.omit(x))) >= 2
        })]

        if(length(tree_cols) < 2) return(tibble(mean_cor = NA_real_))

        cor_mat <- suppressWarnings(
          cor(mat_df[, tree_cols, drop = FALSE], use = "pairwise.complete.obs")
        )

        tibble(mean_cor = mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE))
      }) %>%
      mutate(ew_lw = ew_type)

  }) %>%
    select(ew_lw, param, stat, mean_cor)


  # --- 7️⃣ Optional CSV export ---
  if(write_csv){
    # Summary
    params <- unique(summary_data$param)
    for(p in params){
      readr::write_csv(summary_data %>% filter(param == p),
                       paste0(fname_out, "_", p, "_summary.csv"))
    }
    # Tree-year
    readr::write_csv(tree_year, paste0(fname_out, "_tree_year.csv"))
    # Chronology
    readr::write_csv(chronology, paste0(fname_out, "_chronology.csv"))
    # Correlation
    readr::write_csv(correlation, paste0(fname_out, "_correlation.csv"))
  }

  # --- 8️⃣ Return tidy results ---
  list(
    summary_data = summary_data,   # per-tree min/max/delta/mean
    timeseries    = tree_year,      # per-tree × year × param
    chronology   = chronology,     # per-year Tukey biweight mean
    correlation  = correlation     # mean inter-tree correlation
  )
}



