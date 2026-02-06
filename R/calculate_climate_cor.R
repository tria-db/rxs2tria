#' Compute cell-wise correlations between growth chronologies and climate
#'
#' This function computes cell-wise Pearson correlations between per-group
#' growth chronologies (sector-wise or band-wise) and seasonal climate variables.
#' The climate data is provided as monthly `SpatRaster` layers, and correlations
#' are computed for each season and group.
#'
#' @param clim_rasts A named list of `SpatRaster` objects with climate variables,
#'   e.g., \code{list(temp = tmp_rast, pre = pre_rast)}. Layers should be
#'   monthly and in chronological order from `start_year`.
#' @param chronos A tibble containing growth chronologies. Must include columns:
#'   the grouping column (`group_col`), `param`, `stat`, `year`, and `value`.
#' @param group_col Character. Column in `chronos` defining groups (e.g., sector_n, ew_lw).
#' @param groups Optional vector of groups to process. If NULL, all groups in `chronos` are used.
#' @param params Optional vector of growth parameters to include. If NULL, all available parameters are used.
#' @param stat Statistic to filter chronologies by (default = "mean").
#' @param seasons Named list of seasons with corresponding months, e.g., \code{list(JJA = 6:8, JJ = 6:7)}.
#' @param clim_var Character. Climate variable to use from `clim_rasts` (default = "temp").
#' @param start_year Integer. First year to include (default = 1901).
#' @param end_year Integer. Last year to include (default = 2023).
#' @param min_years Minimum number of overlapping years required to compute a correlation (default = 20).
#'
#' @return A named list of `SpatRaster` objects. Each raster layer contains
#'   cell-wise correlations for one combination of group × parameter × stat ×
#'   climate variable × season. Layer names have the format:
#'   \code{group.param.stat.climvar.season}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters chronologies by group and statistic.
#'   \item Converts the monthly climate raster into a long-format tibble with columns
#'         \code{lon}, \code{lat}, \code{year}, \code{month}, and \code{clim_val}.
#'   \item Loops over groups and parameters.
#'   \item For each season, computes seasonal means of climate variables.
#'   \item Joins seasonal climate values with growth chronologies by year.
#'   \item Computes cell-wise correlations, setting values to NA if the number
#'         of overlapping years is less than `min_years`.
#'   \item Converts correlation values back to `SpatRaster` layers with descriptive names.
#' }
#'
#' @examples
#' \dontrun{
#' # Example: compute correlations for sectors 1 and 5
#' r_all <- compute_corr_rasters(
#'   clim_rasts,
#'   summary_sector$chronology,
#'   group_col = "sector_n",
#'   groups = c(1, 5),
#'   params = c("cwttan"),
#'   stat = "q75",
#'   seasons = list(JJA = 6:8)
#' )
#'
#' # Convert raster to ggplot-friendly dataframe
#' corr_df <- lapply(names(r_all), raster_to_ggplot) %>%
#'   bind_rows()
#' }
#'
#' @export
compute_corr_rasters <- function(
    clim_rasts,
    chronos,
    group_col = "sector_n",
    groups = NULL,
    params = NULL,
    stat = "mean",
    seasons = list(JJA = 6:8, JJ = 6:7),
    clim_var = "temp",
    start_year = 1901,
    end_year = 2023,
    min_years = 20
) {

  results <- list()

  # Determine groups
  if (!is.null(groups)) {
    chronos <- chronos %>%
      filter(.data[[group_col]] %in% groups)
  }


  # Precompute year/month lookup (CRU-style)
  years  <- rep(start_year:end_year, each = 12)
  months <- rep(1:12, times = length(start_year:end_year))

  # Convert climate raster once
  clim_df <- as.data.frame(clim_rasts[[clim_var]], xy = TRUE) %>%
    pivot_longer(
      cols = starts_with("tmp_"),
      names_to = "layer",
      values_to = "clim_val"
    ) %>%
    mutate(
      layer_index = as.integer(gsub("tmp_", "", layer)),
      year  = years[layer_index],
      month = months[layer_index]
    ) %>%
    select(lon = x, lat = y, year, month, clim_val)

  # Loop over groups (sector / EW-LW / etc.)
  for (grp in groups) {

    grp_data <- chronos %>%
      filter(.data[[group_col]] == grp, stat == stat)

    param_list <- if (is.null(params)) unique(grp_data$param) else params

    for (param in param_list) {

      growth_df <- grp_data %>%
        filter(param == !!param) %>%
        filter(year >= start_year, year <= end_year) %>%
        group_by(year) %>%
        summarize(
          growth = if_else(param[1] == "mrw", max(value, na.rm = TRUE), first(value)),
          .groups = "drop"
        ) %>%
        arrange(year)



      if (nrow(growth_df) < min_years) next

      # Loop over seasons
      for (season_name in names(seasons)) {

        months_sel <- seasons[[season_name]]

        tmp_season <- clim_df %>%
          filter(month %in% months_sel, year %in% growth_df$year) %>%
          group_by(lon, lat, year) %>%
          summarize(
            season_val = mean(clim_val, na.rm = TRUE),
            .groups = "drop"
          )

        corr_df <- tmp_season %>%
          inner_join(growth_df, by = "year") %>%
          group_by(lon, lat) %>%
          summarize(
            n = sum(complete.cases(season_val, growth)),
            r = ifelse(
              n >= min_years,
              cor(season_val, growth, use = "pairwise.complete.obs"),
              NA_real_
            ),
            .groups = "drop"
          )

        r_rast <- terra::rast(corr_df[, c("lon", "lat", "r")])

        nm <- paste(grp, param, stat, clim_var, season_name, sep = ".")
        names(r_rast) <- nm
        results[[nm]] <- r_rast
      }
    }
  }

  results
}


#' Convert a SpatRaster to a tidy data frame for ggplot
#'
#' This function converts a single-layer or multi-layer `SpatRaster` object into
#' a tidy `data.frame` suitable for visualization with `ggplot2`. Each cell's
#' longitude, latitude, and value are stored in columns `x`, `y`, and `r`.
#'
#' @param rast_obj A `SpatRaster` object (from the `terra` package). Can be
#'   single-layer or multi-layer. Only the first layer will be returned if multiple layers exist.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{x}{Longitude or x-coordinate of raster cell.}
#'     \item{y}{Latitude or y-coordinate of raster cell.}
#'     \item{r}{Raster value of the cell.}
#'   }
#'
#' @details
#' This function is useful for visualizing raster data with `ggplot2`. Once
#' converted, the resulting data frame can be used with `geom_raster()` or
#' `geom_tile()`. For multi-layer rasters, consider iterating over layer names
#' or combining layers with `pivot_longer()` for faceted plotting.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(ggplot2)
#'
#' # Example SpatRaster
#' r <- rast(nrows=10, ncols=10)
#' values(r) <- runif(ncell(r))
#'
#' df <- raster_to_ggplot(r)
#' ggplot(df) +
#'   geom_raster(aes(x = x, y = y, fill = r)) +
#'   coord_fixed()
#' }
#'
#' @export
raster_to_ggplot <- function(rast_obj) {
  df <- as.data.frame(rast_obj, xy = TRUE)
  names(df)[3] <- "r"
  df
}








