#' Create a Tucson (.rwl) file from ROXAS ring-width or profile data
#'
#' This function creates a dendrochronological Tucson-format (\code{.rwl})
#' file from ROXAS output data. Depending on the selected parameter, the
#' function either uses ring-level measurements (e.g. mean ring width, \code{mrw})
#' or profile-level measurements aggregated at a given sector.
#'
#' Duplicate rings and user-defined exclusions are automatically removed
#' before constructing the final time series.
#'
#' @details
#' The function operates in two modes:
#'
#' \itemize{
#'   \item{\strong{Ring-level mode} (\code{PAR == "mrw"}):}
#'   Ring-width values are taken from \code{df_rings}. Rings flagged as
#'   duplicated (\code{exclude_issues == TRUE}) are removed. If multiple
#'   measurements exist for the same year and image, values are averaged.
#'   Ring widths are converted from micrometers to millimeters before export.
#'
#'   \item{\strong{Profile-level mode} (\code{PAR != "mrw"}):}
#'   Measurements are taken from \code{prf_data} for the selected sector.
#'   Years flagged either as duplicated rings or excluded via
#'   \code{exclude_scope} are removed prior to aggregation.
#' }
#'
#' Image identifiers are simplified to tree-level series IDs before exporting,
#' and the resulting data are written to a Tucson \code{.rwl} file using
#' \code{dplR::write.tucson()}.
#'
#' @param prf_data A data frame containing ROXAS profile-level measurements
#'   (e.g. sector-based anatomical parameters).
#' @param df_rings A data frame containing ROXAS ring-level measurements and
#'   logical flag columns (default: \code{QWA_data$rings}).
#' @param PAR Character string specifying the parameter to export.
#'   Use \code{"mrw"} for mean ring width, or any valid column name present
#'   in \code{prf_data} for profile-level parameters.
#' @param SECTOR Integer specifying which sector to use when exporting
#'   profile-level parameters (default: 5).
#' @param path_out Character string specifying the output directory where
#'   the \code{.rwl} file and mapping information will be written.
#'
#' @return
#' The function is called for its side effects. It writes a Tucson-format
#' \code{.rwl} file to disk and returns the final data frame invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an RWL file from mean ring width
#' create_rwl(prf_data = prf_data,
#'            df_rings = QWA_data$rings,
#'            PAR = "mrw",
#'            path_out = tempdir())
#'
#' # Create an RWL file from a profile-level parameter
#' create_rwl(prf_data = prf_data,
#'            PAR = "cwtrad_mean",
#'            df_rings = QWA_data$rings, #df_rings = read.csv(paste0(path_out, "/", "20251230_TRIA_YAM_AI_1880_rings_edited.csv"))
#'            SECTOR = 5,
#'            path_out = tempdir())
#' }
#'
#' @export
create_rwl <- function(prf_data, df_rings, PAR = "mrw", SECTOR = NULL, path_out, remove_excluded = TRUE) {

  # ----------------------------
  # 1. Safety checks
  # ----------------------------
  stopifnot(is.character(PAR), length(PAR) == 1)

  if (!PAR %in% c(names(df_rings), names(prf_data))) {
    stop("Parameter ", PAR, " not found in rings or profiles data.")
  }

  # ----------------------------
  # 2. Helper: standardize series ID
  # ----------------------------
  make_series_id <- function(x) {
    paste0(
      stringr::str_split(x, "_") |> sapply(`[`, 1),
      stringr::str_split(x, "_") |> sapply(`[`, 3)
    )
  }

  # ----------------------------
  # 3. RING-LEVEL PARAMETERS
  # ----------------------------
  if (PAR %in% names(df_rings)) {

    rings_df <- df_rings |>
      tibble::as_tibble()

    if (remove_excluded && "exclude_issues" %in% names(rings_df)) {
      rings_df <- rings_df |> dplyr::filter(!exclude_issues)
    }

    rwl <- rings_df |>
      dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
      dplyr::filter(!is.na(.data[[PAR]])) |>
      dplyr::mutate(
        series = make_series_id(image_label),
        value  = .data[[PAR]] / 1000
      ) |>
      dplyr::group_by(year, series) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = series, values_from = value) |>
      dplyr::arrange(desc(year)) |>
      tibble::column_to_rownames("year")

  } else {

    # ----------------------------
    # 4. PROFILE-LEVEL PARAMETERS
    # ----------------------------
    if (is.null(SECTOR)) {
      stop("SECTOR must be provided for profile-level parameters.")
    }

    prf_df <- prf_data |>
      tibble::as_tibble() |>
      dplyr::filter(sector_n == SECTOR)

    # Ensure 'exclude_scope' exists and is character
    if (!"exclude_scope" %in% names(df_rings)) {
      df_rings$exclude_scope <- NA_character_
    }

    if (remove_excluded && all(c("image_label", "year") %in% names(df_rings))) {

      # to_remove <- df_rings  |>   # %>% dplyr::filter(image_label == "YAM_LASI_2718_1") & year == 1550
      #   dplyr::filter(
      #     (exclude_issues %||% FALSE) |
      #       !is.na(exclude_scope) |
      #       x_dating
      #   ) |>
      #   dplyr::filter(!missing_ring & incomplete_ring) |>   # include small rings (missing rings with cno > 0) in the RWL file
      #   dplyr::select(image_label, year) |>
      #   dplyr::distinct()

      to_remove <- df_rings  |>
        mutate(image_label = stringr::str_replace(image_label, "(?<=\\d)_(?:\\d+_?)+$", "")) |> dplyr::filter(!exclude_dupl) %>%
 #       dplyr::filter(image_label == "YAM_LASI_T1.01") %>%
        dplyr::filter(
          (!missing_ring & incomplete_ring) |                      # include small rings (missing rings with cno > 0) in the RWL file
            (x_dating & exclude_scope %in% c("ALL", "EW", "LW")) | # exclude dating issues
            (decay    & exclude_scope %in% c("ALL", "EW", "LW"))   # exclude decay issues
        ) |>
        select(image_label, year) |>
        dplyr::distinct()

      prf_df <- prf_df |>
        mutate(image_label = stringr::str_replace(image_label, "(?<=\\d)_(?:\\d+_?)+$", "")) |>
        # dplyr::mutate(
        #   image_label = stringr::str_replace(image_label, "_[0-9]+$", "")
        # ) |>
        dplyr::anti_join(to_remove, by = c("image_label", "year"))  # %>% dplyr::filter(image_label == "YAM_LASI_T1.01")
    }

    # rwl <- prf_df |>
    #   dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
    #   dplyr::filter(!is.na(.data[[PAR]])) |>
    #   dplyr::mutate(series = make_series_id(image_label)) |>
    #   dplyr::group_by(year, series) |>  # dplyr::filter(image_label == "YAM_LASI_100" & year == 1738) %>%       # df_rings %>%  dplyr::filter(cno == 0)
    #   dplyr::summarise(value = mean(.data[[PAR]], na.rm = TRUE), .groups = "drop") |>
    #   tidyr::pivot_wider(names_from = series, values_from = value) |>
    #   dplyr::arrange(desc(year)) |>
    #   tibble::column_to_rownames("year")

    ## ---- 1. Measured rings (from prf_df) ----
    measured_rings <- prf_df |>
      dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
      dplyr::filter(!is.na(.data[[PAR]])) |>
      dplyr::mutate(
        series = make_series_id(image_label),
        value  = .data[[PAR]]
      ) |>
      dplyr::group_by(year, series) |>
      dplyr::summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )

    ## ---- 2. Missing rings (explicit zeros) ----
    missing_rings <- df_rings |>
      dplyr::filter(missing_ring, !incomplete_ring) |> # include missing rings in the RWL file
      dplyr::mutate(
        series = make_series_id(image_label),
        value  = 0.01                                  # missing rings are assigned to value 1
      ) |>
      dplyr::select(image_label, year, series, value) |>
      mutate(image_label = stringr::str_replace(image_label, "(?<=\\d)_(?:\\d+_?)+$", "")) |>   # KEEP image_label for anti_join
      dplyr::anti_join(to_remove, by = c("image_label", "year")) |>
      dplyr::select(-image_label)  # %>%  dplyr::filter(series == "YAM91")

    ## ---- 3. Combine measured + missing rings ----
    rwl_long <- dplyr::bind_rows(
      measured_rings,
      missing_rings
    ) |>
      # enforce missing rings (0) if any duplicates exist
      dplyr::group_by(year, series) |>
      dplyr::summarise(
        value = min(value),
        .groups = "drop"
      )

    ## ---- 4. Pivot to RWL matrix ----
    rwl <- rwl_long |>
      tidyr::pivot_wider(
        names_from  = series,
        values_from = value
      ) |>
      dplyr::arrange(desc(year)) |>
      tibble::column_to_rownames("year")

  }

  # ----------------------------
  # 5. Write RWL
  # ----------------------------
  out_file <- file.path(path_out, paste0(PAR, "_roxas.rwl"))

  dplR::write.tucson(
    rwl,
    fname = out_file,
    prec = 0.01,
    header = NULL,
    append = FALSE,
    long.names = TRUE,
    mapping.fname = file.path(path_out, "ROXAS.RWL_new.IDs")
  )

  invisible(rwl)
}

