#' Update QWA metadata for selected images
#'
#' Re-reads the settings/metadata files for the given images and replaces the
#' corresponding rows in the existing [QWAimages] object. Useful when source
#' files have changed after initial import.
#'
#' @param meta a [QWAimages] object
#' @param imgs_to_update character vector of `image_label` values to update;
#'   must be a subset of `meta$image_label`
#' @param settings_date_orders date order string(s) passed to
#'   [lubridate::parse_date_time()] to parse the `rxs_created_at` field
#' @returns An updated [QWAimages] object
#' @export
update_rxsmeta <- function(meta, imgs_to_update,
                           settings_date_orders) {
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_update, meta$image_label)

  files_to_update <- meta |>
    dplyr::filter(image_label %in% imgs_to_update) |> 
    dplyr::pull(fname_settings)

  roxas_version <- attr(meta, "roxas_version")

  rv_file <- if (roxas_version == "roxas") "ROXAS settings" else "ROXAS AI metadata"
  results <- files_to_update |>
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version),
               .progress = list(name = glue::glue("Reading {rv_file} files..."), clear = TRUE))
  df_settings_new <- purrr::list_rbind(results)

  num_cols <- c("spatial_resolution", "dbl_cwt_threshold", "max_cwtrad_s", "max_cwtrad_l",
    "relwidth_cwt_window", "maxrel_opp_cwt", "max_cwttan_s", "max_cwttan_l")
  int_cols <- c("circ_lower_limit", "circ_upper_limit", "outmost_year", 
    "min_cell_area", "max_cell_area")
  df_settings_new <- df_settings_new |>
      dplyr::mutate(dplyr::across(dplyr::any_of(num_cols), as.numeric),
                    dplyr::across(dplyr::any_of(int_cols), as.integer)
                    #sample_type = NA_character_ # TODO: as input?
                  )
  # convert created at dates to POSIXct
  df_settings_new$rxs_created_at <- lubridate::parse_date_time(
    df_settings_new$rxs_created_at,
    orders = settings_date_orders, # cf. lubridate::parse_date_time
    tz = Sys.timezone())

  # UPDATE THE DATAFRAME WITH THE NEW SETTINGS INFO
  meta <- meta |>
    dplyr::rows_update(df_settings_new, by = "fname_settings")

  meta
}

#' Update a QWAdata object with re-read raw data for selected images
#'
#' Re-reads the raw data files for `imgs_to_update`, recomputes any derived
#' cell/ring measures and flag columns that are already present in `x`, and
#' splices the result back into the existing [QWAdata] object. Columns not yet
#' present in `x` are not added (i.e. the set of columns is preserved).
#'
#' Duplicate-ring flags are recalculated across all images sharing a woodpiece
#' with any updated image, because overlaps may change when one image changes.
#'
#' @param x a [QWAdata] object
#' @param imgs_to_update character vector of `image_label` values to update;
#'   must be a subset of `meta$image_label`
#' @param meta a [QWAimages] object, required for [complete_flags()] when flag
#'   columns are present in `x`
#' @param exclude_mode passed to [complete_flags()]; only relevant when flag
#'   columns are present in `x`. `NULL` uses the [complete_flags()] default
#'   (`"either"`).
#' @returns An updated [QWAdata] object
#' @export
update_QWAdata <- function(x, imgs_to_update, meta, exclude_mode = NULL) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_update, meta$image_label)

  files_to_update <- meta |>
    dplyr::filter(image_label %in% imgs_to_update)

  # read and preprocess raw cells/rings data for the updated images
  x_new <- collect_raw_data(files_to_update)

  # recalculate only the measures that already exist 
  cell_meas_all <- c("tca", "rwd2", "dcwt", "raddistr.st", "cwtall.adj",
                     "cdrad", "cdtan", "cdratio", "sector100", "ew_lw")
  cell_do_not_calc <- names(x$cells)[!names(x$cells) %in% cell_meas_all]
  ring_meas_all <- c("eww","lww")
  ring_do_not_calc <- names(x$rings)[!names(x$rings) %in% ring_meas_all]
  x_new$cells[cell_do_not_calc] <- NA
  x_new$rings[ring_do_not_calc] <- NA
  x_new <- complete_measures(x_new)
  x_new$cells[cell_do_not_calc] <- NULL
  x_new$rings[ring_do_not_calc] <- NULL

  # for the rings data, we still need to recalculate the automatic flags
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")
  if (any(flag_cols %in% names(x$rings))) {
    if (!all(flag_cols %in% names(x$rings))) {
      cli::cli_abort("Cannot update with partial calculated flag columns. Remove all or run complete_QWAdata first.")
    }
    x_new <- complete_flags(x_new, meta, exclude_mode)

    # only for duplicate rings: WE NEED TO RECALCULATE ALSO FOR OTHER IMAGES FROM SAME WOODPIECE
    # default behavior is to select the year with the highest cell count (that isn't incomplete or missing)
    affected_wps <- unique(files_to_update$woodpiece_label)
    df_rings_wps <- x$rings |> # add old data for affected wps
      dplyr::filter(woodpiece_label %in% affected_wps) |>
      dplyr::filter(!image_label %in% imgs_to_update) |> # filter out old data for updated images
      dplyr::bind_rows(x_new$rings) # add new data

    df_rings_wps <- flag_duplicate_rings(df_rings_wps) # this affects only the duplicate_ring and exclude_dupl cols, so any other edited cols stay the same

    x$rings <- x$rings |>
      dplyr::filter(!woodpiece_label %in% affected_wps) |> # filter out wps to update
      dplyr::bind_rows(df_rings_wps) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~tidyr::replace_na(., FALSE))) |>
      dplyr::arrange(woodpiece_label, slide_label, image_label, year)
  } else {
    x$rings <- x$rings |>
      dplyr::filter(!image_label %in% imgs_to_update) |>
      dplyr::bind_rows(x_new$rings) |>
      dplyr::arrange(woodpiece_label, slide_label, image_label, year)
  }

  x$cells <- x$cells |>
    dplyr::filter(!image_label %in% imgs_to_update) |> # filter out old data
    dplyr::bind_rows(x_new$cells) |>
    dplyr::arrange(image_label, year)

  x
}
