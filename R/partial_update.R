#' Update QWA metadata for selected images
#'
#' Re-reads the settings/metadata files for the given images and replaces the
#' corresponding rows in the existing [QWAimages] object. Useful when source
#' files have changed after initial import.
#'
#' @param meta a [QWAimages] object
#' @param imgs_to_update character vector of `image_label` values to update;
#'   must be a subset of `meta$image_label`
#' @param imgs_date_orders Character; date order string(s) passed to
#'   [lubridate::parse_date_time()] to parse the `img_created_at` field
#' @param settings_date_orders Character; date order string(s) passed to
#'   [lubridate::parse_date_time()] to parse the `rxs_created_at` field
#' @returns An updated [QWAimages] object
#' @export
update_QWAimages <- function(meta, imgs_to_update, 
                             imgs_date_orders, settings_date_orders = NULL,
                             imgs_date_tz = Sys.timezone(), settings_date_tz = Sys.timezone()
                            ) {
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_update, meta$image_label)

  # files_to_update <- meta |>
  #   dplyr::filter(image_label %in% imgs_to_update) |> 
  #   dplyr::pull(fname_settings)

  roxas_version <- attr(meta, "roxas_version")

  # read in settings files again
  # rv_file <- if (roxas_version == "roxas") "ROXAS settings" else "ROXAS AI metadata"
  # results <- files_to_update |>
  #   purrr::map(\(x) read_roxas_settings(x, roxas_version = roxas_version),
  #              .progress = list(name = glue::glue("Reading {rv_file} files..."), clear = TRUE))
  # df_settings_new <- purrr::list_rbind(results)
  df_settings_new <- collect_settings_data(meta |>
    dplyr::filter(image_label %in% imgs_to_update))

  # coerce raw character columns to their target types (shared with collect_settings_data)
  df_settings_new <- cast_settings_types(df_settings_new, roxas_version)
  
  # convert img_created_at dates to POSIXct
    df_settings_new$img_created_at <- df_settings_new$img_created_at |> 
      lubridate::parse_date_time(
        orders = imgs_date_orders, # cf. lubridate::parse_date_time
        tz = imgs_date_tz)

  # convert rxs_created_at dates to POSIXct (already done for standardized roxas ai)
  if (roxas_version == "roxas") {
    df_settings_new$rxs_created_at <- df_settings_new$rxs_created_at |> 
      lubridate::parse_date_time(
        orders = settings_date_orders, # cf. lubridate::parse_date_time
        tz = settings_date_tz)
  }

  # UPDATE THE DATAFRAME WITH THE NEW SETTINGS INFO
  meta <- meta |>
    dplyr::rows_update(df_settings_new, by = "fname_settings")

  meta
}

#' Update a QWAdata object with re-read raw data for selected images
#'
#' Re-reads the raw data files for `imgs_to_update`, recomputes any derived
#' cell/ring measures and derived flag columns that are already present in `x`, and
#' splices the result back into the existing [QWAdata] object.
#'
#' Duplicate-ring flags are recalculated across all images sharing a woodpiece
#' with any updated image, because overlaps may change when one image changes.
#'
#' @param x a [QWAdata] object
#' @param imgs_to_update character vector of `image_label` values to update;
#'   must be a subset of `meta$image_label`
#' @param meta a [QWAimages] object, required for [complete_flags()] when flag
#'   columns are present in `x`
#' @param exclude_mode passed to [complete_flags()]; only relevant when the
#'   `$exclude_issues` flag is present in `x`: how should the `$exclude_issues`
#'    flag column be initialized, to exclude any incomplete or missing rings
#'   (`"either"`, default) or only the incomplete rings (i.e., not flagging
#'   missing/wedging rings for exclusion from analyses, `"incomplete_only"`).
#' @param reset_manual_flags Logical; if `TRUE` (default), any manual flag
#'   column edits made via the flags Shiny app for the updated images are
#'   reset. If `FALSE`, existing values in the manual flag columns are kept
#'   (i.e. this assumes that the dating was not affected by the re-analysis).
#' @returns An updated [QWAdata] object
#' @export
update_QWAdata <- function(x, imgs_to_update, meta,
                           exclude_mode = c("either","incomplete_only"),
                           reset_manual_flags = TRUE) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_update, meta$image_label)
  checkmate::assert_flag(reset_manual_flags)
  exclude_mode <- match.arg(exclude_mode)

  files_to_update <- meta |>
    dplyr::filter(image_label %in% imgs_to_update)

  # read and preprocess raw cells/rings data for the updated images
  x_new <- collect_raw_data(files_to_update)

  # recalculate only the measures that already exist in x
  meas_all <- c("tca", "rwd2", "dcwt", "cwtall_adj",
                "cdrad", "cdtan", "cdratio", "sector100", "ew_lw",
                "eww", "lww")
  meas_to_recalc <- intersect(meas_all, c(names(x$cells), names(x$rings)))
  x_new <- complete_measures(x_new, only = meas_to_recalc)

  # for the rings data, we still need to recalculate the automatic flags
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")

  # first: carry over or reset manual flag columns
  manual_lgl_cols <- setdiff(names(x$rings)[sapply(x$rings, is.logical)], flag_cols)
  manual_char_cols <- intersect(c("affected_tissue", "comment"), names(x$rings))
  manual_cols <- c(manual_lgl_cols, manual_char_cols)

  if (length(manual_cols) > 0) {
    # if we reset: nothing to do, bind rows and replace NA with FALSE
    # if we want to keep old values: add them to x_new
    if (!reset_manual_flags) {
      old_manual <- x$rings |>
        dplyr::filter(image_label %in% imgs_to_update) |>
        dplyr::select(image_label, year, dplyr::all_of(manual_cols))
      x_new$rings <- x_new$rings |>
        dplyr::left_join(old_manual, by = c("image_label", "year"))
    }
  }
  
  # recaluclate flag columns if present:
  if (any(flag_cols %in% names(x$rings))) {
    if (!all(flag_cols %in% names(x$rings))) {
      cli::cli_abort(
        "Cannot update with partial calculated flag columns. Remove all or run complete_QWAdata first."
      )
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

    # update the rings component with the data (incl calc flags for all affected wps)
    x$rings <- x$rings |>
      dplyr::filter(!woodpiece_label %in% affected_wps) |> # filter out any affected/old wp data
      dplyr::bind_rows(df_rings_wps) |>
      dplyr::mutate(dplyr::across(
        dplyr::where(is.logical),
        ~ tidyr::replace_na(., FALSE)
      )) |>
      dplyr::arrange(woodpiece_label, slide_label, image_label, year)
  } else {
    # update the rings component with the data (no calc flags, only imgs_to_update)
    x$rings <- x$rings |>
      dplyr::filter(!image_label %in% imgs_to_update) |>
      dplyr::bind_rows(x_new$rings) |>
      dplyr::mutate(dplyr::across(
        dplyr::where(is.logical),
        ~ tidyr::replace_na(., FALSE)
      )) |>
      dplyr::arrange(woodpiece_label, slide_label, image_label, year)
  }

  # update the cells component with the new data
  x$cells <- x$cells |>
    dplyr::filter(!image_label %in% imgs_to_update) |> # filter out old data
    dplyr::bind_rows(x_new$cells) |>
    dplyr::arrange(image_label, year)

  x
}

# TODO:
# update_QWAprofile