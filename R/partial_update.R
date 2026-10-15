#' Update QWA metadata for selected images
#'
#' Re-reads the settings/metadata files for the given images and replaces the
#' corresponding rows in the existing [QWAimages] object. Useful when source
#' files have changed after initial import.
#'
#' @param meta a [QWAimages] object
#' @param imgs_to_reread character vector of `image_label` values whose
#'   settings/metadata files are re-read; must be a subset of `meta$image_label`
#' @param settings_date_orders Character; date order string(s) passed to
#'   [lubridate::parse_date_time()] to parse the `meas_created_at` field for
#'   ROXAS classic.
#' @param settings_date_tz Optional timezone passed to
#'   [lubridate::parse_date_time()] to parse the `meas_created_at` field. 
#'   Defaults to system timezone.
#' @returns An updated [QWAimages] object
#' @export
update_QWAimages <- function(meta, imgs_to_reread,
                             settings_date_orders = NULL,
                             settings_date_tz = Sys.timezone()
                            ) {
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_reread, meta$image_label)

  roxas_version <- attr(meta, "roxas_version") # TODO: check roxas version ok?

  # read in settings files again
  df_settings_new <- collect_settings_data(meta |>
    dplyr::filter(.data$image_label %in% imgs_to_reread))

  # convert meas_created_at dates to POSIXct (already done for standardized roxas ai)
  if (roxas_version == "roxas") {
    df_settings_new$meas_created_at <- df_settings_new$meas_created_at |> 
      lubridate::parse_date_time(
        orders = settings_date_orders, # cf. lubridate::parse_date_time
        tz = settings_date_tz)
  }

  # UPDATE THE DATAFRAME WITH THE NEW SETTINGS INFO
  meta <- meta |>
    dplyr::rows_update(df_settings_new, by = "fname_settings")

  meta
}

#' Update a QWAdata object to match a QWAimages object
#'
#' Synchronises `x` with the images in `meta`:
#' - images in `meta` but not in `x` are read and added,
#' - images in `x` but not in `meta` are removed,
#' - images in `imgs_to_reread` are read again from their raw data files and
#'   replace the existing data (e.g. after re-analysis in ROXAS/ROXAS AI).
#'
#' For all read images, derived cell/ring measures and flag columns that are
#' already present in `x` are recalculated. Duplicate-ring flags
#' (`duplicate_ring`, `exclude_dupl`) are recalculated for all woodpiece-years
#' that contain an added, removed or re-read image, since overlaps may change;
#' any manual duplicate selection in these woodpiece-years is replaced.
#'
#' Added and re-read images are treated as new data: their flags need to be
#' inspected again, e.g. in the flags Shiny app or by re-applying any custom
#' rule for `exclude_issues`. Only if a re-analysis was minor and did not
#' change the dating, use `reset_manual_flags = FALSE` to keep the previous
#' manual flags.
#'
#' @param x a [QWAdata] object
#' @param meta a [QWAimages] object; defines the images in the updated data
#' @param imgs_to_reread optional character vector of `image_label` values
#'   whose raw data files are read again; must be a subset of
#'   `meta$image_label`. `NULL` (default) re-reads none.
#' @param exclude_mode Deprecated, see [complete_flags()].
#' @param reset_manual_flags Logical; only applies to `imgs_to_reread`. If
#'   `TRUE` (default), any manual edits made via the flags Shiny app are reset.
#'   If `FALSE` (i.e. assuming the dating was not affected by the re-analysis),
#'   the manual flag and comment columns are kept, and so is `exclude_issues`
#'   where it differs from the value derived from the old automatic flags.
#' @returns An updated [QWAdata] object
#' @export
update_QWAdata <- function(x, meta, imgs_to_reread = NULL,
                           exclude_mode = NULL,
                           reset_manual_flags = TRUE) {
  checkmate::assert_class(x, "QWAdata")
  checkmate::assert_class(meta, "QWAimages")
  checkmate::assert_subset(imgs_to_reread, meta$image_label)
  checkmate::assert_flag(reset_manual_flags)
  mode <- resolve_exclude_mode(exclude_mode)

  # sort images into new (read), deleted (drop) and re-read (read and replace)
  imgs_old <- unique(x$rings$image_label)
  imgs_new <- setdiff(meta$image_label, imgs_old)
  imgs_del <- setdiff(imgs_old, meta$image_label)
  imgs_reread <- intersect(imgs_to_reread, imgs_old)
  imgs_read <- c(imgs_new, imgs_reread)

  if (length(c(imgs_read, imgs_del)) == 0) {
    cli::cli_inform(c("i" = "Nothing to update: {.arg x} already matches {.arg meta}."))
    return(x)
  }
  cli::cli_inform(c(
    "i" = if (length(imgs_new) > 0) "Adding {length(imgs_new)} new image{?s}: {.val {imgs_new}}",
    "i" = if (length(imgs_del) > 0) "Removing {length(imgs_del)} image{?s} not in {.arg meta}: {.val {imgs_del}}",
    "i" = if (length(imgs_reread) > 0) "Re-reading {length(imgs_reread)} image{?s}: {.val {imgs_reread}}"
  ))

  # automatic flag columns: either all or none present
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")
  has_flags <- any(flag_cols %in% names(x$rings))
  if (has_flags && !all(flag_cols %in% names(x$rings))) {
    cli::cli_abort(c(
      "x" = "Cannot update with only some of the flag columns {.field {flag_cols}}.",
      "i" = "Remove them all or run {.fn complete_QWAdata} first."
    ))
  }
  manual_lgl_cols <- setdiff(names(x$rings)[sapply(x$rings, is.logical)], flag_cols)
  manual_char_cols <- intersect(c("affected_tissue", "comment"), names(x$rings))
  manual_cols <- c(manual_lgl_cols, manual_char_cols)

  by_img_yr <- c("image_label", "year")
  by_wp_yr <- c("woodpiece_label", "year")

  # keep old rings of re-read images (to carry over manual values) and the
  # woodpiece-years they occupy (duplicate flags need recalculation there)
  rings_reread_old <- x$rings |>
    dplyr::filter(.data$image_label %in% imgs_reread)
  affected_wp_yrs <- x$rings |>
    dplyr::filter(.data$image_label %in% c(imgs_del, imgs_reread)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(by_wp_yr)))

  # drop deleted and re-read images
  x$rings <- x$rings |>
    dplyr::filter(!.data$image_label %in% c(imgs_del, imgs_reread))
  x$cells <- x$cells |>
    dplyr::filter(!.data$image_label %in% c(imgs_del, imgs_reread))

  if (length(imgs_read) > 0) {
    x_new <- collect_raw_data(meta |>
      dplyr::filter(.data$image_label %in% imgs_read))

    # recalculate only the measures that already exist in x
    meas_all <- c("tca", "rwd2", "dcwt", "cwtall_adj",
                  "cdrad", "cdtan", "cdratio", "sector100", "ew_lw",
                  "eww", "lww")
    meas_to_recalc <- intersect(meas_all, c(names(x$cells), names(x$rings)))
    x_new <- complete_measures(x_new, only = meas_to_recalc)

    # carry over manual values for re-read images (new images have none)
    if (!reset_manual_flags && length(manual_cols) > 0) {
      x_new$rings <- x_new$rings |>
        dplyr::left_join(
          rings_reread_old |>
            dplyr::select(dplyr::all_of(c(by_img_yr, manual_cols))),
          by = by_img_yr)
    }

    if (has_flags) {
      x_new <- complete_flags(x_new, meta, exclude_mode)
      # keep exclude_issues where it was edited manually, i.e. where the old
      # value differs from the automatic rule applied to the old flags
      if (!reset_manual_flags) {
        excl_manual <- rings_reread_old |>
          dplyr::mutate(excl_auto = switch(mode,
            "either" = .data$incomplete_ring | .data$missing_ring,
            "incomplete_only" = .data$incomplete_ring)) |>
          dplyr::filter(.data$exclude_issues != .data$excl_auto) |>
          dplyr::select(dplyr::all_of(by_img_yr), excl_old = "exclude_issues")
        x_new$rings <- x_new$rings |>
          dplyr::left_join(excl_manual, by = by_img_yr) |>
          dplyr::mutate(exclude_issues = dplyr::coalesce(.data$excl_old, .data$exclude_issues)) |>
          dplyr::select(-dplyr::all_of("excl_old"))
      }
      affected_wp_yrs <- affected_wp_yrs |>
        dplyr::bind_rows(
          x_new$rings |> dplyr::distinct(dplyr::across(dplyr::all_of(by_wp_yr)))) |>
        dplyr::distinct()
    }

    x$rings <- dplyr::bind_rows(x$rings, x_new$rings)
    x$cells <- dplyr::bind_rows(x$cells, x_new$cells)
  }

  # recalculate duplicate flags only for affected woodpiece-years
  if (has_flags) {
    x$rings <- recalc_duplicates(x$rings, affected_wp_yrs)
  }

  x$rings <- x$rings |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ tidyr::replace_na(., FALSE))) |>
    dplyr::arrange(.data$woodpiece_label, .data$slide_label, .data$image_label, .data$year)
  x$cells <- x$cells |>
    dplyr::arrange(.data$image_label, .data$year)

  x
}

#' Update a QWAmetadata object to match a QWAimages object
#'
#' Replaces the `$images` component of `x` with `images` and updates the
#' hierarchical tables (`$sites`, `$trees`, `$woodpieces`, `$slides`)
#' accordingly:
#' - the hierarchy columns and counts (`n_trees`, `n_woodpieces`, `n_slides`,
#'   `n_images`) are derived from `images`,
#' - all other columns are kept from `x`,
#' - rows for IDs new in `images` are added (other columns `NA`),
#' - rows for IDs no longer in `images` are removed.
#'
#' Tables that are `NULL` in `x` remain `NULL` (see [complete_QWAmetadata()]).
#' The columns edited in the metadata Shiny app (`only_ew`, `img_comment`) are
#' filled from `x$images` where they are missing or `NA` in `images`, e.g.
#' after rebuilding it with [build_QWAimages()].
#'
#' @param x a [QWAmetadata] object
#' @param images a [QWAimages] object; the complete set of images of the
#'   updated dataset
#' @returns An updated [QWAmetadata] object
#' @seealso [update_QWAimages()], [update_QWAdata()], [check_QWAmetadata()]
#' @export
update_QWAmetadata <- function(x, images) {
  checkmate::assert_class(x, "QWAmetadata")
  checkmate::assert_class(images, "QWAimages")
  rv <- attr(images, "roxas_version")

  # images: fill app-editable columns from x$images where missing or NA
  app_cols <- intersect(c("only_ew", "img_comment"), names(x$images))
  images <- tibble::as_tibble(images)
  for (col in setdiff(app_cols, names(images))) {
    images[[col]] <- x$images[[col]][NA_integer_] # typed NA
  }
  images <- images |>
    dplyr::rows_patch(
      tibble::as_tibble(x$images)[c("image_label", app_cols)],
      by = "image_label", unmatched = "ignore") |>
    new_QWAimages(rv)

  # hierarchical tables: counts and hierarchy columns from images, other
  # columns kept from x; rows for new IDs added, rows for missing IDs removed
  for (tbl in names(hierarchy_tbls)) {
    df_old <- x[[tbl]]
    if (is.null(df_old)) next
    h <- hierarchy_tbls[[tbl]]
    df_counts <- h$summary_fn(images)
    ids_new <- setdiff(df_counts[[h$id_col]], df_old[[h$id_col]])
    ids_del <- setdiff(df_old[[h$id_col]], df_counts[[h$id_col]])
    if (length(ids_new) + length(ids_del) > 0) {
      cli::cli_inform(c(
        "i" = if (length(ids_new) > 0) "{.field {tbl}}: adding {length(ids_new)} row{?s}: {.val {ids_new}}",
        "!" = if (length(ids_del) > 0) "{.field {tbl}}: removing {length(ids_del)} row{?s}: {.val {ids_del}}"
      ))
    }
    x[[tbl]] <- df_counts |>
      dplyr::left_join(
        df_old |> dplyr::select(-dplyr::any_of(setdiff(names(df_counts), h$id_col))),
        by = h$id_col) |>
      dplyr::select(dplyr::all_of(names(df_old)))
  }

  x$images <- images
  x
}

# TODO:
# update_QWAprofile