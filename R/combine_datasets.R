#' Combine two QWAimages objects
#'
#' Combines the images of two [QWAimages] objects, e.g. from separate input
#' folders, into one. Both objects must have the same `roxas_version` and
#' distinct `image_label` values. The result is aligned and validated by
#' [QWAimages()].
#'
#' @param x,y [QWAimages] objects
#' @returns A combined [QWAimages] object
#' @seealso [combine_QWAdata()], [combine_QWAmetadata()]
#' @export
combine_QWAimages <- function(x, y) {
  validate_QWAimages(x)
  validate_QWAimages(y)
  rv <- attr(x, "roxas_version")
  if (!identical(rv, attr(y, "roxas_version"))) {
    cli::cli_abort("Cannot combine {.cls QWAimages} with different ROXAS versions.")
  }
  shared <- intersect(x$image_label, y$image_label)
  if (length(shared) > 0) {
    cli::cli_abort(c("x" = "{length(shared)} image label{?s} in both objects.",
                     "i" = "Shared: {.val {shared}}",
                     "i" = "Remove the duplicates from one object before combining, e.g. with {.code dplyr::filter(y, !image_label %in% x$image_label)}."))
  }
  QWAimages(dplyr::bind_rows(tibble::as_tibble(x), tibble::as_tibble(y)), rv)
}

#' Combine two QWAdata objects
#'
#' Combines the cells and rings data of two [QWAdata] objects with distinct
#' `image_label` values into one. Columns present in only one object are
#' filled with `NA`; `NA` in logical ring columns is set to `FALSE`.
#'
#' If the ring flag columns are present (both objects must have them or
#' neither), the duplicate-ring flags (`duplicate_ring`, `exclude_dupl`) are
#' recalculated for woodpiece-years with rings from both objects, replacing any
#' manual duplicate selection there. All other flags are kept.
#'
#' @param x,y [QWAdata] objects
#' @returns A combined [QWAdata] object
#' @seealso [combine_QWAimages()], [combine_QWAmetadata()]
#' @export
combine_QWAdata <- function(x, y) {
  validate_QWAdata(x)
  validate_QWAdata(y)
  shared <- intersect(x$rings$image_label, y$rings$image_label)
  if (length(shared) > 0) {
    cli::cli_abort(c("x" = "{length(shared)} image label{?s} in both objects.",
                     "i" = "Shared: {.val {shared}}",
                     "i" = "Remove the duplicates from the {.field cells} and {.field rings} of one object before combining."))
  }
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring", "exclude_dupl", "exclude_issues")
  has_flags <- c(all(flag_cols %in% names(x$rings)), all(flag_cols %in% names(y$rings)))
  if (has_flags[1] != has_flags[2]) {
    cli::cli_abort(c("x" = "Only one object has the ring flag columns.",
                     "i" = "Run {.fn complete_QWAdata} on both or neither."))
  }
  for (comp in c("cells", "rings")) {
    cols_one <- setdiff(union(names(x[[comp]]), names(y[[comp]])),
                        intersect(names(x[[comp]]), names(y[[comp]])))
    if (length(cols_one) > 0) {
      cli::cli_inform(c("i" = "{.field {comp}}: column{?s} {.field {cols_one}} only in one object, filled with {.val {NA}} (logical: {.val {FALSE}})."))
    }
  }

  rings <- dplyr::bind_rows(x$rings, y$rings)
  # duplicates can only change where both objects have rings for a woodpiece-year
  if (has_flags[1]) {
    by_wp_yr <- c("woodpiece_label", "year")
    shared_wp_yrs <- dplyr::inner_join(
      dplyr::distinct(x$rings, dplyr::across(dplyr::all_of(by_wp_yr))),
      dplyr::distinct(y$rings, dplyr::across(dplyr::all_of(by_wp_yr))),
      by = by_wp_yr)
    rings <- recalc_duplicates(rings, shared_wp_yrs)
  }
  rings <- rings |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ tidyr::replace_na(., FALSE))) |>
    dplyr::arrange(.data$woodpiece_label, .data$slide_label, .data$image_label, .data$year)
  cells <- dplyr::bind_rows(x$cells, y$cells) |>
    dplyr::arrange(.data$image_label, .data$year)

  new_QWAdata(cells = cells, rings = rings)
}

# bind two tables, dropping rows of df_y whose key is already in df_x
# (rows with NA key are kept); without key, drop duplicate rows
#' @noRd
bind_by_key <- function(df_x, df_y, key = NULL) {
  if (is.null(df_x) || is.null(df_y)) return(df_x %||% df_y)
  if (is.null(key)) return(dplyr::distinct(dplyr::bind_rows(df_x, df_y)))
  df_y <- df_y |>
    dplyr::filter(is.na(.data[[key]]) | !.data[[key]] %in% df_x[[key]])
  dplyr::bind_rows(df_x, df_y)
}

#' Combine two QWAmetadata objects
#'
#' Combines two [QWAmetadata] objects, e.g. prepared from separate input
#' folders, into one. The images are combined with [combine_QWAimages()], so both
#' objects must have the same `roxas_version` and distinct `image_label`
#' values. Where both objects contain the same entry, `x` takes precedence:
#' - `$dataset` is taken from `x`,
#' - `$authors` are combined and deduplicated by `email`, then renumbered with
#'   the authors of `x` first; the contact person of `x` is kept,
#' - `$funding` is combined and deduplicated by entire row,
#' - `$related` is combined and deduplicated by `citation`,
#' - `$sites`, `$trees`, `$woodpieces` and `$slides` are combined and
#'   deduplicated by their ID column (with a warning for shared IDs with
#'   different values), and their counts are updated with
#'   [update_QWAmetadata()].
#'
#' Submission files only; TRIA dataset files are not edited.
#'
#' @param x,y [QWAmetadata] objects
#' @returns A combined [QWAmetadata] object
#' @seealso [combine_QWAimages()], [combine_QWAdata()], [update_QWAmetadata()]
#' @export
combine_QWAmetadata <- function(x, y) {
  validate_QWAmetadata(x)
  validate_QWAmetadata(y)
  assert_submission(x, "combine_QWAmetadata") # TODO: might it make sense to use this fct to combine published datasets into one?
  assert_submission(y, "combine_QWAmetadata")
  images <- combine_QWAimages(x$images, y$images)

  if (!is.null(y$dataset)) {
    cli::cli_inform(c("i" = "{.field dataset}: kept from {.arg x}, dropped from {.arg y}."))
  }

  # authors: x first, renumbered; only one contact person
  authors_y <- y$authors
  if (!is.null(authors_y)) {
    authors_y <- dplyr::arrange(authors_y, .data$author_nr)
    if (any(x$authors$contact_person %in% TRUE)) authors_y$contact_person <- FALSE
  }
  authors <- bind_by_key(x$authors, authors_y, "email")
  if (!is.null(authors)) authors$author_nr <- seq_len(nrow(authors))

  funding <- bind_by_key(x$funding, y$funding)
  related <- bind_by_key(x$related, y$related, "citation")

  # hierarchical tables: x wins for shared IDs, warn if the values differ
  count_cols <- c("n_trees", "n_woodpieces", "n_slides", "n_images")
  hier <- purrr::imap(hierarchy_tbls, \(h, tbl) {
    df_x <- x[[tbl]]
    df_y <- y[[tbl]]
    if (!is.null(df_x) && !is.null(df_y)) {
      cmp_cols <- setdiff(intersect(names(df_x), names(df_y)), count_cols)
      conflicts <- df_y[cmp_cols] |>
        dplyr::filter(.data[[h$id_col]] %in% df_x[[h$id_col]]) |>
        dplyr::anti_join(df_x[cmp_cols], by = cmp_cols) |>
        dplyr::pull(h$id_col)
      if (length(conflicts) > 0) {
        cli::cli_warn(c("{.field {tbl}}: {length(conflicts)} shared ID{?s} with different values, kept from {.arg x}.",
                        "i" = "IDs: {.val {conflicts}}"))
      }
    }
    bind_by_key(df_x, df_y, h$id_col)
  })

  combined <- new_QWAmetadata(x$dataset, authors, funding, related,
    datafiles = NULL, # written by TRIA at publish time
    hier$sites, hier$trees, hier$woodpieces, hier$slides, images)
  # correct counts for shared IDs
  update_QWAmetadata(combined, images)
}
