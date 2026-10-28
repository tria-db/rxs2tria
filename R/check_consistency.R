#' Check the consistency of QWA objects
#'
#' Checks that the objects of a dataset match each other, as the last check
#' before a submission ([check_QWAimages()], [check_QWAmetadata()],
#' [check_QWAdata()] and [check_QWAprofile()] check each object on its own).
#' Aborts if fewer than two
#' objects are given or an object is not valid. Warns (or, with
#' `strict = TRUE`, aborts) on:
#' - `meta` and `data`: images in `meta` but not in `data$cells` or
#'   `data$rings`, and vice versa; `woodpiece_label` or `slide_label` in
#'   `data$rings` differing from `meta`; ring years after `outmost_year` + 1
#'   (an incomplete outermost ring). Images without `outmost_year` are reported
#'   as not checked.
#' - `data` and `profile`: years of an image in `profile` without a row in
#'   `data$rings` (the flags app and [extract_rwl()] refuse such profiles).
#' - `meta` and `profile`, if `data` is not given: images in `profile` but not
#'   in `meta`.
#'
#' @param meta A [QWAimages] or [QWAmetadata] object (its `images` component
#'   is used).
#' @param data A [QWAdata] object.
#' @param profile A [QWAprofile] object.
#' @param strict If `TRUE`, abort on the first issue instead of warning.
#' @returns `TRUE` if no issue was found, `FALSE` otherwise (invisibly).
#' @seealso [check_QWAimages()], [check_QWAmetadata()], [check_QWAdata()],
#'   [check_QWAprofile()]
#' @export
check_consistency <- function(meta = NULL, data = NULL, profile = NULL,
                              strict = FALSE) {
  checkmate::assert_flag(strict)
  if (inherits(meta, "QWAmetadata")) meta <- validate_QWAmetadata(meta)$images
  if (!is.null(meta)) validate_QWAimages(meta)
  if (!is.null(data)) validate_QWAdata(data)
  if (!is.null(profile)) validate_QWAprofile(profile)
  if (sum(!is.null(meta), !is.null(data), !is.null(profile)) < 2) {
    cli::cli_abort("Give at least two of {.arg meta}, {.arg data} and {.arg profile}.")
  }

  ok <- TRUE
  if (!is.null(meta) && !is.null(data)) {
    ok <- c(ok, check_meta_data(meta, data, strict))
  }
  if (!is.null(data) && !is.null(profile)) {
    rings <- data$rings %||% data.frame(image_label = character(), year = integer())
    no_ring <- check_rows_in_rings(
      profile, rings, "a profile", strict,
      consequence = "The flags app and {.fn extract_rwl} ({.arg prf_data}) refuse such profiles.",
      hint = "Recalculate the profiles from {.arg data}."
    )
    ok <- c(ok, nrow(no_ring) == 0)
  } else if (!is.null(meta) && !is.null(profile)) {
    only_profile <- setdiff(unique(profile$image_label), meta$image_label)
    if (length(only_profile) > 0) {
      ok <- c(ok, report_issue(c(
        "!" = "{length(only_profile)} image{?s} in {.arg profile} but not in {.arg meta}:",
        cli_truncated_list(only_profile)
      ), strict))
    }
  }

  if (all(ok)) {
    cli::cli_inform(c("v" = "Consistency: all checks passed."))
  } else {
    cli::cli_inform(c("x" = "Consistency: issues found, see warnings."))
  }
  invisible(all(ok))
}

# meta vs. data: images in both directions, rings labels, ring years after
# outmost_year
#' @noRd
check_meta_data <- function(meta, data, strict = FALSE) {
  ok <- TRUE
  hint <- "Run {.fn update_QWAdata} to add or remove images in {.arg data}."
  for (comp in c("cells", "rings")) {
    if (is.null(data[[comp]])) next
    imgs <- unique(data[[comp]]$image_label)
    ok <- c(ok, check_images_in_meta(imgs, meta, comp, strict, hint = hint))
    only_meta <- setdiff(meta$image_label, imgs)
    if (length(only_meta) > 0) {
      ok <- c(ok, report_issue(c(
        "!" = "{length(only_meta)} image{?s} in {.arg meta} but not in {.field {comp}}:",
        cli_truncated_list(only_meta), "i" = hint
      ), strict))
    }
  }

  if (!is.null(data$rings)) {
    cmp <- data$rings |>
      dplyr::distinct(.data$image_label, .data$woodpiece_label, .data$slide_label) |>
      dplyr::inner_join(
        meta |> dplyr::select("image_label", meta_wp = "woodpiece_label",
                              meta_sl = "slide_label"),
        by = "image_label"
      )
    differs <- unique(cmp$image_label[cmp$woodpiece_label != cmp$meta_wp |
                                        cmp$slide_label != cmp$meta_sl])
    if (length(differs) > 0) {
      ok <- c(ok, report_issue(c(
        "!" = "{.field rings}: {.field woodpiece_label} or {.field slide_label} differ from {.arg meta} in {length(differs)} image{?s}:",
        cli_truncated_list(differs),
        "i" = "{.fn extract_rwl} and the profiles use the labels in {.field rings}.",
        "i" = "Re-read these images with {.fn update_QWAdata} ({.arg imgs_to_reread})."
      ), strict))
    }

    ok <- c(ok, check_outmost_year(data$rings, meta, strict))
  }
  invisible(all(ok))
}
