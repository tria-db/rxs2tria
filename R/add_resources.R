#' Compile resources manifest from directory
#'
#' @description
#' Scans a directory (incl. subdirectories) for supplementary resource files 
#' (original and annotated images, raw ROXAS output, reference series, etc.) 
#' and returns a data frame listing each file together with its inferred
#' resource type and the hierarchical entity it belongs to (e.g. image_label or
#' woodpiece_label; if available).
#'
#' This function does not move, copy, rename, or compress any files. Rather it
#' creates a manifest of supplementary files by leveraging the suffix naming 
#' conventions used by ROXAS and ROXAS AI. Therefore, it is only necessary
#' if you wish to include supporting materials beyond the [QWAdata] and 
#' [QWAmetadata] files in your submission. In those cases, the function works
#' as a sanity check to ensure only the 'right' supplementary files are 
#' included in the submission directory. Specifically, the found files are
#' checked for validty (e.g. if there were files for which the resource type
#' or label could not be inferred), with warnings raised in case of issues. To
#' address such warnings, you may need to manually change the contents of the
#' input directory and re-run the function (to add/remove/rename files), or
#' edit the resources data frame directly (to update the values for 
#' `resource_type`, `linked_level`, `linked_label` and `description`).
#'
#' `add_resources()` is a thin wrapper around `collect_resources()`: instead of
#' a [QWAimages] object, it takes an entire a [QWAmetadata] object as input
#' (feeding its `$images` component to `collect_resources()`) and returns
#' the object with the resource manifest as the `$resources` component. 
#' 
#' See `vignette("resources")` for a worked example and the complete list of
#' recognised resource types.
#'
#' @details
#' # Resource types
#'
#' The `resource_type` of each file is inferred from its name (for
#' example `*_Output_Cells.txt` becomes `"roxas_output_cells"`,
#' `*.metadata.json` becomes `"rai_metadata"`). Patterns are checked most
#' specific first, and `rxs_images`'s `roxas_version` is used to
#' disambiguate patterns shared between classic ROXAS and ROXAS AI. Files
#' that match no known pattern are typed as `"other"`. Known backup and junk
#' files (ROXAS `_bu` backups, `Thumbs.db`, Office lock files, ...) are typed
#' as `"junk"`, and \pkg{rxs2tria}-generated files (`QWAmetadata.json`, the
#' `QWAcells`/`QWArings`/`QWAprofile` `.csv(.gz)` files) are typed
#' accordingly. The full pattern table is documented in `vignette("resources")`.
#'
#' Every file found is kept as a row (see [check_resources()] for how
#' `status`/`note` classify what happens to each one)---junk files will never
#' be included in the file upload to the TRIA database, but are shown here too 
#' so the table is a complete record of the directory.
#'
#' # Hierarchy level and linked entity
#'
#' Every resource type has a default `linked_level` describing the level of the
#' data hierarchy the file pertains to: `"dataset"` (applies to the whole
#' submission, e.g. a reference chronology), `"woodpiece"`, `"slide"`,
#' `"image"`, or `"analysis"` (per-image ROXAS (AI) analysis files such as
#' shapefiles or annotated images). `linked_label` identifies *which* entity at
#' that level the file belongs to (e.g. a specific `image_label`).
#'
#' `linked_label` is filled automatically for `image`-, `analysis`-,
#' `slide`-, and `woodpiece`-level resources using `rxs_images`.
#'
#' # Automatic label matching
#'
#' Each unlabelled `image`/`analysis` resource is matched to an image by
#' testing whether its file name starts with an image's base name (from
#' `rxs_images$org_img_name`). The longest matching base name wins, which
#' prevents false matches between labels that share a prefix (e.g. `S22_L1`
#' vs `S22_L10`).
#'
#' `slide`/`woodpiece`-level resources (e.g. `SLIDENAME_Preview.jpg`,
#' `WPNAME_Panorama.tif`) are typically named such that they do not fully
#' match the constructed `slide_label`/`woodpiece_label`. Rather than
#' matching against those constructed labels, the original identifier is
#' recovered empirically: sibling images sharing a `slide_label` (or
#' `woodpiece_label`) are almost always named with a common prefix followed
#' by an image-specific suffix, so the longest common prefix of their
#' `org_img_name` values recovers that original identifier without needing to
#' know the naming pattern. A slide/woodpiece with only one image has no
#' sibling to compare against, so instead its own trailing token (the part
#' after the last separator, assumed to be the image-specific part) is
#' stripped.
#'
#' Resources that cannot be matched are left as `NA`, and the number of
#' successful matches is reported.
#'
#' # Manual editing
#'
#' While `collect_resources()` aims to recognize the correct `resource_type`
#' and `linked_label` for each file, there may be cases (e.g. special files,
#' renamed files) where one or both values are not inferred correctly. Check
#' the function output carefully, and make the appropriate changes in the
#' input directory (and re-run the function) or in the resources table
#' directly.
#'
#' @param path Path to a directory to scan for files.
#' @param rxs_images A [QWAimages] object. Its `roxas_version` attribute is
#'   used to disambiguate resource-type patterns shared between classic ROXAS
#'   and ROXAS AI, and its `org_img_name`, `image_label`, `slide_label`, and
#'  `woodpiece_label` columns are used to auto-populate `linked_label` where
#'   appropriate.
#'
#' @returns A data frame with one row per file and columns:
#'   - `resource_name`: name and path of the file relative to `path`
#'   - `resource_type`: inferred resource type string (see Details).
#'   - `linked_level`: hierarchy level for this type (e.g. `"dataset"`,
#'     `"woodpiece"`, or `"image"`).
#'   - `linked_label`: label of the linked entity, auto-filled from where 
#'      possible, otherwise `NA` (fill in manually if required).
#'   - `fname_resource`: absolute path to the file.
#'   - `status`, `note`: computed by [check_resources()], status is `"ok"`,
#'     `"review"`, or `"ignore"`, note gives further details.
#'   For `add_resources()` the input `x`is returned with the resources data
#'   frame in the `x$resources` component.
#'
#' @seealso [check_resources()], `vignette("resources")`
#' @examples
#' \dontrun{
#' res <- collect_resources("path/to/submission_files", rxs_images = my_images)
#' 
#' # typical workflow with an existing QWAmetadata object
#' QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
#' QWA_meta <- add_resources(QWA_meta, path = "input_data/submission_files")
#' # review and make changes are required, then persist:
#' QWA_meta$resources
#' write_QWAmetadata(QWA_meta, "output_data/my_dataset_QWAmetadata.json")
#' }
#' @export
collect_resources <- function(path, rxs_images) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_class(rxs_images, "QWAimages")
  checkmate::assert_names(names(rxs_images),
                          must.include = c("org_img_name", "image_label",
                          "slide_label", "woodpiece_label"))

  roxas_version <- attr(rxs_images, "roxas_version")

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema  <- resolve_schema(schema_obj, schema_path)
  tbl_props   <- get_tbl_props(tbl_schema)

  files <- fs::dir_ls(path, recurse = TRUE, type = "file")

  if (length(files) == 0) {
    cli::cli_alert_info("No files found in {.path {path}}")
    return(create_empty_df(tbl_props))
  }

  new_res <- infer_resource_types(files, roxas_version = roxas_version)
  new_res$linked_label <- NA_character_

  # resource_name is the path relative to the scanned directory
  new_res$resource_name <- as.character(fs::path_rel(new_res$fname_resource, start = path))

  # auto-fill linked_label for image/analysis/slide/woodpiece-level resources
  to_match <- new_res$linked_level %in% c("analysis", "image", "slide", "woodpiece")

  if (any(to_match)) {
    # Strip image extensions from org_img_name to obtain base label pattern.
    # Handles classic ROXAS (.jpg) and ROXAS AI (.scan.jpg).
    img_bases <- sub("(\\.scan)?\\.(jpg|jpeg|png|tif|tiff)$", "",
                     rxs_images$org_img_name, ignore.case = TRUE)

    # image/analysis match against each image's own base name; slide/woodpiece
    # match against a prefix derived from their sibling images (see
    # .derive_level_prefixes()) since there is no "original" slide/woodpiece
    # name stored anywhere -- only the fully constructed labels.
    lookups <- list(
      image     = list(prefix = img_bases, label = as.character(rxs_images$image_label)),
      analysis  = list(prefix = img_bases, label = as.character(rxs_images$image_label)),
      slide     = .derive_level_prefixes(img_bases, rxs_images$slide_label),
      woodpiece = .derive_level_prefixes(img_bases, rxs_images$woodpiece_label)
    )

    # matched against each file's own basename -- org_img_name never includes
    # a directory component, so the relative-path resource_name must be
    # stripped back down to its basename before testing startsWith()
    new_res$linked_label[to_match] <- mapply(
      function(rname, lvl) {
        lut <- lookups[[lvl]]
        keep <- !is.na(lut$prefix)
        hits <- which(startsWith(rname, lut$prefix[keep]))
        if (length(hits) == 0L) return(NA_character_)
        # Longest match wins to avoid false positives (e.g. "S22_L1" vs "S22_L10")
        best <- hits[which.max(nchar(lut$prefix[keep][hits]))]
        as.character(lut$label[keep][[best]])
      },
      as.character(fs::path_file(new_res$resource_name[to_match])),
      new_res$linked_level[to_match],
      USE.NAMES = FALSE
    )

    n_matched <- sum(!is.na(new_res$linked_label[to_match]))
    cli::cli_inform(c("i" = "Matched {n_matched}/{sum(to_match)} resource{?s} to a data-structure label"))
  }

  # ensure we have all schema columns
  new_res <- create_empty_df(tbl_props) |>
    dplyr::bind_rows(new_res)

  check_resources(new_res, rxs_images)
}

#' @rdname collect_resources
#' @param x A [QWAmetadata] object. Its `$images` component is fed to 
#'   `collect_resources()`.
#' @export
add_resources <- function(x, path) {
  checkmate::assert_class(x, "QWAmetadata")
  checkmate::assert_directory_exists(path)
  x$resources <- collect_resources(path = path, rxs_images = x$images)
  x
}

#' Validate a resources manifest and compute status for each file
#'
#' Runs a series of checks on a resources data frame (e.g. `x$resources`, or
#' the output of [collect_resources()]) and annotates every row with a
#' `status`:
#' - `"ignore"`: a junk file (ROXAS `_bu` backups, `Thumbs.db`, Office lock
#'   files, ...). Never part of TRIA upload, so we will ignore such files.
#' - `"review"`: needs manual attention before this file can be included in
#'   submission. Covers an unrecognised (`"other"`) supplementary file without
#'   `description` (add one to justify including it as-is, or fix
#'   `resource_type`), a missing `linked_label` on a type that requires one, 
#'   more than one file of a type expected to occur once per level, a resource
#'   type specific to the other ROXAS version, or a `linked_label` that does
#'   not match any known label. `note` records which of these applies 
#'   (possibly more than one).
#' - `"ok"`: passes every check, ready to include in the submission zip.
#'
#' A summary is printed: files needing review are listed (with their `note`),
#' or, once none remain, a message confirms the directory of supplementary
#' files is ready to be zipped for submission.
#'
#' [collect_resources()] runs this automatically, call it directly to re-run 
#' the checks, e.g. after manually editing a resources table.
#'
#' @param res A resources data frame.
#' @param rxs_images A [QWAimages] object, used to validate `linked_label`
#'   values (see [collect_resources()]).
#' @returns `res` with `status` and `note` (re)computed for every row.
#' @seealso [collect_resources()], [add_resources()]
#' @export
check_resources <- function(res, rxs_images) {
  checkmate::assert_data_frame(res)
  checkmate::assert_class(rxs_images, "QWAimages")

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  check_schema(res, schema_obj, "resources", warn_only = TRUE, greedy = FALSE)

  res_meta <- dplyr::left_join(res, .resource_type_meta(), by = "resource_type")
  roxas_version <- attr(rxs_images, "roxas_version")

  is_junk      <- res$resource_type == "junk"
  is_generated <- res$resource_type %in% c(
    "QWAdata_cells", "QWAdata_rings", "QWAmetadata", "QWAprofile")

  review_note <- .review_reasons(res, res_meta, roxas_version, rxs_images)

  res$status <- dplyr::case_when(
    is_junk              ~ "ignore",
    nzchar(review_note)    ~ "review",
    TRUE                   ~ "ok"
  )
  res$note <- dplyr::case_when(
    is_junk              ~ "System/backup file, always excluded from the archive",
    is_generated         ~ "Generated by rxs2tria",
    nzchar(review_note)  ~ review_note,
    TRUE                 ~ NA_character_
  )

  .report_resource_status(res)

  res
}

# Helper functions ----

#' Load resource types table
#' @noRd
load_resource_types <- function() {
  path <- system.file("extdata", "resource_types.csv", package = "rxs2tria")
  tbl <- vroom::vroom(path, delim = ",", show_col_types = FALSE, progress = FALSE)
  tbl[order(tbl$priority), ]
}

#' Infer resource type from a file name
#'
#' Matches file names against the naming patterns listed in
#' `inst/extdata/resource_types.csv` (see also `vignette("resources")`) and 
#' returns the corresponding `resource_type` and relevant info, or `"other"`
#' for unrecognised files. Patterns are tried in ascending `priority` order
#' (most specific first) and the first match wins.
#'
#' @param filenames Character; one or more file names (full paths).
#' @param roxas_version `"roxas"` or `"roxas_ai"`. Used to
#'   disambiguate patterns that are specific to one ROXAS version.
#' @returns A tibble with one row per file: `resource_name`, `fname_resource`,
#'   `resource_type`, `linked_level`.
#' @seealso [collect_resources()]
#' @noRd
infer_resource_types <- function(filenames, roxas_version) {
  checkmate::assert_choice(roxas_version, c("roxas", "roxas_ai"))
  exclude_type <- switch(roxas_version,
    roxas = "rai_image_annotated", roxas_ai = "roxas_image_annotated"
  )
  res_tbl <- load_resource_types() |>
    dplyr::filter(.data$resource_type != exclude_type)

  fnames <- fs::path_file(filenames)

  # for each pattern (priority order), record its row index wherever it
  # matches, else NA; coalesce() then keeps, per file, the index of the
  # first (= highest-priority) pattern that matched.
  match_idx <- purrr::imap(res_tbl$identifier, \(pattern, i) {
    dplyr::if_else(grepl(pattern, fnames, ignore.case = TRUE), i, NA_integer_)
  }) |> purrr::reduce(dplyr::coalesce)

  # add an "other" row for unmatched filenames: pulling from this row keeps
  # linked_level as NA, without needing a separate coalesce() step.
  match_idx[is.na(match_idx)] <- nrow(res_tbl) + 1L
  res_tbl[nrow(res_tbl) + 1L, "resource_type"] <- "other"

  tibble::tibble(
    resource_name  = fnames,
    fname_resource = filenames
  ) |>
    dplyr::bind_cols(res_tbl[match_idx, c("resource_type", "linked_level")])
}

# --- Auto-label detection -------------------------------------------------
#' Derive an "original name" prefix for each group of a hierarchy level
#'
#' Sibling images that belong to the same slide/woodpiece are almost always
#' named with a shared prefix (the part of `org_img_name` identifying that
#' slide/woodpiece) followed by an image-specific suffix -- that shared prefix
#' is exactly the string other slide-/woodpiece-level files (e.g.
#' `SLIDENAME_Preview.jpg`) are named after. This is derived empirically as
#' the longest common prefix of `img_bases` within each `group_label`, so it
#' works for any labelling pattern without needing the original regex.
#'
#' A group with a single member has no sibling to compare against, so its
#' own trailing token (the part after the last separator, assumed to be the
#' image-specific part) is stripped instead, on the same assumption used for
#' multi-member groups.
#'
#' @param img_bases Character vector of extension-stripped `org_img_name`.
#' @param group_label Character vector (same length), e.g. `slide_label`.
#' @returns A list with `prefix` and `label`, one entry per distinct
#'   `group_label` (`NA` groups dropped).
#' @noRd
.derive_level_prefixes <- function(img_bases, group_label) {
  keep <- !is.na(group_label) & !is.na(img_bases)
  labels <- unique(group_label[keep])
  prefixes <- vapply(labels, function(lbl) {
    .common_prefix(unique(img_bases[keep][group_label[keep] == lbl]))
  }, character(1))
  list(prefix = unname(prefixes), label = labels)
}

#' @noRd
.common_prefix <- function(x) {
  # A singleton group has no sibling to diverge from, so there is no true
  # LCP to compute -- but the same trailing-token trim still applies to its
  # own name, on the assumption that (as with multi-image groups) the image-
  # specific part is confined to the token after the last separator.
  lcp <- if (length(x) == 1L) x else Reduce(.pairwise_lcp, x)
  if (!nzchar(lcp)) return(NA_character_)
  # trim back to the last separator to avoid overshooting into characters
  # that coincidentally match at the start of the differing suffix (e.g.
  # the shared "0" in "..._01" vs "..._02")
  trimmed <- .trim_to_separator(lcp)
  if (nzchar(trimmed)) trimmed else lcp
}

#' @noRd
.pairwise_lcp <- function(a, b) {
  n <- min(nchar(a), nchar(b))
  if (n == 0L) return("")
  ca <- substr(a, 1, n)
  cb <- substr(b, 1, n)
  diffs <- which(strsplit(ca, "")[[1]] != strsplit(cb, "")[[1]])
  if (length(diffs) == 0L) ca else substr(ca, 1, diffs[[1]] - 1L)
}

#' @noRd
.trim_to_separator <- function(s) {
  m <- regexpr("^.*[_.\\- ]", s, perl = TRUE)
  if (m[[1]] == -1L) return("")
  regmatches(s, m)
}



# --- Validity checks ---------------------------------------------------
# Extended checks beyond the JSON schema, using the metadata columns
# (link_required, one_per_level, source) from resource_types.csv. These are
# looked up by resource_type via a join, rather than carried as columns
# through the resources table itself (which only has the schema columns).
# Each check below returns a per-row reason string ("" if it doesn't apply),
# which .review_reasons() combines into a single note per row.

#' @noRd
.resource_type_meta <- function(tbl = load_resource_types()) {
  tbl[!duplicated(tbl$resource_type),
      c("resource_type", "link_required", "one_per_level", "source")]
}

#' @noRd
.review_reasons <- function(res, res_meta, roxas_version, rxs_images) {
  reasons <- rep("", nrow(res))
  add <- function(reasons, cond, msg) {
    cond[is.na(cond)] <- FALSE
    reasons[cond] <- ifelse(nzchar(reasons[cond]), paste0(reasons[cond], "; ", msg), msg)
    reasons
  }

  has_description <- !is.na(res$description) & nzchar(res$description)
  reasons <- add(reasons, res$resource_type == "other" & !has_description,
    "Unrecognised file type - assign a resource_type, or add a description justifying its inclusion")

  reasons <- add(reasons,
    !is.na(res_meta$link_required) & res_meta$link_required == "x" & is.na(res_meta$linked_label),
    "Missing linked_label (required for this resource type)")

  reasons <- add(reasons, .flag_duplicates(res_meta),
    "More than one file of this resource_type for this linked_label (expected only one)")

  other_rv <- if (roxas_version == "roxas") "roxas_ai" else "roxas"
  reasons <- add(reasons,
    !is.na(res_meta$source) & res_meta$source == other_rv,
    glue::glue("Resource type is specific to {rv_msg(other_rv)}, but this dataset is {rv_msg(roxas_version)}"))

  reasons <- add(reasons, .flag_invalid_labels(res, rxs_images),
    "linked_label does not match any known label at this linked_level")

  reasons
}

#' @noRd
.flag_duplicates <- function(df_meta) {
  check_rows <- !is.na(df_meta$one_per_level) & df_meta$one_per_level == "x" &
    !is.na(df_meta$linked_label)
  dup <- rep(FALSE, nrow(df_meta))
  if (!any(check_rows)) return(dup)

  counts <- df_meta[check_rows, ] |>
    dplyr::count(.data$resource_type, .data$linked_label, name = "n")
  dup[check_rows] <- dplyr::left_join(
    df_meta[check_rows, c("resource_type", "linked_label")], counts,
    by = c("resource_type", "linked_label")
  )$n > 1
  dup
}

#' @noRd
.flag_invalid_labels <- function(df, rxs_images) {
  valid_labels <- list(
    image     = unique(as.character(rxs_images$image_label)),
    analysis  = unique(as.character(rxs_images$image_label)),
    slide     = unique(as.character(rxs_images$slide_label)),
    woodpiece = unique(as.character(rxs_images$woodpiece_label))
  )
  check_rows <- !is.na(df$linked_label) & df$linked_level %in% names(valid_labels)
  invalid <- rep(FALSE, nrow(df))
  if (!any(check_rows)) return(invalid)

  invalid[check_rows] <- !mapply(
    function(lbl, lvl) lbl %in% valid_labels[[lvl]],
    df$linked_label[check_rows], df$linked_level[check_rows]
  )
  invalid
}

#' @noRd
.report_resource_status <- function(res) {
  n_ignore <- sum(res$status == "ignore")
  n_review <- sum(res$status == "review")
  n_ok     <- sum(res$status == "ok")

  if (n_ignore > 0) {
    cli::cli_inform(c("i" = "{n_ignore} junk file{?s} will be ignored, consider deleting them from the directory."))
  }
  if (n_review > 0) {
    review <- res[res$status == "review", ]
    cli::cli_warn(c(
      "!" = "{n_review} resource{?s} need review before this dataset can be zipped for submission:",
      cli_truncated_list(paste0(review$resource_name, ": ", review$note))
    ))
  } else {
    cli::cli_inform(c("v" = "{n_ok} resource{?s} OK ({n_ignore+n_ok} total), dir ready to zip for submission."))
  }
  invisible(NULL)
}

