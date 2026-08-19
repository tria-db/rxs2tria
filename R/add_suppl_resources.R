#' Compile a supplementary resources manifest
#'
#' @description
#' Scans a directory or zip archive for supplementary resource files (e.g.
#' original and annotated images, raw ROXAS outputs, reference RWL series, etc.)
#' and returns a data frame listing each file together with its inferred
#' resource type and the hierarchical entity it belongs to (e.g. the
#' corresponding image_label or woodpiece_label; if applicable).
#'
#' This function is only relevant for TRIA submissions which include supporting
#' materials in addition to the required [QWAdata] and [QWAmetadata] files. It
#' compiles a manifest of the supplementary resources---it does not move, copy,
#' rename, or compress any files. TRIA limits the kind of supplementary files
#' which can be submitted (see `vignette("resources")` for a full list), for any
#' other submitted files the contributor is required to provide a description of
#' what it is and why it is relevant. Thus the purpose of the manifest is to
#' ensure that only the *right* supplementary files are included in the
#' submitted zip, and that these files can all be correctly identified. 
#' 
#' The automatic inference of resource type and linked label leverages the
#' suffix file name conventions used by ROXAS and ROXAS AI plus the original
#' image file names. The results are checked for validity (e.g. if there were
#' files for which the resource type or label could not be inferred), with
#' warnings raised in case of issues. Two additional columns (`status`/`note`)
#' are generated during the validity check and describe how the resource would
#' be treated upon submission (`"ok"`, `"review"`, `"ignore"`) and why.
#' 
#' Inspect the returned data frame and address any raised warnings. You may want to 
#' manually change the contents of the
#' input directory (adding/removing/renaming files), or edit the data frame directly (editing the 
#' values for `resource_type`, `linked_level`, `linked_label` or `description`). 
#' **Do not** edit the derived `status`/`note` columns.
#' After your edits, use [recompile_resources()] to reconcile the updated
#' manifest against the current contents of `path` and re-derive the `status`/`note` 
#' columns without losing any manual edits.
#' 
#' To verify a finalized manifest against a directory or
#' zip archive, ses [check_supplementary()]. Then
#' save the resulting table to a CSV (e.g. `vroom::vroom_write()`) and submit 
#' it alongside the supplementary zip archive.
#'
#' See `vignette("resources")` for a worked example and the complete list of
#' recognised resource types.
#'
#' @param path Path to a directory to scan for files, or to an existing
#'   (non-nested) zip archive to read the file list from directly. Reading a
#'   zip archive requires the \pkg{zip} package, and does not extract any
#'   files---`fname_resource` is `NA` for resources found this way.
#' @param rxs_images A [QWAimages] object. Its `roxas_version` attribute is
#'   used to disambiguate resource-type patterns shared between classic ROXAS
#'   and ROXAS AI, and its `org_img_name`, `image_label`, `slide_label`, and
#'  `woodpiece_label` columns are used to auto-populate `linked_label` where
#'   appropriate.
#'
#' @returns A data frame with one row per file and columns:
#'   - `resource_name`: name and path of the file relative to `path`
#'   - `description`: normally `NA`, can be filled manually if required to
#'      provide justification for "other" supplementary files that should
#'      be included in the upload
#'   - `resource_type`: inferred resource type string (see Details).
#'   - `linked_level`: hierarchy level for this type (e.g. `"dataset"`,
#'     `"woodpiece"`, or `"analysis"`).
#'   - `linked_label`: label of the linked entity, auto-filled from where
#'      possible, otherwise `NA` (fill in manually if required).
#'   - `fname_resource`: absolute path to the file, or `NA` for a resource
#'     read from a zip archive.
#'   - `status`, `note`: status is `"ok"`, `"review"`, or `"ignore"`, note
#'     gives further details.
#'
#'   Returns `NULL` if no files are found in `path`.
#'
#' @seealso [recompile_resources()], [check_supplementary()], `vignette("resources")`
#' @examples
#' \dontrun{
#' suppl_res <- compile_resources("path/to/submission_files", rxs_images = my_images)
#' # review and make changes are required, then persist as a standalone CSV
#' # to submit alongside the resources zip:
#' vroom::vroom_write(suppl_res, "output_data/submission_files_resources.csv")
#' }
#' @export
compile_resources <- function(path, rxs_images) {
  checkmate::assert(
    checkmate::check_directory_exists(path),
    checkmate::check_file_exists(path, extension = "zip")
  )
  checkmate::assert_class(rxs_images, "QWAimages")
  checkmate::assert_names(names(rxs_images),
                          must.include = c("org_img_name", "image_label",
                          "slide_label", "woodpiece_label"))

  new_res <- .scan_resources(path, rxs_images)

  if (nrow(new_res) == 0) {
    cli::cli_alert_info("No files found in {.path {path}}")
    return(NULL)
  }

  .check_resources(new_res, rxs_images)
}

#' Reconcile and re-check a supplementary resources manifest
#'
#' @description
#' Re-scans `path` and reconciles the result against `suppl_res`, an existing
#' manifest (e.g. the output of a previous [compile_resources()]/
#' `recompile_resources()` call, or one read back in from a CSV). Use this
#' after making manual edits and/or adding or removing files in `path` to
#' create a re-checked manifest up-to-date with the contents of `path`.
#'
#' `suppl_res` is treated as the source of truth wherever it overlaps with
#' what's actually found in `path`, see `vignette("resources")` for details.
#'
#' @inheritParams compile_resources
#' @param suppl_res An existing resources data frame.
#' @param add_new_files Logical; if `TRUE`, files found in `path` that
#'   aren't declared in `suppl_res` are added as new, freshly-typed rows
#'   instead of being left out. Default `FALSE`.
#' @returns A data frame, see [compile_resources()]. Aborts instead of
#'   returning if `suppl_res` lists a file with `status == "ok"` that isn't
#'   found in `path`.
#' @seealso [compile_resources()], [check_supplementary()], `vignette("resources")`
#' @examples
#' \dontrun{
#' # re-run after adding more files to path: manual edits on suppl_res survive;
#' # add_new_files = TRUE also picks up the new ones
#' suppl_res <- recompile_resources(suppl_res, "path/to/submission_files",
#'                                  rxs_images = my_images, add_new_files = TRUE)
#' }
#' @export
recompile_resources <- function(suppl_res, path, rxs_images, add_new_files = FALSE) {
  checkmate::assert_data_frame(suppl_res)
  checkmate::assert(
    checkmate::check_directory_exists(path),
    checkmate::check_file_exists(path, extension = "zip")
  )
  checkmate::assert_class(rxs_images, "QWAimages")
  checkmate::assert_names(names(rxs_images),
                          must.include = c("org_img_name", "image_label",
                          "slide_label", "woodpiece_label"))
  checkmate::assert_flag(add_new_files)

  new_res <- .scan_resources(path, rxs_images)

  # add_new_files decides the new manifest's rows for untracked files;
  # everything else about reconciling against the current scan (dropping
  # vanished-but-resolved rows, aborting on vanished-but-"ok" rows) happens
  # in .check_compare_manifests() below
  new_files <- setdiff(new_res$resource_name, suppl_res$resource_name)
  manifest <- if (isTRUE(add_new_files) && length(new_files) > 0) {
    dplyr::bind_rows(suppl_res, new_res[new_res$resource_name %in% new_files, ])
  } else {
    suppl_res
  }

  manifest <- .check_compare_manifests(manifest, new_res)
  .check_resources(manifest, rxs_images)
}

#' Verify a supplementary resources manifest against a directory or archive
#'
#' A pass/fail check confirming that your finalised manifest accurately
#' describes the content of the supplementary archive/directory and is ready
#' for submission.
#' Aborts if `suppl_res` lists a file with `status == "ok"` that isn't found
#' in `path`---a genuine inconsistency between the manifest and the archive.
#' Returns `FALSE` (with a warning pointing you back to
#' [recompile_resources()]) if any resource still needs review, or `TRUE` if
#' everything is ready.
#'
#' @inheritParams compile_resources
#' @param suppl_res An existing resources data frame.
#' @returns `TRUE`/`FALSE` (invisibly), or aborts -- see Description.
#' @seealso [recompile_resources()], [compile_resources()], `vignette("resources")`
#' @examples
#' \dontrun{
#' # verify a finalized manifest against an already-zipped submission
#' suppl_res <- vroom::vroom("output_data/supplementary_files.csv",
#'                           show_col_types = FALSE)
#' if (!check_supplementary(suppl_res, "supplementary_data.zip", rxs_images = my_images)) {
#'   suppl_res <- recompile_resources(suppl_res, "supplementary_data", rxs_images = my_images)
#'   # fix the flagged resource(s) in suppl_res, then check again
#' }
#' }
#' @export
check_supplementary <- function(suppl_res, path, rxs_images) {
  checked <- recompile_resources(suppl_res, path, rxs_images, add_new_files = FALSE)
  # recompile_resources() already aborts if a submission-ready ("ok")
  # resource has gone missing; the only remaining failure is lingering
  # "review" rows, already listed in detail by the call above
  n_review <- sum(checked$status == "review")

  if (n_review > 0) {
    cli::cli_warn(c(
      "!" = "Not ready for submission: {n_review} resource{?s} still {?need/needs} review.",
      "i" = "Fix the manifest {.field suppl_res} or contents under {.field path}, and (re-)compile."
    ))
    return(invisible(FALSE))
  }

  cli::cli_inform(c("v" = "Supplementary resources in {.path {path}} match the manifest, ready for submission."))
  invisible(TRUE)
}

# Helper functions ----

#' Scan a directory or archive and compile a fresh (unchecked) resources table
#' @noRd
.scan_resources <- function(path, rxs_images) {
  roxas_version <- attr(rxs_images, "roxas_version")

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema  <- resolve_schema(schema_obj, schema_path)
  tbl_props   <- get_tbl_props(tbl_schema)

  if (fs::is_dir(path)) {
    # all = TRUE: include hidden files
    files <- fs::dir_ls(path, recurse = TRUE, type = "file", all = TRUE)
    resource_name <- as.character(fs::path_rel(files, start = path)) # name rel to input path
    fname_resource <- as.character(files) # full paths for info
  } else {
    rlang::check_installed("zip", reason = "to read resources directly from a zip archive.")
    entries <- zip::zip_list(path)
    if (any(grepl("\\.zip$", entries$filename, ignore.case = TRUE))) {
      cli::cli_warn("{.path {path}} contains a nested zip archive; its contents cannot be inspected.")
    }
    # directory entries are conventionally stored with a trailing "/"
    if ("type" %in% names(entries)) { # for zip version > 2.3.3 we have type
      resource_name <- entries |>
        dplyr::filter(.data$type == "file") |>
        dplyr::pull(.data$filename)
    } else { # backcomp
      resource_name <- entries |>
        dplyr::filter(!stringr::str_detect(.data$filename, "/$")) |>
        dplyr::pull(.data$filename)
    }
    fname_resource <- rep(NA_character_, length(resource_name)) # no abs filepaths
  }

  if (length(resource_name) == 0) {
    return(create_empty_df(tbl_props))
  }

  new_res <- infer_resource_types(resource_name, roxas_version = roxas_version)
  new_res$fname_resource <- fname_resource
  new_res$linked_label <- NA_character_ # default: unmatched

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
  }

  # ensure we have all schema columns
  create_empty_df(tbl_props) |>
    dplyr::bind_rows(new_res)
}

#' Reconcile an existing manifest against a freshly-scanned one
#'
#' Given `res` (an existing manifest, possibly already merged with untracked
#' new files -- see [recompile_resources()]) and `new_res` (a fresh scan of
#' the same location), aborts if `res` lists a `status == "ok"` file no
#' longer found in `new_res`, silently drops any other `res` row no longer
#' found in `new_res`, and reports (but doesn't act on) files found in
#' `new_res` that `res` doesn't know about and any type/label corrections
#' `res` carries relative to a fresh inference.
#'
#' @param res The manifest to reconcile and return.
#' @param new_res A freshly-scanned manifest (e.g. from `.scan_resources()`)
#'   to reconcile `res` against.
#' @returns `res`, with rows dropped as described above.
#' @noRd
.check_compare_manifests <- function(res, new_res) {
  df_comp <- new_res |> dplyr::full_join(
    res, by = "resource_name", suffix = c("", ".usr"), keep = TRUE
  )

  # found in the fresh scan, not declared in res
  new_files <- df_comp$resource_name[is.na(df_comp$resource_name.usr)]

  # old manifest has different values than derived (assumed usr overwrites)
  usr_overwrites <- df_comp |>
    dplyr::filter(!is.na(.data$resource_name.usr) &
      !is.na(.data$resource_name)) |>
    dplyr::filter(.data$resource_type != .data$resource_type.usr |
      .data$linked_level != .data$linked_level.usr |
      .data$linked_label != .data$linked_label.usr) |> nrow()

  # declared in res, not found in the fresh scan
  vanished <- df_comp |> dplyr::filter(is.na(.data$resource_name))
  # only a problem if it's something we expected to be submitted
  missing_files <- vanished |> dplyr::filter(.data$status.usr == "ok")

  if (nrow(missing_files) > 0) {
    cli::cli_abort(c(
      "Submission-ready resource(s) in {.field suppl_res} not found in {.field path}",
      "i" = "Either remove these {nrow(missing_files)} file{?s} from the table or add them back to the dir/zip:",
      cli_truncated_list(missing_files$resource_name.usr)
    ))
  }

  already_gone <- vanished$resource_name.usr[vanished$status.usr != "ok"]
  res <- res[!res$resource_name %in% already_gone, ]

  if (length(new_files) + usr_overwrites + length(already_gone) > 0) {
    cli::cli_inform(c(
      if (usr_overwrites > 0) c("i" = "{usr_overwrites} resource{?s} have manual corrections in resource table"),
      if (length(already_gone) > 0) c("i" = '{length(already_gone)} file{?s} no longer found in dir/archive (not marked "ok"), dropped from resource table'),
      if (length(new_files) > 0) c("i" = "{length(new_files)} file{?s} found not declared in input resource table, ignored (use {.code recompile_resources()} with {.code add_new_files = TRUE} to add them)")
    ))
  }

  res
}

#' Compute status/note for a resources manifest
#'
#' Annotates every row of `res` with a `status`:
#' - `"ignore"`: a junk file (ROXAS `_bu` backups, `Thumbs.db`, Office lock
#'   files, ...), or any other file manually marked `resource_type = "junk"`
#'   to permanently exclude it without deleting it from `path` (see
#'   [recompile_resources()]). Never part of TRIA upload, so we will ignore
#'   such files.
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
#' @param res A resources data frame.
#' @param rxs_images A [QWAimages] object, used to validate `linked_label`
#'   values.
#' @returns `res` with `status` and `note` (re)computed for every row.
#' @noRd
.check_resources <- function(res, rxs_images) {
  checkmate::assert_data_frame(res)
  checkmate::assert_class(rxs_images, "QWAimages")

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  check_schema(res, schema_obj, "resources", warn_only = TRUE, greedy = FALSE)

  res_meta <- dplyr::left_join(res, .resource_type_meta(), by = "resource_type")
  roxas_version <- attr(rxs_images, "roxas_version")

  is_junk <- res$resource_type == "junk"
  is_generated <- res$resource_type %in% c(
    "QWAdata_cells", "QWAdata_rings", "QWAmetadata", "QWAprofile")

  review_note <- .review_reasons(res, res_meta, roxas_version, rxs_images)

  res$status <- dplyr::case_when(
    is_junk              ~ "ignore",
    nzchar(review_note)    ~ "review",
    TRUE                   ~ "ok"
  )
  res$note <- dplyr::case_when(
    is_junk              ~ "Junk, or manually excluded; always excluded from the archive",
    is_generated         ~ "Generated by rxs2tria",
    nzchar(review_note)  ~ review_note,
    TRUE                 ~ NA_character_
  )

  .report_resource_status(res)

  res
}

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
#' @param filenames Character; one or more file names.
#' @param roxas_version `"roxas"` or `"roxas_ai"`. Used to
#'   disambiguate patterns that are specific to one ROXAS version.
#' @returns A tibble with one row per file: `resource_name`, `fname_resource`,
#'   `resource_type`, `linked_level`.
#' @seealso [compile_resources()]
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

  # create an "other" row for any unmatched filenames
  match_idx[is.na(match_idx)] <- nrow(res_tbl) + 1L
  res_tbl[nrow(res_tbl) + 1L, "resource_type"] <- "other"

  tibble::tibble(
    resource_name  = filenames
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
