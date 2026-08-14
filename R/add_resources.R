#' Compile resources manifest from directory
#'
#' @description
#' Scans a directory (incl. subdirectories), or a zip archive, for
#' supplementary resource files (original and annotated images, raw ROXAS
#' output, reference series, etc.) and returns a data frame listing each file
#' together with its inferred resource type and the hierarchical entity it
#' belongs to (e.g. image_label or woodpiece_label; if available).
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
#' `add_resources()` is a thin wrapper around `compile_resources()`: instead of
#' a [QWAimages] object, it takes an entire a [QWAmetadata] object as input
#' (feeding its `$images` component to `compile_resources()`) and returns
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
#' `"junk"` can also be set manually on a real (non-junk) file you want
#' permanently excluded without deleting it from `path` -- see Reconciling
#' below for why this is preferable to deleting its row from the table.
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
#' While `compile_resources()` aims to recognize the correct `resource_type`
#' and `linked_label` for each file, there may be cases (e.g. special files,
#' renamed files) where one or both values are not inferred correctly. Check
#' the function output carefully, and make the appropriate changes in the
#' input directory (and re-run the function) or in the resources table
#' directly.
#'
#' # Reconciling against an existing table or archive
#'
#' Pass a previous resources table as `res` (or, via `add_resources()`, rely
#' on `x$resources` already being set) to avoid losing manual edits made
#' since it was created, and/or to re-verify an already-packaged archive
#' against it (e.g. a submission zip, before uploading it):
#' - Any file both in `res` and found in `path` keeps its
#'   `resource_type`/`linked_level`/`linked_label`/`description` from `res`
#'   rather than having them re-inferred, so manual edits survive a re-run.
#' - A file `res` lists (with `status` other than `"ignore"`) that is *not*
#'   found in `path` aborts the call -- `res` no longer matches what's
#'   actually there, and either the table or `path` needs fixing before
#'   proceeding.
#' - A file found in `path` that `res` doesn't know about is, by default,
#'   left out of the returned table entirely (just reported as a count) --
#'   `res` is trusted as the complete list. Pass `add_new_files = TRUE` to
#'   add it instead, freshly typed, e.g. after deliberately adding files to
#'   `path` and wanting them picked up.
#'
#' Because an unlisted file is always treated as "not yet seen" rather than
#' "deliberately excluded," deleting a row from `res` does not permanently
#' exclude that file -- it just resets it to unseen, and it reappears (or, by
#' default, is silently ignored again) next time. To permanently exclude a
#' real file without removing it from `path`, set its `resource_type` to
#' `"junk"` instead of deleting its row; that status is stable across
#' re-runs for as long as the file exists in `path`, and only disappears
#' once the file itself is actually removed.
#'
#' @param path Path to a directory to scan for files, or to an existing
#'   (non-nested) zip archive to read the file list from directly. Reading a
#'   zip archive requires the \pkg{zip} package, and does not extract any
#'   files -- `fname_resource` is `NA` for resources found this way.
#' @param rxs_images A [QWAimages] object. Its `roxas_version` attribute is
#'   used to disambiguate resource-type patterns shared between classic ROXAS
#'   and ROXAS AI, and its `org_img_name`, `image_label`, `slide_label`, and
#'  `woodpiece_label` columns are used to auto-populate `linked_label` where
#'   appropriate.
#' @param res An existing resources data frame (e.g. from a previous
#'   `compile_resources()` call), or `NULL` (the default) to build the table
#'   from scratch. When supplied, it is treated as the source of truth: see
#'   Reconciling below.
#' @param add_new_files Logical; if `TRUE`, files found in `path` that
#'   aren't declared in `res` are added as new, freshly-typed rows instead
#'   of being left out. Ignored if `res` is `NULL`. Default `FALSE`.
#'
#' @returns A data frame with one row per file and columns:
#'   - `resource_name`: name and path of the file relative to `path`
#'   - `description`: normally `NA`, can be filled manually if required to 
#'      provide justification for "other" supplementary files that should
#'      be included in the upload
#'   - `resource_type`: inferred resource type string (see Details).
#'   - `linked_level`: hierarchy level for this type (e.g. `"dataset"`,
#'     `"woodpiece"`, or `"image"`).
#'   - `linked_label`: label of the linked entity, auto-filled from where 
#'      possible, otherwise `NA` (fill in manually if required).
#'   - `fname_resource`: absolute path to the file, or `NA` for a resource
#'     read from a zip archive.
#'   - `status`, `note`: computed by [check_resources()], status is `"ok"`,
#'     `"review"`, or `"ignore"`, note gives further details.
#'   For `add_resources()` the input `x`is returned with the resources data
#'   frame in the `x$resources` component.
#'
#'   Returns `NULL` (or, via `add_resources()`, leaves `x$resources` as
#'   `NULL`) if no files are found in `path` and no `res` was supplied.
#'   Aborts instead of returning if `res` lists a file (with `status` other
#'   than `"ignore"`) that isn't found in `path` -- see Reconciling above.
#'
#' @seealso [check_resources()], `vignette("resources")`
#' @examples
#' \dontrun{
#' res <- compile_resources("path/to/submission_files", rxs_images = my_images)
#'
#' # typical workflow with an existing QWAmetadata object
#' QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
#' QWA_meta <- add_resources(QWA_meta, path = "input_data/submission_files")
#' # review and make changes are required, then persist:
#' QWA_meta$resources
#' write_QWAmetadata(QWA_meta, "output_data/my_dataset_QWAmetadata.json")
#'
#' # re-run after adding files -- x$resources is reused automatically, so
#' # manual edits survive; add_new_files = TRUE picks up the new ones too
#' QWA_meta <- add_resources(QWA_meta, path = "input_data/submission_files",
#'                            add_new_files = TRUE)
#'
#' # re-verify an already-zipped submission against its manifest before
#' # upload -- errors if a file the manifest expects is missing from the zip
#' compile_resources("output_data/submission_files.zip",
#'   rxs_images = QWA_meta$images, res = QWA_meta$resources)
#' }
#' @export
compile_resources <- function(path, rxs_images, res = NULL, add_new_files = FALSE) {
  checkmate::assert(
    checkmate::check_directory_exists(path),
    checkmate::check_file_exists(path, extension = "zip")
  )
  checkmate::assert_class(rxs_images, "QWAimages")
  checkmate::assert_names(names(rxs_images),
                          must.include = c("org_img_name", "image_label",
                          "slide_label", "woodpiece_label"))
  checkmate::assert_data_frame(res, null.ok = TRUE)
  checkmate::assert_flag(add_new_files)

  roxas_version <- attr(rxs_images, "roxas_version")

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema  <- resolve_schema(schema_obj, schema_path)
  tbl_props   <- get_tbl_props(tbl_schema)

  if (fs::is_dir(path)) {
    files <- fs::dir_ls(path, recurse = TRUE, type = "file")
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
    cli::cli_alert_info("No files found in {.path {path}}")
    # nothing scanned and no table to reconcile against -- NULL so a next
    # call starts fresh rather than treating an empty table as meaningful
    if (is.null(res)) return(NULL)
    # otherwise fall through so res's "ok"/"review" rows are still checked
    # against this (empty) scan, e.g. to catch an entirely empty archive
    new_res <- create_empty_df(tbl_props)
  } else {
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

      # n_matched <- sum(!is.na(new_res$linked_label[to_match]))
      # cli::cli_inform(c("i" = "Matched {n_matched}/{sum(to_match)} resource{?s} to a data-structure label")) # the check will list unmatched anyway
    }

    # ensure we have all schema columns
    new_res <- create_empty_df(tbl_props) |>
      dplyr::bind_rows(new_res)
  }

  # reconcile the compiled new_res against input res, if given -- see
  # check_resources() for the actual reconciliation logic
  if (!is.null(res)) {
    check_resources(res, rxs_images, new_res = new_res, add_new_files = add_new_files)
  } else {
    check_resources(new_res, rxs_images)
  }
}

#' @rdname compile_resources
#' @param x A [QWAmetadata] object. Its `$images` component is fed to
#'   `compile_resources()` as `rxs_images`, and its `$resources` component
#'   (if any) is fed to `compile_resources()` as `res`, unless
#'   `reset_resources = TRUE`.
#' @param reset_resources Logical; if `TRUE`, ignore `$resources` and rebuild the
#'   table from scratch, as if it were `NULL`. Default `FALSE`.
#' @export
add_resources <- function(x, path, reset_resources = FALSE, add_new_files = FALSE) {
  checkmate::assert_class(x, "QWAmetadata")
  checkmate::assert_flag(reset_resources)
  res <- if (isTRUE(reset_resources)) NULL else x$resources
  x$resources <- compile_resources(path = path, rxs_images = x$images, res = res,
                                    add_new_files = add_new_files)
  x
}

#' Validate a resources manifest and compute status for each file
#'
#' Runs a series of checks on a resources data frame (e.g. `x$resources`, or
#' the output of [compile_resources()]) and annotates every row with a
#' `status`:
#' - `"ignore"`: a junk file (ROXAS `_bu` backups, `Thumbs.db`, Office lock
#'   files, ...), or any other file manually marked `resource_type = "junk"`
#'   to permanently exclude it without deleting it from `path` (see
#'   [compile_resources()]). Never part of TRIA upload, so we will ignore
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
#' [compile_resources()] runs this automatically, call it directly to re-run
#' the checks, e.g. after manually editing a resources table.
#'
#' Pass `new_res` (a freshly-scanned table, e.g. from [compile_resources()]'s
#' own scanning step) to reconcile `res` against it before checking: `res` is
#' treated as ground truth for any file also found in `new_res`, a file
#' `new_res` doesn't know about but `res` lists (with `status` other than
#' `"ignore"`) aborts the call, a `res` row whose file is gone and was
#' already `"ignore"` is dropped quietly (so reporting reflects what's
#' actually there rather than stale junk references), and a file `new_res`
#' finds that `res` doesn't list is left out unless `add_new_files = TRUE`.
#' This is what lets [compile_resources()] point `path` at a zip archive to
#' re-verify a submission is still complete right before uploading it.
#'
#' @param res A resources data frame, treated as ground truth wherever it
#'   overlaps with `new_res` (see above).
#' @param rxs_images A [QWAimages] object, used to validate `linked_label`
#'   values (see [compile_resources()]).
#' @param new_res A freshly-scanned resources data frame to reconcile `res`
#'   against, or `NULL` (the default) to just check `res` as-is.
#' @param add_new_files Logical; if `TRUE`, files in `new_res` not declared
#'   in `res` are added as new rows instead of being left out. Ignored if
#'   `new_res` is `NULL`. Default `FALSE`.
#' @returns `res` (reconciled against `new_res` first, if supplied) with
#'   `status` and `note` (re)computed for every row.
#' @seealso [compile_resources()], [add_resources()]
#' @export
check_resources <- function(res, rxs_images, new_res = NULL, add_new_files = FALSE) {
  checkmate::assert_data_frame(res)
  checkmate::assert_class(rxs_images, "QWAimages")
  checkmate::assert_data_frame(new_res, null.ok = TRUE)
  checkmate::assert_flag(add_new_files)

  if (!is.null(new_res)) {
    df_comp <- new_res |> dplyr::full_join(
      res, by = "resource_name", suffix = c("", ".usr"), keep = TRUE
    )

    # found in the fresh scan, not declared in res
    new_files <- df_comp$resource_name[is.na(df_comp$resource_name.usr)]
    usr_overwrites <- df_comp |>
      dplyr::filter(!is.na(.data$resource_name.usr) &
        !is.na(.data$resource_name)) |>
      dplyr::filter(.data$resource_type != .data$resource_type.usr |
        .data$linked_level != .data$linked_level.usr |
        .data$linked_label != .data$linked_label.usr) |> nrow()

    # declared in res, not found in the fresh scan
    vanished <- df_comp |> dplyr::filter(is.na(.data$resource_name))
    # only a problem if it mattered for submission, junk is ok
    missing_files <- vanished |> dplyr::filter(.data$status.usr != "ignore")

    if (nrow(missing_files) > 0) {
      cli::cli_abort(c(
        "Input resource table contains resources not found in the files just scanned",
        "i" = "Either remove the following {nrow(missing_files)} file{?s} from the table or add them back:",
        cli_truncated_list(missing_files$resource_name.usr)
      ))
    }

    already_gone <- vanished$resource_name.usr[vanished$status.usr == "ignore"]
    res <- res[!res$resource_name %in% already_gone, ]

    # by default, files not declared in res are ignored (res is trusted as
    # the complete list); add_new_files = TRUE merges them in as new,
    # freshly-typed rows instead -- e.g. after adding files to path and
    # wanting them picked up without losing manual edits on the rest
    if (isTRUE(add_new_files) && length(new_files) > 0) {
      res <- dplyr::bind_rows(res, new_res[new_res$resource_name %in% new_files, ])
    }

    if (length(new_files) + usr_overwrites +  length(already_gone) > 0) {
      cli::cli_inform(c(
        if (usr_overwrites > 0) c("i" = "{usr_overwrites} resource{?s} have manual corrections in resource table"),
        if (length(already_gone)> 0) c("i" = '{length(already_gone)} file{?s} with status "ignored" already deleted in dir/archive, dropped from resource table'),
        if (length(new_files) > 0 && !isTRUE(add_new_files))
          c("i" = "{length(new_files)} file{?s} found not declared in input resource table, ignored (rerun {.code compile_resources} with {.code add_new_files = TRUE} to add them)"),
        if (length(new_files) > 0 && isTRUE(add_new_files))
          c("i" = "{length(new_files)} new file{?s} added to the resource table")
      ))
    }
  }

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

