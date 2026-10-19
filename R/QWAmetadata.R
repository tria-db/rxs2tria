# Helpers ----
# align QWAmetadata components with respective schema
#' @noRd
align_component <- function(data, schema, submission = TRUE) {
  if (is.null(data) || length(data) == 0) return(NULL)

  checkmate::assert_data_frame(data, .var.name = schema)
  data <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)

  schema_path <- system.file(schema_rel_path(schema, submission), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  data <- align_to_schema(data, tbl_props, schema)

  data
}

# check QWAmetadata components against respective schema
#' @noRd
check_component <- function(df, schema, submission = TRUE) {
    if (is.null(df)) {
      cli::cli_inform(c("i" = "{.field {schema}}: not provided, skipping."))
      return(invisible(NULL))
    }
    schema_path <- system.file(schema_rel_path(schema, submission), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    check_schema(df, schema_obj, schema, warn_only = TRUE, greedy = FALSE)
    check_missing_opt(df, tbl_props, schema)
  }

# abort for TRIA dataset files, which are not edited
#' @noRd
assert_submission <- function(x, fn) {
  if (!is_submission(x))
    cli::cli_abort(c("x" = "TRIA dataset files are not edited: {.fn {fn}} applies to submission files only."))
  invisible(TRUE)
}

# hierarchical tables: ID column and counts derived from the images
#' @noRd
hierarchy_tbls <- list(
  sites = list(id_col = "site_label", summary_fn = function(d) {
    d |>
      dplyr::group_by(.data$site_label) |>
      dplyr::summarise(n_trees = dplyr::n_distinct(.data$tree_label), .groups = "drop")
  }),
  trees = list(id_col = "tree_label", summary_fn = function(d) {
    d |>
      dplyr::group_by(.data$site_label, .data$species_code, .data$tree_label) |>
      dplyr::summarise(n_woodpieces = dplyr::n_distinct(.data$woodpiece_label), .groups = "drop")
  }),
  woodpieces = list(id_col = "woodpiece_label", summary_fn = function(d) {
    d |>
      dplyr::group_by(.data$tree_label, .data$woodpiece_label) |>
      dplyr::summarise(n_slides = dplyr::n_distinct(.data$slide_label), .groups = "drop")
  }),
  slides = list(id_col = "slide_label", summary_fn = function(d) {
    d |>
      dplyr::group_by(.data$woodpiece_label, .data$slide_label) |>
      dplyr::summarise(n_images = dplyr::n_distinct(.data$image_label), .groups = "drop")
  })
)

# check the structure of a given QWAmetadata component matches the underlying images structure
#' @noRd
check_hierarchy <- function(df, name, id_col, img, summary_fn) {
  check_dupl <- duplicated(df[[id_col]])
  if (any(check_dupl)) {
    dupls <- unique(df[check_dupl, id_col])
    cli::cli_abort(c(
      "x" = "{length(dupls)} non-unique ID{?s} found for {.var ${id_col}} in {.field {name}}",
      "i" = "Duplicates: {dupls}"
    ))
  }

  check_missing <- setdiff(img[[id_col]], df[[id_col]])
  if (length(check_missing) > 0) {
    cli::cli_abort(c(
      "x" = "{length(check_missing)} missing ID{?s} found for {.var ${id_col}} in {.field {name}}",
      "i" = "Missing: {check_missing}"
    ))
  }
  check_extra <- setdiff(df[[id_col]], img[[id_col]])
  if (length(check_extra) > 0) {
    cli::cli_abort(c(
      "x" = "{length(check_extra)} extra ID{?s} found for {.var ${id_col}} in {.field {name}}",
      "i" = "Extra: {check_extra}"
    ))
  }
  # find IDs where the values from df vs imgs-derived don't match (i.e. distinct rows)
  df_counts <- summary_fn(img)
  check_mismatch <- df[names(df_counts)] |>
    dplyr::bind_rows(df_counts, .id = "source") |>
    dplyr::distinct(dplyr::across(names(df_counts)), .keep_all = TRUE) |>
    dplyr::filter(source == 2) |>
    dplyr::pull(.data[[id_col]])
  if (length(check_mismatch) > 0) {
    hint_cols <- setdiff(names(df_counts), id_col)
    cli::cli_abort(c(
      "x" = "{length(check_mismatch)} ID{?s} with mismatched data found for {.var ${id_col}} in {.field {name}}",
      "i" = "Compare {.field {hint_cols}} in {.field {name}} vs the corresponding values derived from {.field images}",
      "i" = "Mismatches: {check_mismatch}"
    ))
  }

  invisible(TRUE)
}

# ensure component exists (create minimal empty if NULL), expand to include all possible columns (incl. optional)
#' @noRd
complete_tbl <- function(df, schema, img = NULL, summary_fn = NULL) {
  schema_path <- system.file(schema_rel_path(schema), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  df <- align_to_schema(
    df, tbl_props, schema,
    add_opt = TRUE, # !!!
    mute_info = FALSE)

  # for dataset, authors, funding: force at least one row
  if (nrow(df)==0 && schema %in% c("dataset","authors","funding")) {
    df[1,] <- NA 
  }

  # for sites, trees, woodpieces, slides: create structure from imgs if empty
  if (nrow(df)==0 && schema %in% c("sites","trees","woodpieces","slides")) {
    df_empty <- df
    df_counts <- summary_fn(img)
    join_by <- intersect(names(df_counts), names(df_empty))
    df <- dplyr::left_join(df_counts, df_empty, by = join_by)
    df <- df[names(df_empty)]  # ensure schema column order
  }

  df
}

# Internal S3 constructor ----
#' @noRd
new_QWAmetadata <- function(dataset = NULL,
                            authors = NULL,
                            funding = NULL,
                            related = NULL,
                            datafiles = NULL,
                            sites = NULL,
                            trees = NULL,
                            woodpieces = NULL,
                            slides = NULL,
                            images = new_QWAimages(data.frame(), "roxas"),
                            submission = TRUE) {
  checkmate::assert_data_frame(dataset, null.ok = TRUE)
  checkmate::assert_data_frame(authors, null.ok = TRUE)
  checkmate::assert_data_frame(funding, null.ok = TRUE)
  checkmate::assert_data_frame(related, null.ok = TRUE)
  checkmate::assert_data_frame(datafiles, null.ok = TRUE)
  checkmate::assert_data_frame(sites, null.ok = TRUE)
  checkmate::assert_data_frame(trees, null.ok = TRUE)
  checkmate::assert_data_frame(woodpieces, null.ok = TRUE)
  checkmate::assert_data_frame(slides, null.ok = TRUE)
  checkmate::assert_class(images, "QWAimages")
  checkmate::assert_flag(submission)

  structure(
    list(
      dataset = dataset,
      authors = authors,
      funding = funding,
      related = related,
      datafiles = datafiles,
      sites = sites,
      trees = trees,
      woodpieces = woodpieces,
      slides = slides,
      images = images
    ),
    class = c("QWAmetadata", "list"),
    submission = submission
  )
}

# Public constructor -----
#' Create a QWAmetadata object
#'
#' @description
#' `QWAmetadata` holds the hierarchical metadata associated with a QWA dataset,
#' structured by level of organisation:
#'
#' - **`$dataset`**: dataset-level information (name, description, license).
#' - **`$authors`**: author information.
#' - **`$funding`**: funding sources and grant numbers.
#' - **`$related`**: related objects (publications, datasets, etc.). Submission
#'   files only.
#' - **`$datafiles`**: the cells and rings files. TRIA dataset files only.
#' - **`$sites`**: site-level metadata (location, climate, etc.).
#' - **`$trees`**: tree-level metadata (species, DBH, age, etc.).
#' - **`$woodpieces`**: woodpiece-level metadata (disc, core, etc.).
#' - **`$slides`**: slide-level metadata (preparation, staining, etc.).
#' - **`$images`**: image-level metadata, including the ROXAS output file
#'   paths, EXIF data, and ROXAS settings. This is the only required component
#'   and is usually populated automatically by [build_QWAimages()].
#'
#' All components except `images` are optional and can be filled in later to
#' prepare a complete dataset submission to the TRIA database.
#'
#' The `submission` attribute ([is_submission()]) tells submission files from
#' TRIA dataset files. It is inferred from the components: TRIA dataset files
#' have a `$datafiles` component or a `ds_label` in `$dataset`. Components are
#' aligned with the schema of their stage; `$related` is dropped from TRIA
#' dataset files with a warning.
#'
#' [as_QWAmetadata()] safely constructs a [QWAmetadata] object from a named list. 
#' Component names are matched against the [QWAmetadata()] constructor parameters;
#' unknown names are dropped with a warning.
#'
#' @param dataset Data frame with dataset-level metadata. Typically provided via
#'   metadata Shiny app ([launch_metadata_app()]).
#' @param authors Data frame with author information. Typically provided via
#'   metadata Shiny app.
#' @param funding Data frame with funding information. Typically provided via
#'   metadata Shiny app.
#' @param related Data frame with related publications or datasets. Typically 
#'   provided via metadata Shiny app.
#' @param datafiles Data frame listing the cells and rings files of a TRIA
#'   dataset, with size, checksum and S3 key. Written by TRIA at publish time;
#'   leave `NULL` for submission files. (The supplementary CSV built by
#'   [compile_supplementary()] is a separate file, see
#'   `vignette("supplementary")`.)
#' @param sites Data frame with site-level metadata. Typically provided via
#'   metadata Shiny app.
#' @param trees Data frame with tree-level metadata. Typically provided via
#'   metadata Shiny app.
#' @param woodpieces Data frame with woodpiece-level metadata. Typically 
#'   provided via metadata Shiny app.
#' @param slides Data frame with slide-level metadata. Typically provided via
#'   metadata Shiny app.
#' @param images A [QWAimages] object or plain data frame with image-level
#'   metadata. Required. Typically the output of [build_QWAimages()].
#'
#' @returns An object of class `QWAmetadata`.
#'
#' @seealso [QWAdata()], [QWAimages()], [build_QWAimages()], [read_QWAmetadata()]
#' @export
QWAmetadata <- function(dataset = NULL,
                        authors = NULL,
                        funding = NULL,
                        related = NULL,
                        datafiles = NULL,
                        sites = NULL,
                        trees = NULL,
                        woodpieces = NULL,
                        slides = NULL,
                        images = data.frame()) {
  if (is.data.frame(dataset) && identical(images, data.frame()))
    cli::cli_abort(c(
      "A data frame was passed as {.arg dataset} while {.arg images} was not supplied.",
      "i" = "Did you mean {.code QWAmetadata(images = <your data frame>)}?"
    ))

  # images: validation via QWAimages
  images <- QWAimages(images)

  # stage: TRIA dataset files have $datafiles or a ds_label
  submission <- length(datafiles) == 0 && !("ds_label" %in% names(dataset))
  if (!submission && length(related) > 0) {
    cli::cli_warn(c("!" = "{.field related} is not part of TRIA dataset files, dropped."))
    related <- NULL
  }

  dataset <- align_component(dataset, "dataset", submission)
  authors <- align_component(authors, "authors", submission)
  funding <- align_component(funding, "funding", submission)
  related <- align_component(related, "related", submission)
  datafiles <- align_component(datafiles, "datafiles", submission)
  sites <- align_component(sites, "sites", submission)
  trees <- align_component(trees, "trees", submission)
  woodpieces <- align_component(woodpieces, "woodpieces", submission)
  slides <- align_component(slides, "slides", submission)

  new_QWAmetadata(dataset, authors, funding, related, datafiles, sites, trees,
                  woodpieces, slides, images, submission)
}

#' Is a QWAmetadata object from submission files?
#'
#' @param x A [QWAmetadata] object.
#' @returns `TRUE` for submission files, `FALSE` for TRIA dataset files (see
#'   [QWAmetadata()]).
#' @export
is_submission <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")
  attr(x, "submission")
}

# Methods (general and specific) ------
#' Print a QWAmetadata object
#'
#' Displays a compact overview of a [QWAmetadata] object: image-level coverage
#' (number of images, slides, woodpieces, species, and sites) and the
#' availability of the optional metadata components.
#'
#' @param x A `QWAmetadata` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAmetadata()], [summary.QWAmetadata()]
#' @export
print.QWAmetadata <- function(x, ...) {
  cli::cli_h3("{.cls QWAmetadata}")

  sub <- is_submission(x)
  if (sub) {
    cli::cli_text("Submission files")
  } else {
    ds <- x$dataset
    cli::cli_text("TRIA dataset files: {.val {ds$ds_label}} (version {ds$dataset_version})")
    cli::cli_text("Citation: {ds$citation}")
  }

  # images (cf. summary.QWAimages)
  img <- x$images
  rv <- attr(img, "roxas_version")
  n_imgs <- nrow(img)
  n_slides <- length(unique(img$slide_label))
  n_wps <- length(unique(img$woodpiece_label))
  n_trees <- length(unique(img$tree_label))
  sites <- unique(img$site_label)
  species <- unique(img$species_code)
  yrs <- range(img$outmost_year, na.rm = TRUE)
  
  cli::cli_text("{.strong Images}")
  cli::cli_bullets(c(
    "*" = "software: {.val {rv}}",
    "*" = "{n_imgs} images from {n_slides} slides > {n_wps} woodpieces > {n_trees} trees",
    "*" = "{length(sites)} site{?s}: {.val {sites}}",
    "*" = "{length(species)} species: {.val {species}}",
    "*" = "outmost years coverage: {yrs[1]}\u2013{yrs[2]}"
  ))

  # optional components
  optional <- c("dataset", "authors", "funding", if (sub) "related" else "datafiles",
                "sites", "trees", "woodpieces", "slides")
  cli::cli_text("{.strong Other components}")
  for (comp in optional) {
    if (!is.null(x[[comp]])) {
      cli::cli_bullets(c("*" = "{.field {comp}} ({nrow(x[[comp]])} row{?s})"))
    } else {
      cli::cli_bullets(c("*" = "{.emph {comp} (not provided)}"))
    }
  }

  invisible(x)
}

#' @rdname print.QWAmetadata
#' @param object A `QWAmetadata` object.
#' @export
summary.QWAmetadata <- function(object, ...) {
  print(object, ...)
}

#' @rdname QWAmetadata
#' @param x A named list with any subset of the [QWAmetadata()] components.
#' @export
as_QWAmetadata <- function(x) {
  checkmate::assert_list(x, names = "named")
  valid_args <- names(formals(QWAmetadata))
  extra <- setdiff(names(x), valid_args)
  if (length(extra) > 0)
    cli::cli_warn("Ignoring unknown components: {.val {extra}}")
  do.call(QWAmetadata, x[intersect(names(x), valid_args)])
}

#' Validate a QWAmetadata object
#'
#' Runs a series of checks on each component of a [QWAmetadata] object and
#' warns about any issues found. For `$images`, delegates to [check_QWAimages()].
#' For all other components, checks compliance with the corresponding JSON schema
#' and warns about missing optional columns. Absent components are skipped.
#' The schemas are those of the object's stage (see [is_submission()]).
#'
#' @param x A [QWAmetadata] object.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found;
#'   an error is thrown if a component fails structural validation.
#' @seealso [QWAmetadata()], [check_QWAimages()]
#' @export
check_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")

  sub <- is_submission(x)
  components <- c("dataset", "authors", "funding", if (sub) "related" else "datafiles",
                  "sites", "trees", "woodpieces", "slides")
  for (comp in components) { # check against schema, missing optional cols
    check_component(x[[comp]], comp, sub)
  }

  # hierarchy consistency checks against x$images
  for (tbl in names(hierarchy_tbls)) {
    h <- hierarchy_tbls[[tbl]]
    check_hierarchy(x[[tbl]], tbl, h$id_col, x$images, h$summary_fn)
  }

  # TODO: add advanced checks: countries, species, contact person, embargo date, license, ...

  check_QWAimages(x$images, submission = sub)

  invisible(TRUE)
}


#' Complete a QWAmetadata object with all optional fields
#'
#' Adds missing optional columns to `$images` (see [complete_QWAimages()]) and
#' to any already-present optional component tables. For absent hierarchical
#' tables (`$sites`, `$trees`, `$woodpieces`, `$slides`), pre-fills known
#' values from the hierarchy in `$images` (the same logic used by the metadata
#' Shiny app). For absent flat tables, inserts a skeleton with all schema
#' columns so the user can see the expected fields: one empty row for
#' `$dataset`, `$authors` and `$funding`, no rows for `$related`.
#'
#' Existing tables are never overwritten; only missing optional columns are
#' added to them. Submission files only; TRIA dataset files are not edited.
#'
#' @param x A [QWAmetadata] object.
#' @returns A [QWAmetadata] object with all optional columns and component
#'   tables initialized.
#' @seealso [complete_QWAimages()], [QWAmetadata()]
#' @export
complete_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")
  assert_submission(x, "complete_QWAmetadata")

  images <- complete_QWAimages(x$images)

  # create or complete the tables
  dataset <- complete_tbl(x$dataset, "dataset")
  authors <- complete_tbl(x$authors, "authors")
  if (all(is.na(authors$author_nr))) {
    authors$author_nr <- 1:nrow(authors)
  }
  funding <- complete_tbl(x$funding, "funding")
  related <- complete_tbl(x$related, "related")

  # create + prefill or complete hierarchical tables
  hier <- purrr::imap(hierarchy_tbls, \(h, tbl)
    complete_tbl(x[[tbl]], tbl, x$images, h$summary_fn))

  new_QWAmetadata(dataset, authors, funding, related,
    datafiles = x$datafiles, # written by TRIA at publish time, not completed
    hier$sites, hier$trees, hier$woodpieces, hier$slides, images)
}


#' Write a QWAmetadata object to a JSON file
#'
#' Empty values are written as `null`; absent (`NULL`) components are not
#' written.
#'
#' @param x A [QWAmetadata] object.
#' @param file Path to the output file.
#' @param compress If `TRUE`, write compressed `.gz` file. (default `FALSE`).
#' @param overwrite Allow to overwrite existing files? (default `TRUE`).
#' @param drop_empty If `TRUE` (default), optional fields filled in the Shiny
#'   apps (`colType` `"meta_input"` or `"qc_input"`) that are empty in all rows
#'   of a component are not written. Fields required under a condition (e.g.
#'   `embargoed_until` for restricted datasets) are kept where it applies.
#' @returns The output file path, invisibly.
#' @seealso [read_QWAmetadata()]
#' @export
write_QWAmetadata <- function(x, file, compress = FALSE, overwrite = TRUE,
                              drop_empty = TRUE) {
  checkmate::assert_class(x, "QWAmetadata")
  checkmate::assert_flag(drop_empty)
  # if compress, enforce .gz extension
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")

  checkmate::assert_path_for_output(file, overwrite = overwrite)

  # component = schema name
  schemas <- c(dataset = "dataset", authors = "authors", funding = "funding",
               related = "related", datafiles = "datafiles", sites = "sites",
               trees = "trees", woodpieces = "woodpieces", slides = "slides",
               images = attr(x$images, "roxas_version"))
  sub <- is_submission(x)
  x <- unclass(x)
  for (comp in names(schemas)) {
    x[comp] <- list(prepare_write(x[[comp]], schemas[[comp]], drop_empty, sub))
  }
  x <- purrr::compact(x) # absent components are not written

  if (compress){
    write(
      jsonlite::toJSON(x, na = "null", POSIXt = "ISO8601", digits = NA),
      gzfile(file)
    )
  } else {
    jsonlite::write_json(x, file, na = "null", POSIXt = "ISO8601", digits = NA, pretty = TRUE)
  }
  
  cli::cli_inform(c("v" = "QWAmetadata written to {.file {file}}"))
  invisible(file)
}

#' Read a QWAmetadata object from a JSON file
#'
#' @param file Path to a `.json` or `.json.gz` file of QWAmetadata.
#' @returns A [QWAmetadata] object.
#' @seealso [write_QWAmetadata()]
#' @export
read_QWAmetadata <- function(file) {
  checkmate::assert_file_exists(file)
  raw <- jsonlite::read_json(file, simplifyVector = TRUE)

  # renaming backcompatibility
  if (is.data.frame(raw$images)) {
    raw$images <- raw$images |>
      dplyr::rename(dplyr::any_of(c(
        # current_name = 'old_name'
        cluster_dbl_cwt_threshold = 'dbl_cwt_threshold',
        opposite_cwt_ratio_limit = 'maxrel_opp_cwt',
        relwidth_cwt_integration = 'relwidth_cwt_window',
        meas_created_at = 'rxs_created_at',
        img_comment = 'comment'
      )))
  }

  # back-compatibility: older files have no ds_title, fall back to ds_name
  if ("dataset" %in% names(raw) && is.null(raw$dataset$ds_title)) {
    raw$dataset$ds_title <- raw$dataset$ds_name
  }

  aligned_data <- as_QWAmetadata(raw)
  cli::cli_inform(c("v" = "QWAmetadata read from {.file {file}}"))
  aligned_data
}