# Helpers ----
# align QWAmetadata components with respective schema
#' @noRd
align_component <- function(data, schema, submission = TRUE) {
  if (is.null(data) || length(data) == 0) return(NULL)

  checkmate::assert_data_frame(data, .var.name = schema)
  data <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)

  data <- align_to_schema(data, schema_tbl_props(schema, submission), schema)

  data
}

# check QWAmetadata components against respective schema; an absent component
# warns if required at the stage, else informs
#' @return `TRUE` if no warning was issued, `FALSE` otherwise (invisibly).
#' @noRd
check_component <- function(df, schema, submission = TRUE, strict = FALSE) {
  if (is.null(df)) {
    main_path <- system.file(schema_rel_path("QWAmetadata", submission), package = "rxs2tria")
    if (schema %in% unlist(jsonlite::read_json(main_path)$required)) {
      return(report_issue(c("!" = "{.field {schema}}: required component not provided."), strict))
    }
    cli::cli_inform(c("i" = "{.field {schema}}: not provided, skipping."))
    return(invisible(TRUE))
  }
  schema_path <- system.file(schema_rel_path(schema, submission), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- schema_tbl_props(schema, submission)
  ok <- c(
    check_schema(df, schema_obj, schema, strict = strict, greedy = FALSE),
    check_unique_cols(df, tbl_props, schema, strict)
  )
  check_missing_opt(df, tbl_props, schema)
  invisible(all(ok))
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

# check the structure of a given QWAmetadata component matches the underlying images structure:
# duplicate IDs abort; missing/extra IDs and values differing from those derived from the
# images warn (error with strict). Derived values that are NA are not compared (only
# species_code, optional in images, e.g. not in the filenames).
#' @noRd
check_hierarchy <- function(df, name, id_col, img, summary_fn, strict = FALSE) {
  check_dupl <- duplicated(df[[id_col]])
  if (any(check_dupl)) {
    dupls <- unique(df[[id_col]][check_dupl])
    cli::cli_abort(c(
      "x" = "{.field {name}}: {length(dupls)} non-unique ID{?s} in {.var {id_col}}: {.val {dupls}}"
    ))
  }

  ok <- TRUE
  hint <- c("i" = "Run {.code update_QWAmetadata(x, x$images)} to resync {.field {name}} with {.field images}.")
  ids_missing <- setdiff(img[[id_col]], df[[id_col]])
  if (length(ids_missing) > 0) {
    ok <- report_issue(c(
      "!" = "{.field {name}}: {length(ids_missing)} ID{?s} of {.field images} missing in {.var {id_col}}: {.val {ids_missing}}",
      hint), strict)
  }
  ids_extra <- setdiff(df[[id_col]], img[[id_col]])
  if (length(ids_extra) > 0) {
    ok <- report_issue(c(
      "!" = "{.field {name}}: {length(ids_extra)} ID{?s} in {.var {id_col}} not in {.field images}: {.val {ids_extra}}",
      hint), strict)
  }

  # values derived from images vs values in df, per column
  df_counts <- summary_fn(img)
  cmp <- dplyr::inner_join(df_counts, df[names(df_counts)], by = id_col, suffix = c("", ".tbl"))
  for (col in setdiff(names(df_counts), id_col)) {
    derived <- cmp[[col]]
    given <- cmp[[paste0(col, ".tbl")]]
    differs <- !is.na(derived) & (is.na(given) | derived != given)
    if (any(differs)) {
      ids <- unique(cmp[[id_col]][differs])
      ok <- report_issue(c(
        "!" = "{.field {name}}: {.var {col}} differs from the value derived from {.field images} for {length(ids)} ID{?s}: {.val {ids}}",
        hint), strict)
    }
  }

  invisible(ok)
}

# exactly one contact person among the authors; NA counts as not selected
#' @noRd
check_contact_person <- function(authors, strict = FALSE) {
  cp <- authors$contact_person
  if (is.null(cp)) return(invisible(TRUE)) # absent table or column, reported by the schema check
  n <- sum(cp %in% TRUE)
  if (n != 1) {
    return(report_issue(c(
      "!" = "{.field authors}: {n} contact person{?s} selected, exactly one expected.",
      "i" = if (anyNA(cp)) "Empty {.field contact_person} values count as not selected."
    ), strict))
  }
  invisible(TRUE)
}

# author_nr numbers the authors from 1 to n
#' @noRd
check_author_nr <- function(authors, strict = FALSE) {
  nr <- authors$author_nr
  if (is.null(nr) || anyNA(nr)) return(invisible(TRUE)) # reported by the schema check
  if (!isTRUE(all(sort(nr) == seq_along(nr)))) {
    return(report_issue(c(
      "!" = "{.field authors}: {.field author_nr} must number the authors from 1 to {length(nr)}, without duplicates or gaps: {.val {nr}}"
    ), strict))
  }
  invisible(TRUE)
}

# lookup tables in inst/extdata, as used by the metadata app (all columns
# character; no NA strings, "NA" is the code of Namibia)
#' @noRd
read_lookup <- function(file) {
  path <- system.file("extdata", file, package = "rxs2tria")
  vroom::vroom(path, delim = ",", na = character(), col_types = vroom::cols(.default = "c"),
               progress = FALSE)
}

# country codes of sites and author affiliations in the ISO 3166-1 alpha-2 list
#' @noRd
check_country_codes <- function(x, strict = FALSE) {
  codes <- read_lookup("country_ISO3166-1-alpha-2_20250513.csv")$country_iso_code
  ok <- TRUE
  for (f in list(c("sites", "country_code"), c("authors", "org_country"))) {
    vals <- x[[f[1]]][[f[2]]]
    unknown <- unique(vals[!is_blank(vals) & !vals %in% codes])
    if (length(unknown) > 0) {
      ok <- report_issue(c(
        "!" = "{.field {f[1]}}: {length(unknown)} unknown country code{?s} in {.field {f[2]}}: {.val {unknown}}",
        "i" = "Use ISO 3166-1 alpha-2 codes."
      ), strict)
    }
  }
  invisible(ok)
}

# species codes in the ITRDB list; for known codes, the species columns of the
# trees (where filled) match the list
#' @noRd
check_species <- function(x, strict = FALSE) {
  lookup <- read_lookup("species_ITRDB_codes_20250701.csv")
  ok <- TRUE

  sc <- x$sites$stand_main_species_code
  unknown <- unique(sc[!is_blank(sc) & !sc %in% lookup$itrdb_species_code])
  if (length(unknown) > 0) {
    ok <- report_issue(c(
      "!" = "{.field sites}: {length(unknown)} unknown species code{?s} in {.field stand_main_species_code}: {.val {unknown}}",
      "i" = "Use ITRDB species codes."
    ), strict)
  }

  trees <- x$trees
  if (is.null(trees$species_code)) return(invisible(ok)) # reported by the schema check
  unknown <- !is_blank(trees$species_code) & !trees$species_code %in% lookup$itrdb_species_code
  if (any(unknown)) {
    ok <- report_issue(c(
      "!" = "{.field trees}: unknown {.field species_code} for {sum(unknown)} tree{?s}:",
      cli_truncated_list(paste0(trees$tree_label[unknown], ": ", trees$species_code[unknown])),
      "i" = "Use ITRDB species codes."
    ), strict)
  }

  ref <- lookup[match(trees$species_code, lookup$itrdb_species_code), ]
  for (col in intersect(c("species_name", "phylogenetic_group", "leaf_habit", "tree_ring_structure"),
                        names(trees))) {
    given <- trees[[col]]
    differs <- !is_blank(ref[[col]]) & !is_blank(given) & given != ref[[col]]
    if (any(differs)) {
      ok <- report_issue(c(
        "!" = "{.field trees}: {.field {col}} does not match {.field species_code} for {sum(differs)} tree{?s}:",
        cli_truncated_list(paste0(trees$tree_label[differs], ": ", given[differs],
                                  " (expected ", ref[[col]][differs], ")")),
        "i" = "The metadata app fills in this column from {.field species_code}."
      ), strict)
    }
  }
  invisible(ok)
}

# ensure component exists (create minimal empty if NULL), expand to include all possible columns (incl. optional)
#' @noRd
complete_tbl <- function(df, schema, img = NULL, summary_fn = NULL) {
  df <- align_to_schema(
    df, schema_tbl_props(schema), schema,
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

# Internal S3 validator ----
#' Abort if a QWAmetadata object violates the class invariants
#'
#' Components data frames or `NULL`; `$images` a valid [QWAimages]; logical
#' `submission` attribute. The hierarchy IDs are not checked (incomplete while
#' the metadata is built up, see `check_hierarchy()`).
#' @returns `x`, invisibly.
#' @noRd
validate_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")
  comps <- setdiff(names(formals(new_QWAmetadata)), c("images", "submission"))
  not_df <- comps[!vapply(comps, \(comp) is.null(x[[comp]]) || is.data.frame(x[[comp]]), logical(1))]
  if (length(not_df) > 0) {
    cli::cli_abort(c("x" = "{.cls QWAmetadata}: component{?s} {.field {not_df}} must be data frames or {.code NULL}."))
  }
  validate_QWAimages(x$images)
  if (!checkmate::test_flag(attr(x, "submission"))) {
    cli::cli_abort(c(
      "x" = "{.cls QWAmetadata}: invalid {.arg submission} attribute.",
      "i" = "Rebuild the object with {.fn QWAmetadata}."
    ))
  }
  invisible(x)
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

  x <- new_QWAmetadata(dataset, authors, funding, related, datafiles, sites, trees,
                       woodpieces, slides, images, submission)
  validate_QWAmetadata(x)
  x
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
#' Displays a compact overview of a [QWAmetadata] object: the stage (submission
#' files, or TRIA dataset files with label, version and citation), image-level
#' coverage (number of images, slides, woodpieces, species, and sites) and the
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

#' Check a QWAmetadata object
#'
#' Checks each component of a [QWAmetadata] object, as the final check of the
#' metadata before a submission. Aborts if the object is not a valid
#' `QWAmetadata` or if IDs in `$sites`, `$trees`, `$woodpieces` or `$slides`
#' are not unique. Warns (or, with `strict = TRUE`, aborts) on:
#' - a missing component required at the object's stage (see [is_submission()]);
#' - values violating the JSON schema of the stage;
#' - duplicate values in columns that must be unique (e.g. `email`, `site_name`);
#' - not exactly one contact person, or `author_nr` not numbering the authors
#'   from 1 to n;
#' - country codes not in ISO 3166-1 alpha-2;
#' - species codes not in the ITRDB list, or species columns of `$trees` not
#'   matching their code;
#' - template placeholders left in the dataset `description`;
#' - IDs or derived values (counts, parent labels, `species_code`) in the
#'   hierarchical tables not matching `$images` (resync with
#'   [update_QWAmetadata()]);
#' - issues in `$images`, see [check_QWAimages()].
#'
#' Also informs about missing optional columns and absent optional components.
#'
#' @param x A [QWAmetadata] object.
#' @param strict If `TRUE`, abort on the first issue instead of warning.
#' @returns `TRUE` if no issue was found, `FALSE` otherwise (invisibly).
#' @seealso [QWAmetadata()], [check_QWAimages()], [check_consistency()]
#' @export
check_QWAmetadata <- function(x, strict = FALSE) {
  validate_QWAmetadata(x)
  checkmate::assert_flag(strict)

  sub <- is_submission(x)
  components <- c("dataset", "authors", "funding", if (sub) "related" else "datafiles",
                  "sites", "trees", "woodpieces", "slides")
  # check against schema, missing optional cols
  ok <- vapply(components, \(comp) check_component(x[[comp]], comp, sub, strict), logical(1))

  # checks beyond the schema, as in the metadata app
  ok <- c(ok,
    check_contact_person(x$authors, strict),
    check_author_nr(x$authors, strict),
    check_country_codes(x, strict),
    check_species(x, strict)
  )
  if (any(grepl("[ADD INFO", x$dataset$description, fixed = TRUE))) {
    ok <- c(ok, report_issue(c(
      "!" = "{.field dataset}: {.field description} still contains {.val [ADD INFO ...]} placeholders of the template."
    ), strict))
  }

  # hierarchy consistency checks against x$images
  for (tbl in names(hierarchy_tbls)) {
    if (is.null(x[[tbl]])) next # absent, reported by check_component()
    h <- hierarchy_tbls[[tbl]]
    ok <- c(ok, check_hierarchy(x[[tbl]], tbl, h$id_col, x$images, h$summary_fn, strict))
  }

  ok <- c(ok, check_QWAimages(x$images, submission = sub, strict = strict))

  if (all(ok)) {
    cli::cli_inform(c("v" = "{.cls QWAmetadata}: all checks passed."))
  } else {
    cli::cli_inform(c("x" = "{.cls QWAmetadata}: issues found, see warnings."))
  }
  invisible(all(ok))
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
  validate_QWAmetadata(x)
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
  validate_QWAmetadata(x)
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