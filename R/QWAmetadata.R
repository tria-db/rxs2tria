# Helpers ----
# align QWAmetadata components with respective schema
#' @noRd
align_component <- function(data, schema) {
  if (is.null(data) || length(data) == 0) return(NULL)
  
  checkmate::assert_data_frame(data, .var.name = schema)
  data <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)
  
  schema_path <- system.file(schema_rel_path(schema), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)

  data <- align_to_schema(data, tbl_props, schema)

  data
}

# check QWAmetadata components against respective schema
#' @noRd
check_component <- function(df, schema) {
    if (is.null(df)) {
      cli::cli_inform(c("i" = "{.field {schema}}: not provided, skipping."))
      return(invisible(NULL))
    }
    schema_path <- system.file(schema_rel_path(schema), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    check_schema(df, schema_obj, schema, warn_only = TRUE, greedy = FALSE)
    check_missing_opt(df, tbl_props, schema)
  }

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
                            resources = NULL,
                            sites = NULL,
                            trees = NULL,
                            woodpieces = NULL,
                            slides = NULL,
                            images = new_QWAimages(data.frame(), "roxas")) {
  checkmate::assert_data_frame(dataset, null.ok = TRUE)
  checkmate::assert_data_frame(authors, null.ok = TRUE)
  checkmate::assert_data_frame(funding, null.ok = TRUE)
  checkmate::assert_data_frame(related, null.ok = TRUE)
  checkmate::assert_data_frame(resources, null.ok = TRUE)
  checkmate::assert_data_frame(sites, null.ok = TRUE)
  checkmate::assert_data_frame(trees, null.ok = TRUE)
  checkmate::assert_data_frame(woodpieces, null.ok = TRUE)
  checkmate::assert_data_frame(slides, null.ok = TRUE)
  checkmate::assert_class(images, "QWAimages")
 
  structure(
    list(
      dataset = dataset,
      authors = authors,
      funding = funding,
      related = related,
      resources = resources,
      sites = sites,
      trees = trees,
      woodpieces = woodpieces,
      slides = slides,
      images = images
    ),
    class = c("QWAmetadata", "list")
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
#' - **`$related`**: related objects (publications, datasets, etc.).
#' - **`$resources`**: included resources (data files)
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
#' @param resources Data frame listing all raw data files to be submitted as
#'   part of the dataset. Typically created by [compile_resources()] or 
#'   [add_resources()].
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
                        resources = NULL,
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

  dataset <- align_component(dataset, "dataset")
  authors <- align_component(authors, "authors")
  funding <- align_component(funding, "funding")
  related <- align_component(related, "related")
  resources <- align_component(resources, "resources")
  sites <- align_component(sites, "sites")
  trees <- align_component(trees, "trees")
  woodpieces <- align_component(woodpieces, "woodpieces")
  slides <- align_component(slides, "slides")

  new_QWAmetadata(dataset, authors, funding, related, resources, sites, trees,
                  woodpieces, slides, images)
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
  optional <- c("dataset", "authors", "funding", "related", "resources",
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
#'
#' @param x A [QWAmetadata] object.
#' @returns `TRUE`, invisibly. Warnings are issued for any problems found;
#'   an error is thrown if a component fails structural validation.
#' @seealso [QWAmetadata()], [check_QWAimages()]
#' @export
check_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")

  components <- c("dataset", "authors", "funding", "related",
                  "resources", "sites", "trees", "woodpieces", "slides")
  for (comp in components) { # check against schema, missing optional cols
    check_component(x[[comp]], comp)
  }

  # hierarchy consistency checks against x$images
  check_hierarchy(x$sites, "sites", "site_label", x$images, function(d) {
    d |>
      dplyr::group_by(site_label) |>
      dplyr::summarise(n_trees = dplyr::n_distinct(tree_label), .groups = "drop")
  })
  check_hierarchy(x$trees, "trees", "tree_label", x$images, function(d) {
    d |>
      dplyr::group_by(site_label, species_code, tree_label) |>
      dplyr::summarise(n_woodpieces = dplyr::n_distinct(woodpiece_label), .groups = "drop")
  })
  check_hierarchy(x$woodpieces, "woodpieces", "woodpiece_label", x$images, function(d) {
    d |>
      dplyr::group_by(tree_label, woodpiece_label) |>
      dplyr::summarise(n_slides = dplyr::n_distinct(slide_label), .groups = "drop")
  })
  check_hierarchy(x$slides, "slides", "slide_label", x$images, function(d) {
    d |>
      dplyr::group_by(woodpiece_label, slide_label) |>
      dplyr::summarise(n_images = dplyr::n_distinct(image_label), .groups = "drop")
  })

  # TODO: add advanced checks: countries, species, contact person, embargo date, license, ...
  # check_resources? -> chema check within, so remove from list above

  check_QWAimages(x$images)

  invisible(TRUE)
}


#' Complete a QWAmetadata object with all optional fields
#'
#' Adds missing optional columns to `$images` and to any already-present
#' optional component tables. For absent hierarchical tables (`$sites`,
#' `$trees`, `$woodpieces`, `$slides`), pre-fills known values from the
#' hierarchy in `$images` (the same logic used by the metadata Shiny app).
#' For absent flat tables (`$dataset`, `$authors`, `$funding`, `$related`),
#' inserts a 0-row skeleton with all schema columns so the user can see the
#' expected fields.
#'
#' Existing tables are never overwritten; only missing optional columns are
#' added to them.
#'
#' @param x A [QWAmetadata] object.
#' @returns A [QWAmetadata] object with all optional columns and component
#'   tables initialized.
#' @seealso [complete_QWAimages()], [QWAmetadata()]
#' @export
complete_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")

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
  sites <- complete_tbl(x$sites, "sites", x$images, function(d) {
    d |>
      dplyr::group_by(site_label) |>
      dplyr::summarise(n_trees = dplyr::n_distinct(tree_label), .groups = "drop")
  })

  trees <- complete_tbl(x$trees, "trees", x$images, function(d) {
    d |>
      dplyr::group_by(site_label, species_code, tree_label) |>
      dplyr::summarise(n_woodpieces = dplyr::n_distinct(woodpiece_label), .groups = "drop")
  })

  woodpieces <- complete_tbl(x$woodpieces, "woodpieces", x$images, function(d) {
    d |>
      dplyr::group_by(tree_label, woodpiece_label) |>
      dplyr::summarise(n_slides = dplyr::n_distinct(slide_label), .groups = "drop")
  })

  slides <- complete_tbl(x$slides, "slides", x$images, function(d) {
    d |>
      dplyr::group_by(woodpiece_label, slide_label) |>
      dplyr::summarise(n_images = dplyr::n_distinct(image_label), .groups = "drop")
  })

  new_QWAmetadata(dataset, authors, funding, related, 
    resources = x$resources, # TODO: not touching resources for now
    sites, trees, woodpieces, slides, images)
}


#' Write a QWAmetadata object to a JSON file
#'
#' @param x A [QWAmetadata] object.
#' @param file Path to the output file.
#' @param compress If `TRUE`, write compressed `.gz` file. (default `FALSE`).
#' @param overwrite Allow to overwrite existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAmetadata()]
#' @export
write_QWAmetadata <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAmetadata")
  # if compress, enforce .gz extension
  if (compress && fs::path_ext(file) != "gz")
    file <- paste0(file, ".gz")

  checkmate::assert_path_for_output(file, overwrite = overwrite)
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
  if ("images" %in% names(raw)){
    if ("dbl_cwt_threshold" %in% names(raw$images)) {
      raw$images <- raw$images |>
        dplyr::rename(cluster_dbl_cwt_threshold = dbl_cwt_threshold)
    }
  }

  # back-compatibility: older files have no ds_title, fall back to ds_name
  if ("dataset" %in% names(raw) && is.null(raw$dataset$ds_title)) {
    raw$dataset$ds_title <- raw$dataset$ds_name
  }

  aligned_data <- as_QWAmetadata(raw)
  cli::cli_inform(c("v" = "QWAmetadata read from {.file {file}}"))
  aligned_data
}