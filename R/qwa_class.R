# Internal low-level constructors -------------------------------------------
# These do minimal type checking and are called by package functions that
# already own their outputs. Users should call the public QWAdata() /
# QWAmetadata() constructors instead.

#' @noRd
new_QWAimages <- function(data, roxas_version) {
  checkmate::assert_class(data, classes = "data.frame")
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  structure(
    data,
    class = c("QWAimages", class(data)),
    roxas_version = roxas_version
  )
}

#' @noRd
new_QWAmetadata <- function(dataset = NULL,
                            authors = NULL,
                            funding = NULL,
                            related = NULL,
                            sites = NULL,
                            trees = NULL,
                            woodpieces = NULL,
                            slides = NULL,
                            images = new_QWAimages(data.frame(), "roxas")) {
  stopifnot(is.null(dataset) || is.data.frame(dataset))
  stopifnot(is.null(authors) || is.data.frame(authors))
  stopifnot(is.null(funding) || is.data.frame(funding))
  stopifnot(is.null(related) || is.data.frame(related))
  stopifnot(is.null(sites) || is.data.frame(sites))
  stopifnot(is.null(trees) || is.data.frame(trees))
  stopifnot(is.null(woodpieces) || is.data.frame(woodpieces))
  stopifnot(is.null(slides) || is.data.frame(slides))
  stopifnot(is.data.frame(images))
  # auto-promote a plain data.frame to QWAimages
  # TODO: make more strict and fail otherwise?
  if (!inherits(images, "QWAimages")) {
    rv <- if (nrow(images) > 0 && "software" %in% names(images)) {
      unique(images$software)[1]
    } else "roxas"
    images <- new_QWAimages(images, roxas_version = rv)
  }

  structure(
    list(
      dataset = dataset,
      authors = authors,
      funding = funding,
      related = related,
      sites = sites,
      trees = trees, 
      woodpieces = woodpieces, 
      slides = slides, 
      images = images 
    ),
    class = c("QWAmetadata", "list")
  )
}

#' @noRd
new_QWAdata <- function(cells = NULL,
                        rings = NULL,
                        profiles = NULL) {
  stopifnot(is.null(cells) || is.data.frame(cells))
  stopifnot(is.null(rings) || is.data.frame(rings))
  stopifnot(
    is.null(profiles) || (
      is.list(profiles) &&
      # !is.null(names(profiles)) &&
      # all(nchar(names(profiles)) > 0) &&
      all(vapply(profiles, inherits, logical(1), "QWAprofile"))
    )
  )

  structure(
    list(
      cells = cells,
      rings = rings,
      profiles = profiles
    ),
    class = c("QWAdata", "list")
  )
}

#' @noRd
new_QWAprofile <- function(data, profile_type) {
  stopifnot(is.data.frame(data))
  stopifnot(profile_type %in% c("sector","band"))

  structure(
    data,
    class = c("QWAprofile", "data.frame"),
    profile_type = profile_type
  )
}


# Public constructors --------------------------------------------------------
#' Create a QWAimages object
#'
#' `QWAimages` is a subclass of `data.frame` that holds image-level metadata
#' for a QWA dataset. It is the required component of [QWAmetadata] and can be
#' used as input for [collect_raw_data()].
#'
#' Each row represents one image with its associated ROXAS (AI) output file paths,
#' hierarchical labels (site, tree, woodpiece, slide, image), EXIF metadata,
#' and ROXAS (AI) settings. It is typically constructed by [build_QWAimages()].
#'
#' The `roxas_version` attribute records whether the data were produced by
#' classic ROXAS (`"roxas"`) or ROXAS AI (`"roxas_ai"`).
#'
#' @param data A data frame with image-level metadata. Is aligned with the required
#'   structure for QWAimages objects and validated against the corresponding JSON schema. 
#' @param roxas_version The ROXAS software version: `"roxas"` or `"roxas_ai"`.
#'   If `NULL` (default), inferred from the `software` column of `data`.
#'
#' @returns An object of class `c("QWAimages", "data.frame")` with a
#'   `roxas_version` attribute.
#'
#' @seealso [QWAmetadata()], [build_QWAimages()], [print.QWAimages()]
#' @export
QWAimages <- function(data, roxas_version = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(roxas_version, c("roxas", "roxas_ai"), null.ok = TRUE)
  if (is.null(roxas_version)) {
    rv <- unique(data$software)
    if (length(rv) != 1 || !(rv %in% c("roxas", "roxas_ai")))
      cli::cli_abort("Could not infer {.arg roxas_version} from {.var data$software}. Provide it explicitly.")
    roxas_version <- rv
  }
  df <- tibble::as_tibble(data, .name_repair = janitor::make_clean_names)
  df <- align_df_to_schema(df, "images", rv, 
    allow_missing_req = FALSE, add_missing_opt = FALSE)
  validate_schema(df, "images", rv, warn_only = TRUE, greedy = FALSE)
  check_structure(df) # check the labelling hierarchy is valid

  new_QWAimages(df, roxas_version)
}

#' Create a QWAmetadata object
#'
#' @description
#' `QWAmetadata` holds the hierarchical metadata associated with a QWA dataset,
#' structured by level of organisation:
#'
#' - **`$dataset`**: dataset-level information (name, description, license).
#' - **`$authors`**: author information.
#' - **`$funding`**: funding sources and grant numbers.
#' - **`$related`**: related resources (publications, datasets, etc.).
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
#' @param dataset Data frame with dataset-level metadata (optional).
#' @param authors Data frame with author information (optional).
#' @param funding Data frame with funding information (optional).
#' @param related Data frame with related resources (optional).
#' @param sites Data frame with site-level metadata (optional).
#' @param trees Data frame with tree-level metadata (optional).
#' @param woodpieces Data frame with woodpiece-level metadata (optional).
#' @param slides Data frame with slide-level metadata (optional).
#' @param images A [QWAimages] object or plain data frame with image-level
#'   metadata. Required. Typically the output of [build_QWAimages()].
#'
#' @returns An object of class `QWAmetadata`.
#'
#' @seealso [QWAdata()], [QWAdata()], [build_QWAimages()], [print.QWAmetadata()]
#' @export
QWAmetadata <- function(dataset = NULL,
                        authors = NULL,
                        funding = NULL,
                        related = NULL,
                        sites = NULL,
                        trees = NULL,
                        woodpieces = NULL,
                        slides = NULL,
                        images = data.frame()) {
  stopifnot(is.data.frame(images))

  validate_component <- function(df, schema, roxas_version = NULL) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df <- align_df_to_schema(df, schema, roxas_version,
                             allow_missing_req = FALSE, add_missing_opt = FALSE)
    validate_schema(df, schema, roxas_version, warn_only = TRUE, greedy = FALSE)
    df
  }

  # images: validation via QWAimages
  if (!inherits(images, "QWAimages")){
    images <- QWAimages(images)
  }

  dataset <- validate_component(dataset, "dataset")
  authors <- validate_component(authors, "authors")
  funding <- validate_component(funding, "funding")
  related <- validate_component(related, "related")
  sites <- validate_component(sites, "sites")
  trees <- validate_component(trees, "trees")
  woodpieces <- validate_component(woodpieces, "woodpieces")
  slides <- validate_component(slides, "slides")

  new_QWAmetadata(dataset, authors, funding, related, sites, trees,
                  woodpieces, slides, images)
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


#' Complete a QWAimages object with all optional columns
#'
#' Adds any missing optional columns to a [QWAimages] object as `NA`, using
#' the corresponding JSON schema for the detected ROXAS version. Columns that
#' are already present are left unchanged.
#'
#' @param x A [QWAimages] object.
#' @returns A [QWAimages] object with all optional columns present.
#' @seealso [complete_QWAmetadata()], [QWAimages()], [make_schema_skeleton()]
#' @export
complete_QWAimages <- function(x) {
  checkmate::assert_class(x, "QWAimages")
  rv <- attr(x, "roxas_version")
  df <- align_df_to_schema(x, "images", rv,
                           allow_missing_req = FALSE,
                           add_missing_opt   = TRUE,
                           mute_info         = TRUE)
  new_QWAimages(df, rv)
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
#' @seealso [complete_QWAimages()], [QWAmetadata()], [make_schema_skeleton()]
#' @export
complete_QWAmetadata <- function(x) {
  checkmate::assert_class(x, "QWAmetadata")

  images <- complete_QWAimages(x$images)

  # helper: add missing optional columns to an existing table
  complete_tbl <- function(df, schema) {
    nrows <- if (schema == "related") 0 else 1 # initialize 0 rows for optional related
    if (is.null(df)) {
      return(make_schema_skeleton(schema, nrows = nrows))
    } 
    # else complete the existing table
    # TODO: assume valid ok?
    align_df_to_schema(df, schema,
                       allow_missing_req = FALSE,
                       add_missing_opt = TRUE,
                       mute_info = TRUE)
  }

  # helper: pre-fill a hierarchical table from images, then add remaining schema cols
  prefill_tbl <- function(df, schema, img, summary_fn) {
    if (!is.null(df)) {
      # if already present, just add the missing columns
      # TODO: here we assume that structure is ok if component present -> check?
      return(align_df_to_schema(df, schema,
                                allow_missing_req = FALSE,
                                add_missing_opt = TRUE,
                                mute_info = TRUE))
    }
    df_empty <- make_schema_skeleton(schema, nrows = 0)
    df_counts <- summary_fn(img)
    join_by <- intersect(names(df_counts), names(df_empty))
    res <- dplyr::left_join(df_counts, df_empty, by = join_by)
    res[names(df_empty)]  # ensure schema column order
  }

  # create + prefill or complete hierarchical tables
  sites <- prefill_tbl(x$sites, "sites", x$images, function(d) {
    d |>
      dplyr::group_by(site_label) |>
      dplyr::summarise(n_trees = dplyr::n_distinct(tree_label), .groups = "drop")
  })

  trees <- prefill_tbl(x$trees, "trees", x$images, function(d) {
    d |>
      dplyr::group_by(site_label, species_code, tree_label) |>
      dplyr::summarise(n_woodpieces = dplyr::n_distinct(woodpiece_label), .groups = "drop")
  })

  woodpieces <- prefill_tbl(x$woodpieces, "woodpieces", x$images, function(d) {
    d |>
      dplyr::group_by(tree_label, woodpiece_label) |>
      dplyr::summarise(n_slides = dplyr::n_distinct(slide_label), .groups = "drop")
  })

  slides <- prefill_tbl(x$slides, "slides", x$images, function(d) {
    d |>
      dplyr::group_by(woodpiece_label, slide_label) |>
      dplyr::summarise(n_images = dplyr::n_distinct(image_label), .groups = "drop")
  })

  # create or complete the other tables
  dataset <- complete_tbl(x$dataset, "dataset")
  authors <- complete_tbl(x$authors, "authors")
  funding <- complete_tbl(x$funding, "funding")
  related <- complete_tbl(x$related, "related")

  new_QWAmetadata(dataset, authors, funding, related,
                  sites, trees, woodpieces, slides, images)
}


#' Create a QWAdata object
#'
#' The S3 class `QWAdata` holds the quantitative wood anatomy (QWA) measurements
#' data produced from ROXAS output files, organised into up
#' to three components:
#'
#' - **`$cells`**: cell-level measurements (one row per cell).
#' - **`$rings`**: ring-level measurements, and possibly also quality flags 
#'   (one row per annual ring per image).
#' - **`$profiles`**: a list of [QWAprofile] objects with cell measurement profiles
#'   computed along annual rings, with [calculate_sector_profiles()] or
#'   [calculate_band_profiles()].
#'
#' At least one component must be provided. The typical workflow builds a
#' `QWAdata` object incrementally using [collect_raw_data()],
#' [remove_outliers()], [complete_cell_measures()], and [validate_QWA_data()].
#' Metadata is kept separately as a [QWAmetadata] or [QWAimages] object.
#'
#' @param cells Data frame with cell-level measurements (optional).
#' @param rings Data frame with ring-level measurements and flags (optional).
#' @param profiles A named list of [QWAprofile] objects, or a single [QWAprofile]
#'   (auto-wrapped using its `profile_type` as name). Optional.
#'
#' @returns An object of class `QWAdata`.
#'
#' @seealso [QWAmetadata()], [QWAimages()], [collect_raw_data()],
#'   [validate_QWA_data()], [print.QWAdata()]
#' @export
QWAdata <- function(cells = NULL,
                    rings = NULL,
                    profiles = NULL) {

  if (is.null(cells) && is.null(rings) && is.null(profiles)) {
    stop("At least one component must be provided")
  }

  checkmate::assert_data_frame(cells, null.ok = TRUE)
  checkmate::assert_data_frame(rings, null.ok = TRUE)

  # TODO: add cells and rings component validations

  # accept a single QWAprofile for convenience: wrap in named list using profile_type
  if (inherits(profiles, "QWAprofile")) {
    pt <- attr(profiles, "profile_type")
    profiles <- setNames(list(profiles), pt)
  }
  if (!is.null(profiles)) {
    checkmate::assert_list(profiles)
    vapply(profiles, \(x) checkmate::assert_class(x, classes = "QWAprofile",
      .var.name = "profiles_class_checks"),
    logical(1))
  }
  # TODO: just convert to QWAprofile if not already? similar to images in metadata
  # TODO: profile validation checks

  new_QWAdata(cells, rings, profiles)
}

#' Create a QWAprofile object
#'
#' `QWAprofile` is a subclass of `data.frame` that holds profiles of QWA
#' measurements aggregated along the annual rings. Usually elements of the 
#' `$profiles` slot of a `QWAdata` object.
#'
#' Two profile types are supported, set via the `profile_type` attribute:
#' - **`"sector"`**: bins defined by equal relative position sectors across the
#'   ring width (0–100%). [calculate_sector_profiles()]
#' - **`"band"`**: bins defined by fixed-width distance bands in microns.
#'   [calculate_band_profiles()]
#'
#' @param data A data frame containing the profile data. Must include at minimum
#'   columns identifying the image/ring (`image_label`, `year`) and the bin
#'   position.
#' @param profile_type The binning scheme used: `"sector"` (relative position,
#'   default) or `"band"` (fixed-width distance bands).
#'
#' @returns An object of class `c("QWAprofile", "data.frame")` with a
#'   `profile_type` attribute.
#'
#' @seealso [QWAdata], [calculate_sector_profiles()], [calculate_band_profiles()],
#'   [print.QWAprofile()]
#' @export
QWAprofile <- function(data, profile_type = c("sector", "band")) {
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(profile_type, c("sector","band"))
  
  # TODO: validation checks

  new_QWAprofile(data, profile_type)
}


# S3 methods -----------------------------------------------------------------

#' Print a QWAdata object
#'
#' Displays a compact overview of a [QWAdata] object: which components are
#' present, the spatial and temporal coverage of the data, and (if the data
#' have been validated) a summary of ring quality flags.
#'
#' @param x A `QWAdata` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAdata()], [summary.QWAdata()]
#' @export
print.QWAdata <- function(x, ...) {
  cli::cli_h2("<QWAdata>")

  # --- Components ---
  fmt_comp <- function(label, val) {
    if (is.null(val)) {
      paste0(format(label, width = 9), ": {.emph none}")
    } else if (is.data.frame(val)) {
      paste0(format(label, width = 9), ": ", nrow(val), " \u00d7 ", ncol(val))
    } else {
      paste0(format(label, width = 9), ": present")
    }
  }
  cli::cli_text("{.strong Components}")
  cli::cli_bullets(c(
    " " = fmt_comp("cells", x$cells),
    " " = fmt_comp("rings", x$rings)
  ))
  if (is.null(x$profiles) || length(x$profiles) == 0) {
    cli::cli_bullets(c(" " = "profiles  : {.emph none}"))
  } else {
    for (nm in names(x$profiles)) {
      p  <- x$profiles[[nm]]
      pt <- attr(p, "profile_type")
      cli::cli_bullets(c(" " = "profiles [{.val {nm}}]: {.val {pt}}, {nrow(p)} rows"))
    }
  }

  # --- Coverage ---
  ref <- if (!is.null(x$rings)) x$rings else x$cells
  if (!is.null(ref)) {
    cli::cli_text("{.strong Coverage}")
    bullets <- character(0)
    if ("woodpiece_label" %in% names(ref))
      bullets <- c(bullets, " " = "{length(unique(ref$woodpiece_label))} woodpiece{?s}")
    if ("slide_label" %in% names(ref))
      bullets <- c(bullets, " " = "{length(unique(ref$slide_label))} slide{?s}")
    bullets <- c(bullets, " " = "{length(unique(ref$image_label))} image{?s}")
    if ("year" %in% names(ref)) {
      yr <- range(ref$year, na.rm = TRUE)
      bullets <- c(bullets, " " = "years: {yr[1]}\u2013{yr[2]}")
    }
    cli::cli_bullets(bullets)
  }

  # --- Flags (only after validate_QWA_data) ---
  flag_cols <- c("incomplete_ring", "missing_ring", "duplicate_ring")
  if (!is.null(x$rings) && all(flag_cols %in% names(x$rings))) {
    r <- x$rings
    cli::cli_text("{.strong Ring flags}")
    cli::cli_bullets(c(
      " " = "incomplete: {sum(r$incomplete_ring, na.rm = TRUE)}",
      " " = "missing:    {sum(r$missing_ring,    na.rm = TRUE)}",
      " " = "duplicate:  {sum(r$duplicate_ring,  na.rm = TRUE)}"
    ))
  }

  invisible(x)
}

#' @rdname print.QWAdata
#' @param object A `QWAdata` object.
#' @export
summary.QWAdata <- function(object, ...) {
  print(object, ...)
}


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
  cli::cli_h2("<QWAmetadata>")

  # --- Images ---
  img <- x$images
  rv <- toupper(gsub("_", " ", attr(img, "roxas_version")))
  if (nrow(img) > 0) {
    cli::cli_text("{.strong Images} ({nrow(img)} total, analysed with {rv})")
    bullets <- character(0)
    if ("site_label" %in% names(img) && !all(is.na(img$site_label)))
      bullets <- c(bullets, " " = "site{?s}: {.val {unique(stats::na.omit(img$site_label))}}")
    if ("species_code" %in% names(img) && !all(is.na(img$species_code)))
      bullets <- c(bullets, " " = "species: {.val {unique(stats::na.omit(img$species_code))}}")
    if ("tree_label" %in% names(img))
      bullets <- c(bullets, " " = "{length(unique(img$tree_label))} tree{?s}")
    if ("woodpiece_label" %in% names(img))
      bullets <- c(bullets, " " = "{length(unique(img$woodpiece_label))} woodpiece{?s}")
    if ("slide_label" %in% names(img))
      bullets <- c(bullets, " " = "{length(unique(img$slide_label))} slide{?s}")
    cli::cli_bullets(bullets)
  } else {
    cli::cli_alert_warning("No image-level metadata.")
  }

  # --- Optional components checklist ---
  optional <- c("dataset", "authors", "funding", "related",
                "sites", "trees", "woodpieces", "slides")
  cli::cli_text("{.strong Optional components}")
  for (comp in optional) {
    if (!is.null(x[[comp]])) {
      cli::cli_bullets(c("v" = paste0(format(comp, width = 12), " ({nrow(x[[comp]])} rows)")))
    } else {
      cli::cli_bullets(c(" " = paste0("{.emph ", format(comp, width = 12), "} (not provided)")))
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


#' Summary of a QWAprofile object
#'
#' Displays a compact overview of a [QWAprofile] object: the profile type,
#' the number of position bins, and the coverage (images, rings, year range).
#'
#' @param x A `QWAprofile` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAprofile()], [summary.QWAprofile()]
#' @export
summary.QWAprofile <- function(object, ...) {
  profile_type <- attr(x, "profile_type")
  cli::cli_h2("<QWAprofile>")
  cli::cli_bullets(c(
    " " = "type:  {.val {profile_type}}",
    " " = "size:  {nrow(x)} rows \u00d7 {ncol(x)} cols"
  ))
  if ("image_label" %in% names(x)) {
    bullets <- c(" " = "{length(unique(x$image_label))} image{?s}")
    if ("year" %in% names(x)) {
      yr <- range(x$year, na.rm = TRUE)
      bullets <- c(bullets, " " = "years: {yr[1]}\u2013{yr[2]}")
    }
    cli::cli_text("{.strong Coverage}")
    cli::cli_bullets(bullets)
  }
  invisible(x)
}


#' Print summary of a QWAimages object
#'
#' Displays a compact overview of a [QWAimages] object: the ROXAS software
#' version, dimensions, and coverage (woodpieces, slides, images, year range).
#'
#' @param x A `QWAimages` object.
#' @param ... Further arguments (currently unused).
#'
#' @returns `x`, invisibly.
#'
#' @seealso [QWAimages()]
#' @export
summary.QWAimages <- function(object, ...) {
    rv <- attr(x, "roxas_version")
  cli::cli_h2("<QWAimages>")
  cli::cli_bullets(c(
    " " = "software: {.val {rv}}",
    " " = "size:     {nrow(x)} rows \u00d7 {ncol(x)} cols"
  ))
  if ("image_label" %in% names(x)) {
    bullets <- character(0)
    if ("woodpiece_label" %in% names(x))
      bullets <- c(bullets, " " = "{length(unique(x$woodpiece_label))} woodpiece{?s}")
    if ("slide_label" %in% names(x))
      bullets <- c(bullets, " " = "{length(unique(x$slide_label))} slide{?s}")
    bullets <- c(bullets, " " = "{length(unique(x$image_label))} image{?s}")
    if ("outmost_year" %in% names(x)) {
      yr <- range(x$outmost_year, na.rm = TRUE)
      bullets <- c(bullets, " " = "outmost years: {yr[1]}\u2013{yr[2]}")
    }
    cli::cli_text("{.strong Coverage}")
    cli::cli_bullets(bullets)
  }
  invisible(x)
}
