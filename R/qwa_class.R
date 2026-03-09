# Internal low-level constructors -------------------------------------------
# These do minimal type checking and are called by package functions that
# already own their outputs. Users should call the public QWAdata() /
# QWAmetadata() constructors instead.

#' @noRd
new_QWAdata <- function(cells = NULL,
                        rings = NULL,
                        profiles = NULL,
                        metadata = NULL) {
  stopifnot(is.null(cells) || is.data.frame(cells))
  stopifnot(is.null(rings) || is.data.frame(rings))
  stopifnot(is.null(profiles) || inherits(profiles, "QWAprofile"))
  stopifnot(is.null(metadata) || inherits(metadata, "QWAmetadata"))

  structure(
    list(
      cells = cells,
      rings = rings,
      profiles = profiles,
      metadata = metadata
    ),
    class = "QWAdata"
  )
}

#' @noRd
new_QWAmetadata <- function(dataset = NULL,
                            authors = NULL,
                            funding = NULL,
                            relresources = NULL,
                            sites = NULL,
                            trees = NULL,
                            woodpieces = NULL,
                            slides = NULL,
                            images = data.frame()) {
  stopifnot(is.null(dataset) || is.data.frame(dataset))
  stopifnot(is.null(authors) || is.data.frame(authors))
  stopifnot(is.null(funding) || is.data.frame(funding))
  stopifnot(is.null(relresources) || is.data.frame(relresources))
  stopifnot(is.null(sites) || is.data.frame(sites))
  stopifnot(is.null(trees) || is.data.frame(trees))
  stopifnot(is.null(woodpieces) || is.data.frame(woodpieces))
  stopifnot(is.null(slides) || is.data.frame(slides))
  stopifnot(is.data.frame(images))

  structure(
    list(
      dataset = dataset,    # name, description, license
      authors = authors,    # detailed info about the author(s)
      funding = funding,    # funding sources and grants
      relresources = relresources, # related resources (publications, datasets, etc.)
      sites = sites,        # site-level metadata
      trees = trees,        # tree-level metadata
      woodpieces = woodpieces, # woodpiece-level metadata
      slides = slides,      # slide-level metadata
      images = images       # image-level metadata (ROXAS output + EXIF + settings)
    ),
    class = "QWAmetadata"
  )
}

#' @noRd
new_QWAprofile <- function(data, profile_type = c("sector", "band")) {
  profile_type <- match.arg(profile_type)

  structure(
    data,
    class = c("QWAprofile", "data.frame"),
    profile_type = profile_type
  )
}


# Public constructors --------------------------------------------------------

#' Create a QWAprofile object
#'
#' `QWAprofile` is a subclass of `data.frame` that holds radial profiles of QWA
#' measurements across the ring width. Each row represents one position bin,
#' typically computed by [calculate_profiles()].
#'
#' Two profile types are supported, set via the `profile_type` attribute:
#' - **`"sector"`**: bins defined by equal relative position sectors across the
#'   ring width (0–100%).
#' - **`"band"`**: bins defined by fixed-width distance bands in microns.
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
#' @seealso [QWAdata()], [calculate_profiles()], [print.QWAprofile()]
#' @export
QWAprofile <- function(data, profile_type = c("sector", "band")) {
  checkmate::assert_data_frame(data)
  profile_type <- match.arg(profile_type)
  new_QWAprofile(data, profile_type)
}

#' Create a QWAdata object
#'
#' `QWAdata` is the central S3 class of this package. It holds the quantitative
#' wood anatomy (QWA) data produced from ROXAS output files, organised into up
#' to four components:
#'
#' - **`$cells`**: cell-level measurements (one row per cell).
#' - **`$rings`**: ring-level measurements and quality flags (one row per annual
#'   ring per image). Populated progressively: raw measurements are added by
#'   [collect_raw_data()], flags by [validate_QWA_data()].
#' - **`$profiles`**: a [QWAprofile] object with radial profiles computed across
#'   the ring width, e.g. from [calculate_profiles()].
#' - **`$metadata`**: a [QWAmetadata] object with dataset-, site-, tree-,
#'   woodpiece-, slide-, and image-level metadata.
#'
#' At least one component must be provided. The typical workflow builds a
#' `QWAdata` object incrementally using [collect_raw_data()],
#' [remove_outliers()], [complete_cell_measures()], and [validate_QWA_data()].
#'
#' @param cells Data frame with cell-level measurements (optional).
#' @param rings Data frame with ring-level measurements and flags (optional).
#' @param profiles A [QWAprofile] object with computed radial profiles (optional).
#' @param metadata A [QWAmetadata] object (optional).
#'
#' @returns An object of class `QWAdata`.
#'
#' @seealso [QWAmetadata()], [collect_raw_data()], [validate_QWA_data()],
#'   [print.QWAdata()]
#' @export
QWAdata <- function(cells = NULL,
                    rings = NULL,
                    profiles = NULL,
                    metadata = NULL) {

  if (is.null(cells) && is.null(rings) && is.null(profiles) && is.null(metadata)) {
    stop("At least one component must be provided")
  }

  # TODO:
  # - validate individual components
  # e.g. check required columns in cells and rings, check metadata structure, etc.
  # This can be done with helper functions like validate_cells(), validate_rings(), etc.
  # - validate integrity between components

  new_QWAdata(cells, rings, profiles, metadata)
}


#' Create a QWAmetadata object
#'
#' `QWAmetadata` holds the hierarchical metadata associated with a QWA dataset,
#' structured by level of organisation:
#'
#' - **`$dataset`**: dataset-level information (name, description, license).
#' - **`$authors`**: author information.
#' - **`$funding`**: funding sources and grant numbers.
#' - **`$relresources`**: related resources (publications, datasets, etc.).
#' - **`$sites`**: site-level metadata (location, climate, etc.).
#' - **`$trees`**: tree-level metadata (species, DBH, age, etc.).
#' - **`$woodpieces`**: woodpiece-level metadata (disc, core, etc.).
#' - **`$slides`**: slide-level metadata (preparation date, staining, etc.).
#' - **`$images`**: image-level metadata, including the ROXAS output file
#'   paths, EXIF data, and ROXAS settings. This is the only required component
#'   and is populated automatically by [combine_rxs_metadata()].
#'
#' All components except `images` are optional and can be filled in later to
#' prepare a complete dataset submission to the TRIA database.
#'
#' @param dataset Data frame with dataset-level metadata (optional).
#' @param authors Data frame with author information (optional).
#' @param funding Data frame with funding information (optional).
#' @param relresources Data frame with related resources (optional).
#' @param sites Data frame with site-level metadata (optional).
#' @param trees Data frame with tree-level metadata (optional).
#' @param woodpieces Data frame with woodpiece-level metadata (optional).
#' @param slides Data frame with slide-level metadata (optional).
#' @param images Data frame with image-level metadata. Required (defaults to
#'   an empty data frame). Populated automatically by [combine_rxs_metadata()].
#'
#' @returns An object of class `QWAmetadata`.
#'
#' @seealso [QWAdata()], [combine_rxs_metadata()], [print.QWAmetadata()]
#' @export
QWAmetadata <- function(dataset = NULL,
                        authors = NULL,
                        funding = NULL,
                        relresources = NULL,
                        sites = NULL,
                        trees = NULL,
                        woodpieces = NULL,
                        slides = NULL,
                        images = data.frame()) {
  stopifnot(is.data.frame(images))

  # TODO:
  # validation of the components (if present)
  # validation of integrity between components

  new_QWAmetadata(dataset, authors, funding, relresources, sites, trees,
                  woodpieces, slides, images)
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
    " " = fmt_comp("cells",    x$cells),
    " " = fmt_comp("rings",    x$rings),
    " " = fmt_comp("profiles", x$profiles),
    " " = fmt_comp("metadata", x$metadata)
  ))

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
  if (nrow(img) > 0) {
    cli::cli_text("{.strong Images} ({nrow(img)} total)")
    bullets <- character(0)
    if ("woodpiece_label" %in% names(img))
      bullets <- c(bullets, " " = "{length(unique(img$woodpiece_label))} woodpiece{?s}")
    if ("slide_label" %in% names(img))
      bullets <- c(bullets, " " = "{length(unique(img$slide_label))} slide{?s}")
    if ("species_code" %in% names(img) && !all(is.na(img$species_code)))
      bullets <- c(bullets, " " = "species: {.val {unique(stats::na.omit(img$species_code))}}")
    if ("site_label" %in% names(img) && !all(is.na(img$site_label)))
      bullets <- c(bullets, " " = "site{?s}: {.val {unique(stats::na.omit(img$site_label))}}")
    cli::cli_bullets(bullets)
  } else {
    cli::cli_alert_warning("No image-level metadata.")
  }

  # --- Optional components checklist ---
  optional <- c("dataset", "authors", "funding", "relresources",
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


#' Print a QWAprofile object
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
print.QWAprofile <- function(x, ...) {
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

#' @rdname print.QWAprofile
#' @param object A `QWAprofile` object.
#' @export
summary.QWAprofile <- function(object, ...) {
  print(object, ...)
}
