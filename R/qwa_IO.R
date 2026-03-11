# QWAmetadata I/O --------------------------------------------------------------

#' Write a QWAmetadata object to a JSON file
#'
#' @param x A [QWAmetadata] object.
#' @param file Path to the output `.json` file.
#' @param compress If `TRUE` (default), write `.json.gz` file.
#' @param overwrite Allow to overwrite existing files? (default `FALSE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAmetadata()]
#' @export
write_QWAmetadata <- function(x, file, compress = TRUE, overwrite = FALSE) {
  checkmate::assert_class(x, "QWAmetadata")
  ext <- if (compress) ".json.gz" else ".json"
  file_fixed <- paste0(sub("\\.json(\\.gz)?$", "", file), ext)
  if (file_fixed != file) {
    cli::cli_warn("Adjusted {.arg file} extension to {.val {ext}}: {.file {file_fixed}}")
    file <- file_fixed
  }
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  if (compress){
    write(
      jsonlite::toJSON(x, na = "null", POSIXt = "ISO8601"),
      gzfile(file)
    )
  } else {
    jsonlite::write_json(x, file, na = "null", POSIXt = "ISO8601", pretty = TRUE)
  }
  
  cli::cli_inform(c("v" = "QWAmetadata written to {.file {file}}"))
  invisible(file)
}


#' Read a QWAmetadata object from a JSON file
#'
#' @param file Path to a `.json` metadata file. 
#' @param warn_only If `TRUE` (default), schema validation issues raise warnings
#'   rather than errors.
#' @returns A [QWAmetadata] object.
#' @seealso [write_QWAmetadata()]
#' @export
read_QWAmetadata <- function(file, warn_only = TRUE) {
  checkmate::assert_file_exists(file)
  raw <- jsonlite::read_json(file, simplifyVector = TRUE)
  # NULL components are serialised as [] and read back as list(); restore to NULL
  df_or_null <- function(x) if (length(x) == 0) NULL else x

  meta <- new_QWAmetadata(
    dataset      = df_or_null(raw$dataset),
    authors      = df_or_null(raw$authors),
    funding      = df_or_null(raw$funding),
    relresources = df_or_null(raw$relresources),
    sites        = df_or_null(raw$sites),
    trees        = df_or_null(raw$trees),
    woodpieces   = df_or_null(raw$woodpieces),
    slides       = df_or_null(raw$slides),
    images       = raw$images %||% data.frame()
  )
  # TODO: validate all the available components
  validate_schema(meta$images, "images", warn_only = warn_only)
  cli::cli_inform(c("v" = "QWAmetadata read from {.file {file}}"))
  meta
}


# QWAdata I/O ------------------------------------------------------------------

#' Write a QWAdata object to files
#'
#' Cells and rings are written as (compressed) CSV files. If the object carries
#' a `$metadata` slot, the metadata is written as a JSON file alongside.
#'
#' @param x A [QWAdata] object.
#' @param dir Directory to write to. Files are auto-named using `dataset_name`
#'   (or the name from `x$metadata$dataset$ds_name`). Mutually exclusive with
#'   `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit output paths for the cells and rings
#'   CSV files. Both must be provided together. Mutually exclusive with `dir`.
#' @param file_meta Output path for the metadata JSON when using explicit file
#'   paths. Ignored when using `dir` (metadata is auto-named). If `NULL` and
#'   the object has metadata, a warning is issued.
#' @param dataset_name Name prefix for auto-generated filenames when using
#'   `dir`. Falls back to `x$metadata$dataset$ds_name` then `"QWAdata"`.
#' @param compress If `TRUE` (default), write `.csv.gz` files.
#' @param overwrite Allow to overwrite existing files? (default `FALSE`).
#' @returns A named list of written file paths, invisibly.
#' @seealso [read_QWAdata()]
#' @export
write_QWAdata <- function(x, dir = NULL, file_cells = NULL, file_rings = NULL,
                          file_meta = NULL, dataset_name = NULL,
                          compress = TRUE, overwrite = FALSE) {
  checkmate::assert_class(x, "QWAdata")
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }

  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    prefix <- dataset_name %||% x$metadata$dataset$ds_name[1] %||% "QWAdata"
    prefix <- gsub("[^[:alnum:]_-]", "_", prefix)
    ext <- if (compress) ".csv.gz" else ".csv"
    file_cells <- file.path(dir, paste0(prefix, "_cells", ext))
    file_rings <- file.path(dir, paste0(prefix, "_rings", ext))
    if (!is.null(x$metadata))
      file_meta <- file.path(dir, paste0(prefix, "_metadata.json"))
  } else {
    if (!is.null(x$metadata) && is.null(file_meta))
      cli::cli_warn(c("!" = "Object has {.var $metadata} but {.arg file_meta} was not provided - metadata not written."))
    ext <- if (compress) ".csv.gz" else ".csv"
    fix_ext <- function(path) paste0(sub("\\.csv(\\.gz)?$", "", path), ext)
    file_cells_fixed <- fix_ext(file_cells)
    file_rings_fixed <- fix_ext(file_rings)
    if (file_cells_fixed != file_cells) {
      cli::cli_warn("Adjusted {.arg file_cells} extension to {.val {ext}}: {.file {file_cells_fixed}}")
      file_cells <- file_cells_fixed
    }
    if (file_rings_fixed != file_rings) {
      cli::cli_warn("Adjusted {.arg file_rings} extension to {.val {ext}}: {.file {file_rings_fixed}}")
      file_rings <- file_rings_fixed
    }
  }
  checkmate::assert_path_for_output(file_cells, overwrite = overwrite)
  checkmate::assert_path_for_output(file_rings, overwrite = overwrite)

  vroom::vroom_write(x$cells, file_cells, delim = ",")
  vroom::vroom_write(x$rings, file_rings, delim = ",")

  cli::cli_inform(c(
    "v" = "Cells written to {.file {file_cells}}",
    "v" = "Rings written to {.file {file_rings}}"
  ))

  if (!is.null(x$metadata) && !is.null(file_meta))
    write_QWAmetadata(x$metadata, file = file_meta, overwrite = overwrite)

  invisible(list(file_cells = file_cells, file_rings = file_rings, file_meta = file_meta))
}


#' Read a QWAdata object from CSV files
#'
#' Reads cells and rings from (compressed) CSV files and optionally attaches a
#' [QWAmetadata] object if a matching JSON file is found or provided.
#'
#' @param dir Directory to search for cells, rings, and (optionally) metadata
#'   files. Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit paths to the cells and rings CSV files.
#'   Both must be provided together. Mutually exclusive with `dir`.
#' @param file_meta Optional path to a metadata JSON file. If `NULL` and `dir`
#'   is used, a matching metadata file is auto-detected.
#' @param dataset_name Optional string to disambiguate when multiple matching
#'   files are found in `dir`.
#' @param warn_only Passed to [read_QWAmetadata()] for schema validation.
#' @returns A [QWAdata] object, with `$metadata` attached if a metadata file
#'   was found/provided.
#' @seealso [write_QWAdata()]
#' @export
read_QWAdata <- function(dir = NULL, file_cells = NULL, file_rings = NULL,
                         file_meta = NULL, dataset_name = NULL, warn_only = TRUE) {
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }
  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    all_files <- fs::dir_ls(
      fs::path_abs(dir), 
      type = "file", 
      regexp = "(\\.csv)|(\\.json)"
    )
    csv_files <- grep("\\.csv(\\.gz)?$", all_files, value = TRUE)

    cell_candidates <- grep("cells", csv_files, value = TRUE)
    ring_candidates <- grep("rings", csv_files, value = TRUE)
    if (!is.null(dataset_name)) {
      cell_candidates <- grep(dataset_name, cell_candidates, value = TRUE)
      ring_candidates <- grep(dataset_name, ring_candidates, value = TRUE)
    }
    if (length(cell_candidates) != 1)
      cli::cli_abort("Could not uniquely identify a cells file in {.path {dir}} ({length(cell_candidates)} matches).")
    if (length(ring_candidates) != 1)
      cli::cli_abort("Could not uniquely identify a rings file in {.path {dir}} ({length(ring_candidates)} matches).")

    file_cells <- cell_candidates
    file_rings <- ring_candidates

    if (is.null(file_meta)) {
      meta_candidates <- grep("metadata.*\\.json$", all_files, value = TRUE)
      if (!is.null(dataset_name)) meta_candidates <- grep(dataset_name, meta_candidates, value = TRUE)
      if (length(meta_candidates) == 1) file_meta <- meta_candidates
    }
  }

  # TODO: validate / align to schema instead
  rings <- vroom::vroom(file_rings,
    col_types = vroom::cols(
      .default          = vroom::col_double(),
      tree_label        = vroom::col_character(),
      woodpiece_label   = vroom::col_character(),
      slide_label       = vroom::col_character(),
      image_label       = vroom::col_character(),
      year              = vroom::col_integer(),
      cno               = vroom::col_integer(),
      incomplete_ring   = vroom::col_logical(),
      missing_ring      = vroom::col_logical(),
      duplicate_ring    = vroom::col_logical(),
      exclude_dupl      = vroom::col_logical(),
      exclude_issues    = vroom::col_logical()
    ), show_col_types = FALSE)

  cells <- vroom::vroom(file_cells,
    col_types = vroom::cols(
      .default   = vroom::col_double(),
      image_label = vroom::col_character(),
      year        = vroom::col_integer(),
      xpix        = vroom::col_integer(),
      ypix        = vroom::col_integer(),
      nbrno       = vroom::col_integer(),
      nbrid       = vroom::col_integer(),
      sector100   = vroom::col_integer(),
      ew_lw       = vroom::col_character()
    ), show_col_types = FALSE)

  metadata <- if (!is.null(file_meta)) {
    read_QWAmetadata(file = file_meta, warn_only = warn_only)
  } else NULL

  cli::cli_inform(c(
    "v" = "QWAdata read: {nrow(cells)} cells, {nrow(rings)} rings",
    if (!is.null(metadata)) c("v" = "QWAmetadata attached from {.file {file_meta}}")
  ))
  new_QWAdata(cells = cells, rings = rings, metadata = metadata)
}
