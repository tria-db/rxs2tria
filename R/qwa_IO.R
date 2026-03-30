# QWAimages I/O ----------------------------------------------------------------

#' Write a QWAimages object to a CSV file
#'
#' @param x A [QWAimages] object.
#' @param file Path to the output `.csv` or `.csv.gz` file.
#' @param compress If `TRUE`, write compressed `.csv.gz` file. (default `FALSE`)
#' @param overwrite Allow to overwrite existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAimages()], [QWAimages]
#' @export
write_QWAimages <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAimages")
  ext <- if (compress) ".csv.gz" else ".csv"
  file_fixed <- paste0(sub("\\.csv(\\.gz)?$", "", file), ext)
  if (file_fixed != file)
    cli::cli_warn("Adjusted {.arg file} extension to {.val {ext}}: {.file {file_fixed}}")
  file <- file_fixed
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  vroom::vroom_write(x, file, delim = ",")
  cli::cli_inform(c("v" = "QWAimages written to {.file {file}}"))
  invisible(file)
}


#' Read a QWAimages object from a CSV file
#'
#' Reads image-level metadata from a (compressed) CSV file written by
#' [write_QWAimages()] and returns a [QWAimages] object. The `roxas_version`
#' is inferred from the `software` column of the data.
#'
#' @param file Path to a `.csv` or `.csv.gz` file.
#' @param allow_missing_req If `FALSE` (default), missing required columns raise
#'   an error; if `TRUE`, they raise a warning and are added as `NA`.
#' @param add_missing_opt If `FALSE` (default), missing optional columns raise a
#'   warning only; if `TRUE` they are added as `NA`.
#' @returns A [QWAimages] object.
#' @seealso [write_QWAimages()]
#' @export
read_QWAimages <- function(file, allow_missing_req = FALSE, add_missing_opt = FALSE) {
  checkmate::assert_file_exists(file)

  df <- vroom::vroom(file, show_col_types = FALSE)

  rv <- unique(df$software)
  if (length(rv) != 1 || !(rv %in% c("roxas", "roxas_ai")))
    cli::cli_abort("Could not establish ROXAS software version from {.var software} column.")

  df <- tibble::as_tibble(df, .name_repair = janitor::make_clean_names)
  df <- align_df_to_schema(df, "images", rv, allow_missing_req, add_missing_opt)
  validate_schema(df, "images", rv, warn_only = TRUE, greedy = FALSE)
  check_structure(df)

  cli::cli_inform(c("v" = "QWAimages read from {.file {file}}"))
  new_QWAimages(df, roxas_version = rv)
}

# QWAmetadata I/O --------------------------------------------------------------

#' Write a QWAmetadata object to a JSON file
#'
#' @param x A [QWAmetadata] object.
#' @param file Path to the output `.json` file.
#' @param compress If `TRUE`, write compressed `.json.gz` file. (default `FALSE`).
#' @param overwrite Allow to overwrite existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAmetadata()]
#' @export
write_QWAmetadata <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAmetadata")
  if (compress){
    file_fixed <- fs::path_ext_set(file, ".json.gz")
    if (file_fixed != file){
      cli::cli_warn("Adjusted {.arg file} extension to {.val .json.gz}: {.file {file_fixed}}")
    }
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
#' @param file Path to a `.json` or `.json.gz` file of QWAmetadata.
#' @param roxas_version ROXAS software version (optional). Should usually be inferred
#'  from the data directly, but can be overwritten here (`"roxas"` or `"roxas_ai"``).
#' @param allow_missing_req If `FALSE` (default), missing required columns raise an error; 
#'   if `TRUE`, they raise a warning and are added as `NA`.
#' @param add_missing_opt If `FALSE` (default), missing optional columns raise a warning only;
#'   if `TRUE` they raise a warning and are added as `NA`.
#' @returns A [QWAmetadata] object.
#' @seealso [write_QWAmetadata()]
#' @export
read_QWAmetadata <- function(file, roxas_version = NULL, allow_missing_req = FALSE, add_missing_opt = FALSE) {
  checkmate::assert_file_exists(file)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"), null.ok = TRUE)
  checkmate::assert_logical(allow_missing_req, add_missing_opt)
  
  raw <- jsonlite::read_json(file, simplifyVector = TRUE)

  images <- QWAimages(raw$images, roxas_version) # takes care of images validation

  preprocess_raw_tbl <- function(df, schema){
    if (is.null(df) || length(df) == 0) {
      return(NULL)
    }
    df_prep <- tibble::as_tibble(df, .name_repair = janitor::make_clean_names)
    # try to read roxas_version from data for images table
    df_prep <- align_df_to_schema(df_prep, schema, roxas_version, 
      allow_missing_req, add_missing_opt) 
    validate_schema(df_prep, schema, roxas_version, warn_only = TRUE, greedy = FALSE)
    df_prep
  }

  meta <- new_QWAmetadata(
    dataset = preprocess_raw_tbl(raw$dataset, "dataset"),
    authors = preprocess_raw_tbl(raw$authors, "authors"),
    funding = preprocess_raw_tbl(raw$funding, "funding"),
    related = preprocess_raw_tbl(raw$related, "related"),
    sites = preprocess_raw_tbl(raw$sites, "sites"),
    trees = preprocess_raw_tbl(raw$trees, "trees"),
    woodpieces = preprocess_raw_tbl(raw$woodpieces, "woodpieces"),
    slides = preprocess_raw_tbl(raw$slides, "slides"),
    images = images  # required by def
  )

  # TODO: check that structure across tables image - site works

  extra_components <- setdiff(names(raw), names(meta))
  if (length(extra_components)>0){
    cli::cli_warn(c("i" = "Extra components in the json are ignored."))
  }
  
  cli::cli_inform(c("v" = "QWAmetadata read from {.file {file}}"))
  meta
}


# QWAdata I/O ------------------------------------------------------------------

#' Write a QWAdata object to files
#'
#' Cells and rings are written as (compressed) CSV files.
#'
#' @param x A [QWAdata] object.
#' @param dir Directory to write to. Files are auto-named using `dataset_name`.
#'   Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit output paths for the cells and rings
#'   CSV files. Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Name prefix for auto-generated filenames when using
#'   `dir`. Defaults to `"QWAdata"`.
#' @param compress If `TRUE` (default), write `.csv.gz` files.
#' @param overwrite Allow to overwrite existing files? (default `FALSE`).
#' @returns A named list of written file paths, invisibly.
#' @seealso [read_QWAdata()], [write_QWAprofile()]
#' @export
write_QWAdata <- function(x, dir = NULL,
                          file_cells = NULL, file_rings = NULL,
                          dataset_name = NULL,
                          compress = TRUE, overwrite = FALSE) {
  checkmate::assert_class(x, "QWAdata")
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }

  ext <- if (compress) ".csv.gz" else ".csv"
  fix_ext <- function(path) paste0(sub("\\.csv(\\.gz)?$", "", path), ext)

  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    prefix <- dataset_name %||% "QWAdata"
    prefix <- gsub("[^[:alnum:]_-]", "_", prefix)
    file_cells <- file.path(dir, paste0(prefix, "_cells", ext))
    file_rings <- file.path(dir, paste0(prefix, "_rings", ext))
  } else {
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

  invisible(list(file_cells = file_cells, file_rings = file_rings))
}

#' Read a QWAdata object from CSV files
#'
#' Reads cells and rings from (compressed) CSV files.
#' Use the `components` argument to load only a subset, e.g. to avoid reading
#' a large cells file when only rings are needed.
#'
#' @param dir Directory to search for cells and rings files.
#'   Mutually exclusive with `file_cells`/`file_rings`.
#' @param file_cells,file_rings Explicit paths to the cells and rings CSV files.
#'   Both must be provided together. Mutually exclusive with `dir`.
#' @param dataset_name Optional string to disambiguate when multiple matching
#'   files are found in `dir`.
#' @param components Character vector of components to read. Any subset of
#'   `c("cells", "rings")`. Defaults to `c("cells", "rings")`.
#'   Omitted components are `NULL` in the returned [QWAdata] object.
#' @returns A [QWAdata] object.
#' @seealso [write_QWAdata()], [read_QWAprofile()]
#' @export
read_QWAdata <- function(dir = NULL, file_cells = NULL, file_rings = NULL,
                         dataset_name = NULL,
                         components = c("cells", "rings")) {
  components <- match.arg(components, c("cells", "rings"), several.ok = TRUE)
  use_dir <- !is.null(dir)
  use_files <- !is.null(file_cells) && !is.null(file_rings)
  if (use_dir == use_files) {
    cli::cli_abort(
      "Provide either {.arg dir} or both {.arg file_cells} and {.arg file_rings}.")
  }
  if (use_dir) {
    checkmate::assert_directory_exists(dir)
    csv_files <- fs::dir_ls(fs::path_abs(dir), type = "file",
                            regexp = "\\.csv(\\.gz)?$")

    filter_candidates <- function(pattern) {
      cands <- grep(pattern, csv_files, value = TRUE)
      if (!is.null(dataset_name)) cands <- grep(dataset_name, cands, value = TRUE)
      cands
    }

    if ("cells" %in% components) {
      cell_candidates <- filter_candidates("cells")
      if (length(cell_candidates) != 1)
        cli::cli_abort("Could not uniquely identify a cells file in {.path {dir}} ({length(cell_candidates)} matches).")
      file_cells <- cell_candidates
    }
    if ("rings" %in% components) {
      ring_candidates <- filter_candidates("rings")
      if (length(ring_candidates) != 1)
        cli::cli_abort("Could not uniquely identify a rings file in {.path {dir}} ({length(ring_candidates)} matches).")
      file_rings <- ring_candidates
    }
  }

  rings <- NULL
  cells <- NULL

  if ("rings" %in% components) {
    rings <- vroom::vroom(file_rings, show_col_types = FALSE)
  }

  if ("cells" %in% components) {
    cells <- vroom::vroom(file_cells, show_col_types = FALSE)
  }

  # TODO: validate / align to schema
  # check structures match / can be joined

  msgs <- character(0)
  if (!is.null(cells)) msgs <- c(msgs, "v" = "{nrow(cells)} cells read from {.file {file_cells}}")
  if (!is.null(rings)) msgs <- c(msgs, "v" = "{nrow(rings)} rings read from {.file {file_rings}}")
  if (length(msgs) > 0) cli::cli_inform(msgs)

  new_QWAdata(cells = cells, rings = rings)
}


# QWAprofile I/O ---------------------------------------------------------------

#' Write a QWAprofile object to a CSV file
#'
#' @param x A [QWAprofile] object.
#' @param file Path to the output `.csv` or `.csv.gz` file.
#' @param compress If `TRUE`, write a compressed `.csv.gz` file (default `FALSE`).
#' @param overwrite Allow overwriting existing files? (default `TRUE`).
#' @returns The output file path, invisibly.
#' @seealso [read_QWAprofile()], [QWAprofile()]
#' @export
write_QWAprofile <- function(x, file, compress = FALSE, overwrite = TRUE) {
  checkmate::assert_class(x, "QWAprofile")
  ext <- if (compress) ".csv.gz" else ".csv"
  file_fixed <- paste0(sub("\\.csv(\\.gz)?$", "", file), ext)
  if (file_fixed != file)
    cli::cli_warn("Adjusted {.arg file} extension to {.val {ext}}: {.file {file_fixed}}")
  file <- file_fixed
  checkmate::assert_path_for_output(file, overwrite = overwrite)
  vroom::vroom_write(x, file, delim = ",")
  cli::cli_inform(c("v" = "QWAprofile written to {.file {file}}"))
  
  invisible(file)
}


#' Read a QWAprofile object from a CSV file
#'
#' Reads a profile written by [write_QWAprofile()] and reconstructs the
#' [QWAprofile] object. The `profile_type` is inferred from the available columns.
#'
#' @param file Path to a `.csv` or `.csv.gz` file.
#' @returns A [QWAprofile] object.
#' @seealso [write_QWAprofile()]
#' @export
read_QWAprofile <- function(file) {
  checkmate::assert_file_exists(file)
  df <- vroom::vroom(file, show_col_types = FALSE)

  if ("sector_n" %in% names(df)) {
    pt <- "sector"
  } else if (all(c("start","end") %in% names(df))) {
    pt <- "band"
  } else { 
    cli::cli_abort("Profile type could not be established in {.file {file}}. Is it a {.val QWAprofile}?")
  }

  # TODO: validate profile data
  # - image_label, year available and complete,
  # - valid measurement and count cols
  # - sectors: n sectors no NA, consistent
  # - bands: start, end, bandwith no NA, consistent
  cli::cli_inform(c("v" = "QWAprofile read from {.file {file}}"))
  new_QWAprofile(df, profile_type = pt)
}
