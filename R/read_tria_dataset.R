#' Re-open a TRIA dataset
#'
#' Reads the files of a TRIA dataset --- as downloaded from the TRIA database
#' or written locally by the package's `write_*` functions --- and
#' reconstructs the corresponding rxs2tria objects in a single call. Files are
#' located in `dir` by their names and read via [read_QWAmetadata()],
#' [read_QWAdata()], and [read_QWAprofile()].
#'
#' Only the QWAmetadata `.json` and the two QWAdata `.csv` files (cells and
#' rings) are needed to re-open a dataset; the supplementary files are not
#' required. This makes it possible to download a published dataset and, for
#' example, recompute a [QWAprofile][QWAprofile()] or review the ring flags in
#' the flags app. See `vignette("reopen-dataset")` for the full workflow.
#'
#' @param dir Path to a directory holding the dataset files.
#' @param components Character vector selecting which components to read; any
#'   subset of `c("metadata", "data", "profile")`. Defaults to all three. The
#'   `"profile"` component is optional: if no profile file is present it is
#'   skipped with a message.
#' @param dataset_name Optional string used to disambiguate when several
#'   candidate files are found in `dir` (matched against the file names).
#' @returns A named list containing the requested components that were found:
#'   - `metadata`: a [QWAmetadata] object.
#'   - `data`: a [QWAdata] object (cells and rings).
#'   - `profiles`: a named list of [QWAprofile] objects, named by file (empty
#'     if none were found). A dataset may contain more than one profile (e.g. a
#'     sector and a band profile), hence a list.
#' @seealso [read_QWAmetadata()], [read_QWAdata()], [read_QWAprofile()]
#' @examples
#' \dontrun{
#' ds <- read_tria_dataset("path/to/downloaded_dataset")
#' ds$metadata
#' ds$data
#'
#' # metadata only (e.g. to inspect the images table without loading the CSVs):
#' meta <- read_tria_dataset("path/to/downloaded_dataset",
#'                           components = "metadata")$metadata
#' }
#' @export
read_tria_dataset <- function(dir,
                              components = c("metadata", "data", "profile"),
                              dataset_name = NULL) {
  checkmate::assert_directory_exists(dir)
  components <- match.arg(components, c("metadata", "data", "profile"),
                          several.ok = TRUE)
  checkmate::assert_string(dataset_name, null.ok = TRUE)

  dir <- fs::path_abs(dir)
  out <- list()

  # --- metadata --------------------------------------------------------------
  if ("metadata" %in% components) {
    json_files <- fs::dir_ls(dir, type = "file", regexp = "\\.json(\\.gz)?$")
    # exclude ROXAS AI per-image metadata files (*.metadata.json)
    json_files <- json_files[!grepl("\\.metadata\\.json(\\.gz)?$", json_files)]
    if (!is.null(dataset_name)) {
      json_files <- grep(dataset_name, json_files, value = TRUE)
    }
    # prefer files that look like a QWAmetadata file when several are present
    if (length(json_files) > 1) {
      pref <- grep("QWAmetadata", json_files, value = TRUE)
      if (length(pref) >= 1) json_files <- pref
    }
    if (length(json_files) != 1) {
      cli::cli_abort(c(
        "Could not uniquely identify a QWAmetadata {.file .json} file in {.path {dir}}.",
        "i" = "{length(json_files)} candidate{?s} found; pass {.arg dataset_name} to disambiguate."
      ))
    }
    out$metadata <- read_QWAmetadata(json_files)
  }

  # --- data (cells + rings) --------------------------------------------------
  if ("data" %in% components) {
    out$data <- read_QWAdata(dir = dir, dataset_name = dataset_name)
  }

  # --- profiles (optional) ---------------------------------------------------
  if ("profile" %in% components) {
    prf_files <- fs::dir_ls(dir, type = "file",
                            regexp = "_QWAprofile.*\\.csv(\\.gz)?$")
    if (!is.null(dataset_name)) {
      prf_files <- grep(dataset_name, prf_files, value = TRUE)
    }
    profiles <- lapply(prf_files, read_QWAprofile)
    names(profiles) <- sub("\\.csv(\\.gz)?$", "", fs::path_file(prf_files))
    out$profiles <- profiles
    if (length(profiles) == 0) {
      cli::cli_inform(c("i" = "No QWAprofile file found in {.path {dir}}."))
    }
  }

  cli::cli_inform(c("v" = "TRIA dataset read from {.path {dir}}"))
  out
}
