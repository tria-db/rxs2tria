# # Resource type inference and collection

# #' @noRd
# .resource_descriptions <- c(
#   # QWA pipeline outputs
#   QWAdata_cells                  = "QWA cell-level measurements (CSV)",
#   QWAdata_rings                  = "QWA ring-level measurements (CSV)",
#   QWAmetadata                    = "QWA dataset metadata (JSON)",
#   QWAprofile                     = "QWA radial profile data (CSV)",
#   roxas_full_output_summary      = "ROXAS full output summary across all images (XLSX)",
#   # Reference / auxiliary
#   reference_series               = "Reference series / master chronology (RWL)",
#   # Images
#   image_panorama                 = "Panoramic composite image of a woodpiece",
#   image_original                 = "Original scan image",
#   image_preview                  = "Preview image of a slide",
#   image_refseries                = "Reference series scan image",
#   # Classic ROXAS annotated images
#   roxas_image_annotated          = "ROXAS annotated output image",
#   roxas_image_annotated_cells    = "ROXAS annotated image with cells overlay",
#   roxas_image_annotated_twin     = "ROXAS twin-ring annotated image",
#   # Classic ROXAS tabular output
#   roxas_output_cells             = "ROXAS cell output file",
#   roxas_output_rings             = "ROXAS ring output file",
#   roxas_output_xlsx              = "ROXAS per-image Excel output",
#   roxas_output_summary           = "ROXAS per-image output summary",
#   roxas_settings                 = "ROXAS settings file",
#   # Classic ROXAS vector / analysis files
#   roxas_shapefile_vessels        = "ROXAS vessel shapefile (.scl)",
#   roxas_shapefile_vessels_bu     = "ROXAS vessel shapefile backup (.scl)",
#   roxas_shapefile_ringtraces     = "ROXAS ring traces file",
#   roxas_cal                      = "ROXAS calibration file",
#   roxas_AOI                      = "ROXAS area of interest file",
#   roxas_AOE                      = "ROXAS area of exclusion file",
#   roxas_CWT                      = "ROXAS cell wall thickness file",
#   roxas_junkobjects              = "ROXAS junk objects file",
#   roxas_proj                     = "ROXAS project file",
#   # ROXAS AI output files
#   rai_metadata                   = "ROXAS AI metadata (JSON)",
#   rai_cells_table                = "ROXAS AI cell measurements table (CSV)",
#   rai_rings_table                = "ROXAS AI ring measurements table (CSV)",
#   rai_image_cells                = "ROXAS AI image with cells overlay",
#   rai_image_rings                = "ROXAS AI image with ring traces",
#   rai_image_annotated            = "ROXAS AI annotated output image",
#   # Fallback
#   other                          = "Other file"
# )

#' @noRd
.resource_linked_levels <- c(
  QWAdata_cells                  = "dataset",
  QWAdata_rings                  = "dataset",
  QWAmetadata                    = "dataset",
  QWAprofile                     = "dataset",
  roxas_full_output_summary      = "dataset",
  reference_series               = "dataset",
  image_panorama                 = "woodpiece",
  image_original                 = "image",
  image_preview                  = "slide",
  image_refseries                = "image",
  roxas_image_annotated          = "analysis",
  roxas_image_annotated_cells    = "analysis",
  roxas_image_annotated_twin     = "analysis",
  roxas_output_cells             = "analysis",
  roxas_output_rings             = "analysis",
  roxas_output_xlsx              = "analysis",
  roxas_output_summary           = "analysis",
  roxas_settings                 = "analysis",
  roxas_shapefile_vessels        = "analysis",
  roxas_shapefile_ringtraces     = "analysis",
  roxas_cal                      = "analysis",
  roxas_AOI                      = "analysis",
  roxas_AOE                      = "analysis",
  roxas_AOEClass                 = "analysis",
  roxas_CWT                      = "analysis",
  roxas_junkobjects              = "analysis",
  roxas_proj                     = "analysis",
  rai_metadata                   = "analysis",
  rai_cells_table                = "analysis",
  rai_rings_table                = "analysis",
  rai_image_cells                = "analysis",
  rai_image_rings                = "analysis",
  rai_image_annotated            = "analysis",
  other                          = NA_character_
)

#' Infer resource type from a file name
#'
#' Matches file names against known ROXAS and QWA naming patterns and returns
#' one of the resource type strings listed in the table below, or `"other"` for
#' unrecognised files. Patterns are checked most-specific first.
#'
#' | Type | Pattern |
#' |------|---------|
#' | `"QWAdata_cells"` | `*_QWAdata_cells*.csv(.gz)` |
#' | `"QWAdata_rings"` | `*_QWAdata_rings*.csv(.gz)` |
#' | `"QWAmetadata"` | `*.json(.gz)` (not `.metadata.json`) |
#' | `"QWAprofile"` | `*_QWAprofile*.csv(.gz)` |
#' | `"roxas_full_output_summary"` | `*_Output_Summary_ROXAS.xlsx` |
#' | `"reference_series"` | `*.rwl` |
#' | `"image_refseries"` | `*_ReferenceSeries.jpg/jpeg` |
#' | `"image_preview"` | `*_Preview*.jpg/jpeg` |
#' | `"image_panorama"` | `*panorama*.tif/jpg` |
#' | `"image_original"` | `*.jpg/.jpeg/.tif/.tiff` (other) |
#' | `"roxas_image_annotated_cells"` | `*_annotated_cells.jpg` |
#' | `"roxas_image_annotated_twin"` | `*_annotated_twin.jpg` |
#' | `"roxas_image_annotated"` | `*_annotated.jpg` |
#' | `"roxas_output_cells"` | `*_Output_Cells.txt` |
#' | `"roxas_output_rings"` | `*_Output_Rings.txt` |
#' | `"roxas_output_summary"` | `*_Output_Summary.txt` |
#' | `"roxas_output_xlsx"` | `*_Output*.xlsx` |
#' | `"roxas_settings"` | `*_ROXAS_Settings.txt` |
#' | `"roxas_shapefile_ringtraces"` | `*_RingTraces.txt` |
#' | `"roxas_shapefile_vessels"` | `*_Vessels.scl` |
#' | `"roxas_cal"` | `*.cal` |
#' | `"roxas_AOI"` | `*_AOI.out` |
#' | `"roxas_AOE"` | `*_AOE.out` |
#' | `"roxas_CWT"` | `*_CellWallThickness.out` |
#' | `"roxas_junkobjects"` | `*_JunkObjects.scl` |
#' | `"roxas_proj"` | `*_proj.rpf` |
#' | `"rai_metadata"` | `*.metadata.json` |
#' | `"rai_cells_table"` | `*.cells_table.csv` |
#' | `"rai_rings_table"` | `*.rings_table.csv` |
#' | `"rai_image_cells"` | `*.cells.png` |
#' | `"rai_image_rings"` | `*.rings.tif` |
#' | `"rai_image_annotated"` | (set manually; `_annotated.jpg` maps to `roxas_image_annotated`) |
#' | `"other"` | anything else |
#'
#' @param filename Character; one or more file names (basenames or full paths).
#' @returns Character vector of resource type strings, same length as `filename`.
#' @seealso [collect_resources()]
#' @noRd
infer_resource_type <- function(filename) {
  vapply(basename(filename), .infer_one_resource_type, character(1),
         USE.NAMES = FALSE)
}

#' @noRd
.infer_one_resource_type <- function(fn) {
  # --- QWA pipeline outputs (check before generic extensions) ----------------
  if (grepl("_QWAdata_cells.*\\.csv(\\.gz)?$",  fn)) return("QWAdata_cells")
  if (grepl("_QWAdata_rings.*\\.csv(\\.gz)?$",  fn)) return("QWAdata_rings")
  if (grepl("_QWAprofile.*\\.csv(\\.gz)?$",     fn)) return("QWAprofile")

  # --- ROXAS AI tabular output (check .metadata.json BEFORE generic .json) --
  if (grepl("\\.metadata\\.json$",              fn)) return("rai_metadata")
  if (grepl("\\.cells_table\\.csv$",            fn)) return("rai_cells_table")
  if (grepl("\\.rings_table\\.csv$",            fn)) return("rai_rings_table")

  # --- JSON (after rai_metadata) ---------------------------------------------
  if (grepl("\\.json(\\.gz)?$",                 fn)) return("QWAmetadata")

  # --- Classic ROXAS tabular output (most specific .txt/.xlsx first) ---------
  if (grepl("_Output_Cells\\.txt$",             fn)) return("roxas_output_cells")
  if (grepl("_Output_Rings\\.txt$",             fn)) return("roxas_output_rings")
  if (grepl("_Output_Summary\\.txt$",           fn)) return("roxas_output_summary")
  if (grepl("_ROXAS_Settings\\.txt$",           fn, ignore.case = TRUE)) return("roxas_settings")
  if (grepl("_RingTraces\\.txt$",               fn)) return("roxas_shapefile_ringtraces")
  # Dataset-level summary before per-image xlsx
  if (grepl("_Output_Summary_ROXAS\\.xlsx$",    fn)) return("roxas_full_output_summary")
  if (grepl("_Output.*\\.xlsx$",                fn, ignore.case = TRUE)) return("roxas_output_xlsx")

  # --- Classic ROXAS vector / analysis files ---------------------------------
  if (grepl("_Vessels\\.scl$",                  fn)) return("roxas_shapefile_vessels")
  if (grepl("_JunkObjects\\.scl$",              fn)) return("roxas_junkobjects")
  if (grepl("_AOI\\.out$",                      fn)) return("roxas_AOI")
  if (grepl("_AOE\\.out$",                      fn)) return("roxas_AOE")
  if (grepl("_AOEClass\\.txt$",                 fn)) return("roxas_AOEClass")
  if (grepl("_CellWallThickness\\.out$",        fn)) return("roxas_CWT")
  if (grepl("\\.cal$",                          fn, ignore.case = TRUE)) return("roxas_cal")
  if (grepl("_proj\\.rpf$",                  fn, ignore.case = TRUE)) return("roxas_proj")

  # --- ROXAS AI image files --------------------------------------------------
  if (grepl("\\.cells\\.png$",                  fn, ignore.case = TRUE)) return("rai_image_cells")
  if (grepl("\\.rings\\.(tif|tiff)$",           fn, ignore.case = TRUE)) return("rai_image_rings")

  # --- Classic ROXAS image files (annotated variants before plain) -----------
  if (grepl("_annotated_cells\\.(jpg|jpeg)$",   fn, ignore.case = TRUE)) return("roxas_image_annotated_cells")
  if (grepl("_annotated_twin\\.(jpg|jpeg)$",    fn, ignore.case = TRUE)) return("roxas_image_annotated_twin")
  if (grepl("_annotated\\.(jpg|jpeg)$",         fn, ignore.case = TRUE)) return("roxas_image_annotated")
  if (grepl("_ReferenceSeries\\.(jpg|jpeg|gif)$",   fn)) return("image_refseries")
  if (grepl("_Preview.*\\.(jpg|jpeg)$",         fn, ignore.case = TRUE)) return("image_preview")

  # --- Generic image files ---------------------------------------------------
  if (grepl("panorama|pano",                    fn, ignore.case = TRUE) &&
      grepl("\\.(jpg|jpeg|tif|tiff)$",          fn, ignore.case = TRUE)) return("image_panorama")
  if (grepl("\\.(jpg|jpeg|tif|tiff)$",          fn, ignore.case = TRUE)) return("image_original")

  # --- Reference series ------------------------------------------------------
  if (grepl("\\.rwl$",                          fn, ignore.case = TRUE)) return("reference_series")

  "other"
}


#' Collect resource file information from a directory
#'
#' Scans a directory for files and returns a data frame ("resources table")
#' listing each file together with its inferred resource type, the hierarchy
#' level and entity it belongs to, and a checksum and size used for integrity
#' checking during submission. Files can be collected incrementally from
#' several directories by passing the previous result to `append_to`, so a
#' single table can describe processed outputs, raw ROXAS files, and images
#' that live in different folders.
#'
#' This function produces a *manifest* of individual files. It does not move,
#' copy, rename, or compress anything --- the files stay where they are and are
#' only referenced by their path in `fname_resource`. How the listed files are
#' ultimately packaged and stored (e.g. grouped into archives in the TRIA
#' storage bucket) is handled downstream and does not need to be described
#' here.
#'
#' See `vignette("resources")` for a worked example and the complete list of
#' recognised resource types.
#'
#' @details
#' # Resource types
#'
#' The `resource_type` of each file is inferred from its name by matching it
#' against the known ROXAS, ROXAS AI, and QWA naming conventions (for example
#' `*_Output_Cells.txt` becomes `"roxas_output_cells"`, `*.metadata.json`
#' becomes `"rai_metadata"`). Patterns are checked most-specific first. Files
#' that match no known pattern are typed as `"other"` and, unless
#' `include_unmatched = TRUE`, dropped from the result. Known backup and junk
#' files (ROXAS `_bu` backups, `Thumbs.db`, Office lock files) are always
#' excluded. The full pattern table is documented in `vignette("resources")`.
#'
#' # Hierarchy level and linked entity
#'
#' Every resource type has a default `linked_level` describing the level of the
#' data hierarchy the file pertains to: `"dataset"` (applies to the whole
#' submission, e.g. a reference chronology), `"woodpiece"`, `"slide"`,
#' `"image"`, or `"analysis"` (per-image ROXAS analysis files such as
#' shapefiles or annotated images). `linked_label` identifies *which* entity at
#' that level the file belongs to (e.g. a specific `image_label`).
#'
#' `linked_label` is filled automatically for `image`- and `analysis`-level
#' resources when `df_structure` is supplied (see below); all other rows start
#' as `NA` and should be reviewed and completed manually where relevant.
#'
#' # Automatic label matching
#'
#' When `df_structure` (a data structure table from [extract_data_structure()],
#' or the `$images` component of a [QWAmetadata] object) is provided, each
#' unlabelled `image`/`analysis` resource is matched to an image by testing
#' whether its file name starts with an image's base name. The longest matching
#' base name wins, which prevents false matches between labels that share a
#' prefix (e.g. `S22_L1` vs `S22_L10`). Resources that cannot be matched are
#' left as `NA`, and the number of successful matches is reported.
#'
#' @param path Path to a directory to scan for files.
#' @param append_to Optional resources data frame from a previous call to
#'   `collect_resources()`. When provided, the new resources are appended to it
#'   (validated against the resources schema first).
#' @param df_structure Optional data frame with the label columns
#'   `org_img_name` and `image_label` (e.g. from [extract_data_structure()] or
#'   the `$images` component of a [QWAmetadata] object). Used to auto-populate
#'   `linked_label` for `image`- and `analysis`-level resources.
#' @param recursive If `TRUE`, recurse into sub-directories (default `FALSE`).
#' @param include_unmatched If `TRUE`, files that could not be matched to a
#'   specific type are kept with `resource_type = "other"` (default `FALSE`,
#'   i.e. such files are dropped).
#'
#' @returns A [tibble][tibble::tibble] with one row per file and columns:
#'   - `resource_name`: base file name.
#'   - `resource_type`: inferred resource type string (see Details).
#'   - `linked_level`: default hierarchy level for this type (`"dataset"`,
#'     `"woodpiece"`, `"slide"`, `"image"`, or `"analysis"`).
#'   - `linked_label`: label of the linked entity, auto-filled from
#'     `df_structure` where possible, otherwise `NA` (fill in manually).
#'   - `fname_resource`: absolute path to the file on your machine.
#'   - `checksum`: MD5 checksum of the file contents.
#'   - `size_bytes`: file size in bytes.
#'
#' @seealso [add_resources()] to attach the result to a [QWAmetadata] object;
#'   `vignette("resources")` for the full workflow and resource-type table.
#' @examples
#' \dontrun{
#' # Collect the processed outputs, then append raw ROXAS files and images
#' # scanned recursively from a second directory:
#' res <- collect_resources("path/to/output_data")
#' res <- collect_resources("path/to/raw_roxas_files",
#'                          append_to = res, recursive = TRUE)
#'
#' # Auto-fill linked_label for per-image resources using a data structure:
#' res <- collect_resources("path/to/raw_roxas_files",
#'                          df_structure = my_structure, recursive = TRUE)
#' }
#' @export
collect_resources <- function(path, append_to = NULL, df_structure = NULL,
                              recursive = FALSE, include_unmatched = FALSE) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_data_frame(append_to, null.ok = TRUE)
  checkmate::assert_data_frame(df_structure, null.ok = TRUE)
  checkmate::assert_flag(recursive)
  checkmate::assert_flag(include_unmatched)

  schema_path <- system.file(schema_rel_path("resources"), package = "rxs2tria")
  schema_obj  <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema  <- resolve_schema(schema_obj, schema_path)
  tbl_props   <- get_tbl_props(tbl_schema)

  if (!is.null(append_to)) {
    df <- align_to_schema(append_to, tbl_props, "resources")
    check_schema(df, schema_obj, "resources", warn_only = TRUE, greedy = FALSE)
  } else {
    df <- create_empty_df(tbl_props)
  }

  files <- fs::dir_ls(path, recurse = recursive, type = "file")

  # Exclude backup files and known junk files
  exclude <- grepl(
    "_annotated\\.cal$|_Vessels_bu\\.scl$|_Ringtraces_bu\\.txt$|^Thumbs\\.db$|^~\\$",
    fs::path_file(files)
  )
  files <- files[!exclude]

  if (length(files) == 0) {
    cli::cli_alert_info("No files found in {.path {path}}")
  } else {
    types <- infer_resource_type(fs::path_file(files))
    new_res <- tibble::tibble(
      resource_name  = fs::path_file(files),
      resource_type  = types,
      linked_level   = unname(.resource_linked_levels[types]),
      linked_label   = NA_character_,
      fname_resource = as.character(files),
      checksum       = unname(tools::md5sum(files)),
      size_bytes     = as.numeric(fs::file_size(files))
    )
    df <- df |> dplyr::bind_rows(new_res)
    if (!include_unmatched){
      df <- df |> dplyr::filter(resource_type != "other")
    }
  }

  if (!is.null(df_structure)) {
    checkmate::assert_names(names(df_structure),
                            must.include = c("org_img_name", "image_label"))

    to_match <- df$linked_level %in% c("analysis", "image") & is.na(df$linked_label)

    if (any(to_match)) {
      # Strip image extensions from org_img_name to obtain base label pattern.
      # Handles classic ROXAS (.jpg) and ROXAS AI (.scan.jpg).
      img_bases <- sub("(\\.scan)?\\.(jpg|jpeg|png|tif|tiff)$", "",
                       df_structure$org_img_name, ignore.case = TRUE)

      df$linked_label[to_match] <- vapply(
        as.character(df$resource_name[to_match]),
        function(rname) {
          hits <- which(startsWith(rname, img_bases))
          if (length(hits) == 0L) return(NA_character_)
          # Longest match wins to avoid false positives (e.g. "S22_L1" vs "S22_L10")
          best <- hits[which.max(nchar(img_bases[hits]))]
          as.character(df_structure$image_label[best])
        },
        character(1)
      )

      n_matched <- sum(!is.na(df$linked_label[to_match]))
      cli::cli_inform(c("i" = "Matched {n_matched}/{sum(to_match)} resource{?s} to an image label"))
    }
  }

  n_new <- if (exists("new_res", inherits = FALSE)) nrow(new_res) else 0L
  cli::cli_inform(c("v" = "Found {n_new} resource{?s} in {.path {path}}"))
  df
}


#' Add supplementary resource files to a QWAmetadata object
#'
#' Scans a directory and records the files it finds in the `$resources`
#' component of a [QWAmetadata] object. This is the convenient entry point for
#' listing the supplementary files (original and annotated images, raw ROXAS
#' output, reference series, etc.) you wish to submit alongside the required
#' `QWAmetadata` and `QWAdata` files.
#'
#' `add_resources()` is a thin wrapper around [collect_resources()]: it scans
#' `path`, appends the newly found files to any resources already stored in
#' `x$resources`, and uses the object's own `$images` component as the data
#' structure for auto-filling `linked_label` on per-image resources. Call it
#' once per directory to build up the table across several source folders.
#'
#' Only a *listing* of files is produced --- the files themselves are not
#' modified or copied. See [collect_resources()] for the meaning of each column
#' and `vignette("resources")` for the full workflow, including how to review
#' and complete `linked_label` and the complete resource-type table.
#'
#' @param x A [QWAmetadata] object. Its `$images` component, if present, is
#'   used to auto-fill `linked_label` for `image`- and `analysis`-level files.
#' @param path Path to a directory to scan for files.
#' @param recursive If `TRUE`, recurse into sub-directories (default `FALSE`).
#' @param include_unmatched If `TRUE`, files that could not be matched to a
#'   specific type are kept with `resource_type = "other"` (default `FALSE`).
#' @returns The [QWAmetadata] object with the `$resources` component updated.
#' @examples
#' \dontrun{
#' QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
#' QWA_meta <- add_resources(QWA_meta, path = "raw_roxas_files", recursive = TRUE)
#' # review / complete the table, then persist:
#' QWA_meta$resources
#' write_QWAmetadata(QWA_meta, "output_data/my_dataset_QWAmetadata.json")
#' }
#'
#' @seealso [collect_resources()], [QWAmetadata()]
#' @export
add_resources <- function(x, path, recursive = FALSE, include_unmatched = FALSE) {
  checkmate::assert_class(x, "QWAmetadata")
  checkmate::assert_directory_exists(path)
  x$resources <- collect_resources(
    path = path, append_to = x$resources, df_structure = x$images,
    recursive = recursive, include_unmatched = include_unmatched
  )
  x
}

