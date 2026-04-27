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
#' @export
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
#' Scans a directory for files and returns a data frame listing each file with
#' its inferred resource type. Optionally appends to an existing resources
#' data frame, making it easy to build a resource list incrementally from
#' multiple directories (e.g., processed outputs + raw ROXAS files + images).
#'
#' Resource types are inferred from file names via [infer_resource_type()].
#' See that function's documentation for the full pattern table.
#'
#' @param path Path to a directory to scan for files.
#' @param append_to Optional resources data frame from a previous call to
#'   `collect_resources()`. When provided, the new resources are appended to it.
#' @param df_structure Optional data frame with label columns (e.g. from
#'   [extract_data_structure()]). Reserved for future use to auto-populate
#'   `linked_label` from matching file names.
#' @param recursive If `TRUE`, recurse into sub-directories (default `FALSE`).
#' @param include_unmatched If `TRUE`, resources that could not be matched
#'   to a specific type are includes as "other" (default `FALSE`).
#'
#' @returns A [tibble][tibble::tibble] with columns:
#'   - `resource_name`: base file name.
#'   - `resource_type`: inferred resource type string.
#'   - `linked_level`: default hierarchy level for this type (`"dataset"`,
#'     `"woodpiece"`, `"slide"`, `"image"`, or `"analysis"`).
#'   - `linked_label`: label of the linked entity (`NA`; fill in manually).
#'   - `fname_resource`: absolute file path.
#'
#' @seealso [add_resources()], [infer_resource_type()]
#' @export
collect_resources <- function(path, append_to = NULL, df_structure = NULL,
                              recursive = FALSE, include_unmatched = FALSE) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_data_frame(append_to, null.ok = TRUE)
  checkmate::assert_data_frame(df_structure, null.ok = TRUE)
  checkmate::assert_flag(recursive)
  checkmate::assert_flag(include_unmatched)

   # TODO: fix with new api
  if (!is.null(append_to)) {
    df <- align_to_schema(append_to, "resources")
    check_schema(df, "resources", warn_only = TRUE, greedy = FALSE)
  } else {
    df <- create_empty_df("resources")
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
      fname_resource = as.character(files)
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

  n_new <- if (exists("new_res")) nrow(df) else 0L
  cli::cli_inform(c("v" = "Found {n_new} resource{?s} in {.path {path}}"))
  df
}


#' Add a resources table to a QWAmetadata object
#'
#' Stores a resources data frame (typically created by [collect_resources()])
#' as the `$resources` component of a [QWAmetadata] object. Appended to any
#' previously stored resources, `$images` used to infer linked labels.
#'
#' @param x A [QWAmetadata] object.
#' @param path Path to a directory to scan for files.
#' @param recursive If `TRUE`, recurse into sub-directories (default `FALSE`).
#' @param include_unmatched If `TRUE`, resources that could not be matched
#'   to a specific type are includes as "other" (default `FALSE`).
#' @returns The [QWAmetadata] object with the `$resources` component updated.
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

