#' Identify the ROXAS (AI) files in a directory
#'
#' Returns a data frame with the full paths of the four types of relevant 
#' ROXAS (AI) files (i.e., images, cell measurements, ring measurements, and 
#' ROXAS settings resp. ROXAS AI metadata files) found within the given 
#' directory. 
#' Recurses into any subdirectories except those explicitly excluded. 
#' The files are identified by matching on the standard naming suffixes produced
#' by ROXAS resp. ROXAS AI, see the Details below.
#' 
#' For ROXAS, the required files per image are:
#' * The original image file (`{IMAGEID}.jpg` or `.jpeg`).
#' * The ROXAS settings file (`{IMAGEID}_ROXAS_Settings.txt`).
#' * The cell measurements output (`{IMAGEID}_Output_Cells.txt`).
#' * The ring measurements output (`{IMAGEID}_Output_Rings.txt`).
#' 
#' For ROXAS AI, the required files per image are:
#' * The original image file (`{IMAGEID}.scan.jpg` or `.jpeg`).
#' * The ROXAS AI metadata file (`{IMAGEID}.metadata.json`).
#' * The cell measurements table (`{IMAGEID}.cells_table.csv`).
#' * The ring measurements table (`{IMAGEID}.rings_table.csv`).
#'
#' @param path_in The path of the input directory.
#' @param roxas_version Which software was used to create the files, either
#'   `"roxas"` or `"roxas_ai"`.
#' @param exclude_dirs Optional character vector of subdirectory names that 
#'   should not be searched for ROXAS (AI) files (e.g., `"unused_files"`).
#' @returns A data frame with columns `prefix`, `fname_image`, `fname_settings`,
#'   `fname_cells` and `fname_rings` containing the common prefixes and full 
#'   file paths.
#' @export
get_roxas_files <- function(path_in, roxas_version, exclude_dirs = NULL) {
  checkmate::assert_directory_exists(path_in)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  checkmate::assert_character(exclude_dirs, null.ok = TRUE)
  
  # ROXAS files are identified by matching regex patterns
  if (roxas_version == "roxas") {
    pattern_orgimg_files <- "\\.(jpg|jpeg|png|bmp|tiff)$"
    pattern_settings_files <- "_ROXAS_Settings\\.txt$"
    pattern_cell_files <- "_Output_Cells\\.txt$"
    pattern_ring_files <- "_Output_Rings\\.txt$"
  } else {
    pattern_orgimg_files <- "\\.scan\\.(jpg|jpeg|png|bmp|tiff)$" # TODO: 'scan' might change to 'rxs' in future
    pattern_settings_files <- "\\.metadata\\.json$"
    pattern_cell_files <- "\\.cells_table\\.csv$"
    pattern_ring_files <- "\\.rings_table\\.csv$"
  }
  # Processed image files need to be filtered out
  imgfiles_exclude_keywords <- c("annotated", "ReferenceSeries", "Preview", 
                                 "\\.cells\\.", "\\.rings\\.")
  pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse = "|")

  files_images <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_orgimg_files,
    recurse = TRUE) 
  files_images <- files_images |>
    stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)
  files_settings <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_settings_files,
    recurse = TRUE)
  files_cells <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_cell_files,
    recurse = TRUE)
  files_rings <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_ring_files,
    recurse = TRUE)

  if (!is.null(exclude_dirs)) {
    excl_dirs_pattern <- paste0(
      "/(", paste(stringr::str_escape(exclude_dirs), collapse = "|"), ")(/|$)"
    )
    files_images <- files_images |> 
      stringr::str_subset(excl_dirs_pattern, negate = TRUE)
    files_settings <- files_settings |> 
      stringr::str_subset(excl_dirs_pattern, negate = TRUE)
    files_cells <- files_cells |> 
      stringr::str_subset(excl_dirs_pattern, negate = TRUE)
    files_rings <- files_rings |> 
      stringr::str_subset(excl_dirs_pattern, negate = TRUE)
  }

  # join all four file lists by the common prefix to identify missing components
  df_files <- tibble::tibble(
    prefix = stringr::str_remove(files_images, stringr::regex(pattern_orgimg_files)), 
    fname_image = files_images
  ) |>
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_settings, stringr::regex(pattern_settings_files)), 
      fname_settings = files_settings
    ), by = "prefix"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_cells, stringr::regex(pattern_cell_files)), 
      fname_cells = files_cells
    ), by = "prefix"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_rings, stringr::regex(pattern_ring_files)), 
      fname_rings = files_rings
    ), by = "prefix"
  )

  # Image pattern is least specific, so found files might include non-ROXAS 
  # images such as un-analysed test images or supplementary image files 
  # (e.g. reference plots or woodpiece scans). Drop with a warning.
  has_no_roxas <- is.na(df_files$fname_cells) & is.na(df_files$fname_rings) & is.na(df_files$fname_settings)
  imgs_no_roxas <- df_files$fname_image[has_no_roxas]
  df_files <- df_files[!has_no_roxas, ]

  rv <- rv_msg(roxas_version)
  if (nrow(df_files) == 0) {
    cli::cli_abort(c(
      "Can't find any {rv} files under {.file {path_in}}."
    ))
  }

  is_incomplete <- rowSums(is.na(df_files)) > 0
  if (any(is_incomplete)) {
    incomplete_prefixes <- df_files$prefix[is_incomplete]
    cli::cli_warn(c(
      "Four files (image, settings, cells, rings) are required per image.",
      "!" = "Inspect before continuing.",
      "i" = "{sum(is_incomplete)} prefix{?es} with incomplete sets of {rv} files:",
      cli_truncated_list(incomplete_prefixes)
    ))
  } else {
    # success only if no incomplete sets
    cli::cli_alert_success(
      "Found {nrow(df_files)} complete set{?s} of {rv} files."
    )
  }

  if (any(has_no_roxas)) {
    cli::cli_warn(c(
      "Ignored {sum(has_no_roxas)} image file{?s} without any corresponding ROXAS files:",
      cli_truncated_list(imgs_no_roxas)
    ))
  }

  df_files
}
