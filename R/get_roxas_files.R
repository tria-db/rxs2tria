#' Compile ROXAS (AI) files
#' 
#' @description
#' Returns a data frame with the shared prefix and file paths of the four types
#' of relevant ROXAS (AI) files (i.e., images, cell measurements, ring
#' measurements, and ROXAS settings resp. ROXAS AI metadata files) found under 
#' the given path(s). 
#' By default, it recurses into any subdirectories except those explicitly 
#' excluded. 
#' The files are identified by matching on the standard naming suffixes produced
#' by ROXAS resp. ROXAS AI, see the Details below. The input path(s) should 
#' include either ROXAS or ROXAS AI files, not both.
#' 
#' @details
#' For ROXAS, the required files per image are:
#' * The original image file (e.g., `{IMAGEID}.jpg`).
#' * The ROXAS settings file (`{IMAGEID}_ROXAS_Settings.txt`).
#' * The cell measurements output (`{IMAGEID}_Output_Cells.txt`).
#' * The ring measurements output (`{IMAGEID}_Output_Rings.txt`).
#'
#' For ROXAS AI, the required files per image are:
#' * The original image file (`{IMAGEID}.scan.jpg`).
#' * The ROXAS AI metadata file (`{IMAGEID}.metadata.json`).
#' * The cell measurements table (`{IMAGEID}.cells_table.csv` or `.txt`).
#' * The ring measurements table (`{IMAGEID}.rings_table.csv` or `.txt`).
#'
#' Images without any corresponding ROXAS (AI) files are ignored, sets lacking 
#' only some of the files are returned with a warning. 
#' If several images with the same name exist, the order of preference is
#' `.tif`, `.tiff`, `.jp2`, `.bmp`, `.jpg`, `.jpeg`, `.pct`, `.pict` for ROXAS,
#' and `.jpg`, `.jpeg`, `.png`, `.tif`, `.tiff`, `.bmp` `.jp2` for ROXAS AI, 
#' with extensions matched case-insensitively.
#'
#' @param path_in A character vector of one or more paths.
#' @param roxas_version Which software was used to create the files, either
#'   `"roxas"` or `"roxas_ai"`.
#' @param exclude_dirs Optional character vector of subdirectory names that 
#'   should not be searched for ROXAS (AI) files (e.g., `"unused_files"`).
#' @param recurse If `TRUE` (default), subdirectories of `path_in` are also
#'   searched; if `FALSE`, only files directly in `path_in` are used.
#' @returns A data frame with columns `prefix`, `fname_image`, `fname_settings`,
#'   `fname_cells` and `fname_rings` containing the common prefixes and full 
#'   file paths.
#' @export
get_roxas_files <- function(path_in, roxas_version, 
                            exclude_dirs = NULL, recurse = TRUE) {
  checkmate::assert_directory_exists(path_in)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  checkmate::assert_character(exclude_dirs, null.ok = TRUE)
  checkmate::assert_flag(recurse)
  
  # ROXAS (AI) files are identified by matching regex patterns
  # (images: via extensions of supported formats; priority order is used
  # to choose one if the same image exists in several formats)
  imgformat_priority <- img_ext_priority(roxas_version)
  if (roxas_version == "roxas") {
    pattern_orgimg_files <- "\\.(tiff?|jp2|bmp|jpe?g|pct|pict)$"
    pattern_settings_files <- "_ROXAS_Settings\\.txt$"
    pattern_cell_files <- "_Output_Cells\\.txt$"
    pattern_ring_files <- "_Output_Rings\\.txt$"
  } else {
    pattern_orgimg_files <- "\\.scan\\.(jpe?g|png|tiff?|bmp|jp2)$" # TODO: 'scan' might change to 'rxs' in future
    pattern_settings_files <- "\\.metadata\\.json$"
    pattern_cell_files <- "\\.cells_table\\.(csv|txt)$"
    pattern_ring_files <- "\\.rings_table\\.(csv|txt)$"
  }
  # Processed image files need to be filtered out
  imgfiles_exclude_keywords <- c("annotated", "ReferenceSeries", "Preview",
                                 "\\.cells\\.", "\\.rings\\.")
  pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse = "|")

  path_in <- unique(fs::path_abs(path_in))
  files_images <- fs::dir_ls(
    path_in, type = "file",
    regexp = pattern_orgimg_files,
    ignore.case = TRUE, 
    recurse = recurse) |> unique() # deduplicate nested paths
  files_images <- files_images |>
    stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)
  files_settings <- fs::dir_ls(
    path_in, type = "file",
    regexp = pattern_settings_files,
    recurse = recurse) |> unique()
  files_cells <- fs::dir_ls(
    path_in, type = "file",
    regexp = pattern_cell_files,
    recurse = recurse) |> unique()
  files_rings <- fs::dir_ls(
    path_in, type = "file",
    regexp = pattern_ring_files,
    recurse = recurse) |> unique()

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

  # The same image might exist in more than one format (e.g. the original
  # .tif alongside an exported .jpg copy). Keep only the highest-priority
  # format per prefix and drop the rest with a message.
  prefix_images <- stringr::str_remove(files_images,
    stringr::regex(pattern_orgimg_files, ignore_case = TRUE))
  ext_rank <- match(tolower(fs::path_ext(files_images)), imgformat_priority)
  ord <- order(prefix_images, ext_rank) # sort alphabetically, break ties with ext_rank
  files_images <- files_images[ord]
  prefix_images <- prefix_images[ord]
  is_dup_img <- duplicated(prefix_images) # the second occurences are lower priority
  img_duplicates <- files_images[is_dup_img]
  files_images <- files_images[!is_dup_img]
  prefix_images <- prefix_images[!is_dup_img]

  # join all four file lists by the common prefix to identify missing components
  # (multiple settings/cells/rings files per prefix are reported below)
  df_files <- tibble::tibble(
    prefix = prefix_images,
    fname_image = files_images
  ) |>
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_settings, stringr::regex(pattern_settings_files)), 
      fname_settings = files_settings
    ), by = "prefix", relationship = "many-to-many"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_cells, stringr::regex(pattern_cell_files)), 
      fname_cells = files_cells
    ), by = "prefix", relationship = "many-to-many"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_rings, stringr::regex(pattern_ring_files)), 
      fname_rings = files_rings
    ), by = "prefix", relationship = "many-to-many"
  )

  # Image pattern is least specific, so found files might include non-ROXAS
  # images such as un-analysed test images or supplementary image files
  # (e.g. reference plots or woodpiece scans). Drop with an info message.
  has_no_roxas <- is.na(df_files$fname_cells) & is.na(df_files$fname_rings) & is.na(df_files$fname_settings)
  imgs_no_roxas <- df_files$fname_image[has_no_roxas]
  df_files <- df_files[!has_no_roxas, ]

  rv <- rv_msg(roxas_version)
  if (nrow(df_files) == 0) {
    cli::cli_abort(c(
      "Can't find any {rv} files under {.file {path_in}}."
    ))
  }

  # incomplete sets, grouped by their missing file types
  fname_cols <- c(image = "fname_image", settings = "fname_settings",
                  cells = "fname_cells", rings = "fname_rings")
  is_missing <- is.na(df_files[fname_cols])
  is_incomplete <- rowSums(is_missing) > 0
  if (any(is_incomplete)) {
    missing_types <- apply(is_missing, 1,
                           \(x) paste("Missing", paste(names(fname_cols)[x], collapse = ", ")))
    cli::cli_warn(c(
      "Four files are expected per image (raw image, settings, cells, rings).",
      "!" = "Found {sum(is_incomplete)} incomplete set{?s} of {rv} files:",
      cli_truncated_groups(
        split(df_files$prefix[is_incomplete], missing_types[is_incomplete]),
        max_show_out = 5, quote = FALSE),
      "i" = "Missing files lead to missing (meta-)data. Inspect the output and consider removing these files."
    ))
  }

  # Multiple settings or measurement files for the same prefix (e.g. legacy
  # .txt alongside newer .csv tables) duplicate its row. Keep all rows, as
  # the user needs to remove the outdated files.
  multi_prefixes <- unique(df_files$prefix[duplicated(df_files$prefix)])
  if (length(multi_prefixes)) {
    cli::cli_warn(c(
      "Only one set of {rv} files is allowed per image.",
      "!" = "{length(multi_prefixes)} prefix{?es} with multiple {rv} analysis results:",
      cli_truncated_list(multi_prefixes),
      "i" = "Remove outdated files before continuing."
    ))
  }

  # success message
  n_sets <- dplyr::n_distinct(df_files$prefix)
  n_complete <- sum(!is_incomplete & !df_files$prefix %in% multi_prefixes)
  if (n_complete == n_sets) {
    cli::cli_alert_success("Found {n_sets} complete set{?s} of {rv} files.")
  } else {
    cli::cli_alert_info(
      "Found {n_complete} complete set{?s} of {rv} files and {n_sets - n_complete} with issues, see warnings."
    )
  }

  if (any(has_no_roxas)) {
    cli::cli_inform(c(
      "!" = "Ignored {sum(has_no_roxas)} image file{?s} without any corresponding ROXAS files:",
      cli_truncated_list(imgs_no_roxas)
    ))
  }

  if (length(img_duplicates)) {
    cli::cli_inform(c(
      "!" = "Ignored {length(img_duplicates)} duplicate image file{?s} of lower-priority formats:",
      cli_truncated_list(img_duplicates)
    ))
  }

  df_files
}

#' Image format priority per ROXAS version
#'
#' File extensions of the original image formats supported by ROXAS resp.
#' ROXAS AI, in order of preference if the same image exists in several
#' formats. Used by [get_roxas_files()] and [compile_supplementary()].
#'
#' @param roxas_version Either `"roxas"` or `"roxas_ai"`.
#' @returns Character vector of lower-case file extensions.
#' @noRd
img_ext_priority <- function(roxas_version) {
  switch(roxas_version,
    roxas = c("tif", "tiff", "jp2", "bmp", "jpg", "jpeg", "pct", "pict"), # png not compatible
    # TODO: may switch to lossless priority too once formats other than jpeg fully supported in roxas ai
    # (and the mixed formats bugs fixed)
    roxas_ai = c("jpg", "jpeg", "png", "tif", "tiff", "bmp", "jp2")
  )
}
