#' Identify the ROXAS (AI) files in a directory
#'
#' Returns a data frame with the full path names of the relevant ROXAS (AI) files
#' (i.e., images, cell measurements, ring measurements, and ROXAS settings resp.
#' ROXAS AI metadata files) found within the given directory. 
#' The function recurses into any subdirectories, except those explicitly excluded. 
#' The files are identified by matching on the standard naming suffixes produced
#' by ROXAS resp. ROXAS AI.
#' 
#' For ROXAS, the required files per image are:
#' - Image file (`{IMAGEID}.jpg` or `.jpeg`)
#' - Cells output (`{IMAGEID}_Output_Cells.txt`)
#' - Rings output (`{IMAGEID}_Output_Rings.txt`)
#' - Settings file (`{IMAGEID}_ROXAS_Settings.txt`)
#' 
#' For ROXAS AI, the required files per image are:
#' - Image file (`{IMAGEID}.scan.jpg` or `.jpeg`)
#' - Cells table (`{IMAGEID}.cells_table.csv`)
#' - Rings table (`{IMAGEID}.rings_table.csv`)
#' - Metadata file (`{IMAGEID}.metadata.json`)
#'
#' @param path_in path of the input directory.
#' @param roxas_version which ROXAS version was used to create the files (classic
#'   `"roxas"` or new AI version `"roxas_ai"`).
#' @param exclude_dirs directory names / keywords that should be ignored
#'   when searching for ROXAS files (e.g., "unused_files", optional)
#' @returns A data frame with columns `prefix`, `fname_image`, `fname_settings`,
#'   `fname_cells` and `fname_rings` containing the full file paths.
#' @export
get_roxas_files <- function(path_in, roxas_version,
                            exclude_dirs = NULL) {
  checkmate::assert_directory_exists(path_in)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  checkmate::assert_character(exclude_dirs, null.ok = TRUE)
  
  # regex patterns to be matched by the different ROXAS files
  # image files might include annotated images, etc., these are filtered out with keywords
  if (roxas_version == "roxas"){
    pattern_cell_files <- "_Output_Cells\\.txt$"
    pattern_ring_files <- "_Output_Rings\\.txt$"
    pattern_settings_files <- "_ROXAS_Settings\\.txt$"
    pattern_orgimg_files <- "\\.(jpg|jpeg)$" # expand to "\\.(jpg|jpeg|png|gif|bmp|tiff)$"?
    rv <- "ROXAS"
  } else {
    pattern_cell_files <- "\\.cells_table\\.csv$"
    pattern_ring_files <- "\\.rings_table\\.csv$"
    pattern_settings_files <- "\\.metadata\\.json$"
    pattern_orgimg_files <- "\\.scan\\.(jpg|jpeg)$" # .rxs in future? expand to "\\.(jpg|jpeg|png|gif|bmp|tiff)$"?
    pattern_excl_keywords <- NULL
    rv <- "ROXAS AI"
  }
  imgfiles_exclude_keywords <- c("annotated", "ReferenceSeries", "Preview")
  pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse = "|")
  
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
  files_settings <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_settings_files,
    recurse = TRUE)
  files_images <- fs::dir_ls(
    fs::path_abs(path_in),
    type = "file",
    regexp = pattern_orgimg_files,
    recurse = TRUE) 
  files_images <- files_images |>
    stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)

  if (!is.null(exclude_dirs)) {
    excl_dirs_pattern <- paste0("/(", paste(exclude_dirs, collapse = "|"), ")(/|$)")
    files_cells <- stringr::str_subset(files_cells, excl_dirs_pattern, negate = TRUE)
    files_rings <- stringr::str_subset(files_rings, excl_dirs_pattern, negate = TRUE)
    files_settings <- stringr::str_subset(files_settings, excl_dirs_pattern, negate = TRUE)
    files_images <- stringr::str_subset(files_images, excl_dirs_pattern, negate = TRUE)
  }

  # join all four file lists by the common prefix to identify missing components
  df_files <- tibble::tibble(
    prefix = stringr::str_remove(files_images, stringr::regex(pattern_orgimg_files, ignore_case = TRUE)), 
    fname_image = files_images
  ) |>
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_settings, stringr::regex(pattern_settings_files, ignore_case = TRUE)), 
      fname_settings = files_settings
    ), by = "prefix"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_cells, stringr::regex(pattern_cell_files, ignore_case = TRUE)), 
      fname_cells = files_cells
    ), by = "prefix"
  ) |> 
  dplyr::full_join(
    tibble::tibble(
      prefix = stringr::str_remove(files_rings, stringr::regex(pattern_ring_files, ignore_case = TRUE)), 
      fname_rings = files_rings
    ), by = "prefix"
  )

  # ignore images without any ROXAS related files (e.g. unanalysed, or other supplementary image files)
  has_no_roxas <- is.na(df_files$fname_cells) & is.na(df_files$fname_rings) & is.na(df_files$fname_settings)
  imgs_no_roxas <- df_files$fname_image[has_no_roxas]
  df_files <- df_files[!has_no_roxas, ]

  if (nrow(df_files) == 0) {
    cli::cli_abort(c(
      "No {rv} files under {.file {path_in}}",
      "i" = "Ensure {.var path_in} points to the right directory."
    ))
  }
  
  if (any(has_no_roxas)) {
    cli::cli_warn(c(
      "i" = "Ignoring {sum(has_no_roxas)} image{?s} without any corresponding ROXAS files:",
      cli_truncated_list(imgs_no_roxas)
    ))
  }

  is_incomplete <- rowSums(is.na(df_files)) > 0
  if (any(is_incomplete)) {
    incomplete_prefixes <- df_files$prefix[is_incomplete]
    cli::cli_warn(c(
      "!" ="{rv} file mismatch: incomplete sets found - check before continuing",
      "i" = "Four files (image, cells, rings, settings) are required per image, but",
      "i" = "{sum(is_incomplete)} prefix{?es} have missing files, returned with {.code NA}:",
      cli_truncated_list(incomplete_prefixes)
    ))
  } else {
    cli::cli_inform(c(
      "v" = "{rv} file paths extracted from {.file {path_in}}",
      "i" = "Found {nrow(df_files)} complete set{?s} of {rv} files"
    ))
  }

  df_files
}

#' Extract the data structure from file names
#'
#' @description
#' Extract the hierarchical structure of the data (i.e., which images belong to
#' which slide, woodpiece, tree, site) from the given file names based on a
#' provided regex labeling pattern.
#' The extracted site, species, tree, woodpiece, slide, image identifiers are
#' joined by underscores to derive unique codes for each level of the data
#' structure for the subsequent calculations
#' (e.g. `tree_code = {site}_{species}_{tree}`).
#'
#' Any custom labeling pattern may be used, provided that it allows to
#' uniquely identify each image and is consistently applied across all files.
#' The first example below assumes the standard labeling pattern
#' `{site}_{species}_{tree}_{slide}_{image}`,
#' suggested in [Fonti et al. (2025)](https://doi.org/10.3389/fpls.2025.1505389).
#' 
#' Not all hierarchical components are mandatory in the pattern, as long as the
#' images can be uniquely identified (e.g. if exactly one woodpiece was
#' extracted per tree, there might not be a specific woodpiece identifier,
#' or if all data is from the same site, there might not be a site identifier).
#' If all images pertain to the same site and/or the same species, and these
#' components are not part of the labeling pattern, they may also be provided
#' via the `site_label` and `species_code` input parameters.
#' Here, `species_code` should follow the standard ITRDB species codes.
#'
#' [extract_data_structure()] is a wrapper around [get_structure_from_filenames()]
#' that uses the data frame of ROXAS (AI) files from [get_roxas_files()] as input,
#' applies the hierarchy extraction to the `$prefix` component,
#' and appends the original file paths as columns to the returned structure data frame.
#'
#' @param filenames The vector of file names.
#' @param pattern The labeling pattern followed by the file names, a regex
#' with named groups.
#' @param site_label Optional site code to be used if it is not part of the
#' pattern, only if all images are from the same site.
#' @param species_code Optional species code to be used if it is not part of the
#' pattern, only if all images are from the same species.
#'
#' @returns A data frame containing the extracted data structure (with the
#'   original file names appended for [extract_data_structure()]).
#' @export
#' @examples
#' # the standard pattern {site}_{species}_{tree}_{slide}_{image}
#' pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
#' filenames <- c(
#'   "SITEA_PISY_01_1_1.jpg",
#'   "SITEA_PISY_01_1_2.jpg",
#'   "SITEA_PISY_01_2_1.jpg",
#'   "SITEB_LASI_02_1_1.jpg",
#'   "SITEB_LASI_03_1_1.jpg"
#' )
#' get_structure_from_filenames(filenames, pattern)
#'
#' # custom pattern of the form {tree}-{slide}_{image}
#' filenames <- c(
#'  "tree1-sl1_img1.jpg",
#'  "tree1-sl2_img1.jpg",
#'  "tree2-sl1_img1.jpg",
#'  "tree2-sl1_img2.jpg"
#'  )
#'  pattern <- "(?<tree>[[:alnum:]].+)-(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
#'  get_structure_from_filenames(
#'    filenames, pattern,
#'    site_label = "SITEA", species_code = "LASI")
#' 
get_structure_from_filenames <- function(
    filenames,
    pattern,
    site_label = NULL, species_code = NULL) {
  checkmate::assert_character(filenames, any.missing = FALSE, min.len = 1)
  checkmate::assert_string(pattern)
  checkmate::assert_string(site_label, null.ok = TRUE)
  checkmate::assert_string(species_code, null.ok = TRUE)
  # site_label provided or "site" in pattern (%??% is checkmate's coalesce operator)
  checkmate::assert_string(site_label %??% pattern, pattern = site_label %??% "site")
  # if site_label provided then "site" not in pattern
  checkmate::assert_false(
    !is.null(site_label) &&
      stringr::str_detect(pattern, "\\(\\?<(site)>[^)]+\\)"),
  )
  # if species_code provided then "species" not in pattern
  checkmate::assert_false(
    !is.null(species_code) &&
      stringr::str_detect(pattern, "\\(\\?<(species)>[^)]+\\)"),
  )

  # replace group regex with {group} for human readable pattern in messages
  lbl_structure <- stringr::str_replace_all(pattern, "\\(\\?<(\\w+)>[^)]+\\)", "{\\1}")

  # extract the relevant pattern part of the file names
  fnames <- stringr::str_extract(filenames, pattern)

  if (any(is.na(fnames))) {
    dontmatch <- filenames[is.na(fnames)]
    cli::cli_abort(c(
      "All files must match the provided pattern",
      "x" = "The following {length(dontmatch)} file{?s} do not follow the structure",
      " " = "{.code {lbl_structure}}:",
      cli_truncated_list(dontmatch)
    ))
  }

  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates) > 0) {
    cli::cli_abort(c(
      "Extracted structure must yield unique image identifiers",
      "x" = "The following files yield duplicate image identifiers when",
      " " = "extracting the pattern {.code {lbl_structure}}:",
      cli_truncated_list(grep(paste(duplicates, collapse = "|"), filenames, value=TRUE))
    ))
  }

  # extract the matched pattern groups and collect info into df
  # add org_img_name as the base filename (no path, no extension)
  # TODO: what if org_img_name needs part of path to be identifiable?
  # potential fix: count nr of dir levels included in pattern, 
  # but not robust to cross platform specifications?
  # incl_supdirs <- stringr::str_count(pattern, "/") 
  # org_img_names <- fs::path_ext_remove(
  #   fs::path_select_components(filenames,1+incl_supdirs, "end"))
  df_structure <- as.data.frame(stringr::str_match(fnames, pattern)) |>
    dplyr::select(!"V1") |>
    dplyr::mutate(org_img_name = as.character(fs::path_file(filenames)),
                  .before = 1)
  
  df_structure <- tibble::tibble(
    org_img_name = character(0),
    site = character(0),
    species = character(0),
    tree = character(0),
    woodpiece = character(0),
    slide = character(0),
    image = character(0)) |>
    dplyr::bind_rows(df_structure) |> # ensure all columns are present
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(.x, "")))

  if (!is.null(site_label)) {
    df_structure$site <- site_label
  }
  if (!is.null(species_code)) {
    df_structure$species <- species_code
  }

  df_structure <- df_structure |>
    tidyr::unite('tree_label', "site":"tree",
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('woodpiece_label', "site":"woodpiece",
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('slide_label', "site":"slide",
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('image_label', "site":"image",
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    dplyr::select("image_label", "slide_label", "woodpiece_label",
                  "tree_label", "species", "site", "org_img_name") |> # reorder columns
    dplyr::rename("species_code" = "species", "site_label" = "site")

  df_structure
}

#' @rdname get_structure_from_filenames
#' @param files The data frame of prefix and file paths returned by [get_roxas_files()].
#' @export
extract_data_structure <- function(
    files, pattern,
    site_label = NULL, species_code = NULL) {
  checkmate::assert_data_frame(
    files, types = 'character')
  checkmate::assert_subset(
    c('prefix', 'fname_image','fname_settings','fname_cells','fname_rings'),
    names(files))

  # we use only the prefix to extract the data structure (all files
  # should match, as checked in get_roxas_files)
  df_structure <- get_structure_from_filenames(files$prefix,
                                               pattern,
                                               site_label,
                                               species_code)
  df_structure <- dplyr::bind_cols(df_structure, files |> dplyr::select(!"prefix"))

  cli::cli_inform(c(
    "v" = "Data structure extracted from file names",
    "i" = "Identified {length(unique(df_structure$woodpiece_label))} woodpiece{?s}:",
    cli_truncated_list(unique(df_structure$woodpiece_label), max_show = 5)
  ))

  df_structure
}

#' Read and combine image exif data
#'
#' Collect the exif data from all image files. Used preliminarily for data generated
#' with classic ROXAS, for ROXAS AI the image exif information is already stored in the 
#' settings files.
#'
#' @param files_images Vector/list of image filenames
#' @param batch_size Nr of images to read exif data from at a time. 
#'   Note that [exifr::read_exif()] is vectorized, so the only purpose for the 
#'   batching is to generate a progress bar. Increase for more speed.
#' @returns A dataframe containing the extracted data.
#' @export
collect_image_info <- function(files_images, batch_size = 50) {
  #checkmate::assert(all(fs::file_exists(files_images))) # too time consuming, fail later
  checkmate::assert_character(files_images, any.missing = FALSE)

  # split into batches for progress bar
  # (big batches are faster since read_exif is already vectorized)
  batches <- split(files_images, ceiling(seq_along(files_images) / batch_size))
  results <- lapply(
    cli::cli_progress_along(seq_along(batches),
                            "Reading image metadata..."),
    function(i) {
      # read exif data for each batch
      exifr::read_exif(batches[[i]],
                       tags = c("FileType", "FileSize",
                                "ImageWidth", "ImageHeight",
                                "Software",
                                "DateTimeOriginal", "DateCreated", # potential date tags
                                "DateTimeDigitized", "CreateDate"))
  })

  df_image_meta <- dplyr::bind_rows(results)
  df_image_meta[setdiff(c("Software","DateTimeOriginal", "DateCreated",
                          "DateTimeDigitized", "CreateDate"),
                        names(df_image_meta))] <- NA_character_

  df_image_meta <- df_image_meta |>
    dplyr::mutate(
      img_created_at = dplyr::coalesce(
        .data$DateTimeOriginal, .data$DateCreated,
        .data$DateTimeDigitized, .data$CreateDate
      ),
      # scan_mode = NA_character_, # TODO: part of ROXAS AI metadata, do we need it?
      .keep = "unused" # remove the original exif date cols
    ) 

  cli::cli_inform(c(
    "v" = "Image metadata extracted from {length(files_images)} images"
  ))

  df_image_meta |>
    dplyr::rename("fname_image" = "SourceFile",
                  "img_filetype" = "FileType",
                  "img_size" = "FileSize",
                  "img_width" = "ImageWidth",
                  "img_height" = "ImageHeight",
                  "img_software" = "Software")
}

#' Extract data from a ROXAS settings file
#'
#' Helper function to read and extract the relevant metadata from a single ROXAS
#' settings file
#'
#' @param file_settings The file to be read.
#' @param roxas_version The version of ROXAS used to create the file
#'
#' @returns A dataframe containing the extracted data.
#' @noRd
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
# TODO: finalize support for ROXAS AI
extract_roxas_settings <- function(file_settings, roxas_version) {
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  if (roxas_version == "roxas") {
    # read from a single settings txt file
    df_settings <- vroom::vroom(file_settings,
                                delim = "\t",
                                col_types = c(.default = "c", RNUM = "d"),
                                progress = FALSE)

    # NOTE: this relies heavily on the consistent layout of the settings file
    # in particular, we need tab delimiters, columns RNUM and SETTING,
    # and the right values in the rows 8,9,10,12,13,17,20,31,33,166,203:208!
    df_settings <- df_settings |>
      dplyr::filter(.data$RNUM %in% c(8,9,10,
                                      12,13,
                                      17,18,19,20,
                                      31,33,166,
                                      203,204,205,206,207,208
                                      )) |>
      dplyr::mutate(new_names = c(
        "configuration_file", "rxs_created_at", "sw_version",
        "spatial_resolution", "origin_calibrated",
        "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
        "min_cell_area", "max_cell_area", "dbl_cwt_threshold",
        "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_window", "maxrel_opp_cwt",
        "max_cwttan_s", "max_cwttan_l"
      )) |>
      dplyr::select("SETTING", "new_names") |>
      tidyr::pivot_wider(names_from = "new_names", values_from = "SETTING") |>
      dplyr::mutate(
        meas_geometry = dplyr::if_else(.data$meas_geometry==1, "linear", "circular"),
        fname_settings = file_settings,
        software = "roxas") |>
      tidyr::separate_wider_regex("origin_calibrated",
                                  patterns = c(origin_calibrated_x = "[^/]+",
                                               "[ ]*/[ ]*",
                                               origin_calibrated_y = ".+")) |>
      dplyr::mutate(dplyr::across(c("origin_calibrated_x", "origin_calibrated_y"), as.numeric)) |>
      dplyr::relocate("fname_settings", "software", "sw_version")
  } else {
    # read from a single metadata json file
    raw <- jsonlite::read_json(file_settings, simplifyVector = TRUE)
    df_settings <- raw |> 
      purrr::map(\(x) ifelse(is.null(x) || length(x)>1, list(x), x)) |> 
      tibble::as_tibble() |> 
      tidyr::unnest_wider(col = "scan_size", names_sep = "_") |> 
      tidyr::unnest_wider(col = "scan_exif") |>
      dplyr::bind_rows(data.frame("Software" = character(0), # ensure we have all columns even if scan_exis is null
        "DateTimeOriginal" = character(0),
        "DateTimeDigitized"= character(0))) |> 
      dplyr::select(!"scan_info") |> # TODO: include or not?
      dplyr::rename(c(
        # config files: rings_segmentation_model, cells_segmentation_model,
        # analysis created at: rings_segmentation_datetime, cells_segmentation_datetime (both? choose? measurements?)
        # TODO: sw_version - there is currently no ROXAS AI version in the file?
        # sample_type: new, provide by user for classic?
        "spatial_resolution" = "sample_scale",
        # no calibrated origin anymore (?)
        "meas_geometry" = "sample_geometry",
        "outmost_year" = "rings_outmost_complete_year",
        # TODO: will include model filter settings in future versions?
        "img_filetype" = "scan_format", # TODO: what about img_size?
        "img_width" = "scan_size_1",
        "img_height" = "scan_size_2",
        "img_software" = "Software"
        # scan_mode -> add to classic version from exif if possible?
        # DateTimeOriginal, DateTimeDigitized -> use min as img_created_at
      )) |>
      dplyr::mutate(
        fname_settings = file_settings, 
        software = "roxas_ai", 
        sw_version = NA_character_, 
        rxs_created_at = NA_character_, 
        img_size = NA_integer_
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "fname_settings", "sample_type", "meas_geometry", 
          "img_filetype", "img_width", "img_height", "img_size", "img_software", "scan_mode",
          "DateTimeOriginal", "DateTimeDigitized", 
          "spatial_resolution",
          "software", "sw_version", "rxs_created_at", "outmost_year",
          "rings_segmentation_model", "cells_segmentation_model",
          "rings_segmentation_datetime", "cells_segmentation_datetime"
        ))
      )
  }

  df_settings
}

#' Read and combine ROXAS (AI) settings data
#'
#' Collect the settings data from all ROXAS (AI) settings files.
#'
#' For `roxas_version = "roxas"`, image EXIF metadata is collected from the
#' image files via [collect_image_info()] and joined to the settings data.
#' `files_images` is required in this case and must be in the same order as
#' `files_settings`.
#'
#' For `roxas_version = "roxas_ai"`, image metadata is already embedded in
#' the settings JSON files; `files_images` is not needed.
#'
#' @param files_settings Vector of ROXAS settings file names.
#' @param files_images Vector of image file names in the same order as
#'   `files_settings`. Required when `roxas_version = "roxas"`.
#' @param roxas_version The version of ROXAS used to create the files.
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_settings_data <- function(files_settings,
                                  files_images = NULL,
                                  roxas_version) {
  #checkmate::assert(all(fs::file_exists(files_settings))) # too time consuming, fail later
  checkmate::assert_character(files_settings, any.missing = FALSE)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  if (roxas_version == "roxas") {
    checkmate::assert_character(files_images, len = length(files_settings), any.missing = FALSE,
    .var.name = "Missing image files input for `roxas` data."
    )
  }
  
  rv_msg <- if (roxas_version == "roxas") "ROXAS settings" else "ROXAS AI metadata"
  results <- files_settings |>
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version),
               .progress = list(name = glue::glue("Reading {rv_msg} files..."), clear = TRUE))
  df_settings_all <- purrr::list_rbind(results)

  if (roxas_version == "roxas") {
    # convert columns to numeric and integer
    df_settings_all <- df_settings_all |>
      dplyr::mutate(dplyr::across(c("spatial_resolution",
                                    "dbl_cwt_threshold":"max_cwttan_l"), as.numeric),
                    dplyr::across("circ_lower_limit":"max_cell_area", as.integer)
                    #sample_type = NA_character_ # TODO: as input?
                  )
    # collect image EXIF metadata and bind alongside settings columns
    # NOTE: files_images and files_settings need to be in the same order
    df_images <- collect_image_info(files_images)
    df_settings_all <- dplyr::bind_cols(df_settings_all,
                                        df_images |> dplyr::select(!"fname_image"))
  } else {
    cli::cli_warn("ROXAS AI support is still under development, not all metadata variables may be available.")

    df_settings_all <- df_settings_all |>
      dplyr::mutate(dplyr::across(c("spatial_resolution"), as.numeric),
                    dplyr::across(c("img_width", "img_height", "outmost_year"), as.integer)) |>
      dplyr::mutate(  # we know the fixed ISO format for ROXAS AI
        rings_segmentation_datetime = lubridate::parse_date_time(.data$rings_segmentation_datetime, orders = "ymdHMS"),
        cells_segmentation_datetime = lubridate::parse_date_time(.data$cells_segmentation_datetime, orders = "ymdHMS"),
        rxs_created_at = pmax(.data$rings_segmentation_datetime, .data$cells_segmentation_datetime, na.rm = TRUE)
      ) |> 
      dplyr::mutate(
        img_created_at = dplyr::coalesce(.data$DateTimeOriginal, .data$DateTimeDigitized),
        .keep = "unused", .after = "scan_mode"
      )
  }

  rv_msg <- if (roxas_version == "roxas") rv_msg else paste(rv_msg, "and image exif")
  cli::cli_inform(c(
    "v" = "{rv_msg} data extracted from {nrow(df_settings_all)} files"
  ))
  df_settings_all 
}

#' Build a QWAimages object from raw ROXAS (AI) metadata
#'
#' Combines the extracted ROXAS / ROXAS AI file names plus data structure (from
#' [extract_data_structure()]) and ROXAS settings and image exif data (from
#' [collect_settings_data()]) into a [QWAimages] object.
#'
#' This is the typical starting point of the `prepare_rxs_dataset` workflow.
#' The resulting [QWAimages] can be passed directly to [collect_raw_data()] (to
#' read the cell and ring measurements) and to the metadata Shiny app
#' ([launch_metadata_app()], where one can enrich it with site-, tree-, and
#' dataset-level metadata to produce a full [QWAmetadata] object).
#'
#' @param df_structure Data frame with all input file names and data structure.
#' @param df_settings Data frame with extracted ROXAS (AI) settings data.
#'
#' @returns A [QWAimages] object with the joined image-level metadata.
#' @seealso [extract_data_structure()], [collect_settings_data()],
#'   [QWAimages()], [write_QWAimages()]
#' @export
build_QWAimages <- function(df_structure,
                            df_settings) {
  # assert the minimal required conditions to create a QWAimages data frame
  checkmate::assert_data_frame(df_structure,
    #any.missing = FALSE, could have missing species code?
     min.rows = 1, .var.name = "df_structure")
  checkmate::assert_subset(
    c("image_label", "slide_label", "woodpiece_label", "tree_label", "site_label",  
      "fname_image",'fname_cells','fname_rings','fname_settings'),
    colnames(df_structure))
  checkmate::assert_data_frame(df_settings)
  checkmate::assert_subset(c('fname_settings'), colnames(df_settings))
  checkmate::assert_permutation(
    df_structure$fname_settings,
    df_settings$fname_settings, na.ok = FALSE)
  # custom check of the hierarchy defined by the structure columns
  checkmate::assert_character(df_structure$image_label, unique = TRUE)
  check_structure(df_structure)

  df_rxsmeta <- df_structure |>
    dplyr::left_join(df_settings, by = 'fname_settings')

  # auto-detect schema version from the software column
  rv <- resolve_roxas_version(df_rxsmeta)

  # compliance with base schema
  schema_path <- system.file(schema_rel_path(rv), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  
  # align columns and types to schema (coerce with warnings)
  df_rxsmeta <- align_to_schema(df_rxsmeta, tbl_props, rv, add_opt = FALSE, mute_info = FALSE)

  # validate against the base schema, but warn only. user may want to fix
  # things manually in the metadata app
  check_schema(df_rxsmeta, schema_obj, rv, warn_only = TRUE, greedy = FALSE)

  rv_msg <- if (rv == "roxas") "ROXAS" else "ROXAS AI"
  cli::cli_inform(c(
    "v" = "Available {rv_msg} metadata extracted to {.var QWAimages} object"
  ))

  new_QWAimages(df_rxsmeta, roxas_version = rv)
}
