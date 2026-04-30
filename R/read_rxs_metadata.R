#' Identify ROXAS data files
#'
#' Given a path to a directory, returns a nested list with the full filepaths of
#' all required ROXAS files (images, cell outputs, ring outputs, settings resp. 
#' metadata) found within (including all subdirectories except those explicitly
#' excluded). 
#' Note that this function assumes that the files follow the standard naming 
#' suffixes form ROXAS resp. ROXAS AI, and that each set of four files is stored
#' in the same (sub-)directory.
#'
#' @param path_in path of the input directory.
#' @param roxas_version which ROXAS version was used to create the files (classic
#'   `"roxas"` or new AI version `"roxas_ai"`).
#' @param exclude_dirs directory names / keywords that should be ignored
#'   when searching for ROXAS files (e.g., "unused_files", optional)
#' @returns A named list of four character vectors (`fname_image`, `fname_cells`,
#'   `fname_rings`, `fname_settings`) containing the full filepaths.
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
    pattern_orgimg_files <- "\\.(jpg|jpeg)$" # or "\\.(jpg|jpeg|png|gif|bmp|tiff)$"?
    imgfiles_exclude_keywords <- c("annotated", "ReferenceSeries", "Preview")
    pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse = "|")
    rv <- "ROXAS"
  } else {
    pattern_cell_files <- "\\.cells_table\\.csv$"
    pattern_ring_files <- "\\.rings_table\\.csv$"
    pattern_settings_files <- "\\.metadata\\.json$"
    pattern_orgimg_files <- "\\.scan\\.(jpg|jpeg)$" # or "\\.(jpg|jpeg|png|gif|bmp|tiff)$"?
    pattern_excl_keywords <- NULL
    rv <- "ROXAS AI"
  }
  
  # NOTE: use fs rather than base list.files, much faster for network shares
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
  if (!is.null(pattern_excl_keywords)) {
    files_images <- files_images |>
      stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)
  }

  if (!is.null(exclude_dirs)) {
    excl_dirs_pattern <- paste0("/(", paste(exclude_dirs, collapse = "|"), ")(/|$)")
    files_cells <- stringr::str_subset(files_cells, excl_dirs_pattern, negate = TRUE)
    files_rings <- stringr::str_subset(files_rings, excl_dirs_pattern, negate = TRUE)
    files_settings <- stringr::str_subset(files_settings, excl_dirs_pattern, negate = TRUE)
    files_images <- stringr::str_subset(files_images, excl_dirs_pattern, negate = TRUE)
  }

  # check: if the patterns are removed, the four file lists should match
  l_files <- list(
    stringr::str_remove(files_cells, stringr::regex(pattern_cell_files, ignore_case = TRUE)),
    stringr::str_remove(files_rings, stringr::regex(pattern_ring_files, ignore_case = TRUE)),
    stringr::str_remove(files_settings, stringr::regex(pattern_settings_files, ignore_case = TRUE)),
    stringr::str_remove(files_images, stringr::regex(pattern_orgimg_files, ignore_case = TRUE))
  )
  all_fnames <- Reduce(union, l_files)

  # stop if there are no ROXAS files at all
  if (length(all_fnames) == 0) {
    cli::cli_abort(c(
      "Did not find any {rv} files under {.var path_in}",
      "i" = "Please ensure path {.file path_in} points to the correct directory."
    ))
  }

  # identify any mismatches between ring, cell, settings, and image files
  dontmatch <- setdiff(
    all_fnames,
    Reduce(intersect, l_files)
  )

  # stop if there are any mismatches
  if (length(dontmatch) > 0) {
    cli::cli_abort(c(
      "{rv} file mismatch detected",
      "i" = "Four {rv} files (image, cells, rings, settings) are required per image",
      "x" = "The following path snippets do not yield complete sets of {rv} files:",
      dontmatch
    ))
  }

  cli::cli_inform(c(
    "v" = "{rv} filepaths extracted from {.var path_in}",
    "i" = "Found {length(files_images)} image{?s} with associated {rv} files"
  ))

  # sorting to ensure matching order in the four components
  list(
    fname_image = sort(files_images),
    fname_cells = sort(files_cells),
    fname_rings = sort(files_rings),
    fname_settings = sort(files_settings)
  )
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
#' The default pattern assumes that all file names follow the labeling scheme
#' `{site}_{species}_{tree}{woodpiece}_{slide}_{image}`,
#' where `{tree}` is a two-character code, and `{woodpiece}` is optional, as
#' suggested in [Fonti et al. (2025)](https://doi.org/10.3389/fpls.2016.00781).
#'
#' However, a different labeling pattern may be used, provided that it allows to
#' uniquely identify each image in the list of file names and is consistently
#' applied across all files.
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
#' that uses the list of ROXAS (AI) files from [get_roxas_files()] as input,
#' applies the hierarchy extraction to the `$fname_image` component,
#' and appends the original filepaths as columns to the structure data frame output.
#'
#' @param filenames The vector of file names.
#' @param pattern The labeling pattern followed by the file names, a regex
#' with named groups.
#' @param site_label Optional site code to be used if it is not part of the
#' pattern, only if all images are from the same site.
#' @param species_code Optional species code to be used if it is not part of the
#' pattern, only if all images are from the same species.
#'
#' @returns A dataframe containing the extracted data structure (plus the
#' original file names for [extract_data_structure()]).
#' @export
#' @examples
#' # with the default pattern ({site}_{species}_{tree}{woodpiece}_{slide}_{image})
#' filenames <- c(
#'   "SITEA_PISY_01A_1_1.jpg",
#'   "SITEA_PISY_01A_1_2.jpg",
#'   "SITEA_PISY_01A_2_1.jpg",
#'   "SITEB_LASI_02A_1_1.jpg",
#'   "SITEB_LASI_02B_1_1.jpg",
#'   "SITEB_LASI_03_1_1.jpg"
#' )
#' get_structure_from_filenames(filenames)
#'
#' # custom pattern of the form {tree}_{slide}_{image}
#' filenames <- c(
#'  "tree1_sl1_img1.jpg",
#'  "tree1_sl2_img1.jpg",
#'  "tree2_sl1_img1.jpg",
#'  "tree2_sl1_img2.jpg"
#'  )
#'  pattern <- "(?<tree>[[:alnum:]].+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
#'  get_structure_from_filenames(
#'    filenames, pattern,
#'    site_label = "SITEA", species_code = "PISY")
#' 
get_structure_from_filenames <- function(
    filenames,
    pattern = "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]][[:alnum:]])(?<woodpiece>[[:alnum:]]*)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)",
    site_label = NULL, species_code = NULL){
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
      "All files must match the labeling pattern",
      "x" = "There {?is/are} {length(dontmatch)} file{?s} not following",
      "x" = "{.code {lbl_structure}}:",
      dontmatch
    ))
  }

  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates) > 0) {
    cli::cli_abort(c(
      "Extracted structure must yield unique image identifiers",
      "x" = "The following files yield duplicate image identifiers",
      "x" = "when extracting the pattern {.code {lbl_structure}}:",
      grep(paste(duplicates, collapse = "|"), filenames, value=TRUE)
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
    dplyr::mutate(org_img_name = as.character(fs::path_ext_remove(fs::path_file(filenames))),
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
#' @param files The named list of file vectors returned by [get_roxas_files()].
#' @export
extract_data_structure <- function(
    files,
    pattern = "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:].]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)",
    site_label = NULL, species_code = NULL) {
  checkmate::assert_list(
    files, types = 'character', any.missing = FALSE, len = 4)
  checkmate::assert_subset(
    names(files),
    c('fname_image','fname_cells','fname_rings','fname_settings'))

  # we use only the image filenames to extract the data structure (all files
  # should match, as checked in get_roxas_files)
  img_filenames <- files$fname_image
  df_structure <- get_structure_from_filenames(img_filenames,
                                               pattern,
                                               site_label,
                                               species_code)
  files_fs <- files |> purrr::map(\(x) fs::path(x)) # ensure normalized fs paths
  df_structure <- dplyr::bind_cols(df_structure, files_fs)

  cli::cli_inform(c(
    "v" = "Data structure extracted from filenames",
    "i" = "Identified the following {length(unique(df_structure$woodpiece_label))} woodpiece{?s}:",
    unique(df_structure$woodpiece_label)
  ))

  df_structure
}


#' Read and combine image exif data
#'
#' Collect the exif data from all image files. Used preliminarily for data generated
#' with classic ROXAS, for ROXAS AI the image exif inormation is already stored in the 
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
  checkmate::assert_character(files_images)

  # split into batches for progress bar
  # (big batches are faster since read_exif is already vecotrized)
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
      # scan_mode = NA_character_, # TODO: infer somehow?
      .keep = "unused" # remove the original date cols
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
  if (roxas_version == "roxas"){
    # read from a single settings file
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
    cli::cli_warn("ROXAS AI support is still under development.")

    raw <- jsonlite::read_json(file_settings, simplifyVector = TRUE)
    df_settings <- raw |> 
      purrr::map(\(x) ifelse(length(x)>1, list(x), x)) |> 
      tibble::as_tibble() |> 
      tidyr::unnest_wider(col = "scan_size", names_sep = "_") |> 
      tidyr::unnest_wider(col = "scan_exif") |> 
      dplyr::select(!"scan_info") |> # not needed?
      dplyr::rename(c(
        # config files: rings_segmentation_model, cells_segmentation_model,
        # analysis created at: rings_segmentation_datetime, cells_segmentation_datetime (both? choose? measurements?)
        # TODO: need ROXAS AI version???
        # sample_type: new, provide by user for classic?
        "spatial_resolution" = "sample_scale",
        # no calibrated origin anymore (?)
        "meas_geometry" = "sample_geometry",
        "outmost_year" = "rings_outmost_complete_year",
        # model filter settings?
        "img_filetype" = "scan_format", # TODO: what about img_size?
        "img_width" = "scan_size_1",
        "img_height" = "scan_size_2", 
        "img_software" = "Software" 
        # scan_mode -> add to classic version from exif if possible?
        #DateTimeOriginal, DateTimeDigitized -> use min as img_created_at?
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


#' Read and combine ROXAS settings data
#'
#' Collect the settings data from all ROXAS settings files.
#'
#' For `roxas_version = "roxas"`, image EXIF metadata is collected from the
#' image files via [collect_image_info()] and joined to the settings data.
#' `files_images` is required in this case and must be in the same order as
#' `files_settings`.
#'
#' For `roxas_version = "roxas_ai"`, image metadata is already embedded in
#' the settings JSON files; `files_images` is not needed.
#'
#' @param files_settings Vector/list of ROXAS settings filenames.
#' @param files_images Vector/list of image filenames in the same order as
#'   `files_settings`. Required when `roxas_version = "roxas"`.
#' @param roxas_version The version of ROXAS used to create the files.
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_settings_data <- function(files_settings,
                                  files_images = NULL,
                                  roxas_version) {
  #checkmate::assert(all(fs::file_exists(files_settings))) # too time consuming, fail later
  checkmate::assert_character(files_settings) 
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  if (roxas_version == "roxas"){
    checkmate::assert_character(files_images, len = length(files_settings))
  }
  
  rv_file <- if (roxas_version == "roxas") "ROXAS settings" else "ROXAS AI metadata"
  results <- files_settings |>
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version),
               .progress = list(name = glue::glue("Reading {rv_file} files..."), clear = TRUE))
  df_settings_all <- purrr::list_rbind(results)

  if (roxas_version == "roxas"){
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
    cli::cli_warn("ROXAS AI support is still under development, check data carefully.")

    df_settings_all <- df_settings_all |>
      dplyr::mutate(dplyr::across(c("spatial_resolution"), as.numeric),
                    dplyr::across(c("img_width", "img_height", "outmost_year"), "as.integer")) |>
      dplyr::mutate(
        img_created_at = dplyr::coalesce(.data$DateTimeOriginal, .data$DateTimeDigitized),
        .keep = "unused", .after = "scan_mode"
      )
  }

  rv_file <- if (roxas_version == "roxas") rv_file else paste(rv_file, "and image exif")
  cli::cli_inform(c(
    "v" = "{rv_file} data extracted from {nrow(df_settings_all)} files"
  ))
  df_settings_all
}


#' Build a QWAimages object from raw ROXAS metadata
#'
#' Combines the extracted ROXAS / ROXAS AI filenames plus data structure (from
#' [extract_data_structure()]) and ROXAS settings and image exif data (from
#' [collect_settings_data()]) into a [QWAimages] object.
#'
#' This is the typical starting point of the `prepare_rxs_dataset` workflow.
#' The resulting [QWAimages] can be passed directly to [collect_raw_data()] and
#' to the metadata Shiny app ([launch_metadata_app()]), which enriches it with
#' site-, tree-, and dataset-level metadata to produce a full [QWAmetadata] object.
#'
#' @param df_structure Dataframe with all input filenames and data structure.
#' @param df_settings Dataframe with extracted ROXAS settings data.
#'
#' @returns A [QWAimages] object with the joined image-level metadata.
#' @seealso [extract_data_structure()], [collect_settings_data()],
#'   [QWAimages()], [write_QWAimages()]
#' @export
build_QWAimages <- function(df_structure,
                            df_settings) {
  # assert the minimal required conditions to create a QWAmetadata$images data frame
  checkmate::assert_data_frame(df_structure,
    any.missing = FALSE, min.rows = 1, .var.name = "df_structure")
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
  check_structure(df_structure)

  df_rxsmeta <- df_structure |>
    dplyr::left_join(df_settings, by = 'fname_settings')

  # auto-detect schema version from the software column
  schema_name <- "images"
  rv <- resolve_roxas_version(df_rxsmeta)

  # compliance with base schema
  schema_path <- system.file(schema_rel_path(rv), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  
  # align columns and types to schema (coerce with warnings)
  df_rxsmeta <- align_to_schema(df_rxsmeta, tbl_props, rv, add_opt = FALSE, mute_info = FALSE)
   # silent mode except for important warnings

  # validate against the base schema, but warn only. user may want to fix
  # things manually in the metadata app
  check_schema(df_rxsmeta, schema_obj, rv, warn_only = TRUE, greedy = FALSE)

  cli::cli_inform(c(
    "v" = "Available ROXAS metadata extracted to {.var QWAimages} object"
  ))

  new_QWAimages(df_rxsmeta, roxas_version = rv)
}
