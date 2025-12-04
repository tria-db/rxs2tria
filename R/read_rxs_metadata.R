#' Identify ROXAS data files
#'
#' Given a path to a directory, returns a nested list with the full filepaths of
#' all required ROXAS files (images, cell outputs, ring outputs, settings) found
#' within (including subdirectories).
#'
#' @param path_in path of the input directory.
#'
#' @returns A list of lists containing filepaths of images and ROXAS output
#'          files (cells, rings, settings).
#' @export
get_roxas_files <- function(path_in) {
  checkmate::assert_directory_exists(path_in)

  # regex patterns to be matched by the different ROXAS files
  # might include annotated images, etc., these are filtered out with keywords
  pattern_cell_files = "_Output_Cells\\.txt$"
  pattern_ring_files = "_Output_Rings\\.txt$"
  pattern_settings_files = "_ROXAS_Settings\\.txt$"
  pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"
  imgfiles_exclude_keywords = c("annotated", "ReferenceSeries", "Preview")
  pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse="|")

  files_cells <- list.files(
    path_in,
    pattern = pattern_cell_files,
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  files_rings <- list.files(
    path_in,
    pattern = pattern_ring_files,
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  files_settings <- list.files(
    path_in,
    pattern = pattern_settings_files,
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  files_images <- list.files(
    path_in,
    pattern = pattern_orgimg_files,
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  ) |> stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)

  # check: if the patterns are removed, the four file lists should match
  l_files <- list(
    sub(pattern_cell_files, "", files_cells, ignore.case = TRUE),
    sub(pattern_ring_files, "", files_rings, ignore.case = TRUE),
    sub(pattern_settings_files, "", files_settings, ignore.case = TRUE),
    sub(pattern_orgimg_files, "", files_images, ignore.case = TRUE)
  )
  all_fnames <- Reduce(union, l_files)

  # stop if there are no ROXAS files at all
  if (length(all_fnames) == 0) {
    cli::cli_abort(c(
      "Can't find any ROXAS files under {.var path_in}",
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
      "ROXAS file mismatch detected",
      "i" = "Four ROXAS files (image, cells, rings, settings) are required per image.",
      "x" = "The following path snippets do not yield complete sets of ROXAS files:",
      dontmatch
    ))
  }

  cli::cli_inform(c(
    "v" = "ROXAS filepaths extracted from {.var path_in}",
    "i" = "Found {length(files_images)} image{?s} with associated ROXAS files."
  ))

  list(
    fname_image = files_images,
    fname_cells = files_cells,
    fname_rings = files_rings,
    fname_settings = files_settings
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
#' or if all data is from the same site, there might not be a site identifier
#' in the name).
#' Iff all images pertain to the same site and/or the same species, and these
#' components are not part of the labeling pattern, they may also be provided
#' via the `site_label` and `species_code` input parameters.
#' Here, `species_code` should follow the standard ITRDB species codes,
#' c.f. [webapps.wsl.com/tria/#/about](https://webapps.wsl.com/tria/#/about).
#'
#' [extract_data_structure()] is a wrapper around [get_structure_from_filenames()]
#' that uses the list of ROXAS files from [get_roxas_files()] as input,
#' applies the hierarchy extraction to the image file names,
#' and adds the original file names to the resulting dataframe.
#'
#' @param files The vector of file names.
#' @param pattern The labeling pattern followed by the file names, as a regex
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
#' files <- c(
#'   "SITEA_PISY_01A_1_1.jpg",
#'   "SITEA_PISY_01A_1_2.jpg",
#'   "SITEA_PISY_01A_2_1.jpg",
#'   "SITEB_LASI_02A_1_1.jpg",
#'   "SITEB_LASI_02B_1_1.jpg"
#' )
#' get_structure_from_filenames(files)
#'
#' # custom pattern of the form {tree}_{slide}_{image}
#' files <- c(
#'  "tree1_sl1_img1.jpg",
#'  "tree1_sl2_img1.jpg",
#'  "tree2_sl1_img1.jpg",
#'  "tree2_sl1_img2.jpg"
#'  )
#'  pattern <- "(?<tree>[:alnum:].+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"
#'  get_structure_from_filenames(
#'    files, pattern,
#'    site_label = "SITEA", species_code = "PISY")
get_structure_from_filenames <- function(
    filenames,
    pattern = "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
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

  if (any(is.na(fnames))){
    dontmatch <- filenames[is.na(fnames)]
    cli::cli_abort(c(
      "All files must match the labeling pattern",
      "x" = "There {?is/are} {length(dontmatch)} file{?s} not following",
      "x" = "{.code {lbl_structure}}:",
      dontmatch
    ))
  }

  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates > 0)) {
    cli::cli_abort(c(
      "Extracted structure must yield unique image identifiers",
      "x" = "The following files yield duplicate image identifiers",
      "x" = "when extracting the pattern {.code {lbl_structure}}:",
      grep(paste(duplicates, collapse="|"), filenames, value=TRUE)
    ))
  }

  # extract the matched pattern groups and collect info into df
  df_structure <- as.data.frame(stringr::str_match(fnames,pattern)) |>
    dplyr::rename(org_img_name = V1) # original pattern is in column 1

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

  if (!is.null(site_label)){
    df_structure$site <- site_label
  }
  if (!is.null(species_code)){
    df_structure$species <- species_code
  }

  df_structure <- df_structure |>
    tidyr::unite('tree_label', site:tree,
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('woodpiece_label', site:woodpiece,
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('slide_label', site:slide,
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    tidyr::unite('image_label', site:image,
                 sep = '_', na.rm = TRUE, remove = FALSE) |>
    dplyr::select(image_label, slide_label, woodpiece_label,
                  tree_label, species, site, org_img_name) |> # reorder columns
    dplyr::rename(species_code = species, site_label = site)

  df_structure
}

#' @rdname get_structure_from_filenames
#' @param files The list of vectors with ROXAS file names.
#' @export
extract_data_structure <- function(
    files,
    pattern = "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
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

  df_structure <- dplyr::bind_cols(df_structure, files) # bind filename columns

  cli::cli_inform(c(
    "v" = "Data structure extracted from ROXAS filenames",
    "i" = "Identified the following {length(unique(df_structure$woodpiece_label))} woodpiece{?s}:",
    unique(df_structure$woodpiece_label)
  ))

  df_structure
}


#' Read and combine image exif data
#'
#' Collect the exif data from all image files
#'
#' @param files_images Vector/list of image filenames
#'
#' @returns A dataframe containing the extracted data.
#' @export
# TODO: check this works on Windows? (exifr requires PERL)
# TODO: is it robust for different image types? (the exif tags)
# TODO: can get date as well if we have original images? error handling for missing tags?
collect_image_info <- function(files_images) {
  checkmate::assert_file_exists(
    files_images,
    extension = c("jpg","jpeg","png","tiff","bmp","gif"))

  df_image_meta <- exifr::read_exif(files_images,
                                    tags = c(
                                      "FileType", "FileSize",
                                      "ImageWidth", "ImageHeight")) |>
    dplyr::rename(fname_image = SourceFile,
                  img_filetype = FileType,
                  img_size = FileSize,
                  img_width = ImageWidth,
                  img_height = ImageHeight
    )

  df_image_meta
}


#' Extract data from a ROXAS settings file
#'
#' Helper function to read and extract the relevant metadata from a single ROXAS
#' settings file
#'
#' @param file_settings The file to be read.
#' @param roxas_version The version of ROXAS used to create the file (classic, AI)
#'
#' @returns A dataframe containing the extracted data.
#' @keywords internal
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655 (different date formats)
# TODO: add support for ROXAS AI
extract_roxas_settings <- function(file_settings,
                                   roxas_version = "classic") {
  if (roxas_version == "classic"){
    # read from a single settings file
    df_settings <- readr::read_delim(file_settings,
                                     delim = "\t",
                                     col_types = readr::cols(.default = "c", RNUM = "d"))

    # NOTE: this relies heavily on the consistent layout of the settings file
    # in particular, we need tab delimiters, columns RNUM, SETTING, DESCRIPTION
    # and the right values in the rows 8,9,10,12,13,17,20,31,33,166,203:208!
    df_settings <- df_settings |>
      dplyr::filter(RNUM %in% c(8,9,10,
                                12,13,
                                17,18,19,20,
                                31,33,166,
                                203,204,205,206,207,208
                                )) |>
      dplyr::mutate(new_names = c(
        "configuration_file", "created_at", "sw_version",
        "spatial_resolution", "origin_calibrated",
        "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
        "min_cell_area", "max_cell_area", "dbl_cwt_threshold",
        "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_window", "maxrel_opp_cwt",
        "max_cwttan_s", "max_cwttan_l"
      )) |>
      dplyr::select(SETTING, new_names) |>
      tidyr::pivot_wider(names_from = new_names, values_from = SETTING) |>
      dplyr::mutate(
        meas_geometry = dplyr::if_else(meas_geometry==1, "linear", "circular"),
        fname_settings = file_settings,
        software = "ROXAS") |>
      tidyr::separate(origin_calibrated,
                      into = c("origin_calibrated_x", "origin_calibrated_y"),
                      sep = "[ ]*/[ ]*", remove = TRUE, convert = TRUE) |>
      dplyr::relocate(fname_settings, software, sw_version)
  } else {
    cli::cli_abort("Only classical ROXAS supported atm.")
  }

  df_settings
}


#' Read and combine ROXAS settings data
#'
#' Collect the settings data from all ROXAS settings files
#'
#' @param files_settings Vector/list of ROXAS settings filenames
#' @param roxas_version The version of ROXAS used to create the files (classic, AI)
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_settings_data <- function(files_settings,
                                  roxas_version = 'classic') {
  checkmate::assert_file_exists(
    files_settings,
    extension = c("txt"))
  checkmate::assert_subset(roxas_version, c("classic"))

  df_settings_all <- files_settings |>
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version)) |>
    purrr::list_rbind()

  # convert columns to numeric and integer
  df_settings_all <- df_settings_all |>
    dplyr::mutate(dplyr::across(c(spatial_resolution,
                                  dbl_cwt_threshold:max_cwttan_l), as.numeric),
                  dplyr::across(circ_lower_limit:max_cell_area, as.integer))

  df_settings_all
}

#' Handle date conversion for ROXAS settings created at dates
#'
#' The Date created field in the raw ROXAS settings files may have different
#' formats depending on the ROXAS version and user locale.
#'
#' @param date_strings Vector of date strings to be converted.
#' @param orders One or more date formats to be used for parsing,
#' see [lubridate::parse_date_time()].
#' @param tz Timezone for the converted dates (default: system timezone).
#' @param exact Whether to enforce exact matching of the provided format(s), or
#' allow lubridate some flexibility to guess.
#'
#' @returns A vector of POSIXct dates.
#' @export
convert_settings_dates <- function(date_strings,
                                  orders = c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M"),
                                  tz = Sys.timezone(),
                                  exact = TRUE) {
  checkmate::assert_character(date_strings, any.missing = FALSE)
  checkmate::assert_character(orders, any.missing = FALSE)
  checkmate::assert_string(tz)

  conv_dates <- date_strings %>%
    lubridate::parse_date_time(., orders = orders, tz=tz)

  # plausibility checks: no NAs and not in future
  check_dates <- any(is.na(conv_dates)) || any(conv_dates > Sys.time())
  if (check_dates){
    cli::cli_abort(c(
      "Error converting dates from the ROXAS settings files",
      "x" = "Please check the format of the Date created field in the raw files."
    ))
  }

  conv_dates
}


#' Combine raw ROXAS metadata
#'
#' Combine the extracted ROXAS filenames plus data structures (from
#' [extract_data_structure()]), image exif data (from [collect_image_info()]),
#' and ROXAS settings data (with converted dates, cf. [collect_settings_data()]
#' and [convert_settings_dates()]) into one raw metadata dataframe.
#'
#' @param df_structure Dataframe with all input filenames and data structure.
#' @param df_images Dataframe with extracted image exif data.
#' @param df_settings Dataframe with extracted ROXAS settings data.
#'
#' @returns A single dataframe consisting of the joined input dataframes.
#' @export
combine_rxs_metadata <- function(df_structure,
                                 df_images,
                                 df_settings) {
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_subset(
    c('fname_image','fname_cells','fname_rings','fname_settings'),
    colnames(df_structure))
  checkmate::assert_data_frame(df_images)
  checkmate::assert_subset(c('fname_image'), colnames(df_images))
  checkmate::assert_data_frame(df_settings)
  checkmate::assert_subset(c('fname_settings'), colnames(df_settings))

  df_rxsmeta <- df_structure |>
    dplyr::left_join(df_images, by='fname_image') |>
    dplyr::left_join(df_settings, by='fname_settings') |>
    dplyr::relocate(dplyr::starts_with('fname'), .after = dplyr::last_col())

  cli::cli_inform(c(
    "v" = "Available ROXAS metadata extracted to {.var df_rxsmeta}"
  ))

  df_rxsmeta
}
