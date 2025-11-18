#' Get list of input files
#'
#' Given a path_in, get the paths and filenames of the ROXAS files to be
#' processed.
#'
#' @param path_in path of the input directory.
#'
#' @returns A list of lists containing filepaths of images and ROXAS output
#'          files (cells, rings, settings).
#' @export
get_roxas_files <- function(path_in) {
  # Regex patterns to be matched by the different ROXAS files
  # NOTE: in addition to the original images (IMGNAME.jpg), the ROXAS output
  # might include annotated images, etc. These are filtered out with keywords.
  pattern_cell_files = "_Output_Cells\\.txt$"
  pattern_ring_files = "_Output_Rings\\.txt$"
  pattern_settings_files = "_ROXAS_Settings\\.txt$"
  pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"
  imgfiles_exclude_keywords = c("annotated",
                                "ReferenceSeries",
                                "Preview")

  # assert that path_in is valid
  checkmate::assert_directory_exists(path_in)

  # cells, rings, and settings files can be identified directly by pattern
  files_cells <- list.files(path_in,
                            pattern = pattern_cell_files,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = TRUE, include.dirs = TRUE
                            )
  files_rings <- list.files(path_in,
                            pattern = pattern_ring_files,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = TRUE, include.dirs = TRUE
                            )
  files_settings <- list.files(path_in,
                               pattern = pattern_settings_files,
                               full.names = TRUE, recursive = TRUE,
                               ignore.case = TRUE, include.dirs = TRUE
                               )

  # original image pattern is unspecific, need to exclude other image files
  pattern_excl_keywords <- paste(imgfiles_exclude_keywords, collapse="|")
  files_images <- list.files(
    path_in,
    pattern = pattern_orgimg_files,
    full.names = TRUE, recursive = TRUE,
    ignore.case = TRUE, include.dirs = TRUE
  ) |> stringr::str_subset(pattern = pattern_excl_keywords, negate = TRUE)

  # check that we did find some valid files
  l_files <- list(
    sub(pattern_cell_files, "", files_cells, ignore.case = TRUE),
    sub(pattern_ring_files, "", files_rings, ignore.case = TRUE),
    sub(pattern_settings_files, "", files_settings, ignore.case = TRUE),
    sub(pattern_orgimg_files, "", files_images, ignore.case = TRUE)
  )
  all_fnames <- Reduce(union, l_files)

  # stop if there are no ROXAS files at all
  if (length(all_fnames) == 0) {
    stop(
      "NO ROXAS output files found under path", "\n",
      paste0("  ", path_in)
    )
  }

  # identify any mismatches between ring, cell, settings, and image files
  dontmatch <- setdiff(
    all_fnames,
    Reduce(intersect, l_files)
  )

  # stop if there are any mismatches
  if (length(dontmatch) > 0) {
    stop(
      "ROXAS output files DON'T MATCH for:", "\n",
      paste(paste0("  ", dontmatch), collapse = "\n")
    )
  }

  # success!
  message(paste("Found", length(files_images),
                "images and associated ROXAS files under the given path."))

  # return all the file paths we need as lists
  return(list(
    fname_image = files_images,
    fname_cells = files_cells,
    fname_rings = files_rings,
    fname_settings = files_settings
    )
  )
}


#' Extract the data structure from input files
#'
#' Extract the structure of the data (i.e., which images belong to which slide,
#' woodpiece, tree, site) from the filenames of the input data into a dataframe.
#' This requires that all files follow the same labeling pattern.
#' The extracted site, species, tree, woodpiece, slide, image identifiers are
#' joined by underscores to derive unique codes for each level of the data
#' structure for the subsequent calculations
#' (e.g. `tree_code = {site}_{species}_{tree}`).
#'
#' For the default, we assume that all files contain the following pattern:
#' `{site}_{species}_{tree}{woodpiece}_{slide}_{image}`,
#' where tree is a two-character code, and woodpiece is optional, as suggested
#' in [Fonti et al. (2025)](https://doi.org/10.3389/fpls.2016.00781).
#'
#' We already checked that the ROXAS cells, rings, settings and images
#' filenames match in [get_roxas_files()], so it is ok to do the pattern
#' extraction on the image filenames only.
#'
#' The filenames should identify the site, species, tree, woodpiece, slide, image
#' such that each image file is uniquely identified by the combination of these
#' It's ok if not all components are present in the pattern, as long as the images
#' can be uniquely identified (e.g. if exactly one woodpiece was extracted per tree,
#' there might not be a specific woodpiece identifier, or if all data is from the
#' same site, there might not be a site identifier in the name). So
#' - tree is a required components of the pattern
#' - site is optional iff there is only one site. In that case, provide the `site_label` as an input parameter
#' - woodpiece, slide and/or image are optional iff there is exactly one woodpiece per tree, resp. one slide per woodpiece, resp. one image per slide.
#' - species is optional. Can also be provided as an input parameter iff there is only one species.
#'
#' @param files The list of vectors with input filenames.
#' @param pattern The labeling pattern followed by the original image filenames,
#' as a regex with named groups.
#' @param site_label Optional site code to be used if it is not part of the pattern,
#' only if all images are from the same site.
#' @param species_code Optional species code to be used if it is not part of the pattern,
#' only if all images are from the same species.
#'
#' @returns A dataframe containing (the different filenames and) data structure.
#' @export
extract_data_structure <- function(files,
                                   pattern = "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
                                   site_label = NULL, species_code = NULL) {

  img_filenames <- files$fname_image
  df_structure <- get_structure_from_filenames(img_filenames,
                                               pattern,
                                               site_label,
                                               species_code)

  df_structure <- dplyr::bind_cols(df_structure, files) # add filenames

  # report the identified woodpieces and beep successful ending of the function
  # TODO: woodpiece_code versus org label?
  message("Data structure successfully extracted from ROXAS filenames\n",
          "for the following ",
          length(unique(df_structure$woodpiece_label)),
          " woodpiece labels:\n",
          paste(paste0(' ', unique(df_structure$woodpiece_label)), collapse = "\n"))

 df_structure
}

#' @rdname extract_data_structure
#' @param filenames Vector of filenames to be processed.
#' @export
get_structure_from_filenames <- function(filenames,
                                         pattern = "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
                                         site_label = NULL, species_code = NULL){
  # replace group regex with {group} for human readable pattern in error message
  lbl_structure <- stringr::str_replace_all(pattern, "\\(\\?<(\\w+)>[^)]+\\)", "{\\1}")

  # assert that at least tree and site can be inferred
  checkmate::assert_string(pattern, pattern = "tree")
  checkmate::assert_string(site_label %??% pattern, pattern = site_label %??% "site") # %??% is checkmate's coalesce operator

  # extract the relevant pattern part of the filenames
  fnames <- stringr::str_extract(filenames, pattern)

  # check that all fnames match the pattern
  if (any(is.na(fnames))){
    stop("The label structure of the files is not consistent.\n",
         "  All labels must be composed following the structure:\n",
         paste0("    ", lbl_structure)
    )
  }

  # check that we have no duplicate image labels (e.g. from different subdirectories)
  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates > 0)) {
    stop(
      "There are duplicate extracted image labels, please remove or rename:", "\n",
      paste(paste0("  ", grep(paste(duplicates, collapse ='|'),
                              filenames, value = TRUE)),
            collapse = "\n")
    )
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
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(.x,"")))

  if (!is.null(site_label)){
    if (!all(is.na(df_structure$site))) {
      warning("Replacing extracted site with provided site_label input parameter.")
    }
    df_structure$site <- site_label
  }
  if (!is.null(species_code)){
    if (!all(is.na(df_structure$species))) {
      warning("Replacing extracted species with provided species_code input parameter.")
    }
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


#' Read and combine image exif data
#'
#' Collect the exif data from all image files
#'
#' @param files_images Vector/list of image filenames
#'
#' @returns A dataframe containing the extracted data.
#'
# TODO: check this works on Windows? (exifr requires PERL)
# TODO: is it robust for different image types?
# TODO: can get date as well if we have original images? error handling for missing tags?
collect_image_info <- function(files_images) {
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
#'
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655 (different date formats)
# TODO: add support for ROXAS AI
extract_roxas_settings <- function(file_settings,
                                   roxas_version = 'classic') {
  # check input
  checkmate::assert_subset(roxas_version, c('classic'))

  # read from a single settings file
  df_settings <- readr::read_delim(file_settings,
                        delim = "\t",
                        col_types = readr::cols(.default = "c", RNUM = "d"))

  # extract and reformat the relevant info
  if (roxas_version == 'classic'){
    # NOTE: this relies heavily on the consistent layout of the settings file
    # in particular, we need tab delimiters, columns RNUM, SETTING, DESCRIPTION
    # and the right values in the rows 8,9,10,12,13,17,20,31,33,166,203:208!
    df_settings <- df_settings %>%
      dplyr::filter(RNUM %in% c(8,9,10,
                                12,13,
                                17,18,19,20,
                                31,33,166,
                                203,204,205,206,207,208
                                )) %>%
      dplyr::mutate(new_names = c(
        "configuration_file", "created_at", "sw_version",
        "spatial_resolution", "origin_calibrated",
        "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
        "min_cell_area", "max_cell_area", "dbl_cwt_threshold",
        "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_window", "maxrel_opp_cwt",
        "max_cwttan_s", "max_cwttan_l"
      )) %>%
      dplyr::select(SETTING, new_names) %>%
      tidyr::pivot_wider(names_from = new_names, values_from = SETTING) %>%
      dplyr::mutate(
        meas_geometry = dplyr::if_else(meas_geometry==1, "linear", "circular"),
        fname_settings = file_settings,
        software = "ROXAS") %>%
      tidyr::separate(origin_calibrated,
                      into = c("origin_calibrated_x", "origin_calibrated_y"),
                      sep = "[ ]*/[ ]*", remove = TRUE, convert = TRUE) %>%
      dplyr::relocate(fname_settings, software, sw_version)
  }

  return(df_settings)
}


#' Read and combine ROXAS settings data
#'
#' Collect the settings data from all ROXAS settings files
#'
#' @param files_settings Vector/list of ROXAS settings filenames
#' @param roxas_version The version of ROXAS used to create the files (classic, AI)
#'
#' @returns A dataframe containing the extracted data.
#'
collect_settings_data <- function(files_settings,
                                  roxas_version = 'classic',
                                  tz = NULL) {
  df_settings_all <- files_settings %>%
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version)) %>%
    purrr::list_rbind()

  # convert string columns to numeric and integer
  df_settings_all <- df_settings_all %>%
    dplyr::mutate(dplyr::across(c(spatial_resolution,
                                  dbl_cwt_threshold:max_cwttan_l), as.numeric),
                  dplyr::across(circ_lower_limit:max_cell_area, as.integer))

  # convert created_at string to datetime
  # TODO: check robustness for different date formats
  if (is.null(tz)) {
    tz <- Sys.timezone()
  }

  conv_dates <- df_settings_all$created_at %>% lubridate::parse_date_time(., c("%d.%m.%Y %H:%M:%S", "%m/%d/%Y %H:%M"), tz=tz)
  # plausibility checks: no NA and not before outmost year
  # TODO: this also checks undated?
  check_dates <- any(is.na(conv_dates)) |
    any(df_settings_all$outmost_year > lubridate::year(conv_dates))
  if (check_dates){
    stop("Error converting dates in the settings files.")
  }
  df_settings_all$created_at <- conv_dates

  return(df_settings_all)
}


#' Read and combine raw metadata
#'
#' Collect the ROXAS settings and image exif data from all raw files and
#' combine into one dataframe with the data structure.
#'
#' @param df_structure Dataframe with all input filenames
#' @param roxas_version ROXAS version (required to correctly extract info from settings files)
#' @param tz Timezone for date conversion of settings created at date (default: system tz)
#'
#' @returns A dataframe containing the extracted settings data.
#' @export
collect_metadata_from_files <- function(df_structure, roxas_version = 'classic', tz = NULL) {
  df_images <- collect_image_info(df_structure$fname_image)
  df_settings <- collect_settings_data(df_structure$fname_settings,
                                       roxas_version, tz=tz)

  df_rxsmeta <- df_structure |>
    dplyr::left_join(df_images, by='fname_image') |>
    dplyr::left_join(df_settings, by='fname_settings') |>
    dplyr::relocate(dplyr::starts_with('fname'), .after = dplyr::last_col())


  # success
  message("Available metadata successfully extracted from the raw files.")
  beepr::beep(sound = 1, expr = NULL)

  df_rxsmeta
}



# OLD STUFF: -----------------------------

#' Recover original labels from data structure
#'
#' Inverse of extract_data_structure, i.e. given a dataframe with the
#' data structure (site, species, tree, woodpiece, slide, image), and the
#' labeling pattern followed by the original image filenames, we reconstruct
#' the filenames.
#' This requires that all files follow the same labeling pattern.
#'
#' NOTE: At the moment, only tested with following pattern:
#' `{site}_{species}_{tree}{woodpiece}_{slide}_{image}`
#' where tree is assumed to be a two-character code, and woodpiece is optional.

#'
#' @param df_structure The df with the data structure.
#' @param pattern The labeling pattern followed by the original image filenames,
#' as a regex expression with named groups.
#'
#' @returns A list of reconstructed file names (without filetype extension)
# TODO: do we really need this? export? (now we have image_name in df_structure?)
reconstruct_img_names <- function(df_structure, pattern) {
  # which groups are named in the pattern?
  named_groups <- stringr::str_match_all(pattern, "\\?<(\\w+)>")[[1]][,2]

  all_groups <- c('site','species','tree','woodpiece','slide','image')

  if (length(setdiff(named_groups, all_groups)) > 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The pattern contains invalid named groups.")
  }

  # construct a template image name out of the pattern
  template <- stringr::str_replace_all(pattern, "\\^|\\$", "")  # remove start and end anchors
  template <- stringr::str_replace_all(template, "\\(\\?<(\\w+)>[^)]+\\)", "{\\1}") # replace group regex with {group}

  # get the correct columns from df_structure
  df_names <- df_structure[named_groups]
  df_names[is.na(df_names)] <- ""

  # replace groups in template with corresponding values from df_names
  file_names <- template
  for (col in named_groups) {
    file_names <- stringr::str_replace_all(file_names, paste0("\\{", col, "\\}"), df_names[[col]])
  }

  if (nunique(file_names) != nrow(df_structure)) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The reconstructed filenames are not unique, check the provided pattern.")
  }

  return(file_names)
}


#' Include or exclude woodpiece codes from data collection
#'
#' This function filters the data structure dataframe based on the woodpiece codes provided
#'
#' @param df_structure The dataframe containing all input filenames and data structure.
#' @param include_codes EITHER provide a vector of the woodpiece codes to be included
#' @param exclude_codes OR provide a vector of the woodpiece codes to be excluded
#'
#' @returns filtered df_structure.
#' @export
subset_woodpiece_codes <- function(df_structure,
                                   include_codes=NULL,
                                   exclude_codes=NULL) {
  # check that only one of the two options is used
  if (!xor(is.null(include_codes), is.null(exclude_codes))){
    beepr::beep(sound = 2, expr = NULL)
    stop("Please provide either `include_codes` or `exclude_codes`, but not both.")
  }

  # if include_codes is not null, filter for these
  if (!is.null(include_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(include_codes, df_structure$woodpiece_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(woodpiece_code %in% include_codes)
  }

  # if exclude_codes is not null, filter these out
  if (!is.null(exclude_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(exclude_codes, df_structure$woodpiece_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(!(woodpiece_code %in% exclude_codes))
  }

  return(df_struct_filt)
}
