#' Extract the hierarchical data structure from ROXAS file names
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
#' proposed by [Fonti et al. (2025)](https://doi.org/10.3389/fpls.2025.1505389).
#' 
#' Not all hierarchical components are mandatory in the pattern, as long as the
#' images can be uniquely identified (e.g. if exactly one woodpiece was
#' extracted per tree, tree and woodpiece identifiers might be identical---in 
#' that case, capture the highest possible hierarchical level in the pattern).
#' If all images pertain to the same site and/or the same species, the site label
#' or species code can alternatively be provided as input parameters.
#' A site identifier (via pattern or site_label) is always required, while the
#' species code is optional. It is recommended to use the ITRDB species codes. 
#' 
#' [extract_data_structure()] is a wrapper around 
#' [get_structure_from_filenames()] that uses the data frame of ROXAS (AI) 
#' files from [get_roxas_files()] as input, applies the hierarchy extraction 
#' to the column `prefix`, and appends the original file paths as columns to 
#' the returned structure data frame.
#'
#' @param filenames The character vector of file names or prefixes.
#' @param pattern A regex with named groups (site, species, tree, slide and/or
#'   image) to describe the labeling pattern of the `filenames`.
#' @param site_label Optional site label to be used if it is not part of the
#'   `pattern`, only if all images are from the same site.
#' @param species_code Optional species code to be used if it is not part of 
#'   the `pattern`, only if all images are from the same species.
#' @returns A data frame containing the extracted data structure (with the
#'   original file name columns appended for [extract_data_structure()]).
#' @export
#' @examples
#' # the common pattern {site}_{species}_{tree}_{slide}_{image}
#' pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
#' filenames <- c(
#'   "path/to/file/SITEA_PISY_01_1_1.jpg",
#'   "path/to/file/SITEA_PISY_01_1_2.jpg",
#'   "path/to/file/SITEA_PISY_01_2_1.jpg",
#'   "path/to/file/SITEB_LASI_02_1_1.jpg",
#'   "path/to/file/SITEB_LASI_03_1_1.jpg"
#' )
#' get_structure_from_filenames(filenames, pattern)
#'
#' # pattern of the form {tree}-{slide}_{image}
#' # NOTE: filenames without full paths or extension also work
#' filenames <- c(
#'  "tree1-sl1_img1",
#'  "tree1-sl2_img1",
#'  "tree2-sl1_img1",
#'  "tree2-sl1_img2"
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
  checkmate::assert_string(site_label, min.chars = 1, null.ok = TRUE)
  checkmate::assert_string(species_code, min.chars = 1, null.ok = TRUE)
  # require EITHER "site" group in pattern OR site_label as input
  checkmate::assert_true(
    xor(
      stringr::str_detect(pattern, "\\(\\?<(site)>[^)]+\\)"), 
      !is.null(site_label)
    ),
    .var.name = "site label required either in pattern or as input parameter"
  )
  # if species_code provided then "species" not in pattern (to avoid conflicts)
  checkmate::assert_false(
    !is.null(species_code) &&
      stringr::str_detect(pattern, "\\(\\?<(species)>[^)]+\\)"),
    .var.name = "species code either in pattern or as input parameter"
  )

  # replace group regex with {group} for human readable pattern in messages
  lbl_structure <- stringr::str_replace_all(pattern, "\\(\\?<(\\w+)>[^)]+\\)", "{\\1}")

  # extract the relevant pattern part of the file names
  fnames <- stringr::str_extract(filenames, pattern)

  if (any(is.na(fnames))) {
    dontmatch <- filenames[is.na(fnames)]
    cli::cli_abort(c(
      "Each filename must match the given {.var pattern}.",
      "x" = "{length(dontmatch)} file{?s} do{?es/} not follow the structure",
      " " = "{.code {lbl_structure}}:",
      cli_truncated_list(dontmatch)
    ))
  }

  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates) > 0) {
    dup_groups <- split(filenames[fnames %in% duplicates], fnames[fnames %in% duplicates])
    cli::cli_abort(c(
      "Extracted column {.var image_label} must have unique values.",
      "x" = "The following files lead to duplicate image identifiers based on",
      " " = "the structure {.code {lbl_structure}}:",
      cli_truncated_groups(dup_groups)
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
  df_structure <- stringr::str_match(fnames, pattern) |>
    as.data.frame() |> 
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
    image = character(0)
  ) |> # ensure all columns are present
    dplyr::bind_rows(df_structure) |> 
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

  uc <- df_structure |> dplyr::summarise(
    n_imgs = dplyr::n_distinct(.data$image_label),
    n_slides = dplyr::n_distinct(.data$slide_label),
    n_wps = dplyr::n_distinct(.data$woodpiece_label),
    n_trees = dplyr::n_distinct(.data$tree_label),
    n_species = dplyr::n_distinct(.data$species_code),
    n_sites = dplyr::n_distinct(.data$site_label),
  )
  if (all(is.na(df_structure$species_code))) {
    uc$n_species <- NA
  }

  cli::cli_inform(c(
    "v" = "Identified {uc$n_imgs} image{?s} from {uc$n_slides} slide{?s} > {uc$n_wps} woodpiece{?s} > {uc$n_trees} tree{?s} of {uc$n_species} species at {uc$n_sites} site{?s}.",
    "i" = "The {uc$n_wps} derived woodpiece label{?s} are:",
    cli_truncated_list(unique(df_structure$woodpiece_label), max_show = 5)
  ))

  df_structure
}

#' @rdname get_structure_from_filenames
#' @param files The data frame of prefixes and file paths returned by [get_roxas_files()].
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
  df_structure <- get_structure_from_filenames(
    files$prefix, pattern, site_label, species_code
  ) |> 
    dplyr::bind_cols(files |> dplyr::select(!"prefix"))

  df_structure
}
