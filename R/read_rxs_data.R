#' Read single cell output file
#'
#' Helper function to read a single (classical) ROXAS cells output file
#'
#' @param file_cells filename to be read
#' @param selcols_cells character vector of the required columns to keep
#' @param colname_variants named character vector of any variant column names to rename
#' @returns A dataframe with the raw data (relevant columns only).
#' @keywords internal
# TODO: adapt to use cell data schema as basis?
# TODO: adapt to ROXAS AI?
read_cells_output <- function(file_cells, selcols_cells, colname_variants){
  # read in the raw data
  # catch errors: print the current filename if there are issues
  tryCatch(
    {
      df_raw <- vroom::vroom(file_cells, delim = "\t",
                             col_types = c(.default="d", ID="c")) %>%
        dplyr::rename(dplyr::any_of(colname_variants)) %>%
        dplyr::select(dplyr::all_of(selcols_cells)) %>%
        janitor::clean_names()
      return(df_raw)
    },
    error = function(e){
      cli::cli_inform(c(
        x = "An error occurred while reading file {.file {file_cells}}",
        " " = e$message
      ))
      return(data.frame(year = NA_integer_))
    }
  )
}


#' Collect raw cells output data
#'
#' Read and combine data from all cell output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns A dataframe with the raw data from all files combined (relevant columns only).
#' @export
collect_cells_data <- function(df_structure){
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_subset(c('fname_cells', 'image_label'), names(df_structure))

  selcols_cells <- c(
    'YEAR', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
    'NBRNO', 'NBRID',
    'LA', 'ASP', 'MAJAX', 'KH',
    'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
    'RTSR', 'CTSR', 'DH', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
    # not included cols are:
    # ID, CID, RADDIST, ANGLE, XCAL, YCAL, 'NBRDST', 'AOI', (superfluous)

    # if all read, then  df_cells_all %>% purrr::discard(~all(is.na(.x))) might be interesting
    #  (relates to areas of interest)
  )
  # specify the columns we expect and require in a ROXAS cells output file
  # NOTE: it looks like we should have these columns for ROXAS versions
  # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655

  # define any variant name mappings from old ROXAS versions
  # use format: current_name = 'old_name', current_name = 'older_name', etc.
  colname_variants <- c(
    TB2 = 'BEND',
    TB2 = 'CRI',
    LA = 'CA'
  )

  df_cells_all <- df_structure %>%
    dplyr::select(image_label, fname_cells) %>%
    dplyr::mutate(raw_data = purrr::map(
      fname_cells,
      \(x) read_cells_output(x, selcols_cells, colname_variants)
    )) %>%
    tidyr::unnest(raw_data) %>%
    dplyr::select(-fname_cells) |>
    dplyr::arrange(image_label, year)

  # check for any files that could not be read properly
  df_cells_failed <- df_cells_all |>
    dplyr::filter(is.na(year))
  if(nrow(df_cells_failed) > 0){
    failed_files <- df_structure |>
      dplyr::filter(image_label %in% df_cells_failed$image_label) |>
      dplyr::select(image_label, fname_cells)
    cli::cli_warn(c(
      "{nrow(failed_files)} file{?s} could not be read properly.",
      "Please check the raw file{?s} for the following {nrow(failed_files)} image label{?s}:",
      failed_files$image_label
    ))
  } else {
    cli::cli_alert_success("All ROXAS cells output files read successfully.")
  }

  df_cells_all |>
    dplyr::filter(!is.na(year))
}


#' Read single rings output file
#'
#' Helper function to read a single (classical) ROXAS rings output file
#'
#' @param file_rings filename to be read
#' @param selcols_cells character vector of the required columns to keep
#' @param colname_variants named character vector of any variant column names to rename
#' @returns A dataframe with the raw data (relevant columns only).
#' @keywords internal
read_rings_output <- function(file_rings, selcols_rings, colname_variants){
  tryCatch(
    {
      df_raw <- vroom::vroom(file_rings, delim = "\t",
                             col_types = c(.default="d", ID="c")) %>%
        dplyr::select(dplyr::all_of(selcols_rings)) %>%
        dplyr::rename(dplyr::any_of(colname_variants)) %>%
        janitor::clean_names()
      return(df_raw)
    },
    error = function(e){
      cli::cli_inform(c(
        x = "An error occurred while reading file {.file {file_rings}}",
        " " = e$message
      ))
      return(data.frame(year = NA_integer_))
    }
  )
}


#' Collect raw rings output data
#'
#' Read and combine data from all ring output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns A dataframe with the raw data from all files combined (relevant columns only).
#' @export
# TODO: adapt to use rings data schema as basis?
# TODO: adapt to ROXAS AI?
collect_rings_data <- function(df_structure){
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_subset(
    c('fname_rings', 'woodpiece_label', 'slide_label', 'image_label'),
    names(df_structure))

  # specify the columns we expect and require in a ROXAS rings output file
  # NOTE: it looks like we should have these columns for ROXAS versions
  # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
  selcols_rings <- c(
    'YEAR','RA','MRW',
    'RVGI', 'RVSF', 'RGSGV', 'AOIAR', 'DH', 'DH2'
    # not included cols are:
    # 'CNO', 'CD', 'CTA', 'RCTA', 'MLA', # we re-calculate from cells output
    # 'ID', 'MINRW', 'MAXRW', 'MRADDIST', # not relevant
    # and the following which could still be recalculated from cells if needed
    # 'MINLA', 'MAXLA', 'KH', 'KS', 'RAOIAR', 'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN',
    # 'CWTRAD', 'CWTALL', 'RTSR', 'CTSR', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD',
    # plus other AOI / AOE related measures (not relevant)
  )

  # define any variant name mappings from old ROXAS versions
  # use format: current_name = 'old_name', current_name = 'older_name', etc.
  colname_variants <- c(
    DH_W = 'DH', # DH is actually hydraulically weighted mean diameter (Kolb & Sperry, 1998)
    DH_M = 'DH2' # while DH2 is mean hydraulic diameter (Tyree & Zimmermann, 2002)
  )

  df_rings_all <- df_structure %>%
    dplyr::select(woodpiece_label, slide_label, image_label, fname_rings) %>%
    dplyr::mutate(raw_data = purrr::map(
      fname_rings,
      \(x) read_rings_output(x, selcols_rings, colname_variants))) %>%
    tidyr::unnest(raw_data) %>%
    dplyr::select(-fname_rings) |>
    dplyr::arrange(image_label, year)

  # check for any files that could not be read properly
  df_rings_failed <- df_rings_all |>
    dplyr::filter(is.na(year))
  if(nrow(df_rings_failed) > 0){
    failed_files <- df_structure |>
      dplyr::filter(image_label %in% df_rings_failed$image_label) |>
      dplyr::select(image_label, fname_rings)
    cli::cli_warn(c(
      "{nrow(failed_files)} file{?s} could not be read properly.",
      "Please check the raw file{?s} for the following {nrow(failed_files)} image label{?s}:",
      failed_files$image_label
    ))
  } else {
    cli::cli_alert_success("All ROXAS rings output files read successfully.")
  }

  df_rings_all |>
    dplyr::filter(!is.na(year))
}


#' Collect raw cells and rings output data
#'
#' Read and combine data from all cells and ring output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns `QWA_data`, i.e. a named list containing the combined raw data
#' for cells and rings in two dataframes under `$cells` and `$rings`, respectively.
#' @export
collect_raw_data <- function(df_structure){
  df_cells_all <- collect_cells_data(df_structure)
  df_rings_all <- collect_rings_data(df_structure)

  return(setNames(
    list(df_cells_all, df_rings_all),
    c('cells', 'rings')))
}


