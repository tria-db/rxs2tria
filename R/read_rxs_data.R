#' Read single cell or rings measurements file
#'
#' Helper function to read a single ROXAS (AI) output (table) file
#'
#' @param filename filename to be read
#' @param selcols character vector of the required columns to keep
#' @param colname_variants named character vector of any variant column names to rename
#' @param delim File delimiter (usually `"\t"` for ROXAS, `";"` for ROXAS AI).
#' @returns A dataframe with the raw data (relevant columns only, cleaned names).
#' @keywords internal
read_output_file <- function(filename, selcols, colname_variants, delim) {
  # safely read in the raw data while catch errors
  tryCatch(
    {
      sel_vars_wrapper <- function() { # to avoid tidyselect warning
        dplyr::all_of(selcols)
      }
      df_raw <- vroom::vroom(filename, delim = delim,
                             col_types = c(.default = "d"),
                             col_select = sel_vars_wrapper(),
                             progress = FALSE)
      df_raw <- df_raw |> 
        dplyr::rename(dplyr::any_of(colname_variants)) |> 
        janitor::clean_names() # includes tolower
      return(df_raw)
    },
    error = function(e) {
      cli::cli_inform(c( 
        x = "An error occurred while reading file {.file {filename}}",
        " " = e$message
      ))
      return(data.frame(year = NA_integer_)) # return a single NA row for this file
    }
  )
}


#' Collect raw output data from multiple files
#'
#' Read and combine data from multple cells/rings data files (i.e. the
#' ROXAS output txts or ROXAS AI table csvs with the raw measurements).
#' Used by [collect_raw_data()] to read each `QWAdata` component.
#'
#' @param df_structure Data frame containing filenames (`$fname_cells` or `$fname_rings`) 
#'   and image labels (`$image_label`).
#' @param roxas_version The software used to create the files (`"roxas"` or `"roxas_ai"`).
#' @param ftype which type of files should be read (`"cells"` or `"rings"`).
#' @returns A data frame with the relevant raw measurements data from all
#'   files combined.
#' @seealso [collect_raw_data()]
#' @export
collect_raw_outputs <- function(df_structure, roxas_version, ftype) {
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_choice(roxas_version, c("roxas", "roxas_ai"))
  checkmate::assert_choice(ftype, c("cells", "rings"))

  # TODO: move hardcoded var names to schema files?
  if (ftype == "cells") {
    # specify the columns we expect and require in a ROXAS (AI) cells output file
    # NOTE: should have these columns for ROXAS versions
    #   3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
    # as well as for ROXAS AI
    selcols <- c(
      'YEAR', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
      'NBRNO', 'NBRID',
      'LA', 'ASP', 'MAJAX', 'KH',
      'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
      'RTSR', 'CTSR', 'DH', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
      # not included cols are (ROXAS only):
      # ID, CID, RADDIST, ANGLE, XCAL, YCAL, 'NBRDST', 'AOI' (superfluous)
    )

    # define any variant name mappings from old ROXAS versions
    # use format: current_name = 'old_name', current_name = 'older_name', etc.
    colname_variants <- c(
      TB2 = 'BEND',
      TB2 = 'CRI',
      LA = 'CA'
    )
    checkmate::assert_subset(c('fname_cells', 'image_label'), names(df_structure))
    fname_col <- "fname_cells"
    structure_cols <- c("image_label")
  } else {
    # specify the columns we expect and require in a ROXAS rings output file
    # NOTE: it looks like we should have these columns for ROXAS versions
    # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
    selcols <- c(
      'YEAR','RA','MRW',
      'RVGI', 'RVSF', 'RGSGV', 'AOIAR', 'DH', 'DH2'
      # not included cols are (ROXAS):
      # 'CNO' # we re-calculate from cells output
      # 'ID', 'MINRW', 'MAXRW', 'MRADDIST', # not relevant
      # and the following which can be recalculated from cells if needed
      # 'CD', 'CTA', 'RCTA', 'MLA', 'MINLA', 'MAXLA', 'KH', 'KS', 'RAOIAR',
      # 'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL', 
      # 'RTSR', 'CTSR', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
      # plus other AOI / AOE related measures (not relevant)
    )
    # ROXAS AI also has:
    #  YEAR;RA;MRW;RVGI;RVSF;RGSGV;DH;DH2; 
    #  TODO: WHAT ABOUT AOIAR?
    # and not included
    #  CNO;CD;CTA;RCTA;MLA;ID; MINLA;MAXLA;KH;KS;
    #  CWTPI;CWTBA;CWTLE;CWTRI;CWTTAN;CWTRAD;CWTALL;
    #  RTSR;CTSR;DRAD;DTAN;TB2;CWA;RWD;
    # in addition
    #   RBXY; <- ring boundary arrays
    #   cells_above;enabled <- first for sorting, second is incomplete flag?
    # TODO: use enabled as incomplete?
    if (roxas_version == "roxas_ai") {
      selcols <- selcols[selcols != "AOIAR"]
    }

    colname_variants <- c(
      DH_W = 'DH', # DH is actually hydraulically weighted mean diameter (Kolb & Sperry, 1998)
      DH_M = 'DH2' # while DH2 is mean hydraulic diameter (Tyree & Zimmermann, 2002)
    )

    checkmate::assert_subset(c('fname_rings', 'image_label'), names(df_structure))
    fname_col <- "fname_rings"
    structure_cols <- c("woodpiece_label", "slide_label", "image_label")
  }

  delim <- if (roxas_version == "roxas") "\t" else ";"

  df_raw_all <- df_structure |> 
    dplyr::select(dplyr::all_of(c(structure_cols,fname_col))) |> 
    dplyr::mutate(raw_data = purrr::map(
      .data[[fname_col]],
      \(x) read_output_file(x, selcols, colname_variants, delim),
      .progress = list(name = glue::glue("Reading {ftype} data files..."), clear = TRUE)
    )) |> 
    tidyr::unnest("raw_data") |> 
    dplyr::arrange(dplyr::pick("image_label", "year"))

  # check for any files that could not be read properly
  df_failed <- df_raw_all |>
    dplyr::filter(is.na(.data$year))
  if (nrow(df_failed) > 0) {
    failed_files <- df_structure |>
      dplyr::filter(.data$image_label %in% df_failed$image_label) |>
      dplyr::select("image_label", {{fname_col}})
    cli::cli_warn(c(
      "{nrow(failed_files)} {ftype} {cli::qty(nrow(failed_files))} file{?s} could not be read properly.",
      "Please check the raw file{?s} for the following {nrow(failed_files)} image label{?s}:",
      failed_files$image_label
    ))
  } else {
    cli::cli_alert_success("All {ftype} data files read successfully")
  }

  df_raw_all |>
    dplyr::select(!{{fname_col}}) |>
    dplyr::filter(!is.na(.data$year))
}


#' Collect raw cells and rings output data
#'
#' Read and combine data from all raw cells and ring data files.
#' The input data frame with filenames plus data structure can
#' be a `QWAimages` object derived with [build_QWAimages()],
#' or the output from the earlier [extract_data_structure()] step.
#'
#' The resulting [QWAdata] should then be passed through the additional
#' preprocessing steps descriped in the `prepare_rxs_dataset` template
#' (i.e., [remove_outliers()], [complete_cell_measures()] and 
#' [validate_QWA_data()]).
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @param roxas_version The software used to create the files (`"roxas"` or `"roxas_ai"`).
#'   Required only if `df_structure` is not a `QWAimages` object.
#' @returns A (non-validated) `QWAdata` object containing the combined 
#' raw data for cells and rings in `$cells` and `$rings`, respectively.
#' @seealso [build_QWAimages()], [extract_data_structure()],
#' [collect_raw_outputs()], [QWAdata()], [remove_outliers()], 
#' [complete_cell_measures()], [validate_QWA_data()]
#' @export
collect_raw_data <- function(df_structure, roxas_version = NULL){
  checkmate::assert_data_frame(df_structure)
  checkmate::assert_subset(
    c("fname_cells", 'fname_rings', 'woodpiece_label',
      'slide_label', 'image_label'),
    names(df_structure))

  if (inherits(df_structure, "QWAimages")) {
    roxas_version <- attr(df_structure, "roxas_version")
  } else {
    checkmate::assert_choice(roxas_version, c("roxas", "roxas_ai"))
  }

  df_cells_all <- collect_raw_outputs(df_structure, roxas_version, "cells")
  df_rings_all <- collect_raw_outputs(df_structure, roxas_version, "rings")

  new_QWAdata(cells = df_cells_all, rings = df_rings_all)
}

