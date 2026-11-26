# create the modal for input source selection
input_source_modal <- function(ns){
  shiny::modalDialog(
    title = "Select input source",
    shiny::tagList(
      shiny::radioButtons(
        ns("load_type"),
        "Choose input option:",
        choices = c(
          "Load data from R environment" = "env",
          "Load data from csv files" = "csv",
          "Load example data" = "example")
      ),
      shiny::hr(),
      shiny::uiOutput(ns("load_details_ui")) # dynamic
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_input"), "Proceed", class = "btn-secondary")
    )
  )
}

# create modal with confirm inputs overwrite warning
input_warning_modal <- function(ns) {
  shiny::modalDialog(
    title = "Warning",
    "This action overwrites any existing inputs provided in the app,
     including any unsaved edits. Are you sure you want to proceed?",
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_overwrite"), "Proceed", class = "btn-secondary")
    )
  )
}

# create input fields for env vars / csv paths / example data
# (dynamic part of load_details_ui)
build_env_inputs <- function(ns) {
  shiny::tagList(
    "Provide the names of the data.frames in the current R environment:",
    shiny::textInput(ns("name_prf"), "Profile data",
                     value = "prf_sector"),
    shiny::textInput(ns("name_rings"), "QWA rings data",
                     value = "QWA_data$rings"),
    shiny::textInput(ns("name_rxsmeta"), "QWA images metadata",
                     value = "rxs_images")
  )
}
build_csv_inputs <- function(ns) {
  shiny::tagList(
    "Provide the filepaths for the following:",
    shiny::fileInput(ns("file_prf"), "Upload the QWA profile data",
                     accept = c(".csv", ".gz")),
    shiny::fileInput(ns("file_rings"), "Upload the QWA rings data",
                     accept = c(".csv", ".gz")),
    shiny::fileInput(ns("file_rxsmeta"),
                     "Upload the images metadata (QWAmetadata .json or QWAimages .csv)",
                     accept = c(".json", ".csv"))
  )
}
build_example_inputs <- function(ns) {
  # TODO: implement example data selection
  shiny::tags$p(
    shiny::tags$em("Using example dataset attached to package"),
    shiny::tags$code("rxs2tria")
  )
}

#' Read an object from the global environment by name
#'
#' Supports plain names (`my_df`), list element access (`obj$element`),
#' and bracket notation (`obj[['element']]`).
#'
#' @param var_name Character string identifying the object
#' @param envir Environment of the object (default: .GlobalEnv)
#' @return The object itself
get_from_env <- function(var_name, envir = .GlobalEnv) {
  var_name <- trimws(var_name)

  # plain name
  # may contain letters, numbers, . and _, but not starting with _ or number
  if (grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", var_name)) {
    return(get(var_name, envir = envir))
  }

  # obj$element format
  # valid name + '$' + valid name (same rules as above)
  pattern1 <- "^([a-zA-Z.][a-zA-Z0-9._]*)\\$([a-zA-Z.][a-zA-Z0-9._]*)$"
  if (grepl(pattern1, var_name)) {
    obj_name <- sub(pattern1, "\\1", var_name)
    element_name <- sub(pattern1, "\\2", var_name)
    return(get(obj_name, envir = envir)[[element_name]])
  }

  # obj[['element']] or obj[["element"]] format
  # valid name + '[[' + quote (single or double) + valid name + same quote + ']]'
  # \\2 is backreference to the quote type (second group)
  pattern2 <- "^([a-zA-Z.][a-zA-Z0-9._]*)\\[\\[(['\"])([a-zA-Z.][a-zA-Z0-9._]*)\\2\\]\\]$"
  if (grepl(pattern2, var_name)) {
    obj_name <- sub(pattern2, "\\1", var_name)
    element_name <- sub(pattern2, "\\3", var_name)
    return(get(obj_name, envir = envir)[[element_name]])
  }

  stop("Unrecognized variable name format: ", var_name)
}

#' Get input data objects from global environment
#' @param name_prf Name of profile data frame object in global environment
#' @param name_rings Name of rings data frame object in global environment
#' @param name_rxsmeta Name of rxsmeta data frame object in global environment
#' @return A named list with three data frames: prf_data_in, rings_data_in,
#' rxsmeta_data_in
load_data_env <- function(name_prf, name_rings, name_rxsmeta,
                          specs = input_specs){
  if (is.na(name_prf) || name_prf == "") {
    prf_data_in <- NULL
  } else {
    prf_data_in <- get_from_env(name_prf)

    prf_cols <- split(names(specs$prf_data$req_cols),
                      specs$prf_data$req_cols)
    # NOTE: enforces existence and correct type or required cols
    prf_data_in <- prf_data_in |>
      tibble::as_tibble() |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(prf_cols[['c']]), as.character),
        dplyr::across(dplyr::all_of(prf_cols[['i']]), as.integer),
        dplyr::across(dplyr::all_of(prf_cols[['l']]), as.logical)
      )

    # remove any of the count columns (except sector_n),
    # keep only required and measurements (assumed numeric)
    prf_data_in <- prf_data_in |>
      dplyr::select(dplyr::all_of(names(specs$prf_data$req_cols)),
                    dplyr::where(is.numeric) & !dplyr::ends_with("_N"))
  }

  rings_data_in <- get_from_env(name_rings)

  rings_cols <- split(names(specs$rings_data$req_cols),
                      specs$rings_data$req_cols)
  # missing required columns are reported in validate_input_dfs()
  rings_data_in <- rings_data_in |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(rings_cols[['c']]), as.character),
      dplyr::across(dplyr::any_of(rings_cols[['i']]), as.integer),
      dplyr::across(dplyr::any_of(rings_cols[['l']]), as.logical)
    )

  if (is.na(name_rxsmeta) || name_rxsmeta == "") {
    rxsmeta_data_in <- NULL
  } else {
    rxsmeta_data_in <- get_from_env(name_rxsmeta)

    rxsmeta_cols <- split(names(specs$rxsmeta_data$req_cols),
                          specs$rxsmeta_data$req_cols)
    rxsmeta_data_in <- rxsmeta_data_in |>
      tibble::as_tibble() |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(rxsmeta_cols[['c']]), as.character),
        dplyr::across(dplyr::all_of(rxsmeta_cols[['i']]), as.integer),
        dplyr::across(dplyr::all_of(rxsmeta_cols[['l']]), as.logical)
      )

    # NOTE: force correct type for optional cols as well
    rxsmeta_cols <- split(names(specs$rxsmeta_data$opt_cols),
                          specs$rxsmeta_data$opt_cols)
    rxsmeta_data_in <- rxsmeta_data_in |>
      # dplyr::select(dplyr::all_of(names(specs$rxsmeta_data$req_cols)),
      #               dplyr::any_of(names(specs$rxsmeta_data$opt_cols))) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(rxsmeta_cols[['c']]), as.character),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['i']]), as.integer),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['l']]), as.logical)
      )
    
    # drop all NA columns for species and site
    if ("site_label" %in% names(rxsmeta_data_in) & all(is.na(rxsmeta_data_in$site_label))) rxsmeta_data_in$site_label <- NULL 
    if ("species_code" %in% names(rxsmeta_data_in) & all(is.na(rxsmeta_data_in$species_code))) rxsmeta_data_in$species_code <- NULL 

  }

  return(
    setNames(
      list(prf_data_in, rings_data_in, rxsmeta_data_in),
      c("prf_data", "rings_data", "rxsmeta_data")
    )
  )
}

#' Read input data frames from csv files
#' @param path_prf Path to profile data csv file
#' @param path_rings Path to rings data csv file
#' @param path_rxsmeta Path to the images metadata file. Either a QWAmetadata
#'   `.json(.gz)` file (the `$images` component is extracted) or a QWAimages
#'   `.csv` file.
#' @param name_rxsmeta Original file name of `path_rxsmeta`, used to detect the
#'   file type (JSON vs CSV) when `path_rxsmeta` is an extension-less temp file
#'   from a Shiny upload. Defaults to `path_rxsmeta`.
#' @return A named list with three data frames: prf_data, rings_data,
#' rxsmeta_data
load_data_csv <- function(path_prf, path_rings, path_rxsmeta,
                          name_rxsmeta = NULL,
                          specs = input_specs){
  if (is.null(path_prf) || is.na(path_prf) || path_prf == "") {
    prf_data_in <- NULL
  } else {
    # NOTE: col_types enforces existence and correct type or required cols
    prf_data_in <- vroom::vroom(path_prf,
                                col_types = specs$prf_data$req_cols)

    # remove any of the count columns (except sector_n),
    # keep only required and measurements (assumed numeric)
    prf_data_in <- prf_data_in |>
      dplyr::select(dplyr::all_of(names(specs$prf_data$req_cols)),
                    dplyr::where(is.numeric) & !dplyr::ends_with("_N"))
  }

  # types for the required columns present; missing ones are reported in
  # validate_input_dfs()
  rings_header <- names(vroom::vroom(path_rings, n_max = 0, show_col_types = FALSE))
  rings_types <- specs$rings_data$req_cols
  rings_data_in <- vroom::vroom(path_rings,
                                col_types = rings_types[names(rings_types) %in% rings_header],
                                guess_max = 1000)

  if (is.null(path_rxsmeta) || is.na(path_rxsmeta) || path_rxsmeta == "") {
    rxsmeta_data_in <- NULL
  } else {
    # accept either the QWAmetadata JSON (extract its $images component) or a
    # plain QWAimages CSV, detected from the original file name where available
    ref_name <- if (is.null(name_rxsmeta) || is.na(name_rxsmeta)) {
      path_rxsmeta
    } else {
      name_rxsmeta
    }
    if (grepl("\\.json(\\.gz)?$", ref_name, ignore.case = TRUE)) {
      meta <- jsonlite::read_json(path_rxsmeta, simplifyVector = TRUE)
      if (is.null(meta$images) || !is.data.frame(meta$images) ||
          nrow(meta$images) == 0) {
        stop("The uploaded QWAmetadata JSON does not contain a non-empty ",
             "'images' component.")
      }
      rxsmeta_data_in <- tibble::as_tibble(meta$images)
      rxsmeta_data_in$image_label <- as.character(rxsmeta_data_in$image_label)
    } else {
      rxsmeta_data_in <- vroom::vroom(
        path_rxsmeta, col_types = specs$rxsmeta_data$req_cols
      )
    }
    # backwards-compatibility renames (mirrors read_QWAmetadata()/read_QWAimages())
    rxsmeta_data_in <- rxsmeta_data_in |>
      dplyr::rename(dplyr::any_of(c(
        cluster_dbl_cwt_threshold = 'dbl_cwt_threshold',
        opposite_cwt_ratio_limit = 'maxrel_opp_cwt',
        relwidth_cwt_integration = 'relwidth_cwt_window',
        meas_created_at = 'rxs_created_at',
        img_comment = 'comment'
      )))
    # NOTE: force correct type for optional cols as well
    rxsmeta_cols <- split(names(specs$rxsmeta_data$opt_cols),
                          specs$rxsmeta_data$opt_cols)
    rxsmeta_data_in <- rxsmeta_data_in |>
      # dplyr::select(dplyr::all_of(names(specs$rxsmeta_data$req_cols)),
      #               dplyr::any_of(names(specs$rxsmeta_data$opt_cols))) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(rxsmeta_cols[['c']]), as.character),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['i']]), as.integer),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['l']]), as.logical)
      )
    
    # drop all NA columns for species and site
    if ("site_label" %in% names(rxsmeta_data_in) & all(is.na(rxsmeta_data_in$site_label))) rxsmeta_data_in$site_label <- NULL 
    if ("species_code" %in% names(rxsmeta_data_in) & all(is.na(rxsmeta_data_in$species_code))) rxsmeta_data_in$species_code <- NULL 
    
  }

  return(
    setNames(
      list(prf_data_in, rings_data_in, rxsmeta_data_in),
      c("prf_data", "rings_data", "rxsmeta_data")
    )
  )
}

# check input dfs and throw error if not all checks passed
validate_input_dfs <- function(prf_data_in, rings_data_in,
                               specs = input_specs){
  missing_cols <- setdiff(names(specs$rings_data$req_cols), names(rings_data_in))
  if (length(missing_cols) > 0) {
    hint <- if (any(missing_cols %in% auto_flags)) {
      " Run rxs2tria::complete_QWAdata() on the full data (cells and rings) to set the automatic ring flags."
    }
    stop("Missing required column(s) in the rings data: ", toString(missing_cols),
         ".", hint, call. = FALSE)
  }

  # automatic ring flags (rxs2tria::complete_QWAdata()) must be set for all rings
  flag_cols <- intersect(names(specs$rings_data$req_cols)[specs$rings_data$req_cols == "l"],
                         names(rings_data_in))
  n_na <- vapply(rings_data_in[flag_cols], \(x) sum(is.na(x)), integer(1))
  if (any(n_na > 0)) {
    stop("Missing values in ring flag column(s) ",
         toString(paste0(names(n_na)[n_na > 0], " (", n_na[n_na > 0], " rings)")),
         ". Set them, or drop the flag columns and rerun rxs2tria::complete_QWAdata().",
         call. = FALSE)
  }

  checkmate::assert_data_frame(
    rings_data_in[names(specs$rings_data$req_cols)],
    min.rows = 1, any.missing = FALSE
  )

  # TODO: check that duplicate rings and exclude dupl are consistent

  n_meas_cols <- rings_data_in |>
    dplyr::select(dplyr::where(is.numeric), -year) |> ncol()

  if (!is.null(prf_data_in)) {
    # at least one row, no missing in required columns
    checkmate::assert_data_frame(prf_data_in[names(specs$prf_data$req_cols)],
                                 min.rows = 1, any.missing = FALSE)
    # ensure at least one measurement column with some data
    checkmate::assert_data_frame(
      prf_data_in |> dplyr::select(-names(specs$prf_data$req_cols)),
      min.cols = 1, all.missing = FALSE
    )

    n_meas_cols_prf <- prf_data_in |>
      dplyr::select(dplyr::where(is.numeric), -year, -sector_n) |> ncol()
    n_meas_cols <- n_meas_cols + n_meas_cols_prf

    # profile rings (image_label, year) must be a subset of the rings data
    prf_only <- prf_data_in |>
      dplyr::distinct(image_label, year) |>
      dplyr::anti_join(rings_data_in, by = c("image_label", "year"))
    if (nrow(prf_only) > 0) {
      ex <- paste0(prf_only$image_label, " (", prf_only$year, ")")
      stop(nrow(prf_only), " ring(s) in profile data not found in rings data: ",
           toString(utils::head(ex, 5)), if (length(ex) > 5) ", ...",
           call. = FALSE)
    }
  }

  checkmate::assert_true(n_meas_cols > 0)

  invisible(TRUE)
}

# check rxsmeta df against rings data; on failure, warn and return NULL so that
# the remaining input data can still be used
validate_rxsmeta_df <- function(rxsmeta_data_in, rings_data_in) {
  if (is.null(rxsmeta_data_in)) return(NULL)

  fmt_labels <- function(x) {
    paste0(toString(utils::head(x, 5)), if (length(x) > 5) ", ...")
  }

  # at least one row, no missing in required column nor in species or site if present
  chk_df <- checkmate::check_data_frame(
    rxsmeta_data_in |> dplyr::select(
      "image_label", dplyr::any_of(c("species_code", "site_label"))),
    min.rows = 1, any.missing = FALSE)
  # image labels unique and covering all images in rings data
  img_labels <- rxsmeta_data_in$image_label
  dupl <- unique(img_labels[duplicated(img_labels)])
  miss <- setdiff(unique(rings_data_in$image_label), img_labels)

  problems <- c(
    if (!isTRUE(chk_df)) chk_df,
    if (length(dupl) > 0) paste("duplicated image_label:", fmt_labels(dupl)),
    if (length(miss) > 0) paste("missing image_label:", fmt_labels(miss))
  )

  if (length(problems) > 0) {
    warning("Images metadata not loaded (", paste(problems, collapse = "; "), ")",
            call. = FALSE)
    return(NULL)
  }

  extra <- setdiff(img_labels, rings_data_in$image_label)
  if (length(extra) > 0) {
    warning(length(extra), " image(s) in images metadata without rings data are ignored: ",
            fmt_labels(extra), call. = FALSE)
  }

  rxsmeta_data_in
}

# collection of helpers to derive rings_data_org from input rings_data:
# initialize or coerce logical flag columns (NA → FALSE)
init_flag_columns <- function(df, flag_cols = all_flags) {
  new_flags <- setdiff(flag_cols, names(df))
  df[new_flags] <- FALSE

  df |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(flag_cols),
      \(x) tidyr::replace_na(as.logical(x), FALSE)
    ))
}

# initialize or coerce comments/affected_tissue columns to character
init_char_columns <- function(df) {
  new_cols <- setdiff(c("affected_tissue","comment"), names(df))
  df[new_cols] <- NA_character_

  df <- df |>
    dplyr::mutate(
      comment = as.character(comment),
      affected_tissue = tolower(as.character(affected_tissue))
    )

  invalid_tissue <- (
    !is.na(df$affected_tissue) &
      !df$affected_tissue %in% c("all","ew","lw",""))
  if (any(invalid_tissue)) {
    shiny::showNotification(
      "Affected tissue values not in ['ew','lw','all'] converted to NA",
      type = "warning"
    )
    df$affected_tissue[invalid_tissue] <- NA_character_
  }

  df
}

# order rings data by woodpiece, image with the minimum year
arrange_rings <- function(df) {
  df |>
    dplyr::group_by(woodpiece_label) |>
    dplyr::mutate(.min_year_wp = min(year)) |>
    dplyr::group_by(woodpiece_label, image_label) |>
    dplyr::mutate(.min_year_img = min(year)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.min_year_wp, woodpiece_label, .min_year_img, image_label, year) |>
    dplyr::select(-".min_year_wp", -".min_year_img")
}

# prepare edited flags data frame for output: the input rings data (rows,
# order, columns not in the editor) updated with the edits, plus the flag
# columns not in the input; empty flag columns are dropped on writing to file
prepare_rings_out <- function(df_edited, rings_data){
  id_cols <- c("woodpiece_label", "slide_label", "image_label", "year")
  df_same <- df_edited |> dplyr::select(dplyr::any_of(names(rings_data)))
  df_new <- df_edited |> dplyr::select(
    dplyr::all_of(id_cols),
    dplyr::all_of(setdiff(names(df_edited), names(rings_data))))
  rings_data |>
    dplyr::rows_update(df_same, by = id_cols) |>
    dplyr::left_join(df_new, by = id_cols)
}

# OUTPUTS 
# create modal with save file paths ("Save as")
save_modal <- function(ns, settings, have_comments) {
  launch_wd <- shiny::getShinyOption("launch_wd", default = getwd())
  wd_hint <- shiny::tags$span(
    shiny::tags$i(glue::glue("(Current directory: {launch_wd})")),
    style = "font-size: 0.8em; margin-top: -12px; margin-bottom: 16px; display: block;"
  )
  shiny::modalDialog(
    title = "Save as",
    shiny::textInput(ns("modal_filepath"), "Rings file path",
      placeholder = "e.g., out/QWArings_edited.csv", value = settings$filepath),
    wd_hint,
    shiny::uiOutput(ns("modal_filepath_note")),

    if (have_comments) {
      shiny::tagList(
        shiny::tags$hr(),
        shiny::textInput(ns("modal_filepath_imgs"),
          "Images file path (handled comments progress)",
          placeholder = "e.g., out/QWAimages_progress.csv", value = settings$filepath_rxsmeta),
        wd_hint,
        shiny::uiOutput(ns("modal_filepath_imgs_note"))
      )
    },

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("save_confirm"), "Save", class = "btn-secondary")
    )
  )
}

# create modal to confirm closing the app with unsaved changes
close_modal <- function(ns) {
  shiny::modalDialog(
    title = "Close app",
    "There are unsaved changes. The edited data is returned to R either way,
     but only saving writes it to file.",
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("close_nosave"), "Close without saving", class = "btn-outline-secondary"),
      shiny::actionButton(ns("close_save"), "Save and close", class = "btn-secondary")
    )
  )
}

# note below a save path input if the file already exists or is the same as
# the other save path (other_path)
path_exists_note <- function(path, other_path = NULL) {
  shiny::req(shiny::isTruthy(path))
  launch_wd <- shiny::getShinyOption("launch_wd", default = getwd())
  note <- if (is_same_path(path, other_path)) {
    "Same as the rings file path, choose a different file."
  } else if (fs::file_exists(fs::path_abs(path, start = launch_wd))) {
    "File exists and will be replaced."
  }
  if (is.null(note)) return(NULL)
  shiny::tags$span(
    note,
    class = "small",
    style = "margin-top: -8px; margin-bottom: 16px; display: block;"
  )
}

# whether two save paths resolve to the same file
is_same_path <- function(path, other_path) {
  if (!shiny::isTruthy(path) || !shiny::isTruthy(other_path)) return(FALSE)
  launch_wd <- shiny::getShinyOption("launch_wd", default = getwd())
  fs::path_abs(path, start = launch_wd) == fs::path_abs(other_path, start = launch_wd)
}

# create modal to confirm/adjust an rwl export, prepopulated with the
# auto-computed scaling factor and default file name
export_rwl_modal <- function(ns, default_scaling, default_fname) {
  launch_wd <- shiny::getShinyOption("launch_wd", default = getwd())
  wd_hint <- shiny::tags$span(
    shiny::tags$i(glue::glue("(Current directory: {launch_wd})")),
    style = "font-size: 0.8em; margin-top: -12px; margin-bottom: 16px; display: block;"
  )
  shiny::modalDialog(
    title = "Export rwl",
    shiny::p(
      "Exports the currently selected parameter (and sector) to an ",
      shiny::tags$code(".rwl"), " file."
    ),
    shiny::tags$ul(
      shiny::tags$li("Respects the current woodpiece filtering and detrending settings."),
      shiny::tags$li("Duplicate and excluded rings are removed based on the latest edits."),
      shiny::tags$li("The suggested scaling converts ring widths (µm) to mm; other ",
        "parameters (or detrended series) are scaled by a power of ten to make ",
        "optimal use of the 5 available digits at ", shiny::tags$code("prec = 0.001"), "."),
      shiny::tags$li("If possible, woodpiece labels are replaced by short IDs derived from",
        " the QWA images metadata. Otherwise, a ",
        shiny::tags$code("_mapping.txt"), " file is saved alongside the rwl."),
      shiny::tags$li(
        "Internally uses ", shiny::tags$code("extract_rwl()"), ", ",
        shiny::tags$code("rename_for_tucson()"), ", ",
        shiny::tags$code("scale_for_tucson()"), " and ",
        shiny::tags$code("dplR::write.tucson()"), "; see their docs for details."
      )
    ),
    bslib::layout_column_wrap(
      width = 1/2,
      shiny::checkboxInput(ns("modal_rwl_autoscale"), paste0("Use suggested scaling: ", default_scaling), value = TRUE),
      shinyjs::hidden(
        shiny::textInput(ns("modal_rwl_scaling"), "Custom scaling factor",
          value = as.character(default_scaling))
      )
    ),
    shiny::textInput(ns("modal_rwl_fname"), "Rwl file path",
      value = default_fname, width = "100%"),
    wd_hint,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"), # TODO: download button?
      shiny::actionButton(ns("export_rwl_confirm"), "Confirm and export", class = "btn-secondary")
    )
  )
}

# edited data for saving to file and returning on close: the rings (cf.
# prepare_rings_out()) and, if comments were handled, the rxsmeta data with
# the updated comment_handled column (else NULL)
# rxsmeta: rxsmeta_data data frame (or NULL); handled: images_edited() data
# frame with image_label and comment_handled (or NULL)
prepare_flags_out <- function(df, df_in, rxsmeta = NULL, handled = NULL) {
  df_rxsmeta_out <- NULL
  if (!is.null(rxsmeta) && !is.null(handled)) {
    # rxsmeta has comments otherwise handled would not exist. but might need to initialize comment_handled
    df_rxsmeta_out <- rxsmeta
    if (!"comment_handled" %in% names(df_rxsmeta_out)) {
      df_rxsmeta_out$comment_handled <- NA
    }
    df_rxsmeta_out <- df_rxsmeta_out |>
      dplyr::rows_update(handled[c("image_label","comment_handled")], by = "image_label")
  }
  list(rings = prepare_rings_out(df, df_in), images = df_rxsmeta_out)
}

# edited data returned to R on close (cf. launch_flags_app()): empty flag
# columns dropped as write_QWAdata() does for the file, QWAimages class
# restored (dropped by as_tibble() on load)
prepare_flags_return <- function(out) {
  out$rings <- rxs2tria:::prepare_write(out$rings, "rings")
  rv <- attr(out$images, "roxas_version")
  if (!is.null(rv)) out$images <- rxs2tria:::new_QWAimages(out$images, rv)
  out
}

# helper function to safely write edited data to file
# out: prepare_flags_out() result
# returns TRUE on success, NULL on error
save_ring_edits <- function(out, settings) {
  safe_block({
    launch_wd <- shiny::getShinyOption("launch_wd", default = getwd())
    filepath <- fs::path_abs(settings$filepath, start = launch_wd)
    rxs2tria::write_QWAdata(rxs2tria::QWAdata(rings = out$rings),
                            file_rings = filepath,
                            compress = fs::path_ext(filepath) == "gz",
                            overwrite = TRUE)
    shiny::showNotification(
      paste0("Saved to file: ", filepath),
      type = "message"
    )
    # save rxsmeta with updated comments_handled column
    if (!is.null(out$images)) {
      filepath_rxsmeta <- fs::path_abs(settings$filepath_rxsmeta, start = launch_wd)
      checkmate::assert_path_for_output(filepath_rxsmeta, overwrite = TRUE)
      vroom::vroom_write(out$images, filepath_rxsmeta, delim = ",", na = "")
      shiny::showNotification(
        paste0("Image metadata saved to: ", filepath_rxsmeta),
        type = "message"
      )
    }
    TRUE
  },
  err_title = "Error saving data",
  err_message = "",
  propagate_err = FALSE
  )
}

# # helper function to update the ring editor card inputs (radiobuttons and
# # checkboxes of the current ring flags) given selected ring data
# update_ring_editor_inputs <- function(saved_flags, session){
#   # update exclude_issues radio buttons
#   updateRadioButtons(session, "sel_exclude",
#                      selected = ifelse(saved_flags$exclude_issues, "yes", "no"))
#
#   # if exclude_issues is TRUE, show affected_tissue radio buttons and set value
#   shinyjs::toggle(id = "sel_affected", condition = saved_flags$exclude_issues)
#   if (saved_flags$exclude_issues) {
#     updateRadioButtons(session, "sel_affected",
#                        selected = ifelse(is.na(saved_flags$affected_tissue),
#                                          "NA", saved_flags$affected_tissue))
#   }
#
#   # update the flag checkboxes for the different categories
#   sel_disc_flags <- saved_flags %>%
#     dplyr::select(dplyr::all_of(unname(discrete_features)))
#   sel_disc_flags <- names(sel_disc_flags)[sel_disc_flags[1,] == TRUE]
#   updateCheckboxGroupInput(session, "sel_discrete",
#                            selected = sel_disc_flags)
#
#   sel_disq_flags <- saved_flags %>%
#     dplyr::select(dplyr::all_of(unname(disqual_issues)))
#   sel_disq_flags <- names(sel_disq_flags)[sel_disq_flags[1,] == TRUE]
#   updateCheckboxGroupInput(session, "sel_disqual",
#                            selected = sel_disq_flags)
#
#   sel_tech_issues <- saved_flags %>%
#     dplyr::select(dplyr::all_of(unname(technical_issues)))
#   sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
#   updateCheckboxGroupInput(session, "sel_technical_exact",
#                            selected = sel_tech_issues)
#
#   sel_other_issues <- saved_flags %>%
#     dplyr::select(dplyr::all_of(unname(other_issues)))
#   sel_other_issues <- names(sel_other_issues)[sel_other_issues[1,] == TRUE]
#   updateCheckboxGroupInput(session, "sel_other_iss",
#                            selected = sel_other_issues)
#
#   updateTextAreaInput(session, "sel_comment",
#                       value = saved_flags$comment)
# }
