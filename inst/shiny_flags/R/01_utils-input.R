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
      shiny::actionButton(ns("confirm_input"), "Proceed")
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
      shiny::actionButton(ns("confirm_overwrite"), "Proceed")
    )
  )
}

# create input fields for env vars / csv paths / example data
# (dynamic part of load_details_ui)
build_env_inputs <- function(ns) {
  shiny::tagList(
    "Provide the names of the data.frames in the current R environment:",
    shiny::textInput(ns("name_prf"), "Profile data*",
                     value = "prf_data"),
    shiny::textInput(ns("name_rings"), "QWA rings data",
                     value = "QWA_data$rings"),
    shiny::textInput(ns("name_rxsmeta"), "ROXAS (image) metadata*",
                     value = "df_rxsmeta")
  )
}
build_csv_inputs <- function(ns) {
  shiny::tagList(
    "Provide the filepaths for the following:",
    shiny::fileInput(ns("file_prf"), "Upload the QWA profile data",
                     accept = c(".csv")),
    shiny::fileInput(ns("file_rings"), "Upload the QWA rings data",
                     accept = c(".csv")),
    shiny::fileInput(ns("file_rxsmeta"), "Upload the ROXAS metadata",
                     accept = c(".csv"))
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
  rings_data_in <- rings_data_in |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(rings_cols[['c']]), as.character),
      dplyr::across(dplyr::all_of(rings_cols[['i']]), as.integer),
      dplyr::across(dplyr::all_of(rings_cols[['l']]), as.logical)
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
    rsxmeta_cols <- split(names(specs$rxsmeta_data$opt_cols),
                          specs$rxsmeta_data$opt_cols)
    rxsmeta_data_in <- rxsmeta_data_in |>
      dplyr::select(dplyr::all_of(names(specs$rxsmeta_data$req_cols)),
                    dplyr::any_of(names(specs$rxsmeta_data$opt_cols))) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(rxsmeta_cols[['c']]), as.character),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['i']]), as.integer),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['l']]), as.logical)
      )
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
#' @param path_rxsmeta Path to rxsmeta data csv file
#' @return A named list with three data frames: prf_data, rings_data,
#' rxsmeta_data
load_data_csv <- function(path_prf, path_rings, path_rxsmeta,
                          specs = input_specs){
  if (is.na(path_prf) || path_prf == "") {
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

  rings_data_in <- vroom::vroom(path_rings,
                                col_types = specs$rings_data$req_cols)

  if (is.na(path_rxsmeta) || path_rxsmeta == "") {
    rxsmeta_data_in <- NULL
  } else {
    rxsmeta_data_in <- vroom::vroom(
      path_rxsmeta, col_types = specs$rxsmeta_data$req_cols
    )
    # NOTE: force correct type for optional cols as well
    rxsmeta_cols <- split(names(specs$rxsmeta_data$opt_cols),
                          specs$rxsmeta_data$opt_cols)
    rxsmeta_data_in <- rxsmeta_data_in |>
      dplyr::select(dplyr::all_of(names(specs$rxsmeta_data$req_cols)),
                    dplyr::any_of(names(specs$rxsmeta_data$opt_cols))) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(rxsmeta_cols[['c']]), as.character),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['i']]), as.integer),
        dplyr::across(dplyr::any_of(rxsmeta_cols[['l']]), as.logical)
      )
  }

  return(
    setNames(
      list(prf_data_in, rings_data_in, rxsmeta_data_in),
      c("prf_data", "rings_data", "rxsmeta_data")
    )
  )
}

# check input dfs and throw error if not all checks passed
validate_input_dfs <- function(prf_data_in, rings_data_in, rxsmeta_data_in,
                               specs = input_specs){
  checkmate::assert_data_frame(
    rings_data_in[names(specs$rings_data$req_cols)],
    min.rows = 1, any.missing = FALSE
  )

  # TODO: check that duplicate rings and exclude dupl are consistent
  # TODO: maybe check / replace missing ring measurements with 0 where applicable?

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

    # TODO: check that prf_data (image_label, year) are subset of rings_data (image_label, year)
  }

  checkmate::assert_true(n_meas_cols > 0)

  if (!is.null(rxsmeta_data_in)) {
    # at least one row, no missing in required column nor in species or site if present
    checkmate::assert_data_frame(
      rxsmeta_data_in |> dplyr::select(
        "image_label", dplyr::any_of(c("species_code", "site_label"))),
      min.rows = 1, any.missing = FALSE)

    # TODO: check that rxsmeta_data (image_label, year) are subset of rings_data (image_label, year)
  }

  invisible(TRUE)
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

# prepare edited flags data frame for output
prepare_rings_out <- function(df_edited, rings_data = NULL){
  # the character cols
  df_char <- df_edited |> dplyr::select(
    affected_tissue, comment)
  # the optional flag colums
  df_flags_opt <- df_edited |> dplyr::select(
    dplyr::any_of(setdiff(disqual_issues, c('incomplete_ring','missing_ring'))),
    dplyr::any_of(c(unname(technical_issues),
                    unname(other_issues),
                    unname(discrete_features))))
  # the rest of the columns
  df <- df_edited[setdiff(names(df_edited),
                          c("affected_tissue", "comment", names(df_flags_opt)))]
  # char: remove completely empty columns
  df_char <- df_char |> janitor::remove_empty(which = "cols", cutoff = 1)
  # opt: remove completely empty / all FALSE cols
  df_flags_opt <- df_flags_opt |>
    janitor::remove_empty(which = "cols", cutoff = 1) |>
    dplyr::select(dplyr::where(~any(.)))

  # combine and restore original column order
  df <- df |> dplyr::bind_cols(df_char, df_flags_opt)
  df <- df |> dplyr::select(dplyr::any_of(names(df_edited)))

  if (!is.null(rings_data)) {
    id_cols <- c("woodpiece_label", "slide_label", "image_label", "year")
    df_same <- df |> dplyr::select(dplyr::any_of(names(rings_data)))
    df_new <- df |> dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(setdiff(names(df), names(rings_data))))
    df <- rings_data |>
      dplyr::rows_update(df_same, by = id_cols) |>
      dplyr::left_join(df_new, by = id_cols)
  }

  df
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
