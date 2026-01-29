# helper function to show the modal for input source selection
show_input_source_modal <- function(ns){
  showModal(
    modalDialog(
      title = "Select input source",
      tagList(
        radioButtons(
          ns("load_type"),
          "Choose input option:",
          choices = c(
            "Load data from R environment" = "env",
            "Load data from csv files" = "csv",
            "Load example data" = "example")
        ),
        hr(),
        uiOutput(ns("load_details_ui"))# dynamic UI to provide input details given load_type
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm_input"), "Proceed")
      )
    )
  )
}

# helper to get a named list element in the global environment
get_list_item <- function(var_name, envir = .GlobalEnv) {
  # Handle obj$element format
  if (grepl("\\$", var_name)) {
    parts <- strsplit(var_name, "$", fixed = TRUE)[[1]]
    obj_name <- parts[1]
    element_name <- parts[2]
    # Handle obj[['element']] or obj[["element"]] format
  } else if (grepl("\\[\\[", rings_name)) {
    pattern <- "^([^\\[]+)\\[\\[(['\"])(.+?)\\2\\]\\]$"
    obj_name <- sub(pattern, "\\1", rings_name)
    element_name <- sub(pattern, "\\3", rings_name)
  } else {
    stop("Invalid variable name format.")
  }
  get(obj_name, envir = envir)[[element_name]]
}

#' Get input data objects from global environment
#' @param name_prf Name of profile data frame object in global environment
#' @param name_rings Name of rings data frame object in global environment
#' @param name_rxsmeta Name of rxsmeta data frame object in global environment
#' @return A named list with three data frames: prf_data_in, rings_data,
#' rxsmeta_data_in
load_data_env <- function(name_prf, name_rings, name_rxsmeta){
  prf_data_in <- get(name_prf, envir = .GlobalEnv) |>
    tibble::as_tibble() |>
    # NOTE: enforces existence and correct type or required cols
    dplyr::mutate(
      image_label = as.character(image_label),
      year = as.integer(year),
      sector_n = as.integer(sector_n)
    )
  # remove any of the count columns (except sector_n)
  prf_data_in <- prf_data_in |>
    dplyr::select(-dplyr::ends_with("_N", ignore.case = FALSE))

  # rings data may be element of a list (e.g. QWA_data$rings)
  if (grepl("\\$", name_rings) ||
      grepl("\\[\\[", name_rings)) {
    rings_data_in <- get_list_item(name_rings, envir = .GlobalEnv)
  } else {
    rings_data_in <- get(name_rings, envir = .GlobalEnv)
  }
  logi_cols <- c(unname(discrete_features),
                 unname(disqual_issues),
                 unname(technical_issues))
  rings_data_in <- rings_data_in |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c(woodpiece_label, slide_label, image_label), as.character),
      year = as.integer(year),
      dplyr::across(c(incomplete_ring, missing_ring, duplicate_ring, exclude_dupl, exclude_issues), as.logical),
      dplyr::across(dplyr::any_of(logi_cols),
                    \(x) tidyr::replace_na(as.logical(x), FALSE)) # TODO: OK? or require no missing?
    )

  rxsmeta_data_in <- get(name_rxsmeta, envir = .GlobalEnv) |>
    tibble::as_tibble() |>
    # TODO: maybe not (all) required (if we don't want to open images, if only one site/species -> might not need filter)
    dplyr::mutate(
      dplyr::across(
        c(image_label, woodpiece_label, species_code, site_label, fname_image),
        as.character
      )
    )

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
load_data_csv <- function(path_prf, path_rings, path_rxsmeta){
  prf_data_in <- vroom::vroom(
    path_prf,
    # NOTE: col_types enforces existence and correct type or required cols
    col_types = c(image_label = "c", year = "i", sector_n = "i")
  )
  # remove any of the count columns (except sector_n)
  prf_data_in <- prf_data_in |>
    dplyr::select(-dplyr::ends_with("_N", ignore.case = FALSE))

  rings_data_in <- vroom::vroom(
    path_rings,
    col_types = c(woodpiece_label = "c", slide_label = "c", image_label = "c",
                  year = "i", incomplete_ring = "l", missing_ring = "l",
                  duplicate_ring = "l", exclude_dupl = "l", exclude_issues = "l")
  )

  rxsmeta_data_in <- vroom::vroom(
    path_rxsmeta,
    col_types = c(image_label = "c", woodpiece_label = "c",
                  species_code = "c", site_label = "c",
                  fname_image =  "c")
  )
  return(
    setNames(
      list(prf_data_in, rings_data_in, rxsmeta_data_in),
      c("prf_data", "rings_data", "rxsmeta_data")
    )
  )
}


# helper throws error if not all checks passed
validate_input_dfs <- function(prf_data_in, rings_data_in, rxsmeta_data_in){
  checkmate::assert_data_frame(prf_data_in[c("image_label","year","sector_n")],
                               min.rows = 1, any.missing = FALSE)
  # ensure we have at least one measurement column with data
  checkmate::assert_data_frame(
    prf_data_in |> dplyr::select(-image_label, -year, -sector_n,
                                 dplyr::where(is.numeric)),
    min.cols = 1, all.missing = FALSE)

  # ensure we have data and there are no missing values in structure cols
  checkmate::assert_data_frame(rings_data_in[c("woodpiece_label", "slide_label", "image_label","year")],
                               min.rows = 1, any.missing = FALSE)

  checkmate::assert_data_frame(rxsmeta_data_in[c("image_label","species_code", "site_label")],
                               min.rows = 1, any.missing = FALSE)
  TRUE
}

# helper function to update the ring editor card inputs (radiobuttons and
# checkboxes of the current ring flags) given selected ring data
update_ring_editor_inputs <- function(saved_flags, session){
  # update exclude_issues radio buttons
  updateRadioButtons(session, "sel_exclude",
                     selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

  # if exclude_issues is TRUE, show affected_tissue radio buttons and set value
  shinyjs::toggle(id = "sel_affected", condition = saved_flags$exclude_issues)
  if (saved_flags$exclude_issues) {
    updateRadioButtons(session, "sel_affected",
                       selected = ifelse(is.na(saved_flags$affected_tissue),
                                         "NA", saved_flags$affected_tissue))
  }

  # update the flag checkboxes for the different categories
  sel_disc_flags <- saved_flags %>%
    dplyr::select(dplyr::all_of(unname(discrete_features)))
  sel_disc_flags <- names(sel_disc_flags)[sel_disc_flags[1,] == TRUE]
  updateCheckboxGroupInput(session, "sel_discrete",
                           selected = sel_disc_flags)

  sel_disq_flags <- saved_flags %>%
    dplyr::select(dplyr::all_of(unname(disqual_issues)))
  sel_disq_flags <- names(sel_disq_flags)[sel_disq_flags[1,] == TRUE]
  updateCheckboxGroupInput(session, "sel_disqual",
                           selected = sel_disq_flags)

  sel_tech_issues <- saved_flags %>%
    dplyr::select(dplyr::all_of(unname(technical_issues)))
  sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
  updateCheckboxGroupInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)

  sel_other_issues <- saved_flags %>%
    dplyr::select(dplyr::all_of(unname(other_issues)))
  sel_other_issues <- names(sel_other_issues)[sel_other_issues[1,] == TRUE]
  updateCheckboxGroupInput(session, "sel_other_iss",
                           selected = sel_other_issues)

  updateTextAreaInput(session, "sel_comment",
                      value = saved_flags$comment)
}
