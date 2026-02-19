# GLOBAL OPTIONS -----------
options(shiny.maxRequestSize = 100 * 1024^2)  # Set limit to 500 MB

# GLOBAL VARS ------------
discrete_features <- c(
  "Blue ring" = "blue_ring",
  "Frost ring" = "frost_ring",
  "Light ring" = "light_ring",
  "Intra-annual density fluctuations" = "iadf",
  "Traumatic resin ducts" = "traum_resin_ducts",
  "Trabeculae" = "trabeculae",
  "Other features" = "other_discrete"
)

disqual_issues <- c(
  "Radially incomplete ring" = "incomplete_ring",
  "Wedging/missing ring" = "missing_ring",
  "Crossdating" = "x_dating",
  "Compression wood" = "compression_wood",
  "Orientation" = "orientation"
)

technical_issues <- c(
  "Out of focus" = "out_of_focus",
  "Crack(s)" = "cracks",
  "Paraffin" = "paraffin",
  "Compressed cells" = "compressed_cells",
  "Overlapping cells" = "overlapping_cells",
  "Broken cells" = "broken_cells",
  "Tangentially incomplete" = "tang_incomplete"
)

other_issues <- c(
  "Tyloses" = "tyloses",
  "Decay" = "decay",
  "Other issues" = "other_disqual"
)

all_flags <- unname(c(discrete_features, disqual_issues,
                      technical_issues, other_issues))

input_specs <- list(
  prf_data = list(
    req_cols = c(image_label = "c", year = "i", sector_n = "i")
    # opt_cols: any numeric are considered as measurements for the plot
    # (except for counts -> see below)
  ),
  rings_data = list(
    req_cols = c(woodpiece_label = "c", slide_label = "c", image_label = "c",
                 year = "i", incomplete_ring = "l", missing_ring = "l",
                 duplicate_ring = "l", exclude_dupl = "l", exclude_issues = "l"),
    opt_cols = c(
      # any of the quality flag columns (e.g. if data was previously edited in app)
      setNames(rep("l", length(all_flags)), all_flags),
      affected_tissue = "c", comments = "c")
    # plus any numeric columns to considered as measurements -> see below
  ),
  rxsmeta_data = list(
    req_cols = c(image_label = "c"),
    opt_cols = c(woodpiece_label = "c",
                 species_code = "c", site_label = "c", comments = "c",
                 fname_image = "c", fname_annotated = "c",
                 fname_annotated_twin = "c")
  )
)

# helper functions
shinyInput_CB_DT <- function(id, num, values, disabled = FALSE){
  inputs <- character(num)
  for (i in seq_len(num)) {
    if (!is.na(values[i])){
      if (disabled){
        inputs[i] <- as.character(shinyjs::disabled(shiny::checkboxInput(
          paste0(id, i), label = NULL, value = values[i], width = NULL)))
      } else {
        inputs[i] <- as.character(shiny::checkboxInput(
          paste0(id, i), label = NULL, value = values[i], width = NULL))
      }
    } else {
      inputs[i] <- ""
    }
  }
  inputs
}

get_new_excluded <- function(rings_org, rings_edit, sel_wp, plt_df, param){
  new_excl <- rings_edit %>%
    dplyr::filter(woodpiece_label %in% sel_wp) %>%
    #dplyr::filter(exclude_issues) %>%
    dplyr::anti_join(rings_org,
                     by = c("image_label", "year", "exclude_issues")) |>
    dplyr::select(image_label, year)

  excl_markers <- plt_df |> # plot data has no duplicate years
    dplyr::inner_join(new_excl, by = c("image_label", "year")) |>
    dplyr::mutate(y = .data[[param]]) |>
    dplyr::select(year, y, woodpiece_label)

  return(excl_markers)
}

get_excluded <- function(rings_edit, sel_wp, plt_df){
  new_excl <- rings_edit %>%
    dplyr::filter(woodpiece_label %in% sel_wp) %>%
    dplyr::filter(exclude_issues) %>%
    dplyr::select(image_label, year)

  excl_markers <- plt_df |> # plot data has no duplicate years
    dplyr::inner_join(new_excl, by = c("image_label", "year")) |>
    #dplyr::mutate(y = .data[[param]]) |>
    dplyr::mutate(y = vals) |>
    dplyr::filter(!is.na(vals)) |>
    dplyr::select(year, y, vals, woodpiece_label)

  return(excl_markers)
}

draw_excl_markers <- function(plot_obj, x_years, y_vals){
  plot_obj %>%
    plotly::plotlyProxyInvoke(
      "addTraces",
      list(
        x = as.list(x_years),
        y = as.list(y_vals),
        name = "excl_rings",
        mode = "markers",
        marker = list(
          size = 6,
          color = "hotpink",
          symbol = "circle-open"
        ),
        showlegend = FALSE,
        hoverinfo = "skip",
        meta = list(role = "exclring")
      )
    )
}

calc_mean_vals <- function(df, sel_mean){
  if (sel_mean == "mean"){
    df_mean <- df |>
      dplyr::select(year, vals) |>
      collapse::fgroup_by(year) |>
      collapse::fsummarise(N = collapse::fnobs(vals),
                           vals = collapse::fmean(vals)) |>
      collapse::fsubset(N > 1) |>
      dplyr::select(year, vals)
  } else if (sel_mean == "tbrm"){
    df_mean <- df |>
      dplyr::select(year, vals) |>
      dplyr::group_by(year) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::summarise(vals = dplR::tbrm(vals))

  } else {
    return(NULL)
  }
  df_mean
}

draw_mean_trace <- function(plot_obj, x_years, y_vals, sel_mean){
  plot_obj %>%
    plotly::plotlyProxyInvoke(
      "addTraces",
      list(
        x = as.list(x_years),
        y = as.list(y_vals),
        name = paste("crn.", sel_mean, sep = ""),
        type = 'scatter',
        mode = 'lines',
        line = list(width = 2, color = 'black'),
        showlegend = FALSE,
        hoverinfo = "skip",
        meta = list(role = "crnline")
      ))
}

label_with_pop <- function(label_text, popover_text, icon_name = "info-circle", icon_title = "Info", popover_title = NULL){
  span(
    label_text,
    bslib::popover(
      bsicons::bs_icon(icon_name, title = icon_title),
      title = popover_title,
      popover_text
    )
  )
}





# ERRORS and WARNINGS ------------------------------
# wrapper for an error message
show_error_modal <- function(title, message) {
  if (shiny::isRunning()) {
    showModal(modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    message(sprintf("[ERROR] %s: %s", title, message))
  }
}

# wrapper for a warning notification
show_warning_notification <- function(message) {
  if (shiny::isRunning()) {
    shiny::showNotification(message, type = "warning")
  } else {
    message(sprintf("[WARNING] %s", message))
  }
}

# Safely try to run a given expression without crashing the app
# run the expr in the given block
# and catch errors and warnings, showing them in a modal dialog or notification
# in case of a warning, the rest of the block is still executed
# in case of an error, the block is exited, returning NULL, and the error is shown in a modal dialog
# if propagate_err = FALSE, the parent context will not receive an error and execution continues with the NULL return from the current block
# modal dialog is shown only if the error is not already a modal_shown class, so only for the lowest level
safe_block <- function(expr,
                       err_title = "Error", err_message = "An error occurred:",
                       propagate_err = TRUE,
                       warn_message = "A warning occurred:") {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        show_warning_notification(paste(warn_message, w$message))
        rlang::cnd_muffle(w) # muffle warnings so they don't propagate up
      }
    ),
    error = function(e) {
      if (!inherits(e, "modal_shown")) {
        show_error_modal(err_title, paste(err_message, e$message))
        e <- structure(e, class = c("modal_shown", class(e)))
      }
      if (propagate_err) {
        stop(e)  # re-throw error in parent context
      }
      return(NULL)
    }
  )
}






draw_selected_marker <- function(plot_obj, x_val, y_val, marker_name, meta_info){
  plot_obj %>%
    # add new marker trace
    plotly::plotlyProxyInvoke(
      "addTraces",
      list(
        x = list(x_val),
        y = list(y_val),
        name = marker_name,
        mode = "markers",
        marker = list(
          size = 10,
          color = "red",
          symbol = "circle"
        ),
        showlegend = FALSE,
        hoverinfo = "skip",
        meta = meta_info
      )
    )
}

get_adjacent_year <- function(df, woodpiece, current_year, param, direction){
  adj_years <- df |>
    dplyr::filter(
      woodpiece_label == woodpiece,
      if (direction == "prev") year < current_year else year > current_year #,
      # !is.na(.data[[param]])
    ) |>
    dplyr::select(year, param) |>
    dplyr::arrange(if (direction == "prev") dplyr::desc(year) else year)

  if (nrow(adj_years) == 0){
    showNotification(sprintf(
      "No %s year available for this woodpiece in current plot.",
      if (direction == "prev") "earlier" else "later"),
      type = "warning")
    return(NULL)
  }

  return(setNames(
    list(adj_years$year[1], adj_years[[param]][1]),
    c("year", "val")
  ))
}

get_overlapping_images <- function(df_rings, target_image) {
  # Get year range for target image
  target_range <- df_rings |>
    dplyr::filter(image_label == target_image) |>
    dplyr:: summarise(start = min(year), end = max(year))

  # Find images that overlap or are within 2 years
  valid_images <- df_rings |>
    dplyr::group_by(image_label) |>
    dplyr::summarise(start = min(year), end = max(year)) |>
    dplyr::filter(
      # overlaps with target
      (start <= target_range$end & end >= target_range$start) |
        # ends within 2 years before target starts
        (end >= target_range$start - 2 & end < target_range$start) |
        # starts within 2 years after target ends
        (start <= target_range$end + 2 & start > target_range$end)
    ) |>
    dplyr:: pull(image_label)

  df_rings |>
    dplyr::filter(image_label %in% valid_images)
}


