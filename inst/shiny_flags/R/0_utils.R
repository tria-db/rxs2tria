# COLORS ------------------
# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

options(shiny.maxRequestSize = 100 * 1024^2)  # Set limit to 500 MB
#base_palette <- RColorBrewer::brewer.pal(11, "Spectral")
base_palette <- scales::pal_hue()(9)

# helper function to extend palette to desired length, reordering for better contrast if desired
extend_palette <- function(palette, n, contrasting = TRUE) {
  if (n>length(palette)){
    full_palette <- grDevices::colorRampPalette(palette)(n)
  } else {
    full_palette <- palette[1:n]
  }

  if (contrasting){
    k <- n %/% length(palette) + 1
    indices <- integer(0)
    for (j in 1:k){
      indices <- c(indices,seq(from = j, to = n, by = k))
    }
    full_palette <- full_palette[indices]
  }

  full_palette
}

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
    # dplyr::anti_join(rings_org,
    #                  by = c("image_label", "year", "exclude_issues")) |>
    dplyr::select(image_label, year)

  excl_markers <- plt_df |> # plot data has no duplicate years
    dplyr::inner_join(new_excl, by = c("image_label", "year")) |>
    #dplyr::mutate(y = .data[[param]]) |>
    dplyr::mutate(y = vals) |>
    dplyr::select(year, y, woodpiece_label)

  return(excl_markers)
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


