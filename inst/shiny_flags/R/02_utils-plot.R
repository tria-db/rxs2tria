
subplot_heights <- list(
  "none"     = "350px",
  "sd"       = "400px",
  "cov"      = "550px",
  "cov+sd"   = "600px"
)

plot_max_height <- "700px"

# compute the plot container min_height based on sel subplots as css string
compute_plot_height <- function(active_subplots) {
  key <- if (length(active_subplots) == 0) {
    "none"
  } else {
    paste(sort(active_subplots), collapse = "+")
  }
  subplot_heights[[key]] %||% subplot_heights[["none"]]
}

# create the plot settings modal
plot_settings_modal <- function(ns, current_selection) {
  shiny::modalDialog(
    title = "Plot settings",
    shiny::checkboxGroupInput(
      ns("sel_plots"),
      label = "Show / hide optional subplots",
      choices = c(
        "Sample depth plot" = "sd",
        "Woodpiece coverage plot" = "cov"
      ),
      selected = current_selection
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_plots"), "Proceed")
    )
  )
}

# create the df used as basis for the chronology plot
build_chronology_df <- function(rings_data, prf_data,
                                filt_wp, sel_param, sel_sector, show_excl) {
  df <- rings_data |>
    dplyr::filter(woodpiece_label %in% filt_wp) |>
    dplyr::select(
      woodpiece_label, slide_label, image_label, year,
      exclude_dupl, exclude_issues,
      dplyr::any_of(sel_param)
    )

  # join profile data if the parameter lives there
  if (!is.null(prf_data) && sel_param %in% names(prf_data)) {
    df <- df |>
      dplyr::left_join(
        prf_data |>
          dplyr::filter(sector_n == as.numeric(sel_sector)) |>
          dplyr::select(dplyr::all_of(c("image_label", "year", sel_param))),
        by = c("image_label", "year")
      )
  }

  # assign generic column vals to hold the measurements for the traces,
  # mask excluded years if not show_excl
  df$vals <- df[[sel_param]]
  if (!show_excl) {
    df$vals[df$exclude_issues] <- NA
  }

  # order chronologically within each woodpiece for consecutive lines,
  # without affecting woodpiece order
  df <- df |>
    dplyr::mutate(
      woodpiece_label = factor(woodpiece_label, levels = unique(woodpiece_label))
    ) |>
    dplyr::group_by(woodpiece_label) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(woodpiece_label = as.character(woodpiece_label))

  df
}

# helper to spline detrend data for a specific woodpiece
fit_detrend_curve <- function(df, method = "Spline", nyrs = 32) {
  df |>
    # ignore unselected duplicates for fit
    dplyr::filter(!exclude_dupl) |>
    # ensure we have no gaps in the years, e.g. between images/slides
    tidyr::complete(year = tidyr::full_seq(year, 1), explicit = FALSE) |>
    dplyr::mutate(
      vals_int  = zoo::na.approx(vals, na.rm = FALSE), # interpolate NAs for fit
      det_curve = dplR::detrend.series(
        vals_int, method = method, nyrs = nyrs,
        return.info = TRUE, make.plot = FALSE
      )$curves # return fitted curve rather than standardized series so we can apply also to excluded values
    ) |>
    dplyr::select(year, det_curve)
}

# detrend a chronology data frame
# by woodpiece, fit on vals column but extended to estimate values also for
# excluded_dupl / exclude_issue years
detrend_crn <- function(df, sel_param, method = "Spline", nyrs = 32){
  # get the spline fitted curves for each woodpiece, based on vals col
  fitted_curves <- df |>
    dplyr::group_by(woodpiece_label) |>
    dplyr::group_modify(\(grp, key) fit_detrend_curve(
      grp, method = method, nyrs = nyrs)) |>
    dplyr::ungroup()

  # join to full df (with exclude_dupl rows),
  # calculate standardized sel_param and vals from fitted curve
  df_det <- df |>
    dplyr::left_join(fitted_curves, by = c("woodpiece_label", "year")) |>
    dplyr::mutate(
      vals = vals/det_curve,
      "{sel_param}" := .data[[sel_param]]/det_curve) |>
    dplyr::select(-det_curve)

  df_det
}

# build df for sample depth plot
build_sample_depth_df <- function(df_crn) {
  df_crn |>
    dplyr::filter(!exclude_dupl) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_obs = sum(!is.na(vals)), .groups = "drop") |>
    dplyr::arrange(year)
}

# build df for coverage plot of selected woodpiece
build_coverage_df <- function(rings_data, sel_wp) {
  rings_data |>
    dplyr::filter(woodpiece_label == sel_wp) |>
    # factor to ensure order of images matches chronological ordering in rings_data_org
    dplyr::mutate(image_label = factor(image_label, levels = unique(image_label)))
}

# build the crn plot
build_crn_plot <- function(df_other, df_selwp, sel_wp, palette) {
  color_selwp <- palette[[sel_wp]]

  # exclude duplicates for the selected wp line
  df_selwp_line <- dplyr::filter(df_selwp, !exclude_dupl)

  plotly::plot_ly(
    data = df_other,
    x = ~year,
    y = ~vals,
    color = ~woodpiece_label,
    colors = palette,
    type = "scatter",
    mode = "lines",
    opacity = 0.2,
    name = ~woodpiece_label,
    source = "crn_plot",
    showlegend = TRUE,
    meta = list(role = "otherwp")
  ) |>
    plotly::add_trace(
      data = df_selwp_line,
      x = ~year,
      y = ~vals,
      type = "scatter",
      mode = "lines",
      line = list(color = color_selwp, width = 2),
      opacity = 1,
      name = paste0("<b>", sel_wp, "</b>"),
      showlegend = TRUE,
      meta = list(role = "selwp", wp_label = sel_wp)
    ) |>
    plotly::layout(
      legend = list(
        orientation = "h",
        xanchor = "center", x = 0.5,
        yanchor = "bottom", y = 1.1
      )
    )
}

# build sample depth plot
build_sample_depth_plot <- function(df_sd, sd_color = sample_depth_color) {
  plotly::plot_ly(
    data = df_sd,
    x = ~year,
    y = ~n_obs,
    type = "scatter",
    mode = "lines+markers",
    line = list(shape = "hvh", color = sd_color),
    marker = list(size = 5, color = sd_color),
    name = "sample depth",
    showlegend = FALSE,
    source = "crn_plot",
    meta = list(role = "sampledepth")
  )
}

# build the coverage plot for the selected woodpiece
build_coverage_plot <- function(df_cov, sel_wp, cov_colorscale = coverage_colorscale) {
  plotly::plot_ly(
    data = df_cov,
    x = ~year,
    y = ~image_label,
    type = "scatter",
    mode = "lines+markers",
    marker = list(
      size = 10,
      color = ~cno,
      colorscale = cov_colorscale,
      symbol = "square"
    ),
    line = list(color = "darkgrey"),
    name = ~image_label,
    hoverinfo = "text",
    text = ~paste0(image_label, "<br>", year, ": ", cno, " cells"),
    showlegend = FALSE,
    source = "crn_plot",
    meta = list(role = "covlines", wp_label = sel_wp)
  )
}

# combine the subplots to the main plot
assemble_main_plot <- function(crn_plot, sd_plot, cov_plot, sel_param) {
  plots   <- list(crn_plot)
  heights <- c(0.5)

  if (!is.null(sd_plot)) {
    plots   <- c(plots, list(sd_plot))
    heights <- c(heights, 0.1)
  }

  if (!is.null(cov_plot)) {
    plots   <- c(plots, list(cov_plot))
    heights <- c(heights, 0.4)
  }

  heights <- heights / sum(heights) # normalize heights

  plotly::subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    heights = heights
  ) |>
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = sel_param),
      hovermode = "closest"
    ) |>
    plotly::event_register("plotly_click") |>
    plotly::event_register("plotly_relayout") |>
    plotly::config(modeBarButtonsToRemove =  list('select2d', 'lasso2d',
                                                  'hoverClosestCartesian',
                                                  'hoverCompareCartesian'))
}

# create modal to confirm clicking on another woodpiece trace
switch_selwp_modal <- function(ns, new_wp) {
  shiny::modalDialog(
    title = "Warning",
    glue::glue("You have clicked on a different woodpiece trace: {new_wp}.
                Do you want to switch to editing this woodpiece?"),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_wp"), "Proceed")
    )
  )
}
