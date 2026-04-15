
subplot_heights <- list(
  "none"     = "350px",
  "sd"       = "400px",
  "cov"      = "550px",
  "cov+sd"   = "600px"
)

# compute the plot container height based on sel subplots as css string
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

update_marker_info <- function(marker, df_data, sel_param){
  year_val <- marker$year
  ycov_val <- marker$ycov_val
  ywp_val <- df_data |>
    dplyr::filter(image_label == ycov_val, year == year_val) |>
    dplyr::pull(dplyr::all_of(sel_param)) |>
    dplyr::first()

  df_excl <- df_data |> # TODO: or rings_data_edited? update color if edited?
    dplyr::filter(image_label == ycov_val, year == year_val)
  not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
  marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

  return(
    list(
      year = marker$year,
      wp_label = marker$wp_label,
      ywp_val = ywp_val,
      ycov_val = marker$ycov_val,
      marker_col = marker_col
    )
  )
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


# create marker (lookup image_label / measueremnt val / color) given click info
resolve_click_marker <- function(click_data, df_data, sel_param) {

  year_val <- click_data$x
  wp_label <- click_data$wp_label

  switch(click_data$role,
    "selwp" = {
      ywp_val <- click_data$y
      ycov_val <- df_data |>
        dplyr::filter(year == year_val, !exclude_dupl) |> # selected image
        dplyr::pull(image_label) |>
        dplyr::first()
    },
    "covlines" = {
      ycov_val <- click_data$y
      ywp_val <- df_data |>
        dplyr::filter(image_label == ycov_val, year == year_val) |>
        dplyr::pull(dplyr::all_of(sel_param)) |>
        dplyr::first()
    },
    "otherwp" = {
      ywp_val <- click_data$y
      ycov_val <- df_data |>
        dplyr::filter(woodpiece_label == wp_label, year == year_val, !exclude_dupl) |>
        dplyr::pull(image_label)
    },
    stop("Unknown click role")
  )

  df_excl <- df_data |> # TODO: or rings_data_edited? update color if edited?
    dplyr::filter(image_label == ycov_val, year == year_val)
  not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
  marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

  return(
    list(
      year = year_val,
      wp_label = wp_label,
      ywp_val = ywp_val,
      ycov_val = ycov_val,
      marker_col = marker_col
    )
  )
}

# build info for adding marker traces
build_marker_trace <- function(x, y, color, role,
                               hoverinfo = "skip", text = NULL, yaxis = "y",
                               marker_size = 10, marker_symbol = "circle") {
  trace <- list(
    x = as.list(x),
    y = as.list(y),
    type = "scatter",
    mode = "markers",
    name = role,
    marker = list(
      size = marker_size,
      color = color,
      symbol = marker_symbol
    ),
    hoverinfo = hoverinfo,
    showlegend = FALSE,
    meta = list(role = role),
    yaxis = yaxis
  )
  if (hoverinfo == "text") {
    trace$text <- list(text)
  }

  trace
}

# build info for restyling marker traces
restyle_marker_trace <- function(x, y, color, text = NULL,
                                 marker_size = 10, marker_symbol = "circle") {
  list(
    x = list(list(x)),
    y = list(list(y)),
    marker = list(
      size = marker_size,
      color = color,
      symbol = marker_symbol
    ),
    hovertext = text
  )
}

# draw (add / restyle) the selected marker trace(s)
draw_sel_marker <- function(plot_obj, marker, traces, sel_subpl) {
  selm_trace_exists <- "sel_marker_wp" %in% names(traces)

  if (!selm_trace_exists) {
    trace_info_crn <- build_marker_trace(
      x = marker$year,
      y = marker$ywp_val,
      color = marker$marker_col,
      role = "sel_marker_wp",
      hoverinfo = "text",
      text = list(paste0(marker$ycov_val, "<br>",
                         marker$year, ": ", marker$ywp_val)),
      yaxis = "y"
    )

    plot_obj <- plot_obj %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        trace_info_crn
      )
  } else {
    wp_curveNumber <- traces[['sel_marker_wp']]$curveNumber

    trace_info_crn <- restyle_marker_trace(
      x = marker$year,
      y = marker$ywp_val,
      color = marker$marker_col,
      text = list(paste0(marker$ycov_val, "<br>",
                         marker$year, ": ", marker$ywp_val))
    )

    plot_obj <- plot_obj %>%
      plotly::plotlyProxyInvoke(
        "restyle",
        trace_info_crn,
        wp_curveNumber
      )
  }

  if ("cov" %in% sel_subpl) {
    selm_trace_cov_exists <- "sel_marker_cov" %in% names(traces)
    yaxis_cov <- if ("sd" %in% sel_subpl) "y3" else "y2"

    if (!selm_trace_cov_exists) {
      trace_info_cov <- build_marker_trace(
        x = marker$year,
        y = marker$ycov_val,
        color = marker$marker_col,
        role = "sel_marker_cov",
        hoverinfo = "skip",
        yaxis = yaxis_cov
      )

      plot_obj <- plot_obj %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          trace_info_cov
        )
    } else {
      cov_curveNumber <- traces[['sel_marker_cov']]$curveNumber

      trace_info_cov <- restyle_marker_trace(
        x = marker$year,
        y = marker$ycov_val,
        color = marker$marker_col
      )

      plot_obj <- plot_obj %>%
        plotly::plotlyProxyInvoke(
          "restyle",
          trace_info_cov,
          cov_curveNumber
        )
    }
  }

  plot_obj
}

# clear marker(s) from plot
clear_sel_marker <- function(plot_obj, traces, sel_subpl){
  selm_trace_exists <- "sel_marker_wp" %in% names(traces)
  if (selm_trace_exists) {
    selm_curveNumber <- traces[['sel_marker_wp']]$curveNumber

    if ("cov" %in% sel_subpl) {
      selm_trace_cov_exists <- "sel_marker_cov" %in% names(traces)
      if (selm_trace_cov_exists) {
        selm_curveNumber <- c(selm_curveNumber, traces[['sel_marker_cov']]$curveNumber)
      }
    }

    plot_obj <- plot_obj %>% 
      plotly::plotlyProxyInvoke("deleteTraces", selm_curveNumber)
  }

  plot_obj
}

# create shifted marker
resolve_shift_marker <- function(sel_wp, current_year, shift_dir, df_data, sel_param){
  adj_years <- df_data |>
    dplyr::filter(!exclude_dupl) |>
    dplyr::filter(
      if (shift_dir == "prev") year < current_year else year > current_year
    ) |>
    dplyr::select(image_label, year, dplyr::all_of(sel_param)) |>
    dplyr::arrange(if (shift_dir == "prev") dplyr::desc(year) else year) |>
    dplyr::slice(1)

  if (nrow(adj_years) == 0) {
    return(NULL)
  }

  year_val <- adj_years$year
  ywp_val <- adj_years[[sel_param]]
  ycov_val <- adj_years$image_label

  df_excl <- df_data |> dplyr::filter(
    image_label == ycov_val, year == year_val)
  not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
  marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

  return(
    list(
      year = year_val,
      wp_label = sel_wp,
      ywp_val = ywp_val,
      ycov_val = ycov_val,
      marker_col = marker_col
    )
  )
}

# create marker given new selected row
resolve_row_marker <- function(sel_wp, sel_img, new_year, df_data, sel_param) {
  ywp_val <- df_data |>
    dplyr::filter(year == new_year, image_label == sel_img) |>
    dplyr::pull(dplyr::any_of(sel_param))

  df_excl <- df_data |> dplyr::filter(
    image_label == sel_img, year == new_year)
  not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
  marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

  return(
    list(
      year = new_year,
      wp_label = sel_wp,
      ywp_val = ywp_val,
      ycov_val = sel_img,
      marker_col = marker_col
    )
  )
}

# get the excluded (due to issues) rings for selected woodpiece
get_excl_markers <- function(sel_wp, df_data, df_flags) {

  # find the rings exclude_issues flags for the selected woodpiece
  df_rings <- df_flags |>
    dplyr::filter(woodpiece_label == sel_wp) |>
    dplyr::filter(!exclude_dupl, exclude_issues) |>
    dplyr::select(image_label, year)

  # get the corresponding measurement vals from the plot data
  excl_markers <- df_data |>
    dplyr::inner_join(df_rings, by = c("image_label", "year")) |>
    dplyr::select(year, vals) |>
    dplyr::filter(!is.na(vals))
  names(excl_markers) <- c("year", "ywp_val")

  excl_markers
}

# draw (add / restyle) the trace for excluded issues markers
draw_excl_markers <- function(plot_obj, excl_markers, traces) {
  excl_trace_exists <- "excl_markers" %in% names(traces)
  have_excl_markers <- nrow(excl_markers) > 0

  if (!excl_trace_exists && have_excl_markers) {
    excl_trace_info <- build_marker_trace(
      x = excl_markers$year,
      y = excl_markers$ywp_val,
      color = "blue",
      role = "excl_markers",
      hoverinfo = "skip",
      marker_size = 7,
      marker_symbol = "circle-open",
      yaxis = "y"
    )
    plot_obj <- plot_obj %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        excl_trace_info
      )
  } else if (have_excl_markers) {
    excl_curveNumber <- traces[['excl_markers']]$curveNumber
    if (nrow(excl_markers) == 1) {
      xvals <- list(list(excl_markers$year))
      yvals <- list(list(excl_markers$ywp_val))
    } else {
      xvals <- list(excl_markers$year)
      yvals <- list(excl_markers$ywp_val)
    }

    plot_obj <- plot_obj %>%
      plotly::plotlyProxyInvoke(
        "restyle",
        list(
          x = xvals,
          y = yvals
        ),
        excl_curveNumber
      )
  } else if (excl_trace_exists) {
    excl_curveNumber <- traces[['excl_markers']]$curveNumber
    plot_obj <- plot_obj %>%
      plotly::plotlyProxyInvoke("deleteTraces", excl_curveNumber)
  }
  plot_obj
}

# calculate the crn mean
calc_mean_vals <- function(df_all, sel_mean) {
  switch(sel_mean,
    "mean" = {
      df_all |>
        dplyr::select(year, vals) |>
        collapse::fgroup_by(year) |>
        collapse::fsummarise(N = collapse::fnobs(vals),
                             vals = collapse::fmean(vals)) |>
        collapse::fsubset(N > 1) |> # only if we have at sample depth > 1
        dplyr::select(year, vals)
    },
    "tbrm" = {
      df_all |>
        dplyr::select(year, vals) |>
        dplyr::group_by(year) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::summarise(vals = dplR::tbrm(vals))
    },
    return(NULL)
  )
}

# draw (add) the mean trace
draw_mean_trace <- function(plot_obj, x_years, y_vals, sel_mean) {
  plot_obj %>%
    plotly::plotlyProxyInvoke(
      "addTraces",
      list(
        x = as.list(x_years),
        y = as.list(y_vals),
        name = "mean_trace",
        type = "scatter",
        mode = "lines",
        line = list(width = 2, color = "black"),
        showlegend = FALSE,
        hoverinfo = "skip",
        meta = list(role = "crnline", crn_type = sel_mean)
      )
    )
}

# helper to transform input$traces_crn into df for debugging
traces_to_df <- function(traces){
  traces |>
    purrr::map(\(x) {
      # separate nested from flat elements
      nested <- purrr::keep(x, is.list)
      flat   <- purrr::discard(x, is.list)
      if (!is.null(flat$visible)) { # visible may be logical or "legendonly" -> as char
        flat$visible <- as.character(flat$visible)
      }
      # flatten one level and convert to tibble row
      tibble::as_tibble(c(flat, unlist(nested, recursive = FALSE)))
    }) |>
    purrr::list_rbind(names_to = "name")
}

# calculate the corellation of sel wp trace with mean
calculate_correlation <- function(df_sel, df_all, x_axes, sel_mean) {
  # Use plot axes limits if available
  x_min <- x_axes$x_min %||% min(df_sel$year)
  x_max <- x_axes$x_max %||% max(df_sel$year)

  sel_wp <- df_sel$woodpiece_label[1]

  # filter selected trace values
  df_sel <- df_sel |>
    dplyr::filter(year >= x_min, year <= x_max, !exclude_dupl) |>
    dplyr::select(year, vals)
  df_all <- df_all |> 
    dplyr::filter(year >= x_min, year <= x_max, !exclude_dupl) 
  df_mean <- calc_mean_vals(df_all, sel_mean) # TODO: or only others?
  df_corr <- dplyr::inner_join(df_sel, df_mean, by = "year")

  if (nrow(df_corr) < 5) {
    msg <- "Not enough overlapping years to compute correlation."
  } else {
    # estimate correlation
    r <- cor(
      df_corr$vals.x,
      df_corr$vals.y,
      method = "pearson",
      use = "complete.obs"
    )
    n <- nrow(df_corr)
    msg <- glue::glue("Pearson correlation of {sel_wp} with {sel_mean} ({n} overlapping years): <strong>{round(r,3)}</strong>")
  }
  msg
}
