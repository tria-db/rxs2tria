server <- function(input, output, session) {
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

  # load prf_data from input file
  # TODO: or Renv like in shiny_meta?
  prf_data <- reactive({
    req(input$file_prf)
    tryCatch(
      {
        df_prf <- vroom::vroom(
          input$file_prf$datapath,
          col_types = c(.default = "d",
                        image_label = "c", year = "i", sector_n = "i")
        )
        # TODO: expand to use validate_df and align_to_structure, safe_block from shiny_meta?
        checkmate::assert_subset(c("image_label","year","sector_n"), names(df_prf))
        checkmate::assert_data_frame(df_prf[c("image_label","year","sector_n")], min.rows = 1, any.missing = FALSE)
        checkmate::assert_character(names(df_prf), min.len = 4)
        # check_vals <- checkmate::test_subset(df_prf |> dplyr::select(-dplyr::all_of(c("image_label","year","sector_n"))) |> names(),
        #                                     c("la_mean", ...))
        return(df_prf)
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error in QWA profile data",
          paste("There was an error reading the uploaded QWA profile data file:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
    )
  })

  # load rings data from input file
  # TODO: or Renv like in shiny_meta?
  rings_data <- reactive({
    req(input$file_rings)
    tryCatch(
      {
        # TODO: expand to use validate_df and align_to_structure, safe_block from shiny_meta?
        df_rings <- vroom::vroom(
          input$file_rings$datapath,
          col_types = c(.default = "d",
                        woodpiece_label = "c", slide_label = "c", image_label = "c",
                        year = "i", incomplete_ring = "l", missing_ring = "l",
                        duplicate_ring = "l", exclude_dupl = "l", exclude_issues = "l")
        )
        checkmate::assert_subset(c("woodpiece_label", "slide_label", "image_label","year",
                                   "incomplete_ring", "missing_ring","duplicate_ring",
                                   "exclude_dupl","exclude_issues"), names(df_rings))
        checkmate::assert_data_frame(df_rings[c("woodpiece_label", "slide_label", "image_label","year")],
                                     min.rows = 1, any.missing = FALSE)
        #checkmate::assert_character(names(df_prf), min.length = 10) # at least on measurements col?
        return(df_rings)
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error in QWA rings data",
          paste("There was an error reading the uploaded QWA rings data file:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
    )
  })

  observe({
    req(prf_data(), rings_data())
    # wp_choices <- unique(rings_data()$woodpiece_label)
    # updateSelectInput(session, "sel_wp", choices = wp_choices, selected = wp_choices)

    param_choices <- setdiff(names(prf_data()), c("image_label", "year", "sector_n"))
    updateSelectInput(session, "sel_param", choices = param_choices)

    sector_choices <- sort(unique(prf_data()$sector_n))
    updateSelectInput(session, "sel_sector", choices = sector_choices)
  })



  crn_data <- reactive({
    req(prf_data(), rings_data(), input$sel_sector)

    df_rings <- rings_data() |>
      dplyr::select(woodpiece_label, slide_label, image_label, year, exclude_dupl) |>
      dplyr::filter(!exclude_dupl)
    df_prf <- prf_data() |>
      dplyr::filter(sector_n == as.numeric(input$sel_sector)) |>
      dplyr::inner_join(df_rings, by = c("image_label", "year")) |>
      dplyr::arrange(woodpiece_label, year)

    df_prf
  })


  js_traces <- "function(el, x, inputName){
    el.on('plotly_restyle', function(evtData) {
      var out = {};
      function getTraceOpacity(trace, traceindex) {
        out[trace.name] = {curveNumber: traceindex, opacity: trace.opacity};
      }
      el._fullData.forEach(getTraceOpacity);
      Shiny.setInputValue(inputName, out);
    });
  }"

  output$ts_crn_plot <- plotly::renderPlotly({
    validate(
      need(prf_data(), "Please upload QWA profile data"),
      need(rings_data(), "Please upload QWA rings data"),
      need(input$sel_param, "Please select a QWA parameter to display"),
      need(input$sel_sector, "Please select a ring sector to display")
    )

    sel_param <- input$sel_param
    df_plot <- crn_data()

    color_palette <- extend_palette(base_palette, length(unique(df_plot$woodpiece_label)), contrasting = TRUE)

    p <- plotly::plot_ly(
      data = df_plot,
      x = ~year,
      y = ~.data[[sel_param]], # Dynamically select the PAR column
      color = ~woodpiece_label, # Ensure plot_treecode exists in the dataset
      colors = color_palette,
      type = 'scatter',
      mode = 'lines',
      name = ~woodpiece_label,
      source = "crn_plot"  # Set source ID here
    ) %>%
      plotly::layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = sel_param),
        showlegend = TRUE,
        legend = list(
          orientation = 'h',     # Horizontal orientation
          yanchor = 'bottom',    # Anchor the legend at the bottom
          y = 1.1,               # Place it slightly above the plot area
          xanchor = 'center',    # Center the legend horizontally
          x = 0.5,                # Position the legend at the center horizontally,
          itemclick = FALSE,      # Disable single click on legend
          itemdoubleclick = FALSE # Disable double click on legend
        )
      ) %>%
      plotly::event_register("plotly_click") %>%
      plotly::event_register("plotly_legendclick") %>%
      plotly::event_register("plotly_legenddoubleclick")

    plotly::config(p, doubleClickDelay=400, modeBarButtonsToRemove = list('hoverClosestCartesian', 'hoverCompareCartesian')) %>%
      htmlwidgets::onRender(js_traces, data = "traces_crn")
  })

  crn_click_data <- reactive({
    req(crn_data())
    plotly::event_data("plotly_click", source = "crn_plot", priority = "event")
  })

  crn_lgnd_click <- reactive({
    req(crn_data())
    plotly::event_data("plotly_legendclick", source = "crn_plot", priority = "event") # reevaluate on each click, even on same item
  })


  crn_lgnd_dblclick <- reactive({
    req(crn_data())
    plotly::event_data("plotly_legenddoubleclick", source = "crn_plot", priority = "event")
  })

  # Track pending single click
  pending_single_click <- reactiveVal(FALSE)

  # Legend single click handler with delay to ignore legend double clicks
  # Single click on legend item should toggle opacity of corresponding trace
  observeEvent(crn_lgnd_click(), {
    pending_single_click(TRUE)

    # Wait briefly to see if double-click comes
    shinyjs::delay(450, {
      # If still pending after delay, it was a true single click
      if (pending_single_click()) {
        pending_single_click(FALSE)

        sel_wp <- crn_lgnd_click()$name
        n_traces <- length(unique(crn_data()$woodpiece_label))
        curr_traces <- input$traces_crn
        if (is.null(curr_traces)){ # edge case for initialization
          current_op <- 1
          trace_id <- which(unique(crn_data()$woodpiece_label) == sel_wp) - 1  # zero-indexed
        } else{
          current_op <- curr_traces[[sel_wp]]$opacity
          trace_id <- curr_traces[[sel_wp]]$curveNumber
        }

        req(isTruthy(current_op), isTruthy(trace_id))

        # toggle opacity level and restyle the plot
        new_op <- if (current_op == 1) 0.25 else 1
        #has_marker <- (!is.null(input$traces_crn)) && (length(input$traces_crn) > n_traces)
        marker_id <- if (!is.null(crn_click_data()$curveNumber)) crn_click_data()$curveNumber else -1

        # if we dim a trace with the current marker, remove the marker as well
        if ((new_op == 0.25) &&
            (marker_id == trace_id)){
              plotly::plotlyProxy("ts_crn_plot", session) %>%
                plotly::plotlyProxyInvoke("deleteTraces", n_traces) %>%  # Remove nth trace if it exists (i.e. marker)
                plotly::plotlyProxyInvoke(
                  method = "restyle",
                  list(
                    opacity = new_op
                  ),
                  trace_id
                )
        } else {
          # otherwise just restyle the trace opacity
          plotly::plotlyProxy("ts_crn_plot", session) %>%
            #plotly::plotlyProxyInvoke("deleteTraces", n_traces) %>%  # Remove nth trace if it exists (i.e. marker)
            plotly::plotlyProxyInvoke(
              method = "restyle",
              list(
                opacity = new_op
              ),
              trace_id
            )
        }
      }
    })
  })

  # Legend double click handler
  # Double click on legend item toggles isolation of that trace (dimming all others)
  observeEvent(crn_lgnd_dblclick(), {
    pending_single_click(FALSE)  # Cancel the pending single click

    sel_wp <- crn_lgnd_dblclick()$name
    current_opacities <- input$traces_crn
    req(isTruthy(current_opacities))

    current_op <- current_opacities[[sel_wp]]$opacity
    n_traces <- length(unique(crn_data()$woodpiece_label))
    other_opacities <- sapply(current_opacities, function(x) x$opacity)
    other_opacities <- other_opacities[1:n_traces] # ignore potential marker trace
    other_opacities <- other_opacities[names(other_opacities) != sel_wp] # ignore clicked trace
    trace_id <- current_opacities[[sel_wp]]$curveNumber
    marker_id <- if (!is.null(crn_click_data()$curveNumber)) crn_click_data()$curveNumber else -1

    if (current_op == 1 && any(other_opacities > 0.25)) {
      # if clicked trace is currently visible and not already isolated, dim ALL OTHER traces
      p <- plotly::plotlyProxy("ts_crn_plot", session)
      if (marker_id != trace_id) {
        p <- p %>%
          plotly::plotlyProxyInvoke("deleteTraces", n_traces)  # Remove last trace if it's a marker
      }
      p %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 0.25
          ),
          (0:(n_traces-1))[-(trace_id+1)]
        )
    } else {
      # if clicked trace is currently dimmed or isolated, reset ALL traces to visible
      plotly::plotlyProxy("ts_crn_plot", session) %>%
        #plotly::plotlyProxyInvoke("deleteTraces", n_traces) %>%  # Remove last trace if it's a marker
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 1
          )
        )
    }
  })

  # Add a click event to add marker on selected point and highlight the trace
  observeEvent(crn_click_data(),{
    click_data <- crn_click_data()

    if (!is.null(click_data)) {
      trace_id <- click_data$curveNumber
      n_traces <- length(unique(crn_data()$woodpiece_label))

      # restyle the plot
      plotly::plotlyProxy("ts_crn_plot", session) %>%
        plotly::plotlyProxyInvoke("deleteTraces", n_traces) %>%  # Remove last trace if it's a marker
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 1
          ),
          trace_id
        ) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = list(click_data$x),
            y = list(click_data$y),
            mode = "markers",
            marker = list(
              size = 10,
              color = "red",
              symbol = "circle"
            ),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
        )
    }
  })

  output$debug <- renderPrint({
  #   req(input$traces_crn)
  #   click_data <- crn_click_data()
  #   sel_wp <- names(input$traces_crn[click_data$curveNumber + 1])
  #   sel_year <- click_data$x
  #   rings_data() %>% dplyr::filter(
  #     woodpiece_label == sel_wp,
  #     year == sel_year
  #   )
  })



  # prf_data_filt <- reactive({
  #   req(prf_data(), input$sel_wp, input$sel_param, input$sel_sector)
  #   df_filt <- prf_data() %>%
  #     dplyr::filter(
  #       sector_n == as.numeric(input$sel_sector)
  #     ) %>%
  #     dplyr::select(woodpiece_label, year, sector_n, dplyr::all_of(input$sel_param))
  #   return(df_filt)
  # })

}


