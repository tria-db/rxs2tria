server <- function(input, output, session) {

  # LOAD INPUT DATA ------------------------------------------------------------
  # reactive container for input data
  input_data <- reactiveValues(
    prf_data = NULL,
    rings_data = NULL,
    rxsmeta_data = NULL
  )

  # open input modal when btn_input is clicked
  observe({
    # warn before overwriting existing data
    no_existing_inputs <- all(
      is.null(input_data$prf_data),
      is.null(input_data$rings_data),
      is.null(input_data$rxsmeta_data))
    if (!no_existing_inputs) {
      showModal(
        modalDialog(
          title = "Warning",
          "This action overwrites any existing inputs provided in the app,
           including any unsaved edits.
           Are you sure you want to proceed?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_overwrite", "Proceed")
          )
        )
      )
    } else {
      show_input_source_modal()
    }
  }) |> bindEvent(input$btn_input)

  # if overwrite confirmed, also show input source modal
  observe({
    removeModal()
    show_input_source_modal()
  }) |> bindEvent(input$confirm_overwrite)

  # ui to provide input data based on selection (within input source modal)
  output$load_details_ui <- renderUI({
    req(input$load_type)
    if (input$load_type == "env") {
      tagList(
        "Provide the names of the data.frames in the current R environment:",
        textInput("name_prf", "Profile data", value = "prf_data"),
        textInput("name_rings", "QWA rings data", value = "QWA_data$rings"),
        textInput("name_rxsmeta", "ROXAS (image) metadata", value = "df_rxsmeta")
      )
    } else if (input$load_type == "csv") {
      tagList(
        "Provide the filepaths for the following:",
        fileInput("file_prf", "Upload the QWA profile data", accept = c(".csv")),
        fileInput("file_rings", "Upload the QWA rings data", accept = c(".csv")),
        fileInput("file_rxsmeta", "Upload the ROXAS metadata", accept = c(".csv"))
      )
    } else if (input$load_type == "example") { # example
      "not yet available..."
    } else {
      NULL
    }
  })

  # when input is confirmed, load data
  observe({
    safe_block({
      if (input$load_type == "env") {
        res <- load_data_env(
          name_prf = input$name_prf,
          name_rings = input$name_rings,
          name_rxsmeta = input$name_rxsmeta
        )
      } else if (input$load_type == "csv"){
        res <- load_data_csv(
          path_prf = input$file_prf$datapath,
          path_rings = input$file_rings$datapath,
          path_rxsmeta = input$file_rxsmeta$datapath
        )
      } else { # example
        # TODO: res <- load_example_data()
        removeModal()
        return(NULL)
      }

      valid_inputs <- validate_input_dfs(res$prf_data, res$rings_data, res$rxsmeta_data)
      # TODO: added flag cols as NA or FALSE?
      # TODO: what if we don't want to provide prf, only rings?
      # TODO: validate that the different dataframes match each other (prf in rings in rxsmeta)
      # TODO: check that duplicate ring flags are correct, and exclude_dupl is ok
      # TODO: check missing rings -> 0 mrw and prf measurements where applicable, rather than missing rows or NA?

      input_data$prf_data <- res$prf_data
      input_data$rings_data <- res$rings_data
      input_data$rxsmeta_data <- res$rxsmeta_data
      removeModal()
    },
      err_title = glue::glue("Error loading data from {input$load_type}"),
      err_message = "There was an error reading the provided input data.frames:\n",
      propagate_err = FALSE
    )
  }) |> bindEvent(input$confirm_input)

  # update sidebar UI based on loaded input data
  observe({
    # TODO: what if we have no rxsmeta_data or no prf_data?
    req(input_data$prf_data, input_data$rings_data, input_data$rxsmeta_data)
    prf_data <- input_data$prf_data
    rings_data <- input_data$rings_data
    rxsmeta_data <- input_data$rxsmeta_data

    # site and species select inputs
    species_choices <- unique(rxsmeta_data$species_code)
    updateSelectInput(session, "sel_species", choices = species_choices,
                      selected = species_choices[1])
    site_choices <- unique(rxsmeta_data$site_label)
    sel_sites <- rxsmeta_data |>
      dplyr::filter(species_code == species_choices[1]) |>
      dplyr::pull(site_label) |> unique()
    updateSelectInput(session, "sel_site", choices = site_choices,
                      selected = sel_sites)

    # parameter select input
    param_choices_prf <-  prf_data |>
      dplyr::select(dplyr::where(is.numeric), -year, -sector_n) |> names()
    param_choices_ring <- rings_data |>
      dplyr::select(dplyr::where(is.numeric), -year) |> names()
    updateSelectInput(session, "sel_param", choices = list(
      "Ring level" = param_choices_ring,
      "Agg. cell level" = param_choices_prf
    ), selected = param_choices_prf[1])

    # sector select input
    sector_choices <- sort(unique(prf_data$sector_n))
    updateSelectInput(session, "sel_sector", choices = sector_choices,
                      selected = sector_choices[1])
  }) |> bindEvent(input_data$prf_data,
                  input_data$rings_data,
                  input_data$rxsmeta_data,
                  ignoreInit = TRUE)

  # update select wp input based on sel site and species
  observe({
    req(input$sel_site, input$sel_species)
    rxsmeta_data <- input_data$rxsmeta_data |> dplyr::filter(
      site_label %in% input$sel_site,
      species_code %in% input$sel_species
    )
    wp_choices <- unique(rxsmeta_data$woodpiece_label)
    updateSelectInput(session, "sel_wp", choices = wp_choices,
                      selected = wp_choices)
  }) |> bindEvent(input$sel_site, input$sel_species, ignoreInit = TRUE)

  # enable/disable sector selection based on selected parameter
  observe({
    sel_param <- input$sel_param
    if (sel_param %in% names(input_data$prf_data)){
      shinyjs::enable("sel_sector")
    } else {
      shinyjs::disable("sel_sector")
    }
  }) |> bindEvent(input$sel_param, ignoreInit = TRUE)


  # REACTIVE CONTAINER: EDITED FLAGS DATA --------------------------------------
  # NOTE: rings_data_org is the basis for the plot, rings_data_edited tracks
  # edits of the flag inputs for selected rings. edits are then propagated back
  # to rings_data_org when the save / update btn is clicked
  rings_data_org <- reactiveVal(NULL)
  rings_data_edited <- reactiveVal(NULL)

  # initialize rings_data_out with input data
  observe({
    df_rings <- input_data$rings_data
    # initialize new flag columns if not present
    new_flag_cols <- setdiff(c(unname(discrete_features),
                               unname(disqual_issues),
                               unname(technical_issues),
                               unname(other_issues)),
                              # "exclude_scope", "comment"), #affected_tissue
                             names(df_rings))
    df_rings[new_flag_cols] <- FALSE
    # initialize comment and exclude_scope columns if not present
    if ("comment" %in% names(df_rings)){
      df_rings$comment <- as.character(df_rings$comment)
    } else {
      df_rings["comment"] <- NA_character_
    }
    if ("affected_tissue" %in% names(df_rings)){
      df_rings$affected_tissue <- as.character(df_rings$affected_tissue)
      # TODO: check in validation?
      if (any(!df_rings["affected_tissue"] %in% c("ew", "lw", "all"))){
        shiny::showNotification("Affected tissue values not in ['ew','lw','all'] converted to NA",
                                type = "warning")
        print("WARNING: INVALID AFFECTED TISSUE VALUES CONVERTED TO NA")
        df_rings[!df_rings["affected_tissue"] %in% c("ew", "lw", "all"), "affected_tissue"] <- NA_character_
      }
    } else {
      df_rings["affected_tissue"] <- NA_character_
    }

    rings_data_org(df_rings)
  }) |> bindEvent(input_data$rings_data)

  # initialize editable copy of rings_data_out
  observe({
    rings_data_edited(rings_data_org())
  }) |> bindEvent(rings_data_org())


  # REACTIVE CONTAINER: PLOT DATA ----------------------------------------------
  plot_data <- reactive({
    req(rings_data_org(), input$sel_param)
    sel_param <- input$sel_param

    # start from rings df for structure, exclusions and (if applicable) sel_param
    df_crn <- rings_data_org() |>
      dplyr::select(woodpiece_label, slide_label, image_label, year,
                    exclude_issues, exclude_dupl,
                    dplyr::any_of(sel_param))

    # if sel_param is in prf df, then join (selected sector only)
    if (sel_param %in% names(input_data$prf_data)){
      df_crn  <- input_data$prf_data |>
        dplyr::filter(sector_n == as.numeric(input$sel_sector)) |>
        dplyr::select(dplyr::all_of(c("image_label","year",sel_param))) |>
        dplyr::right_join(df_crn, by = c("image_label","year"))
    }

    # make sure we only have one value per year/woodpiece
    df_crn <- df_crn |> dplyr::filter(!exclude_dupl)
    df_crn$vals <- df_crn[[sel_param]]

    if (!input$show_excl){
      # 'remove' excluded years by setting param to NA
      #df_crn[df_crn$exclude_issues, input$sel_param] <- NA
      df_crn$vals[df_crn$exclude_issues] <- NA
    }

    # if (input$spline_det){
      # TODO
      # transform the plot data to df where
      # pivot wider to have (woodpiece_label, sel_param) as columns
      # year to row names
      # apply dplr::detrend(df, method = "Spline", nyrs = 32), check output
      # potentially reapply NAS
      # pivot longer back to original format
    # }

    # sort to get chronological timelines
    df_crn <- df_crn |>
      dplyr::arrange(woodpiece_label, year)

    df_crn
  }) |> bindEvent(rings_data_org(), # update of input_data$prf_data -> update of rings_data_org()
                  input$sel_param, input$sel_sector, input$show_excl, # TODO: , input$spline_det
                  ignoreInit = TRUE)

  # create color palette based on how many traces (wp) we plot
  color_palette <- reactive({
    req(plot_data())
    extend_palette(base_palette,
                   length(unique(plot_data()$woodpiece_label)),
                   contrasting = TRUE)
  })


  # PLOTLY ---------------------------------------------------------------------
  # JavaScript to capture trace info and send to Shiny
  js_traces <- "function(el, x, inputName){

    // Function to extract trace information
    function extractTraceInfo() {
      var out = {};
      // Use both el.data (original) and el._fullData (processed)
      el.data.forEach(function(trace, traceindex) {
        var fullTrace = el._fullData[traceindex];
        out[fullTrace.name] = {
          curveNumber:  traceindex,
          opacity:  fullTrace.opacity !== undefined ? fullTrace.opacity : 1,
          visible: fullTrace.visible !== undefined ? fullTrace.visible : true,
          meta: trace.meta !== undefined ? trace.meta : (fullTrace.meta !== undefined ? fullTrace.meta : null)
        };
      });
      return out;
    }

    // Helper to update Shiny input
    function updateShinyInput() {
      Shiny.setInputValue(inputName, extractTraceInfo());
    }

    // Initial state (after plot is fully rendered)
    el.on('plotly_afterplot', function() {
      console.log('Plot rendered, updating trace info');
      updateShinyInput();
    });

    // When traces are restyled (legend clicks, opacity changes)
    el.on('plotly_restyle', function(evtData) {
      console.log('Plot restyled');
      updateShinyInput();
    });
  }"

  # render the plot given the ring data and selected inputs
  output$ts_crn_plot <- plotly::renderPlotly({
    validate(
      need(isTruthy(plot_data()), "Please provide input data")
    )
    sel_param <- input$sel_param
    validate(need(any(!is.na(plot_data()$vals)),
                  "No data to display for the selected parameter and filters."))

    cat("=== PLOT RENDERING ===\n")
    cat("Time:", Sys.time(), "\n")

    p <- plotly::plot_ly(
      data = plot_data(),
      x = ~year,
      y = ~vals,
      color = ~woodpiece_label,
      colors = color_palette(),
      type = 'scatter',
      mode = 'lines',
      name = ~woodpiece_label,
      source = "crn_plot",  # set source ID
      meta = list(role = "orgline") # trace info for wp lines
    )

    p <- p %>%
      plotly::layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = sel_param),
        showlegend = TRUE,
        legend = list(
          orientation = 'h',      # Horizontal orientation
          yanchor = 'bottom',     # Anchor the legend at the bottom
          y = 1.1,                # Place it slightly above the plot area
          xanchor = 'center',     # Center the legend horizontally
          x = 0.5,                # Position the legend at the center horizontally,
          itemclick = FALSE,      # Disable single click on legend (custom handling)
          itemdoubleclick = FALSE # Disable double click on legend (custom handling)
        )
      ) %>%
      plotly::event_register("plotly_click") %>%
      plotly::event_register("plotly_legendclick") %>%
      plotly::event_register("plotly_legenddoubleclick") %>%
      plotly::event_register("plotly_relayout")

    # TODO: if show_excl, add a marker trace to highlight excluded points?
    # input$show_excl -> plot_data() -> plot render
    # what about highlighting other flagged years based on some input?

    # if we have axes limits from previous render, reset them
    if (!is.null(crn_x_axes())){
      x_axes <- crn_x_axes()
      if (!is.null(x_axes$x_min) && !is.null(x_axes$x_max)){
        p <- p %>%
          plotly::layout(
            xaxis = list(range = c(x_axes$x_min, x_axes$x_max))
          )
      }
    }

    # if (!is.null(trace_visiblity())){
    #   wp_traces <- sort(unique(plot_data()$woodpiece_label)) # get order of traces
    #   vis_ids <- match(trace_visiblity()$visible, wp_traces) - 1 # zero based
    #   invis_ids <- match(trace_visiblity()$invisible, wp_traces) - 1
    #
    #
    # }

    # plotly interaction settings
    plotly::config(
      p,
      doubleClickDelay=400,
      modeBarButtonsToRemove = list('hoverClosestCartesian',
                                    'hoverCompareCartesian')) %>%
      # add custom JS to capture shown traces
      htmlwidgets::onRender(js_traces, data = "traces_crn")
  }) |> bindEvent(plot_data(), ignoreNULL = FALSE)


  # PLOTLY REACTIVITY: AXES CHANGES --------------------------------------------
  # capture axes limit changes and keep track of them
  crn_change_axes <- reactive({
    req(plot_data())
    plotly::event_data("plotly_relayout",
                       source = "crn_plot")
  })

  crn_x_axes <- reactiveVal(NULL)

  observe({
    relayout <- crn_change_axes()
    x_axes <- crn_x_axes()

    if (!is.null(relayout[["xaxis.range[0]"]])){
      x_axes$x_min <- relayout[["xaxis.range[0]"]]
    }
    if (!is.null(relayout[["xaxis.range[1]"]])){
      x_axes$x_max <- relayout[["xaxis.range[1]"]]
    }
    if (!is.null(relayout[["xaxis.autorange"]])){
      x_axes$x_min <- NULL
      x_axes$x_max <- NULL
    }

    crn_x_axes(x_axes)
  }) |> bindEvent(crn_change_axes())


  # PLOTLY REACTIVITY: LEGEND (DOUBLE) CLICKS ----------------------------------
  # legend clicks can highlight / dim traces
  trace_opacity <- reactiveVal(list(
    on = character(0),
    off = character(0)
  ))

  crn_lgnd_click <- reactive({
    req(plot_data())
    plotly::event_data("plotly_legendclick",
                       source = "crn_plot", priority = "event")
  })

  crn_lgnd_dblclick <- reactive({
    req(plot_data())
    plotly::event_data("plotly_legenddoubleclick",
                       source = "crn_plot", priority = "event")
  })

  # keep track of pending legend single clicks
  pending_single_click <- reactiveVal(FALSE)

  # observe legend single clicks: toggle visibility of that trace
  observe({
    # register pending first click
    req(!awaiting_restoration()) # avoid running if plot is rerendered
    pending_single_click(TRUE)

    # delay briefly to identify single/double clicks
    # if it's still pending after delay, it was a true single click -> handle click
    # otherwise, do nothing here, see double click handler
    shinyjs::delay(450, {
      if (pending_single_click()){
        cat(".   single click\n")
        pending_single_click(FALSE) # reset pending

        req(isTruthy(input$traces_crn))
        trace_name <- crn_lgnd_click()$name
        current_traces <- input$traces_crn
        trace_id <- current_traces[[trace_name]]$curveNumber
        current_op <- current_traces[[trace_name]]$opacity

        op_on <- 1
        op_off <- 0.15
        new_op <- if (current_op == op_on) op_off else op_on

        wp_traces <- purrr::keep(current_traces, \(x) isTRUE(x$meta$role == "orgline"))
        wp_ops <- purrr::map_dbl(wp_traces, "opacity")
        wp_ops[trace_name] <- new_op
        trace_opacity(list(
          on = names(wp_ops)[wp_ops == op_on],
          off = names(wp_ops)[wp_ops == op_off]
        ))

        # check for marker on this trace
        marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "selring"))
        if (!is.null(marker_trace)){
          marker_on_trace <- (marker_trace$meta$orgName == trace_name)
        } else {
          marker_on_trace <- FALSE
        }

        p <- plotly::plotlyProxy("ts_crn_plot", session)
        if (new_op == op_off && marker_on_trace){
          # remove marker on selected trace (which will be dimmed)
          latest_marker(NULL)
          p <- p %>%
            plotly::plotlyProxyInvoke("deleteTraces", marker_trace$curveNumber)
        }
        p %>% plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = new_op
          ),
          trace_id
        )
      }
    })
  }) |> bindEvent(crn_lgnd_click(), ignoreNULL = TRUE, ignoreInit = TRUE)

  # observe legend double clicks:
  # toggle isolation of that trace (dimming all others)
  observe({
    req(!awaiting_restoration()) # avoid running if plot is rerendered
    req(isTruthy(input$traces_crn))
    cat(".   double click\n")

    pending_single_click(FALSE)  # cancel the pending single click from first click

    trace_name <- crn_lgnd_dblclick()$name
    current_traces <- input$traces_crn

    current_op <- current_traces[[trace_name]]$opacity
    other_traces <- purrr::keep(current_traces, \(x) isTRUE(x$meta$role == "orgline"))
    other_traces <- other_traces[names(other_traces) != trace_name]
    other_ops <- purrr::map_dbl(other_traces, "opacity")
    other_ids <- unname(purrr::map_int(other_traces, "curveNumber"))

    # check for marker on this trace
    marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "selring"))
    if (!is.null(marker_trace)){
      marker_on_trace <- (marker_trace$meta$orgName == trace_name)
    } else {
      marker_on_trace <- FALSE
    }

    op_on <- 1
    op_off <- 0.15
    # isolate trace if it's currently on and not already isolated
    if (current_op == op_on && any(other_ops > op_off)){
      trace_opacity(list(
        on = trace_name,
        off = names(other_traces)
      ))
      p <- plotly::plotlyProxy("ts_crn_plot", session)
      if (!marker_on_trace) {
        # remove marker on other trace (that will be dimmed)
        latest_marker(NULL)
        p <- p %>%
          plotly::plotlyProxyInvoke("deleteTraces", marker_trace$curveNumber)
      }
      # dim other traces
      p %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = op_off
          ),
          other_ids
        )
    } else {
      trace_opacity(list(
        on = names(other_traces),
        off = character(0)
      ))
      # reset all traces to on, leave marker as is
      plotly::plotlyProxy("ts_crn_plot", session) %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = op_on
          )
        )
    }
  }) |> bindEvent(crn_lgnd_dblclick(), ignoreNULL = TRUE, ignoreInit = TRUE)

  # PLOTLY REACTIVITY: CLICK TRACE EVENTS --------------------------------------
  # capture (single) click events on plotly (select ring)
  crn_click_data <- reactive({
    req(plot_data())
    # priority event: reevaluate on each click, even if same item
    plotly::event_data("plotly_click",
                       source = "crn_plot", priority = "event")
  })

  # observe plot clicks: add marker on selected point and highlight the trace
  latest_marker <- reactiveVal(NULL)

  observe({
    req(isTruthy(crn_click_data()), isTruthy(input$traces_crn))
    req(!awaiting_restoration())
    cat(".   plot click\n")

    click_data <- crn_click_data()
    current_traces <- input$traces_crn

    trace_id <- click_data$curveNumber
    trace_name <- names(current_traces)[
      purrr::detect_index(current_traces, \(x) isTRUE(x$curveNumber == trace_id))]
    marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "selring"))
    excl_marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "exclring"))
    existing_ids <- c(marker_trace$curveNumber,
                      excl_marker_trace$curveNumber)

    new_marker_name <- paste0(trace_name, ".", click_data$x)
    latest_marker(
        list(
        marker_name = new_marker_name,
        orgCurveNumber = trace_id,
        orgName = trace_name,
        year = click_data$x,
        val = click_data$y
      )
    )

    op_on <- 1
    op_off <- 0.15
    wp_traces <- purrr::keep(current_traces, \(x) isTRUE(x$meta$role == "orgline"))
    wp_ops <- purrr::map_dbl(wp_traces, "opacity")
    wp_ops[trace_name] <- op_on
    trace_opacity(list(
      on = names(wp_ops)[wp_ops == op_on],
      off = names(wp_ops)[wp_ops == op_off]
    ))

    # update the plot
    p <- plotly::plotlyProxy("ts_crn_plot", session)
    if (!is.null(existing_ids)){
      p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", existing_ids)
    }
    meta_info <- list(role = "selring", orgCurveNumber = trace_id,
                      orgName = trace_name, year = click_data$x)
    p <- p %>%
      # add new marker trace
      draw_selected_marker(click_data$x, click_data$y,
                           new_marker_name, meta_info) %>%
      # ensure that the clicked trace is "on"
      plotly::plotlyProxyInvoke(
        method = "restyle",
        list(opacity = op_on),
        trace_id
      )

    # adding pink trace
    # excl_markers <- get_new_excluded(
    #   rings_org = rings_data_org(),
    #   rings_edit = rings_data_edited(),
    #   sel_wp = input$sel_wp,
    #   plt_df = plot_data(),
    #   param = input$sel_param
    # )
    excl_markers <- get_excluded(
      rings_edit = rings_data_edited(),
      sel_wp = input$sel_wp,
      plt_df = plot_data()
    )

    if (nrow(excl_markers) > 0){
      p <- p %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = as.list(excl_markers$year),
            y = as.list(excl_markers$y),
            name = "excl_rings",
            mode = "markers",
            marker = list(
              size = 6,
              color = "hotpink",
              symbol = "circle-open"
            ),
            showlegend = FALSE,
            hoverinfo = "skip",
            meta = list(role = "exclring",
                        exclmarkers = paste(excl_markers$woodpiece_label,
                                            excl_markers$year,
                                            sep = ".", collapse = "; "))
          ))
    }

    p
  }) |> bindEvent(crn_click_data(), ignoreNULL = TRUE, ignoreInit = TRUE)


  # OBSERVE: Navigate to previous/next ring ------------------------------------
  observe({
    req(latest_marker())  # ensure a ring is currently selected
    current_marker <- latest_marker()
    sel_param <- input$sel_param

    adj_year <- get_adjacent_year(
      df = plot_data(),
      woodpiece = current_marker$orgName,
      current_year = current_marker$year,
      param = sel_param,
      direction = "prev"
    )

    if (!is.null(adj_year)){
      new_marker <- list(
        marker_name = paste0(current_marker$orgName, ".", adj_year$year),
        orgCurveNumber = current_marker$orgCurveNumber,
        orgName = current_marker$orgName,
        year = adj_year$year,
        val = adj_year$val
      )

      # update latest_marker
      latest_marker(new_marker)

      p <- plotly::plotlyProxy("ts_crn_plot", session)
      # remove existing marker
      marker_trace <- purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "selring"))
      excl_marker_trace <- purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "exclring"))
      existing_ids <- c(marker_trace$curveNumber,
                        excl_marker_trace$curveNumber)
      if (!is.null(existing_ids)){ # should always have it?
        p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", existing_ids)
      }

      # add new marker
      meta_info <- list(role = "selring", orgCurveNumber = new_marker$orgCurveNumber,
                        orgName = new_marker$orgName, year = adj_year$year)
      p <- p %>%
        draw_selected_marker(adj_year$year, adj_year$val,
                             new_marker$marker_name, meta_info)

      excl_markers <- get_excluded(
        rings_edit = rings_data_edited(),
        sel_wp = input$sel_wp,
        plt_df = plot_data()
      )

      if (nrow(excl_markers) > 0){
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = as.list(excl_markers$year),
              y = as.list(excl_markers$y),
              name = "excl_rings",
              mode = "markers",
              marker = list(
                size = 6,
                color = "hotpink",
                symbol = "circle-open"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "exclring",
                          exclmarkers = paste(excl_markers$woodpiece_label,
                                              excl_markers$year,
                                              sep = ".", collapse = "; "))
            ))
      }

      p
    }
  }) |> bindEvent(input$prev_ring)

  observe({
    req(latest_marker())  # ensure a ring is currently selected
    current_marker <- latest_marker()
    sel_param <- input$sel_param

    adj_year <- get_adjacent_year(
      df = plot_data(),
      woodpiece = current_marker$orgName,
      current_year = current_marker$year,
      param = sel_param,
      direction = "next"
    )

    if (!is.null(adj_year)){
      new_marker <- list(
        marker_name = paste0(current_marker$orgName, ".", adj_year$year),
        orgCurveNumber = current_marker$orgCurveNumber,
        orgName = current_marker$orgName,
        year = adj_year$year,
        val = adj_year$val
      )

      # update latest_marker
      latest_marker(new_marker)

      p <- plotly::plotlyProxy("ts_crn_plot", session)
      # remove existing marker
      marker_trace <- purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "selring"))
      excl_marker_trace <- purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "exclring"))
      existing_ids <- c(marker_trace$curveNumber,
                        excl_marker_trace$curveNumber)
      if (!is.null(existing_ids)){ # should always have it?
        p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", existing_ids)
      }

      # add new marker
      meta_info <- list(role = "selring", orgCurveNumber = new_marker$orgCurveNumber,
                        orgName = new_marker$orgName, year = adj_year$year)
      p <- p %>%
        draw_selected_marker(adj_year$year, adj_year$val,
                             new_marker$marker_name, meta_info)

      excl_markers <- get_excluded(
        rings_edit = rings_data_edited(),
        sel_wp = input$sel_wp,
        plt_df = plot_data()
      )

      if (nrow(excl_markers) > 0){
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = as.list(excl_markers$year),
              y = as.list(excl_markers$y),
              name = "excl_rings",
              mode = "markers",
              marker = list(
                size = 6,
                color = "hotpink",
                symbol = "circle-open"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "exclring",
                          exclmarkers = paste(excl_markers$woodpiece_label,
                                              excl_markers$year,
                                              sep = ".", collapse = "; "))
            ))
      }

      p
    }

  }) |> bindEvent(input$next_ring)


  # PLOTLY REACTIVITY: SELECTED WOODPIECES -------------------------------------
  trace_visibility <- reactiveVal(list(
    visible = character(0),
    invisible = character(0)
  ))

  # make deselected woodpieces invisible
  observe({
    req(isTruthy(input$traces_crn), isTruthy(input$sel_wp))
    cat(".   changed sel wp\n")
    selected_wps <- input$sel_wp
    current_traces <- input$traces_crn

    wp_traces <- purrr::keep(current_traces, \(x) isTRUE(x$meta$role == "orgline"))

    vis_traces <- wp_traces[names(wp_traces) %in% selected_wps]
    vis_ids <- unname(purrr::map_int(vis_traces, "curveNumber"))
    invis_traces <- setdiff(
      names(wp_traces),
      selected_wps
    )
    invis_traces <- wp_traces[names(wp_traces) %in% invis_traces]
    invis_ids <- unname(purrr::map_int(invis_traces, "curveNumber"))

    # Store the visibility state
    trace_visibility(list(
      visible = names(vis_traces),
      invisible = names(invis_traces)
    ))

    # remove existing any additional traces before updating them
    marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "selring"))
    excl_marker_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "exclring"))
    crn_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "crnline"))

    if (!is.null(marker_trace) &&
        (marker_trace$meta$orgName %in% names(invis_traces))) {
      # remove the sel marker trace only if its org trace is now invisible
      rm_selring_id <- marker_trace$curveNumber
      latest_marker(NULL)
    } else {
      rm_selring_id <- NULL
    }

    existing_ids <- c(rm_selring_id, excl_marker_trace$curveNumber, crn_trace$curveNumber)

    p <- plotly::plotlyProxy("ts_crn_plot", session)
    if (!is.null(existing_ids)){
      p <- p %>%
        plotly::plotlyProxyInvoke("deleteTraces", existing_ids)
    }

    # apply visibility changes
    all_trace_ids <- c(vis_ids, invis_ids)
    visibility_values <- c(rep(TRUE, length(vis_ids)), rep(FALSE, length(invis_ids)))

    p <- p %>%
      plotly::plotlyProxyInvoke(
        method = "restyle",
        list(
          visible = visibility_values
        ),
        all_trace_ids
      )

    # re-add the excl markers if applicable
    # excl_markers <- get_new_excluded(
    #   rings_org = rings_data_org(),
    #   rings_edit = rings_data_edited(),
    #   sel_wp = input$sel_wp,
    #   plt_df = plot_data(),
    #   param = input$sel_param
    # )
    excl_markers <- get_excluded(
      rings_edit = rings_data_edited(),
      sel_wp = input$sel_wp,
      plt_df = plot_data()
    )

    if (nrow(excl_markers)>0){
      cat(".   restoring excl markers after sel_wp change\n")
      p <- p %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = as.list(excl_markers$year),
            y = as.list(excl_markers$y),
            name = "excl_rings",
            mode = "markers",
            marker = list(
              size = 6,
              color = "hotpink",
              symbol = "circle-open"
            ),
            showlegend = FALSE,
            hoverinfo = "skip",
            meta = list(role = "exclring",
                        exclmarkers = paste(excl_markers$woodpiece_label,
                                            excl_markers$year,
                                            sep = ".", collapse = "; "))
          ))
    }

    # re-add the crn mean trace if applicable
    sel_mean <- input$mean_type
    if (sel_mean %in% c("mean", "tbrm")){
      cat(".   restoring crn mean after sel_wp change\n")
      df_crn <- plot_data()
      sel_prm <- input$sel_param
      if (sel_mean == "mean"){
        df_mean <- df_crn |>
          dplyr::filter(!exclude_issues) |>
          dplyr::filter(woodpiece_label %in% input$sel_wp) |>
          dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
          collapse::fgroup_by(year) |>
          collapse::fmean()
      } else if (sel_mean == "tbrm"){
        df_mean <- df_crn |>
          dplyr::filter(woodpiece_label %in% input$sel_wp) |>
          dplyr::filter(!exclude_issues) |>
          dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
          dplyr::group_by(year) |>
          dplyr::summarise("{sel_prm}" := dplR::tbrm(.data[[sel_prm]]))
      }

      p <- p %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = df_mean$year,
            y = df_mean[[sel_prm]],
            name = paste("crn.", sel_mean, sep = ""),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 2, color = 'black'),
            showlegend = FALSE,
            hoverinfo = "skip",
            meta = list(role = "crnline")
          ))
    }

    p
  }) |> bindEvent(input$sel_wp, ignoreNULL = TRUE, ignoreInit = TRUE)


  # COMPUTE CORRELATION --------------------------------------------------------
  correlation_result <- reactive({
    req(plot_data())
    req(latest_marker())
    req(input$sel_param)
    req(input$sel_wp)

    # Get current axis limits
    x_axes <- crn_x_axes()
    # If no axis limits yet, set to NULL (optional, but safe)
    if (is.null(x_axes)) x_axes <- list(x_min = NULL, x_max = NULL)

    sel_prm <- input$sel_param
    current_trace <- latest_marker()$orgName

    # ---- single trace ----
    df_plot <- plot_data() %>%
      dplyr::filter(
        woodpiece_label == current_trace,
        !exclude_issues
      ) %>%
      dplyr::select(year, value = dplyr::all_of(sel_prm))

    # ---- mean chronology ----
    df_mean <- plot_data() %>%
      dplyr::filter(
        woodpiece_label %in% input$sel_wp,
        !exclude_issues
      ) %>%
      dplyr::select(year, mean_value = dplyr::all_of(sel_prm))

    if (input$mean_type %in% c("mean", "none")){
      df_mean <- df_mean %>%
        collapse::fgroup_by(year) %>%
        collapse::fmean()
    } else { #if (input$mean_type == "tbrm")
      df_mean <- df_mean %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(mean_value = dplR::tbrm(.data$mean_value))
    }

    # ---- align by year ----
    df_corr <- dplyr::inner_join(df_plot, df_mean, by = "year")

    # ---- filter by current x-axis range if set ----
    if (!is.null(x_axes$x_min) && !is.null(x_axes$x_max)) {
      df_corr <- df_corr %>%
        dplyr::filter(year >= x_axes$x_min, year <= x_axes$x_max)
    }

    # ---- safety check ----
    if (nrow(df_corr) < 5) {
      return(list(
        ok = FALSE,
        mean_type = ifelse(input$mean_type == "tbrm", "tbrm", "mean"),
        msg = "Not enough overlapping years to compute correlation."
      ))
    }

    # ---- compute correlation ----
    r <- cor(
      df_corr$value,
      df_corr$mean_value,
      method = "pearson",
      use = "complete.obs"
    )

    list(
      ok = TRUE,
      mean_type = ifelse(input$mean_type == "tbrm", "tbrm", "mean"),
      r = r,
      n = nrow(df_corr),
      trace = current_trace
    )
  })



  # RESTORE EDITS IF THE PLOT IS RERENDERED ------------------------------------
  awaiting_restoration <- reactiveVal(FALSE)

  # if plot data changes, set flag to restore state after render
  observe({
    awaiting_restoration(TRUE)
  }) |> bindEvent(plot_data(),
                  ignoreNULL = TRUE,
                  ignoreInit = TRUE)

   # Restore after plot renders
  observe({
    req(awaiting_restoration())
    req(input$traces_crn)
    req(isTruthy(trace_visibility()))
    req(isTruthy(trace_opacity()))
    cat("...traces_crn updated, restoring state\n")

    awaiting_restoration(FALSE) # only do it once

    current_traces <- input$traces_crn
    isolate({

      visibility_state <- trace_visibility()
      opacity_state <- trace_opacity()
      op_on <- 1
      op_off <- 0.15

      wp_traces <- purrr::keep(current_traces, \(x) isTRUE(x$meta$role == "orgline"))

      # Find traces that should be visible / on
      vis_traces <- wp_traces[names(wp_traces) %in% visibility_state$visible]
      vis_ids <- unname(purrr::map_int(vis_traces, "curveNumber"))
      on_traces <- wp_traces[names(wp_traces) %in% opacity_state$on]
      on_ids <- unname(purrr::map_int(on_traces, "curveNumber"))

      # Find traces that should be invisible / off
      invis_traces <- wp_traces[names(wp_traces) %in% visibility_state$invisible]
      invis_ids <- unname(purrr::map_int(invis_traces, "curveNumber"))
      off_traces <- wp_traces[names(wp_traces) %in% opacity_state$off]
      off_ids <- unname(purrr::map_int(off_traces, "curveNumber"))

      sel_marker <- latest_marker()
      marker_y <- NULL
      if (!is.null(sel_marker)){
        marker_y <- plot_data() |>
          dplyr:: filter(woodpiece_label == sel_marker$orgName,
                         year == sel_marker$year, !exclude_dupl) |>
          dplyr:: pull(input$sel_param)
      }

      # Apply changes to plot
      p <- plotly::plotlyProxy("ts_crn_plot", session)

      # visibility
      vis_trace_ids <- c(vis_ids, invis_ids)
      visibility_values <- c(rep(TRUE, length(vis_ids)), rep(FALSE, length(invis_ids)))
      cat(".   restoring visibility:\n")
      cat("    on:", paste(vis_ids, collapse = ", "), "\n")
      cat("    off:", paste(invis_ids, collapse = ", "), "\n")

      p <- p %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(visible = visibility_values),
          vis_trace_ids
        )

      op_trace_ids <- c(on_ids, off_ids)
      op_values <- c(rep(op_on, length(on_ids)), rep(op_off, length(off_ids)))
      cat(".   restoring opacity:\n")
      cat("    on:", paste(on_ids, collapse = ", "), "\n")
      cat("    off:", paste(off_ids, collapse = ", "), "\n")
      # opacity
      p <- p %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(opacity = op_values),
          op_trace_ids
        )

      # re-add marker if applicable
      if (!is.null(sel_marker) && length(marker_y) > 0){
        cat(".   restoring sel ring marker\n")
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = list(sel_marker$year),
              y = list(marker_y),
              name = sel_marker$marker_name,
              mode = "markers",
              marker = list(
                size = 10,
                color = "red",
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "selring", orgCurveNumber = sel_marker$orgCurveNumber,
                          orgName = sel_marker$orgName, year = sel_marker$year)
            ))
      }

      # re-add excl if applicable
      # excl_markers <- get_new_excluded(
      #   rings_org = rings_data_org(),
      #   rings_edit = rings_data_edited(),
      #   sel_wp = input$sel_wp,
      #   plt_df = plot_data(),
      #   param = input$sel_param
      # )
      excl_markers <- get_excluded(
        rings_edit = rings_data_edited(),
        sel_wp = input$sel_wp,
        plt_df = plot_data()
      )

      if (nrow(excl_markers)>0){
        cat(".   restoring excl markers\n")
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = as.list(excl_markers$year),
              y = as.list(excl_markers$y),
              name = "excl_rings",
              mode = "markers",
              marker = list(
                size = 6,
                color = "hotpink",
                symbol = "circle-open"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "exclring",
                          exclmarkers = paste(excl_markers$woodpiece_label,
                                              excl_markers$year,
                                              sep = ".", collapse = "; "))
            ))
      }

      # re-add crn mean trace if applicable
      sel_mean <- input$mean_type
      if (sel_mean %in% c("mean", "tbrm")){
        df_crn <- plot_data()
        sel_prm <- input$sel_param

      if (sel_mean == "mean"){
        df_mean <- df_crn |>
          dplyr::filter(!exclude_issues) |>
          dplyr::filter(woodpiece_label %in% input$sel_wp) |>
          dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
          collapse::fgroup_by(year) |>
          collapse::fmean()
      } else if (sel_mean == "tbrm"){
        df_mean <- df_crn |>
          dplyr::filter(woodpiece_label %in% input$sel_wp) |>
          dplyr::filter(!exclude_issues) |>
          dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
          dplyr::group_by(year) |>
          dplyr::summarise("{sel_prm}" := dplR::tbrm(.data[[sel_prm]]))
      }

      cat(".   restoring mean trace\n")
      p <- p %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = df_mean$year,
            y = df_mean[[sel_prm]],
            name = paste("crn.", sel_mean, sep = ""),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 2, color = 'black'),
            showlegend = FALSE,
            hoverinfo = "skip",
            meta = list(role = "crnline")
          ))
      }

      cat("... rerender complete\n")
      p
    })

  }) |> bindEvent(
    input$traces_crn,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )




  # adding a mean trace based on input
  observe({
    req(plot_data(), input$traces_crn)
    cat(".   mean crn update\n")

    df_crn <- plot_data()
    sel_prm <- input$sel_param
    sel_mean <- input$mean_type

    current_traces <- input$traces_crn
    crn_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "crnline"))

    p <- plotly::plotlyProxy("ts_crn_plot", session)
    if (!is.null(crn_trace)){
      # remove existing mean trace
      p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", crn_trace$curveNumber)
    }

    if (sel_mean == "none"){
      return(p)
    } else if (sel_mean == "mean"){
      df_mean <- df_crn |>
        dplyr::filter(!exclude_issues) |>
        dplyr::filter(woodpiece_label %in% input$sel_wp) |>
        dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
        collapse::fgroup_by(year) |>
        collapse::fmean()
    } else if (sel_mean == "tbrm"){
      df_mean <- df_crn |>
        dplyr::filter(!exclude_issues) |>
        dplyr::filter(woodpiece_label %in% input$sel_wp) |>
        dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
        dplyr::group_by(year) |>
        dplyr::summarise("{sel_prm}" := dplR::tbrm(.data[[sel_prm]]))
    }

    p <- p %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          x = df_mean$year,
          y = df_mean[[sel_prm]],
          name = paste("crn.", sel_mean, sep = ""),
          type = 'scatter',
          mode = 'lines',
          line = list(width = 2, color = 'black'),
          showlegend = FALSE,
          hoverinfo = "skip",
          meta = list(role = "crnline")
        ))
    return(p)
  }) |> bindEvent(input$mean_type)


  #shinyjs::disable(selector = paste0("#sel_desquial .checkbox:nth-child(1) label"))

  # REACTIVE CONTAINER: SELECTED RING ------------------------------------------
  clicked_ring <- reactive({
    sel_marker <- latest_marker()
    if (!is.null(sel_marker)) {
      sel_wp_label <- sel_marker$orgName
      sel_year <- sel_marker$year
      sel_param <- input$sel_param

      sel_flags_data <- rings_data_edited() |>
        dplyr::filter(woodpiece_label == sel_wp_label, year == sel_year) |>
        dplyr::filter(!exclude_dupl) # TODO: keep all for handling duplicates?

      cov_data <- rings_data_edited() |>
        dplyr::filter(woodpiece_label == sel_wp_label) |>
        dplyr::select(woodpiece_label, year, exclude_dupl, slide_label, image_label, cno, dplyr::any_of(sel_param)) |>
        get_overlapping_images(sel_flags_data$image_label)
      cov_imgs <- unique(cov_data$image_label)
      if (sel_param %in% names(input_data$prf_data)){
        cov_data  <- input_data$prf_data |>
          dplyr::filter(image_label %in% cov_imgs, year == sel_year,
                        sector_n == as.numeric(input$sel_sector)) |>
          dplyr::select(dplyr::all_of(c("image_label","year",sel_param))) |>
          dplyr::right_join(cov_data, by = c("image_label","year")) |>
          dplyr::select(woodpiece_label, year, exclude_dupl, slide_label, image_label, cno, dplyr::any_of(sel_param))
      }

      return(
        list(
          woodpiece_label = sel_wp_label,
          year = sel_year,
          val = sel_marker$val,
          data = sel_flags_data,
          coverage = cov_data
        )
      )
    } else {
      NULL
    }
  }) %>% bindEvent(latest_marker(), input$sel_param, ignoreNULL = FALSE, ignoreInit = TRUE) # input$sel_wp

  # the ring editor card title
  output$sel_ring <- renderUI({
    req(clicked_ring())
    df <- clicked_ring()$data
    paste("Selected ring:", clicked_ring()$woodpiece_label, "| Year:", df$year)
  })

  output$ring_info <- renderUI({
    req(clicked_ring())

    valid_samples <- rings_data_edited() |>
        dplyr::filter(woodpiece_label %in% input$sel_wp,
                      year == clicked_ring()$year,
                      !exclude_dupl, !exclude_issues) |> dplyr::pull(image_label)
    sample_depth <- plot_data() |> dplyr::filter(
      image_label %in% valid_samples,
      year == clicked_ring()$year,
      !is.na(.data[[input$sel_param]])
    ) |> nrow()

    corr_res <- correlation_result()

    tags$div(
      # tags$strong("Woodpiece:  "), clicked_ring()$woodpiece_label, tags$br(),
      # tags$strong("Slide: "), clicked_ring()$data$slide_label, tags$br(),
      # tags$strong("Image: "), clicked_ring()$data$image_label, tags$br(),
      # tags$strong("Year: "), clicked_ring()$year, tags$br(),
      # tags$strong("Value: "), sprintf("%.3f", clicked_ring()$val), tags$br(),
      tags$strong("Sample depth: "), sample_depth,"  |  ",
      tags$strong("Correlation of trace with", corr_res$mean_type, ": "),
      if (corr_res$ok) {
        tags$span(paste0(sprintf("%.3f", corr_res$r),
          " (", corr_res$n, " overlapping years)"))
      } else {
        tags$i(corr_res$msg)
      }, "  |  ",
      tags$strong("Image overlap: "),
      if (clicked_ring()$data$duplicate_ring){
        tags$strong(style="color: red;", "Multiple images from the woodpiece cover this year, see coverage.")
      } else {
        tags$i("None")
      }
    )
  })

  output$sample_depth <- renderUI({
    req(clicked_ring())

    valid_samples <- rings_data_edited() |>
      dplyr::filter(woodpiece_label %in% input$sel_wp,
                    year == clicked_ring()$year,
                    !exclude_dupl, !exclude_issues) |> dplyr::pull(image_label)
    sample_depth <- plot_data() |> dplyr::filter(
      image_label %in% valid_samples,
      year == clicked_ring()$year,
      !is.na(.data[[input$sel_param]])
    ) |> nrow()

    tags$span(paste(sample_depth, "samples with current selections"))
  })

  output$corr_res <- renderUI({
    corr_res <- correlation_result()
    if (corr_res$ok) {
      tags$span(paste0(sprintf("%.3f", corr_res$r),
                       " (", corr_res$n, " overlapping years)"))
    } else {
      tags$span(corr_res$msg)
    }
  })

  output$ring_overview <- reactable::renderReactable({
    req(clicked_ring())
    df_rings <- clicked_ring()$coverage
    df_rings <- df_rings |>
      dplyr::filter(year == clicked_ring()$year) |>
      dplyr::select(dplyr::any_of(
        c("woodpiece_label", "slide_label", "image_label",
          "year", "cno", input$sel_param, "exclude_dupl")))

    if (nrow(df_rings) > 1) {
      df_rings <- df_rings |> dplyr::filter(!exclude_dupl)
    }
    df_rings$selected <- "X"
    df_rings <- df_rings |> dplyr::select(-exclude_dupl)

    reactable::reactable(
      df_rings,
      columns = c(
        setNames(
          list(reactable::colDef(format = reactable::colFormat(digits = 3))),
          input$sel_param
        ),
        list(
          selected = reactable::colDef(
            name = "",
            cell = function(value, row_index) {
              if (value == "X") {
                ""
              } else {
                value
              }
            }
          )
        )
      ),
      rowStyle = function(index) {
        list(fontWeight = "bold", background = "#e6f3ff")
      },
      compact = TRUE,
      borderless = FALSE
    )
  })


  output$cov_info <- renderPlot({
    req(clicked_ring())
    df_rings <- clicked_ring()$coverage
    ggplot2::ggplot(df_rings, ggplot2::aes(x = year, y = image_label, fill = cno)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(direction = -1) +
      ggplot2::geom_point(
         data = df_rings |> dplyr::filter(image_label == clicked_ring()$data$image_label,
                                          year == clicked_ring()$year),
         color = "red", fill = NA,   size = 5
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                     axis.title=ggplot2::element_blank()) +
      ggplot2::coord_fixed()

  })

  output$tree_overview <- reactable::renderReactable({
    req(clicked_ring())
    df_rings <- clicked_ring()$coverage
    df_rings <- df_rings |>
      dplyr::filter(year == clicked_ring()$year) |>
      dplyr::select(dplyr::any_of(
        c("woodpiece_label", "slide_label", "image_label",
          "year", "cno", input$sel_param, "exclude_dupl")))

    if (nrow(df_rings) > 1) {
      df_rings$selected <- ifelse(df_rings$exclude_dupl, "", "selected")
      df_rings <- df_rings |> dplyr::arrange(desc(selected), image_label)
    } else {
      df_rings$selected <- "X"
    }
    df_rings <- df_rings |> dplyr::select(-exclude_dupl)

    reactable::reactable(
      df_rings,
      columns = c(
        setNames(
          list(reactable::colDef(format = reactable::colFormat(digits = 3))),
          input$sel_param
        ),
        list(
          selected = reactable::colDef(
            name = "",
            cell = function(value, row_index) {
              if (value == "X") {
                ""
              } else if (value == "selected") {
                value
              } else {
                row <- df_rings[row_index, ]
                htmltools::tags$button(
                  class = "btn btn-sm btn-outline-primary",
                  onclick = sprintf(
                    "Shiny.setInputValue('select_dupl', {image:  '%s', year:  %s, nonce: Math.random()})",
                    row$image_label, row$year
                  ),
                  "Select instead"
                )
              }
            }
          )
        )
      ),
      rowStyle = function(index) {
        if (!df_rings$selected[index] %in% c("X","selected")) {
          list(color = "#999", background = "#f5f5f5")
        } else {
          list(fontWeight = "bold", background = "#e6f3ff")
        }
      },
      compact = TRUE,
      borderless = FALSE
    )
  })

  expected_excl <- reactiveVal(NULL)

  # update ring edit card when a ring is selected with existing data
  observe({
    shinyjs::toggle(id = "ring_editor_card", condition = !is.null(clicked_ring()))

    # if a ring is selected, update the inputs with saved flags and comment
    req(clicked_ring())

    saved_flags <- clicked_ring()$data
    expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

    shinyjs::toggle(id = "sel_affected", condition = saved_flags$exclude_issues)
    if (saved_flags$exclude_issues) {
      updateRadioButtons(session, "sel_affected",
                         selected = ifelse(is.na(saved_flags$affected_tissue),
                                           "NA", saved_flags$affected_tissue))
    }
    # TODO: needed? } else {
    #   updateRadioButtons(session, "sel_affected",
    #                      selected = "NA")
    # }

    sel_disc_flags <- saved_flags %>%
      dplyr::select(unname(discrete_features))
    sel_disc_flags <- names(sel_disc_flags)[sel_disc_flags[1,] == TRUE]

    updateCheckboxGroupInput(session, "sel_discrete",
                             selected = sel_disc_flags)

    sel_disq_flags <- saved_flags %>%
      dplyr::select(unname(disqual_issues))
    sel_disq_flags <- names(sel_disq_flags)[sel_disq_flags[1,] == TRUE]

    updateCheckboxGroupInput(session, "sel_disqual",
                             selected = sel_disq_flags)
    #shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')

    sel_tech_issues <- saved_flags %>%
      dplyr::select(unname(technical_issues))
    sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
    updateCheckboxGroupInput(session, "sel_technical_exact",
                         selected = sel_tech_issues)

    sel_other_issues <- saved_flags %>%
      dplyr::select(unname(other_issues))
    sel_other_issues <- names(sel_other_issues)[sel_other_issues[1,] == TRUE]
    updateCheckboxGroupInput(session, "sel_other_iss",
                         selected = sel_other_issues)

    # TODO: add other issues for disq card?

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(clicked_ring(), ignoreNULL = FALSE, ignoreInit = FALSE)


  # toggle the affected tissue selector based on exclude yes/no
  observe({
    shinyjs::toggle(id = "sel_affected", condition = input$sel_exclude == "yes")
    # TODO: should also reset the value if hidden? read from where if shown?
  }) |> bindEvent(input$sel_exclude)


  # toggle warning message on certain disqualifying issues
  # if at least one of "incomplete_ring", "missing_ring", "compression_wood" is in input$sel_disqual,
  # then the uiOutput "warn_disq" should give a warning message:
  observeEvent(c(input$sel_disqual, input$sel_exclude), {
    if (isTruthy(input$sel_disqual)) {
      sel_warning <- intersect(c("incomplete_ring" , "missing_ring", "x_dating",
                                 "compression_wood", "orientation"),
                               input$sel_disqual)
      if (length(sel_warning) > 0 && input$sel_exclude == "no") {
        output$warn_disq <- renderUI({
          tags$div(
            style = "color: red; font-weight: bold;",
            "Warning: Rings with these issues should normally be excluded!\n",
            paste(sel_warning, collapse = ", ")
          )
        })
      } else {
        output$warn_disq <- renderUI({
          NULL
        })
      }
    } else {
      output$warn_disq <- renderUI({
        NULL
      })
    }

  }, ignoreNULL = FALSE)


  # SAVING FLAG EDITS ----------------------------------------------------------
  # Reactive container to hold all the flag edits (slightly debounced to avoid too many updates)
  flag_changes <- reactive({
    #req(isTruthy(clicked_ring()))
    list(
      exclude = input$sel_exclude,
      affected = input$sel_affected,
      # exclude_scope = if (!is.null(input$sel_exclude_scope))
      #   input$sel_exclude_scope
      # else
      #   NA_character_,
      discrete = input$sel_discrete,
      disqual = input$sel_disqual,
      technical = input$sel_technical_exact,
      other = input$sel_other_iss,
      comment = input$sel_comment
    )
  }) %>% debounce(150)

  # update rings_data_edited when flag_changes occur
  observe({
    req(isTruthy(clicked_ring()))
    # TODO: should not be triggerd if a different ring is clicked!!

    excl_flag <- ifelse(flag_changes()$exclude == "yes", TRUE, FALSE)
    disc_flags_on <- flag_changes()$discrete
    disc_flags_off <- setdiff(unname(discrete_features), disc_flags_on)
    disq_flags_on <- flag_changes()$disqual
    disq_flags_off <- setdiff(unname(disqual_issues), disq_flags_on)
    techn_flags_on <- flag_changes()$technical
    techn_flags_off <- setdiff(unname(technical_issues), techn_flags_on)
    other_flags_on <- flag_changes()$other
    other_flags_off <- setdiff(unname(other_issues), other_flags_on)

    df_rings <- rings_data_edited()
    clicked_data <- clicked_ring()$data
    ring_id <- which(df_rings$image_label == clicked_data$image_label &
                          df_rings$year == clicked_data$year)

    df_rings[ring_id, "exclude_issues"] <- excl_flag
    df_rings[ring_id, c(disc_flags_on, disq_flags_on, techn_flags_on, other_flags_on)] <- TRUE#
    df_rings[ring_id, c(disq_flags_off, disc_flags_off, techn_flags_off, other_flags_off)] <- FALSE #
    df_rings[ring_id, "comment"] <- flag_changes()$comment

    # TODO: change to affected_tissue
    # Add the new flag column for EW/LW/ALL
    # scope_val <- flag_changes()$exclude_scope
    # if (is.null(scope_val) || length(scope_val) == 0) scope_val <- NA_character_
    #
    # if (!"exclude_scope" %in% names(df_rings)) {
    #   df_rings$exclude_scope <- NA_character_
    # }
    #
    # if (excl_flag) {
    #   df_rings[ring_id, "exclude_scope"] <- scope_val
    # } else {
    #   df_rings[ring_id, "exclude_scope"] <- NA_character_
    # }

    rings_data_edited(df_rings)

  }) |> bindEvent(flag_changes(), ignoreNULL = FALSE, ignoreInit = TRUE)


  # # revert to pre-edit data if "Discard changes" is clicked
  # # can use clicked_ring()$data which is only updated when (another) ring is clicked
  # observe({
  #   req(isTruthy(clicked_ring()))
  #
  #   saved_flags <- clicked_ring()$data
  #   expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
  #   updateRadioButtons(session, "sel_exclude",
  #                      selected = ifelse(saved_flags$exclude_issues, "yes", "no"))
  #
  #   # NEW: Restore EW/LW/ALL selection
  #   # TODO: change to affected tissue
  #   # if (!is.na(saved_flags$exclude_scope)) {
  #   #   updateRadioButtons(session, "sel_exclude_scope",
  #   #                      selected = saved_flags$exclude_scope)
  #   # } else {
  #   #   updateRadioButtons(session, "sel_exclude_scope", selected = "ALL")
  #   # }
  #
  #   sel_disc_flags <- saved_flags %>%
  #     dplyr::select(unname(discrete_features))
  #   sel_disc_flags <- names(sel_disc_flags)[sel_disc_flags[1,] == TRUE]
  #
  #   updateCheckboxGroupInput(session, "sel_discrete",
  #                            selected = sel_disc_flags)
  #
  #   sel_disq_flags <- saved_flags %>%
  #     dplyr::select(unname(disqual_issues))
  #   sel_disq_flags <- names(sel_disq_flags)[sel_disq_flags[1,] == TRUE]
  #
  #   updateCheckboxGroupInput(session, "sel_disqual",
  #                            selected = sel_disq_flags)
  #   shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')
  #
  #   # if ("technical_issues" %in% sel_disq_flags) {
  #   #   shinyjs::enable("sel_technical_exact")
  #     sel_tech_issues <- saved_flags %>%
  #       dplyr::select(unname(technical_issues))
  #     sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
  #     updateSelectizeInput(session, "sel_technical_exact",
  #                          selected = sel_tech_issues)
  #   # } else {
  #   #   shinyjs::disable("sel_technical_exact")
  #   #   updateSelectizeInput(session, "sel_technical_exact",
  #   #                        selected = character(0))
  #   # }
  #
  #   updateTextAreaInput(session, "sel_comment",
  #                       value = saved_flags$comment)
  #
  # }) |> bindEvent(input$undo_sel_flags, ignoreNULL = TRUE, ignoreInit = TRUE)


  # revert to raw input data if "reset_to_raw" is clicked
  observe({
    req(isTruthy(clicked_ring()))

    sel_img <- clicked_ring()$data$image_label
    sel_year <- clicked_ring()$data$year

    # initialize new flag columns if not present
    new_flag_cols <- setdiff(c(unname(discrete_features),
                               unname(disqual_issues), unname(technical_issues),
                               unname(other_issues),
                               "comment",
                               "exclude_scope"),
                             names(input_data$rings_data))
    saved_flags <- input_data$rings_data |> dplyr::filter(
      image_label == sel_img,
      year == sel_year
    )
    saved_flags[new_flag_cols] <- FALSE
    if ("comment" %in% new_flag_cols){
      saved_flags$comment <- NA_character_
    }
    if ("exclude_scope" %in% new_flag_cols){
      saved_flags$exclude_scope <- NA_character_
    }

    expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

    # NEW: Restore EW/LW/ALL selection
    if (!is.na(saved_flags$exclude_scope)) {
      updateRadioButtons(session, "sel_exclude_scope",
                         selected = saved_flags$exclude_scope)
    } else {
      updateRadioButtons(session, "sel_exclude_scope", selected = "ALL")
    }

    sel_disc_flags <- saved_flags %>%
      dplyr::select(unname(discrete_features))
    sel_disc_flags <- names(sel_disc_flags)[sel_disc_flags[1,] == TRUE]

    updateCheckboxGroupInput(session, "sel_discrete",
                             selected = sel_disc_flags)

    sel_disq_flags <- saved_flags %>%
      dplyr::select(unname(disqual_issues))
    sel_disq_flags <- names(sel_disq_flags)[sel_disq_flags[1,] == TRUE]

    updateCheckboxGroupInput(session, "sel_disqual",
                             selected = sel_disq_flags)
    #shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')

    # if ("technical_issues" %in% sel_disq_flags) {
    #   shinyjs::enable("sel_technical_exact")
      sel_tech_issues <- saved_flags %>%
        dplyr::select(unname(technical_issues))
      sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)
    # } else {
    #   shinyjs::disable("sel_technical_exact")
    #   updateSelectizeInput(session, "sel_technical_exact",
    #                        selected = character(0))
    # }

      sel_other_issues <- saved_flags %>%
        dplyr::select(unname(other_issues))
      sel_other_issues <- names(sel_other_issues)[sel_other_issues[1,] == TRUE]
      updateSelectizeInput(session, "sel_other_iss",
                           selected = sel_other_issues)

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(input$reset_to_raw, ignoreNULL = TRUE, ignoreInit = TRUE)


  # save edits, update plot
  observe({
    req(isTruthy(rings_data_edited()))
    rings_data_org(rings_data_edited())
  }) |> bindEvent(input$apply_changes, ignoreNULL = TRUE, ignoreInit = TRUE)


  # OPEN IMAGE -----------------------------------------------------------------
  # Track the last opened image to avoid opening duplicate windows
  last_opened_image <- reactiveVal(NULL)

  # Function to determine which image to open and open it
  open_ring_image <- function(df) {
    req(df)

    # Get image path from metadata
    image_path <- input_data$rxsmeta_data %>%
      dplyr::filter(image_label == df$image_label) %>%
      dplyr::pull(fname_image)

    base_path <- dirname(image_path)

    # Check for annotated twin image
    annotated_image <- list.files(
      base_path,
      pattern = paste0(df$image_label, "_annotated\\."), # paste0(df$image_label, "_annotated_twin\\."),
      full.names = TRUE
    )

    # Prefer annotated image if exists
    file_to_open <- if (length(annotated_image) == 1) annotated_image else image_path

    # Only open if different from last opened
    if (!identical(file_to_open, last_opened_image())) {
      if (file.exists(file_to_open)) {
        browseURL(file_to_open)
        last_opened_image(file_to_open)
      } else {
        showNotification(
          glue::glue("The following image could not be opened: {file_to_open}"),
          type = "error"
        )
      }
    }
  }

  # Manual "Open Image" button — works regardless of checkbox
  observeEvent(input$show_image, {
    req(isTruthy(clicked_ring()), isTruthy(input_data$rxsmeta_data))
    open_ring_image(clicked_ring()$data)
  })

  # Auto-open when a ring is clicked (only if checkbox is selected)
  observe({
    req(isTruthy(clicked_ring()), isTruthy(input_data$rxsmeta_data))
    req(input$auto_open_image)  # only if checkbox is checked
    open_ring_image(clicked_ring()$data)
  }) |> bindEvent(clicked_ring(), ignoreNULL = TRUE, ignoreInit = TRUE)

  # # Track the last opened image to avoid opening duplicate windows
  # last_opened_image <- reactiveVal(NULL)
  #
  # # Cross-platform function to open image in a separate graphics window
  # open_image_external <- function(image_path, width = 10, height = 8) {
  #   if (!file.exists(image_path)) {
  #     showNotification(glue::glue("The following image could not be opened: {image_path}"), type = "error")
  #     return()
  #   }
  #
  #   # Open OS-specific graphics device
  #   if (.Platform$OS.type == "windows") {
  #     windows(width = width, height = height)
  #   } else if (Sys.info()["sysname"] == "Darwin") {
  #     quartz(width = width, height = height)
  #   } else {
  #     X11(width = width, height = height)
  #   }
  #
  #   # Load image
  #   ext <- tools::file_ext(image_path)
  #   if (tolower(ext) %in% c("png")) {
  #     img <- png::readPNG(image_path)
  #   } else if (tolower(ext) %in% c("jpg", "jpeg")) {
  #     img <- jpeg::readJPEG(image_path)
  #   } else {
  #     showNotification("Unsupported image format", type = "error")
  #     dev.off()
  #     return()
  #   }
  #
  #   # Display image
  #   grid::grid.raster(img)
  # }
  #
  # # Function to determine which image to open and open it
  # open_ring_image <- function(df) {
  #   req(df)
  #
  #   # Get image path from metadata
  #   image_path <- input_data$rxsmeta_data %>%
  #     dplyr::filter(image_label == df$image_label) %>%
  #     dplyr::pull(fname_image)
  #
  #   base_path <- dirname(image_path)
  #
  #   # Check for annotated twin image
  #   annotated_image <- list.files(
  #     base_path,
  #     pattern = paste0(df$image_label, "_annotated_twin\\."),
  #     full.names = TRUE
  #   )
  #
  #   # Prefer annotated image if exists
  #   file_to_open <- if (length(annotated_image) == 1) annotated_image else image_path
  #
  #   # Only open if different from last opened
  #   if (!identical(file_to_open, last_opened_image())) {
  #     open_image_external(file_to_open)
  #     last_opened_image(file_to_open)
  #   }
  # }
  #
  # # Manual "Open Image" button — works regardless of checkbox
  # observeEvent(input$show_image, {
  #   req(isTruthy(clicked_ring()), isTruthy(input_data$rxsmeta_data))
  #   open_ring_image(clicked_ring()$data)
  # })
  #
  # # Auto-open when a ring is clicked (only if checkbox is selected)
  # observe({
  #   req(isTruthy(clicked_ring()), isTruthy(input_data$rxsmeta_data))
  #   req(input$auto_open_image)  # only if checkbox is checked
  #   open_ring_image(clicked_ring()$data)
  # }) |> bindEvent(clicked_ring(), ignoreNULL = TRUE, ignoreInit = TRUE)




  # COVERAGE -------------------------------------------------------------------












  # SAVE RESULTS TO FILE -------------------------------------------------------
  output$save_flags <- downloadHandler(
    filename = function() {
      glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_DATASETNAME_rings_edited.csv")
    },
    content = function(con) {
      readr::write_csv(rings_data_edited(),
                       con)
    }
  )






  output$debug <- renderPrint({
    trace_visibility()
    #input$select_dupl
    # # For debugging: show traces_crn structure
  #   #purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "exclring"))
    # print(sample.int(1e6, 1))
    # if (isTruthy(input$traces_crn)){
    # traces_df <- purrr::map_dfr(names(input$traces_crn), function(name) {
    #   item <- input$traces_crn[[name]]
    #
    #   # Start with basic columns
    #   result <- list(
    #     name = name,
    #     curveNumber = item$curveNumber,
    #     opacity = item$opacity,
    #     visible = item$visible
    #   )
    #
    #   # Add all meta elements if they exist
    #   if (!is.null(item$meta)) {
    #     meta_flat <- unlist(item$meta)
    #     # Add meta_ prefix to distinguish from main columns
    #     names(meta_flat) <- paste0("meta_", names(meta_flat))
    #     result <- c(result, as.list(meta_flat))
    #   }
    #
    #   # Convert to tibble
    #   tibble::as_tibble(result)
    # })
    # tail(traces_df)
    # } else{
    #   NULL
    # }



  })

}


