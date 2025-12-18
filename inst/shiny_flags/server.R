server <- function(input, output, session) {

  # reactive container for input data
  input_data <- reactiveValues(
    prf_data = NULL,
    rings_data = NULL,
    rxsmeta_data = NULL
  )

  # LOAD INPUT DATA ------------------------------------------------------------
  observeEvent(input$btn_input, {
    # warn before overwriting existing data
    all_null <- all(
      is.null(input_data$prf_data),
      is.null(input_data$rings_data),
      is.null(input_data$rxsmeta_data))
    if (!all_null) {
      showModal(
        modalDialog(
          title = "Warning",
          "This action overwrites any existing inputs provided in the app,
           including any unsaved edits.
           Are you sure you want to proceed?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("conf_inp_overwrite", "Proceed")
          )
        )
      )
    } else {
      # input source modal
      showModal(
        modalDialog(
          title = "Select input source",
          tagList(
            radioButtons(
              "load_type",
              "Choose input option:",
              choices = c(
                "Load data from R environment" = "env",
                "Load data from csv files" = "csv",
                "Load example data (for demonstration only)" = "example")
            ),
            hr(),
            uiOutput("load_details_ui")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_input", "Proceed")
          )
        )
      )
    }
  })

  # ui to provide input data based on selected option
  output$load_details_ui <- renderUI({
    req(input$load_type)
    if (input$load_type == "env") {
      tagList(
        "Provide the names of the data.frames in the current R environment:",
        textInput("prf_name", "Profile data", value = "prf_data"),
        textInput("rings_name", "QWA rings data", value = "QWA_data$rings"),
        textInput("rxsmeta_name", "ROXAS (image) metadata", value = "df_rxsmeta")
      )
    } else if (input$load_type == "csv") {
      tagList(
        "Provide the filepaths for the follwoing:",
        fileInput("file_prf", "Upload the QWA profile data", accept = c(".csv")),
        fileInput("file_rings", "Upload the QWA rings data", accept = c(".csv")),
        fileInput("file_rxsmeta", "Upload the ROXAS metadata", accept = c(".csv"))
      )
    } else { # example
      "not yet available..."
    }
  })

  # if overwrite confirmed, also show input source modal
  observeEvent(input$conf_inp_overwrite, {
    removeModal()
    # input source modal
    showModal(
      modalDialog(
        title = "Select input source",
        tagList(
          radioButtons(
            "load_type",
            "Choose input option:",
            choices = c(
              "Load data from R environment" = "env",
              "Load data from csv files" = "csv",
              "Load example data (for demonstration only)" = "example")
          ),
          hr(),
          uiOutput("load_details_ui")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_input", "Proceed")
        )
      )
    )
  })


  observeEvent(input$confirm_input, {
    if (input$load_type == "env") {
      # load from R environment
      tryCatch({
        prf_data_in <- get(input$prf_name, envir = .GlobalEnv)
        rings_name <- input$rings_name
        # Handle tmp$a format
        if (grepl("\\$", rings_name)) {
          parts <- strsplit(rings_name, "$", fixed = TRUE)[[1]]
          obj <- get(parts[1], envir = .GlobalEnv)
          rings_data_in <- obj[[parts[2]]]
        } else if (grepl("\\[\\[", rings_name)) {
        # Handle tmp[['a']] or tmp[["a"]] format
          pattern <- "^([^\\[]+)\\[\\[(['\"])(.+?)\\2\\]\\]$"
          obj_name <- sub(pattern, "\\1", rings_name)
          element_name <- sub(pattern, "\\3", rings_name)
          rings_data_in <- get(obj_name, envir = .GlobalEnv)[[element_name]]
        } else {
          rings_data_in <- get(rings_name, envir = .GlobalEnv)
        }
        rxsmeta_data_in <- get(input$rxsmeta_name, envir = .GlobalEnv)
        # TODO: expand to use validate_df and align_to_structure, safe_block from shiny_meta?
        checkmate::assert_subset(c("image_label","year","sector_n"), names(prf_data_in))
        checkmate::assert_data_frame(prf_data_in[c("image_label","year","sector_n")], min.rows = 1, any.missing = FALSE)
        checkmate::assert_character(names(prf_data_in), min.len = 4)
        # check_vals <- checkmate::test_subset(df_prf |> dplyr::select(-dplyr::all_of(c("image_label","year","sector_n"))) |> names(),
        #                                     c("la_mean", ...))
        checkmate::assert_subset(c("woodpiece_label", "slide_label", "image_label","year",
                                   "incomplete_ring", "missing_ring","duplicate_ring",
                                   "exclude_dupl","exclude_issues"), names(rings_data_in))
        checkmate::assert_data_frame(rings_data_in[c("woodpiece_label", "slide_label", "image_label","year")],
                                     min.rows = 1, any.missing = FALSE)
        #checkmate::assert_character(names(df_prf), min.length = 10) # at least on measurements col?
        checkmate::assert_subset(c("image_label", "slide_label", "woodpiece_label",
                                   "tree_label", "species_code", "site_label", "fname_image"),
                                 names(rxsmeta_data_in))
        input_data$prf_data <- prf_data_in
        input_data$rings_data <- rings_data_in
        input_data$rxsmeta_data <- rxsmeta_data_in
        removeModal()
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error loading data from environment",
          paste("There was an error reading the provided input data.frames:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })

    } else if (input$load_type == "csv") {
      # load from csv files
      tryCatch({
        prf_data_in <- vroom::vroom(
          input$file_prf$datapath,
          col_types = c(.default = "d",
                        image_label = "c", year = "i", sector_n = "i")
          )
        # TODO: expand to use validate_df and align_to_structure, safe_block from shiny_meta?
        checkmate::assert_subset(c("image_label","year","sector_n"), names(prf_data_in))
        checkmate::assert_data_frame(prf_data_in[c("image_label","year","sector_n")], min.rows = 1, any.missing = FALSE)
        checkmate::assert_character(names(prf_data_in), min.len = 4)
        # check_vals <- checkmate::test_subset(df_prf |> dplyr::select(-dplyr::all_of(c("image_label","year","sector_n"))) |> names(),
        #                                     c("la_mean", ...))
        rings_data_in <- vroom::vroom(
          input$file_rings$datapath,
          col_types = c(.default = "d",
                        woodpiece_label = "c", slide_label = "c", image_label = "c",
                        year = "i", incomplete_ring = "l", missing_ring = "l",
                        duplicate_ring = "l", exclude_dupl = "l", exclude_issues = "l")
        )
        checkmate::assert_subset(c("woodpiece_label", "slide_label", "image_label","year",
                                   "incomplete_ring", "missing_ring","duplicate_ring",
                                   "exclude_dupl","exclude_issues"), names(rings_data_in))
        checkmate::assert_data_frame(rings_data_in[c("woodpiece_label", "slide_label", "image_label","year")],
                                     min.rows = 1, any.missing = FALSE)
        #checkmate::assert_character(names(df_prf), min.length = 10) # at least on measurements col?
        rxsmeta_data_in <- vroom::vroom(
          input$file_rxsmeta$datapath,
          col_types = c(.default = "c")
        )
        checkmate::assert_subset(c("image_label", "slide_label", "woodpiece_label",
                                   "tree_label", "species_code", "site_label", "fname_image"),
                                 names(rxsmeta_data_in))
        input_data$prf_data <- prf_data_in
        input_data$rings_data <- rings_data_in
        input_data$rxsmeta_data <- rxsmeta_data_in
        removeModal()
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error loading data from csv files",
          paste("There was an error reading the provided input files:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    } else {
      # load example data
      NULL
      removeModal()
    }
    # TODO: check consistency btw files
  })


  # update filtering / selection UI based on input data
  observe({
    req(isTruthy(input_data$prf_data),
        isTruthy(input_data$rings_data),
        isTruthy(input_data$rxsmeta_data))
    prf_data <- input_data$prf_data
    rings_data <- input_data$rings_data
    rxsmeta_data <- input_data$rxsmeta_data

    site_choices <- unique(rxsmeta_data$site_label)
    updateSelectInput(session, "sel_site", choices = site_choices,
                      selected = site_choices)
    species_choices <- unique(rxsmeta_data$species_code)
    updateSelectInput(session, "sel_species", choices = species_choices,
                      selected = species_choices[1])

    param_choices_prf <- setdiff(names(prf_data), c("image_label", "year", "sector_n"))
    param_choices_ring <- setdiff(names(rings_data),
                                      c("woodpiece_label", "slide_label", "image_label",
                                        "year", "incomplete_ring", "missing_ring",
                                        "duplicate_ring", "exclude_dupl", "exclude_issues"))
    updateSelectInput(session, "sel_param", choices = list(
      "Ring level" = param_choices_ring,
      "Agg. cell level" = param_choices_prf
    ), selected = param_choices_prf[1])

    sector_choices <- sort(unique(prf_data$sector_n))
    updateSelectInput(session, "sel_sector", choices = sector_choices,
                      selected = sector_choices[1])
  })

  observe({
    req(isTruthy(input_data$rxsmeta_data))
    rxsmeta_data <- input_data$rxsmeta_data |> dplyr::filter(
      site_label %in% input$sel_site,
      species_code %in% input$sel_species
    )
    wp_choices <- unique(rxsmeta_data$woodpiece_label)
    updateSelectInput(session, "sel_wp", choices = wp_choices,
                      selected = wp_choices)
  }) |> bindEvent(list(input$sel_site, input$sel_species),
                  ignoreNULL = TRUE, ignoreInit = TRUE)

  # enable/disable sector selection based on selected parameter
  observe({
    req(isTruthy(input_data$prf_data))
    sel_param <- input$sel_param
    if (sel_param %in% names(input_data$prf_data)){
      shinyjs::enable("sel_sector")
    } else {
      shinyjs::disable("sel_sector")
    }
  })


  # REACTIVE CONTAINER: EDITED FLAGS DATA --------------------------------------
  rings_data_org <- reactiveVal(NULL)
  rings_data_edited <- reactiveVal(NULL)


  # initialize rings_data_out with input_data$rings_data
  observe({
    # initialize new flag columns if not present
    new_flag_cols <- setdiff(c(unname(discrete_features),
                               unname(disqual_issues), unname(technical_issues),
                               "comment"),
                             names(input_data$rings_data))
    df_rings <- input_data$rings_data
    df_rings[new_flag_cols] <- FALSE
    if ("comment" %in% new_flag_cols){
      df_rings$comment <- NA_character_
    }

    rings_data_org(df_rings)
  }) |> bindEvent(input_data$rings_data, ignoreNULL = TRUE, ignoreInit = TRUE)

  # initalize copy rings_data_edited of rings_data_out
  observe({
    rings_data_edited(rings_data_org())
  }) |> bindEvent(rings_data_org(), ignoreNULL = TRUE, ignoreInit = TRUE)




  # REACTIVE CONTAINER: PLOT DATA ----------------------------------------------
  plot_data <- reactive({
    req(isTruthy(rings_data_org()),
        isTruthy(input$sel_param))

    # start from rings df for structure, exclusions and (if applicable) sel_param
    df_crn <- rings_data_org() |>
      dplyr::select(woodpiece_label, slide_label, image_label, year,
                    exclude_issues, exclude_dupl,
                    dplyr::any_of(input$sel_param))

    # if sel_param is in prf df, then join (selected sector only)
    if (input$sel_param %in% names(input_data$prf_data)){
      df_crn  <- input_data$prf_data |>
        dplyr::filter(sector_n == as.numeric(input$sel_sector)) |>
        dplyr::select(dplyr::all_of(c("image_label","year",input$sel_param))) |>
        dplyr::right_join(df_crn, by = c("image_label","year"))
    }

    # make sure we only have one value per year/woodpiece
    df_crn <- df_crn |> dplyr::filter(!exclude_dupl)

    if (!input$show_excl){
      # 'remove' excluded years by setting param to NA
      df_crn[df_crn$exclude_issues, input$sel_param] <- NA
      #df_crn <- df_crn |> dplyr::filter(!exclude_issues)
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

    # sort to make sure color palette stays consistent (?)
    df_crn <- df_crn |>
      dplyr::arrange(woodpiece_label, year)

    df_crn
  }) |> bindEvent(list(rings_data_org(),
                       #input$sel_wp,
                       input$sel_param,
                       input$sel_sector,
                       input$show_excl #, input$spline_det
                       )
                  )

  # create color palette based on how many traces (wp) we plot
  color_palette <- reactive({
    req(plot_data())
    extend_palette(base_palette,
                   length(unique(plot_data()$woodpiece_label)),
                   contrasting = TRUE)
  })
  # // Function to extract trace information
  # function extractTraceInfo() {
  #   var out = {};
  #   el._fullData.forEach(function(trace, traceindex) {
  #     console.log(trace)
  #     out[trace.name] = {
  #       curveNumber: traceindex,
  #       opacity: trace.opacity !== undefined ? trace.opacity : 1,
  #       visible: trace.visible !== undefined ? trace.visible : true,
  #       meta: trace.meta !== undefined ? trace.meta : null
  #     };
  #   });
  #   return out;
  # }

  # PLOTLY ---------------------------------------------------------------------
  last_restored_render <- reactiveVal(NULL)

  # JavaScript to capture trace opacities on restyle events
  js_traces <- "function(el, x, inputName){

    // Function to extract trace information
    function extractTraceInfo() {
      var out = {};
      // Use both el.data (original) and el._fullData (processed)
      el.data.forEach(function(trace, traceindex) {
        var fullTrace = el._fullData[traceindex];
        console.log('Trace', traceindex, 'name:', (fullTrace.name !== undefined ? fullTrace.name : 'na'), 'data.visible:', trace.visible, 'fullData.visible:', fullTrace.visible);
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
      // Signal that plot has been rendered
      // Shiny. setInputValue(inputName + '_rendered', Math.random());
    });

    // When traces are restyled (legend clicks, opacity changes)
    el.on('plotly_restyle', function(evtData) {
      console.log('Plot restyled');
      updateShinyInput();
    });
  }"


  output$ts_crn_plot <- plotly::renderPlotly({
    cat("=== PLOT RENDERING ===\n")
    cat("Time:", Sys.time(), "\n")

    validate(
      need(isTruthy(plot_data()), "Please provide input data")
    )

    sel_param <- input$sel_param
    validate(need(any(!is.na(plot_data()[sel_param])),
                  "No data to display for the selected parameter and filters."))

    p <- plotly::plot_ly(
      data = plot_data(),
      x = ~year,
      y = ~.data[[sel_param]],
      color = ~woodpiece_label,
      colors = color_palette(),
      type = 'scatter',
      mode = 'lines',
      name = ~woodpiece_label,
      source = "crn_plot",  # Set source ID here,
      meta = list(role = "orgline")
    )
      # # Add empty red markers trace
      # plotly::add_trace(
      #   x = numeric(0),
      #   y = numeric(0),
      #   name = "sel_ring",
      #   type = 'scatter',
      #   mode = "markers",
      #   marker = list(
      #     size = 10,
      #     color = "red",
      #     symbol = "circle"
      #   ),
      #   showlegend = FALSE,
      #   hoverinfo = "skip",
      #   meta = list(role = "selring"),
      #   inherit = FALSE  # Don't inherit data from plot_ly()
      # ) |>
      # Add empty pink markers trace
      # plotly::add_trace(
      #   x = numeric(0),
      #   y = numeric(0),
      #   name = "excl_rings",
      #   type = 'scatter',
      #   mode = "markers",
      #   marker = list(
      #     size = 6,
      #     color = "hotpink",
      #     symbol = "x"
      #   ),
      #   showlegend = FALSE,
      #   hoverinfo = "skip",
      #   meta = list(role = "exclring"),
      #   inherit = FALSE
      # )

    p <- p %>%
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

    if (!is.null(crn_x_axes())){
      x_axes <- crn_x_axes()
      if (!is.null(x_axes$x_min) && !is.null(x_axes$x_max)){
        p <- p %>%
          plotly::layout(
            xaxis = list(range = c(x_axes$x_min, x_axes$x_max))
          )
      }
    }

    # plotly interaction settings
    plotly::config(
      p,
      doubleClickDelay=400,
      modeBarButtonsToRemove = list('hoverClosestCartesian',
                                    'hoverCompareCartesian')) %>%
      # add custom JS to capture shown traces
      htmlwidgets::onRender(js_traces, data = "traces_crn")
  }) |> bindEvent(plot_data())

  # Capture click events on plotly
  crn_click_data <- reactive({
    req(plot_data())
    plotly::event_data("plotly_click",
                       source = "crn_plot", priority = "event") # event: reevaluate on each click, even if same item
  })

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
  }) |> bindEvent(crn_change_axes(), ignoreNULL = TRUE, ignoreInit = TRUE)

  # Keep track of pending legend single clicks
  pending_single_click <- reactiveVal(FALSE)

  trace_opacity <- reactiveVal(list(
      on = character(0),
      off = character(0)
  ))

  # Observe legend single clicks: toggle visibility of that trace
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

  # Observe legend double clicks:
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


  # Observe plot clicks: add marker on selected point and highlight the trace
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
        year = click_data$x
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
    p <- p %>%
      # add new marker trace
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          x = list(click_data$x),
          y = list(click_data$y),
          name = new_marker_name,
          mode = "markers",
          marker = list(
            size = 10,
            color = "red",
            symbol = "circle"
          ),
          showlegend = FALSE,
          hoverinfo = "skip",
          meta = list(role = "selring", orgCurveNumber = trace_id,
                      orgName = trace_name, year = click_data$x)
        )) %>%
      # ensure that the clicked trace is on
      plotly::plotlyProxyInvoke(
        method = "restyle",
        list(
          opacity = op_on
        ),
        trace_id # in first n_wp traces, so does not shift
      )


    # adding pink trace
    excl_markers <- get_new_excluded(
      rings_org = rings_data_org(),
      rings_edit = rings_data_edited(),
      sel_wp = input$sel_wp,
      plt_df = plot_data(),
      param = input$sel_param
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
      p <- plotly::plotlyProxy("ts_crn_plot", session) %>%
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
    excl_markers <- get_new_excluded(
      rings_org = rings_data_org(),
      rings_edit = rings_data_edited(),
      sel_wp = selected_wps,
      plt_df = plot_data(),
      param = input$sel_param
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
      excl_markers <- get_new_excluded(
        rings_org = rings_data_org(),
        rings_edit = rings_data_edited(),
        sel_wp = input$sel_wp,
        plt_df = plot_data(),
        param = input$sel_param
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


  shinyjs::disable(selector = paste0("#sel_desquial .checkbox:nth-child(1) label"))

  # REACTIVE CONTAINER: SELECTED RING ------------------------------------------
  clicked_ring <- reactive({
    sel_marker <- latest_marker()
    if (!is.null(sel_marker)) {
      sel_wp_label <- sel_marker$orgName
      sel_year <- sel_marker$year

      sel_data <- rings_data_edited() |>
        dplyr::filter(woodpiece_label == sel_wp_label, year == sel_year) |>
        dplyr::filter(!exclude_dupl) # TODO: keep all for handling duplicates?

      return(
        list(
          woodpiece_label = sel_wp_label,
          year = sel_year,
          data = sel_data
        )
      )
    } else {
      NULL
    }
  }) %>% bindEvent(latest_marker(), ignoreNULL = FALSE, ignoreInit = TRUE) # input$sel_wp


  expected_excl <- reactiveVal(NULL)

  # update ring edit card when a ring is selected
  observe({
    shinyjs::toggle(id = "ring_editor_card", condition = !is.null(clicked_ring()))

    # if a ring is selected, update the inputs with saved flags and comment
    req(isTruthy(clicked_ring()))


    saved_flags <- clicked_ring()$data
    expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

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
    shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')

    if ("technical_issues" %in% sel_disq_flags) {
      shinyjs::enable("sel_technical_exact")
      sel_tech_issues <- saved_flags %>%
        dplyr::select(unname(technical_issues))
      sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
      updateCheckboxGroupInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)
    }

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(clicked_ring(), ignoreNULL = FALSE, ignoreInit = FALSE)


  # the ring editor card title
  output$sel_ring <- renderUI({
    req(clicked_ring())
    df <- clicked_ring()$data
    paste("Selected ring:", df$image_label, "| Year:", df$year)
  })

  # toggle the technical issue specification checkboxes
  observeEvent(input$sel_disqual, {
    if (isTruthy(input$sel_disqual) && "technical_issues" %in% input$sel_disqual) {
      shinyjs::enable("sel_technical_exact")
    } else {
      shinyjs::disable("sel_technical_exact") #techn_reason_el
      updateSelectizeInput(session, "sel_technical_exact", selected = character(0))
    }
  }, ignoreNULL = FALSE)

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
      discrete = input$sel_discrete,
      disqual = input$sel_disqual,
      technical = input$sel_technical_exact,
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


    df_rings <- rings_data_edited()
    clicked_data <- clicked_ring()$data
    ring_id <- which(df_rings$image_label == clicked_data$image_label &
                          df_rings$year == clicked_data$year)

    df_rings[ring_id, "exclude_issues"] <- excl_flag
    df_rings[ring_id, c(disc_flags_on, disq_flags_on, techn_flags_on)] <- TRUE
    df_rings[ring_id, c(disq_flags_off, disc_flags_off, techn_flags_off)] <- FALSE
    df_rings[ring_id, "comment"] <- flag_changes()$comment

    rings_data_edited(df_rings)

  }) |> bindEvent(flag_changes(), ignoreNULL = FALSE, ignoreInit = TRUE)


  # revert to pre-edit data if "Discard changes" is clicked
  # can use clicked_ring()$data which is only updated when (another) ring is clicked
  observe({
    req(isTruthy(clicked_ring()))

    saved_flags <- clicked_ring()$data
    expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

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
    shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')

    if ("technical_issues" %in% sel_disq_flags) {
      shinyjs::enable("sel_technical_exact")
      sel_tech_issues <- saved_flags %>%
        dplyr::select(unname(technical_issues))
      sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)
    } else {
      shinyjs::disable("sel_technical_exact")
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = character(0))
    }

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(input$undo_sel_flags, ignoreNULL = TRUE, ignoreInit = TRUE)


  # revert to raw input data if "reset_to_raw" is clicked
  observe({
    req(isTruthy(clicked_ring()))

    sel_img <- clicked_ring()$data$image_label
    sel_year <- clicked_ring()$data$year

    # initialize new flag columns if not present
    new_flag_cols <- setdiff(c(unname(discrete_features),
                               unname(disqual_issues), unname(technical_issues),
                               "comment"),
                             names(input_data$rings_data))
    saved_flags <- input_data$rings_data |> dplyr::filter(
      image_label == sel_img,
      year == sel_year
    )
    saved_flags[new_flag_cols] <- FALSE
    if ("comment" %in% new_flag_cols){
      saved_flags$comment <- NA_character_
    }

    expected_excl(ifelse(saved_flags$exclude_issues, "yes", "no"))
    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

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
    shinyjs::runjs('$("#sel_disqual input[type=checkbox]").first().prop("disabled", true);')

    if ("technical_issues" %in% sel_disq_flags) {
      shinyjs::enable("sel_technical_exact")
      sel_tech_issues <- saved_flags %>%
        dplyr::select(unname(technical_issues))
      sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)
    } else {
      shinyjs::disable("sel_technical_exact")
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = character(0))
    }

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(input$reset_to_raw, ignoreNULL = TRUE, ignoreInit = TRUE)


  # save edits, update plot
  observe({
    req(isTruthy(rings_data_edited()))
    rings_data_org(rings_data_edited())
  }) |> bindEvent(input$apply_changes, ignoreNULL = TRUE, ignoreInit = TRUE)


  # OPEN IMAGE -----------------------------------------------------------------
  observe({
    req(isTruthy(clicked_ring()), isTruthy(input_data$rxsmeta_data))
    df <- clicked_ring()$data

    image_path <- input_data$rxsmeta_data |>
      dplyr::filter(image_label == df$image_label) |> dplyr::pull(fname_image)
    print(image_path)
    base_path <- dirname(image_path)
    print(base_path)
    annotated_image <- list.files(base_path, pattern = paste0(df$image_label, "_annotated\\."), full.names = TRUE)
    print(annotated_image)
    if (length(annotated_image) == 1){
      print("tried to open?")
      browseURL(annotated_image)
    } else if (file.exists(image_path)){
      showNotification("Annotated image not found, opening original image instead.", type = "warning")
      browseURL(image_path)
    } else {
      showNotification(glue::glue("The following image could not be opened: {image_path}"), type = "error")
    }
    #   # Alternative: Open image based on OS
    #   if (.Platform$OS.type == "windows") {
    #     system2("cmd", c("/c", "start", shQuote(image_path)), wait = FALSE)
    #   } else if (Sys.info()["sysname"] == "Darwin") {
    #     system(paste("open", shQuote(image_path)))
    #   } else {
    #     system(paste("xdg-open", shQuote(image_path)))
    #   }
  }) |> bindEvent(input$show_image, ignoreNULL = TRUE, ignoreInit = TRUE)




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

  #   #purrr::detect(input$traces_crn, \(x) isTRUE(x$meta$role == "exclring"))
    req(input$traces_crn)
    traces_df <- purrr::map_dfr(names(input$traces_crn), function(name) {
      item <- input$traces_crn[[name]]

      # Start with basic columns
      result <- list(
        name = name,
        curveNumber = item$curveNumber,
        opacity = item$opacity,
        visible = item$visible
      )

      # Add all meta elements if they exist
      if (!is.null(item$meta)) {
        meta_flat <- unlist(item$meta)
        # Add meta_ prefix to distinguish from main columns
        names(meta_flat) <- paste0("meta_", names(meta_flat))
        result <- c(result, as.list(meta_flat))
      }

      # Convert to tibble
      tibble::as_tibble(result)
    })

    tail(traces_df)

  })

}


