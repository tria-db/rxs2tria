server <- function(input, output, session) {

  # reactive container for input data
  input_data <- reactiveValues(
    prf_data = NULL,
    rings_data = NULL,
    rxsmeta_data = NULL
  )

  # LOAD INPUT DATA ----------------------------------------------------------
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
    req(isTruthy(input_data$prf_data), isTruthy(input_data$rings_data))
    prf_data <- input_data$prf_data
    rings_data <- input_data$rings_data
    # wp_choices <- unique(rings_data()$woodpiece_label)
    # updateSelectInput(session, "sel_wp", choices = wp_choices, selected = wp_choices)
    # site, species choices

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


  # (re-) initialize rings_data_out with input_data$rings_data
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


  # PLOTLY ---------------------------------------------------------------------
  # JavaScript to capture trace opacities on restyle events
  js_traces <- "function(el, x, inputName){
    // Function to extract trace information
    function extractTraceInfo() {
      var out = {};
      el._fullData.forEach(function(trace, traceindex) {
        console.log(trace)
        out[trace.name] = {
          curveNumber: traceindex,
          opacity: trace.opacity !== undefined ? trace.opacity : 1,
          meta: trace.meta !== undefined ? trace.meta : null
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
      updateShinyInput();
    });

    // When traces are restyled (legend clicks, opacity changes)
    el.on('plotly_restyle', function(evtData) {
      updateShinyInput();
    });
  }"

  output$ts_crn_plot <- plotly::renderPlotly({
    validate(
      need(isTruthy(input_data$prf_data), "Please upload QWA profile data"),
      need(isTruthy(input_data$rings_data), "Please upload QWA rings data")
    )
    req(plot_data())

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
          itemclick = FALSE,      # Disable single click on legend (custom handling)
          itemdoubleclick = FALSE # Disable double click on legend (custom handling)
        )
      ) %>%
      plotly::event_register("plotly_click") %>%
      plotly::event_register("plotly_legendclick") %>%
      plotly::event_register("plotly_legenddoubleclick")

    # plotly interaction settings
    plotly::config(
      p,
      doubleClickDelay=400,
      modeBarButtonsToRemove = list('hoverClosestCartesian',
                                    'hoverCompareCartesian')) %>%
      # add custom JS to capture shown traces
      htmlwidgets::onRender(js_traces, data = "traces_crn")
  })

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

  # Keep track of pending legend single clicks
  pending_single_click <- reactiveVal(FALSE)

  # Observe legend single clicks: toggle visibility of that trace
  observe({
    # register pending first click
    pending_single_click(TRUE)

    # delay briefly to identify single/double clicks
    # if it's still pending after delay, it was a true single click -> handle click
    # otherwise, do nothing here, see double click handler
    shinyjs::delay(450, {
      if (pending_single_click()){
        pending_single_click(FALSE) # reset pending

        req(isTruthy(input$traces_crn))
        trace_name <- crn_lgnd_click()$name
        current_traces <- input$traces_crn
        trace_id <- current_traces[[trace_name]]$curveNumber
        current_op <- current_traces[[trace_name]]$opacity

        op_on <- 1
        op_off <- 0.15
        new_op <- if (current_op == op_on) op_off else op_on

        # check for marker on this trace
        marker_trace <- purrr::detect(current_traces, \(x) x$meta$role == "selring")
        if (!is.null(marker_trace)){
          marker_on_trace <- (marker_trace$meta$orgName == trace_name)
        } else {
          marker_on_trace <- FALSE
        }

        p <- plotly::plotlyProxy("ts_crn_plot", session)
        if (new_op == op_off && marker_on_trace){
          # remove marker on selected trace (which will be dimmed)
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
    req(isTruthy(input$traces_crn))

    pending_single_click(FALSE)  # cancel the pending single click from first click

    trace_name <- crn_lgnd_dblclick()$name
    current_traces <- input$traces_crn

    current_op <- current_traces[[trace_name]]$opacity
    other_traces <- purrr::keep(current_traces, \(x) x$meta$role == "orgline")
    other_traces <- other_traces[names(other_traces) != trace_name]
    other_ops <- purrr::map_dbl(other_traces, "opacity")
    other_ids <- unname(purrr::map_int(other_traces, "curveNumber"))

    # check for marker on this trace
    marker_trace <- purrr::detect(current_traces, \(x) x$meta$role == "selring")
    if (!is.null(marker_trace)){
      marker_on_trace <- (marker_trace$meta$orgName == trace_name)
    } else {
      marker_on_trace <- FALSE
    }

    op_on <- 1
    op_off <- 0.15
    # isolate trace if it's currently on and not already isolated
    if (current_op == op_on && any(other_ops > op_off)){
      p <- plotly::plotlyProxy("ts_crn_plot", session)
      if (!marker_on_trace) {
        # remove marker on other trace (that will be dimmed)
        p <- p %>%
          plotly::plotlyProxyInvoke("deleteTraces", marker_trace$curveNumber)
      }
      # dim other traces
      p %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 0.15
          ),
          other_ids
        )
    } else {
      # reset all traces to visible, leave marker as is
      plotly::plotlyProxy("ts_crn_plot", session) %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 1
          )
        )
    }
  }) |> bindEvent(crn_lgnd_dblclick(), ignoreNULL = TRUE, ignoreInit = TRUE)

  # Observe plot clicks: add marker on selected point and highlight the trace
  observe({
    req(isTruthy(crn_click_data()), isTruthy(input$traces_crn))

    click_data <- crn_click_data()
    current_traces <- input$traces_crn

    trace_id <- click_data$curveNumber
    trace_name <- names(current_traces)[
      purrr::detect_index(current_traces, \(x) x$curveNumber == trace_id)]
    marker_trace <- purrr::detect(current_traces, \(x) x$meta$role == "selring")

    new_marker_name <- paste0(trace_name, ".", click_data$x)

    # update the plot
    p <- plotly::plotlyProxy("ts_crn_plot", session)
    if (!is.null(marker_trace)){
      p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", marker_trace$curveNumber)
    }
    p %>%
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
      # ensure that the clicked trace is fully visible
      plotly::plotlyProxyInvoke(
        method = "restyle",
        list(
          opacity = 1
        ),
        trace_id
      )
  }) |> bindEvent(crn_click_data(), ignoreNULL = TRUE, ignoreInit = TRUE)


  # adding a mean trace based on input
  observe({
    req(plot_data(), input$traces_crn)

    df_crn <- plot_data()
    sel_prm <- input$sel_param
    sel_mean <- input$mean_type

    current_traces <- input$traces_crn
    crn_trace <- purrr::detect(current_traces, \(x) x$meta$role == "crnline")

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
        dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
        collapse::fgroup_by(year) |>
        collapse::fmean()
    } else if (sel_mean == "tbrm"){
      df_mean <- df_crn |>
        dplyr::filter(!exclude_issues) |>
        dplyr::select(dplyr::all_of(c("year", sel_prm))) |>
        dplyr::group_by(year) |>
        dplyr::summarise("{sel_prm}" := dplR::tbrm(.data[[sel_prm]]))
    }
    p %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          x = df_mean$year,
          y = df_mean[[sel_prm]],
          name = paste("crn.", sel_mean, sep = ""),
          type = 'scatter',
          mode = 'lines',
          line = list(dash = 'dash', width = 1, color = 'black'),
          showlegend = FALSE,
          hoverinfo = "skip",
          meta = list(role = "crnline")
        ))
  }) |> bindEvent(list(plot_data(), input$mean_type))




  # REACTIVE CONTAINER: SELECTED RING ------------------------------------------
  clicked_ring <- reactive({
    traces <- input$traces_crn
    marker_trace <- purrr::detect(input$traces_crn, \(x) x$meta$role == "selring")
    if (!is.null(marker_trace)) {
      sel_wp_label <- marker_trace$meta$orgName
      sel_year <- marker_trace$meta$year

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
  }) %>% bindEvent(input$traces_crn, ignoreNULL = TRUE, ignoreInit = TRUE)

  # update ring edit card when a ring is selected
  observe({
    shinyjs::toggle(id = "ring_editor_card", condition = !is.null(clicked_ring()))

    # if a ring is selected, update the inputs with saved flags and comment
    req(isTruthy(clicked_ring()))
    saved_flags <- clicked_ring()$data
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

    if ("technical_issues" %in% sel_disq_flags) {
      shinyjs::enable("sel_technical_exact")
      sel_tech_issues <- saved_flags %>%
        dplyr::select(unname(technical_issues))
      sel_tech_issues <- names(sel_tech_issues)[sel_tech_issues[1,] == TRUE]
      updateSelectizeInput(session, "sel_technical_exact",
                           selected = sel_tech_issues)
    }

    updateTextAreaInput(session, "sel_comment",
                        value = saved_flags$comment)

  }) |> bindEvent(clicked_ring(), ignoreNULL = FALSE, ignoreInit = TRUE)

  # the ring editor card title
  output$sel_ring <- renderUI({
    req(isTruthy(clicked_ring()))
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
    req(isTruthy(clicked_ring()))
    list(
      exclude = input$sel_exclude,
      discrete = input$sel_discrete,
      disqual = input$sel_disqual,
      technical = input$sel_technical_exact,
      comment = input$sel_comment
    )
  }) %>% debounce(250)

  # update rings_data_edited when flag_changes occur
  observe({
    req(isTruthy(clicked_ring()))

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
    base_path <- dirname(image_path)
    annotated_image <- list.files(base_path, pattern = paste0(df$image_label, "_annotated"), full.names = TRUE)
    if (length(annotated_image) == 1){
      browseURL(annotated_image)
    } else if (file.exists(image_path)){
      showNotification("Annotated image not found, opening original image instead.", type = "warning")
      browseURL(annotated_image)
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



  # TODO:
  # - plus additional cards if there are duplicates?
  # - update data with selected flags
  # - save results
  # - show image
  # - show coverage overview



  output$debug <- renderPrint({
    input$traces_crn
  })

}


