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

  # REACTIVE DATA FOR PLOT -----------------------------------------------------
  # filtered data based on selected inputs
  crn_data <- reactive({
    req(isTruthy(input_data$prf_data),
        isTruthy(input_data$rings_data),
        isTruthy(input_data$rxsmeta_data))
    flag_cols <- c("incomplete_ring", "missing_ring",
                   "duplicate_ring", "exclude_dupl", "exclude_issues")

    sel_param <- input$sel_param

    df_crn <- input_data$rings_data |>
      dplyr::select(woodpiece_label, slide_label, image_label, year,
                    dplyr::any_of(flag_cols),
                    dplyr::any_of(sel_param))

    if (sel_param %in% names(input_data$prf_data)){
      df_crn  <- input_data$prf_data |>
        dplyr::filter(sector_n == as.numeric(input$sel_sector)) |>
        dplyr::select(dplyr::all_of(c("image_label","year",sel_param))) |>
        dplyr::right_join(df_crn, by = c("image_label","year"))
    }

    df_crn |>
      dplyr::arrange(woodpiece_label, year)
  })



  # PLOTLY ---------------------------------------------------------------------
  # JavaScript to capture trace opacities on restyle events
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
      need(isTruthy(input_data$prf_data), "Please upload QWA profile data"),
      need(isTruthy(input_data$rings_data), "Please upload QWA rings data"),
      need(input$sel_param, "Please select a QWA parameter to display"),
      need(input$sel_sector, "Please select a ring sector to display")
    )

    sel_param <- input$sel_param
    show_excl <- input$show_excl
    df_plot <- crn_data() |> dplyr::filter(!exclude_dupl)

    if (!show_excl){
      df_plot <- df_plot |> dplyr::filter(!exclude_issues)
    }

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

  # Capture click events on plotly
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
        new_op <- if (current_op == 1) 0.15 else 1
        #has_marker <- (!is.null(input$traces_crn)) && (length(input$traces_crn) > n_traces)
        marker_id <- if (!is.null(crn_click_data()$curveNumber)) crn_click_data()$curveNumber else -1

        # if we dim a trace with the current marker, remove the marker as well
        if ((new_op == 0.15) &&
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

    if (current_op == 1 && any(other_opacities > 0.15)) {
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
            opacity = 0.15
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

      # Get the curve name from traces_crn
      curr_traces <- input$traces_crn
      curve_name <- if (!is.null(curr_traces)) {
        names(curr_traces)[trace_id + 1]
      } else {
        unique(crn_data()$woodpiece_label)[trace_id + 1]
      }

      # Create marker name
      marker_name <- paste0(curve_name, ".", click_data$x)

      # restyle the plot
      plotly::plotlyProxy("ts_crn_plot", session) %>%
        plotly::plotlyProxyInvoke("deleteTraces", n_traces) %>%  # Remove last trace if it's a marker
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = list(click_data$x),
            y = list(click_data$y),
            name = marker_name,
            mode = "markers",
            marker = list(
              size = 10,
              color = "red",
              symbol = "circle"
            ),
            showlegend = FALSE,
            hoverinfo = "skip"
          )) %>%
        plotly::plotlyProxyInvoke(
          method = "restyle",
          list(
            opacity = 1
          ),
          trace_id
        )
    }
  })


  # DT flag selection ----------------------------------------------------------
  df_flags <- reactive({
    req(input$traces_crn)
    req(isTruthy(input_data$rings_data))
    n_traces <- length(unique(input_data$rings_data$woodpiece_label))
    if (length(input$traces_crn) != n_traces + 1){
      return(NULL)
    }
    marker_name <- names(input$traces_crn)[n_traces + 1]  # force reevaluation on marker trace change
    sel_wp_label <- sub("\\.[^.]*$", "", marker_name)
    sel_year <- as.integer(sub(".*\\.", "", marker_name))

    input_data$rings_data |>
      dplyr::filter(woodpiece_label == sel_wp_label, year == sel_year)
    # |>
    #   dplyr::select(woodpiece_label, slide_label, image_label, year,
    #                 incomplete_ring, missing_ring, duplicate_ring,
    #                 exclude_dupl, exclude_issues)
  })


  observeEvent(df_flags(), {
    shinyjs::toggle(id = "sel_ring_card", condition = !is.null(df_flags()))
  }, ignoreNULL = FALSE)


  output$sel_ring <- renderUI({
    req(df_flags())
    df <- df_flags() |> dplyr::filter(!exclude_dupl)
    paste("Selected ring:", df$image_label, "| Year:", df$year)
  })

  # Update flag edit card when a new ring is selected
  observeEvent(df_flags(), {
    # output$image_ui <- renderUI({
    #   NULL
    # })
    bslib::accordion_panel_close("ring_details","Show image")
    bslib::accordion_panel_close("ring_details","Show woodpiece coverage")

    #req(rings_data_out())
    saved_flags <- rings_data_out() |>
      dplyr::inner_join(df_flags() |>
                         dplyr::select(woodpiece_label, image_label, year),
                       by = c("woodpiece_label", "image_label", "year")) |>
      dplyr::filter(!exclude_dupl) |>
      dplyr::select(exclude_issues, unname(discrete_features),
                    unname(disqual_issues), unname(technical_issues),
                    comment)

    updateRadioButtons(session, "sel_exclude",
                       selected = ifelse(saved_flags$exclude_issues, "yes", "no"))

    sel_disc_flags <- saved_flags %>%
      dplyr::select(unname(discrete_features)) |>
      dplyr::slice(1) |>
      unlist(use.names = FALSE)
    sel_disc_flags <- discrete_features[sel_disc_flags]

    updateCheckboxGroupInput(session, "sel_discrete",
                             selected = sel_disc_flags)

    sel_disq_flags <- saved_flags %>%
      dplyr::select(unname(disqual_issues)) |>
      dplyr::slice(1) |>
      unlist(use.names = FALSE)
    sel_disq_flags <- disqual_issues[sel_disq_flags]

    updateCheckboxGroupInput(session, "sel_disqual",
                             selected = sel_disq_flags)


    # update technical issues
    # update comment

  })

  rings_data_out <- reactive({
    req(isTruthy(input_data$rings_data))
    new_flag_cols <- setdiff(c(unname(discrete_features),
                               unname(disqual_issues), unname(technical_issues),
                               "comment"),
                             names(input_data$rings_data))
    df_rings <- input_data$rings_data
    df_rings[new_flag_cols] <- FALSE
    if ("comment" %in% new_flag_cols){
      df_rings$comment <- ""
    }

    df_rings
  })


  observeEvent(input$sel_disqual, {
    if (isTruthy(input$sel_disqual) && "technical_issues" %in% input$sel_disqual) {
      shinyjs::show("sel_technical_exact")
    } else {
      shinyjs::hide("sel_technical_exact")
      updateSelectizeInput(session, "sel_technical_exact", selected = character(0))
    }
   }, ignoreNULL = FALSE)

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



  output$image_ui <- renderUI({
    df <- df_flags() |> dplyr::filter(!exclude_dupl)

    image_path <- input_data$rxsmeta_data |>
      dplyr::filter(image_label == df$image_label) |> dplyr::pull(fname_image)

    base_path <- dirname(image_path)
    addResourcePath("images", base_path)
    # find the file which matches image_label and contains "annotated"
    annotated_image <- list.files(base_path, pattern = paste0(df$image_label, "_annotated"), full.names = FALSE)
    #print(annotated_image)


    if (length(annotated_image) == 1) {
      image_tags <- tagList(
        #tags$b(paste("Image for ring:", df$image_label, "| Year:", df$year)),
        tags$div(
          style = "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; overflow: auto;",
          tags$img(src = paste0("images/", annotated_image),
                   style = "transform: rotate(-90deg); width: 300px; height: auto;")
        )

      )
    } else {
      image_tags <- tagList(
          #tags$b(paste("Image for ring:", df$image_label, "| Year:", df$year)),
          tags$p("Annotated image not found.")
        )
    }

      image_tags
  })





  # TODO:
  # - maybe easier to have a card with checkbox / radio inputs for the flags for each ring?
  # - can still have measurements displayed? plus additional cards if there are duplicates?

  # - update data with selected flags
  # - save results
  # - show image
  # - show coverage overview



  output$debug <- renderPrint({
    rings_data_out() |> head(5)
    #crn_data()
    # wp_label, image_label, year, param, exclude_issues, exclude_dupl
    # consecutive years, with NAs
    # filter out excluded_dupl
    # filter out excluded_issues if box unchecked


    #names(input$traces_crn)
    #df_flags()
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


