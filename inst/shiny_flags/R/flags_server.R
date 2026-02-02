flags_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # LOAD INPUT DATA ----------------------------------------------------------
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
              actionButton(ns("confirm_overwrite"), "Proceed")
            )
          )
        )
      } else {
        show_input_source_modal(ns)
      }
    }) |> bindEvent(input$open_input_modal)

    # if overwrite confirmed, also show input source modal
    observe({
      removeModal()
      show_input_source_modal(ns)
    }) |> bindEvent(input$confirm_overwrite)

    # ui to provide input data based on selection (within input source modal)
    output$load_details_ui <- renderUI({
      req(input$load_type)
      if (input$load_type == "env") {
        tagList(
          "Provide the names of the data.frames in the current R environment:",
          textInput(ns("name_prf"), "Profile data", value = "prf_data"),
          textInput(ns("name_rings"), "QWA rings data", value = "QWA_data$rings"),
          textInput(ns("name_rxsmeta"), "ROXAS (image) metadata", value = "df_rxsmeta")
        )
      } else if (input$load_type == "csv") {
        tagList(
          "Provide the filepaths for the following:",
          fileInput(ns("file_prf"), "Upload the QWA profile data", accept = c(".csv")),
          fileInput(ns("file_rings"), "Upload the QWA rings data", accept = c(".csv")),
          fileInput(ns("file_rxsmeta"), "Upload the ROXAS metadata", accept = c(".csv"))
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
      updateSelectInput(session, "filt_species", choices = species_choices,
                        selected = species_choices[1])
      site_choices <- unique(rxsmeta_data$site_label)
      filt_sites <- rxsmeta_data |>
        dplyr::filter(species_code == species_choices[1]) |>
        dplyr::pull(site_label) |> unique()
      updateSelectInput(session, "filt_site", choices = site_choices,
                        selected = filt_sites)

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
      req(input$filt_site, input$filt_species)
      rxsmeta_data <- input_data$rxsmeta_data |> dplyr::filter(
        site_label %in% input$filt_site,
        species_code %in% input$filt_species
      )
      wp_choices <- unique(rxsmeta_data$woodpiece_label)
      updateSelectInput(session, "filt_wp", choices = wp_choices,
                        selected = wp_choices)
    }) |> bindEvent(input$filt_site, input$filt_species, ignoreInit = TRUE)

    # enable/disable sector selection based on selected parameter
    observe({
      sel_param <- input$sel_param
      if (sel_param %in% names(input_data$prf_data)){
        shinyjs::enable("sel_sector")
      } else {
        shinyjs::disable("sel_sector")
      }
    }) |> bindEvent(input$sel_param, ignoreInit = TRUE)


    # REACTIVE CONTAINER: EDITED FLAGS DATA ------------------------------------
    # NOTE: rings_data_org is the basis for the plot, rings_data_edited tracks
    # edits of the flag inputs for selected rings. edits are then propagated back
    # to rings_data_org when the save / update btn is clicked
    rings_data_org <- reactiveVal(NULL)
    rings_data_edited <- reactiveVal(NULL)
    sel_woodpiece <- reactiveVal(NULL)

    # initialize rings_data_out with input data
    observe({
      df_rings <- input_data$rings_data
      # initialize new flag columns if not present
      new_flag_cols <- setdiff(c(unname(discrete_features),
                                 unname(disqual_issues),
                                 unname(technical_issues),
                                 unname(other_issues)),
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

      # order by woodpiece, image and year
      df_rings <- df_rings |>
        dplyr::group_by(woodpiece_label) |>
        dplyr::mutate(min_year_wp = min(year)) |>
        dplyr::group_by(woodpiece_label, image_label) |>
        dplyr::mutate(min_year_img = min(year)) |>
        dplyr::ungroup() |>
        dplyr::arrange(min_year_wp, woodpiece_label, min_year_img, image_label, year) |>
        dplyr::select(-min_year_wp, -min_year_img)

      rings_data_org(df_rings)
    }) |> bindEvent(input_data$rings_data)

    # initialize editable copy of rings_data_out
    observe({
      rings_data_edited(rings_data_org())
      sel_woodpiece(unique(rings_data_org()$woodpiece_label)[1])
    }) |> bindEvent(rings_data_org())


    # MEASUREMENTS PLOT --------------------------------------------------------
    color_palette <- reactive({
      req(input_data$rings_data)
      wp_labels_all <- sort(unique(input_data$rings_data$woodpiece_label))
      pal <- extend_palette(base_palette, length(wp_labels_all),
                            contrasting = TRUE)
      pal <- setNames(pal, wp_labels_all)
      pal
    })

    # reactive container to hold all the plot dataframes
    plot_data <- reactiveValues()

    observe({
      req(rings_data_org(), input$filt_wp, input$sel_param, sel_woodpiece())
      sel_param <- input$sel_param
      df_crn <- rings_data_org() |>
        dplyr::filter(woodpiece_label %in% input$filt_wp) |>
        #dplyr::filter(!exclude_dupl) |> # always exclude duplicate rings from plot
        dplyr::select(woodpiece_label, slide_label, image_label, year, exclude_dupl,
                      exclude_issues, dplyr::any_of(sel_param))

      # if sel_param is from prf data, then join (selected sector only)
      if (sel_param %in% names(input_data$prf_data)){
        df_crn  <- df_crn |>
          dplyr::left_join(
            input_data$prf_data |>
              dplyr::filter(sector_n == as.numeric(input$sel_sector)) |>
              dplyr::select(dplyr::all_of(c("image_label", "year", sel_param))),
            by = c("image_label", "year")
          )
      }

      # new column with clean values only
      df_crn$vals <- df_crn[[sel_param]]
      if (!input$show_excl){
        # 'remove' excluded years by setting vals to NA
        df_crn$vals[df_crn$exclude_issues] <- NA
      }
      # TODO: df_crn <- df_crn |> dplyr::select(-exclude_issues, -slide_label)

      # if (input$spline_det){
      # TODO
      # transform the plot data to df where
      # pivot wider to have (woodpiece_label, sel_param) as columns
      # year to row names
      # apply dplr::detrend(df, method = "Spline", nyrs = 32), check output
      # potentially reapply NAS
      # pivot longer back to original format
      # }

      # order chronologically within each woodpiece (Without reordering the woodpieces)
      df_crn <- df_crn |>
        dplyr::mutate(woodpiece_label = factor(woodpiece_label,
                                               levels = unique(woodpiece_label))) |>
        dplyr::group_by(woodpiece_label) |>
        dplyr::arrange(year, .by_group = TRUE) |>
        dplyr::ungroup() |>
        dplyr::mutate(woodpiece_label = as.character(woodpiece_label))

      plot_data$df_other <- df_crn |> dplyr::filter(woodpiece_label != sel_woodpiece())
      plot_data$df_selwp <- df_crn |> dplyr::filter(woodpiece_label == sel_woodpiece())
      plot_data$df_sd <- df_crn |>
        dplyr::filter(!exclude_dupl) |>
        dplyr::select(year,vals) |>
        dplyr::group_by(year) |>
        dplyr::summarise(n_obs = sum(!is.na(vals))) |>
        dplyr::arrange(year)

      plot_data$df_cov <- rings_data_org() |>
        dplyr::filter(woodpiece_label == sel_woodpiece()) |>
        dplyr::mutate(image_label = factor(image_label, levels = unique(image_label)))

      #last_click_id(NULL)

      # update sel_marker?


    }) |> bindEvent(rings_data_org(), input$filt_wp, input$sel_param, input$sel_sector,
                    input$show_excl, sel_woodpiece(), ignoreInit = TRUE)

    js_traces <- readLines("www/traces_to_input.js") |> paste(collapse = "\n")


    output$main_plot <- plotly::renderPlotly({
      validate(need(isTruthy(plot_data$df_selwp), "Please provide input data"))
      sel_param <- input$sel_param
      validate(need(any(!is.na(plot_data$df_selwp$vals)),
                    "No data to display for the selected parameter and filters."))
      # TODO: fix for one woodpiece?

      cat("=== PLOT RENDERING ===\n")
      cat("Time:", Sys.time(), "\n")

      df_selwp <- plot_data$df_selwp |>
        dplyr::filter(!exclude_dupl)
      selwp <- sel_woodpiece()

      df_other <- plot_data$df_other |>
        dplyr::filter(!exclude_dupl)

      palette <- color_palette()
      color_selwp <- palette[[selwp]]

      p <- plotly::plot_ly(
        data = df_other,
        x = ~year,
        y = ~vals,
        color = ~woodpiece_label,
        colors = palette,
        type = 'scatter',
        mode = 'lines',
        opacity = 0.3,
        name = ~woodpiece_label,
        source = "crn_plot", # set source ID
        showlegend = TRUE,
        meta = list(role = "otherwp")
      )

      p <- p %>%
        plotly::add_trace(
          data = df_selwp,
          x = ~year,
          y = ~vals,
          type = 'scatter',
          mode = 'lines',
          line = list(color = color_selwp, width = 2),
          opacity = 1,
          name = paste0("<b>", selwp, "</b>"),
          showlegend = TRUE,
          meta = list(role = "selwp", wp_label = selwp) # trace info for selected wp line
        )

      p <- p %>%
        plotly::layout(
          legend = list(
            orientation = 'h',
            yanchor = 'bottom', y = 1.1,
            xanchor = 'center', x = 0.5
            # itemclick = FALSE,      # Disable single click on legend (custom handling)
            # itemdoubleclick = FALSE # Disable double click on legend (custom handling)
          )
        )

      # create sample depth subplot
      p2 <- plotly::plot_ly(data = plot_data$df_sd,
                            x = ~year,
                            y = ~n_obs,
                            type = "scatter",
                            mode = "lines+markers",
                            line = list(shape = "hvh", color = "darkgrey"),
                            marker = list(size = 2, color = "#00206E"),
                            name = "sample depth",
                            showlegend = FALSE,
                            source = "crn_plot",
                            meta = list(role = "sampledepth"))

      p3 <- plotly::plot_ly(
        data = plot_data$df_cov,
        x = ~year,
        y = ~image_label,
        color = ~image_label,
        colors = "grey",
        type = 'scatter',
        mode = 'line',
        name = ~paste0("covl.", image_label),
        hoverinfo = "skip",
        showlegend = FALSE,
        source = "crn_plot",
        meta = list(role = "covlines", wp_label = selwp)
      ) %>%
        plotly::add_trace(
          x = ~year,
          y = ~image_label,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size =10,
            color = ~cno,
            colorscale = "Viridis",
            symbol = "square"
          ),
          name = ~image_label,
          hoverinfo = 'text',
          text = ~paste0(image_label, "<br>",
                         year, ": ", cno, " cells"),
          showlegend = FALSE,
          meta = list(role = "covpoints", wp_label = selwp)
        )
      # %>%
      #   plotly::layout(shapes = list(list(type = "line",x0 = 0,
      #                                     x1 = 1,
      #                                     xref = "paper",
      #                                     y0 = sel_img,
      #                                     y1 = sel_img,
      #                                     line = list(color = 'red', width = 3),
      #                                     layer = "below")))


      fig <- plotly::subplot(p, p2, p3,
                             nrows = 3,
                             shareX = TRUE,
                             heights = c(0.5, 0.1, 0.4)) %>%
        plotly::layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = sel_param),
          yaxis2 = list(title = "N samples"))

      fig <- fig %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_relayout") %>%
        plotly::config(
        modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                      'hoverClosestCartesian',
                                      'hoverCompareCartesian')) %>%
        # first onRender: add synchronized hover on sample depth plot
        htmlwidgets::onRender("
          function(el, x) {
            el.on('plotly_hover', function(d) {
              var point = d.points[0];
              // check if the hovered trace is wp 'orgline'
              var traceData = el.data[point.curveNumber];
              const wproles = ['selwp', 'otherwp'];
              if (traceData.meta && wproles.includes(traceData.meta.role)) {
                // find the corresponding point on the sample depth trace
                var sdCurveNumber = el.data.findIndex(trace => trace.meta && trace.meta.role === 'sampledepth');
                var xValue = point.x;
                var xData = el.data[sdCurveNumber].x;
                var dsPtNum = xData.indexOf(xValue);
                if (dsPtNum !== -1) {
                  // re-do the hover on both subplots with the identified points
                  Plotly.Fx.hover(el.id, [
                    { curveNumber:point.curveNumber, pointNumber:point.pointNumber },
                    { curveNumber:sdCurveNumber, pointNumber:dsPtNum }
                  ], ['xy','xy2']);
                }
              }
            });
          }
      ") %>%
      # second onRender: capture shown traces as Shiny input
      htmlwidgets::onRender(js_traces, data = ns("traces_crn"))

    }) |> bindEvent(plot_data$df_selwp, plot_data$df_other)


    # restore plot state after re-render -----------------------------------------
    awaiting_restoration <- reactiveVal(FALSE)

    # if plot data changes, set flag to restore state after render
    observe({
      awaiting_restoration(TRUE)
    }) |> bindEvent(plot_data$df_selwp, plot_data$df_other,
                    ignoreNULL = TRUE,
                    ignoreInit = TRUE)

    # restore state of plot
    observe({
      req(awaiting_restoration())
      req(input$traces_crn)

      cat("... restoring state\n")

      awaiting_restoration(FALSE) # only do it once

      current_traces <- input$traces_crn
      isolate({
        if (isTruthy(sel_marker())){
          cat("... redrawing sel ring markers\n")
          marker <- sel_marker()
          if (marker$wp_label %in% plot_data$df_selwp$woodpiece_label){
            # ensure we have up to date y val if param was changed
            ywp_val <- plot_data$df_selwp |>
              dplyr::filter(year == marker$year, image_label == marker$ycov_val) |>
              dplyr::pull(dplyr::any_of(input$sel_param))

            sel_marker(
              list(
                year = marker$year,
                wp_label = marker$wp_label,
                ywp_val = ywp_val,
                ycov_val = marker$ycov_val
              )
            )

            p <- plotly::plotlyProxy("main_plot", session)
            if (!"sel_marker_wp" %in% names(current_traces)){
              p <- p %>%
                plotly::plotlyProxyInvoke(
                  "addTraces",
                  list(
                    x = list(marker$year),
                    y = list(ywp_val),
                    type = "scatter",
                    mode = "markers",
                    name = "sel_marker_wp",
                    marker = list(
                      size = 10,
                      color = "red",
                      symbol = "circle"
                    ),
                    showlegend = FALSE,
                    hoverinfo = "skip",
                    meta = list(role = "sel_marker_wp")
                  )
                ) %>%
                plotly::plotlyProxyInvoke(
                  "addTraces",
                  list(
                    x = list(marker$year),
                    y = list(marker$ycov_val),
                    type = "scatter",
                    mode = "markers",
                    name = "sel_marker_cov",
                    marker = list(
                      size = 10,
                      color = "red",
                      symbol = "circle"
                    ),
                    showlegend = FALSE,
                    hoverinfo = "skip",
                    meta = list(role = "sel_marker_cov"),
                    yaxis = "y3"
                  )
                )
            } else {
              wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
              cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

              p <- p %>%
                plotly::plotlyProxyInvoke(
                  "restyle",
                  list(
                    x = list(list(marker$year)),
                    y = list(list(ywp_val))
                  ),
                  wp_curveNumber
                ) %>%
                plotly::plotlyProxyInvoke(
                  "restyle",
                  list(
                    x = list(list(marker$year)),
                    y = list(list(marker$ycov_val))
                  ),
                  cov_curveNumber
                )
            }
            p
          }
          else {
           cat("MARKER COULD NOT BE DRAWN?")
          }
        }

        cat("... rerender complete\n")

      })

    }) |> bindEvent(input$traces_crn, ignoreNULL = TRUE, ignoreInit = TRUE)


    # PLOT CLICKS --------------------------------------------------------------
    # capture click events on the selected woodpice trace or on a coverage plot point
    plot_click <- reactive({
      req(plot_data$df_selwp)
      event <- plotly::event_data("plotly_click", source = "crn_plot", priority = "event")
      req(event)

      current_traces <- isolate(input$traces_crn)
      clicked_trace <- purrr::keep(current_traces, ~.x$curveNumber == event$curveNumber)
      clicked_role <- clicked_trace[[1]]$meta$role
      if (clicked_role %in% c("selwp", "covlines","covpoints")){
        event$role <- clicked_role
        event$wp_label <- clicked_trace[[1]]$meta$wp_label
        event
      } else if (clicked_role == "otherwp"){
        event$role <- "otherwp"
        event$wp_label <- names(clicked_trace)
        event
      } else {
        NULL
      }
    })

    sel_marker <- reactiveVal(NULL)

    observe({
      req(!awaiting_restoration()) # avoid running if plot is rerendered
      cat(".   plot click\n")
      click_data <- plot_click()

      if (click_data$role == "otherwp"){
        showModal(
          modalDialog(
            title = "Warning",
            glue::glue("You have clicked on a different woodpiece trace: {click_data$wp_label}.
            Do you want to switch to editing this woodpiece?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_wp"), "Proceed")
            )
          )
        )
      } else {
        # update the sel markers in plot
        year_val <- click_data$x
        wp_label <- click_data$wp_label
        if (click_data$role == "selwp"){
          ywp_val <- click_data$y
          ycov_val <- rings_data_edited() |>
            dplyr::filter(woodpiece_label == wp_label, year == year_val, !exclude_dupl) |>
            dplyr::pull(image_label)
        } else {
          ycov_val <- click_data$y
          ywp_val <- plot_data$df_selwp |>
            dplyr::filter(image_label == ycov_val, year == year_val) |>
            dplyr::pull(dplyr::any_of(input$sel_param))
        }
        # TODO: marker colors:
        # if exclude_dupl then different color / shape?
        # if exclude_issues, and or NA value, different color / shape?
        # check in rings_data_edited to keep up to date?

        # update reactive
        marker <- list(
          year = year_val,
          wp_label = wp_label,
          ywp_val = ywp_val,
          ycov_val = ycov_val
        )
        sel_marker(
          marker
        )

        # update plot markers
        current_traces <- input$traces_crn
        p <- plotly::plotlyProxy("main_plot", session)
          if (!"sel_marker_wp" %in% names(current_traces)){
            p <- p %>%
              plotly::plotlyProxyInvoke(
                "addTraces",
                list(
                  x = list(marker$year),
                  y = list(marker$ywp_val),
                  type = "scatter",
                  mode = "markers",
                  name = "sel_marker_wp",
                  marker = list(
                    size = 10,
                    color = "red",
                    symbol = "circle"
                  ),
                  showlegend = FALSE,
                  hoverinfo = "skip",
                  meta = list(role = "sel_marker_wp")
                )
              ) %>%
              plotly::plotlyProxyInvoke(
                "addTraces",
                list(
                  x = list(marker$year),
                  y = list(marker$ycov_val),
                  type = "scatter",
                  mode = "markers",
                  name = "sel_marker_cov",
                  marker = list(
                    size = 10,
                    color = "red",
                    symbol = "circle"
                  ),
                  showlegend = FALSE,
                  hoverinfo = "skip",
                  meta = list(role = "sel_marker_cov"),
                  yaxis = "y3"
                )
              )
          } else {
            wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
            cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

            p <- p %>%
              plotly::plotlyProxyInvoke(
                "restyle",
                list(
                  x = list(list(marker$year)),
                  y = list(list(marker$ywp_val))
                ),
                wp_curveNumber
              ) %>%
              plotly::plotlyProxyInvoke(
                "restyle",
                list(
                  x = list(list(marker$year)),
                  y = list(list(marker$ycov_val))
                ),
                cov_curveNumber
              )
          }
          p
        }
    }) |> bindEvent(plot_click(), ignoreNULL = TRUE, ignoreInit = FALSE)

    observe({
      removeModal()
      #req(!awaiting_restoration()) # avoid running if plot is rerendered
      click_data <- plot_click()
      year_val <- click_data$x
      wp_label <- click_data$wp_label
      ywp_val <- click_data$y
      ycov_val <- plot_data$df_other |>
        dplyr::filter(woodpiece_label == wp_label, year == year_val, !exclude_dupl) |>
        dplyr::pull(image_label)

      sel_woodpiece(wp_label)
      sel_marker(
        list(
          year = year_val,
          wp_label = wp_label,
          ywp_val = ywp_val,
          ycov_val = ycov_val
        )
      )
    }) |> bindEvent(input$confirm_wp)

    shift_ring <- reactiveVal(NULL)

    observe({
      shift_ring(list(
        dir = "next",
        nonce = sample(1e6, 1)
      ))
    }) |> bindEvent(input$next_ring)

    observe({
      shift_ring(list(
        dir = "prev",
        nonce = sample(1e6, 1)
      ))
    }) |> bindEvent(input$prev_ring)

    observe({
      req(sel_marker())
      current_year <- sel_marker()$year
      direction <- shift_ring()$dir

      adj_years <- plot_data$df_selwp |>
        dplyr::filter(!exclude_dupl) |>
        dplyr::filter(
          if (direction == "prev") year < current_year else year > current_year
        ) |>
        dplyr::select(image_label, year, dplyr::all_of(input$sel_param)) |>
        dplyr::arrange(if (direction == "prev") dplyr::desc(year) else year)

      if (nrow(adj_years) == 0){
        showNotification(sprintf(
          "No %s year available for this woodpiece in current plot.",
          if (direction == "prev") "earlier" else "later"),
          type = "warning")
        return(NULL)
      }

      marker <- list(
        year = adj_years$year[1],
        wp_label = sel_woodpiece(),
        ywp_val = adj_years[[input$sel_param]][1],
        ycov_val = adj_years$image_label[1]
      )
      sel_marker(
        marker
      )

      # redraw:
      # update plot markers
      current_traces <- input$traces_crn
      p <- plotly::plotlyProxy("main_plot", session)
      if (!"sel_marker_wp" %in% names(current_traces)){
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = list(marker$year),
              y = list(marker$ywp_val),
              type = "scatter",
              mode = "markers",
              name = "sel_marker_wp",
              marker = list(
                size = 10,
                color = "red",
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "sel_marker_wp")
            )
          ) %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = list(marker$year),
              y = list(marker$ycov_val),
              type = "scatter",
              mode = "markers",
              name = "sel_marker_cov",
              marker = list(
                size = 10,
                color = "red",
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "sel_marker_cov"),
              yaxis = "y3"
            )
          )
      } else {
        wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
        cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

        p <- p %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(list(marker$year)),
              y = list(list(marker$ywp_val))
            ),
            wp_curveNumber
          ) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(list(marker$year)),
              y = list(list(marker$ycov_val))
            ),
            cov_curveNumber
          )
      }
      p


    }) |> bindEvent(shift_ring(), ignoreNULL = TRUE, ignoreInit = TRUE)






    # DEBUG OUTPUT -------------------------------------------------------------
    output$debug <- renderPrint({

    plot_click()







      # df_rings <- rings_data_edited() |> dplyr::filter(woodpiece_label == input$sel_wp)
      # df_hm <- df_rings |>
      #   dplyr::select(image_label, year,cno) |>
      #   tidyr::pivot_wider(names_from = year, values_from = cno)
      # df_hm <- df_hm |>
      #   dplyr::select(dplyr::all_of(sort(colnames(df_hm)))) |>
      #   tibble::column_to_rownames("image_label")
      #
      # plotly::plot_ly(z = as.matrix(df_hm), x = colnames(df_hm), y = rownames(df_hm), type = 'heatmap')
      #
      # p <- ggplot2::ggplot(df_rings, ggplot2::aes(x = year, y = image_label, fill = cno)) +
      #   ggplot2::geom_tile() +
      #   ggplot2::scale_fill_viridis_c(direction = -1) +
      #   # ggplot2::geom_point(
      #   #   data = df_rings |> dplyr::filter(image_label == clicked_ring()$data$image_label,
      #   #                                    year == clicked_ring()$year),
      #   #   color = "red", fill = NA,   size = 5
      #   # ) +
      #   ggplot2::theme_minimal()
      #   # ggplot2::theme(axis.text=ggplot2::element_text(size=12),
      #   #                axis.title=ggplot2::element_blank()) +
      #   #ggplot2::coord_fixed()
      # plotly::ggplotly(p)
    })



  })
}
