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
      #sel_woodpiece(unique(rings_data_org()$woodpiece_label)[1])
    }) |> bindEvent(rings_data_org())


    # MEASUREMENTS PLOT --------------------------------------------------------
    output$selwp <- renderUI({
      req(rings_data_org())
      # TODO: make it a selectInput to change woodpiece directly from plot card?
      card_title <- strong(glue::glue("Selected woodpiece: "))
      wp_choices_filt <- sort(unique(rings_data_org()$woodpiece_label))
      div(
        card_title,
        selectInput(ns("sel_wp_trace"), NULL,
                    choices = wp_choices_filt,
                    selectize = TRUE, multiple = FALSE,
                    selected = wp_choices_filt[1])
      )
    })

    observe({
      req(input$sel_wp_trace)
      sel_woodpiece(input$sel_wp_trace)
    })

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


    # Modal settings
    sel_subplots <- reactiveVal(c("sd","cov"))

    observe({
      showModal(
        modalDialog(
          title = "Plot settings",
          checkboxGroupInput(
            ns("sel_plots"),
            label = "Show / hide optional subplots",
            choices = c(
              "Sample depth plot" = "sd",
              "Woodpiece coverage plot" = "cov"
            ), selected = sel_subplots()
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_plots"), "Proceed")
          )
        )
      )
    }) |> bindEvent(input$plot_settings)

    observe({
      sel_subplots(input$sel_plots)
      removeModal()

    }) |> bindEvent(input$confirm_plots)

    # variable height for the plot
    output$main_plot_ui <- renderUI({
      min_height = "350px"
      if ("sd" %in% sel_subplots()) {
        min_height <- "400px"
      }
      if ("cov" %in% sel_subplots()) {
        min_height <- "550px"
      }
      if ("sd" %in% sel_subplots() && "cov" %in% sel_subplots()){
        min_height <- "600px"
      }
      max_height <- "700px"
      div(
        style = paste0("min-height: ", min_height, "; max-height: ", max_height),
        plotly::plotlyOutput(ns("main_plot"), height = "100%")
      )
    })

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
      color_selwp <- palette[[selwp]] # or a fixed color?

      p <- plotly::plot_ly(
        data = df_other,
        x = ~year,
        y = ~vals,
        color = ~woodpiece_label,
        colors = palette,
        type = 'scatter',
        mode = 'lines',
        opacity = 0.2,
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

      plot_list <- list(p)
      nrows <- 1
      heights <- c(0.5)

      if ("sd" %in% sel_subplots()){
        # create sample depth subplot
        p2 <- plotly::plot_ly(data = plot_data$df_sd,
                              x = ~year,
                              y = ~n_obs,
                              type = "scatter",
                              mode = "lines+markers",
                              line = list(shape = "hvh", color = "#00206E"),
                              marker = list(size = 3, color = "#00206E"),
                              name = "sample depth",
                              showlegend = FALSE,
                              source = "crn_plot",
                              meta = list(role = "sampledepth"))
        plot_list <- c(plot_list, list(p2))
        nrows <- nrows + 1
        heights <- c(heights, 0.1)
      }

      if ("cov" %in% sel_subplots()){
          p3 <- plotly::plot_ly(
          data = plot_data$df_cov,
          x = ~year,
          y = ~image_label,
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(
                        size =10,
                        color = ~cno,
                        colorscale = list(c(0, "#FED976"), c(1, "#006268")),
                        symbol = "square"
                      ),
          line = list(color = "darkgrey"),
          name = ~image_label,
          hoverinfo = 'text',
          text = ~paste0(image_label, "<br>",
                         year, ": ", cno, " cells"),
          showlegend = FALSE,
          source = "crn_plot",
          meta = list(role = "covlines", wp_label = selwp)
        )
        # %>%
        #   plotly::layout(shapes = list(list(type = "line",x0 = 0,
        #                                     x1 = 1,
        #                                     xref = "paper",
        #                                     y0 = sel_img,
        #                                     y1 = sel_img,
        #                                     line = list(color = 'red', width = 3),
        #                                     layer = "below")))

        plot_list <- c(plot_list, list(p3))
        nrows <- nrows + 1
        heights <- c(heights, 0.4)
      }
      heights <- heights / sum(heights) # normalize heights to sum to 1

      fig <- plotly::subplot(plot_list,
                             nrows = nrows,
                             shareX = TRUE,
                             heights = heights) %>%
        plotly::layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = sel_param),
          #yaxis2 = list(title = "N"),
          hovermode = "closest")

      fig <- fig %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_relayout") %>%
        plotly::config(
        modeBarButtonsToRemove = list('select2d', 'lasso2d',
                                      'hoverClosestCartesian',
                                      'hoverCompareCartesian'))

      if ("sd" %in% sel_subplots()){
        fig <- fig %>%
          # add synchronized hover on sample depth plot
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
      ")
      }
      fig <- fig %>%
        # capture shown traces as Shiny input
        htmlwidgets::onRender(js_traces, data = ns("traces_crn"))

    }) |> bindEvent(plot_data$df_selwp, plot_data$df_other, sel_subplots(), ignoreInit = TRUE)


    # restore plot state after re-render ---------------------------------------
    awaiting_restoration <- reactiveVal(FALSE)

    # if plot data changes, set flag to restore state after render
    observe({
      awaiting_restoration(TRUE)
    }) |> bindEvent(plot_data$df_selwp, plot_data$df_other, sel_subplots(),
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

        p <- plotly::plotlyProxy("main_plot", session)

        # if we have axes limits from previous render, reset them
        # TODO: maybe also y if param not changed?
        if (!is.null(crn_x_axes())){
          x_axes <- crn_x_axes()
          if (!is.null(x_axes$x_min) && !is.null(x_axes$x_max)){
            cat("... restoring x axes limits\n")
            p <- p %>%
              plotly::plotlyProxyInvoke(
                method = "relayout",
                list(
                  "xaxis.range[0]" = x_axes$x_min,
                  "xaxis.range[1]" = x_axes$x_max
                )
              )
          }
        }

        cat(".   mean crn update\n")


        if (input$show_mean){
          cat("... redrawing mean trace\n")

          df_crn <- plot_data$df_other |>
            dplyr::bind_rows(plot_data$df_selwp) |>
            dplyr::filter(!exclude_dupl)

          sel_mean <- input$sel_mean
          df_mean <- calc_mean_vals(df_crn, sel_mean)
          p <- p |> draw_mean_trace(df_mean$year, df_mean$vals, sel_mean)
        }


        if (isTruthy(sel_marker())){
          cat("... redrawing sel ring markers\n")
          marker <- sel_marker()
          yaxis_cov <- if ("sd" %in% sel_subplots()) "y3" else "y2"

          if (marker$wp_label %in% plot_data$df_selwp$woodpiece_label){
            # ensure we have up to date y val if param was changed
            ywp_val <- plot_data$df_selwp |>
              dplyr::filter(year == marker$year, image_label == marker$ycov_val) |>
              dplyr::pull(dplyr::any_of(input$sel_param))

            df_excl <- rings_data_edited() |> dplyr::filter(
              image_label == marker$ycov_val, year ==  marker$year) |>
              dplyr::select(exclude_issues, exclude_dupl)
            not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
            marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

            sel_marker(
              list(
                year = marker$year,
                wp_label = marker$wp_label,
                ywp_val = ywp_val,
                ycov_val = marker$ycov_val,
                marker_col = marker_col
              )
            )

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
                      color = marker_col,
                      symbol = "circle"
                    ),
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = list(paste0(marker$ycov_val, "<br>",
                                       marker$year, ": ", ywp_val)),
                    meta = list(role = "sel_marker_wp")
                  )
                )
               if ("cov" %in% sel_subplots()) {
                 p <- p %>%
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
                        color = marker_col,
                        symbol = "circle"
                      ),
                      showlegend = FALSE,
                      hoverinfo = "skip",
                      meta = list(role = "sel_marker_cov"),
                      yaxis = yaxis_cov
                    )
                  )
               }
            } else {
              wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
              cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

              p <- p %>%
                plotly::plotlyProxyInvoke(
                  "restyle",
                  list(
                    x = list(list(marker$year)),
                    y = list(list(ywp_val)),
                    marker = list(
                      size = 10,
                      color = marker_col,
                      symbol = "circle"
                    )
                  ),
                  wp_curveNumber
                )
              if ("cov" %in% sel_subplots()) {
                p <- p %>%
                  plotly::plotlyProxyInvoke(
                    "restyle",
                    list(
                      x = list(list(marker$year)),
                      y = list(list(marker$ycov_val)),
                      marker = list(
                        size = 10,
                        color = marker_col,
                        symbol = "circle"
                      )
                    ),
                    cov_curveNumber
                  )
              }
            }

            cat("... redrawing exclusion markers\n") # TODO: don't need selected year, only wp?
            # TODO: also update if: sel woodpiece changes, plot rerenders
            df_rings <- rings_data_edited() |>
              dplyr::filter(woodpiece_label == sel_marker()$wp_label) |>
              dplyr::filter(!exclude_dupl, exclude_issues) |>
              dplyr::select(image_label, year)

            excl_markers <- plot_data$df_selwp |>
              dplyr::inner_join(df_rings, by = c("image_label", "year")) |>
              dplyr::select(year, vals)
            names(excl_markers) <- c("year", "ywp_val")

            if (!"excl_markers" %in% names(current_traces)){
              p <- p %>%
                plotly::plotlyProxyInvoke(
                  "addTraces",
                  list(
                    x = excl_markers$year,
                    y = excl_markers$ywp_val,
                    type = "scatter",
                    mode = "markers",
                    name = "excl_markers",
                    marker = list(
                      size = 7,
                      color = "blue",
                      symbol = "circle-open"
                    ),
                    showlegend = FALSE,
                    hoverinfo = "skip",
                    meta = list(role = "excl_markers")
                  )
                )
            } else {
              excl_curveNumber <- current_traces[['excl_markers']]$curveNumber

              p <- p %>%
                plotly::plotlyProxyInvoke(
                  "restyle",
                  list(
                    x = list(excl_markers$year),
                    y = list(excl_markers$ywp_val),
                    marker = list(
                      size = 7,
                      color = "blue",
                      symbol = "circle-open"
                    )
                  ),
                  excl_curveNumber
                )
            }


          }

        }

        p
        cat("... rerender complete\n")

      })

    }) |> bindEvent(input$traces_crn, ignoreNULL = TRUE, ignoreInit = TRUE)


    # PLOT CLICKS --------------------------------------------------------------
    # capture click events on the selected woodpice trace or on a coverage plot point
    plot_click <- reactive({
      req(plot_data$df_selwp)
      event <- plotly::event_data("plotly_click", source = "crn_plot", priority = "event")
      req(nrow(event) == 1) # filter out double clicks somehow capturing two points?

      current_traces <- isolate(input$traces_crn)
      clicked_trace <- purrr::keep(current_traces, ~.x$curveNumber == event$curveNumber)
      clicked_role <- clicked_trace[[1]]$meta$role
      if (clicked_role %in% c("selwp", "covlines")){
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
        df_excl <- rings_data_edited() |> dplyr::filter(
          image_label == ycov_val, year == year_val) |>
          dplyr::select(exclude_issues, exclude_dupl)
        not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
        marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

        # update reactive
        marker <- list(
          year = year_val,
          wp_label = wp_label,
          ywp_val = ywp_val,
          ycov_val = ycov_val,
          marker_col = marker_col
        )
        sel_marker(
          marker
        )

        yaxis_cov <- if ("sd" %in% sel_subplots()) "y3" else "y2"
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
                    color = marker_col,
                    symbol = "circle"
                  ),
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = list(paste0(marker$ycov_val, "<br>",
                                     marker$year, ": ", ywp_val)),
                  meta = list(role = "sel_marker_wp")
                )
              )
            if ("cov" %in% sel_subplots()) {
              p <- p %>%
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
                    color = marker_col,
                    symbol = "circle"
                  ),
                  showlegend = FALSE,
                  hoverinfo = "skip",
                  meta = list(role = "sel_marker_cov"),
                  yaxis = yaxis_cov
                )
              )
            }
          } else {
            wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
            cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

            p <- p %>%
              plotly::plotlyProxyInvoke(
                "restyle",
                list(
                  x = list(list(marker$year)),
                  y = list(list(marker$ywp_val)),
                  marker = list(
                    size = 10,
                    color = marker_col,
                    symbol = "circle"
                  )
                ),
                wp_curveNumber
              )
            if ("cov" %in% sel_subplots()) {
              p <- p %>%
              plotly::plotlyProxyInvoke(
                "restyle",
                list(
                  x = list(list(marker$year)),
                  y = list(list(marker$ycov_val)),
                  marker = list(
                    size = 10,
                    color = marker_col,
                    symbol = "circle"
                  )
                ),
                cov_curveNumber
              )
            }
          }
          p
        }
    }) |> bindEvent(plot_click(), ignoreNULL = TRUE, ignoreInit = FALSE)

    observe({
      removeModal()
      click_data <- plot_click()
      year_val <- click_data$x
      wp_label <- click_data$wp_label
      ywp_val <- click_data$y
      ycov_val <- plot_data$df_other |>
        dplyr::filter(woodpiece_label == wp_label, year == year_val, !exclude_dupl) |>
        dplyr::pull(image_label)
      df_excl <- rings_data_edited() |> dplyr::filter(
        image_label == ycov_val, year == year_val) |>
        dplyr::select(exclude_issues, exclude_dupl)
      not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
      marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

      updateSelectInput(session, "sel_wp_trace", selected = wp_label)
      #sel_woodpiece(wp_label)
      sel_marker(
        list(
          year = year_val,
          wp_label = wp_label,
          ywp_val = ywp_val,
          ycov_val = ycov_val,
          marker_col = marker_col
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

      year_val <- adj_years$year[1]
      ywp_val <- adj_years[[input$sel_param]][1]
      ycov_val <- adj_years$image_label[1]

      df_excl <- rings_data_edited() |> dplyr::filter(
        image_label == ycov_val, year == year_val) |>
        dplyr::select(exclude_issues, exclude_dupl)
      not_in_plot <- df_excl$exclude_dupl || df_excl$exclude_issues || is.na(ywp_val)
      marker_col <- ifelse(not_in_plot, "#ff0099", "#e60000")

      marker <- list(
        year = year_val,
        wp_label = sel_woodpiece(),
        ywp_val = ywp_val,
        ycov_val = ycov_val,
        marker_col = marker_col
      )
      sel_marker(
        marker
      )

      # redraw:
      yaxis_cov <- if ("sd" %in% sel_subplots()) "y3" else "y2"
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
                color = marker_col,
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = 'text',
              text = list(paste0(marker$ycov_val, "<br>",
                                 marker$year, ": ", ywp_val)),
              meta = list(role = "sel_marker_wp")
            )
          )
        if ("cov" %in% sel_subplots()) {
          p <- p %>%
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
                color = marker_col,
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "sel_marker_cov"),
              yaxis = yaxis_cov
            )
          )
        }
      } else {
        wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
        cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

        p <- p %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(list(marker$year)),
              y = list(list(marker$ywp_val)),
              marker = list(
                size = 10,
                color = marker_col,
                symbol = "circle"
              )
            ),
            wp_curveNumber
          )
        if ("cov" %in% sel_subplots()) {
          p <- p %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(list(marker$year)),
              y = list(list(marker$ycov_val)),
              marker = list(
                size = 10,
                color = marker_col,
                symbol = "circle"
              )
            ),
            cov_curveNumber
          )
        }
      }

      p


    }) |> bindEvent(shift_ring(), ignoreNULL = TRUE, ignoreInit = TRUE)

    observe({
      # require single selected?
      req(sel_marker())
      old_marker <- sel_marker()
      new_year <- as.integer(rownames(flags_out())[selected_row()])
      if (old_marker$year == new_year){
        return(NULL) # no change
      }
      ywp_val <- plot_data$df_selwp |>
        dplyr::filter(year == new_year, image_label == old_marker$ycov_val) |>
        dplyr::pull(dplyr::any_of(input$sel_param))

      marker_col <- ifelse(flags_out()$exclude_dupl[selected_row()] ||
                             flags_out()$exclude_issues[selected_row()] ||
                             is.na(ywp_val), "#ff0099", "#e60000")
      marker <- list(
        year = new_year,
        wp_label = old_marker$wp_label,
        ywp_val = ywp_val,
        ycov_val = old_marker$ycov_val, # TODO: get new cov val based on year?
        marker_col = marker_col
      )
      sel_marker(marker)
      shinyjs::runjs(sprintf("
        var hot = HTMLWidgets.find('#%s');
        if (hot && hot.hot) {
          hot.hot.render();  // Just re-render cells with new highlighting
        }
      ", ns("img_flags")))

      # redraw:
      yaxis_cov <- if ("sd" %in% sel_subplots()) "y3" else "y2"
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
                color = marker_col,
                symbol = "circle"
              ),
              showlegend = FALSE,
              hoverinfo = 'text',
              text = list(paste0(marker$ycov_val, "<br>",
                                 marker$year, ": ", ywp_val)),
              meta = list(role = "sel_marker_wp")
            )
          )
        if ("cov" %in% sel_subplots()) {
          p <- p %>%
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
                  color = marker_col,
                  symbol = "circle"
                ),
                showlegend = FALSE,
                hoverinfo = "skip",
                meta = list(role = "sel_marker_cov"),
                yaxis = yaxis_cov
              )
            )
        }
      } else {
        wp_curveNumber <- current_traces[['sel_marker_wp']]$curveNumber
        cov_curveNumber <- current_traces[['sel_marker_cov']]$curveNumber

        p <- p %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(list(marker$year)),
              y = list(list(marker$ywp_val)),
              marker = list(
                size = 10,
                color = marker_col,
                symbol = "circle"
              )
            ),
            wp_curveNumber
          )
        if ("cov" %in% sel_subplots()) {
          p <- p %>%
            plotly::plotlyProxyInvoke(
              "restyle",
              list(
                x = list(list(marker$year)),
                y = list(list(marker$ycov_val)),
                marker = list(
                  size = 10,
                  color = marker_col,
                  symbol = "circle"
                )
              ),
              cov_curveNumber
            )
        }
      }

      p

    }) |> bindEvent(selected_row(), ignoreNULL = TRUE)


    # AXIS CHANGES -------------------------------------------------------------
    # capture axes limit changes and keep track of them
    crn_change_axes <- reactive({
      req(plot_data$df_selwp)
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


    # EXCLUDED RINGS -----------------------------------------------------------
    observe({
      # TODO: also update if: sel woodpiece changes, plot rerenders
      df_rings <- rings_data_edited() |>
        dplyr::filter(woodpiece_label == sel_marker()$wp_label) |>
        dplyr::filter(!exclude_dupl, exclude_issues) |>
        dplyr::select(image_label, year)

      excl_markers <- plot_data$df_selwp |>
        dplyr::inner_join(df_rings, by = c("image_label", "year")) |>
        dplyr::select(year, vals)
      names(excl_markers) <- c("year", "ywp_val")

      current_traces <- input$traces_crn
      p <- plotly::plotlyProxy("main_plot", session)
      if (!"excl_markers" %in% names(current_traces)){
        p <- p %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = excl_markers$year,
              y = excl_markers$ywp_val,
              type = "scatter",
              mode = "markers",
              name = "excl_markers",
              marker = list(
                size = 7,
                color = "blue",
                symbol = "circle-open"
              ),
              showlegend = FALSE,
              hoverinfo = "skip",
              meta = list(role = "excl_markers")
            )
          )
      } else {
        excl_curveNumber <- current_traces[['excl_markers']]$curveNumber

        p <- p %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(
              x = list(excl_markers$year),
              y = list(excl_markers$ywp_val),
              marker = list(
                size = 7,
                color = "blue",
                symbol = "circle-open"
              )
            ),
            excl_curveNumber
          )
      }
      p

    }) |> bindEvent(excl_flags())

    # MEAN TRACE ---------------------------------------------------------------
    observe({
      req(plot_data$df_other, input$traces_crn)
      cat(".   mean crn update\n")

      current_traces <- input$traces_crn
      crn_trace <- purrr::detect(current_traces, \(x) isTRUE(x$meta$role == "crnline"))

      p <- plotly::plotlyProxy("main_plot", session)
      if (!is.null(crn_trace)){
        # remove existing mean trace
        p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", crn_trace$curveNumber)
      }

      if (input$show_mean){
        df_crn <- plot_data$df_other |>
          dplyr::bind_rows(plot_data$df_selwp) |>
          dplyr::filter(!exclude_dupl)

        sel_mean <- input$sel_mean
        df_mean <- calc_mean_vals(df_crn, sel_mean)
        p <- p |> draw_mean_trace(df_mean$year, df_mean$vals, sel_mean)
      }

      p
    }) |> bindEvent(input$show_mean, input$sel_mean)


    # FLAGS EDITING ------------------------------------------------------------
    output$selimg <- renderUI({
      req(sel_image())
      sel_img <- sel_image()
      df_img <- input_data$rxsmeta_data
      card_title <- strong(glue::glue("Selected image: {sel_img}"))
      if ("comment" %in% names(df_img)){
        img_comment <- df_img |>
          dplyr::filter(image_label == sel_img) |> dplyr::pull(comment)
        if (!is.na(img_comment) && img_comment != ""){
          card_title <- tagList(
            card_title,
            em(glue::glue("Comment: {img_comment}"))
          )
        }
      }
      card_title

    })

    sel_issues_stored <- reactiveVal(c(disqual_issues, technical_issues, other_issues))
    sel_features_stored <- reactiveVal(discrete_features)

    observe({
      showModal(
        modalDialog(
          title = "Table settings",
          checkboxInput(ns("auto_open_image"), "Try to auto-open images", value = FALSE),
          strong("Show / hide optional columns:"),
          checkboxGroupInput(
            ns("sel_cols_issues"),
            label = "Compromising issues",
            choices = c(
              disqual_issues,
              technical_issues,
              other_issues
            ), selected = sel_issues_stored()
          ),
          checkboxGroupInput(
            ns("sel_cols_features"),
            label = "Discrete features",
            choices = discrete_features,
            selected = sel_features_stored()
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_cols"), "Proceed")
          )
        )
      )
    }) |> bindEvent(input$tbl_settings)

    observe({
      sel_issues_stored(input$sel_cols_issues)
      sel_features_stored(input$sel_cols_features)
      removeModal()

    }) |> bindEvent(input$confirm_cols)

    output$img_flags <- rhandsontable::renderRHandsontable({
      req(sel_marker())
      sel_disq <- intersect(sel_issues_stored(), disqual_issues)
      sel_techn <- intersect(sel_issues_stored(), technical_issues)
      sel_other <- intersect(sel_issues_stored(), other_issues)
      sel_feat <- intersect(sel_features_stored(), discrete_features)
      df_rings <- rings_data_edited() |>
        dplyr::filter(image_label == sel_marker()$ycov_val) |>
        dplyr::select(
          year,
          dplyr::all_of(c(
            "duplicate_ring","exclude_dupl",
            "exclude_issues","affected_tissue",
            sel_disq, sel_techn, sel_other,
            sel_feat,"comment"))
        )

      selring_idx <- which(df_rings$year == sel_marker()$year) - 1
      df_rings <- df_rings |> tibble::column_to_rownames("year")

      ro_ids_dupl <- which(rep(TRUE, nrow(df_rings))) - 1
      ro_ids_excldupl <- which(!df_rings$duplicate_ring) - 1

      warn_col_ids <- which(names(df_rings) %in% disqual_issues) - 1

      color_dupl <- prim_col_grad[4]
      color_excl <- tert_col_grad[4]
      color_iss1 <- tert_col_grad[5]
      color_iss2 <- tert_col_grad[6]

      hot <- rhandsontable::rhandsontable(
        df_rings,
        stretchH = "all",
        contextMenu = FALSE,
        height = 400
      ) |>
      rhandsontable::hot_col("duplicate_ring", type = "checkbox", halign = "htCenter",
                             renderer = renderer_cb_ro(selring_idx, ro_ids_dupl, color_dupl)) |>
      rhandsontable::hot_col("exclude_dupl", type = "checkbox", halign = "htCenter",
                             renderer = renderer_cb_dupl(selring_idx, ro_ids_excldupl, color_dupl)) |>
      rhandsontable::hot_col("exclude_issues",  type = "checkbox", halign = "htCenter",
                             renderer = renderer_cb_val(selring_idx, warn_col_ids, color_excl)) |>
      rhandsontable::hot_col("affected_tissue", type = "dropdown", source = c("", "all", "ew", "lw"),
                             renderer = renderer_dd(selring_idx, color_excl))
      if (length(sel_disq) > 0){
        hot <- hot %>%
          purrr::reduce(
            sel_disq, # names in df
            function(ht, col) {
              ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                           renderer = renderer_cb(selring_idx, color_iss1))
            },
            .init = .
          )
      }
      if (length(sel_techn) > 0){
        hot <- hot %>%
          purrr::reduce(
            sel_techn, # names in df
            function(ht, col) {
              ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                           renderer = renderer_cb(selring_idx, color_iss2))
            },
            .init = .
          )
      }
      if (length(sel_other) > 0){
        hot <- hot %>%
          purrr::reduce(
            sel_other, # names in df
            function(ht, col) {
              ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                           renderer = renderer_cb(selring_idx, color_iss1))
            },
            .init = .
          )
      }
      if (length(sel_feat) > 0){
        hot <- hot %>%
          purrr::reduce(
            sel_feat, # names in df
            function(ht, col) {
              ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                           renderer = renderer_cb(selring_idx, color_iss2))
            },
            .init = .
          )
      }
      hot |>
        rhandsontable::hot_col("comment", renderer = renderer_txt(selring_idx, color_iss1)) |>
        rhandsontable::hot_cols(colWidths = 25) %>%
        # add esc key functionality to table
        htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot;

            // Handle Escape key
            hot.addHook('afterDocumentKeyDown', function(event) {
              if (event.key === 'Escape' || event.keyCode === 27) {
                // Deselect all cells
                hot.deselectCell();

                // Blur the active element to remove focus
                if (document.activeElement) {
                  document.activeElement.blur();
                }

                // Stop event propagation
                event.preventDefault();
                event.stopImmediatePropagation();

                return false;
              }
            });
          }
        ") %>%
        # keep track of user selected row in table
        htmlwidgets::onRender(sprintf("
          function(el, x) {
            var hot = this.hot;

            // Capture user selection
            hot.addHook('afterSelection', function(r, c, r2, c2) {
              Shiny.setInputValue('%s', r + 1, {priority: 'event'});
            });
          }
        ", ns("selected_hot_row")))

    }) |> bindEvent(sel_marker(), input$confirm_cols, ignoreNULL = TRUE)

    # Update reactiveVal when user selects
    observeEvent(input$selected_hot_row, {
        selected_row(input$selected_hot_row)
    }, priority = 1)  # LOW PRIORITY)

    # Create a reactiveVal
    selected_row <- reactiveVal(NULL)

    renderer_cb <- function(row_idx, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            if (row == %s) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
            return td;
        }", row_idx, bgcolor_js, bgcolor_js)))
    }

    renderer_cb_ro <- function(row_idx, readonly_ids = NULL, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      readonly_js <- if(is.null(readonly_ids)) "[]" else jsonlite::toJSON(readonly_ids)

      htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          var readonlyRows = %s;

          if (row == %s) {
            td.style.background = '#ed4c4c';
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // Set readOnly for specific rows
          if (readonlyRows.includes(row)) {
            cellProperties.readOnly = true;
            td.style.opacity = '0.6';  // Optional: visual indication
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", readonly_js, row_idx, bgcolor_js, bgcolor_js)))
    }

    renderer_cb_dupl <- function(row_idx, readonly_ids = NULL, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      readonly_js <- if(is.null(readonly_ids)) "[]" else jsonlite::toJSON(readonly_ids)

      htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          var readonlyRows = %s;
          var rowData = instance.getDataAtRow(row);
          console.log(rowData);

          if (row == %s) {
            td.style.background = '#ed4c4c';
          } else if (rowData[0] && !value) {
              td.style.background = '#99C2C2';  // Light green
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // Set readOnly for specific rows
          if (readonlyRows.includes(row)) {
            cellProperties.readOnly = true;
            td.style.opacity = '0.6';  // Optional: visual indication
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", readonly_js, row_idx, bgcolor_js, bgcolor_js)))
    }

    renderer_cb_val <- function(row_idx, warn_col_ids = NULL, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      warn_js <- if(is.null(warn_col_ids)) "[]" else jsonlite::toJSON(warn_col_ids)

      htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          // remove old tippy if necessary
          if(td.hasOwnProperty('_tippy')) {
            td._tippy.destroy();
          }
          var warnCols = %s;

          // Default styling based on row_idx and bgcolor
          if (row == %s) {
            td.style.background = '#ed4c4c';
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // invalid warning if other cells in same row, but warn_col_ids include value true
          if (warnCols.length > 0) {
            var rowData = instance.getDataAtRow(row);
            var has_issues = warnCols.map((item) => rowData[item]).includes(true);
            if (!value && has_issues) {
              td.style.background = 'pink';
              tippy(td, { content: 'exclude ring with disqualifying issues?' });
            }
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", warn_js, row_idx, bgcolor_js, bgcolor_js)))
    }



    # // green background for dupl_ids rows where value is false (selected duplicate)
    # if (duplRows.includes(row) && value === false) {
    #   td.style.background = '#90EE90';  // Light green
    # }

    renderer_dd <- function(row_idx, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            if (row == %s) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.DropdownRenderer.apply(this, arguments);
            return td;
        }", row_idx, bgcolor_js, bgcolor_js)))
    }
    renderer_txt <- function(row_idx, bgcolor = NULL){
      bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
      htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            if (row == %s) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            return td;
        }", row_idx, bgcolor_js, bgcolor_js)))
    }

    # capture hot table edits
    flags_out <- reactive({
      rhandsontable::hot_to_r(input$img_flags)
    })

    # capture edits on the excl flags column specifically (for excl markers)
    excl_flags <- reactiveVal()

    # updated only if exclude_issues column changes
    observe({
      new_col <- flags_out()$exclude_issues
      if (!identical(new_col, excl_flags())) {
        excl_flags(new_col)
      }
    }) |> bindEvent(flags_out(), ignoreInit = TRUE)

    # flags_out changes to update rings_data_edited
    observe({
      req(flags_out(), sel_marker())
      df_flags <- flags_out()
      img_sel <- sel_marker()$ycov_val
      df_flags$image_label <- img_sel
      df_flags <- df_flags |>
        tibble::rownames_to_column("year") |>
        dplyr::mutate(year = as.integer(year))

      # adjusting the duplicate ring selections if changed ---------------------
      old_dupl_sel <- rings_data_edited() |>
        dplyr::filter(image_label == img_sel) |>
        dplyr::filter(duplicate_ring) |>
        dplyr::select(year, exclude_dupl) |>
        dplyr::arrange(year)

      new_dupl_sel <- df_flags |>
        dplyr::filter(duplicate_ring) |>
        dplyr::select(year, exclude_dupl) |>
        dplyr::arrange(year)

      if (any(old_dupl_sel$exclude_dupl != new_dupl_sel$exclude_dupl)){
        cat(" new duplicate selected!\n")
        for (k in 1:nrow(new_dupl_sel)){
          # if new_dupl_sel is now set to FALSE -> set the other images to TRUE
          if (!new_dupl_sel$exclude_dupl[k]){
            df_new <- rings_data_edited() |>
              dplyr::filter(woodpiece_label == sel_marker()$wp_label) |>
              dplyr::filter(image_label != img_sel) |>
              dplyr::filter(year == new_dupl_sel$year[k]) |>
              dplyr::select(dplyr::all_of(names(df_flags))) |>
              dplyr::mutate(exclude_dupl = TRUE)
              print(df_new[c("image_label","year","exclude_issues", "exclude_dupl")])
            df_flags <- df_flags |> dplyr::bind_rows(df_new)
          } else {
            # if new_dupl_sel is now TRUE -> find the one with max cno to set to FALSE
            df_new <- rings_data_edited() |>
              dplyr::filter(woodpiece_label == sel_marker()$wp_label) |>
              dplyr::filter(image_label != img_sel) |>
              dplyr::filter(year == new_dupl_sel$year[k]) |>
              dplyr::mutate(duplicate_rank = cno - 100*as.numeric(exclude_issues),
                            exclude_dupl = duplicate_rank < max(duplicate_rank)) |>
              dplyr::select(dplyr::all_of(names(df_flags)))
            print(df_new[c("image_label","year","exclude_issues", "exclude_dupl")])
            df_flags <- df_flags |> dplyr::bind_rows(df_new)
          }
        }
      }

      # update the flags in the rings dataframe
      df_rings <- rings_data_edited()
      df_rings <- df_rings |>
        dplyr::rows_update(df_flags, by = c("image_label", "year"))


      rings_data_edited(df_rings)
    }) |> bindEvent(flags_out(), ignoreInit = TRUE)

    # TODO: highlight excluded markers based on rings_data_edited() (up to date)
    # TODO: image comment
    # TODO:


    # save edits, update plot
    observe({
      req(rings_data_edited())
      rings_data_org(rings_data_edited())
    }) |> bindEvent(input$apply_changes, ignoreNULL = TRUE, ignoreInit = TRUE)


    # OPEN IMAGE ---------------------------------------------------------------
    observe({
      req(sel_marker())
      sel_img <- sel_marker()$ycov_val

      image_path <- input_data$rxsmeta_data %>%
        dplyr::filter(image_label == sel_img) %>%
        dplyr::pull(fname_image)

      base_path <- dirname(image_path)

      # Check for annotated twin image in same folder
      annotated_twin <- list.files(
        base_path,
        pattern = paste0(sel_img, "_annotated_twin\\."),
        full.names = TRUE
      )
      annotated_image <- list.files(
        base_path,
        pattern = paste0(sel_img, "_annotated\\."),
        full.names = TRUE
      )

      # Prefer twin > annotated > image file if found
      if (length(annotated_twin) == 1) {
        file_to_open <- annotated_twin
      } else if (length(annotated_image) == 1) {
        file_to_open <- annotated_image
      } else {
        file_to_open <- image_path
      }

      if (file.exists(file_to_open)) {
        browseURL(file_to_open)
      } else {
        showNotification(
          glue::glue("The following image could not be opened: {file_to_open}"),
          type = "error"
        )
      }

    }) |> bindEvent(input$show_image, ignoreNULL = TRUE)

    # auto open if sel image changes
    sel_image <- reactiveVal(NULL)
    observe({
      prev_img <- sel_image()
      new_img <- sel_marker()$ycov_val
      if (!identical(prev_img, new_img)){
        sel_image(new_img)
      }
    }) |> bindEvent(sel_marker(), ignoreNULL = TRUE)

    observe({
      req(input$auto_open_image)
      cat(glue::glue(".   auto open image {sel_image()}"), "\n")
      # trigger image opening when sel_image changes
      shinyjs::click("show_image")
    }) |> bindEvent(sel_image(), ignoreNULL = TRUE)

    # ENTER KEY TO SELECT CELL IN TABLE ----------------------------------------
     observeEvent(input$enter_key, {
       req(sel_marker(), flags_out())
       marker <- sel_marker()

      # Check conditions
      if (is.null(marker$year)){
        return(NULL)
      }

      df_hot <- flags_out()
      # Find matching row
      target_year <- as.character(marker$year)
      row_idx <- which(rownames(df_hot) == target_year)

      if (length(row_idx) == 0) {
        return(NULL)
      }

      # Select the cell
      shinyjs::runjs(sprintf("
        var hot = HTMLWidgets.find('#%s');
        if (hot && hot.hot) {
          hot.hot.selectCell(%d, 2);
          hot.hot.scrollViewportTo(%d, 2);
        }
      ", ns("img_flags"), row_idx - 1, row_idx - 1))
    })

    # SAVE RESULTS TO FILE -------------------------------------------------------
    output$save_data <- downloadHandler(
      filename = function() {
        glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_DATASETNAME_rings_edited.csv")
      },
      content = function(con) {
        df <- rings_data_edited()
        df_char <- df |> dplyr::select(
          affected_tissue, comment)
        df_flags_opt <- df |> dplyr::select(
          dplyr::any_of(setdiff(disqual_issues, c('incomplete_ring','missing_ring'))),
          dplyr::any_of(c(unname(technical_issues), unname(other_issues),
                          unname(discrete_features))))
        df <- df[setdiff(names(df), c("affected_tissue", "comment", names(df_opt)))]
        df_char <- df_char |> janitor::remove_empty(which = "cols", cutoff = 1)
        df_flags_opt <- df_flags_opt |>
          janitor::remove_empty(which = "cols", cutoff = 1) |>
          dplyr::select(dplyr::where(~any(.)))
        df <- df |> dplyr::bind_cols(df_char, df_flags_opt)
        df <- df |> dplyr::select(dplyr::any_of(names(rings_data_edited())))
        readr::write_csv(df, con)
      }
    )

    observe({
      showModal(modalDialog(
        title = "Confirm exit",
        "If you have completed your editing and saved your results, you can exit the application.
        The edited rings dataframe will also be available in the R global environment under df_rings_edited if returning
        to a local R session.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_exit"), "Exit")
        )
      ))
    }) |> bindEvent(input$close_app, ignoreNULL = TRUE)


     observe({
       df <- rings_data_edited()
       df_char <- df |> dplyr::select(
         affected_tissue, comment)
       df_flags_opt <- df |> dplyr::select(
         dplyr::any_of(setdiff(disqual_issues, c('incomplete_ring','missing_ring'))),
         dplyr::any_of(c(unname(technical_issues), unname(other_issues),
                         unname(discrete_features))))
       df <- df[setdiff(names(df), c("affected_tissue", "comment", names(df_opt)))]
       df_char <- df_char |> janitor::remove_empty(which = "cols", cutoff = 1)
       df_flags_opt <- df_flags_opt |>
         janitor::remove_empty(which = "cols", cutoff = 1) |>
         dplyr::select(dplyr::where(~any(.)))
       df <- df |> dplyr::bind_cols(df_char, df_flags_opt)
       df <- df |> dplyr::select(dplyr::any_of(names(rings_data_edited())))
       df_rings_edited <<- df
       stopApp()
     }) |> bindEvent(input$confirm_exit, ignoreNULL = TRUE)


    # DEBUG OUTPUT -------------------------------------------------------------
    output$debug <- renderPrint({
      #print(sample(1:10000, 1))
      #input$traces_crn[['exclude_markers']]


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
