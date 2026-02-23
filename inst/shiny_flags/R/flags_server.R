flags_server <- function(id, main_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # REACTIVE CONTAINERS ------------------------------------------------------
    # reactive container for input data
    input_data <- shiny::reactiveValues(
      prf_data = NULL,
      rings_data = NULL,
      rxsmeta_data = NULL
    )
    # reactive containers for rings data
    # NOTE: rings_data_org is the basis for the plot, rings_data_edited tracks
    # edits of the flag inputs for selected rings. edits are then propagated back
    # to rings_data_org when the save / update btn is clicked
    rings_data_org <- shiny::reactiveVal(NULL)
    rings_data_edited <- shiny::reactiveVal(NULL)

    # reactive containers for currently selected trace / image / year in the
    # plot(s) and table
    sel_woodpiece <- shiny::reactiveVal(NULL)
    sel_image <- shiny::reactiveVal(NULL)
    sel_marker <- shiny::reactiveVal(NULL)

    # reactive containers for plot settings and data
    sel_subplots <- shiny::reactiveVal(c("sd","cov")) # default: show optional subplots
    awaiting_restoration <- shiny::reactiveVal(FALSE)
    shift_ring <- shiny::reactiveVal(NULL)

    # reactive containers for table settings and data
    sel_issue_cols <- shiny::reactiveVal(c(disqual_issues, technical_issues, other_issues)) # default: show all flag columns
    sel_feat_cols <- shiny::reactiveVal(discrete_features)
    excl_flags <- shiny::reactiveVal(NULL)
    current_hot_image <- shiny::reactiveVal(NULL)
    selected_row <- shiny::reactiveVal(NULL)


    # LOAD INPUT DATA ----------------------------------------------------------
    # open input modal when button open_input_modal is clicked
    shiny::observe({
      # warn before overwriting existing data
      no_existing_inputs <- all(
        is.null(input_data$prf_data),
        is.null(input_data$rings_data),
        is.null(input_data$rxsmeta_data))
      if (!no_existing_inputs) {
        shiny::showModal(input_warning_modal(ns))
      } else {
        shiny::showModal(input_source_modal(ns))
      }
    }) |> shiny::bindEvent(input$open_input_modal)

    # show input source modal after overwrite confirmed
    shiny::observe({
      shiny::removeModal()
      shiny::showModal(input_source_modal(ns))
    }) |> shiny::bindEvent(input$confirm_overwrite) # cf. input_warning_modal

    # dynamic ui to provide input data details (cf. input_source_modal)
    output$load_details_ui <- shiny::renderUI({
      shiny::req(input$load_type)

      switch(input$load_type,
        "env" = build_env_inputs(ns),
        "csv" = build_csv_inputs(ns),
        "example" = build_example_inputs(ns)
      )
    })

    # when inputs are confirmed, load data
    shiny::observe({
      safe_block({
        res <- switch(input$load_type,
          "env" = load_data_env(
            name_prf = input$name_prf,
            name_rings = input$name_rings,
            name_rxsmeta = input$name_rxsmeta
          ),
          "csv" = load_data_csv(
            path_prf = input$file_prf$datapath,
            path_rings = input$file_rings$datapath,
            path_rxsmeta = input$file_rxsmeta$datapath
          ),
          "example" = {
            # TODO: add example files to extdata
            # path_prf <- system.file("extdata", "example_input",
            #                         "prf_data.csv", package = "rxs2tria")
            # path_rings <- system.file("extdata", "example_input",
            #                           "rings_data.csv", package = "rxs2tria")
            # path_rxsmeta <- system.file("extdata", "example_input",
            #                             "rxsmeta_data.csv", package = "rxs2tria")
            # load_data_csv(path_prf, path_rings, path_rxsmeta)
          }
        )

        validate_input_dfs(res$prf_data, res$rings_data, res$rxsmeta_data)

        input_data$prf_data <- res$prf_data
        input_data$rings_data <- res$rings_data
        input_data$rxsmeta_data <- res$rxsmeta_data
        shiny::removeModal()
      },
      err_title = glue::glue("Error loading data from {input$load_type}"),
      err_message = "There was an error reading the provided input data:\n",
      propagate_err = FALSE
      )
    }) |> shiny::bindEvent(input$confirm_input)

    # initialize rings_data_out with input rings data
    shiny::observe({
      df_rings <- input_data$rings_data |>
        init_flag_columns(all_flags) |>
        init_char_columns() |>
        dplyr::select(
          dplyr::all_of(names(input_specs$rings_data$req_cols)),
          dplyr::all_of(names(input_specs$rings_data$opt_cols)), # all exist now
          dplyr::where(is.numeric)
        ) |>
        arrange_rings()

      rings_data_org(df_rings)
    }) |> shiny::bindEvent(input_data$rings_data)

    # initialize editable copy of rings_data_out
    shiny::observe({
      rings_data_edited(rings_data_org())
    }) |> shiny::bindEvent(rings_data_org())

    # UI CHANGES GIVEN INPUT DATA:
    # update sidebar UI based on loaded input data
    shiny::observe({
      shiny::req(input_data$rings_data)
      prf_data <- input_data$prf_data
      rings_data <- input_data$rings_data
      rxsmeta_data <- input_data$rxsmeta_data

      # site and species select inputs
      if (!is.null(rxsmeta_data)) {
        shinyjs::show("filt_species")
        shinyjs::show("filt_site")

        species_choices <- unique(rxsmeta_data$species_code)
        shiny::updateSelectInput(session, "filt_species",
                                 choices = species_choices,
                                 selected = species_choices[1])

        site_choices <- unique(rxsmeta_data$site_label)
        filt_sites <- rxsmeta_data |>
          dplyr::filter(species_code == species_choices[1]) |>
          dplyr::pull(site_label) |>
          unique()
        shiny::updateSelectInput(session, "filt_site",
                                 choices = site_choices,
                                 selected = filt_sites)
      } else {
        shinyjs::hide("filt_species")
        shinyjs::hide("filt_site")

        # without rxsmeta, just show all possible wp from rings_data
        wp_choices <- unique(rings_data$woodpiece_label)
        shiny::updateSelectInput(session, "filt_wp",
                                 choices = wp_choices,
                                 selected = wp_choices)
      }
      # parameter select input
      param_choices_ring <- rings_data |>
        dplyr::select(dplyr::where(is.numeric), -year) |>
        names()

      if (!is.null(prf_data)) {
        shinyjs::show("sel_sector")

        param_choices_prf <- prf_data |>
          dplyr::select(dplyr::where(is.numeric), -year, -sector_n) |>
          names()
        shiny::updateSelectInput(
          session, "sel_param",
          choices = list(
            "Ring level"      = param_choices_ring,
            "Agg. cell level" = param_choices_prf
          ),
          selected = param_choices_prf[1]
        )

        sector_choices <- sort(unique(prf_data$sector_n))
        shiny::updateSelectInput(session, "sel_sector",
                                 choices = sector_choices,
                                 selected = sector_choices[1])
      } else {
        shinyjs::hide("sel_sector")
        shiny::updateSelectInput(
          session, "sel_param",
          choices = list(
            "Ring level" = param_choices_ring
          ),
          selected = param_choices_ring[1]
        )
      }

    }) |> shiny::bindEvent(input_data$prf_data,
                           input_data$rings_data,
                           input_data$rxsmeta_data,
                           ignoreInit = TRUE)

    # update filter wp input based on sel site and species
    shiny::observe({
      shiny::req(input_data$rxsmeta_data, input$filt_site, input$filt_species)
      wp_choices <- input_data$rxsmeta_data |>
        dplyr::filter(
          site_label %in% input$filt_site,
          species_code %in% input$filt_species
        ) |>
        dplyr::pull(woodpiece_label) |>
        unique()
      shiny::updateSelectInput(
        session, "filt_wp",
        choices = wp_choices,
        selected = wp_choices
      )
    }) |> shiny::bindEvent(input$filt_site, input$filt_species,
                           ignoreInit = TRUE)

    # enable/disable sector selection based on selected parameter
    shiny::observe({
      shiny::req(input_data$prf_data)
      sel_param <- input$sel_param
      if (sel_param %in% names(input_data$prf_data)) {
        shinyjs::enable("sel_sector")
      } else {
        shinyjs::disable("sel_sector")
      }
    }) |> shiny::bindEvent(input$sel_param, ignoreInit = TRUE)



    # MEASUREMENTS PLOT --------------------------------------------------------
    # UI: Plot card header with dynamic woodpiece selection input
    output$selwp <- shiny::renderUI({
      shiny::req(input$filt_wp)
      card_title <- strong(glue::glue("Selected woodpiece: "))
      wp_choices_filt <- input$filt_wp
      shiny::div(
        card_title,
        shiny::selectInput(
          ns("sel_wp_trace"), NULL,
          choices = wp_choices_filt,
          selected = wp_choices_filt[1],
          selectize = TRUE, multiple = FALSE
        )
      )
    })

    # choosing a different woodpiece via the input
    shiny::observe({
      sel_woodpiece(input$sel_wp_trace)
    }) |> shiny::bindEvent(input$sel_wp_trace,
                           ignoreInit = TRUE, ignoreNULL = TRUE)

    # UI: show modal when clicking on plot settings
    shiny::observe({
      shiny::showModal(
        plot_settings_modal(ns, sel_subplots())
      )
    }) |> shiny::bindEvent(input$plot_settings)

    # update selected subplots based on modal input
    shiny::observe({
      sel_subplots(input$sel_plots)
      shiny::removeModal()
    }) |> shiny::bindEvent(input$confirm_plots)

    # variable height for the plot depending on shown subplots
    output$main_plot_ui <- shiny::renderUI({
      print(sel_subplots())
      min_height <- compute_plot_height(sel_subplots())
      print(min_height)
      shiny::div(
        style = paste0("min-height: ", min_height, "; max-height: ", plot_max_height),
        plotly::plotlyOutput(ns("main_plot"), height = "100%")
      )
    })

    # define REACTIVES as basis for plot(s)
    #  create a color palette for the plot based on the max. nr of traces
    color_palette <- shiny::reactive({
      shiny::req(input_data$rings_data)
      # use all woodpieces (not filtered) to keep colors consistent when changing filters
      wp_labels_all <- sort(unique(input_data$rings_data$woodpiece_label))
      pal <- extend_palette(base_palette, length(wp_labels_all),
                            contrasting = TRUE)
      pal <- setNames(pal, wp_labels_all)
      pal
    })

    # reactive df_crn as basis for chronology traces
    df_crn <- shiny::reactive({
      shiny::req(rings_data_org(), input$filt_wp, input$sel_param)

      df <- build_chronology_df(
        rings_data = rings_data_org(),
        prf_data = input_data$prf_data,
        filt_wp = input$filt_wp,
        sel_param = input$sel_param,
        sel_sector = input$sel_sector,
        show_excl = input$show_excl
      )

      if (input$apply_detrend) {
        df <- detrend_crn(df, input$sel_param, method = "Spline", nyrs = 32)
      }

      df
    })

    # reactive df_selwp for trace of selected woodpiece
    # (includes also exclude_dupl years for plotting markers not on curve)
    df_selwp <- shiny::reactive({
      shiny::req(df_crn(), sel_woodpiece())
      dplyr::filter(df_crn(), woodpiece_label == sel_woodpiece())
    })

    # reactive df_otherwps for traces of all other woodpieces
    df_otherwps <- shiny::reactive({
      shiny::req(df_crn(), sel_woodpiece())
      df_crn() |>
        dplyr::filter(woodpiece_label != sel_woodpiece(), !exclude_dupl)
    })

    # reactive df for sample depth plot
    df_sd <- shiny::reactive({
      if (!"sd" %in% sel_subplots()) return(NULL)
      shiny::req(df_crn())
      build_sample_depth_df(df_crn())
    })

    # reactive df for coverage plot
    df_cov <- shiny::reactive({
      if (!"cov" %in% sel_subplots()) return(NULL)
      shiny::req(rings_data_org(), sel_woodpiece())
      build_coverage_df(rings_data_org(), sel_woodpiece())
    })

    # TODO: update sel_marker when inputs change?
    # TODO: check if ok for single woodpiece?

    # load js callbacks as strings
    js_sync_hover <- readLines("www/js/sync_hover_sd.js") |>
      paste(collapse = "\n")
    js_traces <- readLines("www/js/traces_to_input.js") |>
      paste(collapse = "\n")

    # rendering the main plot
    output$main_plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(shiny::isTruthy(df_selwp()),
                    "Please provide input data"))
      shiny::validate(
        shiny::need(any(!is.na(df_selwp()$vals)),
                    "No data to display for the selected parameter and filters."))

      cat("=== PLOT RENDERING ===\n")
      cat("Time:", format(Sys.time()), "\n")

      p_crn <- build_crn_plot(df_otherwps(), df_selwp(),
                              sel_woodpiece(), color_palette())

      if ("sd" %in% sel_subplots()) {
        p_sd <- build_sample_depth_plot(df_sd())
      } else {
        p_sd <- NULL
      }

      if ("cov" %in% sel_subplots()) {
        p_cov <- build_coverage_plot(df_cov(), sel_woodpiece())
      } else {
        p_cov <- NULL
      }

      fig <- assemble_main_plot(crn_plot = p_crn, sd_plot = p_sd,
                                cov_plot = p_cov, sel_param = input$sel_param)

      # attach JS callbacks
      if ("sd" %in% sel_subplots()) {
        fig <- fig %>% htmlwidgets::onRender(js_sync_hover)
      }
      fig <- fig %>% htmlwidgets::onRender(js_traces, data = ns("traces_crn"))

      fig

    }) |> shiny::bindEvent(df_selwp(), df_otherwps(), df_sd(), df_cov())


    # TODO: highlight row of selected image in cov plot?
    #   plotly::layout(shapes = list(list(type = "line",x0 = 0,
    #                                     x1 = 1,
    #                                     xref = "paper",
    #                                     y0 = sel_img,
    #                                     y1 = sel_img,
    #                                     line = list(color = 'red', width = 3),
    #                                     layer = "below")))


    ## plot rerendering --------------------------------------------------------
    # if plot data changes (-> render), set flag to restore state after render
    shiny::observe({
      awaiting_restoration(TRUE)
    }) |> shiny::bindEvent(df_selwp(), df_otherwps(), df_sd(), df_cov(),
                           ignoreInit = TRUE)

    # restore state of plot
    shiny::observe({
      shiny::req(awaiting_restoration())
      shiny::req(input$traces_crn)

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

          df_crn <- df_crn() |>
            dplyr::filter(!exclude_dupl)
          # df_crn <- plot_data$df_other |>
          #   dplyr::bind_rows(plot_data$df_selwp) |>
          #   dplyr::filter(!exclude_dupl)

          sel_mean <- input$sel_mean
          df_mean <- calc_mean_vals(df_crn, sel_mean)
          p <- p |> draw_mean_trace(df_mean$year, df_mean$vals, sel_mean)
        }


        if (isTruthy(sel_marker())){
          cat("... redrawing sel ring markers\n")
          marker <- sel_marker()
          yaxis_cov <- if ("sd" %in% sel_subplots()) "y3" else "y2"

          if (marker$wp_label %in% df_selwp()$woodpiece_label){
            # ensure we have up to date y val if param was changed
            ywp_val <- df_selwp() |>
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
            # sel_image remains the same

            if (!"sel_marker_wp" %in% names(current_traces)) {
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
                    ),
                    hovertext = list(paste0(marker$ycov_val, "<br>",
                                            marker$year, ": ", ywp_val))
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

            excl_markers <- df_selwp() |>
              dplyr::inner_join(df_rings, by = c("image_label", "year")) |>
              dplyr::select(year, vals)
            names(excl_markers) <- c("year", "ywp_val")

            if (!"excl_markers" %in% names(current_traces)) {
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


    # PLOT REACTIVITY ----------------------------------------------------------
    ## plot clicks -------------------------------------------------------------
    # capture click events on crn or cov plots, identify clicked trace and role
    plot_click <- shiny::reactive({
      shiny::req(df_selwp())
      event <- plotly::event_data(
        "plotly_click", source = "crn_plot", priority = "event"
      )
      shiny::req(nrow(event) == 1) # filter out double clicks somehow capturing two points

      current_traces <- shiny::isolate(input$traces_crn)
      clicked_trace <- purrr::keep(current_traces,
                                   ~.x$curveNumber == event$curveNumber)
      clicked_role <- clicked_trace[[1]]$meta$role

      if (!clicked_role %in% c("selwp", "covlines", "otherwp")) {
        return(NULL)
      }

      event$role <- clicked_role
      event$wp_label <- if (clicked_role == "otherwp") {
        names(clicked_trace)
      } else {
        clicked_trace[[1]]$meta$wp_label
      }

      event
    })

    # react to plot click: switch wp or draw updated marker directly
    shiny::observe({
      shiny::req(!awaiting_restoration()) # avoid running if plot is being rerendered
      cat(".   plot click\n")

      click_data <- plot_click()
      if (click_data$role == "otherwp") {
        showModal(
          switch_selwp_modal(ns, click_data$wp_label)
        )
      } else {
        new_marker <- resolve_click_marker(click_data, df_selwp(), input$sel_param)
        sel_marker(new_marker)
        sel_image(new_marker$ycov_val)

        p <- plotly::plotlyProxy("main_plot", session)
        draw_sel_marker(p, new_marker, input$traces_crn, sel_subplots())
      }

    }) |> shiny::bindEvent(plot_click(), ignoreNULL = TRUE, ignoreInit = FALSE)

    # confirmed click on other woodpiece
    shiny::observe({
      shiny::removeModal()
      click_data <- plot_click()
      new_marker <- resolve_click_marker(click_data, df_otherwps(), input$sel_param)

      sel_marker(new_marker)
      sel_image(new_marker$ycov_val)

      # trigger updates of plot, markers, table indirectly:
      # input$sel_wp_trace > sel_woodpiece() > df_selwp(), df_otherwps() > plot rerender
      shiny::updateSelectInput(session, "sel_wp_trace", selected = new_marker$wp_label)

    }) |> shiny::bindEvent(input$confirm_wp)

    ## ring shifts -------------------------------------------------------------
    # capture and track direction of ring shift (button or keybinding)
    shiny::observe({
      cat(".   shift ring next\n")
      shift_ring(list(
        dir = "next",
        nonce = sample(1e6, 1)
      ))
    }) |> shiny::bindEvent(input$next_ring)

    shiny::observe({
      cat(".   shift ring prev\n")
      shift_ring(list(
        dir = "prev",
        nonce = sample(1e6, 1)
      ))
    }) |> shiny::bindEvent(input$prev_ring)

    # update the selected marker after shift
    shiny::observe({
      shiny::req(sel_marker())
      sel_wp <- sel_marker()$wp_label
      current_year <- sel_marker()$year
      direction <- shift_ring()$dir

      new_marker <- resolve_shift_marker(sel_wp, current_year, direction,
                                         df_selwp(), input$sel_param)
      if (is.null(new_marker)) {
        shiny::showNotification(sprintf(
          "No %s year available for this woodpiece in current plot.",
          if (direction == "prev") "earlier" else "later"),
          type = "warning")
        return(NULL)
      }

      sel_marker(new_marker)
      # only update sel_image() if necessary to avoid hot rerender
      if (!identical(sel_image(), new_marker$ycov_val)) {
        sel_image(new_marker$ycov_val)
      }

      # redraw marker(s)
      p <- plotly::plotlyProxy("main_plot", session)
      draw_sel_marker(p, new_marker, input$traces_crn, sel_subplots())

    }) |> shiny::bindEvent(shift_ring(), ignoreNULL = TRUE, ignoreInit = TRUE)


    ## hot row changes ---------------------------------------------------------
    shiny::observe({
      # require single selected?
      shiny::req(sel_marker())
      sel_wp <- sel_marker()$wp_label
      sel_img <- sel_marker()$ycov_val
      new_year <- as.integer(rownames(flags_out())[selected_row()])

      if (sel_marker()$year == new_year) {
        return(NULL) # same row
      }

      new_marker <- resolve_row_marker(sel_wp, sel_img, new_year, df_selwp(), input$sel_param)

      sel_marker(new_marker)
      # no update to sel_image required

      # redraw sel marker(s)
      p <- plotly::plotlyProxy("main_plot", session)
      draw_sel_marker(p, new_marker, input$traces_crn, sel_subplots())

    }) |> shiny::bindEvent(selected_row(), ignoreNULL = TRUE)


    ## x axis changes ----------------------------------------------------------
    # capture axis limit changes for the x axis via plotly relayout events
    crn_x_axes <- shiny::reactive({
      shiny::req(df_selwp())
      relayout <- plotly::event_data("plotly_relayout", source = "crn_plot")
      shiny::req(relayout)

      # ignore events that don't affect the x-axis (e.g. yaxis changes)
      x_autorange <- !is.null(relayout[["xaxis.autorange"]])
      has_x_range  <- !is.null(relayout[["xaxis.range[0]"]])
      shiny::req(x_autorange || has_x_range)

      if (x_autorange) {
        list(x_min = NULL, x_max = NULL)
      } else {
        list(
          x_min = relayout[["xaxis.range[0]"]],
          x_max = relayout[["xaxis.range[1]"]]
        )
      }
    })


    ## excluded rings markers --------------------------------------------------
    # capture edits on the excl flags column specifically
    shiny::observe({
      new_col <- flags_out()$exclude_issues
      if (!identical(new_col, excl_flags())) {
        excl_flags(new_col)
      }
    }) |> shiny::bindEvent(flags_out(), ignoreInit = TRUE)

    # if exclude flags are edited, update markers
    # NOTE: show_excl / change of sel_wp handled via plot rerender
    shiny::observe({
       shiny::req(excl_flags())
       cat(".   excl flags update\n")
      # TODO: do not trigger if excl_flags change but only because image is changed

      excl_markers <- get_excl_markers(sel_woodpiece(), df_selwp(), rings_data_edited())

      p <- plotly::plotlyProxy("main_plot", session)
      draw_excl_markers(p, excl_markers, input$traces_crn)

    }) |> shiny::bindEvent(excl_flags())


    ## mean crn trace ----------------------------------------------------------
    shiny::observe({
      shiny::req(df_crn())
      cat(".   mean crn update\n")

      has_mean_trace <- "mean_trace" %in% names(input$traces_crn)

      p <- plotly::plotlyProxy("main_plot", session)
      # remove existing mean trace (either no longer shown or needs to be
      # recalculated)
      if (has_mean_trace) {
        crn_curveNumber <- input$traces_crn[['mean_trace']]$curveNumber
        p <- p %>% plotly::plotlyProxyInvoke("deleteTraces", crn_curveNumber)
      }

      # recalculate and redraw mean trace only if shown
      if (input$show_mean) {
        df_data <- df_crn() |> dplyr::filter(!exclude_dupl)
        sel_mean <- input$sel_mean
        df_mean <- calc_mean_vals(df_data, sel_mean)
        p <- draw_mean_trace(p, df_mean$year, df_mean$vals, sel_mean)
      }

      p
    }) |> shiny::bindEvent(input$show_mean, input$sel_mean)


    # FLAGS TABLE --------------------------------------------------------------
    # UI: plot card header with dynamic selected image and comment
    output$selimg <- shiny::renderUI({
      shiny::req(sel_image())
      sel_img <- sel_image()
      df_img <- input_data$rxsmeta_data
      card_title <- strong(glue::glue("Selected image: {sel_img}"))

      if ("comments" %in% names(df_img)) {
        img_comment <- df_img |>
          dplyr::filter(image_label == sel_img) |>
          dplyr::pull(comments)
        if (!is.na(img_comment) && img_comment != "") {
          card_title <- tagList(
            card_title,
            em(glue::glue("Comment: {img_comment}"))
          )
        }
      }
      card_title
    })

    # UI: show modal when clicking on table settings
    shiny::observe({
      shiny::showModal(
        hot_settings_modal(ns, sel_issue_cols(), sel_feat_cols())
      )
    }) |> shiny::bindEvent(input$tbl_settings)

    # update selected table columns based on modal inputs
    shiny::observe({
      sel_issue_cols(input$sel_cols_issues)
      sel_feat_cols(input$sel_cols_features)
      shiny::removeModal()
    }) |> shiny::bindEvent(input$confirm_cols)

    # prepare reactive for hot table input
    df_rings_hot <- shiny::reactive({
      shiny::req(sel_image())
      sel_disq <- intersect(sel_issue_cols(), disqual_issues)
      sel_techn <- intersect(sel_issue_cols(), technical_issues)
      sel_other <- intersect(sel_issue_cols(), other_issues)
      sel_feat <- intersect(sel_feat_cols(), discrete_features)
      df_rings <- rings_data_edited() |>
        dplyr::filter(image_label == sel_image()) |>
        dplyr::select(
          year,
          dplyr::all_of(c(
            "duplicate_ring","exclude_dupl",
            "exclude_issues","affected_tissue",
            sel_disq, sel_techn, sel_other,
            sel_feat,"comments"))
        )
    }) |> shiny::bindEvent(sel_image(), input$confirm_cols)

    # render the HOT
    output$img_flags <- rhandsontable::renderRHandsontable({
      df_rings <- df_rings_hot()
      sel_disq <- intersect(sel_issue_cols(), disqual_issues)
      sel_techn <- intersect(sel_issue_cols(), technical_issues)
      sel_other <- intersect(sel_issue_cols(), other_issues)
      sel_feat <- intersect(sel_feat_cols(), discrete_features)
      current_hot_image(sel_marker()$ycov_val)

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
                             renderer = renderer_cb_ro(ro_ids_dupl, color_dupl)) |>
      rhandsontable::hot_col("exclude_dupl", type = "checkbox", halign = "htCenter",
                             renderer = renderer_cb_dupl(ro_ids_excldupl, color_dupl)) |>
      rhandsontable::hot_col("exclude_issues",  type = "checkbox", halign = "htCenter",
                             renderer = renderer_cb_val(warn_col_ids, color_excl)) |>
      rhandsontable::hot_col("affected_tissue", type = "dropdown", source = c("", "all", "ew", "lw"),
                             renderer = renderer_dd(color_excl))
      if (length(sel_disq) > 0){
        hot <- hot %>%
          purrr::reduce(
            sel_disq, # names in df
            function(ht, col) {
              ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                           renderer = renderer_cb(color_iss1))
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
                                           renderer = renderer_cb(color_iss2))
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
                                           renderer = renderer_cb(color_iss1))
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
                                           renderer = renderer_cb(color_iss2))
            },
            .init = .
          )
      }
      hot |>
        rhandsontable::hot_col("comments", renderer = renderer_txt(color_iss1)) |>
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
          // Set the initial highlighted row
          hot._highlightedRow = %s;
          console.log('Initial highlighted row:', hot._highlightedRow);

          // Remove previously attached hook if it exists
          if (hot._mySelectionHook) {
            hot.removeHook('afterSelection', hot._mySelectionHook);
          }

          // Define and store the new hook
          hot._mySelectionHook = function(r, c, r2, c2) {
            Shiny.setInputValue('%s', r + 1, {priority: 'event'});
          };

          hot.addHook('afterSelection', hot._mySelectionHook);

          hot.render();
        }
        ", selring_idx, ns("selected_hot_row")))

    }) |> bindEvent(df_rings_hot(), ignoreNULL = TRUE)

    # Update selected_row reactive given js hook
    observeEvent(input$selected_hot_row, {
        selected_row(input$selected_hot_row)
    })


    observe({
      # if selected marker is updated but still in same image, update highlighted row
      req(sel_marker())
      req(identical(sel_marker()$ycov_val, current_hot_image()))
      # Compute the row index from current table data
      df <- df_rings_hot()
      new_idx <- which(df$year == sel_marker()$year) - 1

      shinyjs::runjs(sprintf("
        var widget = HTMLWidgets.find('#%s');
        if (widget && widget.hot) {
          widget.hot._highlightedRow = %s;
          widget.hot.render();
        }
      ", ns("img_flags"), new_idx))
    }) |> bindEvent(sel_marker(), ignoreNULL = TRUE)


    # capture hot table edits
    flags_out <- reactive({
      rhandsontable::hot_to_r(input$img_flags)
    })

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

    # auto open if sel_image changes
    observe({
      req(input$auto_open_image)
      cat(glue::glue(".   auto open image {sel_image()}"), "\n")
      # trigger image opening when sel_image changes
      shinyjs::click("show_image")
    }) |> bindEvent(sel_image(), ignoreNULL = TRUE)


    # SAVE RESULTS TO FILE -------------------------------------------------------
    output$save_data <- downloadHandler(
      filename = function() {
        glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_DATASETNAME_rings_edited.csv")
      },
      content = function(con) {
        df <- rings_data_edited()
        df_char <- df |> dplyr::select(
          affected_tissue, comments)
        df_flags_opt <- df |> dplyr::select(
          dplyr::any_of(setdiff(disqual_issues, c('incomplete_ring','missing_ring'))),
          dplyr::any_of(c(unname(technical_issues), unname(other_issues),
                          unname(discrete_features))))
        df <- df[setdiff(names(df), c("affected_tissue", "comments", names(df_flags_opt)))]
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
         affected_tissue, comments)
       df_flags_opt <- df |> dplyr::select(
         dplyr::any_of(setdiff(disqual_issues, c('incomplete_ring','missing_ring'))),
         dplyr::any_of(c(unname(technical_issues), unname(other_issues),
                         unname(discrete_features))))
       df <- df[setdiff(names(df), c("affected_tissue", "comments", names(df_flags_opt)))]
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
        #sel_subplots()
        print(sample(1:10000, 1))
        req(input$traces_crn)
        df <- input$traces_crn |>
          purrr::map(\(x) {
            # separate nested from flat elements
            nested <- purrr::keep(x, is.list)
            flat   <- purrr::discard(x, is.list)
            # flatten one level and convert to tibble row
            tibble::as_tibble(c(flat, unlist(nested, recursive = FALSE)))
          }) |>
          purrr::list_rbind(names_to = "name")
        tail(df)



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
