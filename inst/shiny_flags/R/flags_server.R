flags_server <- function(id, main_session, comments_out) {
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

    # reactive container for image data
    # NOTE: if rxs images with comments provided, save progress on handled comments to this
    images_edited <- shiny::reactiveVal(NULL)

    # reactive containers for currently selected trace / image / year in the
    # plot(s) and table
    sel_woodpiece <- shiny::reactiveVal(NULL)
    sel_image <- shiny::reactiveVal(NULL)
    sel_marker <- shiny::reactiveVal(NULL)

    # reactive containers for plot settings and data
    sel_subplots <- shiny::reactiveVal(c("sd","cov")) # default: show optional subplots
    awaiting_restoration <- shiny::reactiveVal(FALSE)
    shift_ring <- shiny::reactiveVal(NULL)
    shift_image <- shiny::reactiveVal(NULL)

    # reactive containers for table settings and data
    sel_hot_cols <- shiny::reactiveVal(list(
      disqual = disqual_issues,
      techn = technical_issues,
      other = other_issues,
      discrete = discrete_features
    ))
    excl_flags <- shiny::reactiveVal(NULL)
    current_hot_image <- shiny::reactiveVal(NULL)

    # reactive containers for saving edited rings data
    save_settings <- shiny::reactiveValues(
      not_set = TRUE,
      save_to = NULL,
      filepath = NULL,
      varname = NULL,
      save_imgs_to = NULL,
      filepath_rxsmeta = NULL,
      varname_rxsmeta = NULL,
    )

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
            path_rxsmeta = input$file_rxsmeta$datapath,
            name_rxsmeta = input$file_rxsmeta$name
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

    # initialize rings_data_org with input rings data
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

    # initialize editable copy of rings_data_org
    shiny::observe({
      rings_data_edited(rings_data_org())
    }) |> shiny::bindEvent(rings_data_org())

    # if there are comments to handle, initialize images_edited with input rxsmeta data
    # and show the progress tab, else hide it
    shiny::observe({
      bslib::nav_hide("tabs", target = "comments_panel", session = main_session)

      shiny::req(input_data$rxsmeta_data)
      df_comments <- input_data$rxsmeta_data
      
      # check for comments
      if (!"comment" %in% names(df_comments)) {
        df_comments$comment <- NA
      }
      df_comments <- df_comments |>
        dplyr::filter(!is.na(comment) & comment != "")
      
      if (nrow(df_comments)>0){
        # initialize/validate comment_handled column
        if (!"comment_handled" %in% names(df_comments)){
          df_comments$comment_handled <- FALSE
        }
        df_comments$comment_handled[is.na(df_comments$comment_handled)] <- FALSE

        df_comments <- df_comments |> 
          dplyr::select(
            dplyr::any_of(c("site_label", "species_code", "tree_label", "woodpiece_label", "slide_label")),
            image_label, comment, comment_handled
          )
        images_edited(df_comments)
        bslib::nav_show("tabs", target = "comments_panel", session = main_session)
      } else {
        bslib::nav_hide("tabs", target = "comments_panel", session = main_session)
      }
    }) |> shiny::bindEvent(input_data$rxsmeta_data, ignoreNULL = FALSE)


    # UI CHANGES GIVEN INPUT DATA:
    # update sidebar UI based on loaded input data
    shiny::observe({
      shiny::req(input_data$rings_data)
      prf_data <- input_data$prf_data
      rings_data <- input_data$rings_data
      rxsmeta_data <- input_data$rxsmeta_data

      # site and species select inputs
      if (!is.null(rxsmeta_data)) {
        shinyjs::show("filt_site")
        
        site_choices <- unique(rxsmeta_data$site_label)
        filt_sites <- site_choices[1]
        
        # species column is opt, only show if we have it
        if ("species_code" %in% names(rxsmeta_data)) {
          shinyjs::show("filt_species")

          species_choices <- unique(rxsmeta_data$species_code)
          shiny::updateSelectInput(session, "filt_species",
                                  choices = species_choices,
                                  selected = species_choices[1])
          # update preselected sites accordingly
          filt_sites <- rxsmeta_data |>
            dplyr::filter(species_code == species_choices[1]) |>
            dplyr::pull(site_label) |>
            unique()

        } else {
          shinyjs::hide("filt_species")
        }
        shiny::updateSelectInput(session, "filt_site",
                                 choices = site_choices,
                                 selected = filt_sites)
      } else {
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
      shiny::req(input_data$rxsmeta_data, input$filt_site)
      wp_choices <- input_data$rxsmeta_data |>
        dplyr::filter(
          site_label %in% input$filt_site,
        ) 
      if (shiny::isTruthy(input$filt_species)) {
        wp_choices <- wp_choices |>
          dplyr::filter(species_code %in% input$filt_species)
      }
      wp_choices <- wp_choices |>
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
    ## UI related ----
    # UI: Plot card header with dynamic woodpiece selection input
    output$selwp <- shiny::renderUI({
      shiny::req(input$filt_wp)
      wp_choices_filt <- input$filt_wp
      shiny::selectInput(
        ns("sel_wp_trace"), NULL,
        choices = wp_choices_filt,
        selected = wp_choices_filt[1],
        selectize = TRUE, multiple = FALSE,
        width = "auto"
      )
    })

    # choosing a different woodpiece via the input: reset marker and image
    # guard: if sel_woodpiece is already set to this value, it came from a plot
    # click (which set sel_woodpiece directly), so skip the reset
    shiny::observe({
      if (!identical(sel_woodpiece(), input$sel_wp_trace)) {
        sel_woodpiece(input$sel_wp_trace)
        sel_marker(NULL)
        sel_image(NULL)
      }
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
      shiny::req(df_selwp())
      height <- compute_plot_height(sel_subplots())
      shiny::div(
        style = paste0("height: ", height),
        plotly::plotlyOutput(ns("main_plot"), height = "100%")
      )
    })

    ## data prepraration ----
    # define REACTIVES as basis for plot(s)
    # create a color palette for the plot based on the max. nr of traces
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


    ## plot creation ----
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
        fig <- fig |> htmlwidgets::onRender(js_sync_hover)
      }
      fig <- fig |> htmlwidgets::onRender(js_traces, data = ns("traces_crn"))

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
    # (could set inside renderPlotly, but want to ignore init)
    shiny::observe({
      awaiting_restoration(TRUE)
    }) |> shiny::bindEvent(df_selwp(), df_otherwps(), df_sd(), df_cov(),
                           ignoreInit = TRUE)

    # restore state of plot
    # NOTE to self: tried with other triggers rather than traces_crn, e.g. a
    # render_counter(), onFlushed, ... but this seems to be the only way to
    # achieve the intended effect
    shiny::observe({
      shiny::req(awaiting_restoration())
      shiny::req(input$traces_crn) # require that input$traces_crn is available (->plot render complete)

      cat("... restoring state:\n")

      current_traces <- input$traces_crn

      p <- plotly::plotlyProxy("main_plot", session)

      # if we have axes limits from previous render, reset them
      # TODO: maybe also y if param not changed?
      x_axes <- crn_x_axes()
      if (!is.null(x_axes) &&
           !is.null(x_axes$x_min) &&
             !is.null(x_axes$x_max)) {
        cat("... restoring x axes limits\n")
        p <- p |>
          plotly::plotlyProxyInvoke(
            method = "relayout",
            list(
              "xaxis.range[0]" = x_axes$x_min,
              "xaxis.range[1]" = x_axes$x_max
            )
          )
      } else {
        cat("... no axes limits to restore\n")
      }

      if (input$show_mean) {
        cat("... redrawing mean trace\n")
        df_data <- df_crn() |> dplyr::filter(!exclude_dupl)
        sel_mean <- input$sel_mean
        df_mean <- calc_mean_vals(df_data, sel_mean)
        p <- draw_mean_trace(p, df_mean$year, df_mean$vals, sel_mean)
      } else {
        cat("... no mean trace to draw\n")
      }

      if (shiny::isTruthy(sel_marker())) {
        cat("... redrawing sel ring markers\n")
        marker <- sel_marker()
        sel_wp <- sel_woodpiece()
        if (marker$wp_label == sel_wp) {
          # get up-to-date ywp_val and color in case of sel param, detrend, show_excl, apply_changes
          new_marker <- update_marker_info(marker, df_selwp(), input$sel_param)
          sel_marker(new_marker)
          p <- draw_sel_marker(p, new_marker, current_traces, sel_subplots())
        } else {
          # if filtered wps or sel_wp were changed such that marker is no longer on sel trace -> remove
          cat("... ... new sel wp trace, reset marker\n")
          sel_marker(NULL)
          sel_image(NULL)
        }
      } else {
        cat("... no sel marker(s) to draw\n")
      }

      cat("... redrawing excluded markers\n")
      excl_markers <- get_excl_markers(sel_woodpiece(), df_selwp(), rings_data_edited())
      p <- draw_excl_markers(p, excl_markers, current_traces)

      cat("... rerender complete\n")
      awaiting_restoration(FALSE) # only do it once

      p

    }) |> shiny::bindEvent(input$traces_crn, ignoreNULL = TRUE, ignoreInit = TRUE)


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
        shiny::showModal(
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
      print(click_data)
      new_marker <- resolve_click_marker(click_data, df_otherwps(), input$sel_param)

      sel_marker(new_marker)
      sel_image(new_marker$ycov_val)
      sel_woodpiece(new_marker$wp_label)
      # tirggers plot rerender via sel_woodpiece() > df_selwp(), df_otherwps() > plot

      # sync the UI input without triggering the sel_wp_trace observer's reset logic
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
        shiny::showNotification(
          glue::glue("No {ifelse(direction=='next','later','earlier')} year available for this woodpiece."),
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


    ## image shifts -------------------------------------------------------------
    # capture and track directions of image shift
    shiny::observe({
      cat(".   shift image next\n")
      shift_image(list(
        dir = "next",
        nonce = sample(1e6, 1)
      ))
    }) |> shiny::bindEvent(input$next_img)

    shiny::observe({
      cat(".   shift image prev\n")
      shift_image(list(
        dir = "prev",
        nonce = sample(1e6, 1)
      ))
    }) |> shiny::bindEvent(input$prev_img)

    # update the selected image after shift
    shiny::observe({
      shiny::req(sel_image())
      direction <- shift_image()$dir

      img_order <- df_selwp() |> 
        dplyr::slice_min(year, n=1, by = "image_label", with_ties = FALSE) |> 
        dplyr::arrange(year) |> dplyr::pull(image_label)
      idx_current <- match(sel_image(), img_order)
      idx_new <- if (direction == "next") idx_current + 1L else idx_current - 1L

      if (is.na(idx_new) || idx_new < 1L || idx_new > length(img_order)) {
        shiny::showNotification(
          glue::glue("No {ifelse(direction=='next','later','earlier')} image available for this woodpiece."),
          type = "warning"
        )
        return(NULL)
      }

      # update sel_image, reset sel_marker and remove marker from plot
      sel_image(img_order[[idx_new]])
      sel_marker(NULL)
      p <- plotly::plotlyProxy("main_plot", session)
      clear_sel_marker(p, input$traces_crn, sel_subplots())

    }) |> shiny::bindEvent(shift_image(), ignoreNULL = TRUE, ignoreInit = TRUE)


    ## hot row changes ---------------------------------------------------------
    shiny::observe({
      # require single selected?
      shiny::req(flags_out())
      sel_wp <- sel_woodpiece()
      sel_img <- sel_image()
      new_year <- as.integer(rownames(flags_out())[input$sel_hot_row])

      if (shiny::isTruthy(sel_marker()) && sel_marker()$year == new_year) {
        return(NULL) # same row
      }

      new_marker <- resolve_row_marker(sel_wp, sel_img, new_year, df_selwp(), input$sel_param)

      sel_marker(new_marker)
      # no update to sel_image required

      # redraw sel marker(s)
      p <- plotly::plotlyProxy("main_plot", session)
      draw_sel_marker(p, new_marker, input$traces_crn, sel_subplots())

    }) |> shiny::bindEvent(input$sel_hot_row, ignoreNULL = TRUE)


    ## x axis changes ----------------------------------------------------------
    # capture axis limit changes for the x axis via plotly relayout events
    crn_change_axes <- shiny::reactive({
      shiny::req(df_selwp())
      plotly::event_data("plotly_relayout",
                         source = "crn_plot")
    })

    crn_x_axes <- shiny::reactiveVal(NULL)

    shiny::observe({
      relayout <- crn_change_axes()

      # ignore events that don't affect the x-axis (e.g. yaxis changes)
      x_autorange <- !is.null(relayout[["xaxis.autorange"]])
      has_x_range  <- !is.null(relayout[["xaxis.range[0]"]])
      shiny::req(x_autorange || has_x_range)

      if (x_autorange) {
        x_axes <- list(x_min = NULL, x_max = NULL)
      } else {
        x_axes <- list(
          x_min = relayout[["xaxis.range[0]"]],
          x_max = relayout[["xaxis.range[1]"]]
        )
      }

      crn_x_axes(x_axes)
    }) |> shiny::bindEvent(crn_change_axes())


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
        p <- p |> plotly::plotlyProxyInvoke("deleteTraces", crn_curveNumber)
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

    ## correlation info in card footer -----------------------------------------
    output$corr_info_ui <- shiny::renderUI({
      # compute and display correlation between selected woodpiece trace
      # and the (selected) mean, within the current x-axis range
      msg <- calculate_correlation(df_selwp(), df_crn(), crn_x_axes(), input$sel_mean)
      shiny::HTML(msg)
    })


    # FLAGS TABLE --------------------------------------------------------------
    # load js callbacks as strings
    js_escape <- readLines("www/js/escape_keybinding.js") |>
      paste(collapse = "\n")

    js_selrow <- readLines("www/js/selrow_to_input.js") |>
      paste(collapse = "\n")

     # add selected row tracking to hot
    
    # UI: hot card header title with dynamic selected image
    output$selimg <- shiny::renderUI({
      shiny::req(sel_image())
      shiny::strong(sel_image())
    })

    # centered comment + handled checkbox in hot card header, if comment exists
    output$selcomment <- shiny::renderUI({
      shiny::req(sel_image())
      shiny::req(shiny::isTruthy(images_edited()))
      img_comment <- images_edited() |>
        dplyr::filter(image_label == sel_image())
      shiny::req(nrow(img_comment) > 0)

      shiny::div(
        style = "display: flex; align-items: center; gap: 8px;",
        shiny::em(glue::glue("Comment: {img_comment$comment}")),
        shiny::checkboxInput(ns("comment_handled"), "handled",
                             value = img_comment$comment_handled)
      )
    })

    # update images_edited()$comment_handled when user clicks the checkbox
    shiny::observe({
      shiny::req(images_edited(), sel_image())
      df <- images_edited()
      img <- sel_image()
      df[df$image_label == img,"comment_handled"] <- input$comment_handled
      images_edited(df)
    }) |> shiny::bindEvent(input$comment_handled, ignoreInit = TRUE, ignoreNULL = TRUE)


    # UI: show modal when clicking on table settings
    shiny::observe({
      shiny::showModal(
        hot_settings_modal(ns, sel_hot_cols())
      )
    }) |> shiny::bindEvent(input$tbl_settings)

    # update selected table columns based on modal inputs
    shiny::observe({
      shiny::removeModal()
      sel_hot_cols(
        list(
          disqual = intersect(input$sel_cols_issues, disqual_issues),
          techn = intersect(input$sel_cols_issues, technical_issues),
          other = intersect(input$sel_cols_issues, other_issues),
          discrete = intersect(input$sel_cols_features, discrete_features)
        )
      )
    }) |> shiny::bindEvent(input$confirm_cols)

    # prepare reactive as basis for hot
    df_rings_hot <- shiny::reactive({
      shiny::req(sel_image())
      sel_disq <- unname(sel_hot_cols()$disqual)
      sel_techn <- unname(sel_hot_cols()$techn)
      sel_other <- unname(sel_hot_cols()$other)
      sel_feat <- unname(sel_hot_cols()$discrete)
      rings_data_edited() |>
        dplyr::filter(image_label == sel_image()) |>
        dplyr::select(
          year,
          dplyr::all_of(c(
            "duplicate_ring","exclude_dupl",
            "exclude_issues","affected_tissue",
            sel_disq, sel_techn, sel_other,
            sel_feat,"comment"))
        ) |>
        tibble::column_to_rownames("year")
    }) |> shiny::bindEvent(sel_image(), input$confirm_cols)

    # render the HOT
    # reactive to df_rings_hot() < sel_image(), sel_hot_cols() changes
    output$img_flags <- rhandsontable::renderRHandsontable({
      df_rings <- df_rings_hot()

      current_hot_image(sel_image())
      if (shiny::isTruthy(sel_marker())){
        selring_idx <- which(rownames(df_rings) == sel_marker()$year) - 1 
      } else {
        selring_idx <- -1
      }
      
      ro_ids_dupl <- which(rep(TRUE, nrow(df_rings))) - 1
      ro_ids_excldupl <- which(!df_rings$duplicate_ring) - 1
      warn_col_ids <- which(names(df_rings) %in% disqual_issues) - 1

      hot_height <- min(nrow(df_rings) * 23L + 26L, 400L) + 5L

      hot <- rhandsontable::rhandsontable(
        df_rings,
        stretchH = "all",
        contextMenu = FALSE,
        height = hot_height
      ) |> # style the mandatory columns
      rhandsontable::hot_col(
        "duplicate_ring",
        type = "checkbox", halign = "htCenter",
        renderer = renderer_cb_ro(ro_ids_dupl, hot_color_dupl)
      ) |>
      rhandsontable::hot_col(
        "exclude_dupl",
        type = "checkbox", halign = "htCenter",
        renderer = renderer_cb_dupl(ro_ids_excldupl, hot_color_dupl)
      ) |>
      rhandsontable::hot_col(
        "exclude_issues",
        type = "checkbox", halign = "htCenter",
        renderer = renderer_cb_val(warn_col_ids, hot_color_excl)
      ) |>
      rhandsontable::hot_col(
        "affected_tissue",
        type = "dropdown", source = c("", "all", "ew", "lw"),
        renderer = renderer_dd(hot_color_excl)
      ) |>
      rhandsontable::hot_col(
        "comment",
        renderer = renderer_txt(hot_color_iss1)
      )

      # style the optional flag columns depending on which are shown
      n_opt_cols <- purrr::map(sel_hot_cols(), length)
      opt_col_colors <- rep(c(hot_color_iss1, hot_color_iss2,
                          hot_color_iss1, hot_color_iss2),
                        times = n_opt_cols)
      all_opt_cols <- purrr::list_c(sel_hot_cols())
      names(opt_col_colors) <- unname(all_opt_cols)

      if (length(all_opt_cols) > 0) {
        hot <- purrr::reduce(
          all_opt_cols, # need names in df
          function(ht, col) {
            ht |> rhandsontable::hot_col(col, type = "checkbox", halign = "htCenter",
                                         renderer = renderer_cb(opt_col_colors[col]))
          },
          .init = hot
        )
      }

      hot |>
        rhandsontable::hot_cols(colWidths = 25) |>
        # add esc key functionality to table
        htmlwidgets::onRender(js_escape) |>
        # keep track of user selected row in table
        htmlwidgets::onRender(js_selrow,
                              data = list(
                                selrow_idx = selring_idx,
                                input_id = ns("sel_hot_row")))

    }) |> shiny::bindEvent(df_rings_hot(), ignoreNULL = TRUE)

    # HOT REACTIVITY -----------------------------------------------------------

    ## update highlighted row --------------------------------------------------
    # if selected marker is updated but still in same image, update highlighted row
    shiny::observe({
      shiny::req(sel_marker())
      shiny::req(identical(sel_marker()$ycov_val, current_hot_image()))
      # get the row index from current table data
      df <- df_rings_hot()
      new_idx <- which(rownames(df) == sel_marker()$year) - 1

      shinyjs::runjs(sprintf("
        var widget = HTMLWidgets.find('#%s');
        if (widget && widget.hot) {
          widget.hot._highlightedRow = %s;
          widget.hot.render();
        }
      ", ns("img_flags"), new_idx))
    }) |> shiny::bindEvent(sel_marker(), ignoreNULL = TRUE)


    ## capture hot edits -------------------------------------------------------
    # reactive to capture hot edits
    flags_out <- shiny::reactive({
      shiny::req(input$img_flags)
      # rhandsontable may pass values in checkbox columns as JSON strings ("true"/"false")
      # coerce back to logical for R
      all_cols <- c(input_specs$rings_data$req_cols, input_specs$rings_data$opt_cols)
      logical_col_names <- names(all_cols[all_cols == "l"])
      rhandsontable::hot_to_r(input$img_flags) |>
        dplyr::mutate(dplyr::across(dplyr::any_of(logical_col_names), as.logical))
    })

    # update rings_data_edited with the edited flags from the hot
    shiny::observe({
      sel_img <- sel_image()
      sel_wp <- sel_woodpiece()

      df_rings <- rings_data_edited()

      df_flags <- flags_out() |>
        tibble::rownames_to_column("year") |>
        dplyr::mutate(year = as.integer(year),
                      image_label = sel_img)

      # make corresponding changes in overlapping images for duplicate ring updates
      df_flags_w_dupl <- get_dupl_updates(df_flags, df_rings, sel_img, sel_wp)

      # update the flags in the rings data frame
      df_rings_new <- df_rings |>
        dplyr::rows_update(df_flags_w_dupl, by = c("image_label", "year"))

      rings_data_edited(df_rings_new)
    }) |> shiny::bindEvent(flags_out(), ignoreInit = TRUE, ignoreNULL = TRUE)

  
    ## enter key to select cell ------------------------------------------------
    shiny::observe({
      shiny::req(sel_image(), flags_out())
      df_hot <- flags_out()

      if (shiny::isTruthy(sel_marker())) {
        # marker exists: select its row in HOT
        marker <- sel_marker()
        row_idx <- which(rownames(df_hot) == as.character(marker$year))
        if (length(row_idx) == 0) return(NULL)
      } else {
        # no marker: select first row and set sel_marker
        row_idx <- 1L
        first_year <- as.integer(rownames(df_hot)[[1L]])
        new_marker <- resolve_row_marker(
          sel_woodpiece(), sel_image(), first_year, df_selwp(), input$sel_param
        )
        sel_marker(new_marker)
        p <- plotly::plotlyProxy("main_plot", session)
        draw_sel_marker(p, new_marker, input$traces_crn, sel_subplots())
      }

      # select the cell in the 3rd column (i.e. exclude_issues)
      shinyjs::runjs(sprintf("
        var hot = HTMLWidgets.find('#%s');
        if (hot && hot.hot) {
          hot.hot.selectCell(%d, 2);
          hot.hot.scrollViewportTo(%d, 2);
        }
      ", ns("img_flags"), row_idx - 1L, row_idx - 1L))
    }) |> shiny::bindEvent(input$enter_key)


    ## apply edits to plot -----------------------------------------------------
    # clicking the apply button updates rings_data_org() > df_crn() > plot rerender
    shiny::observe({
      shiny::req(rings_data_edited())
      rings_data_org(rings_data_edited())
    }) |> shiny::bindEvent(input$apply_changes)


    # GOTO IMG FROM COMMENTS TAB -----------------------------------------------
    shiny::observe({
      shiny::req(comments_out$goto_img())
      clicked_img <- comments_out$goto_img()
      prev_img <- sel_image()

      if (!shiny::isTruthy(prev_img) || prev_img != clicked_img$image){
        sel_image(clicked_img$image)
        crn_x_axes(NULL)
        sel_marker(NULL)

        prev_wp <- sel_woodpiece()
        if (prev_wp != clicked_img$woodpiece) {
          # causes plot rerender
          shiny::updateSelectInput(session, "sel_wp_trace", selected = clicked_img$woodpiece)
        } else {
          p <- plotly::plotlyProxy("main_plot", session)
          clear_sel_marker(p, input$traces_crn, sel_subplots())
        }
      }
     
    }) |> shiny::bindEvent(comments_out$goto_img())


    # OPEN IMAGES --------------------------------------------------------------
    # when button is clicked
    shiny::observe({
      shiny::req(sel_image(), input_data$rxsmeta_data)
      sel_img <- sel_image()

      image_path <- input_data$rxsmeta_data |>
        dplyr::filter(image_label == sel_img) |>
        dplyr::pull(fname_image)

      org_img_name <- input_data$rxsmeta_data |>
        dplyr::filter(image_label == sel_img) |>
        dplyr::pull(org_img_name)

      base_path <- dirname(image_path)

      # Check for annotated (twin) image(s) in same folder
      annotated_twin <- list.files(
        base_path,
        pattern = paste0(org_img_name, "_annotated_twin\\."),
        full.names = TRUE
      )
      annotated_image <- list.files(
        base_path,
        pattern = paste0(org_img_name, "_annotated\\."),
        full.names = TRUE
      )

      # Prefer annotated > twin > image file if found
      if (length(annotated_image) == 1) {
        file_to_open <- annotated_image
      } else if (length(annotated_twin) == 1) {
        file_to_open <- annotated_twin
      } else {
        file_to_open <- image_path
      }

      if (file.exists(file_to_open)) {
        browseURL(file_to_open)
      } else {
        shiny::showNotification(
          glue::glue("The following image could not be opened: {file_to_open}"),
          type = "error"
        )
      }

    }) |> shiny::bindEvent(input$show_image, ignoreNULL = TRUE)

    # auto open if sel_image changes if option is TRUE
    shiny::observe({
      shiny::req(input$auto_open_image)
      cat(glue::glue(".   auto open image {sel_image()}"), "\n")
      # trigger image open button when sel_image changes
      shinyjs::click("show_image")
    }) |> shiny::bindEvent(sel_image(), ignoreNULL = TRUE)


    # SAVING DATA -----------------------------------------------------------
    # open save settings modal when gear icon clicked
    shiny::observe({
      shiny::req(input_data$rings_data)
      have_comments <- shiny::isTruthy(images_edited())
      shiny::showModal(save_modal(ns, save_settings, have_comments))
    }) |> shiny::bindEvent(input$save_settings_btn)

    # open save settings modal when save button is clicked for the first time
    # i.e. save_settings$not_set is still TRUE
    shiny::observe({
      shiny::req(save_settings$not_set, input_data$rings_data)
      have_comments <- shiny::isTruthy(images_edited())
      shiny::showModal(save_modal(ns, save_settings, have_comments))
    }) |> shiny::bindEvent(input$save_btn)

    # conditional logics for the modal UI
    shiny::observe({
      is_file <- "file" %in% input$modal_save_to
      shinyjs::toggleState("modal_filepath", condition = is_file)
      shinyjs::toggleState("modal_varname", condition = "env" %in% input$modal_save_to)
      shinyjs::toggleState("save_confirm", condition =
        (is_file && shiny::isTruthy(input$modal_filepath)) ||
          (("env" %in% input$modal_save_to) && shiny::isTruthy(input$modal_varname))
      )
    }) |> shiny::bindEvent(input$modal_save_to,
                           input$modal_filepath,
                           input$modal_varname, ignoreNULL = FALSE)
    
    # on confirm save in modal, update settings and save data
    shiny::observe({
      save_settings$not_set <- FALSE
      save_settings$save_to <- input$modal_save_to
      save_settings$filepath <- input$modal_filepath
      save_settings$varname <- input$modal_varname
      save_settings$save_imgs_to <- input$modal_saveimg_to
      save_settings$filepath_rxsmeta <- input$modal_filepath_imgs
      save_settings$varname_rxsmeta <- input$modal_varname_imgs
      shiny::removeModal()
      shiny::req(rings_data_edited())
      df_edited <- rings_data_edited()
      df_org <- input_data$rings_data |> init_char_columns()
      save_ring_edits(df_edited, df_org, save_settings,
                      rxsmeta = input_data$rxsmeta_data,
                      handled = images_edited())
    }) |> shiny::bindEvent(input$save_confirm)

    shiny::observe({
      settings_set <- !save_settings$not_set
      shiny::req(settings_set, rings_data_edited())
      df_edited <- rings_data_edited()
      df_org <- input_data$rings_data |> init_char_columns()
      save_ring_edits(df_edited, df_org, save_settings,
                      rxsmeta = input_data$rxsmeta_data,
                      handled = images_edited())
    }) |> shiny::bindEvent(input$save_btn)
      

    # # DEBUG OUTPUT -------------------------------------------------------------
    output$debug <- shiny::renderPrint({
      #sel_subplots()
      #flags_out()
      #shiny::req(save_settings$initialized)
      #images_edited()
      #comments_out$goto_img()
      #df <- traces_to_df(input$traces_crn)
      #tail(df)
      #rings_data_org()
      #input$enter_key
      #str(input_data$rings_data)

    })


    # return module exports
    list(
      images_edited = images_edited
    )

  })
}
