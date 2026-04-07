# SERVER -----------------------------------------------------------------------
site_server <- function(id, main_session, images_in, site_tbls_in, countries_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mock event to close map tab on start of app
    shiny::observe({
      bslib::accordion_panel_close(id = 'map_acc', values = TRUE)
    }) |> shiny::bindEvent(1) # to fire the event once at startup

    # TODO: remove once table import properly implemented
    shinyjs::disable(id = "file_sites")
    shinyjs::disable(id = "file_trees")
    shinyjs::disable(id = "file_wps")
    shinyjs::disable(id = "file_slides")

    # initialize reactiveVals (responding to changes in input data)
    site_data_in <- shiny::reactiveVal(NULL)
    tree_data_in <- shiny::reactiveVal(NULL)
    wp_data_in <- shiny::reactiveVal(NULL)
    slide_data_in <- shiny::reactiveVal(NULL)

    shiny::observe({
      sites_in <- site_tbls_in()$sites
      if (!is.null(sites_in$country_code)) {
        sites_in$country_code <- iso_to_combined(sites_in$country_code)
      }
      site_data_in(sites_in)
      tree_data_in(site_tbls_in()$trees)
      wp_data_in(site_tbls_in()$woodpieces)
      slide_data_in(site_tbls_in()$slides)
    }) |> shiny::bindEvent(site_tbls_in())

 
    # SITE MAP -----------------------------------------------------------------
    # site coordinates reactiveVal, updates IFF coord cols in site_data_out change
    site_coordinates <- reactiveVal(NULL)

    observeEvent(site_data_out(),{
      # TODO: read valid range from tbl schema?
      min_lng <- -180
      max_lng <- 180
      min_lat <- -90
      max_lat <- 90

      # get valid pairs of coordinates from table
      site_coords <- site_data_out()
      site_coords <- site_coords %>%
        dplyr::select(longitude, latitude) %>%
        dplyr::mutate(
          longitude = suppressWarnings(as.numeric(longitude)),
          latitude = suppressWarnings(as.numeric(latitude))
        ) %>%
        dplyr::mutate(
          longitude = ifelse(longitude < min_lng | longitude > max_lng, NA, longitude),
          latitude = ifelse(latitude < min_lat | latitude > max_lat, NA, latitude)
        ) %>%
        dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
        dplyr::distinct()

      # update reactive
      site_coordinates(site_coords)
    }, ignoreInit = TRUE)

    # render Leaflet map
    output$site_map <- leaflet::renderLeaflet({
      # start map on WSL coordinates
      sitemap <- leaflet::leaflet() %>%
        leaflet::setView(lng = 8.44256, lat = 47.35515, zoom = 8) %>%
        leaflet::addTiles()

      # add markers for all sites in the table
      site_coords <- site_coordinates()

      # if we have both lat and longitude, add markers and zoom to new bounds
      if (!is.null(site_coords) && nrow(site_coords)>0) {
        sitemap <- sitemap %>% leaflet::addMarkers(lng = site_coords$longitude, lat = site_coords$latitude) %>%
          leaflet::fitBounds(lng1 = min(site_coords$longitude)-5, lng2 = max(site_coords$longitude)+5,
                             lat1 = min(site_coords$latitude)-5, lat2 = max(site_coords$latitude)+5)
      }

      sitemap
    })

    # SITE TABLE ---------------------------------------------------------------
    # render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(site_data_in()), "No data to show"))

      schema_path <- system.file("extdata", "json_schema/20251007_tria_site_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(site_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)


      n_rows <- nrow(site_data_in())
      ht_height <- min(max(n_rows * ht_row_height, ht_min_height), ht_max_height)

      rhandsontable::rhandsontable(
        site_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = ht_height,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- tbl_props[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    site_data_out <- reactive({
      rhandsontable::hot_to_r(input$site_table)
    })

    site_data_export <- reactive({
      df <- site_data_out()
      if (!is.null(df$country_code)) {
        df$country_code <- combined_to_iso(df$country_code)
      }
      df
    })

    # update country column in site data based on coordinates
    observeEvent(site_coordinates(),{
      site_coords <- site_coordinates()
      if (nrow(site_coords) > 0) {
        current_df <- site_data_out()
        site_coords$iso_codes <- country_from_coords(lng = site_coords$longitude, lat = site_coords$latitude, countries_sf)
        countries_list <- setNames(
          countries_info$country_iso_code,
          countries_info$combined
        )
        site_coords$new_country <- ifelse(
          is.na(site_coords$iso_codes) | site_coords$iso_codes == "-99",
          "",  # Assign empty string for NA values
          names(countries_list)[match(site_coords$iso_codes, countries_list)]
        )

        # update country column in site data
        current_df <- current_df %>% dplyr::left_join(site_coords, by = c('longitude', 'latitude')) %>%
          dplyr::mutate(country_code = new_country) %>%
          dplyr::select(-iso_codes, -new_country)
        site_data_in(current_df)
      }

    }, ignoreInit = TRUE)

    # import data from file: show modal for confirmation
    observeEvent(input$file_sites,{
      show_ht_import_modal(ns, 'import_site_data')
    })

    # observe confirm import data button
    observeEvent(input$import_site_data, {
      removeModal()
      # safe_block({
      #   imported <- load_single_table(input$file_sites$datapath, 'site_data', site_tbl,
      #                                 force_required = FALSE, col_names = input$col_names, skip = input$skip)
      #
      #   # match with the inferred structure from df_meta
      #   df_meta <- start_info$input_meta$df %>%
      #     dplyr::group_by(site) %>%
      #     dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
      #     dplyr::rename(site_code = site, n_trees = n)
      #
      #   # if imported (site_code, n_trees) is a true subset of df_meta, then update site_data_in
      #   matched_rows <- nrow(dplyr::inner_join(imported, df_meta, by = c('site_code', 'n_trees')))
      #
      #   # TODO: can n_trees be NA?
      #   if (nrow(imported) > 0 && matched_rows > 0) {
      #     if ((matched_rows < nrow(imported)) || (matched_rows < nrow(df_meta))) {
      #       warning("Some site_codes could not be matched or found.")
      #     }
      #     df_site <- df_meta |>
      #       dplyr::left_join(imported, by = c('site_code', 'n_trees'))
      #     site_data_in(df_site)
      #   } else {
      #     stop("No valid site_codes found.")
      #   }
      # },
      # err_title = "Error loading data", err_message = "An error occurred while loading data:",
      # propagate_err = FALSE,
      # warn_message = "A warning occurred while loading data:"
      # )
    })


  # site networks ---
  # TODO: reset site networks if data is reloaded / df_meta or site_data_in are changed
    observeEvent(input$btn_add_nws, {
      site_labels <- site_data_out() %>% dplyr::pull(site_label)
      showModal(modalDialog(
        title = "Add site network",
        tagList(
          textInput(ns("nw_name"), "Network name", value = NA,
                    placeholder = "Specify a name for the network (max 64 char.)"),
          textAreaInput(ns("nw_desc"), "Network description", rows = 4,
                    placeholder = "Provide a brief description of what characterises the site network."),
          selectizeInput(
            inputId = ns("sel_sites"),
            label = "Choose the sites:",
            choices = site_labels,
            multiple = TRUE)
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_nw"), "Add")
        )
      ))
    })

    nw_data_in <- reactiveVal(data.frame(
      Name = character(0),
      Description = character(0),
      Sites = character(0),
      stringsAsFactors = FALSE
    ))

    observeEvent(input$btn_trans_nw, {
      # check if all fields are filled
      validate(need(input$nw_name != "", "Please provide a name for the network"))
      validate(need(input$nw_desc != "", "Please provide a description for the network"))
      validate(need(length(input$sel_sites) > 0, "Please select at least one site"))

      # create new row
      new_row <- data.frame(
        Name = input$nw_name,
        Description = input$nw_desc,
        Sites = paste(input$sel_sites, collapse = ", "),
        stringsAsFactors = FALSE
      )

      # add to existing data
      current_df <- nw_data_in()
      current_df[nrow(current_df)+1,] <- new_row
      nw_data_in(current_df)

      removeModal()
    })

    # TODO:
    # add validator rules for network inputs
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("nw_name", shinyvalidate::sv_required())
    iv$add_rule("nw_name", max_char_limit, limit = 64)
    # iv$add_rule(
    #   "ds_name",
    #   shinyvalidate::sv_regex("^[a-zA-Z0-9]*$", "Only alphanumeric characters allowed")
    # )
    iv$add_rule("nw_desc", shinyvalidate::sv_required())
    iv$add_rule("sel_sites", shinyvalidate::sv_required())

    iv$enable() # TODO: enable from start?

    shiny::observe({
      shinyjs::toggleState(id = "btn_trans_nw",
                           condition = iv$is_valid())
    })

    output$networks <- DT::renderDataTable({
      req(nrow(nw_data_in()) > 0)

      deleteButtonColumn(nw_data_in(), 'delbtn', ns)

    })

    # observe delete row events
    observeEvent(input$deletePressed, {
      rowNum <- parseDeleteEvent(input$deletePressed)
      # Delete the row from the data frame
      current_df <- nw_data_in()
      current_df <- current_df[-rowNum,]
      if (nrow(current_df) > 0) {
        rownames(current_df) <- 1:nrow(current_df) # update rownames
      }
      nw_data_in(current_df)
    })


    # TREE TABLE ---------------------------------------------------------------
    output$tree_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(tree_data_in()), "No data to show"))

      schema_path <- system.file("extdata", "json_schema/20251007_tria_tree_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(tree_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      n_rows <- nrow(tree_data_in())
      ht_height <- min(max(n_rows * ht_row_height, ht_min_height), ht_max_height)

      rhandsontable::rhandsontable(
        tree_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = ht_height,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- tbl_props[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    tree_data_out <- reactive({
      rhandsontable::hot_to_r(input$tree_table)
    })


    # update species info in tree table IFF speciesname or code are changed
    observeEvent(input$tree_table,{

      # TODO: loop through multiple changes at once
      table_changes <- input$tree_table$changes$changes
      current_df <- tree_data_out()
      update_tree_data <- FALSE

      for (k in length(table_changes)){
        row <- table_changes[[k]][[1]] # first index can be more than 1
        col <- table_changes[[k]][[2]]
        new_val <- table_changes[[k]][[4]]

        # col 3 is speciescode (0-index)
        if (!is.null(col) && col == 3) {
          # Look species name, woodtype and leaf habit of new species code
          new_species_info <- species_info[species_info$itrdb_species_code == new_val,]
          if (nrow(new_species_info) > 0) {
            current_df[row + 1, col + 2] <- new_species_info$species_name
            current_df[row + 1, col + 3] <- new_species_info$phylogenetic_group
            current_df[row + 1, col + 4] <- new_species_info$leaf_habit
            current_df[row + 1, col + 5] <- new_species_info$tree_ring_structure
            update_tree_data <- TRUE
          }
        }
        # col 4 is speciesname (0-index)
        if (!is.null(col) && col == 4) {
          # Look species name, woodtype and leaf habit of new species code
          new_species_info <- species_info[species_info$species_name == new_val,]
          if (nrow(new_species_info) > 0) {
            current_df[row + 1, col] <- new_species_info$itrdb_species_code
            current_df[row + 1, col + 2] <- new_species_info$phylogenetic_group
            current_df[row + 1, col + 3] <- new_species_info$leaf_habit
            current_df[row + 1, col + 4] <- new_species_info$tree_ring_structure
            update_tree_data <- TRUE
          }
        }
      }

      if (update_tree_data) {
        tree_data_in(current_df)
      }

    })#, ignoreInit = FALSE


    # WOODPIECE TABLE ----------------------------------------------------------
    output$wp_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(wp_data_in()), "No data to show"))

      schema_path <- system.file("extdata", "json_schema/20251007_tria_woodpiece_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(wp_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      n_rows <- nrow(wp_data_in())
      ht_height <- min(max(n_rows * ht_row_height, ht_min_height), ht_max_height)

      rhandsontable::rhandsontable(
        wp_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = ht_height,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- tbl_props[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    wp_data_out <- reactive({
      rhandsontable::hot_to_r(input$wp_table)
    })

    # SLIDE TABLE --------------------------------------------------------------
    output$slide_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(slide_data_in()), "No data to show"))

      schema_path <- system.file("extdata", "json_schema/20251007_tria_slide_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(slide_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      n_rows <- nrow(slide_data_in())
      ht_height <- min(max(n_rows * ht_row_height, ht_min_height), ht_max_height)

      rhandsontable::rhandsontable(
        slide_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = ht_height,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- tbl_props[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    slide_data_out <- reactive({
      rhandsontable::hot_to_r(input$slide_table)
    })



    # TODO: check configs, val functions, edge cases
    # VALIDATION CHECKS --------------------------------------------------------
    validation_checks <- reactive({
      results <- list()

      # 1) site table
      df_site <- site_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_site_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)
      results$site_data <- collect_hot_val_results(df_site, tbl_props)

      # 2) tree table
      df_tree <- tree_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_tree_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)
      results$tree_data <- collect_hot_val_results(df_tree, tbl_props)

      # 3) woodpiece table
      df_wp <- wp_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_woodpiece_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)
      results$woodpiece_data <- collect_hot_val_results(df_wp, tbl_props)

      # 4) slide table
      df_slide <- slide_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_slide_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)
      results$slide_data <- collect_hot_val_results(df_slide, tbl_props)


      # convert collected results to dataframe
      df_results <- results %>%
        purrr::map(~ .x %>%
                     purrr::map(~ tibble::tibble(
                       field = .x$field,
                       type = .x$type,
                       message = .x$message
                     )) %>%
                     purrr::list_rbind(names_to = 'fname')) %>%
        purrr::list_rbind(names_to = 'tname')

      df_results$topic <- input_field_names[df_results$tname]

      dplyr::bind_rows(
        data.frame(topic = character(0), field = character(0),
                   type = character(0), message = character(0)),
        df_results)
    })


    output$validation_check <- renderUI({
        df_validation <- validation_checks()

        if (nrow(df_validation) == 0) {
          return(tagList(strong("All checks passed", style = paste0('color: ', prim_col, ';'))))
        } else {
          # generate html lists for each topic
          html_output <- df_validation %>%
            dplyr::group_by(topic) %>%
            dplyr::summarise(
              content = paste0("<li>", field, ": ", message, "</li>", collapse = "")
            ) %>%
            dplyr::mutate(
              html = paste0("<b>", topic, ":</b><ul>", content, "</ul>")
            ) %>%
            dplyr::pull(html) %>%
            paste(collapse = "")

          return(HTML(html_output))

          # TODO:
          # add warning messages before switching tab or saving data

        }
    })


    # Next button
    observeEvent(input$btn_next, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_summary, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })


    # TODO:
    # SAVE BUTTON



    return(
      list(
        site_tbls = list(
          sites = site_data_export,
          trees = tree_data_out,
          woodpieces = wp_data_out,
          slides = slide_data_out
        ),
        val_check = validation_checks
      )
    )

  })
}


