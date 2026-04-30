# MODULE SERVER ----------------------------------------------------------------
start_server <- function(id, main_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive container for input data
    input_meta <- shiny::reactiveValues(
      source = "Please select input source and load data",
      images = NULL,
      dataset_tbls = NULL,
      site_tbls = NULL
    )

    # next button
    # toggle: enable only if we have input data and check_raw is checked
    shiny::observe({
      shinyjs::toggleState(
        id = "btn_next", condition = !is.null(input_meta$images))
    })
    # functionality: switch to next tab
    shiny::observeEvent(input$btn_next, {
      bslib::nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })


    # LOAD INPUT DATA ----------------------------------------------------------
    shiny::observeEvent(input$btn_input, {
      # warn before overwriting existing data
      if (!is.null(input_meta$images)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Warning",
            "This action overwrites any existing inputs provided in the app,
            including any data already provided in the other tabs.
            Are you sure you want to proceed?",
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("confirm_overwrite"), "Proceed")
            )
          )
        )
      } else {
        # input source modal
        show_input_modal(ns)
      }
    })

    # if overwrite confirmed, also show input source modal
    shiny::observeEvent(input$confirm_overwrite, {
      shiny::removeModal()
      show_input_modal(ns)
    })

    # ui to provide input data based on selected option
    output$load_details_ui <- shiny::renderUI({
      shiny::req(input$load_type)
      if (input$load_type == "env"){
        if (!server_run){
          shiny::textInput(ns("input_name"), "Provide the name of the object in your current R environment", value = "rxs_images")
        } else {
          shiny::tags$i("Only possible for local runs.")
        }
      } else if (input$load_type == "file") {
        shiny::fileInput(ns("input_file"), "Load data from file (.json, .csv)", accept = c(".json", ".csv"))
      } else { # example
        NULL
      }
    })

    shiny::observeEvent(input$confirm_input, {
      safe_block({
        if (input$load_type == "env") {
          if (is.null(input$input_name) || is.na(input$input_name) || input$input_name == "") {
            stop("Please provide input source.")
          } else {
            res <- get_from_env(input$input_name, envir = .GlobalEnv)
            source <- glue::glue("object {input$input_name} from current R environment")
            if (inherits(res, "data.frame")) {
              df <- rxs2tria::QWAimages(res) |> rxs2tria::complete_QWAimages()
              validated <- rxs2tria::QWAmetadata(images = df) |> rxs2tria::complete_QWAmetadata()
            } else { # assume list/QWAmetadata object
              validated <- rxs2tria::as_QWAmetadata(res) |> rxs2tria::complete_QWAmetadata()
            }
          }
        } else if (input$load_type == "file"){
          if (!shiny::isTruthy(input$input_file)) {
            stop("Please provide input source.")
          } else {
            ext <- tools::file_ext(input$input_file$datapath)
            if (ext == "csv"){
              df <- rxs2tria::read_QWAimages(input$input_file$datapath,
                allow_missing_req = FALSE,
                add_missing_opt = TRUE)
              validated <- rxs2tria::QWAmetadata(images = df) |> rxs2tria::complete_QWAmetadata()
            } else {
              res <- rxs2tria::read_QWAmetadata(input$input_file$datapath,
                allow_missing_req = TRUE, # don't be strict, may be in-progress file
                add_missing_opt = TRUE)
              validated <- res |> rxs2tria::complete_QWAmetadata()
            }
            source <- glue::glue("read from file {input$input_file$name}")
          }
        } else {
          example_file <- system.file("extdata", "20251015_TRIA_POGSTO2024_collected_metadata.json", package = "rxs2tria")
          res <- jsonlite::read_json(example_file, simplifyVector = TRUE)
          source <- glue::glue("using example dataset")
        }
        # generate dataset description template if empty
        if (!shiny::isTruthy(validated$dataset$description)) {
          template <- generate_desc_template(validated$images)
          validated$dataset$description <- template
        }
        # store in reactive containers
        input_meta$source <- source
        input_meta$images <- validated$images
        input_meta$dataset_tbls <- validated[c("dataset","authors","funding","related","resources")]
        input_meta$site_tbls <- validated[c("sites","trees","woodpieces","slides")]

        shiny::removeModal()
      }, err_title = "Error reading data", propagate_err = FALSE)
    })

    # output with source of input data
    output$file_status <- shiny::renderUI({
      if (is.null(input_meta$images)) {
        shiny::tags$code(input_meta$source,
             class = 'code-output', style = 'color: #da292e;') # bs-danger color
      } else {
        shiny::tags$code(input_meta$source, class = 'code-output')
      }
    })


    # RENDER SHINYTREE ---------------------------------------------------------
    # create data.tree and shinyTree compatible JSON of data structure
    dtree_json <- shiny::reactive({
      shiny::req(input_meta$images)

      df_dtree <- input_meta$images |>
        dplyr::select(site_label, species_code, tree_label, woodpiece_label, slide_label, image_label, org_img_name)
      df_dtree <- df_dtree |>
        dplyr::mutate(
          tree = stringr::str_remove(tree_label, glue::glue("({site_label}_)*({species_code}_)*")),
          woodpiece = stringr::str_remove(woodpiece_label, glue::glue("{tree_label}_*")),
          slide = stringr::str_remove(slide_label, glue::glue("{woodpiece_label}_*")),
          image = stringr::str_remove(image_label, glue::glue("{slide_label}_*"))
        ) |>
        dplyr::mutate(
          dplyr::across(c('woodpiece', 'slide'),
          \(x) stringr::str_replace(x, "^$", "(n.a.)"))) |>
        dplyr::mutate(DS = 'dataset', .before = 1) |> # need a root name for data.tree
        tidyr::unite('pathString', DS, site_label, tree_label, woodpiece_label, slide_label, image_label, sep = '/', remove = FALSE)

      dtree <- data.tree::FromDataFrameTable(
        df_dtree,
        colLevels = list(NULL, NULL, c('species_code','tree'),
                         'woodpiece', 'slide', 'org_img_name'))

      for (site in dtree$children) {
        site$state <- c(selected = TRUE, loaded = TRUE, opened = TRUE)
        site$icon <- "glyphicon glyphicon-map-marker"
        site$text <- paste0('<strong>',site$name, '</strong>')
        for (tree in site$children) {
          tree$icon <- "glyphicon glyphicon-tree-conifer"
          tree$text <- paste0('<b>', paste(tree$species_code, tree$tree), '</b>', sprintf("   [<i>label:</i> %s]", tree$name))
          for (wp in tree$children) {
            wp$icon <- "fa fa-bore-hole"
            wp$state <- c(opened = TRUE, loaded = TRUE)
            wp$text <- paste0('<strong>', wp$woodpiece, '</strong>', sprintf("   [<i>label:</i> %s]", wp$name))
            for (slide in wp$children) {
              slide$icon <- "fa fa-vial"
              slide$state <- c(opened = TRUE, loaded = TRUE)
              slide$text <- paste0('<strong>', slide$slide, '</strong>', sprintf("   [<i>label:</i> %s]", slide$name))
              for (img in slide$children) {
                img$icon <- "glyphicon glyphicon-picture"
                img$text <- paste0('<strong>', img$image, '</strong>', sprintf("   [<i>label:</i> %s | <i>org image name:</i> %s]", img$name, img$org_img_name))
              }
            }
          }
        }
      }

      shinyTree::treeToJSON(dtree, pretty = TRUE)
    })

    output$tree <- shinyTree::renderTree({
      shiny::validate(shiny::need(!is.null(input_meta$images), "No data to show"))

      dtree_json()
      })


    # HANDSONTABLE IMAGES -----------------------------------------------------
    # for the conditional panel with the column selection
    output$roxas_data_available <- shiny::reactive({
      !is.null(input_meta$images)
    })
    shiny::outputOptions(output, "roxas_data_available", suspendWhenHidden = FALSE)

    image_data_in <- shiny::reactiveVal(NULL)

    # TODO: the first one here is the roxas schema, adapt to work also for roxas ai
    tbl_props <- rxs2tria:::get_tbl_props(full_schema$properties$images$anyOf[[1]])$properties

    df_hot <- shiny::reactive({
      shiny::req(image_data_in())
      col_groups <- purrr::map_chr(tbl_props, "dtColGroup")
      sel_colgroups <- input$cols_meta
      sel_cols <- col_groups[col_groups %in% sel_colgroups] |> names()
      sel_cols <- union("image_label", sel_cols)

      image_data_in() |> dplyr::select(dplyr::any_of(sel_cols))
      # TODO: missing columns? read only cols?
    })

    shiny::observeEvent(input_meta$images, {
      df <- input_meta$images |>
        dplyr::mutate(rxs_created_at = as.character(rxs_created_at),
                      img_created_at = as.character(img_created_at)
      )
      image_data_in(df)
    })

    output$image_table <- rhandsontable::renderRHandsontable({
      shiny::validate(shiny::need(!is.null(image_data_in()), "No data to show"))

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(df_hot())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      n_rows <- nrow(image_data_in())
      ht_height <- min(max(n_rows * ht_row_height, ht_min_height), ht_max_height)

      ht <- rhandsontable::rhandsontable(
        df_hot(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = ht_height,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) |>
        rhandsontable::hot_cols(fixedColumnsLeft = 1)
      purrr::reduce(
        names(colHeaders), # names in df
        function(ht, col) {
          config <- tbl_props[[col]]
          colName <- colHeaders[col] # name in ht
          hot_col_wrapper(ht, colName, config)
        },
        .init = ht
      )
    })

    # create dataframe reactive to hot update
    # TODO: join with img_data_in for full columns
    image_data_out <- shiny::reactive({
      shiny::req(input$image_table)
      df_out <- rhandsontable::hot_to_r(input$image_table)
      df_in <- image_data_in()
      df_in |> dplyr::rows_update(df_out, by = "image_label")
    })

    output$testing <- shiny::renderPrint({
      #image_data_out()
    })

    # return the input meta and val check for use in other tabs
    return(
      list(
        images = image_data_out,
        dataset_tbls = shiny::reactive(input_meta$dataset_tbls),
        site_tbls = shiny::reactive(input_meta$site_tbls)
        #val_checks = validation_checks
      )
    )

  }) # end of moduleServer
}

    # VALIDATION CHECKS --------------------------------------------------------
    # TODO: any checks on band_witdh and only_ew?
    # validation_checks <- shiny::reactive({
    #   df_results <- data.frame(topic = character(0), field = character(0),
    #                            type = character(0), message = character(0))
    #   if (!input$check_raw) {
    #     df_results <- dplyr::bind_rows(
    #       df_results,
    #      data.frame(topic = "Raw input data",
    #                 field = "Inferred structure",
    #                 type = "error",
    #                 message = "Not confirmed"))
    #   }
    #
    #   df_results
    #
    # })
