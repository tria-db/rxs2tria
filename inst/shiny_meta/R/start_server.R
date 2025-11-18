# MODULE SERVER ----------------------------------------------------------------
start_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive container for input data
    input_meta <- reactiveValues(
      source = "Please select input source and load data",
      roxas_data = NULL,
      dataset_tbls = NULL,
      site_tbls = NULL
    )

    # next button
    # toggle: enable only if we have input data and check_raw is checked
    observe({
      shinyjs::toggleState(
        id = "btn_next", condition = !is.null(input_meta$roxas_data))
    })
    # functionality: switch to next tab
    observeEvent(input$btn_next, {
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })


    # LOAD INPUT DATA ----------------------------------------------------------
    observeEvent(input$btn_input, {
      # warn before overwriting existing data
      if (!is.null(input_meta$roxas_data)) {
        showModal(
          modalDialog(
            title = "Warning",
            "This action overwrites any existing inputs provided in the app,
            including any data already provided in the other tabs.
            Are you sure you want to proceed?",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_overwrite"), "Proceed")
            )
          )
        )
      } else {
        # input source modal
        show_input_modal(ns)
      }
    })

    # if overwrite confirmed, also show input source modal
    observeEvent(input$confirm_overwrite, {
      removeModal()
      show_input_modal(ns)
    })

    # ui to provide input data based on selected option
    output$load_details_ui <- renderUI({
      req(input$load_type)
      if (input$load_type == "fresh") {
        tagList(
          fileInput(ns("csv_file"), "Load CSV containing raw metadata", accept = c(".csv")),
          if (!server_run){
            textInput(ns("df_name"), "Or provide name of data.frame in current R environment", value = "")
          } else NULL
        )
      } else if (input$load_type == "continue") {
        fileInput(ns("json_file"), "Load JSON with partially completed submission", accept = c(".json"))
      } else { # example
        NULL
      }
    })

    observeEvent(input$confirm_input, {
      safe_block({
        # new submission: load df from env or file
        if (input$load_type == "fresh") {
          if (!is.null(input$df_name) && input$df_name != "") {
            df <- get(input$df_name, envir = .GlobalEnv)
            source <- glue::glue("data.frame {input$df_name} from current R environment")
          } else if (!is.null(input$csv_file)) {
            df <- vroom::vroom(input$csv_file$datapath, show_col_types = FALSE)
            source <- glue::glue("read from file {input$csv_file$name}")
          } else {
            stop("Please provide input source.")
          }
          # validate input table and update reactive
          df <- validate_df(df, "roxas", force_required = TRUE, ignore_colnames = FALSE)
          input_meta$source <- source
          input_meta$roxas_data <- df
          input_meta$dataset_tbls <- NULL # reset if necessary
          input_meta$site_tbls <- NULL # reset if necessary
        # existing submission: load data from json
        } else if (input$load_type == "continue") {
          if (!is.null(input$json_file)) {
            imported <- jsonlite::read_json(input$json_file$datapath, simplifyVector = TRUE)
            source <- glue::glue("read from file {input$json_file$name}")
            # validate json and update reactive
            validated <- validate_json(imported)
            input_meta$source <- source
            input_meta$roxas_data <- validated$roxas_data
            input_meta$dataset_tbls <- validated[names(validated) %in% c("ds_data","author_data", "funding_data", "relresource_data")]
            input_meta$site_tbls <- validated[names(validated) %in% c("site_data","tree_data", "woodpiece_data", "slide_data")]
          }
        # example run: load example data
        } else {
          example_file <- system.file("extdata", "20251015_TRIA_POGSTO2024_collected_metadata.json", package = "rxs2tria")
          imported <- jsonlite::read_json(example_file, simplifyVector = TRUE)
          source <- glue::glue("using example dataset")
          # validate json and update reactive
          validated <- validate_json(imported) # should always pass
          input_meta$source <- source
          input_meta$roxas_data <- validated$roxas_data
          input_meta$dataset_tbls <- validated[names(validated) %in% c("ds_data","author_data", "funding_data", "relresource_data")]
          input_meta$site_tbls <- validated[names(validated) %in% c("site_data","tree_data", "woodpiece_data", "slide_data")]
        }
        removeModal()
      }, propagate_err = FALSE)
    })





    # output with source of input data
    output$file_status <- renderUI({
      if (is.null(input_meta$roxas_data)) {
        code(input_meta$source,
             class = 'code-output', style = 'color: #da292e;') # bs-danger color
      } else {
        code(input_meta$source, class = 'code-output')
      }
    })


    # RENDER SHINYTREE ---------------------------------------------------------
    # create data.tree and shinyTree compatible JSON of data structure
    dtree_json <- reactive({
      req(input_meta$roxas_data)

      df_dtree <- input_meta$roxas_data %>% dplyr::select(site_label, species_code, tree_label, woodpiece_label, slide_label, image_label, org_img_name)
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
      validate(need(!is.null(input_meta$roxas_data), "No data to show"))

      dtree_json()
      })


    # RENDER DATATABLE ---------------------------------------------------------
    roxas_data_out <- reactiveVal(NULL)

    observeEvent(input_meta$roxas_data, {
      roxas_data_out(input_meta$roxas_data)
    })

    # for the conditional panel with the column selection
    output$roxas_data_available <- reactive({
      !is.null(input_meta$roxas_data)
    })
    outputOptions(output, "roxas_data_available", suspendWhenHidden = FALSE)

    # callbacks for DT
    autofill_callback <- c(
      "var tbl = $(table.table().node());",
      "var id = tbl.closest('.datatables').attr('id');",

      "delete $.fn.dataTable.AutoFill.actions.increment;",
      "delete $.fn.dataTable.AutoFill.actions.decrement;",

      "table.on('autoFill', function(e, datatable, cells){",
      "  var out = [];",
      "  for(var i = 0; i < cells.length; ++i){",
      "    var c = cells[i][0];",
      "    var value = c.set === null ? 'FALSE' : c.set;",
      "    var rowData = table.row(c.index.row).data();",
      "    var actualRow = rowData[rowData.length - 1];",
      "    out.push({",
      "      row: actualRow,",
      "      col: c.index.column,",
      "      value: value",
      "    });",
      "  }",
      "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
      "  table.rows().invalidate();",
      "});"
    )
    keys_callback <- c(
      "table.on('key', function(e, datatable, key, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  if(key == 13 && targetName == 'body'){",
      "    $(cell.node()).trigger('dblclick.dt');",
      "  }",
      "});",
      "table.on('keydown', function(e){",
      "  var keys = [9,13,37,38,39,40];",
      "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
      "    $(e.target).trigger('blur');",
      "  }",
      "});",
      "table.on('key-focus', function(e, datatable, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  var type = originalEvent.type;",
      "  if(type == 'keydown' && targetName == 'input'){",
      "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }",
      "  }",
      "});"
    )
    checkbox_callback <- c(
      "table.on('click', '.editor-checkbox', function(e){",
      "  var cell = table.cell($(this).closest('td'));",
      "  var rowData = table.row(cell.index().row).data();",
      "  var actualRow = rowData[rowData.length - 1];",
      "  var col = cell.index().column;",
      "  var newValue = $(this).prop('checked') ? 'TRUE' : 'FALSE';",
      "  ",
      "  var tbl = $(table.table().node());",
      "  var id = tbl.closest('.datatables').attr('id');",
      "  ",
      "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', [{",
      "    row: actualRow,",
      "    col: col,",
      "    value: newValue",
      "  }]);",
      "});"
    )
    DT_callbacks <- c(autofill_callback, keys_callback, checkbox_callback)

    # render DT
    output$roxas_table <- DT::renderDT({
      validate(need(!is.null(input_meta$roxas_data), "No data to show"))

      df <- input_meta$roxas_data # render initially based on input_meta$roxas_data

      # get ready for display
      df$.DT_RowIndex <- seq_len(nrow(df))

      # get the index of all columns except the ones named "band_width" and "only_ew"
      cb_col <- "only_ew"
      df[[cb_col]] <- as.character(df[[cb_col]])
      df$only_ew[is.na(df$only_ew)] <- "FALSE"

      # TODO: get idxs of hidden cols from input$cols_meta and add to columnDefs visible = FALSE
      # from schema dtColGroup
      obj <- get_schema('roxas_data')
      tbl_schema <- jsonlite::fromJSON(obj$schema$schema)
      extracted_groups <- sapply(tbl_schema$items$properties, function(x) x$dtColGroup)
      col_groups <- split(names(extracted_groups), extracted_groups)

      locked_cols_idx <- which(!(names(df) %in% col_groups[["cols_userinput"]])) - 1
      edit_cols_idx <- which(names(df) %in% col_groups[["cols_userinput"]]) - 1
      cb_col_idx <- which(names(df) == cb_col) - 1

      selected_groups <- isolate(input$cols_meta)
      visible_cols <- unlist(col_groups[names(col_groups) %in% selected_groups])
      visible_cols <- union(c("image_label", col_groups[["cols_userinput"]]), visible_cols) # always show (note: cannot hide cols_userinput)
      hidden_cols <- setdiff(names(df), visible_cols)
      hidden_cols_idx <- which(names(df) %in% hidden_cols) - 1


      DT::datatable(
        df,
        style = 'default',
        rownames = FALSE,
        selection = 'none',
        extensions = c("FixedColumns", "KeyTable", "AutoFill"),
        editable = list(
          target = 'cell', disable = list(columns = locked_cols_idx)
        ),
        options = list(
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1),
          autoFill = list(columns = edit_cols_idx,
                          alwaysAsk = FALSE,
                          vertical = TRUE,
                          horizontal = FALSE),
          keys = TRUE,
          columnDefs = list(
            list(targets = hidden_cols_idx, visible = FALSE),
            list(className = 'dt-center', targets = cb_col_idx),
            list(
              targets = cb_col_idx,
              render = DT::JS(
                "function(data, type, row, meta) {",
                "  if(type === 'display'){",
                "    var checked = data === 'TRUE' ? 'checked' : '';",
                "    return '<input type=\"checkbox\" class=\"editor-checkbox\" ' + checked + '>';",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        ),
        callback = DT::JS(DT_callbacks),
        class = "inputDT"
      ) |>
      DT::formatStyle(
        columns = 1,
        backgroundColor = sec_col_grad[5]
      ) |>
      DT::formatStyle(
        columns = setdiff(locked_cols_idx, c(0))+1,
        backgroundColor = "#e5e5e5"
      )
    })

    # handle updates and edits via proxy
    roxas_proxy <- DT::dataTableProxy("roxas_table")

    # update visible columns
    observeEvent(input$cols_meta, {
      req(roxas_proxy)

      df <- roxas_data_out()

      # Determine which columns to show (note: .DT_RowIndex is not in df, but always last and always hidden)
      selected_groups <- input$cols_meta

      obj <- get_schema('roxas_data')
      tbl_schema <- jsonlite::fromJSON(obj$schema$schema)
      extracted_groups <- sapply(tbl_schema$items$properties, function(x) x$dtColGroup)
      col_groups <- split(names(extracted_groups), extracted_groups)

      visible_cols <- unlist(col_groups[names(col_groups) %in% selected_groups])
      visible_cols <- union(c("image_label", col_groups[["cols_userinput"]]), visible_cols) # always show (note: cannot hide cols_userinput)
      visible_cols_idx <- which(names(df) %in% visible_cols) - 1
      hidden_cols <- setdiff(names(df), visible_cols)
      hidden_cols_idx <- which(names(df) %in% hidden_cols) - 1

      # Show/hide columns via proxy
      for (idx in visible_cols_idx) {
        DT::showCols(roxas_proxy, idx)
      }
      for (idx in hidden_cols_idx) {
        DT::hideCols(roxas_proxy, idx)
      }

    }, ignoreInit = TRUE, ignoreNULL = FALSE) # Init via isolate above, also trigger if all groups are deselected

    # Store pending edits in a reactiveVal
    pending_edits <- reactiveVal(NULL)

    # Capture edits from autofill and cell edit events
    observeEvent(input$roxas_table_cells_filled, {
      info <- input[["roxas_table_cells_filled"]]
      if(!is.null(info)){
        pending_edits(info)
      }
    })

    observeEvent(input[["roxas_table_cell_edit"]], {
      info <- input[["roxas_table_cell_edit"]]
      if(!is.null(info)){
        pending_edits(info)
      }
    })

    observeEvent(pending_edits(), {
      safe_block({
        info <- pending_edits()
        if(!is.null(info)){
          info$value[info$value == ""] <- NA
          info$value[info$value == "TRUE"] <- TRUE
          info$value[info$value == "FALSE"] <- FALSE
          # update reactive
          updated_data <- DT::editData(roxas_data_out(), info, rownames = FALSE)
          roxas_data_out(updated_data)

          # adaptions for display
          cb_col <- "only_ew"
          updated_data[[cb_col]] <- as.character(updated_data[[cb_col]])
          updated_data$only_ew[is.na(updated_data$only_ew)] <- "FALSE"
          updated_data$.DT_RowIndex <- seq_len(nrow(updated_data))
          DT::replaceData(roxas_proxy, updated_data, resetPaging = FALSE, rownames = FALSE)
        }
      }, propagate_err = FALSE)
    })


    # VALIDATION CHECKS --------------------------------------------------------
    # TODO: any checks on band_witdh and only_ew?
    # validation_checks <- reactive({
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


    # output$testing <- renderPrint({
    #   input$cols_meta
    # })

    # return the input meta and val check for use in other tabs
    return(
      list(
        roxas_data = roxas_data_out,
        dataset_tbls = reactive(input_meta$dataset_tbls),
        site_tbls = reactive(input_meta$site_tbls)
        #val_checks = validation_checks
      )
    )

  }) # end of moduleServer
}


# BUTTONS ------------------------------------------------------------------
# # file input buttons
# # toggle file input based on input source radio buttons
# observe({
#   shinyjs::toggleState(
#     id = "file_input",
#     condition = (!is.null(input$input_src) && input$input_src %in% c("df_meta_csv", "df_meta_json")))
#   shinyjs::toggleState(
#     id = "btn_load_input",
#     condition = (!is.null(input$input_src) && input$input_src == 'df_meta_env') || (!is.null(input$file_input)))
# })

# # column groups of df_meta
# # TODO: eventually, adapt for ROXAS AI
# cols_structure <- names(df_meta_tbl)[sapply(df_meta_tbl, function(x) x$grouping == "cols_structure")]
# cols_images <- names(df_meta_tbl)[sapply(df_meta_tbl, function(x) x$grouping == "cols_images")]
# cols_settings <- names(df_meta_tbl)[sapply(df_meta_tbl, function(x) x$grouping == "cols_settings")]
# cols_paths <- names(df_meta_tbl)[sapply(df_meta_tbl, function(x) x$grouping == "cols_paths")]
# cols_codes <- names(df_meta_tbl)[sapply(df_meta_tbl, function(x) x$grouping == "cols_codes")]

# observeEvent(input$btn_load_input, {
#   # warn before overwriting existing data
#   if (!is.null(input_meta$df)) {
#     showModal(
#       modalDialog(
#         title = "Warning",
#         "This action overwrites any existing inputs provided in the app,
#         including any data already provided in the other tabs.
#         Are you sure you want to proceed?",
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton(ns("confirm_submit"), "Proceed")
#         )
#       )
#     )
#   } else {
#     # load the data directly if no existing df
#     data <- load_input_data(input$input_src, input$file_input)
#     input_meta$df <- data$df
#     input_meta$source <- data$source
#     if (!is.null(data$meta_json)) {
#       input_meta$meta_json <- data$meta_json
#     }
#     shinyjs::reset(id = "check_raw")
#   }
# })
#
# # load input data in case of confirm overwrite
# observeEvent(input$confirm_submit, {
#   removeModal()
#   data <- load_input_data(input$input_src, input$file_input)
#   input_meta$df <- data$df
#   input_meta$source <- data$source
#   if (!is.null(data$meta_json)) {
#     input_meta$meta_json <- data$meta_json
#   }
#   shinyjs::reset(id = "check_raw")
# })
#
#
#
# # --------------------
# # EXAMPLE ONLY (no need for if statements, if not example_run, these ui elements just don't exist)
# shinyjs::disable("file_input_ex") # this is just a 'for show' button
# observeEvent(input$btn_load_input_ex, {
#   # check if df is already set
#   if (!is.null(input_meta$df)) {
#     showModal(
#       modalDialog(
#         title = "Warning",
#         "This action overwrites any existing inputs provided in the app.
#         Are you sure you want to proceed?",
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton(ns("confirm_submit_ex"), "Proceed")
#         )
#       )
#     )
#   } else {
#     # load the data directly if no existing df
#     file_path <- system.file("extdata", "raw_meta_ex.csv", package = "rxs2xcell")
#     df_meta <- vroom::vroom(file_path, col_types = c(tree = "c"))
#     input_meta$df <- df_meta
#     input_meta$source <- "example data, starting fresh from extracted metadata"
#     if (input$input_src_ex == 'df_meta_json'){
#       file_path <- system.file("extdata", "partial_meta_ex.json", package = "rxs2xcell")
#       meta_json <- jsonlite::fromJSON(file_path, flatten = TRUE)
#       input_meta$meta_json <- meta_json
#       input_meta$meta_json$df_meta <- NULL # we want only the rest of the json
#       input_meta$source <- "example data, starting from partially completed metadata"
#     }
#     shinyjs::reset(id = "check_raw")
#   }
# })
#
# # load input data in case of confirm overwrite
# observeEvent(input$confirm_submit_ex, {
#   removeModal()
#   # load the data directly if no existing df
#   file_path <- system.file("extdata", "raw_meta_ex.csv", package = "rxs2xcell")
#   df_meta <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
#   input_meta$df <- df_meta
#   input_meta$source <- "example data, starting fresh from extracted metadata"
#   input_meta$meta_json <- NULL # reset meta_json
#   if (input$input_src_ex == 'df_meta_json'){
#     file_path <- system.file("extdata", "partial_meta_ex.json", package = "rxs2xcell")
#     meta_json <- jsonlite::fromJSON(file_path, flatten = TRUE)
#     input_meta$meta_json <- meta_json
#     input_meta$meta_json$df_meta <- NULL # we want only the rest of the json (don't have it in example anyway)
#     input_meta$source <- "example data, starting from partially completed metadata"
#   }
#   shinyjs::reset(id = "check_raw")
# })
#
# # end of example only code
# # --------------------

# observeEvent(input$roxas_table_cell_edit, {
#   roxas_data_in(
#     DT::editData(roxas_data_in(), input$roxas_table_cell_edit, rownames = FALSE)
#   )
# })
# observeEvent(input$roxas_table_cell_edit, {
#   info <- input$roxas_table_cell_edit
#   # update the data frame with the new value (-1 due to del button col)
#   print(str(info))
#   current_df <- roxas_data_in()
#   current_df[info$row, info$col-1] <- as.numeric(info$value)
#   # roxas_data_in(current_df)
# })



# RENDER DT ----------------------------------------------------------------
# preprocess input metadata
# filt_meta <- reactive({
#   req(input_meta$df)
#
#   # format raw df: cols in right order
#   df <- input_meta$df %>%
#     dplyr::select(image_code, dplyr::all_of(cols_structure),
#                   dplyr::all_of(cols_images), dplyr::all_of(cols_settings),
#                   dplyr::all_of(cols_paths)) %>%
#     dplyr::mutate(dplyr::across(site:slide, as.factor))
#
#   # # filter for selected images in tree
#   # req(input$tree)
#   # selected_imgs <- get_selected_imgs(input$tree)
#   # df <- df %>% dplyr::filter(image_code %in% selected_imgs)
#
#   df
# })


# render the datatable
# output$DTmeta <- DT::renderDT({
#   validate(need(!is.null(input_meta$df), "No data to show"))
#
#   DT::datatable(filt_meta(),
#                 style = 'default',
#                 rownames = FALSE,
#                 container = create_table_sketch(
#                   cols_structure, cols_images, cols_settings, cols_paths,
#                   prim_col_grad[4], prim_col_grad[6]),
#                 selection = 'none',
#                 extensions = "FixedColumns",
#                 options = list(
#                   scrollX = TRUE,
#                   initComplete = DT::JS( # tippy tooltips for first level headers
#                     "function(settings, json) {",
#                     "  const headerCell1 = $(this.api().table().header()).find('th:first-child')[0];",
#                     "  const headerCell2 = $(this.api().table().header()).find('th:nth-child(2)')[0];",
#                     "  const headerCell3 = $(this.api().table().header()).find('th:nth-child(3)')[0];",
#                     "  const headerCell4 = $(this.api().table().header()).find('th:nth-child(4)')[0];",
#                     "  const headerCell5 = $(this.api().table().header()).find('th:nth-child(5)')[0];",
#                     "  if(headerCell1.hasOwnProperty('_tippy')) {headerCell1._tippy.destroy();}",
#                     "  tippy(headerCell1, {content: 'unique identifier derived by rxs2xcell'})",
#                     "  if(headerCell2.hasOwnProperty('_tippy')) {headerCell2._tippy.destroy();}",
#                     "  tippy(headerCell2, {content: 'structure inferred by rxs2xcell from filename patterns'})",
#                     "  if(headerCell3.hasOwnProperty('_tippy')) {headerCell3._tippy.destroy();}",
#                     "  tippy(headerCell3, {content: 'metadata extracted from image files'})",
#                     "  if(headerCell4.hasOwnProperty('_tippy')) {headerCell4._tippy.destroy();}",
#                     "  tippy(headerCell4, {content: 'metadata extracted from Roxas settings files'})",
#                     "  if(headerCell5.hasOwnProperty('_tippy')) {headerCell5._tippy.destroy();}",
#                     "  tippy(headerCell5, {content: 'local paths of raw data files will not be entered in database'})",
#                     "}"
#                   ),
#                   fixedColumns = list(leftColumns = 1)
#                 )
#   ) %>%
#     DT::formatStyle(
#       columns = c('image_code'), backgroundColor = prim_col_grad[4]) %>%
#     DT::formatStyle(
#       columns = c(cols_structure,cols_settings), backgroundColor = prim_col_grad[6])
# })


