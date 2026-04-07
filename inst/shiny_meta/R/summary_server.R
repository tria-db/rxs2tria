summary_server <- function(id, main_session, start_out, dataset_out, site_out) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    valchecks_combined <- reactive({
      df_checks <- dplyr::bind_rows(list(
        #start_out$val_check(), # NULL atm
        dataset_out$val_check(),
        site_out$val_check())
      )

      df_checks <- df_checks %>% dplyr::mutate(
        type = dplyr::if_else(grepl('invalid', tolower(message)), 'warning', 'error')
      )
      df_checks
    })

    output$DT_valcheck <- DT::renderDataTable({
      DT::datatable(
        valchecks_combined() %>% dplyr::select(-dplyr::any_of(c('fname', 'tname'))),
        selection = 'none',
        options = list(dom = 'pt',  pageLength = 10)
      )

    })

    data_combined <- shiny::reactive({
      images <- QWAimages(start_out$images())
      new_QWAmetadata(
        dataset = dataset_out$dataset_tbls$dataset(),
        authors = dataset_out$dataset_tbls$authors(),
        funding = dataset_out$dataset_tbls$funding(),
        related = dataset_out$dataset_tbls$related(),
        sites = site_out$site_tbls$sites(),
        trees = site_out$site_tbls$trees(),
        woodpieces = site_out$site_tbls$woodpieces(),
        slides = site_out$site_tbls$slides(),
        images = images
      )
    })



    observeEvent(input$btn_save, {
      validation_message <- if (nrow(valchecks_combined()) > 0) {
        tags$p(
          style = "color: red;",
          "Note that the automatic validation checks have raised issues with the collected metadata, see the validation check summary."
        )
      } else {
        NULL
      }

      showModal(modalDialog(
        title = "Confirm Export",
        tagList(
          "Do you want export the collected data? Exports can then be submitted or reloaded at a later time to continue working on the metadata.",
          validation_message
        ),
        footer = tagList(
          #downloadButton(ns("save_progress"), "Save Progress"),
          downloadButton(ns("final_export"), "Export"),
          modalButton("Cancel")
        )
      ))
    })

    # output$save_progress <- downloadHandler(
    #   filename = function() {
    #     paste0("collected_data_", Sys.Date(), ".json")
    #   },
    #   content = function(file) {
    #     removeModal()
    #     # Save the data to a file
    #     data_to_export <- data_combined()
    #     #data_to_export$export_type <- 'in_progress'
    #     jsonlite::write_json(data_to_export, file)
    #   }
    # )

    output$final_export <- downloadHandler(
      filename = function() {
        ds_name <- data_combined()$dataset$ds_name[1] %||% ""
        ds_name <- convert_ds_name_filename(ds_name)
        curr_date <- format(Sys.Date(), "%Y%m%d")
        glue::glue("{curr_date}_TRIA_{ds_name}_collected_metadata.json")
      },
      content = function(file) {
        removeModal()
        jsonlite::write_json(data_combined(), file, pretty = TRUE)
      }
    )

    # Previous button
    observeEvent(input$btn_prev, {
      nav_select(id = 'tabs', selected = tab_site, session = main_session)
    })


    output$testing <- shiny::renderPrint({
      dataset_out$dataset_tbls$dataset()
    })
  })
}
