# TODO: multicol validations?


# SERVER -----------------------------------------------------------------------
dataset_server <- function(id, main_session, dataset_tbls_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shinyjs::disable(id = "file_authors")
    shinyjs::disable(id = "file_funding")
    shinyjs::disable(id = "file_relres")

    # mock event to close ROR tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'search_tools', values = TRUE)
      accordion_panel_close(id = 'doi_search_tool', values = TRUE)
    }, ignoreNULL = FALSE) # to fire the event at startup


    # observe changes (i.e. loading) of dataset input tables and fill fields and containers
    observeEvent(dataset_tbls_in(),{
      tbls <- dataset_tbls_in()

      # Update ds_data fields if available and possible
      if (!is.null(tbls$ds_data)){
        updateTextInput(
          session, "ds_name",
          value = ifelse(isTruthy(tbls$ds_data$ds_name), tbls$ds_data$ds_name, ""))
        updateTextAreaInput(
          session, "acknowledgements",
          value = ifelse(isTruthy(tbls$ds_data$acknowledgements), tbls$ds_data$acknowledgements, ""))
        safe_block({
          if (isTruthy(tbls$ds_data$ds_access)) {
            updateRadioButtons(session, "ds_access", selected = tbls$ds_data$ds_access)
            if (tbls$ds_data$ds_access == "public"){
              updateSelectizeInput(
                session, "ds_license",
                selected = ifelse(isTruthy(tbls$ds_data$ds_license), tbls$ds_data$ds_license, "CC BY 4.0"))
            } else if (tbls$ds_data$ds_access == "restricted"){
              updateDateInput(
                session, "embargoed_until",
                value = ifelse(isTruthy(tbls$ds_data$embargoed_until), as.Date(tbls$ds_data$embargoed_until), Sys.Date() + 365))
            }
          }
          if (isTruthy(tbls$ds_data$description)) {
            updateTextAreaInput(
              session, "description",
              value = tbls$ds_data$description)
          } else {
            template <- generate_desc_template(tbls$roxas_data)
            updateTextAreaInput(
              session, "description",
              value = template)
          }
        }, err_message = "Some dataset fields could not be updated", propagate_err = FALSE)
      }

      # Update author_data_in if available
      if(!is.null(tbls$author_data)) {
        author_data_in(tbls$author_data)
      }

      # Update funding_data_in if available
      if(!is.null(tbls$funding_data)) {
        funding_data_in(tbls$funding_data)
      }

      # Update doi_data_in if available
      if(!is.null(tbls$relresource_data)) {
        relres_data_in(tbls$relresource_data)
      }

    })


    # DATASET INPUT ------------------------------------------------------------
    # add validator rules for dataset inputs
    iv_gen <- shinyvalidate::InputValidator$new()
    iv_gen$add_rule("ds_name", shinyvalidate::sv_required())
    iv_gen$add_rule("ds_name", max_char_limit, limit = 64)
    iv_gen$add_rule("description", shinyvalidate::sv_required())
    iv_gen$add_rule(
      "description",
      shinyvalidate::sv_regex("\\[ADD INFO", "Complete or adapt the description template", invert = TRUE)
    )

    iv_gen$enable()


    # ROR SEARCH ---------------------------------------------------------------
    # toggle search button: only enable if we have a country and search string
    shiny::observe({
      shinyjs::toggleState(id = "btn_ror_search",
                           condition = (!(input$ror_search_string=="") &&
                                        !(input$ror_search_country=="")))
    })

    # ror_df: a reactive updated only in the event of the search button being clicked
    ror_df <- eventReactive(input$btn_ror_search, {
      req(input$ror_search_country, input$ror_search_string)

      # run the ROR API request with the input search string
      ror_api_request(search_string = input$ror_search_string,
                      country_code = input$ror_search_country)
    })

    # render instructions
    output$ror_instr <- renderUI({
      if (is.null(ror_df())) {
        tags$i("Run ROR search first...")
      } else {
        tags$i("Click on a row to select and transfer the ROR data to the tables below.")
      }
    })

    # render ROR DT
    output$ror_results <- DT::renderDT({
      validate(need(!is.null(ror_df()), "No data to show"))
      DT::datatable(ror_df() %>% dplyr::select(Link, RORID, Name, Location),
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    options = list(pageLength = 5))
    })

    # observe ROR row selection: open modal
    observeEvent(input$ror_results_rows_selected, {
      showModal(modalDialog(
        title = "Transfer ROR data",
        tagList(
          p("Do you want to transfer the selected ROR data to the author and/or funding table?"),
          checkboxGroupInput(
            ns("selected_authors"),
            label = "Select authors to update:",
            choices = get_author_choices(author_data_out())
          ),
          checkboxGroupInput(
            ns("selected_funders"),
            label = "Select funding institutions to update:",
            choices = get_funding_choices(funding_data_out())
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_ror"), "Transfer")
        )
      ))
    })

    # transfer ROR data to authors on confirm transfer
    observeEvent(input$btn_trans_ror, {
      # update the authors table
      if (!is.null(input$selected_authors) && !is.null(input$ror_results_rows_selected)) {
        # get the ROR data of the selected row (NOTE that only 1 row can be selected)
        selected_ror_data <- ror_df()[input$ror_results_rows_selected, c("RORID", "Name", "city", "country_code")]
        selected_ror_data$Name <- gsub('\n','<br>',stringr::str_wrap(selected_ror_data$Name, width = 50))
        selected_ror_data$country_code <- countries_info |> dplyr::filter(country_iso_code == selected_ror_data$country_code) |> dplyr::pull(combined)
        # update the author data table for the selected authors
        current_df <- author_data_out()
        current_df[input$selected_authors, c("rorid", "org_name", "aff_city", "org_country")] <- selected_ror_data
        # TODO: if new author, generate author nr?
        author_data_in(current_df)
      }
      # update the funding table
      if (!is.null(input$selected_funders) && !is.null(input$ror_results_rows_selected)) {
        # get the ROR data of the selected row (NOTE that only 1 row can be selected)
        selected_ror_data <- ror_df()[input$ror_results_rows_selected, c("RORID", "Name", "country_code")]
        # selected_ror_data$Name <- gsub('\n','<br>',stringr::str_wrap(selected_ror_data$Name, width = 50))
        selected_ror_data$country_code <- countries_info |> dplyr::filter(country_iso_code == selected_ror_data$country_code) |> dplyr::pull(combined)
        # update in the funding data table for the selected funders
        current_df <- funding_data_out()
        current_df[input$selected_funders, c("rorid", "org_name", "org_country")] <- selected_ror_data
        funding_data_in(current_df)
      }
      # close the modal
      removeModal()
    })



    # ORCID SEARCH ---------------------------------------------------------------
    # toggle search button: only enable if we have a country and search string
    observe({
      shinyjs::toggleState(id = "btn_orcid_search",
                           condition = !(input$orcid_search_string==""))
    })

    # reactiveVal (responding to search orcid from string or from table buttons)
    orcid_df <- reactiveVal()

    # run ORCID API search on provided search string
    observeEvent(input$btn_orcid_search, {
      req(input$orcid_search_string)

      # run the ORCID API request with the input search string
      res_df <- orcid_api_request(search_string = input$orcid_search_string)
      res_df$search_terms <- input$orcid_search_string

      # update the reactiveVal
      orcid_df(res_df)
    })

    # run ORCID API search on author table
    observeEvent(input$btn_orcid_tbl, {
      current_df <- author_data_out()
      results_combined <- list()
      for (row in 1:nrow(current_df)){
        # short break every 10th run to avoid crashing the API
        if (row > 10 && row %% 10 == 0) {Sys.sleep(0.6)}

        # run a name based api request for each row with values
        last_name <- current_df$last_name[row]
        first_name <- current_df$first_name[row]
        if (last_name != "" || first_name != ""){
          results <- orcid_api_request(last_name = last_name, first_name = first_name)
          results_combined[[paste(last_name, first_name)]] <- results

          # if the result is unique, update the author table
          if (nrow(results) == 1){
            current_df[row, c("last_name", "first_name", "orcid")] <- results[, c("last_name", "first_name", "orcid_id")]
            # only update email if the field is NA or empty
            if (is.na(current_df[row, "email"]) || current_df[row, "email"] == "") {
              current_df[row, "email"] <- results$email
            }
            # only update org_name if the field is NA or empty
            if (is.na(current_df[row, "org_name"]) || current_df[row, "org_name"] == "") {
              current_df[row, "org_name"] <- results$org_name
            }
          }
        }
      }

      # combine the results and update the reactives
      if (length(results_combined) > 0){
        res_df <- dplyr::bind_rows(results_combined, .id = 'search_terms')
        orcid_df(res_df)
        author_data_in(current_df)
      }

    })

    # render instructions
    output$orcid_instr <- renderUI({
      if (is.null(orcid_df())) {
        tags$i("Run ORCID search first...")
      } else {
        tags$i("Click on a row to select and transfer the ORCID data to the table below.
                Email and affiliation data will only be transferred if not yet provided in the table.")
      }
    })

    # render ORCID DT
    output$orcid_results <- DT::renderDT({
      validate(need(!is.null(orcid_df()), "No data to show"))
      DT::datatable(orcid_df() %>% dplyr::select(search_terms, last_name, first_name, email, orcid, org_name, other_names),
                    extensions = 'RowGroup',
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    options = list(pageLength = 5,
                                   rowGroup = list(dataSrc = 0)))
    })

    # observe ORCID row selection: open modal
    observeEvent(input$orcid_results_rows_selected, {
      showModal(modalDialog(
        title = "Transfer ORCID data",
        tagList(
          p("Do you want to transfer the selected ORCID data to the author table?"),
          radioButtons(
            ns("sel_author_orc"),
            label = "Select author to update:",
            choices = c(get_author_choices(author_data_out()), "Add new author" = "new"),
            selected = "new"
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_orcid"), "Transfer")
        )
      ))
    })

    # transfer ORCID data to author table on confirm transfer
    observeEvent(input$btn_trans_orcid, {
      if (!is.null(input$sel_author_orc) && !is.null(input$orcid_results_rows_selected)) {
        # get the ORCID data of the selected row (NOTE: only 1 row can be selected)
        sel_orcid_data <- orcid_df()[input$orcid_results_rows_selected,]
        sel_orcid_data$org_name <- gsub('\n','<br>',stringr::str_wrap(sel_orcid_data$org_name, width = 50))
        # update in the author data table for the selected author (NOTE: only 1 can be selected)
        current_df <- author_data_out()
        if (input$sel_author_orc == "new"){
          row <- nrow(current_df) + 1
          current_df[row,] <- create_empty_df("author_data", nrows=1)
          current_df[row, "author_nr"] <- as.integer(max(current_df$author_nr, na.rm = TRUE) + 1)
        } else {
          row <- input$sel_author_orc
        }
        current_df[row, c("last_name", "first_name", "orcid")] <- sel_orcid_data[, c("last_name", "first_name", "orcid_id")]
        # only update email if the field is NA or empty
        if (is.na(current_df[row, "email"]) || current_df[row, "email"] == "") {
          current_df[row, "email"] <- sel_orcid_data$email
        }
        # only update org_name if the field is NA or empty
        if (is.na(current_df[row, "org_name"]) || current_df[row, "org_name"] == "") {
          current_df[row, "org_name"] <- sel_orcid_data$org_name
        }
        author_data_in(current_df)
      }

      # close the modal
      removeModal()
    })




    # AUTHOR RHANDSONTABLE -----------------------------------------------------
    # initialize reactiveVal (responding to add/delete row, ror/orcid transfer, file upload)
    author_data_in <- reactiveVal(create_empty_df("author_data", nrows=1) |>
                                    dplyr::mutate(author_nr = as.integer(1)))

    # render editable table
    output$author_table <- rhandsontable::renderRHandsontable({
      schema_path <- system.file("extdata", "json_schema/20251007_tria_author_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(author_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      rhandsontable::rhandsontable(
        author_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #height = 150,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)
      ) %>%
      # custom validation check renderers for all cols based on tbl_config
      purrr::reduce(
        names(colHeaders), # names in df
        function(ht, col) {
          config <- tbl_props[[col]]
          colName <- colHeaders[col] # name in ht
          hot_col_wrapper(ht, colName, config)
        },
        .init = . # start with the base hot
      )

    })


    # create dataframe reactive to hot updates
    author_data_out <- reactive({
      rhandsontable::hot_to_r(input$author_table)
    })

    # observe add row button
    observeEvent(input$btn_add_author, {
      new_row <- create_empty_df("author_data", nrows=1)
      current_df <- author_data_out()
      new_row$author_nr <- as.integer(max(current_df$author_nr, na.rm = TRUE) + 1)
      current_df[nrow(current_df)+1,] <- new_row
      author_data_in(current_df)
    })

    # observe delete row button
    observeEvent(input$btn_del_author, {
      req(nrow(author_data_out()) > 1)
      current_df <- author_data_out()
      current_df <- current_df[-nrow(current_df),]
      author_data_in(current_df)
    })

    # TODO:
    # import data from file
    observeEvent(input$file_authors,{
      show_ht_import_modal(ns, 'import_aut_data')
    })

    # observe confirm import data button
    observeEvent(input$import_aut_data, {
      removeModal()
      # TODO: update with new load function
      # imported <- load_single_table(input$file_authors$datapath, 'author_data', author_tbl,
      #                               force_required = FALSE, col_names = input$col_names, skip = input$skip)
      # author_data_in(imported)
    })


    # FUNDING RHANDSONTABLE ----------------------------------------------------
    # initialize reactiveVal (responding to add/delete row, ror transfer, file upload)
    funding_data_in <- reactiveVal(create_empty_df("funding_data", nrows=1))

    # Render editable table
    output$funding_table <- rhandsontable::renderRHandsontable({
      schema_path <- system.file("extdata", "json_schema/20251007_tria_funding_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(funding_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      rhandsontable::rhandsontable(
        funding_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #height = 150,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)
      ) %>%
        # custom validation check renderers for all cols based on tbl_config
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
    funding_data_out <- reactive({
      rhandsontable::hot_to_r(input$funding_table)
    })

    # observe add row button
    observeEvent(input$btn_add_fund, {
      new_row <- create_empty_df("funding_data", nrows=1)
      current_df <- funding_data_out()
      current_df[nrow(current_df)+1,] <- new_row
      funding_data_in(current_df)
    })

    # observe delete row button
    observeEvent(input$btn_del_fund, {
      req(nrow(funding_data_out()) > 1)
      current_df <- funding_data_out()
      current_df <- current_df[-nrow(current_df),]
      funding_data_in(current_df)
    })


    # import data from file: show modal for confirmation
    observeEvent(input$file_funding,{
      show_ht_import_modal(ns, 'import_fund_data')
    })

    # observe confirm import data button
    observeEvent(input$import_fund_data, {
      removeModal()
      # TODO: update with new load function
      # imported <- load_single_table(input$file_funding$datapath, 'funding_data', funding_tbl,
      #                               force_required = FALSE, col_names = input$col_names, skip = input$skip)
      # funding_data_in(imported)
    })







    # DOI SEARCH ---------------------------------------------------------------
    # toggle add button: only enable if we have a DOI or citation to add
    # TODO: add tria lookup?
    observe({
      shinyjs::toggleState(id = "btn_cite_search",
                           condition = (input$doi_string != "" || input$citation_string != ""))
    })

    # doi_df: a reactive updated only in the event of the search button being clicked
    doi_df <- eventReactive(input$btn_cite_search, {
      #req(input$ror_search_country, input$ror_search_string)

      # run the ROR API request with the input search string
      if (input$doi_string != "") {
        doi_res <- doi_api_request(input$doi_string)
        updateTextInput(session, "doi_string", value = "")
        return(doi_res)
      } else if (input$citation_string != "") {
        cite_res <- cr_api_request(input$citation_string)
        updateTextAreaInput(session, "citation_string", value = "")
        return(cite_res)
      }
      # TODO: tria ds search
    })

    # render instructions
    output$doi_instr <- renderUI({
      if (is.null(doi_df())) {
        tags$i("Run DOI search first...")
      } else {
        tags$i("Click on a row to select and transfer the DOI data to the tables below.")
      }
    })

    # render ROR DT
    output$doi_results <- DT::renderDT({
      validate(need(!is.null(doi_df()), "No data to show"))
      DT::datatable(doi_df(),
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    options = list(paging = FALSE,
                                   searching = FALSE))
    })

    # observe DOI row selection: transfer data
    observeEvent(input$doi_results_rows_selected, {
      selected_doi_data <- doi_df()[input$doi_results_rows_selected, c("citation", "doi")]
      selected_doi_data$citation <- gsub('\n','<br>',stringr::str_wrap(selected_doi_data$citation, width = 50))
      # add to relres data table
      current_df <- relres_data_out()
      nrows <- nrow(current_df)
      current_df[nrows+1, c("citation", "doi")] <- selected_doi_data
      relres_data_in(current_df)
    })


    # REL RESOURCE RHANDSONTABLE -----------------------------------------------
    # initialize reactiveVal (responding to add/delete item, DT cell edits)
    relres_data_in <- reactiveVal(create_empty_df("relresource_data", nrows=0))

    # Render editable table
    output$relres_table <- rhandsontable::renderRHandsontable({
      schema_path <- system.file("extdata", "json_schema/20251007_tria_relresource_ext_schema.json", package = "rxs2tria")
      tbl_props <- load_extended_schema(schema_path)

      colHeaders <- sapply(tbl_props, function(x) x$title)
      colHeaders <- colHeaders[names(relres_data_in())] # ensure correct order
      tippies <- sapply(tbl_props, function(x) x$description)

      rhandsontable::rhandsontable(
        relres_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #height = 150,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)
      ) %>%
        # custom validation check renderers for all cols based on tbl_config
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
    relres_data_out <- reactive({
      rhandsontable::hot_to_r(input$relres_table)
    })

    # observe add row button
    observeEvent(input$btn_add_res, {
      new_row <- create_empty_df("relresource_data", nrows=1)
      current_df <- relres_data_out()
      current_df[nrow(current_df)+1,] <- new_row
      relres_data_in(current_df)
    })

    # observe delete row button
    observeEvent(input$btn_del_res, {
      req(nrow(funding_data_out()) > 0)
      current_df <- relres_data_out()
      current_df <- current_df[-nrow(current_df),]
      relres_data_in(current_df)
    })


    # import data from file: show modal for confirmation
    observeEvent(input$file_relres,{
      show_ht_import_modal(ns, 'import_res_data')
    })

    # observe confirm import data button
    observeEvent(input$import_res_data, {
      removeModal()
      # TODO: update with new load function
      # imported <- load_single_table(input$file_funding$datapath, 'funding_data', funding_tbl,
      #                               force_required = FALSE, col_names = input$col_names, skip = input$skip)
      # funding_data_in(imported)
    })


    # VALIDATION CHECKS --------------------------------------------------------
    validation_checks <- reactive({
      results <- list()

      # 1) dataset input fields from the input validator
      iv_validated <- iv_gen$validate()
      results$ds_data <- collect_validator_results(iv_validated, input_field_names, 'ds')
      # TODO: change to schema validation? what about other fields

      # 2) author table
      df_aut <- author_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_author_ext_schema.json", package = "rxs2tria")
      aut_tbl_props <- load_extended_schema(schema_path)
      results$author_data <- collect_hot_val_results(df_aut, aut_tbl_props)

      # 3) funding table
      df_fund <- funding_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_funding_ext_schema.json", package = "rxs2tria")
      fund_tbl_props <- load_extended_schema(schema_path)
      results$funding_data <- collect_hot_val_results(df_fund, fund_tbl_props)

      # 4) related resources
      df_res <- relres_data_out()
      schema_path <- system.file("extdata", "json_schema/20251007_tria_relresource_ext_schema.json", package = "rxs2tria")
      relres_tbl_props <- load_extended_schema(schema_path)
      results$relres_data <- collect_hot_val_results(df_res, relres_tbl_props)

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

    # set the color of the card header based on the validation results
    observe({
      shinyjs::toggleClass(id = "val_check_header", class = 'bg-secondary',
                           condition = nrow(validation_checks()) > 0)
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

    # TODO:
    # Observe save button
    # observeEvent(input$btn_save_aut, {
    #   data <- author_data$df_out
    #
    #   # TODO: file download rather than predefined file?
    #   # Save to CSV
    #   write.csv(data, file = "author_data.csv", row.names = FALSE)
    #   showModal(modalDialog(
    #     title = "Success",
    #     "Data saved successfully!",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # })



    # Next button
    observeEvent(input$btn_next, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_site, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_start, session = main_session)
    })


    # TODO:
    # additional validations
    # read data from file: append or warn if there is already data?
    # from / to csv or xslx? sheets?
    # more details on validation errors (nr characters, pattern, etc.)
    # orcid transfer: what if names don't match?


    return(
      list(
        dataset_tbls  = list(
          ds_data = reactive(data.frame(
            ds_name = input$ds_name,
            description = input$description,
            ds_access = input$ds_access,
            ds_license = input$ds_license,
            embargoed_until = ifelse(input$ds_access == "restricted", input$embargoed_until, ""),
            acknowledgements = input$acknowledgements
          )),
          author_data = author_data_out,
          funding_data = funding_data_out,
          relresource_data = relres_data_out
        ),
        val_check = validation_checks
      )
    )

  })
}




# # RELATED RESOURCES --------------------------------------------------------
# # TODO: HERE: update to hot
# # toggle add button: only enable if we have a DOI or citation to add
# observe({
#   shinyjs::toggleState(id = "btn_add_pub",
#                        condition = (input$doi != "" || input$citation != ""))
# })
#
# # initialize reactiveVal (responding to add/delete item, DT cell edits)
# doi_data_in <- reactiveVal(create_empty_tbl(doi_tbl, nrows=0))
#
# # render DT
# output$rel_resources <- DT::renderDT({
#   # a DT with a column of delete buttons
#   colHeaders <- get_tbl_colHeaders(doi_tbl)
#   deleteButtonColumn(doi_data_in(), 'delbtn', ns, colHeaders)
# })
#
#
# # observe delete row events
# observeEvent(input$deletePressed, {
#   rowNum <- parseDeleteEvent(input$deletePressed)
#   # Delete the row from the data frame
#   current_df <- doi_data_in()
#   current_df <- current_df[-rowNum,]
#   if (nrow(current_df) > 0) {
#     rownames(current_df) <- 1:nrow(current_df) # update rownames
#   }
#   doi_data_in(current_df)
# })
#
# # observe add item events
# observeEvent(input$btn_add_pub,{
#   # TODO: logic if both are provided? (disable add button?)
#   # TODO: what about XCELL datasets (without DOI)?
#   # EITHER run DOI API request
#   if (!is.null(input$doi) && input$doi != "") {
#     res_df <- doi_api_request(input$doi)
#     if (!is.null(res_df)) {
#       current_df <- doi_data_in()
#       current_df <- current_df |> dplyr::bind_rows(res_df)
#       doi_data_in(current_df)
#       updateTextInput(session, "doi", value = "")
#     }
#
#   # OR copy citation text directly to table
#   } else  if (!is.null(input$citation) && input$citation != "") {
#     current_df <- doi_data_in()
#     nrows <- nrow(current_df)
#     current_df[nrows+1,'citation'] <- input$citation
#     doi_data_in(current_df)
#     # clear input field
#     updateTextInput(session, "citation", value = "")
#   }
# })
#
# # observe DT cell edits
# observeEvent(input$rel_resources_cell_edit, {
#   info <- input$rel_resources_cell_edit
#   # update the data frame with the new value (-1 due to del button col)
#   current_df <- doi_data_in()
#   current_df[info$row, info$col-1] <- info$value
#   doi_data_in(current_df)
# })

# observeEvent(start_info$input_meta$meta_json,{
#   # TODO: add validity checks before updating values?
#   # req(start_info$input_meta$meta_json)
#   if (!is.null(start_info$input_meta$meta_json)){
#     meta_json <- start_info$input_meta$meta_json
#     if (!is.null(meta_json$ds_data)){
#       # if it is not null, it has been aligned to structure, so can expect a 1-row df with all required columns
#       ds_data <- meta_json$ds_data
#       updateTextInput(session, "ds_name", value = ifelse(is.na(ds_data$ds_name), "", ds_data$ds_name))
#       updateTextAreaInput(session, "acknowledgements", value = ifelse(is.na(ds_data$acknowledgements), "", ds_data$acknowledgements))
#       if (!is.na(ds_data$ds_access)){
#         updateRadioButtons(session, "ds_access", selected = ds_data$ds_access)
#         if (ds_data$ds_access == "public"){
#           updateSelectizeInput(session, "ds_license", selected = ifelse(is.na(ds_data$ds_license), "CC BY 4.0", ds_data$ds_license))
#         } else if (ds_data$ds_access == "restricted"){
#           safe_block({
#             updateDateInput(session, "embargoed_until", value = ifelse(is.na(ds_data$embargoed_until), Sys.Date() + 365, as.Date(ds_data$embargoed_until)))
#           }, err_message = "Embargo date could not be updated", err_propagate = FALSE)
#         }
#       }
#     }
#     if (!is.null(meta_json$author_data)) {author_data_in(meta_json$author_data)}
#     if (!is.null(meta_json$funding_data)) {funding_data_in(meta_json$funding_data)}
#     if (!is.null(meta_json$doi_data)) {doi_data_in(meta_json$doi_data)}
#   }
# })
