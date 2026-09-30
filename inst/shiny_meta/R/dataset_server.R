# TODO: multicol validations?


# SERVER -----------------------------------------------------------------------
dataset_server <- function(id, main_session, dataset_tbls_in) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shinyjs::disable(id = "file_authors")
    shinyjs::disable(id = "file_funding")
    shinyjs::disable(id = "file_relres")

    # pre-compute table props and renderers once at module init
    aut_tbl_props_full    <- rxs2tria:::get_tbl_props(full_schema$properties$authors)
    fund_tbl_props_full   <- rxs2tria:::get_tbl_props(full_schema$properties$funding)
    relres_tbl_props_full <- rxs2tria:::get_tbl_props(full_schema$properties$related)

    aut_tbl_props    <- aut_tbl_props_full$properties
    fund_tbl_props   <- fund_tbl_props_full$properties
    relres_tbl_props <- relres_tbl_props_full$properties

    aut_renderers    <- build_tbl_renderers(aut_tbl_props)
    fund_renderers   <- build_tbl_renderers(fund_tbl_props)
    relres_renderers <- build_tbl_renderers(relres_tbl_props)

    # mock event to close ROR tab on start of app
    shiny::observe({
      bslib::accordion_panel_close(id = 'search_tools', values = TRUE)
      bslib::accordion_panel_close(id = 'doi_search_tool', values = TRUE)
    }) |> shiny::bindEvent(1) # to fire the event at startup


    # observe changes (i.e. loading) of dataset input tables and fill fields and containers
    shiny::observe({
      safe_block({
        ds_data <- dataset_tbls_in()$dataset

        if (nrow(ds_data) != 1) {
          cli::cli_abort("Dataset table has {nrow(ds_data)} rows")
        }

        if (shiny::isTruthy(ds_data$ds_name)) {
          shiny::updateTextInput(session, "ds_name", value = ds_data$ds_name)
        }
        if (shiny::isTruthy(ds_data$ds_title)) {
          shiny::updateTextAreaInput(session, "ds_title", value = ds_data$ds_title)
        }
        if (shiny::isTruthy(ds_data$acknowledgements)) {
          shiny::updateTextAreaInput(
            session, "acknowledgements", value = ds_data$acknowledgements
          )
        }
        if (shiny::isTruthy(ds_data$ds_access)) {
          if (ds_data$ds_access %in% c("public", "restricted")) {
            shiny::updateRadioButtons(session, "ds_access", selected = ds_data$ds_access)
          } else {
            cli::cli_abort("Invalid access option {ds_data$ds_access}")
          }
        }

        if (shiny::isTruthy(ds_data$ds_license)) {
          shiny::updateSelectizeInput(session, "ds_license", selected = ds_data$ds_license)
        }

        if (shiny::isTruthy(ds_data$embargoed_until)) {
          if (ds_data$ds_access == "restricted") {
            shiny::updateDateInput(
              session, "embargoed_until", value = as.Date(ds_data$embargoed_until)
            )
          } else {
            cli::cli_warn("Embargo date applies only to restricted datasets.")
          }
        }

        if (shiny::isTruthy(ds_data$description)) {
          shiny::updateTextAreaInput(
            session, "description", value = ds_data$description
          )
        }
      }, err_message = "Some dataset fields could not be updated", propagate_err = FALSE)

      authors_in <- dataset_tbls_in()$authors
      if (!is.null(authors_in$org_country)) {
        authors_in$org_country <- iso_to_combined(authors_in$org_country)
      }
      author_data_in(authors_in)
      funding_in <- dataset_tbls_in()$funding
      if (!is.null(funding_in$org_country)) {
        funding_in$org_country <- iso_to_combined(funding_in$org_country)
      }
      funding_data_in(funding_in)
      relres_data_in(dataset_tbls_in()$related)

    }) |> shiny::bindEvent(dataset_tbls_in())


    # DATASET INPUT ------------------------------------------------------------
    # add validator rules for dataset inputs
    iv_gen <- shinyvalidate::InputValidator$new()
    iv_gen$add_rule("ds_name", shinyvalidate::sv_required())
    iv_gen$add_rule("ds_name", max_char_limit, limit = 64)
    iv_gen$add_rule("ds_title", shinyvalidate::sv_required())
    iv_gen$add_rule("ds_title", max_char_limit, limit = 300)
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
    ror_df <- shiny::eventReactive(input$btn_ror_search, {
      shiny::req(input$ror_search_country, input$ror_search_string)

      # run the ROR API request with the input search string
      ror_api_request(search_string = input$ror_search_string,
                      country_code = input$ror_search_country)
    })

    # render instructions
    output$ror_instr <- shiny::renderUI({
      if (is.null(ror_df())) {
        shiny::tags$i("Run ROR search...")
      } else {
        shiny::tags$i("Click on a row to select and transfer the ROR data to the tables below.")
      }
    })

    # render ROR DT
    output$ror_results <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(ror_df()), "No data to show"))
      DT::datatable(
        ror_df() |> dplyr::select(Link, RORID, Name, Location),
        style = 'default',
        rownames = FALSE,
        selection = "single",
        escape = FALSE,
        fillContainer = TRUE,
        options = list(pageLength = 5)
      )
    })

    # observe ROR row selection: open modal
    shiny::observeEvent(input$ror_results_rows_selected, {
      shiny::showModal(shiny::modalDialog(
        title = "Transfer ROR data",
        shiny::tagList(
          shiny::p("Do you want to transfer the selected ROR data to the author and/or funding table?"),
          shiny::checkboxGroupInput(
            ns("selected_authors"),
            label = "Select authors to update:",
            choices = get_author_choices(author_data_out())
          ),
          shiny::checkboxGroupInput(
            ns("selected_funders"),
            label = "Select funding institutions to update:",
            choices = get_funding_choices(funding_data_out())
          )
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_trans_ror"), "Transfer")
        )
      ))
    })

    # transfer ROR data to authors on confirm transfer
    shiny::observeEvent(input$btn_trans_ror, {
      # update the authors table
      if (!is.null(input$selected_authors) && !is.null(input$ror_results_rows_selected)) {
        # get the ROR data of the selected row (NOTE that only 1 row can be selected)
        selected_ror_data <- ror_df()[input$ror_results_rows_selected, c("RORID", "Name", "city", "country_code")]
        selected_ror_data$Name <- gsub('\n','<br>',stringr::str_wrap(selected_ror_data$Name, width = 50))
        selected_ror_data$country_code <- iso_to_combined(selected_ror_data$country_code)
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
        selected_ror_data$country_code <- iso_to_combined(selected_ror_data$country_code)
        # update in the funding data table for the selected funders
        current_df <- funding_data_out()
        current_df[input$selected_funders, c("rorid", "org_name", "org_country")] <- selected_ror_data
        funding_data_in(current_df)
      }
      # close the modal
      shiny::removeModal()
    })



    # ORCID SEARCH ---------------------------------------------------------------
    # toggle search button: only enable if we have a country and search string
    shiny::observe({
      shinyjs::toggleState(id = "btn_orcid_search",
                           condition = !(input$orcid_search_string==""))
    })

    # reactiveVal (responding to search orcid from string or from table buttons)
    orcid_df <- shiny::reactiveVal()

    # run ORCID API search on provided search string
    shiny::observeEvent(input$btn_orcid_search, {
      shiny::req(input$orcid_search_string)

      # run the ORCID API request with the input search string
      res_df <- orcid_api_request(search_string = input$orcid_search_string)
      res_df$search_terms <- input$orcid_search_string

      # update the reactiveVal
      orcid_df(res_df)
    })

    # run ORCID API search on author table
    shiny::observeEvent(input$btn_orcid_tbl, {
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
    output$orcid_instr <- shiny::renderUI({
      if (is.null(orcid_df())) {
        shiny::tags$i("Run ORCID search...")
      } else {
        shiny::tags$i("Click on a row to select and transfer the ORCID data to the table below.
                Email and affiliation data will only be transferred if not yet provided in the table.")
      }
    })

    # render ORCID DT
    output$orcid_results <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(orcid_df()), "No data to show"))
      DT::datatable(orcid_df() |> dplyr::select(search_terms, last_name, first_name, email, orcid, org_name, other_names),
                    extensions = 'RowGroup',
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    fillContainer = TRUE,
                    options = list(pageLength = 5,
                                   rowGroup = list(dataSrc = 0)))
    })

    # observe ORCID row selection: open modal
    shiny::observeEvent(input$orcid_results_rows_selected, {
      shiny::showModal(shiny::modalDialog(
        title = "Transfer ORCID data",
        shiny::tagList(
          shiny::p("Do you want to transfer the selected ORCID data to the author table?"),
          shiny::radioButtons(
            ns("sel_author_orc"),
            label = "Select author to update:",
            choices = c(get_author_choices(author_data_out()), "Add new author" = "new"),
            selected = "new"
          )
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_trans_orcid"), "Transfer")
        )
      ))
    })

    # transfer ORCID data to author table on confirm transfer
    shiny::observeEvent(input$btn_trans_orcid, {
      if (!is.null(input$sel_author_orc) && !is.null(input$orcid_results_rows_selected)) {
        # get the ORCID data of the selected row (NOTE: only 1 row can be selected)
        sel_orcid_data <- orcid_df()[input$orcid_results_rows_selected,]
        sel_orcid_data$org_name <- gsub('\n','<br>',stringr::str_wrap(sel_orcid_data$org_name, width = 50))
        # update in the author data table for the selected author (NOTE: only 1 can be selected)
        current_df <- author_data_out()
        if (input$sel_author_orc == "new"){
          row <- nrow(current_df) + 1
          current_df[row,] <- rxs2tria:::create_empty_df(aut_tbl_props_full, nrows=1)
          current_df[row, "author_nr"] <- row
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
      shiny::removeModal()
    })




    # AUTHOR RHANDSONTABLE -----------------------------------------------------
    # initialize reactiveVal (responding to add/delete row, ror/orcid transfer, file upload)
    author_data_in <- shiny::reactiveVal()

    # render editable table
    output$author_table <- rhandsontable::renderRHandsontable({
      shiny::validate(shiny::need(!is.null(author_data_in()), "No data to show."))
      render_meta_hot(author_data_in(), aut_tbl_props, aut_renderers)
    })


    # create dataframe reactive to hot updates (keeps combined country values for display)
    author_data_out <- shiny::reactive({
      df <- rhandsontable::hot_to_r(input$author_table)
      if (is.null(df)) df <- author_data_in() # tab never rendered: fall back to loaded data

      if (!is.null(df) && nrow(df)>0) {
        df <- df |>
          dplyr::mutate(contact_person = as.logical(.data$contact_person))
      }
      df
    })

    # output-only reactive: converts org_country from combined display value to ISO code
    author_data_export <- shiny::reactive({
      df <- author_data_out()
      if (!is.null(df$org_country)) {
        df$org_country <- combined_to_iso(df$org_country)
      }
      df
    })

    # observe add row button
    shiny::observeEvent(input$btn_add_author, {
      new_row <- rxs2tria:::create_empty_df(aut_tbl_props_full, nrows=1)
      current_df <- author_data_out()
      if (!is.null(current_df)) {
        new_row$author_nr <- nrow(current_df) + 1 #as.integer(max(current_df$author_nr, na.rm = TRUE) + 1)
        current_df[nrow(current_df)+1,] <- new_row
        author_data_in(current_df)
      } else {
        new_row$author_nr <- 1
        author_data_in(new_row)
      }

    })

    # observe delete row button
    shiny::observeEvent(input$btn_del_author, {
      shiny::req(nrow(author_data_out()) > 1)
      current_df <- author_data_out()
      current_df <- current_df[-nrow(current_df), ]
      author_data_in(current_df)
    })

    # TODO: import data from file
    shiny::observeEvent(input$file_authors, {
      #show_ht_import_modal(ns, 'import_aut_data')
    })

    # observe confirm import data button
    shiny::observeEvent(input$import_aut_data, {
      shiny::removeModal()
    })


    # FUNDING RHANDSONTABLE ----------------------------------------------------
    # initialize reactiveVal (responding to add/delete row, ror transfer, file upload)
    funding_data_in <- shiny::reactiveVal()

    # Render editable table
    output$funding_table <- rhandsontable::renderRHandsontable({
      shiny::validate(shiny::need(!is.null(funding_data_in()), "No data to show."))
      render_meta_hot(funding_data_in(), fund_tbl_props, fund_renderers)
    })

    # create dataframe reactive to hot updates
    funding_data_out <- shiny::reactive({
      df <- rhandsontable::hot_to_r(input$funding_table)
      if (is.null(df)) df <- funding_data_in() # tab never rendered: fall back to loaded data
      df
    })

    funding_data_export <- shiny::reactive({
      df <- funding_data_out()
      if (!is.null(df$org_country)) {
        df$org_country <- combined_to_iso(df$org_country)
      }
      df
    })

    # observe add row button
    shiny::observeEvent(input$btn_add_fund, {
      new_row <- rxs2tria:::create_empty_df(fund_tbl_props_full, nrows=1)
      current_df <- funding_data_out()
      if (!is.null(current_df)) {
        current_df[nrow(current_df)+1,] <- new_row
        funding_data_in(current_df)
      } else {
        funding_data_in(new_row)
      }

    })

    # observe delete row button
    shiny::observeEvent(input$btn_del_fund, {
      shiny::req(nrow(funding_data_out()) > 1)
      current_df <- funding_data_out()
      current_df <- current_df[-nrow(current_df),]
      funding_data_in(current_df)
    })


    # import data from file: show modal for confirmation
    shiny::observeEvent(input$file_funding,{
      #show_ht_import_modal(ns, 'import_fund_data')
    })

    # observe confirm import data button
    shiny::observeEvent(input$import_fund_data, {
      shiny::removeModal()
    })


    # DOI SEARCH ---------------------------------------------------------------
    # toggle add button: only enable if we have a DOI or citation to add
    # TODO: add tria lookup?
    shiny::observe({
      shinyjs::toggleState(id = "btn_cite_search",
                           condition = (input$doi_string != "" || input$citation_string != ""))
    })

    # doi_df: a reactive updated only in the event of the search button being clicked
    doi_df <- shiny::eventReactive(input$btn_cite_search, {
      # run the DOI/crossref API request with the input search string
      if (input$doi_string != "") {
        doi_res <- doi_api_request(input$doi_string)
        shiny::updateTextInput(session, "doi_string", value = "")
        return(doi_res)
      } else if (input$citation_string != "") {
        cite_res <- cr_api_request(input$citation_string)
        shiny::updateTextAreaInput(session, "citation_string", value = "")
        return(cite_res)
      }
      # TODO: tria ds search
    })

    # render instructions
    output$doi_instr <- shiny::renderUI({
      if (is.null(doi_df())) {
        shiny::tags$i("Run DOI search first...")
      } else {
        shiny::tags$i("Click on a row to select and transfer the DOI data to the tables below.")
      }
    })

    # render DOI DT
    output$doi_results <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(doi_df()), "No data to show"))
      DT::datatable(doi_df(),
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    fillContainer = TRUE,
                    options = list(paging = FALSE,
                                   searching = FALSE))
    })

    # observe DOI row selection: transfer data
    shiny::observeEvent(input$doi_results_rows_selected, {
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
    relres_data_in <- shiny::reactiveVal()

    # Render editable table
    output$relres_table <- rhandsontable::renderRHandsontable({
      shiny::req(relres_data_in())
      render_meta_hot(relres_data_in(), relres_tbl_props, relres_renderers)
    })

    # create dataframe reactive to hot updates
    relres_data_out <- shiny::reactive({
      df <- rhandsontable::hot_to_r(input$relres_table)
      if (is.null(df)) df <- relres_data_in() # tab never rendered: fall back to loaded data
      df
    })

    # observe add row button
    shiny::observeEvent(input$btn_add_res, {
      new_row <- rxs2tria:::create_empty_df(relres_tbl_props_full, nrows=1)
      current_df <- relres_data_out()
      if (!is.null(current_df)) {
        current_df[nrow(current_df)+1,] <- new_row  
        relres_data_in(current_df)
      } else {
        relres_data_in(new_row)
      }
 
    })

    # observe delete row button
    shiny::observeEvent(input$btn_del_res, {
      shiny::req(nrow(relres_data_out()) > 0)
      current_df <- relres_data_out()
      current_df <- current_df[-nrow(current_df),]
      relres_data_in(current_df)
    })


    # import data from file: show modal for confirmation
    shiny::observeEvent(input$file_relres,{
      #show_ht_import_modal(ns, 'import_res_data')
    })

    # observe confirm import data button
    shiny::observeEvent(input$import_res_data, {
      shiny::removeModal()
    })


    # VALIDATION CHECKS --------------------------------------------------------
    validation_checks <- shiny::reactive({
      results <- list()

      # 1) dataset input fields from the input validator
      iv_validated <- iv_gen$validate()
      results$dataset <- collect_validator_results(iv_validated, input_field_names, 'ds')
      # TODO: change to schema validation? what about other fields

      # 2) author table
      results$authors <- collect_hot_val_results(author_data_out(), aut_tbl_props)

      # 3) funding table
      results$funding <- collect_hot_val_results(funding_data_out(), fund_tbl_props)

      # 4) related resources
      results$related <- collect_hot_val_results(relres_data_out(), relres_tbl_props)

      collect_valcheck_df(results)
    })

    # set the color of the card header based on the validation results
    shiny::observe({
      shinyjs::toggleClass(id = "val_check_header", class = 'bg-secondary',
                           condition = nrow(validation_checks()) > 0)
    })

    output$validation_check <- shiny::renderUI({
      render_valcheck_ui(validation_checks())
      # TODO:
      # add warning messages before switching tab or saving data
    })

    # Next button
    shiny::observeEvent(input$btn_next, {
      bslib::nav_select(id = 'tabs', selected = tab_site, session = main_session)
    })

    # Previous button
    shiny::observeEvent(input$btn_prev, {
      bslib::nav_select(id = 'tabs', selected = tab_start, session = main_session)
    })


    # TODO:
    # additional validations
    # read data from file: append or warn if there is already data?
    # from / to csv or xslx? sheets?
    # more details on validation errors (nr characters, pattern, etc.)
    # orcid transfer: what if names don't match?

    # output$testing <- shiny::renderPrint({
    #   #validation_checks()
    # })

    ds_data_out <- shiny::reactive({
      tibble::tibble(
        ds_name = input$ds_name,
        ds_title = input$ds_title,
        description = input$description,
        ds_access = input$ds_access,
        ds_license = input$ds_license,
        embargoed_until = if (input$ds_access == "restricted") input$embargoed_until else "",
        acknowledgements = input$acknowledgements
      )
    })

    return(
      list(
        dataset_tbls  = list(
          dataset = ds_data_out,
          authors = author_data_export,
          funding = funding_data_export,
          related = relres_data_out,
          resources = shiny::reactive(dataset_tbls_in()$resources)
        ),
        val_check = validation_checks
      )
    )

  })
}
