dataset_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(

    # sidebar ----
    sidebar = bslib::sidebar(
      title = "Instructions",

      shiny::tags$ol(
        class = 'custom-indent',
        shiny::tags$li("Please provide a name and description for your dataset and specify access rights.")
      ),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent', start = 2,
        shiny::tags$li("List all authors (data owners) of the dataset. You can use the
                ROR and ORCID search tools to look up required information on
                authors, affiliations and/or funding organizations.")
      ),
      bslib::card(
        class = 'card-note',
        bslib::card_header(
          class = 'bg-primary',
          shiny::span(shiny::icon("exclamation", style = "color: white"),'Note')),
        "The author numbers provided here will be used as
        the order of authorship."
      ),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent', start = 3,
        shiny::tags$li("Disclose any funding sources which provided financial support
                related to the generation, collection, curation or processing of
                the dataset.")
      ),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent', start = 4,
        shiny::tags$li("If applicable, list any publications or datasets which are
                 related to this dataset.")
      ),
      shiny::hr(),

      bslib::card(
        class = 'card-note',
        bslib::card_header(
          id = ns('val_check_header'),
          class = 'bg-primary',
          shiny::span(shiny::icon("exclamation", style = "color: white"),'Validation check summary')),
        shiny::uiOutput(ns('validation_check'))
      ),

      shiny::hr(),
      bslib::layout_columns(
        shiny::actionButton(ns('btn_prev'), 'Previous', icon = shiny::icon('angle-double-left')),
        shiny::actionButton(ns('btn_next'), 'Next', icon = shiny::icon('angle-double-right'))
      ),
      shiny::verbatimTextOutput(ns("testing"))
    ),


    # main content ----
    bslib::accordion(
      open = c('General Info','Authors & Funding', 'Related resources'),

      ## Dataset info
      bslib::accordion_panel(
        "General Info",
        shiny::h5('1. Dataset information'),
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::textInput(ns("ds_name"), "Dataset name", value = NA,
                    placeholder = "Specify a name for your dataset (max 64 char.)"),
          shiny::textAreaInput(ns("description"), "Description", rows = 6,
                        placeholder = "Provide a brief description of your dataset")
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::radioButtons(ns("ds_access"), "Dateset access rights",
                       choices = c("public", "restricted"),
                       selected = "public"),
          shiny::div(
            shiny::selectizeInput(ns("ds_license"),
                           "Select a license for the dataset",
                           choices = c("CC BY 4.0", "CC BY-SA 4.0", "CC BY-NC 4.0",
                                       "CC BY-NC-SA 4.0", "CC BY-ND 4.0", "CC BY-NC-ND 4.0",
                                       "CCO 1.0"),
                           selected = "CC BY 4.0",
                           options = list(create = TRUE)),
            shiny::span("CC licenses are preferred (cf. ",
                 shiny::tags$a("Creative Commons License Chooser", href="https://chooser-beta.creativecommons.org/", target='_blank'),
                 "for more information) but a different license option can be added if required.")
          )
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::textAreaInput(
            ns('acknowledgements'), "Acknowledgements", rows = 4,
            placeholder = "If applicable, acknowledge the contributions of specific colleagues, institutions, or agencies that aided these research efforts. Note that all authors and funding sources must be listed separately below."),
          shiny::conditionalPanel(
            condition = "input.ds_access == 'restricted'",
            shiny::dateInput(ns("embargoed_until"),
                      "Select an embargo date",
                      value = Sys.Date() + 365),
            shiny::span("After this date, the dataset will become publically available under the selected license."),
            ns=shiny::NS(id))
        ),
      ),

      ## Authors and funding
      bslib::accordion_panel(
        'Authors & Funding',

        ### search tools
        bslib::accordion(
          id = ns("search_tools"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server

          #### ROR search tool
          bslib::accordion_panel(
            'Organizations search tool (ROR)',
            bslib::layout_columns(
              bslib::card(
                class = "card-tert",
                shiny::selectInput(ns("ror_search_country"), "Select country",
                            choices = c(Choose='', setNames(countries_info$country_iso_code,
                                                            countries_info$combined)),
                            selectize = TRUE),
                shiny::textInput(ns("ror_search_string"), "Enter search string"),
                shiny::actionButton(ns("btn_ror_search"), "Search for ROR", class = "btn btn-info")
              ),
              bslib::card(
                class = "card-tert",
                shiny::h6('ROR search results'),
                shiny::htmlOutput(ns("ror_instr")),
                DT::DTOutput(ns("ror_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          ),

          #### ORCID search tool
          bslib::accordion_panel(
            'Authors search tool (ORCID)',
            bslib::layout_columns(
              bslib::card(
                class = "card-tert",
                shiny::textInput(ns("orcid_search_string"), "Enter search string", placeholder = "Search by name or ORCID..."),
                shiny::actionButton(ns("btn_orcid_search"), "Run search", class = "btn btn-info"),
                shiny::hr(),
                shiny::span(shiny::tags$i("If you have already entered author names in the table below, click this button to search for their ORCIDs.")),
                shiny::actionButton(ns("btn_orcid_tbl"), "Find ORCIDs for author table", class = "btn btn-info")
              ),
              bslib::card(
                class = "card-tert",
                shiny::h6('ORCID search results'),
                shiny::htmlOutput(ns("orcid_instr")),
                DT::DTOutput(ns("orcid_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          )
        ),


        ### Authors input table
        shiny::hr(),
        shiny::h5('2. Author information'),

        shiny::div(style="display: flex; justify-content: space-between; align-items: center;",
            shiny::div(style="display: flex; gap: 10px;",
                shiny::actionButton(ns("btn_add_author"), "Add author", style = "width: 130px",
                             class = "btn btn-secondary", icon = shiny::icon('plus', lib = "glyphicon")),
                shiny::actionButton(ns("btn_del_author"), "Delete author", style = "width: 130px",
                             class = "btn btn-secondary", icon = shiny::icon('trash', lib = "glyphicon"))),

            shiny::div(style = "margin-left: auto; margin-bottom: 0;",
              shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
                shiny::fileInput(ns('file_authors'), "Load author data from file", accept = ".csv")))
        ),

        shiny::br(),

        rhandsontable::rHandsontableOutput(ns("author_table")),
        shiny::br(),


        ### Funding info table
        shiny::hr(),
        shiny::h5('3. Funding information'),

        shiny::div(style="display: flex; justify-content: space-between; align-items: center;",
          shiny::div(style="display: flex; gap: 10px;",
             shiny::actionButton(ns("btn_add_fund"), "Add source", style = "width: 130px",
                          class = "btn btn-secondary", icon = shiny::icon('plus', lib = "glyphicon")),
             shiny::actionButton(ns("btn_del_fund"), "Delete source", style = "width: 130px",
                          class = "btn btn-secondary", icon = shiny::icon('trash', lib = "glyphicon"))),

            shiny::div(style = "margin-left: auto; margin-bottom: 0;",
              shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
                shiny::fileInput(ns('file_funding'), "Load funding data from file", accept = ".csv")))
        ),

        rhandsontable::rHandsontableOutput(ns("funding_table")),
        shiny::br()

      ),

      ## Related resources
      bslib::accordion_panel(
        'Related resources',
        shiny::h5('4. Related publications and datasets'),

        bslib::accordion(
          id = ns("doi_search_tool"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server

          #### DOI search tool
          bslib::accordion_panel(
            'Resources search tool (DOI)',
            bslib::layout_columns(
              bslib::card(
                class = "card-tert",
                shiny::textInput(ns("doi_string"), "Enter DOI", placeholder = "e.g., 10.3389/fpls.2016.00781"),
                shiny::textAreaInput(ns("citation_string"), "Or enter citation search terms (*)", "", rows = 3,
                              placeholder = "e.g., von Arx, G Quantitative Wood Anatomy Guidelines 2016"),
                shiny::textInput(ns("tria_id"), "Or enter QWA dataset ID", placeholder = "e.g., tria.001"),
                shiny::actionButton(ns("btn_cite_search"), "Search for resource", class = "btn btn-info"),
                shiny::p(shiny::tags$i("(*) querying the Crossref API might take a while."))
              ),
              bslib::card(
                class = "card-tert",
                shiny::h6('Resource search results'),
                shiny::htmlOutput(ns("doi_instr")),
                DT::DTOutput(ns("doi_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          )
        ),
        shiny::hr(),

        shiny::div(style="display: flex; justify-content: space-between; align-items: center;",
            shiny::div(style="display: flex; gap: 10px;",
                shiny::actionButton(ns("btn_add_res"), "Add resource", style = "width: 130px",
                             class = "btn btn-secondary", icon = shiny::icon('plus', lib = "glyphicon")),
                shiny::actionButton(ns("btn_del_res"), "Delete resource", style = "width: 130px",
                             class = "btn btn-secondary", icon = shiny::icon('trash', lib = "glyphicon"))),

            shiny::div(style = "margin-left: auto; margin-bottom: 0;",
              shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
                shiny::fileInput(ns('file_relres'), "Load related resource data from file", accept = ".csv")))
        ),

        rhandsontable::rHandsontableOutput(ns("relres_table"))

      )
    ), # end of accordion

    shiny::br(), shiny::br()

  ) # end of layout_sidebar
}
