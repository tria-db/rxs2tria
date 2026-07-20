site_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(

    # sidebar
    sidebar = bslib::sidebar(
      title = "Instructions",
      bslib::card(
        class = 'card-note',
        bslib::card_header(
          class = 'bg-primary',
          shiny::span(shiny::icon("exclamation", style = "color: white"),'Note')),
        "The tables here are partially filled with information from the input data you provided in the Start tab."
      ),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent',
        shiny::tags$li("Please provide information related to how the QWA data in this dataset were obtained and generated, at the site-, tree-, woodpiece- and slide-level."),),
      shiny::hr(),
      bslib::card(
        class = 'card-note',
        bslib::card_header(
          class = 'bg-primary',
          shiny::span(shiny::icon("exclamation", style = "color: white"),'Validation check summary')),
        shiny::uiOutput(ns('validation_check')),
      ),
      shiny::hr(),
      bslib::layout_columns(
        shiny::actionButton(ns('btn_prev'), 'Previous', icon = shiny::icon('angle-double-left')),
        shiny::actionButton(ns('btn_next'), 'Next', icon = shiny::icon('angle-double-right'))
      )
      # shiny::verbatimTextOutput(ns("testing")) # debug
    ),

    # main content
    bslib::accordion(
      open = c('Sites', 'Trees', 'Woodpieces', 'Slides'),

      bslib::accordion_panel(
        'Sites',

        # the map panel
        bslib::accordion(
          id = ns("map_acc"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server
          bslib::accordion_panel(
            "Map",
            leaflet::leafletOutput(ns("site_map"))
          )
        ),
        shiny::hr(),

        # the site info input table
        shiny::h5('Site information'),
        shiny::div(style="display: flex; justify-content: space-between; align-items: center;",
            shiny::span(shiny::tags$i("For valid site coordinates, markers are added to the map tool above.")),
            shiny::div(style = "margin-left: auto; margin-bottom: 0;",
            shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
                shiny::fileInput(ns('file_sites'), "Load site data from file", accept = c(".csv",".json"))))),
        rhandsontable::rHandsontableOutput(ns("site_table")),
        shiny::hr(),
        shiny::p(shiny::tags$i("Are (some of the) sites part of a network? If so, please provide the network information below.")),
        shinyjs::disabled(shiny::actionButton(ns('btn_add_nws'), 'Add site network', icon = shiny::icon('plus'))),
        DT::DTOutput(ns("networks"))
      ),

      bslib::accordion_panel(
        'Trees',
        shiny::h5('Tree information'),
        shiny::div(style='float: right',
          shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
            shiny::fileInput(ns('file_trees'), "Load tree data from file", accept = ".csv"))),
        shiny::br(),
        rhandsontable::rHandsontableOutput(ns("tree_table")),
      ),

      bslib::accordion_panel(
        'Woodpieces',
        shiny::h5('Woodpiece information'),
        shiny::div(style='float: right',
          shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
            shiny::fileInput(ns('file_wps'), "Load woodpiece data from file", accept = ".csv"))),
        shiny::br(),
        rhandsontable::rHandsontableOutput(ns("wp_table")),
      ),

      bslib::accordion_panel(
        'Slides',
        shiny::h5('Slide information'),
        shiny::div(style='float: right',
          shiny::div(
                title = "Coming soon",
                style = "pointer-events: none; opacity: 0.5;",
            shiny::fileInput(ns('file_slides'), "Load slide data from file", accept = ".csv"))),
        shiny::br(),
        rhandsontable::rHandsontableOutput(ns("slide_table")),
      )

    ), # end of main content accordion

    shiny::br(), shiny::br()

  ) # end of layout_sidebar
}
