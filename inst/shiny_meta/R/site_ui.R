site_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar
    sidebar = sidebar(
      title = "Instructions",
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Note')),
        "The tables here are partially filled with information from the input data you provided in the Start tab."
      ),
      hr(),
      tags$ol(
        class = 'custom-indent',
        tags$li("Please provide information related to how the QWA data in this dataset were obtained and generated, at the site-, tree-, woodpiece- and slide-level."),),
      hr(),
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Validation check summary')),
        uiOutput(ns('validation_check')),
      ),
      hr(),
      layout_columns(
        actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),
        actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
      ),
      verbatimTextOutput(ns("testing"))
    ),

    # main content
    accordion(
      open = c('Sites', 'Trees', 'Woodpieces', 'Slides'),

      accordion_panel(
        'Sites',

        # the map panel
        accordion(
          id = ns("map_acc"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server
          accordion_panel(
            "Map",
            leaflet::leafletOutput(ns("site_map"))
          )
        ),
        hr(),

        # the site info input table
        h5('Site information'),
        div(style="display: flex; justify-content: space-between; align-items: center;",
            span(tags$i("For valid site coordinates, markers are added to the map tool above.")),
            div(style = "margin-left: auto; margin-bottom: 0;",
                fileInput(ns('file_sites'), "Load site data from file", accept = c(".csv",".json")))),
        rhandsontable::rHandsontableOutput(ns("site_table")),
        hr(),
        p(tags$i("Are (some of the) sites part of a network? If so, please provide the network information below.")),
        actionButton(ns('btn_add_nws'), 'Add site network', icon = icon('plus')),
        DT::DTOutput(ns("networks"))
      ),

      accordion_panel(
        'Trees',
        h5('Tree information'),
        div(style='float: right',
            fileInput(ns('file_trees'), "Load tree data from file", accept = ".csv")),
        br(),
        rhandsontable::rHandsontableOutput(ns("tree_table")),
      ),

      accordion_panel(
        'Woodpieces',
        h5('Woodpiece information'),
        div(style='float: right',
            fileInput(ns('file_wps'), "Load woodpiece data from file", accept = ".csv")),
        br(),
        rhandsontable::rHandsontableOutput(ns("wp_table")),
      ),

      accordion_panel(
        'Slides',
        h5('Slide information'),
        div(style='float: right',
            fileInput(ns('file_slides'), "Load slide data from file", accept = ".csv")),
        br(),
        rhandsontable::rHandsontableOutput(ns("slide_table")),
      )

    ), # end of main content accordion

    br(),br()

    # export progress to file button
    # div(
    #   style = "text-align: center; margin-top: 20px;", # Centering and adding margin
    #   downloadButton(ns('btn_save'), "Export progress to file",
    #                  style = "font-size: 1.1rem; padding: 8px 15px;") # Bigger button styling
    # )
  ) # end of layout_sidebar
}

