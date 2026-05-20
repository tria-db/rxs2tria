summary_ui <- function(id) {
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
        "The overview here combines all information provided in the previous tabs."
      ),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent',
        shiny::tags$li("Please consult the validation check summary and correct any issues in the data entry tabs."),),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent', start = 2,
        shiny::tags$li("When there are no more issues or if you are certain that you want to ignore the remaining messages,
                please click the 'Export data to file' button to download the final version of the metadata.")),
      shiny::hr(),
      shiny::tags$ol(
        class = 'custom-indent', start = 3,
        shiny::tags$li("You are now ready to share the file alongside the harmonized cells and rings data from the ", 
        shiny::code("rxs2tria"), " package with the TRIA database team, please contact ",
        shiny::tags$a(href = "mailto:tria@wsl.ch","tria@wsl.ch")),
      ),

      shiny::hr(),

      shiny::actionButton(ns('btn_prev'), 'Previous', icon = shiny::icon('angle-double-left')),

      shiny::hr()


    ),

    # main content
    bslib::accordion(
      open = c('Overview'),

      bslib::accordion_panel(
        'Overview',

        shiny::h5('Data overview'),

        bslib::card(
          class = 'card-note',
          bslib::card_header(
            class = 'bg-primary',
            shiny::span(shiny::icon("exclamation", style = "color: white"),'Validation check summary')),
          "List of all failed validation checks:",
          #shiny::uiOutput(ns('validation_check')),
          DT::DTOutput(ns('DT_valcheck'))
        ),
        shiny::hr(),

        shiny::verbatimTextOutput(ns("testing")),
        shiny::hr(),

        shiny::div(
          style = "text-align: center; margin-top: 20px;", # Centering and adding margin
          shiny::actionButton(ns('btn_save'), "Export data to file", icon = shiny::icon('download', lib = "glyphicon"),
                         style = "font-size: 1.1rem; padding: 8px 15px;") # Bigger button styling
        )

      )

    ) # end of main content
  ) # end of layout_sidebar
}
