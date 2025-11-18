summary_ui <- function(id) {
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
        "The overview here combines all information provided in the previous tabs."
      ),
      hr(),
      tags$ol(
        class = 'custom-indent',
        tags$li("Please consult the validation check summary and correct any issues in the data entry tabs."),),
      hr(),
      tags$ol(
        class = 'custom-indent', start = 2,
        tags$li("When there are no more issues or if you are certain that you want to ignore the remaining messages,
                please click the 'Export data to file' button to download the final version of the metadata.")),
      hr(),
      tags$ol(
        class = 'custom-indent', start = 3,
        tags$li("You are now ready to share the file alongside the harmonized cells and rings data from the `rxs2tria` package with the TRIA database team,
                please contact ",
                tags$a(href = "mailto:mara.naegelin@wsl.ch","mara.naegelin(at)wsl.ch")),
      ),

      hr(),

      actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),

      hr()


    ),

    # main content
    accordion(
      open = c('Overview'),

      accordion_panel(
        'Overview',

        h5('Data overview'),

        card(
          class = 'card-note',
          card_header(
            class = 'bg-primary',
            span(icon("exclamation", style = "color: white"),'Validation check summary')),
          "List of all failed validation checks:",
          #uiOutput(ns('validation_check')),
          DT::DTOutput(ns('DT_valcheck'))
        ),
        hr(),

        verbatimTextOutput(ns("testing")),
        hr(),

        div(
          style = "text-align: center; margin-top: 20px;", # Centering and adding margin
          actionButton(ns('btn_save'), "Export data to file", icon = icon('download', lib = "glyphicon"),
                         style = "font-size: 1.1rem; padding: 8px 15px;") # Bigger button styling
        )

      )

    ) # end of main content
  ) # end of layout_sidebar
}
