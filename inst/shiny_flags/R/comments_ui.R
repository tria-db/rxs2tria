comments_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center p-1",
      style = "background-color: #CCE0E0",
      shiny::strong("Overview of comments"),
      shiny::checkboxInput(ns("show_all"), label = "incl. handled comments", value = FALSE)
    ),
    bslib::card_body(
      class = "p-0",
      reactable::reactableOutput(ns("progress_table")),
      #shiny::tableOutput(ns("progress_table")),
      shiny::verbatimTextOutput(ns("debug"))
    )
  )

  

}