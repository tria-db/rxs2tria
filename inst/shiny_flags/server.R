server <- function(input, output, session) {
  flags_server('flags', session)


  # footer ---------------------------------------------------------------------
  # open contact information modal on click
  shiny::observeEvent(input$contact_link, {
    shiny::showModal(shiny::modalDialog(
      title = "Publisher and contact information",
      "Swiss Federal Institute for Forest, Snow and Landscape Research WSL",
      htmltools::br(),
      "Zürcherstrasse 111",
      htmltools::br(),
      "8903 Birmensdorf",
      htmltools::br(),
      "Switzerland",
      htmltools::br(),
      htmltools::br(),
      htmltools::strong("Contact:"), "Mara Nägelin, ",
      htmltools::a(href = "mailto:mara.naegelin@wsl.ch","mara.naegelin(at)wsl.ch"),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  })
}
