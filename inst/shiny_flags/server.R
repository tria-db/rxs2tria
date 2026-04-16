server <- function(input, output, session) {
  flags_out <- flags_server('flags', session, comments_out)

  comments_out <- comments_server('comments', session, flags_out)

  # footer ---------------------------------------------------------------------
  # open contact information modal on click
  shiny::observeEvent(input$contact_link, {
    shiny::showModal(shiny::modalDialog(
      title = "Publisher and contact information",
      "Swiss Federal Institute for Forest, Snow and Landscape Research WSL",
      shiny::br(),
      "Zürcherstrasse 111",
      shiny::br(),
      "8903 Birmensdorf",
      shiny::br(),
      "Switzerland",
      shiny::br(),
      shiny::br(),
      shiny::strong("Contact:"), "Mara Nägelin, ",
      shiny::a(href = "mailto:mara.naegelin@wsl.ch","mara.naegelin(at)wsl.ch"),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  })
}
