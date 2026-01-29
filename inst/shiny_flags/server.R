server <- function(input, output, session) {
  flags_server('flags', session)


  # footer ---------------------------------------------------------------------
  # open contact information modal on click
  observeEvent(input$contact_link, {
    showModal(modalDialog(
      title = "Publisher and contact information",
      "Swiss Federal Institute for Forest, Snow and Landscape Research WSL",
      tags$br(),
      "Zürcherstrasse 111",
      tags$br(),
      "8903 Birmensdorf",
      tags$br(),
      "Switzerland",
      tags$br(),
      tags$br(),
      strong("Contact:"), "Mara Nägelin, ",
      tags$a(href = "mailto:mara.naegelin@wsl.ch","mara.naegelin(at)wsl.ch"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}
