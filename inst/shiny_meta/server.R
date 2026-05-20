# server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # general setup --------------------------------------------------------------
  # hotfix to ignore ctrl+click selection on hot for Mac bc it is buggy (cmd+click works as expected)
  # intercept and stop propagation of Control key events on Mac
  shinyjs::runjs("
    // Only apply on Mac
    if (navigator.platform.toUpperCase().indexOf('MAC') >= 0) {
      // Listen for Control keydown and keyup
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Control') {
          // Stop propagation to prevent Handsontable from using it
          e.stopImmediatePropagation();
        }
      }, true);
      document.addEventListener('keyup', function(e) {
        if (e.key === 'Control') {
          e.stopImmediatePropagation();
        }
      }, true);
    }
  ")


  # SERVER MODULES -------------------------------------------------------------
  start_out <- start_server('start', session)

  # TODO: add: species info, countries_sf vars as inputs
  dataset_out <- dataset_server('ds', session, start_out$dataset_tbls)

  site_out <- site_server('site', session,
                          start_out$images, start_out$site_tbls,
                          countries_list)

  summary_server('summary', session,
                 start_out, dataset_out, site_out)


  # footer ---------------------------------------------------------------------
  # open contact information modal on click
  shiny::observeEvent(input$contact_link, {
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

  #output for general debugging
  # output$debug <- renderPrint({
  #   str(start_out$dataset_tbls()$ds_data)
  # })


}

# other tree visualizations tried:
# radial network with networkD3::diagonalNetwork
# sunburst with plotly
# dendrogram with ggdendro::dendro_data / ggplot2 + ggdendro::theme_dendro()

