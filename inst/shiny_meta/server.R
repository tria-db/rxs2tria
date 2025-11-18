library(magrittr)
library(bslib)

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
                          start_out$roxas_data, start_out$site_tbls,
                          countries_list)

  summary_server('summary', session,
                 start_out, dataset_out, site_out)


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

  #output for general debugging
  # output$debug <- renderPrint({
  #   str(start_out$dataset_tbls()$ds_data)
  # })


}





#tree_info <- tree_server('tree', session, start_info)
# RADIALNETWORK TRY
# Modified JavaScript code to send the selected node to Shiny for the networkD3 plot
# clickJS <- '
# d3.selectAll(".node").on("click", function(d){
#   Shiny.onInputChange("selected_node", d.data.name);
# })
# '

# dtree_list <- data.tree::ToListExplicit(dtree, unname = TRUE)
# dtree_list

# })

# output$radial_network <- networkD3::renderDiagonalNetwork({
#   onRender(networkD3::diagonalNetwork(dtree_list()), clickJS)
# })

# Display the selected node in verbatimTextOutput
# output$selectedNode <- renderPrint({
#   input$selected_node
# })

# SUNBURST TRY
# df_sb <- data.tree::ToDataFrameNetwork(dtree) %>%
#   dplyr::rename(parents = from, ids = to) %>%
#   dplyr::mutate(labels = stringr::str_split_i(ids, "/", -1))
# df_sb[df_sb$parents == 'dataset', 'parents'] = ""

# fig <- plotly::plot_ly(
#   df_sb, ids = ~ids, labels = ~labels, parents = ~parents,
#   type = 'sunburst',
#   insidetextorientation='tangential'
#   ) %>% plotly::layout(showlegend = TRUE)
# fig
#

# DENDROGRAM TRY
# tree_data <- ggdendro::dendro_data(as.dendrogram(dtree))
#
# p <- ggplot2::ggplot() +
#   ggplot2::geom_segment(data = tree_data$segments, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
#                colour = "blue", alpha = 0.5) +
#   ggplot2::geom_text(data = tree_data$labels,
#             ggplot2::aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
#   # ggplot2::geom_text(data = ggdendro::leaf_label(tree_data),
#   #           ggplot2::aes(x = x, y = y, label = label), vjust = 0.5, size = 2) +
#   ggplot2::coord_flip() +
#   ggdendro::theme_dendro()
# p

# shinyTree
#   # helper to get the path of the highest level selected in the tree
# get_selected_highest <- function(tree, ancestry = "", selected = c()) {
#   for (leaf in names(tree)){
#     a <- attr(tree[[leaf]], 'stselected', TRUE)
#     if (!is.null(a) && a == TRUE) {
#       selected <- c(selected, paste0(ancestry, "/", leaf))
#     } else {
#       selected <- get_selected_highest(tree[[leaf]], paste0(ancestry, "/", leaf), selected)
#     }
#   }
#   return(selected)
# }
