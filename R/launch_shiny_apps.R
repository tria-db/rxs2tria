#' Launch the coverage Shiny app
#' @export
# dependencies:
# full: shiny, bslib,
# explicit: plotly, DT, dplyr, tidyr, ggplot2, stringr, shinyalert, sass, shinyjs
launch_coverage_app <- function() {
  app_dir <- system.file("shiny_YTE", package = "rxs2tria")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  shiny::runApp(app_dir)
}

#' Launch the metadata Shiny app
#' @param server_run Logical, if TRUE, runs the app in server mode (for hosting on shiny server).
#' @export
# full: shiny, bslib,
# explicit: DT, dplyr, shinyjs, reactable, shinyvalidata, data.tree, networkD3
launch_metadata_app <- function(server_run = FALSE) {
  #options(rxs2xcell.shinymeta.example_run = example_run)
  options(rxs2xcell.shinymeta.server_run = server_run)
  app_dir <- system.file("shiny_meta", package = "rxs2tria")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Reinstall the package.")
  }
  shiny::runApp(app_dir)
}
