#' Launch the metadata Shiny app
#'
#' @param server_run Logical; if `TRUE`, runs the app in server mode (for
#'   hosting on a Shiny server).
#' @export
launch_metadata_app <- function(server_run = FALSE) {
  rlang::check_installed(
    c("bslib", "data.tree", "DT", "htmlwidgets", "httr", "leaflet", "rhandsontable",
      "sf", "shiny", "shinyjs", "shinyTree", "shinyvalidate"),
    reason = "required to run the metadata Shiny app"
  )
  checkmate::assert_flag(server_run)
  options(rxs2xcell.shinymeta.server_run = server_run)

  app_dir <- system.file("shiny_meta", package = "rxs2tria")
  if (app_dir == "") {
    cli::cli_abort("Could not find the Shiny app directory. Try reinstalling the package.")
  }
  shiny::runApp(app_dir)
}


#' Launch the ring flags Shiny app
#'
#' @export
launch_flags_app <- function() {
  rlang::check_installed(
    c("bslib", "collapse", "htmlwidgets", "plotly", "reactable",
      "rhandsontable", "shiny", "shinyjs"),
    reason = "required to run the flags Shiny app"
  )
  app_dir <- system.file("shiny_flags", package = "rxs2tria")
  if (app_dir == "") {
    cli::cli_abort("Could not find the Shiny app directory. Try reinstalling the package.")
  }
  shiny::shinyOptions(launch_wd = getwd())
  shiny::runApp(app_dir)
}
