#' Launch the metadata Shiny app
#'
#' @param server_run Logical; if `TRUE`, runs the app in server mode (for
#'   hosting on a Shiny server).
#' @export
launch_metadata_app <- function(server_run = FALSE) {
  rlang::check_installed(
    c("bslib", "data.tree", "DT", "htmlwidgets", "httr", "leaflet", "rcrossref", "rhandsontable",
      "sf", "shiny", "shinyjs", "shinyTree", "shinyvalidate"),
    reason = "required to run the metadata Shiny app"
  )
  checkmate::assert_flag(server_run)
  options(rxs2tria.shinymeta.server_run = server_run)

  app_dir <- system.file("shiny_meta", package = "rxs2tria")
  if (app_dir == "") {
    cli::cli_abort("Could not find the Shiny app directory. Try reinstalling the package.")
  }
  shiny::runApp(app_dir)
}


#' Launch the ring flags Shiny app
#'
#' The app returns the edited data when it is closed with "Close app" or by
#' closing the browser tab. Saving to file in the app keeps the edits if the
#' app or the R session stops otherwise.
#'
#' @returns Invisibly, a list with `rings`, the rings data with the ring flags
#'   as saved to file, and `images`, the `QWAimages` with the `comment_handled`
#'   column if image comments were reviewed, otherwise `NULL`; `NULL` if no
#'   data was loaded.
#' @export
launch_flags_app <- function() {
  rlang::check_installed(
    c("bslib", "collapse", "htmlwidgets", "plotly", "reactable",
      "rhandsontable", "shiny", "shinyjs", "shinyvalidate"),
    version = c("0.9.0", rep(NA, 8)),
    reason = "required to run the flags Shiny app"
  )
  app_dir <- system.file("shiny_flags", package = "rxs2tria")
  if (app_dir == "") {
    cli::cli_abort("Could not find the Shiny app directory. Try reinstalling the package.")
  }
  shiny::shinyOptions(launch_wd = getwd())
  invisible(shiny::runApp(app_dir))
}
