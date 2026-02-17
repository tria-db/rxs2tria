flags_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(

    # SIDEBAR  -----------------------------------------------------------------
    sidebar = bslib::sidebar(

      # button to load input data ----
      shiny::actionButton(
        shiny::ns("open_input_modal"),
        "Load input data",
        icon = shiny::icon("upload")
      ),
      shiny::hr(class = "hr-slim"),

      # inputs to filter/subset input data ----
      shiny::strong("Subset data:"),
      shiny::selectInput(
        shiny::ns("filt_species"), "Choose species",
        choices = NULL, multiple = FALSE, selectize = TRUE
      ),
      shiny::selectInput(
        shiny::ns("filt_site"), "Filter site(s)",
        choices = NULL, multiple = TRUE, selectize = TRUE
      ),
      shiny::selectInput(
        shiny::ns("filt_wp"), "Filter woodpiece(s)",
        choices = NULL, multiple = TRUE, selectize = TRUE
      ),
      shiny::hr(class = "hr-slim"),

      # inputs to customize the plot ----
      shiny::strong("Plot settings:"),
      shiny::selectInput(
        shiny::ns("sel_param"), "Select QWA parameter",
        choices = NULL, multiple = FALSE, selectize = TRUE
      ),
      shiny::selectInput(
        shiny::ns("sel_sector"), "Select ring sector",
        choices = NULL, multiple = FALSE, selectize = TRUE
      ),
      shinyjs::disabled( # TODO: implement the detrending
        shiny::checkboxInput(
          shiny::ns("apply_detrend"), "Apply 32-years spline detrend",
          value = FALSE
        )
      ),
      shiny::radioButtons(
        shiny::ns("sel_mean"), "Select mean type",
        choices = c("mean", "tbrm"), inline = TRUE, selected = "mean"
      ),
      shiny::checkboxInput(
        shiny::ns("show_mean"), "Show mean curve",
        value = FALSE
      ),
      shinyjs::disabled( # TODO: implement the highlighting of selected issues
        shiny::selectInput(
          shiny::ns("sel_highlight"), "Highlight rings with these features",
          choices = list(
            "Overlaps" = c("Duplicate ring" = "duplicate_ring"),
            "Issues" = c(disqual_issues, technical_issues, other_issues),
            "Features" = discrete_features),
          multiple = TRUE, selectize = TRUE
        )
      ),
      shiny::hr(class = "hr-slim"),

      # inputs related to excluded / plot updates ----
      shiny::checkboxInput(
        shiny::ns("show_excl"),
        label_with_pop( # TODO: improve popover labels, add to all inputs
          "Include excluded rings",
          "Click update plot to include/exclude newly edited rings (pink markers)"),
        value = FALSE
      ),
      # TODO: change to radio buttons to highlight better?
      # TODO: have a ring edit mode and an overview mode? in overview, can
      # highlight all points with issues / features?
      # TODO: update plot on changes in show_excl?
      shiny::actionButton(
        shiny::ns("apply_changes"), "Update excluded in plot",
        icon = shiny::icon("arrows-rotate"), class = "btn-tert"
      ),
      hr(class = "hr-slim"),

      # button to save and/or download edited data ----
      # TODO: what about saving in between? what about saving if app crashes?
      shiny::downloadButton(
        shiny::ns("save_data"), "Download new ring data",
        class = "btn-tert"
      ),
      shiny::actionButton(
        shiny::ns("close_app"), "Close app",
        icon = shiny::icon("xmark"), class = "btn-danger"
      ),

    ), # end of sidebar


    # MAIN PANEL ---------------------------------------------------------------
    # main plot card
    bslib::card(
      #max_height = "750px",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center p-1",
        style = "background-color: #CCE0E0", # "#CCE0E0" is prim_col_grad[4]
        shiny::uiOutput(shiny::ns("selwp")), # reactive output to contain selwp input
        shiny::actionLink(
          shiny::ns("plot_settings"), "",
          icon = shiny::icon("gear"))
      ),
      bslib::card_body(
        class = "p-0",
        uiOutput(ns("main_plot_ui"))
      )
    ),

    # hot card to edit ring flags of selected image/year
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center p-1",
        style = "background-color: #CCE0E0",
        shiny::uiOutput(shiny::ns("selimg")), # reactive output (selimg title)
        shiny::div(
          shiny::actionButton(shiny::ns("show_image"), "",
                              icon = shiny::icon("image")),
          shiny::actionButton(shiny::ns("prev_ring"), "",
                              icon = shiny::icon("arrow-left")),
          shiny::actionButton(shiny::ns("next_ring"), "",
                              icon = shiny::icon("arrow-right")),
          shiny::actionLink(shiny::ns("tbl_settings"), "",
                            icon = shiny::icon("gear"))
        )
      ),
      bslib::card_body(class = "p-0",
        rhandsontable::rHandsontableOutput(shiny::ns("img_flags"))
      )
    ),


    shiny::verbatimTextOutput(shiny::ns("debug"))
  )
}
