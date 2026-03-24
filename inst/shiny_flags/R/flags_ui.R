flags_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(

    # SIDEBAR  -----------------------------------------------------------------
    sidebar = bslib::sidebar(

      # button to load input data ----
      shiny::actionButton(
        ns("open_input_modal"),
        "Load input data",
        icon = shiny::icon("upload")
      ),
      shiny::hr(class = "hr-slim"),

      # inputs to filter/subset input data ----
      shiny::strong("Subset data:"),
      shinyjs::hidden( # start hidden
        shiny::selectInput(
          ns("filt_species"), "Choose species",
          choices = NULL, multiple = FALSE, selectize = TRUE
        ),
        shiny::selectInput(
          ns("filt_site"), "Filter site(s)",
          choices = NULL, multiple = TRUE, selectize = TRUE
        )
      ),
      shiny::selectInput(
        ns("filt_wp"), "Filter woodpiece(s)",
        choices = NULL, multiple = TRUE, selectize = TRUE
      ),
      shiny::hr(class = "hr-slim"),

      # inputs to customize the plot ----
      shiny::strong("Plot settings:"),
      shiny::selectInput(
        ns("sel_param"), "Select QWA parameter",
        choices = NULL, multiple = FALSE, selectize = TRUE
      ),
      shinyjs::hidden( # start hidden
        shiny::selectInput(
          ns("sel_sector"), "Select ring sector",
          choices = NULL, multiple = FALSE, selectize = TRUE
        )
      ),
    #shinyjs::disabled( # TODO: implement the detrending
        shiny::checkboxInput(
          ns("apply_detrend"), "Apply 32-years spline detrend",
          value = FALSE
        #)
      ),
      shiny::radioButtons(
        ns("sel_mean"), "Select mean type",
        choices = c("mean", "tbrm"), inline = TRUE, selected = "mean"
      ),
      shiny::checkboxInput(
        ns("show_mean"), "Show mean curve",
        value = FALSE
      ),
      shinyjs::disabled( # TODO: implement the highlighting of selected issues
        shiny::selectInput(
          ns("sel_highlight"), "Highlight rings with these features",
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
        ns("show_excl"),
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
        ns("apply_changes"), "Update excluded in plot",
        icon = shiny::icon("arrows-rotate"), class = "btn-tert"
      ),
      hr(class = "hr-slim"),

      # button to save and/or download edited data ----
      # TODO: what about saving in between? what about saving if app crashes?
      # shiny::downloadButton(
      #   ns("save_data"), "Download new ring data",
      #   class = "btn-tert"
      # ),
      # shiny::actionButton(
      #   ns("close_app"), "Close app",
      #   icon = shiny::icon("xmark"), class = "btn-danger"
      # ),

      tags$div(
        style = "display: flex; flex-direction: column;  gap: 4px;",
                shiny::actionButton(ns("save_btn"), "Save ring flags", icon = shiny::icon("save"), 
                            class = "btn-tert", width = "100%"),
        tags$div(
          style = "display: flex; align-items: center; gap: 4px;",
          shiny::actionLink(ns("save_settings_btn"), NULL, shiny::icon("gear")),
          tags$span("Save settings", style = "font-size: 0.8em; ")
        ),

      )
      
      

    ), # end of sidebar


    # MAIN PANEL ---------------------------------------------------------------
    # main plot card
    bslib::card(
      #max_height = "750px",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center p-1",
        style = "background-color: #CCE0E0", # "#CCE0E0" is prim_col_grad[4]
        shiny::uiOutput(ns("selwp")), # reactive output to contain selwp input
        shiny::actionLink(
          ns("plot_settings"), "",
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
        shiny::uiOutput(ns("selimg")), # reactive output (selimg title)
        shiny::div(
          shiny::actionButton(ns("show_image"), "",
                              icon = shiny::icon("image")),
          shiny::actionButton(ns("prev_ring"), "",
                              icon = shiny::icon("arrow-left")),
          shiny::actionButton(ns("next_ring"), "",
                              icon = shiny::icon("arrow-right")),
          shiny::actionLink(ns("tbl_settings"), "",
                            icon = shiny::icon("gear"))
        )
      ),
      bslib::card_body(class = "p-0",
        rhandsontable::rHandsontableOutput(ns("img_flags"))
      )
    ),


    shiny::verbatimTextOutput(ns("debug"))
  )
}
