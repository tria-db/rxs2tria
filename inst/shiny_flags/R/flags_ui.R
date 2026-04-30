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
        label_with_tt( # TODO: improve popover labels, add to all inputs
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
      shiny::hr(class = "hr-slim"),

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

      shiny::tags$div(
        style = "display: flex; flex-direction: column;  gap: 4px;",
        shiny::actionButton(ns("save_btn"), "Save ring flags", icon = shiny::icon("save"),
                    class = "btn-tert", width = "100%"),
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 4px;",
          shiny::actionLink(ns("save_settings_btn"), NULL, shiny::icon("gear")),
          shiny::tags$span("Save settings", style = "font-size: 0.8em; ")
        ),

      )
      
      

    ), # end of sidebar


    # MAIN PANEL ---------------------------------------------------------------
    # main plot card
    bslib::card(
      fill = FALSE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center p-1",
        style = "background-color: #CCE0E0", # "#CCE0E0" is prim_col_grad[4]
        shiny::div(
          style = "display: flex; align-items: center; gap: 8px;",
          shiny::strong("Selected woodpiece:"),
          shiny::uiOutput(ns("selwp")), # reactive output to contain selwp input
        ),
        shiny::actionLink(
          ns("plot_settings"), "",
          icon = shiny::icon("gear"))
      ),
      bslib::card_body(
        class = "p-0",
        fillable = FALSE,
        shiny::uiOutput(ns("main_plot_ui"))
      ),
      bslib::card_footer(
        class = "p-1",
        shiny::uiOutput(ns("corr_info_ui"))
      )
    ),

    # hot card to edit ring flags of selected image/year
    bslib::card(
      fill = FALSE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center p-1",
        style = "background-color: #CCE0E0; position: relative;",
        shiny::div(
          style = "display: flex; align-items: center; gap: 8px;",
          shiny::strong("Selected image:"),
          shiny::uiOutput(ns("selimg"))
        ),
        shiny::div(
          style = "position: absolute; left: 50%; transform: translateX(-50%); max-width: 40%; overflow: hidden; white-space: nowrap; text-overflow: ellipsis;",
          shiny::uiOutput(ns("selcomment"))
        ),
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          bslib::tooltip(
            shiny::actionLink(ns("prev_img"), NULL, icon = shiny::icon("angles-left")),
            "Go to earlier image"
          ),
          bslib::tooltip(
            shiny::actionLink(ns("prev_ring"), NULL, icon = shiny::icon("angle-left")),
            "Go to earlier ring"
          ),
          bslib::tooltip(
            shiny::actionLink(ns("next_ring"), NULL, icon = shiny::icon("angle-right")),
            "Go to next ring"
          ),
          bslib::tooltip(
            shiny::actionLink(ns("next_img"), NULL, icon = shiny::icon("angles-right")),
            "Go to next image"
          ),
          shiny::tags$span(style = "color: #aaa;", "|"),
          bslib::tooltip(
            shiny::actionLink(ns("show_image"), NULL, icon = shiny::icon("image")),
            "Open image file"
          ),
          shiny::tags$span(style = "color: #aaa;", "|"),
          shiny::actionLink(ns("tbl_settings"), NULL, icon = shiny::icon("gear"))
        )
      ),
      bslib::card_body(
        class = "p-0",
        fillable = FALSE,
        rhandsontable::rHandsontableOutput(ns("img_flags"))
      ),
      bslib::card_footer(
        class = "p-1"
      )
    ),

    shiny::verbatimTextOutput(ns("debug"))
  )
}
