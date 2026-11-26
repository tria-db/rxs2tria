flags_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(

    # SIDEBAR  -----------------------------------------------------------------
    sidebar = bslib::sidebar(
      title = "Data & settings",

      # button to load input data ----
      shiny::actionButton(
        ns("open_input_modal"),
        "Load input data",
        icon = shiny::icon("upload"),
        class = "btn-secondary"
      ),

      # inputs to filter/subset input data ----
      shinyjs::hidden( # start hidden
        shiny::selectizeInput(
          ns("filt_species"), "Select species",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "All species")
        ),
        shiny::selectizeInput(
          ns("filt_site"), "Select site(s)",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "All sites")
        )
      ),
      shiny::selectInput(
        ns("filt_wp"), "Subset woodpiece(s)",
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
        icon = shiny::icon("arrows-rotate"), class = "btn-outline-secondary"
      ),
      shiny::hr(class = "hr-slim"),

      # button to export rwl ----
      shiny::actionButton(ns("export_rwl_btn"), "Export rwl",
        icon = shiny::icon("file-export"),
        class = "btn-outline-secondary", width = "100%"),

      shiny::hr(class = "hr-slim"),

      # button to save and/or download edited data ----
      shiny::tags$div(
        style = "display: flex; flex-direction: column;  gap: 4px;",
        bslib::tooltip(
          shiny::actionButton(ns("save_btn"), "Save ring flags", icon = shiny::icon("save"),
                      class = "btn-secondary", width = "100%"),
          "No changes",
          id = ns("save_btn_tt")
        ),
        shiny::actionLink(ns("save_as_btn"), "Save as…", class = "small",
                          style = "align-self: flex-start;")
      ),

      shiny::hr(class = "hr-slim"),

      # button to close the app and return the edited data to R ----
      shiny::actionButton(ns("close_btn"), "Close app", icon = shiny::icon("xmark"),
                          class = "btn-outline-secondary", width = "100%")
      
      

    ), # end of sidebar


    # MAIN PANEL ---------------------------------------------------------------
    # main plot card
    bslib::card(
      fill = FALSE,
      class = "card-workbench",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::div(
          class = "bench-title",
          shiny::strong("Selected woodpiece:"),
          shiny::uiOutput(ns("selwp")), # reactive output to contain selwp input
        ),
        shiny::div(
          class = "bench-controls",
          bench_button(ns("plot_settings"), "layer-group",
                       "Show or hide subplots", label = "Subplots")
        )
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
      class = "card-workbench",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center position-relative",
        shiny::div(
          class = "bench-title",
          shiny::strong("Selected image:"),
          shiny::uiOutput(ns("selimg"))
        ),
        shiny::div(
          class = "bench-comment",
          shiny::uiOutput(ns("selcomment"))
        ),
        shiny::div(
          class = "bench-controls",
          # buttons mirror the arrow keys: up/down rings, left/right images
          shiny::div(
            class = "bench-group",
            shiny::span(class = "bench-label", "Ring"),
            bench_button(ns("prev_ring"), "chevron-up", "Earlier ring (arrow up)"),
            bench_button(ns("next_ring"), "chevron-down", "Next ring (arrow down)")
          ),
          shiny::div(
            class = "bench-group",
            shiny::span(class = "bench-label", "Image"),
            bench_button(ns("prev_img"), "chevron-left", "Earlier image (arrow left)"),
            bench_button(ns("next_img"), "chevron-right", "Next image (arrow right)")
          ),
          bench_button(ns("show_image"), "image", "Open the image file",
                       label = "Open image"),
          bench_button(ns("img_settings"), "gear",
                       "Image settings: auto-open, preferred image"),
          bench_button(ns("tbl_settings"), "table-columns",
                       "Select which flag columns are shown", label = "Columns"),
          bslib::popover(
            shiny::actionButton(ns("tbl_help"), "Help",
                                icon = shiny::icon("circle-question"),
                                class = "btn-sm btn-bench"),
            title = "Help: table columns and buttons",
            placement = "bottom",
            options = list(customClass = "popover-help"),
            shiny::tags$ul(
              class = "ps-3 mb-0",
              shiny::tags$li(shiny::strong("duplicate_ring:"), " ring also appears in an overlapping image (read-only)."),
              shiny::tags$li(shiny::strong("exclude_dupl:"), " for duplicate rings, which copy is not used. Only one copy per ring is kept;
                             unticking here uses the ring from this image and excludes it in the other image."),
              shiny::tags$li(shiny::strong("exclude_issues:"), " exclude the ring from the analysis because of quality issues
                             (e.g. incomplete_ring, cracks, decay, ...). Specify the ", shiny::strong("affected_tissue"),
                             " (all, ew or lw) and tick the relevant issue columns to document the reason."),
              shiny::tags$li(shiny::strong("Ring"), " ", shiny::icon("chevron-up"), shiny::icon("chevron-down"),
                             " and ", shiny::strong("Image"), " ", shiny::icon("chevron-left"), shiny::icon("chevron-right"),
                             " go to the earlier/next ring or image, as do the arrow keys (up/down for rings, left/right for images;
                             press Esc first to leave the table, Enter to select the ring in the table).")
            )
          )
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
