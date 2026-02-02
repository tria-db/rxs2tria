flags_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(

    # SIDEBAR  ---------------------------------------------------------------
    sidebar = bslib::sidebar(

      # load input data
      actionButton(ns("open_input_modal"), "Load input data", icon = icon("upload")),
      hr(class = "hr-slim"),
      # filters
      strong("Subset data:"),
      selectInput(ns("filt_species"), "Choose species", choices = NULL, multiple = FALSE, selectize = TRUE),
      selectInput(ns("filt_site"), "Filter site(s)", choices = NULL, multiple = TRUE, selectize = TRUE),
      selectInput(ns("filt_wp"), "Filter woodpiece(s)", choices = NULL, multiple = TRUE, selectize = TRUE),
      hr(class = "hr-slim"),
      # measurements
      strong("Plot settings:"),
      selectInput(ns("sel_param"), "Select QWA parameter", choices = NULL, multiple = FALSE, selectize = TRUE),
      selectInput(ns("sel_sector"), "Select ring sector", choices = NULL, multiple = FALSE, selectize = TRUE),
      shinyjs::disabled(checkboxInput(ns("apply_detrend"), "Apply 32-years spline detrend", value = FALSE)),
      radioButtons(ns("sel_mean"), "Select mean type", choices = c("mean", "tbrm"), inline = TRUE, selected = "mean"),
      # show additional curves / markers
      hr(class = "hr-slim"),
      checkboxInput(ns("show_mean"), "Show mean curve", value = FALSE),
      checkboxInput(ns("show_excl"),
                    label_with_pop(
                      "Show all excluded rings",
                      "Click update plot to include/exclude newly edited rings (pink markers)"),
                    value = TRUE),
      selectInput(ns("sel_highlight"), "Highlight rings with these features",
                  choices = list(
                    "Overlaps" = c("Duplicate ring" = "duplicate_ring"),
                    "Issues" = c(disqual_issues, technical_issues, other_issues),
                    "Features" = discrete_features),
                  multiple = TRUE, selectize = TRUE),
      # TODO: change to radio buttons to highlight better?
      # TODO: have a ring edit mode and an overview mode? in overview, can highlight all points with issues / features?

      hr(class = "hr-slim"),
      actionButton(ns("apply_changes"), "Apply edits to plot", icon = icon("arrows-rotate"),
                   class = "btn-tert"),

      hr(class = "hr-slim"),
      downloadButton(ns("save_data"), "Download new ring data",
                     class = "btn-tert"),
      hr(class = "hr-slim"),
      checkboxInput(ns("auto_open_image"), "Try to auto-open images", value = FALSE)

      #checkboxInput("tail_YTE", "show tail YTE", value = TRUE),

    ), # end of sidebar

    # MAIN PANEL ---------------------------------------------------------------
    # selectInput(ns("sel_wp"), "Select woodpiece to edit",
    #             choices = c("S22_LADE_L01", "S22_LADE_L02", "S22_LADE_L03"),
    #             multiple = FALSE, selectize = TRUE),
    bslib::card(
      min_height = "650px",
      max_height = "750px",
      bslib::card_body(class = "p-0",
        plotly::plotlyOutput(ns("main_plot")))
    ),
    # two action buttions to navigate rings
    bslib::layout_column_wrap(
      width = "217px", fixed_width = TRUE,
      actionButton(ns("prev_ring"), "Previous", icon = icon("arrow-left")),
      actionButton(ns("next_ring"), "Next", icon = icon("arrow-right"))
    ),


    verbatimTextOutput(ns("debug"))
  )
}



  # # MAIN CONTENT -----------------------------------------------------------
  # bslib::card(
  #   min_height = "450px",
  #   max_height = "450px",
  #   bslib::card_body(class = "p-0",
  #   plotly::plotlyOutput("ts_crn_plot"))
  # ),
  #
  # # SELECTED RING CARD
  # shinyjs::hidden(
  #   div(id = "ring_editor_card",
  #
  #     bslib::navset_card_pill(
  #       id = "sel_ring_navset",
  #       selected = "edit_flags",
  #       title = uiOutput("sel_ring"), # selected ring image_lable and year
  #
  #       # EDITING THE RING FLAGS
  #       bslib::nav_panel(
  #         "Edit ring",
  #         value = "edit_flags",
  #         reactable::reactableOutput("ring_overview"),
  #         uiOutput("ring_info"),
  #         hr(),
  #
  #         bslib::layout_column_wrap(
  #           fill = FALSE,
  #           width = 1/2,
  #           heights_equal = "row",
  #
  #           bslib::layout_column_wrap(
  #             width = 1,
  #             fill = FALSE,
  #             heights_equal = "row",
  #             bslib::card(
  #               fill = FALSE,
  #               class = "clean-card-sec",
  #               bslib::layout_column_wrap(
  #                 width = 1/2,
  #                 radioButtons("sel_exclude", "Exclude ring from analysis?",
  #                              choices = c("yes", "no"), inline = TRUE,
  #                              selected = "no"),
  #                 div(
  #                   radioButtons("sel_affected",
  #                                label_with_pop("Select affected tissue",
  #                                               "For excluded rings, you can indicate if the issue affects primarily one part of the ring, i.e. measurements for the other may still be salvageable."),
  #                                choices = c("N/A" = "NA", "Earlywood" = "ew", "Latewood" = "lw", "All" = "all"),
  #                                inline = TRUE, selected = "N/A"),
  #                   uiOutput("warn_disq")
  #                 )
  #               ),
  #             ),
  #             bslib::card(
  #               class = "clean-card",
  #               fill = FALSE,
  #               checkboxGroupInput("sel_discrete",
  #                                  "Discrete features",
  #                                  choices = discrete_features,
  #                                  inline = TRUE),
  #             ),
  #             bslib::card(
  #               fill = FALSE,
  #               class = "clean-card",
  #               textAreaInput(
  #                 inputId = "sel_comment",
  #                 label = "Additional comments",
  #                 width = "100%",
  #                 placeholder = "Enter any additional notes regarding the selected ring here..."
  #               )
  #             )
  #
  #           ),
  #           bslib::card(
  #             class = "clean-card",
  #             strong("Compromising issues:"),
  #             checkboxGroupInput("sel_disqual",
  #                                "Disqualifying features",
  #                                choices = disqual_issues,
  #                                inline = TRUE),
  #             # div(
  #             #   id = "techn_reason_el",
  #             #   style = "display: flex; align-items: flex-start;",
  #             #   div(style = "width: 25px; padding-top: 0px;",
  #             #       icon("arrow-right-from-bracket", lib = "font-awesome")),
  #             #   div(style = "flex-grow: 1;",
  #             checkboxGroupInput(
  #               "sel_technical_exact",
  #               "Technical issues",
  #               choices = technical_issues,
  #               inline = TRUE
  #             ),
  #             #  )
  #             # )
  #             checkboxGroupInput(
  #               "sel_other_iss",
  #               "Other issues",
  #               choices = other_issues,
  #               inline = TRUE
  #             )
  #           ),
  #
  #           # bslib::layout_column_wrap(
  #           #   width = 1,
  #           #   fill = FALSE,
  #           #   heights_equal = "row",
  #           #
  #           # )
  #         ),
  #
  #         bslib::layout_column_wrap(
  #           width = "217px", fixed_width = TRUE,
  #           #actionButton("undo_sel_flags", "Undo current edits", icon = icon("arrow-left")),
  #           actionButton("reset_to_raw", "Reset to input", icon = icon("undo")),
  #           actionButton("show_image", "Open image", icon = icon("image")),
  #           actionButton("prev_ring", "Previous", icon = icon("arrow-left")),
  #           actionButton("next_ring", "Next", icon = icon("arrow-right"))
  #         )
  #
  #       ),
  #
  #       # COVERAGE PANEL
  #       bslib::nav_panel(
  #         "Show coverage",
  #         value = "show_coverage",
  #         reactable::reactableOutput("tree_overview"),
  #         plotOutput("cov_info")
  #         # verbatimTextOutput("coverage"),
  #         # uiOutput("coverage_ui")
  #
  #       )
  #     )
  #   )
  # ), # end of ring editor card

