library(shiny)
library(magrittr)
library(bslib)

# define the theme
theme <- bslib::bs_theme(
  version = 5,
  primary = prim_col, secondary = sec_col, info = tert_col,
  font_scale = 0.8,
  preset = "zephyr"
) |>
  bslib::bs_add_variables(
    "body-bg" = prim_col_grad[5],
    "focus-ring-color" = sec_col_grad[4],
    "prim-col-grad-1" = prim_col_grad[1],
    "prim-col-grad-2" = prim_col_grad[2],
    "prim-col-grad-3" = prim_col_grad[3],
    "prim-col-grad-4" = prim_col_grad[4],
    "prim-col-grad-5" = prim_col_grad[5],
    "prim-col-grad-6" = prim_col_grad[6],
    "sec-col-grad-1" = sec_col_grad[1],
    "sec-col-grad-2" = sec_col_grad[2],
    "sec-col-grad-3" = sec_col_grad[3],
    "sec-col-grad-4" = sec_col_grad[4],
    "sec-col-grad-5" = sec_col_grad[5],
    "sec-col-grad-6" = sec_col_grad[6],
    "tert-col-grad-1" = tert_col_grad[1],
    "tert-col-grad-2" = tert_col_grad[2],
    "tert-col-grad-3" = tert_col_grad[3],
    "tert-col-grad-4" = tert_col_grad[4],
    "tert-col-grad-5" = tert_col_grad[5],
    "tert-col-grad-6" = tert_col_grad[6]
  ) |>
  bslib::bs_add_rules(
    list(
      sass::sass_file("www/custom-theme.scss")
    )
  )

ui <- bslib::page_navbar(
  # preliminaries
  #shinyjs::useShinyjs(),

  # theme
  theme = theme,

  header = shinyjs::useShinyjs(),

  # TITLE ----------------------------------------------------------------------
  title = "rxs2tria: QWA data quality and climate signal explorer",

  # SIDEBAR  -------------------------------------------------------------------
  sidebar = bslib::sidebar(

    # load input data ----------------------------------------------------------
    actionButton("btn_input", "Load input data"),

    # TODO: loading climate data? in other tabs?
    #fileInput("file_clim", "Upload the 'climate' data", accept = c(".csv")),

    strong("Filter data for plot"),
    selectInput("sel_site", "Filter the sites to display", choices = NULL, multiple = TRUE),
    selectInput("sel_species", "Select the species to display", choices = NULL, multiple = FALSE),
    selectInput("sel_wp", "Select the woodpieces to display", choices = NULL, multiple = FALSE),
    hr(),

      strong("Plot settings"),
      selectInput("sel_param", "Choose QWA parameter to display", choices = NULL, multiple = FALSE),
      selectInput("sel_sector", "Select which ring sector to plot", choices = NULL, multiple = FALSE),
      checkboxInput("spline_det", "32-years spline detrend", value = FALSE),
      selectInput("mean_type", "Select the mean applied for crn",
                  choices = c("mean", "tbrm"), selected = "mean", multiple = FALSE),
      checkboxInput("show_excl", "show excluded rings", value = FALSE)
      #checkboxInput("tail_YTE", "show tail YTE", value = TRUE),

      #conditionalPanel(
       # condition = "input.tabs != 'Time Series with chrono'",


      #),

      # conditionalPanel(
      #   condition = "input.tabs == 'running RBT'",
      #   uiOutput("dynamic_rbt_slider")
      # ),

      # conditionalPanel(
      #   condition = "input.tabs == 'Time Series with chrono'",
      #  sliderInput("range_select", "Select Range of YEAR", min = 0, max = 2025, value = c(1850, 2025)),
      # ),

      # actionButton("calc_button", "Calculate Plots")
    ),

    bslib::nav_panel(
      "Time series with chrono",
      icon = icon("microscope", lib = "font-awesome"),

      # p("Plotly instructions:",
      #   tags$ul(
      #   tags$li("Hover over a point to see its details, click to select."),
      #   tags$li("Click on a legend item to toggle a curve's visibility."),
      #   tags$li("Double click on a legend item to isolate a curve."),
      #   tags$li("Axes can be shifted or zoomed on the plot directly.")
      # )),
      bslib::card(
        min_height = "450px",
        max_height = "450px",
        bslib::card_body(class = "p-0",
        plotly::plotlyOutput("ts_crn_plot"))
      ),

      # New shift controls
      # conditionalPanel(
      #   condition = "input.tabs == 'Time Series with chrono'",
        # fluidRow(
        #   column(4, numericInput("shift_amount", "Shift (rings):", value = 1, step = 1, min = 1)),
        #   column(4, actionButton("shift_left", "← Shift Left")),
        #   column(4, actionButton("shift_right", "Shift Right"))
        # ),
      # ),
      bslib::card(
        id = "sel_ring_card",
        class = "card-tert",
        style = "display: none;",  # Start hidden
        min_height = "450px",
        bslib::card_header(
          tagList(
            uiOutput("sel_ring"))),
        bslib::accordion(
          id = "ring_details",
          open = FALSE,
          bslib::accordion_panel(
            "Show image",
            uiOutput("image_ui")
          ),
          bslib::accordion_panel(
            "Show woodpiece coverage",
            uiOutput("coverage_ui")
          )
        ),

        #tags$i("A ring may exhibit one or more quality-related issues. These may or may not be grounds for
        #excluding the corresponding QWA measurements from analysis."),

        bslib::layout_column_wrap(
          fill = FALSE,
          width = 1/2,

          bslib::layout_column_wrap(
            width = 1,
            fill = FALSE,
            heights_equal = "row",
            bslib::card(
              fill = FALSE,
              class = "clean-card",
              radioButtons("sel_exclude", "Exclude ring from analysis?", choices = c("yes", "no"), inline = TRUE, selected = "no")
            ),
            bslib::card(
              class = "clean-card",
              #min_height = "250px",
              strong("Disqualifying features"),
              "A ring may suffer from one or more of the following issues, which may
             also necessitate the exclusion of the ring's measurements from analyses.",
              checkboxGroupInput("sel_disqual", "Select all that apply",
                                 choices = disqual_issues,
                                 inline = TRUE),
              shinyjs::hidden(
                selectizeInput(
                  inputId = "sel_technical_exact",
                  label = "Specify the reason(s) for the technical issue",
                  choices = technical_issues,
                  multiple = TRUE
                )
              ),
              uiOutput("warn_disq")
            )
          ),

          bslib::layout_column_wrap(
            width = 1,
            fill = FALSE,
            heights_equal = "row",
            bslib::card(
              class = "clean-card",
              fill = FALSE,
              #min_height = "250px",
              strong("Discrete features"),
              "A ring may exhibit one or more of the following features, which may carry an additional discrete signal
               beyond that captured by the QWA measurements.",
              checkboxGroupInput("sel_discrete", "Select all that apply",
                                 choices = discrete_features,
                                 inline = TRUE),
            ),
            bslib::card(
              fill = FALSE,
              class = "clean-card",
              textAreaInput(
                inputId = "sel_notes",
                label = "Additional comments",
                placeholder = "Enter any additional notes regarding the selected ring here..."
              )
            )
          )
        ),





        # bslib::layout_column_wrap(
        #   width = 1/3,
        #   min_height = "300px",
        #   bslib::card(
        #     bslib::card_header("Discrete features"),
        #     checkboxGroupInput("sel_discrete", "Select any that apply",
        #                        choices = c("Blue ring", "Frost ring",
        #                                    "Intra-annual density fluctuations",
        #                                    "Light ring",
        #                                    "Traumatic resin ducts", "Other"),
        #                        inline = TRUE)
        #   ),
        #   bslib::card(
        #     bslib::card_header("Disqualifying features"),
        #     radioButtons("sel_disqf", "Exclude ring from analysis?", choices = c("yes", "no"), inline = TRUE, selected = "no"),
        #     checkboxGroupInput("sel_disqual", "Select any that apply",
        #                        choices = c("Duplicate ring", "Radially incomplete ring",
        #                                    "Wedging/missing ring",
        #                                    "Crossdating", "Compression wood",
        #                                    "Orientation", "Tyloses", "Decay",
        #                                    "Technical issues", "Other"),
        #                        inline = TRUE),
        #     selectizeInput(
        #       inputId = "sel_disqf_reason",
        #       label = "Select reason(s) for exclusion",
        #       choices = c("Out of focus", "Crack(s)", "Paraffin", "Compressed cells",
        #                   "Overlapping cells", "Broken cells", "Tangentially incomplete ring"),
        #       multiple = TRUE
        #     )
        #   ),
        #   bslib::card(
        #     bslib::card_header("Comments"),
        #     textAreaInput(
        #       inputId = "sel_notes",
        #       label = "Additional comments for the selected ring:",
        #       placeholder = "Enter any additional notes here..."
        #     )
        #   )
        # ),

        # bslib::card(
        #   bslib::card_body(
        #     class = "card-input",
        #     bslib::layout_columns(
        #       radioButtons("sel_excl", "Should the ring be excluded from analysis", c("include", "exclude"), inline = TRUE),
        #       radioButtons("sel_aff", "Affected tissue", c("all", "ew", "lw"), inline = TRUE)
        #     ),
        #     checkboxGroupInput(
        #       inputId = "sel_flags",
        #       label = "Select any issues that apply for the selected ring:",
        #       choices = c("Incomplete ring", "Wedging/missing ring",
        #                   "X-dating", "Compression wood", "Orientation",
        #                   "Artefacts", "Crack(s)", "Out of focus", "Frost ring", "Decay",
        #                   "Filled cells", "Compressed cells", "Overlapping cells", "Broken cells",
        #                   "Tyloses", "Paraffin", "Other issues"),
        #       inline = TRUE
        #     )
        #   )
        # ),

        bslib::layout_column_wrap(
          width = 1/6,
          actionButton("save_sel_flags", "Apply selected issues", icon = icon("save")),
          actionButton("restore_sel_flags", "Restore selected ring", icon = icon("undo"))
        )
      ),

      verbatimTextOutput("debug"),


      # fluidRow(
      #   column(6, uiOutput("radioButtons_ui")),
      #   column(6, uiOutput("clicked_id"))
      # ),
      #uiOutput("textAreaInput_ui"),
      #actionButton("save_yte", "Save Selected Info"),

      div(width = "150pt", downloadButton("download_yte_table", "Download new ring data")),
      # DT::DTOutput("yte_table"),
      # uiOutput("dynamic_image")
    ),

    bslib::nav_panel(
      "running RBT",
      icon = icon("cloud-sun", lib = "font-awesome"),

      plotOutput("rbt_plot")
    ),

    bslib::nav_panel(
      "Heatmap of climate signal", #icon = icon("heat", lib = "font-awesome"),
      plotOutput("clim_cor_plot")
    ),

    bslib::nav_panel(
      "Clim_cor data", #icon = icon("table", lib = "font-awesome"),
      DT::DTOutput("numeric_table")
    )

)
