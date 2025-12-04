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
      fileInput("file_prf", "Upload the QWA profile data", accept = c(".csv")),
      fileInput("file_rings", "Upload the QWA rings data", accept = c(".csv")),
      fileInput("file_clim", "Upload the 'climate' data", accept = c(".csv")),

      #selectInput("sel_wp", "Select the woodpieces (cores) to display", choices = NULL, multiple = TRUE),
      selectInput("sel_param", "Choose QWA parameter to display", choices = NULL, multiple = FALSE),
      selectInput("sel_sector", "Select which ring sector to plot", choices = NULL, multiple = FALSE),

      checkboxInput("spline_det", "32-years spline detrend", value = TRUE),
      checkboxInput("YTE", "apply YTE", value = TRUE),
      checkboxInput("tail_YTE", "show tail YTE", value = TRUE),

      #conditionalPanel(
       # condition = "input.tabs != 'Time Series with chrono'",
      selectInput("mean_type", "Select the mean applied for crn",
                    choices = c("mean", "tbrm"), selected = "tbrm", multiple = FALSE)

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
      "Time Series with chrono",
      icon = icon("microscope", lib = "font-awesome"),

      p("Plotly instructions:",
        tags$ul(
        tags$li("Hover over a point to see its year, measurement value and woodpiece label."),
        tags$li("Click and drag the cursor over the plot to zoom in."),
        tags$li("Click on a line to highlight it."),
        tags$li("Double click onto whitespace to reset the zoom and/or highlighting.")
      )),
      plotly::plotlyOutput("ts_crn_plot"),
      # New shift controls
      # conditionalPanel(
      #   condition = "input.tabs == 'Time Series with chrono'",
        # fluidRow(
        #   column(4, numericInput("shift_amount", "Shift (rings):", value = 1, step = 1, min = 1)),
        #   column(4, actionButton("shift_left", "← Shift Left")),
        #   column(4, actionButton("shift_right", "Shift Right"))
        # ),
      # ),
      br(),
      verbatimTextOutput("debug"),

      checkboxGroupInput(
        inputId = "dynamic_checkboxes",
        label = "Check the corresponding property to the clicked ring",
        choices = c("yte", "X-dating", "Compression wood", "Orientation",
                    "Artefacts", "Crack(s)", "Out of focus", "Frost ring?", "Decay",
                    "Filled cells", "Compressed cells", "Overlapping cells", "Broken cells",
                    "Tyloses", "Paraffin", "Others"),
        inline = TRUE
      ),
      fluidRow(
        column(6, uiOutput("radioButtons_ui")),
        column(6, uiOutput("clicked_id"))
      ),
      uiOutput("textAreaInput_ui"),
      actionButton("save_yte", "Save Selected Info"),
      actionButton("restore_selected_yte", "Restore Selected Rings"),
      actionButton("save_to_excel", "Save to Excel"),
      DT::DTOutput("yte_table"),
      uiOutput("dynamic_image")
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
