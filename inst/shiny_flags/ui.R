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

ui <- bslib::page_fluid(
  # preliminaries
  #
  shinyjs::useShinyjs(),

  # theme
  theme = theme,

  # additional CSS
  # for where we need highest priority to overwrite existing properties
  tags$head(
    tags$style(HTML("
      /* bslib style overwrites: */
      /* spacing around sidebar title*/
      .bslib-sidebar-layout .sidebar-title {
        margin-bottom: 0px;
        padding-bottom: 10px;
      }

      .bslib-sidebar-layout>.sidebar>.sidebar-content {
        padding-top: 1rem;
      }

      /* hot style overwrites: */
      /* bg color for the invalid cells */
      .handsontable td.htInvalid {
        background-color: pink !important;
      }

    ")),

    # custom JS
    # for the tippy tooltip
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    # --- Add JS for keyboard arrows ---
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        // Left arrow -> previous ring
        if(e.key === 'ArrowLeft') {
          Shiny.setInputValue('prev_ring_js', Math.random());
        }
        // Right arrow -> next ring
        if(e.key === 'ArrowRight') {
          Shiny.setInputValue('next_ring_js', Math.random());
        }
      });
    ")),
  ),


  # MAIN PANEL -----------------------------------------------------------------
  bslib::navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = "Time series with chrono",

    # TITLE --------------------------------------------------------------------
    title = "rxs2tria: QWA data quality and climate signal explorer",

    # TAB: Start (plot of the time series) -------------------------------------
    bslib::nav_panel(
      "Time series with chrono",
      icon = icon("microscope", lib = "font-awesome"),

      bslib::layout_sidebar(

        # SIDEBAR  ---------------------------------------------------------------
        sidebar = bslib::sidebar(

          # load input data
          actionButton("btn_input", "Load input data", icon = icon("upload")),
          hr(),

          # TODO: loading climate data? in other tabs?
          #fileInput("file_clim", "Upload the 'climate' data", accept = c(".csv")),

          # filters
          strong("Filter data for plot"),
          selectInput("sel_site", "Filter the sites to display", choices = NULL, multiple = TRUE, selectize = TRUE),
          selectInput("sel_species", "Select the species to display", choices = NULL, multiple = FALSE, selectize = TRUE),
          selectInput("sel_wp", "Select the woodpieces to display", choices = NULL, multiple = TRUE, selectize = TRUE),
          hr(),

          # plotly
          strong("Plot settings"),
          selectInput("sel_param", "Choose QWA parameter to display", choices = NULL, multiple = FALSE, selectize = TRUE),
          selectInput("sel_sector", "Select which ring sector to plot", choices = NULL, multiple = FALSE, selectize = TRUE),
          shinyjs::disabled(checkboxInput("spline_det", "32-years spline detrend", value = FALSE)),

          selectInput("mean_type",
                      label_with_pop("Select mean applied for crn",
                                     "Calculated over all selected woodpieces, filtering out excluded rings."),
                      choices = c("none", "mean", "tbrm"), selected = "none", multiple = FALSE, selectize = TRUE),

          checkboxInput("show_excl",
                        label_with_pop(
                          "show excluded rings",
                          "Click update plot to include/exclude newly edited rings (pink markers)"),
                        value = FALSE),
          hr(),

          # saving
          actionButton("apply_changes", "Update plot", icon = icon("arrows-rotate"),
                       class = "btn-tert"),
          hr(),
          downloadButton("save_flags", "Download new ring data",
                         class = "btn-tert")

          #checkboxInput("tail_YTE", "show tail YTE", value = TRUE),

        ), # end of sidebar

        # MAIN CONTENT -----------------------------------------------------------
        bslib::card(
          min_height = "450px",
          max_height = "450px",
          bslib::card_body(class = "p-0",
          plotly::plotlyOutput("ts_crn_plot"))
        ),

        # SELECTED RING CARD
        shinyjs::hidden(
          div(id = "ring_editor_card",

            bslib::navset_card_pill(
              id = "sel_ring_navset",
              selected = "edit_flags",
              title = uiOutput("sel_ring"), # selected ring image_lable and year

              # EDITING THE RING FLAGS
              bslib::nav_panel(
                "Edit ring",
                value = "edit_flags",

                bslib::layout_column_wrap(
                  fill = FALSE,
                  width = 1/2,
                  heights_equal = "row",

                  bslib::layout_column_wrap(
                    width = 1,
                    fill = FALSE,
                    heights_equal = "row",
                    bslib::card(
                      fill = FALSE,
                      class = "clean-card-sec",
                      bslib::layout_column_wrap(
                        width = 1/3,
                        radioButtons("sel_exclude", "Exclude ring from analysis?",
                                     choices = c("yes", "no"), inline = TRUE,
                                     selected = "no"),
                        # SECOND COLUMN
                        div(uiOutput("exclude_scope_ui"),
                            uiOutput("warn_disq")),
                        # THIRD COLUMN
                        div(
                          style = "display: flex; gap: 10px; align-items: center;",
                          actionButton("prev_ring", "Previous"),
                          actionButton("next_ring", "Next"),
                          checkboxInput("auto_open_image", "Auto-open image", value = FALSE)
                        )
                      )
                    ),
                    bslib::card(
                      class = "clean-card",
                      checkboxGroupInput("sel_disqual",
                                         "Disqualifying features: Select all that apply",
                                         choices = disqual_issues,
                                         inline = TRUE),
                      div(
                        id = "techn_reason_el",
                        style = "display: flex; align-items: flex-start;",
                        div(style = "width: 25px; padding-top: 0px;",
                            icon("arrow-right-from-bracket", lib = "font-awesome")),
                        div(style = "flex-grow: 1;",
                          checkboxGroupInput(
                            "sel_technical_exact",
                            "Technical issues: Specify (optional)",
                            choices = technical_issues,
                            inline = TRUE
                          )
                        )
                      )
                    )
                  ),

                  bslib::layout_column_wrap(
                    width = 1,
                    fill = FALSE,
                    heights_equal = "row",
                    bslib::card(
                      class = "clean-card",
                      fill = FALSE,
                      checkboxGroupInput("sel_discrete",
                                         "Discrete features: Select all that apply",
                                         choices = discrete_features,
                                         inline = TRUE),
                    ),
                    bslib::card(
                      fill = FALSE,
                      class = "clean-card",
                      textAreaInput(
                        inputId = "sel_comment",
                        label = "Additional comments",
                        width = "100%",
                        placeholder = "Enter any additional notes regarding the selected ring here..."
                      )
                    ),
                    bslib::card(
                      fill = FALSE,
                      class = "clean-card",
                      verbatimTextOutput("correlation")
                    )
                  )
                ),

                bslib::layout_column_wrap(
                  width = "217px", fixed_width = TRUE,
                  actionButton("undo_sel_flags", "Undo current edits", icon = icon("arrow-left")),
                  actionButton("reset_to_raw", "Reset to input", icon = icon("undo")),
                  actionButton("show_image", "Open image", icon = icon("image"))
                )

              ),

              # COVERAGE PANEL
              bslib::nav_panel(
                "Show coverage",
                value = "show_coverage",
                verbatimTextOutput("coverage"),
                uiOutput("coverage_ui")

              )
            )
          )
        ), # end of ring editor card

        verbatimTextOutput("debug")
      )
    ), # end of tab 1


    # TAB: running RBT ---------------------------------------------------------
    bslib::nav_panel(

      # conditionalPanel(
      #   condition = "input.tabs == 'running RBT'",
      #   uiOutput("dynamic_rbt_slider")
      # ),


      "running RBT",
      icon = icon("cloud-sun", lib = "font-awesome"),

      plotOutput("rbt_plot")
    ),

    # TAB: HEATMAP -------------------------------------------------------------
    bslib::nav_panel(
      "Heatmap of climate signal", #icon = icon("heat", lib = "font-awesome"),
      plotOutput("clim_cor_plot")
    ),

    # TAB: climate correlation -------------------------------------------------
    bslib::nav_panel(
      "Clim_cor data", #icon = icon("table", lib = "font-awesome"),
      DT::DTOutput("numeric_table")
    )

  ) # end of navset_card_underline

) # end of ui
