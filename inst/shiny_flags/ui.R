library(shiny)
#library(magrittr)
#library(bslib)

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
      input.htCheckboxRendererInput[type='checkbox'] {
        accent-color: red !important;
      }
      /* Grey color for checkboxes in the first column (after row names) only */
      .htCore tbody tr td:nth-child(2) input.htCheckboxRendererInput[type='checkbox'] {
        accent-color: grey !important;
      }

    ")),

    # custom JS
    # for the tippy tooltip
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    # --- Add JS for keyboard arrows ---
    tags$script(HTML("
      $(document).ready(function() {
        var editingMode = true; // TODO: limit editing mode to when card is open/in focus/clicked?

        // Listen for arrow key presses
        $(document).on('keydown', function(e) {
          if (editingMode) {
            if (e.key === 'ArrowLeft') {
              e.preventDefault();
              Shiny.setInputValue('flags-prev_ring', Math.random()); // TODO: use action btn id directly
            } else if (e.key === 'ArrowRight') {
              e.preventDefault();
              Shiny.setInputValue('flags-next_ring', Math.random());
            }
          }
        });
      });
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Listen for enter key presses
        $(document).on('keydown', function(e) {
          if (e.key === 'Enter' || e.keyCode === 13) {
            var activeElement = document.activeElement;
            var isBody = activeElement === document.body;
            console.log('Active element tag:', activeElement.tagName);

            // More comprehensive check for interactive elements
            var isInteractive = !isBody && (
              activeElement.tagName.toLowerCase() === 'input' ||
              activeElement.tagName.toLowerCase() === 'textarea' ||
              activeElement.tagName.toLowerCase() === 'select' ||
              activeElement.tagName.toLowerCase() === 'button' ||
              activeElement.tagName.toLowerCase() === 'a' ||
              activeElement.closest('.handsontable') ||
              activeElement.closest('.shiny-input-container') ||
              activeElement.closest('.btn') ||
              activeElement.closest('[role=\"button\"]')
            );

            if (!isInteractive || isBody) {
              console.log('Caught enter key');
              Shiny.setInputValue('flags-enter_key', Math.random(), {priority: 'event'});
              e.preventDefault();
            }
          }
        });
      });
    "))

  ),



  # MAIN PANEL -----------------------------------------------------------------
  bslib::navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = "Quality check series",

    # TITLE --------------------------------------------------------------------
    title = "rxs2tria: QWA data quality and climate signal explorer",

    # TAB: Start (plot of the time series) -------------------------------------
    bslib::nav_panel(
      "Quality check series",
      icon = icon("font-awesome", lib = "font-awesome"),
      flags_ui('flags')

    ), # end of tab 1


    # # TAB: running RBT ---------------------------------------------------------
    # bslib::nav_panel(
    #
    #   # conditionalPanel(
    #   #   condition = "input.tabs == 'running RBT'",
    #   #   uiOutput("dynamic_rbt_slider")
    #   # ),
    #
    #
    #   "running RBT",
    #   icon = icon("cloud-sun", lib = "font-awesome"),
    #
    #   plotOutput("rbt_plot")
    # ),
    #
    # # TAB: HEATMAP -------------------------------------------------------------
    # bslib::nav_panel(
    #   "Heatmap of climate signal", #icon = icon("heat", lib = "font-awesome"),
    #   plotOutput("clim_cor_plot")
    # ),
    #
    # # TAB: climate correlation -------------------------------------------------
    # bslib::nav_panel(
    #   "Clim_cor data", #icon = icon("table", lib = "font-awesome"),
    #   DT::DTOutput("numeric_table")
    # )

  ), # end of navset_card_underline

  # FOOTER ---------------------------------------------------------------------
  tags$footer(
    class = "wsl-footer",
    tags$img(src = "WSL_Logo_neg.png",
             height = "30px", style = "vertical-align:middle; margin-right: 10px;"),
    actionLink("contact_link", "Contact",
               style = "color: white; margin-left: 20px; text-decoration: none;"),
  )

) # end of ui
