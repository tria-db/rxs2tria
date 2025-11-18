library(magrittr) # might not be needed if pckg is loaded due to use_pipe
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

# ui ---------------------------------------------------------------------------
ui <- bslib::page_fluid(

  shinyjs::useShinyjs(),
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
    tags$script(src = "https://unpkg.com/tippy.js@6")
  ),


  # MAIN PANEL -----------------------------------------------------------------
  bslib::navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = tab_start,

    # TITLE --------------------------------------------------------------------
    title = "TRIA database: Contribute metadata",

    # TAB: Start (prefilled metadata) ------------------------------------------
    bslib::nav_panel(
      title = tab_start,
      start_ui('start')
    ),

    # TAB: general (dataset and authors) ---------------------------------------
    bslib::nav_panel(
      title = tab_general,
      dataset_ui('ds')
    ),

    # TAB: sites ---------------------------------------------------------------
    bslib::nav_panel(
      title = tab_site,
      site_ui('site')
    ),

    # TAB: summary -------------------------------------------------------------
    bslib::nav_panel(
      title = tab_summary,
      summary_ui('summary')
    )

  ), # end of tabs

  verbatimTextOutput("debug"), # for debugging

  # FOOTER ---------------------------------------------------------------------
  tags$footer(
    class = "wsl-footer",
    tags$img(src = "WSL_Logo_neg.png",
             height = "30px", style = "vertical-align:middle; margin-right: 10px;"),
    actionLink("contact_link", "Contact",
               style = "color: white; margin-left: 20px; text-decoration: none;"),
  ),

) # end of ui




# old css:
# bs-btn-disabled-bg: #551345;
# .handsontable .autocompleteEditor .htMacScroll {
#   overflow: visible !important;
# }
# .accordion-tert .accordion-header {
#   --bs-accordion-active-bg: ", tert_col_grad[2], " !important;
# }
# ".jstree-default .jstree-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }
# .jstree-default .jstree-wholerow-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }"

# .handsontableEditor .ht_master {
#   height: 300px;
#   overflow-y: visible !important;
# }
# .handsontableEditor .ht_clone_top {
#   transform: none !important;
# }
# .htColumnHeaders {
#   overflow: visible !important;
# }
