#library(shiny) # TODO: namespace all shiny/htmltools functions

ui <- bslib::page_fluid(
  shinyjs::useShinyjs(),

  # theme (see 00_utils-theme.R)
  theme = theme,

  shiny::tags$head(
    # additional CSS where we need highest priority to overwrite properties
    shiny::tags$link(rel = "stylesheet", href = "css/overwrites.css"),

    # custom JS
    # for the tippy tooltip
    shiny::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    shiny::tags$script(src = "https://unpkg.com/tippy.js@6"),
    # for the key bindings (left/right arrows, enter key)
    shiny::tags$script(src = "js/keybindings.js")
  ),

  # MAIN PANEL -----------------------------------------------------------------
  bslib::navset_card_underline(
    id = 'tabs',
    selected = "Quality check series",

    # TITLE --------------------------------------------------------------------
    title = "rxs2tria: QWA data quality and climate signal explorer",

    # TAB: Start (plot of the time series) -------------------------------------
    bslib::nav_panel(
      "Quality check series",
      icon = shiny::icon("font-awesome", lib = "font-awesome"),
      flags_ui('flags')

    ), # end of tab

    # TAB: Comment progress overview -------------------------------------------

    bslib::nav_panel(
        title = shiny::uiOutput("comments-tab_title"),
        value = "comments_panel",
        icon = shiny::icon("circle-check"),
        comments_ui('comments')
    ), # end of tab

    # # TAB: running RBT ---------------------------------------------------------
    # bslib::nav_panel(
    #
    #   conditionalPanel(
    #    condition = "input.tabs == 'running RBT'",
    #    uiOutput("dynamic_rbt_slider")
    #   ),
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
    #   "Heatmap of climate signal", icon = icon("heat", lib = "font-awesome"),
    #   plotOutput("clim_cor_plot")
    # ),
    #
    # # TAB: climate correlation -------------------------------------------------
    # bslib::nav_panel(
    #   "Clim_cor data", icon = icon("table", lib = "font-awesome"),
    #   DT::DTOutput("numeric_table")
    # )

  ), # end of navset_card_underline

  # FOOTER ---------------------------------------------------------------------
  shiny::tags$footer(
    class = "wsl-footer",
    shiny::tags$img(
      src = "WSL_Logo_neg.png",
      height = "30px",
      style = "vertical-align:middle; margin-right: 10px;"
    ),
    shiny::actionLink("contact_link", "Contact",
               style = "color: white; margin-left: 20px; text-decoration: none;")
  )

) # end of ui
