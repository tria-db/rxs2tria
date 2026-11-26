ui <- bslib::page_navbar(
  title = "rxs2tria: QWA data quality explorer",
  id = "tabs",
  selected = "flags",

  # theme (see 00_utils-global.R)
  theme = theme,

  # normal document flow: the window scrolls and the footer sits after the
  # content
  fillable = FALSE,

  # teal navbar with light text and an underline indicator
  navbar_options = bslib::navbar_options(
    bg = prim_col,
    theme = "dark",
    underline = TRUE
  ),

  # page-level assets (in `header` so they aren't treated as nav items)
  header = shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$head(
      # custom JS
      # for the tippy tooltip
      shiny::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      shiny::tags$script(src = "https://unpkg.com/tippy.js@6"),
      # for the key bindings (left/right arrows, enter key)
      shiny::tags$script(src = "js/keybindings.js"),
      # for the resizable plot container
      shiny::tags$script(src = "js/plot_resize.js"),
      # for the unsaved changes indicator
      shiny::tags$script(src = "js/unsaved.js")
    )
  ),

  bslib::nav_spacer(),

  # TAB: Start (plot of the time series) ---------------------------------------
  bslib::nav_panel(
    title = "Quality check series",
    value = "flags",
    icon = shiny::icon("font-awesome", lib = "font-awesome"),
    flags_ui('flags')
  ), # end of tab

  # TAB: Comment progress overview ---------------------------------------------
  bslib::nav_panel(
    title = shiny::uiOutput("comments-tab_title"),
    value = "comments_panel",
    icon = shiny::icon("circle-check"),
    comments_ui('comments')
  ), # end of tab

  # # TAB: running RBT -----------------------------------------------------------
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
  # # TAB: HEATMAP ---------------------------------------------------------------
  # bslib::nav_panel(
  #   "Heatmap of climate signal", icon = icon("heat", lib = "font-awesome"),
  #   plotOutput("clim_cor_plot")
  # ),
  #
  # # TAB: climate correlation ---------------------------------------------------
  # bslib::nav_panel(
  #   "Clim_cor data", icon = icon("table", lib = "font-awesome"),
  #   DT::DTOutput("numeric_table")
  # )

  # FOOTER ---------------------------------------------------------------------
  footer = bslib::card_body(
    class = "wsl-footer",
    fill = FALSE,
    shiny::tags$img(
      src = "WSL_Logo_neg.png",
      height = "30px"
    ),
    shiny::actionLink("contact_link", "Contact")
  )
) # end of ui
