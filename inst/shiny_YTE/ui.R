# library(shiny)
# library(bslib)
# library(sass)
# library(shinyjs)
# library(DT)
# library(ggplot2)
# library(plotly)
# library(checkmate)
# library(shinyalert)

# TODO:
# add button to open image
# save flags to file
# save cov plots to file
# validity check: at least one sel flag per overlap
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")
theme <- bslib::bs_theme(version = 5, primary = prim_col, secondary = sec_col,
                         info = tert_col, font_scale = 0.8, preset = "zephyr",
                         "body-bg" = prim_col_grad[5], "card-border-width" = 0,
                         "focus-ring-color" = sec_col_grad[4])
theme <-  bslib::bs_add_rules(theme, HTML(paste0("
    .btn-secondary {
      color: white;
    }
    .btn-info {
      background-color: ", tert_col, ";
    }
    .sidebar {
      background-color: ", prim_col_grad[4], " !important;
      color: #212529 !important;
    }

    /* styling the navbar: primary bg color, white font, hover, active*/
    .bslib-navs-card-title {
      background-color: ", prim_col, ";
      color: white;
      font-size: 12pt;
    }
    .nav-link {
      font-size: 10pt;
      color: ", prim_col_grad[3], ";
      --bs-nav-link-hover-color: white;
      --bs-nav-underline-link-active-color: white;
    }



    /* class for a code style output*/
    .code-output {
      display:block;
      padding:9.5px;
      margin:0 0 10px;
      margin-top:10px;
      line-height:20px;
      word-break:break-all;
      word-wrap:break-word;
      white-space:pre-wrap;
      border:1px solid rgba(0,0,0,0.15);
      border-radius:4px;
      font-family:monospace;
      background-color:", prim_col_grad[5], " !important;
    }

    /* class for a note stlye card */
    .card-note .card-header{
      padding: 8px;
    }
    .card-note .card-body{
      padding: 8px;
    }

    /* styling the shinyTree resp. jstree */
    .jstree-proton .jstree-clicked {
      background: ", prim_col_grad[1], " !important;
    }

    /* styling the accoridons */
    .accordion-button:not(.collapsed) {
      background-color: ", prim_col_grad[1], " !important;
      color: white !important;
    }
    .accordion-button {
      background-color: ", prim_col_grad[4], ";
    }
    .accordion-tert .accordion-button:not(.collapsed) {
      background-color: ", tert_col_grad[2], " !important;
      color: white !important;
    }
    .accordion-tert .accordion-button {
      background-color: ", tert_col_grad[4], ";
    }
    .accordion-tert .accordion-item {
      background-color: ", tert_col_grad[5], " !important;
    }

    .card-tert {
      background-color: ", tert_col_grad[6], " !important;
    }

    /* datatable styling */
    .dataTables_wrapper .dataTable td {
      padding: 2px 2px !important;
    }
    .dataTables_paginate .paginate_button {
      padding: 2px 2px !important;  /* Adjust padding to make buttons smaller */
    }

    /* Adjust the list spacing*/
    ol.custom-indent {
      padding-left: 15px;
      padding-bottom: 0px;
      margin-top: 0px;
      margin-bottom: 0px;
    }
    .custom-indent .li {
      margin-bottom: 0px;
    }

    /* change header color of ht */
    .handsontable th {
      background-color: ", sec_col_grad[5], " !important;
    }

    /* borders of selected input fields */
    .form-control:focus {
      border-color: ", sec_col, " !important;
    }

    .form-group {
      margin-bottom: 0px !important;
    }

     table.dataTable thead tr { background: #CCE0E0 !important; }


    .btn-file {
      background-color: ", tert_col, " !important;
      border-color: ", tert_col, " !important;
      color: white !important;
    }

  ")))

ui <- page_sidebar(
  # preliminaries
  shinyjs::useShinyjs(),

  # theme
  theme = theme,
  tags$head(
    tags$style(HTML("
      .handsontable td.htInvalid {
        background-color: pink !important;
      }
      .form-control, .selectize-input, .form-control-shiny {
        background-color: white !important;
      }
      .shiny-input-container .radio input:not(:checked)  {
        border-color: ", sec_col, " !important;
        background-color: white !important;
      }
      .shiny-input-container .radio input:checked  {
        border-color: ", sec_col, " !important;
        background-color: ", sec_col, " !important;
      }
      .shiny-input-container .checkbox input:not(:checked)  {
        border-color: ", sec_col, " !important;
        background-color: white !important;
      }
      .shiny-input-container .checkbox input:checked  {
        border-color: ", sec_col, " !important;
        background-color: red !important;
      }

      /* style for input DT similar to hot*/
        .inputDT thead th {
          background-color: ", sec_col_grad[5], " !important;
        }
      .inputDT tbody td {
        background-color: white;
      }

      th.checkbox-col, td.checkbox-col {
        width: 75px !important;
        overflow: hidden;
        max-width: 75px !important;
        text-align: center;
      }



    "))),



  # .dataTables_scrollBody{
  #   overflow-x:hidden !important;
  #   text-overflow: ellipsis;
  #   overflow-y:auto !important;
  # }
  # to add css directly:
  # tags$head(
  #   tags$style(HTML( ... )))
  # list icon styles
  #   ".done {list-style-type:square}
  #      .notdone {list-style-type:circle}"
  # dt styling
  #   ".dt-row-group {
  #     background-color: green !important;}"
  # to avoid dropdown overlapping stuff
  # ".selectize-control .selectize-dropdown {
  #   position: static !important;}"


  # TITLE ----------------------------------------------------------------------
  title = "rxs2xcell: Inspect Yearly Coverage",

  # SIDEBAR  -------------------------------------------------------------------
  sidebar = sidebar(

    # INPUT DATA ---------------------------------------------------------------
    card(
      card_header("Input data"),
      span("If QWA data is available in the current R environment, it will be used directly.
        Alternatively, you can browse for a saved QWA data file."),
      fileInput("file_upload", "Load QWA data from file (csv)", accept = ".csv"),
      strong('Source of shown data:'),
      textOutput("data_source")
    ),

    # ANALYSIS CONTROLS --------------------------------------------------------
    card(
      card_header("Analysis controls"),

      # INPUT WOODPIECE --------------------------------------------------------
      selectInput(
        "woodpiece",
        label = 'Select woodpiece (core) to inspect:',
        choices = "No data yet"
      ),

      HTML("<hr>"),

      # RESET BUTTON -----------------------------------------------------------
      span("Reset the flags for the current woodpiece back to input data:"),
      actionButton("btn_reset", "Reset current flags",
                   class = "btn btn-secondary"),

      HTML("<hr>"),

      # USER FLAG VALIDATION ---------------------------------------------------
      strong("Validity check for the manual changes:"),
      span("The following conditions need to be met for user input to be valid:"),
      uiOutput("flags_check")

    ),

    # SAVE BUTTON --------------------------------------------------------------
    card(
      card_header("Save data"),
      p("Once all the flags are set for all woodpieces, export the data and
        close the app."),
      downloadButton("btn_save", "Export and close app",
                     class = "btn btn-primary")
    ),

  ), # end of sidebar


  # MAIN PANEL -----------------------------------------------------------------
  accordion(
    open = c("Coverage plot", 'Data'),

    # COVERAGE PLOT ------------------------------------------------------------
    accordion_panel(
      "Coverage plot",
      plotly::plotlyOutput("covPlot")
    ),

    # accordion_panel(
    #   "Coverage plot",
    # first try with girafe plot for clickable images, but does not resize properly
    #   girafeOutput("covPlot")
    # ),

    # DATA TABLE ---------------------------------------------------------------
    accordion_panel(
      "Data",
      DT::DTOutput('tbl')

    ),

    verbatimTextOutput('debug')


    # accordion_panel(
    #   "Data",
    # first try with reactable for collapsible groups, but with DT can click cells to edit
    #   reactable.extras::reactable_extras_dependency(),
    #   reactable::reactableOutput("reactTable")
    # )

  ) # end of accordion

) # end of page_sidebar
