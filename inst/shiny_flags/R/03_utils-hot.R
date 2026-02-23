hot_settings_modal <- function(ns, issue_cols, feat_cols){
  shiny::modalDialog(
    title = "Table settings",
    shiny::checkboxInput(ns("auto_open_image"), "Try to auto-open images", value = FALSE),
    shiny::strong("Show / hide optional columns:"),
    shiny::checkboxGroupInput(
      ns("sel_cols_issues"),
      label = "Compromising issues",
      choices = c(
        disqual_issues,
        technical_issues,
        other_issues
      ), selected = issue_cols
    ),
    shiny::checkboxGroupInput(
      ns("sel_cols_features"),
      label = "Discrete features",
      choices = discrete_features,
      selected = feat_cols
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_cols"), "Proceed")
    )
  )
}

renderer_cb <- function(bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
            if (row == selRow) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
            return td;
        }", bgcolor_js, bgcolor_js)))
}

renderer_cb_ro <- function(readonly_ids = NULL, bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  readonly_js <- if(is.null(readonly_ids)) "[]" else jsonlite::toJSON(readonly_ids)

  htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          var readonlyRows = %s;

          var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
          if (row == selRow) {
            td.style.background = '#ed4c4c';
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // Set readOnly for specific rows
          if (readonlyRows.includes(row)) {
            cellProperties.readOnly = true;
            td.style.opacity = '0.6';  // Optional: visual indication
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", readonly_js, bgcolor_js, bgcolor_js)))
}

renderer_cb_dupl <- function(readonly_ids = NULL, bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  readonly_js <- if(is.null(readonly_ids)) "[]" else jsonlite::toJSON(readonly_ids)

  htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          var readonlyRows = %s;
          var rowData = instance.getDataAtRow(row);

          var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
          if (row == selRow) {
            td.style.background = '#ed4c4c';
          } else if (rowData[0] && !value) {
              td.style.background = '#99C2C2';  // Light green
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // Set readOnly for specific rows
          if (readonlyRows.includes(row)) {
            cellProperties.readOnly = true;
            td.style.opacity = '0.6';  // Optional: visual indication
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", readonly_js, bgcolor_js, bgcolor_js)))
}

renderer_cb_val <- function(warn_col_ids = NULL, bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  warn_js <- if(is.null(warn_col_ids)) "[]" else jsonlite::toJSON(warn_col_ids)

  htmlwidgets::JS(htmltools::HTML(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          // remove old tippy if necessary
          if(td.hasOwnProperty('_tippy')) {
            td._tippy.destroy();
          }
          var warnCols = %s;

          var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
          if (row == selRow) {
            td.style.background = '#ed4c4c';
          } else if ('%s' !== '') {
            td.style.background = '%s';
          }

          // invalid warning if other cells in same row, but warn_col_ids include value true
          if (warnCols.length > 0) {
            var rowData = instance.getDataAtRow(row);
            var has_issues = warnCols.map((item) => rowData[item]).includes(true);
            if (!value && has_issues) {
              td.style.background = 'pink';
              tippy(td, { content: 'exclude ring with disqualifying issues?' });
            }
          }

          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          return td;
        }", warn_js, bgcolor_js, bgcolor_js)))
}

# // green background for dupl_ids rows where value is false (selected duplicate)
# if (duplRows.includes(row) && value === false) {
#   td.style.background = '#90EE90';  // Light green
# }

renderer_dd <- function(bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
            if (row == selRow) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.DropdownRenderer.apply(this, arguments);
            return td;
        }", bgcolor_js, bgcolor_js)))
}
renderer_txt <- function(bgcolor = NULL){
  bgcolor_js <- if(is.null(bgcolor)) "" else bgcolor
  htmlwidgets::JS(htmltools::HTML(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
            if (row == selRow) {
              td.style.background = '#ed4c4c';
            } else if ('%s' !== '') {
              td.style.background = '%s';
            }
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            return td;
        }", bgcolor_js, bgcolor_js)))
}
