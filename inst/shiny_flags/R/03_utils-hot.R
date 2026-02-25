hot_settings_modal <- function(ns, sel_cols){
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
      ), selected = c(sel_cols$disqual, sel_cols$techn, sel_cols$other)
    ),
    shiny::checkboxGroupInput(
      ns("sel_cols_features"),
      label = "Discrete features",
      choices = discrete_features,
      selected = sel_cols$discrete
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_cols"), "Proceed")
    )
  )
}

renderer_cb <- function(bgcolor = default_bg_color, selcolor = sel_row_color) {
  bgcolor_js <- bgcolor %||% ""
  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      var rowData = instance.getDataAtRow(row);

      // set background color for selected row vs rest of rows
      if (row == selRow) {
        td.style.background = '__selcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }

      Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}

renderer_cb_ro <- function(readonly_ids = NULL, bgcolor = default_bg_color,
                           selcolor = sel_row_color){
  bgcolor_js <- bgcolor %||% ""
  readonly_js <- jsonlite::toJSON(readonly_ids %||% integer(0))

  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      var readonlyRows = __readonly_js__;

      // set background color for selected row vs rest of rows
      if (row == selRow) {
        td.style.background = '__selcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }

      // set readOnly for given row indices
      if (readonlyRows.includes(row)) {
        cellProperties.readOnly = true;
        td.style.opacity = '0.6';
      }

      Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}

renderer_cb_dupl <- function(readonly_ids = NULL, bgcolor = NULL,
                             selcolor = sel_row_color, duplcolor = chosen_dupl_color) {
  bgcolor_js <- bgcolor %||% ""
  readonly_js <- jsonlite::toJSON(readonly_ids %||% integer(0))

  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      var readonlyRows = __readonly_js__;
      var rowData = instance.getDataAtRow(row);

      // set background color for selected row
      if (row == selRow) {
        td.style.background = '__selcolor__';
      // green background for rows where first col is true but this column is false
      // i.e. chosen duplicate rows if not currently selected
      } else if (rowData[0] && !value) {
          td.style.background = '__duplcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }

      // set readOnly for given row indices
      if (readonlyRows.includes(row)) {
        cellProperties.readOnly = true;
        td.style.opacity = '0.6';
      }

      Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}

renderer_cb_val <- function(warn_col_ids = NULL, bgcolor = default_bg_color,
                            selcolor = sel_row_color, warncolor = warn_inv_color,
                            warn_msg = "ring with disqualifying issues, exclude?") {
  bgcolor_js <- bgcolor %||% ""
  warn_js <- jsonlite::toJSON(warn_col_ids %||% integer(0))

  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      var warnCols = __warn_js__;

      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      // set background color for selected row vs rest of rows
      if (row == selRow) {
        td.style.background = '__selcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }

      // invalid warning (color plus tippy)
      // if other cells in (same row, warn_col_ids) have at least one true value
      if (warnCols.length > 0) {
        var rowData = instance.getDataAtRow(row);
        var has_issues = warnCols.map((item) => rowData[item]).includes(true);
        if (!value && has_issues) {
          td.style.background = '__warncolor__';
          tippy(td, { content: '__warn_msg__' });
        }
      }

      Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}


renderer_dd <- function(bgcolor = default_bg_color, selcolor = sel_row_color) {
  bgcolor_js <- bgcolor %||% ""
  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      if (row == selRow) {
        td.style.background = '__selcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }
      Handsontable.renderers.DropdownRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}

renderer_txt <- function(bgcolor = default_bg_color, selcolor = sel_row_color) {
  bgcolor_js <- bgcolor %||% ""
  htmlwidgets::JS(htmltools::HTML(glue::glue("
    function(instance, td, row, col, prop, value, cellProperties) {
      var selRow = (instance._highlightedRow !== undefined) ? instance._highlightedRow : -1;
      if (row == selRow) {
        td.style.background = '__selcolor__';
      } else if ('__bgcolor_js__' !== '') {
        td.style.background = '__bgcolor_js__';
      }
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      return td;
    }
  ", .open = "__", .close = "__")))
}


get_dupl_updates <- function(df_flags, df_rings, sel_img, sel_wp) {
  old_dupl_sel <- df_rings |>
    dplyr::filter(image_label == sel_img) |>
    dplyr::filter(duplicate_ring) |>
    dplyr::select(year, exclude_dupl) |>
    dplyr::arrange(year)

  new_dupl_sel <- df_flags |>
    dplyr::filter(duplicate_ring) |>
    dplyr::select(year, exclude_dupl) |>
    dplyr::arrange(year)

  df_flags_updated <- df_flags

  # check if there were any changes in the exclude_dupl column
  if (any(old_dupl_sel$exclude_dupl != new_dupl_sel$exclude_dupl)) {
    cat(" new duplicates selection!\n")
    # new dupl means that of the other overlapping images,
    # one image has to swap in the opposite direction -> find and add this row
    for (k in 1:nrow(new_dupl_sel)) {
      # if new_dupl_sel is now FALSE/selected -> set the one previously FALSE image to TRUE
      if (!new_dupl_sel$exclude_dupl[k]) {
        df_new <- df_rings |>
          dplyr::filter(woodpiece_label == sel_wp) |>
          dplyr::filter(image_label != sel_img) |>
          dplyr::filter(year == new_dupl_sel$year[k]) |>
          dplyr::filter(!exclude_dupl) |>
          dplyr::select(dplyr::all_of(names(df_flags))) |>
          dplyr::mutate(exclude_dupl = TRUE)
        # add the new rows (from other images)
        df_flags_updated <- df_flags_updated |> dplyr::bind_rows(df_new)
      } else {
        # if new_dupl_sel is now TRUE -> of the other images, find the one with max cno and set to FALSE
        df_new <- df_rings |>
          dplyr::filter(woodpiece_label == sel_wp) |>
          dplyr::filter(image_label != sel_img) |>
          dplyr::filter(year == new_dupl_sel$year[k]) |>
          dplyr::mutate(duplicate_rank = cno - 100*as.numeric(exclude_issues)) |>
          dplyr::arrange(desc(duplicate_rank)) |>
          dplyr::slice(1) |>
          dplyr::mutate(exclude_dupl = FALSE) |> #  exclude_dupl = duplicate_rank < max(duplicate_rank)) |>
          dplyr::select(dplyr::all_of(names(df_flags)))
        # add the new rows (from other images)
        df_flags_updated <- df_flags_updated |> dplyr::bind_rows(df_new)
      }
    }
  }

  df_flags_updated
}
