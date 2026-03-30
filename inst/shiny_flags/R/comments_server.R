comments_server <- function(id, main_session, flags_out) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # NOTE to self: tried to make comment_handled editable via reactable selction of rows
    # but does not seem to work well. need in / out logic like in hot or working with inputs 
    # column directly -> server output, but needs to work also with badge and filter show_all

    # COMMENTS OVERVIEW TAB ------------------------------------------------------
    # tab title with badge showing count of unhandled comments
    output$tab_title <- shiny::renderUI({
      shiny::req(flags_out$images_edited())
      df_comments <- flags_out$images_edited()
      n_unhandled <- sum(!df_comments$comment_handled)
      if (n_unhandled == 0) {
        shiny::tagList(
          "Comments",
          shiny::tags$span(
            class = "badge bg-success text-dark ms-1",
            n_unhandled
          )
        )
      } else {
        shiny::tagList(
          "Comments",
          shiny::tags$span(
            class = "badge bg-warning text-dark ms-1",
            n_unhandled
          )
        )
      }
    })


    # render table once; isolate show_all so toggling doesn't cause full re-render
    output$progress_table <- reactable::renderReactable({
      shiny::validate(shiny::need(shiny::isTruthy(flags_out$images_edited()), "No data to show"))
      df <- flags_out$images_edited() |>
        dplyr::mutate(goto_img = "open")
      if (!input$show_all){
        df <- df |>
          dplyr::filter(!comment_handled)
      }
      reactable::reactable(
        df,
        columns = list(
          comment_handled = reactable::colDef(
            cell = function(value) {
              # Render as an X mark or check mark
              if (value) "\u2714\ufe0f Yes" else "\u274c No" 
          }),
          goto_img = reactable::colDef(
            name = "",
            cell = function(value, row_index) {
              row <- df[row_index, ]
              htmltools::tags$button(
                class = "btn btn-sm btn-outline-primary",
                onclick = sprintf(
                  "Shiny.setInputValue('comments-goto_img', {woodpiece: '%s', image: '%s', nonce: Math.random()})",
                  row$woodpiece_label, row$image_label
                ),
                "Go to image"
              )
            }
          )
        ),
        # selection = "multiple",
        # defaultSelected = which(df$comment_handled),
        compact = TRUE,
        borderless = FALSE
      )
    })

    shiny::observe({
      bslib::nav_select("tabs", selected = "Quality check series", session = main_session)
    }) |> shiny::bindEvent(input$goto_img)

    output$debug <- shiny::renderPrint({
      # testing
    })

    # return the clicked image
    list(
      goto_img = shiny::reactive(input$goto_img)
    )

  })
}
