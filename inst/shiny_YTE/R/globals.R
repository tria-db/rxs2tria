# globals ----------------------------------------------------------------------

# the validation check function
validate_user_flags <- function(df_wp){
  # one selected duplicate per group of duplicates?
  unique_best_duplicate <- df_wp %>%
    dplyr::filter(duplicate_ring) %>%
    dplyr::group_by(woodpiece_code,year) %>%
    dplyr::summarise(sel_count = sum(duplicate_sel == TRUE), .groups = 'drop') %>%
    dplyr::filter(sel_count != 1) %>% nrow() == 0
  # are there user comments for all other_reason flags?
  no_missing_comments <- df_wp %>%
    dplyr::filter(other_issues & (is.na(other_reason) | other_reason == "")) %>%
    nrow() == 0
  # are there any comments without the corresponding other_reason flag?
  no_superfluous_comments <- df_wp %>%
    dplyr::filter(!other_issues & !is.na(other_reason) & other_reason != "") %>%
    nrow() == 0

  return(c(unique_best_duplicate, no_missing_comments, no_superfluous_comments))
}

# the plot generation function
shiny_cov_plot <- function(df){

  # reformat data for the plot
  df_plot <- df %>%
    dplyr::mutate(
      # set duplicate to FALSE if duplicate_sel is TRUE (we plot duplicate_sel instead)
      duplicate_ring = dplyr::if_else(duplicate_sel | is.na(duplicate_sel),
                                      FALSE, duplicate_ring),
      # labels for the hover text
      lbl_inc = dplyr::case_when(incomplete_ring ~ "incomplete"),
      lbl_dupl = dplyr::case_when(duplicate_ring ~ "duplicate"),
      lbl_dupl_sel = dplyr::case_when(duplicate_sel ~ "best duplicate"),
      lbl_miss = dplyr::case_when(missing_ring ~ "missing"),
      lbl_other = dplyr::case_when(other_issues ~ "other"),
      # to plot the years with the respective flags
      incomplete_ring = dplyr::case_when(incomplete_ring ~ year),
      duplicate_ring = dplyr::case_when(duplicate_ring ~ year),
      duplicate_sel = dplyr::case_when(duplicate_sel ~ year),
      missing_ring = dplyr::case_when(missing_ring ~ year),
      other_issues = dplyr::case_when(other_issues ~ year)) %>%
    # to create the hover labels
    tidyr::unite(col = "labels", lbl_inc, lbl_dupl, lbl_dupl_sel, lbl_miss, lbl_other,
                 na.rm = TRUE, sep = ", ", remove = TRUE) %>%
    dplyr::mutate(
      labels = dplyr::if_else(labels == "", "none", labels),
      labels = paste0("flags: ", labels)) %>%
    # correct order of images to start with earliest
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)),
                  slide_code = factor(slide_code, levels = rev(unique(slide_code))))

  # x limits for plot
  min_year <- floor(min(df_plot$year, na.rm = TRUE) / 5) * 5
  max_year <- ceiling(max(df_plot$year, na.rm = TRUE) / 5) * 5

  # PLOTTING
  p_cov <- suppressWarnings({ # to avoid the unknown aes 'text' warning
    df_plot %>%
      ggplot2::ggplot(ggplot2::aes(x=year, y=image_code, group=image_code)) +
      #add horizontal lines for the years covered by each image
      ggplot2::geom_line(linewidth = 4, color = "grey85", lineend = 'round',
                         position = ggplot2::position_nudge(y=-0.1)) +
      # add points colored by the number of cells for each year
      ggplot2::geom_point(ggplot2::aes(fill = cno, text = labels),
                          shape=21, size = 3, stroke = 0.1) +
      ggplot2::scale_fill_gradient(low='lightskyblue', high='blue4') +
      # add symbols individually for all the flagged years
      ggplot2::geom_point(
        ggplot2::aes(x=incomplete_ring, y=image_code),
        color = 'red4', shape=23, stroke = 0.5, size=4, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=duplicate_ring, y=image_code),
        color = 'firebrick1', shape=1, stroke = 0.5, size = 5, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=duplicate_sel, y=image_code),
        color = 'springgreen4', shape=1, stroke = 0.5, size = 5, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=missing_ring, y=image_code),
        color = 'darkorange2', shape=0, stroke = 0.4, size=6, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=other_issues, y=image_code),
        color = 'gold', shape=1, stroke = 0.5, size=4, na.rm = TRUE) +
      # 5-year ticks on x axis
      ggplot2::scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
      # group images of each slide together
      ggplot2::facet_grid(rows = ggplot2::vars(slide_code),scales = 'free_y') +
      # theme and labels
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(colour = "grey25"),
                     axis.text.y = ggplot2::element_text(size = 7),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(x = "Year", fill = 'Nr of Cells')
  })

  p_cov <- plotly::ggplotly(p_cov, tooltip = c('x','y','labels'))
  return(p_cov)
}
