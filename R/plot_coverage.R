#' Plot overview of annual coverage of images
#'
#' For a single tree, each image covers a span of years. This function allows
#' to visualize which annual rings are covered by which image and which rings
#' have been flagged due to which issues.
#'
#' @param woodpiece the woodpiece (core) for which the plot should be created
#' @param df_rings the dataframe containing the rings data (including flags)
#'
#' @export
plot_woodpiece_coverage <- function(woodpiece, df_rings) {
  # assert that the df_rings underwent validation and double rings checks
  beepr::beep_on_error(
    checkmate::assert_names(colnames(df_rings),
                            must.include = c('incomplete_ring', 'missing_ring',
                                             'duplicate_ring')),
    sound = 2
  )

  df_plot <- df_rings %>% dplyr::filter(woodpiece_code == woodpiece)

  # check that df_rings contains data from the provided woodpiece code
  if (nrow(df_plot) < 1){
    beepr::beep(sound=2)
    stop("The provided dataframe does not contain data from woodpiece ", woodpiece)
  }
  # TODO: additional input checks?

  # reformat data for plot
  df_plot <- df_plot %>%
    dplyr::mutate(incomplete_ring = dplyr::case_when(incomplete_ring ~ year),
                  missing_ring = dplyr::case_when(missing_ring ~ year),
                  duplicate_ring = dplyr::case_when(duplicate_ring ~ year)) %>%
    # correct order of images
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)),
                  slide_code = factor(slide_code, levels = rev(unique(slide_code))))

  # PLOTTING
  p_cov <- df_plot %>%
    ggplot2::ggplot(ggplot2::aes(x=year, y=image_code, group=image_code)) +
    # add horizontal lines for the years covered by each image
    # ggplot2::geom_line(linewidth = 4, color = "grey90", lineend = 'round',
    #                    position = ggplot2::position_nudge(y=-0.1)) +
    # with interactive functionality for use in shiny
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = image_code),
      tooltip = 'Click to open image!',
      linewidth = 4, color = "grey90", lineend = 'round',
      position = ggplot2::position_nudge(y=-0.1)) +
    #onclick = "window.open(\"https://davidgohel.github.io/ggiraph/\")") +
    # add points colored by the number of cells for each year
    ggplot2::geom_point(ggplot2::aes(x=year, y=image_code, fill=cno),
                        shape=21, size = 3, stroke = 0.1) +
    ggplot2::scale_fill_steps(low='lightskyblue', high='blue4',
                              name = "N cells") +
    # add symbols for the flagged years
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes(x=missing_ring, y=image_code, col="missing"),
      shape=0, stroke = 0.6, size=7, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=duplicate_ring, y=image_code, col="duplicate"),
      shape=1, stroke = 0.7, size = 6, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=incomplete_ring, y=image_code, col="incomplete"),
      shape=23, stroke = 0.7, size=4, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c('red4','firebrick1','darkorange2'),
                                breaks = c('incomplete', 'duplicate', 'missing'),
                                name = 'Ring flags') +
    # group images of each slide together
    ggplot2::facet_grid(rows = ggplot2::vars(slide_code),scales = 'free_y') +
    # theme and labels
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "grey25")) +
    ggplot2::labs(
      title = paste("Year coverage for woodpiece", woodpiece),
      x = "Year", y = "Image")

  # make plot size dynamically dependent on nr of rows (images) and total
  # range of years, with some hard limits to avoid extremes (all in px)
  # TODO: check if this is the best way to set the size
  image_width <- min(
    max((max(df_plot$year)-min(df_plot$year))*75+800, 2000),
    6000)
  image_height <- min(
    max(length(unique(df_plot$image_code))*100+200, 750),
    2000)

  return(
    list(p = p_cov,
         image_width = image_width,
         image_height = image_height)
  )
}


#' Create coverage plots for all trees
#'
#' Function to create coverage plots for all unique treecodes in the provided
#' rings df (usually QWA_data$rings) in a for loop. Plots can be shown and/or
#' saved to a specified path.
#'
#' @param df_rings rings dataframe
#' @param save_plot should the plot be written to disk under path_out
#' @param path_out path where the plot should be saved.
#' @export
create_coverage_plots <- function(df_rings,
                                  save_plot = TRUE, path_out = './'){
  woodpiece_codes <- unique(df_rings$woodpiece_code)
  for (wp in woodpiece_codes){

    covplot_list <- plot_woodpiece_coverage(wp, df_rings)

    # save the plot to png
    plot_name <- file.path(path_out, paste0(wp,'_coverage.png')) # TODO: make path safe
    ggplot2::ggsave(plot_name, covplot_list$p, bg='white',
                    width = covplot_list$image_width,
                    height = covplot_list$image_height, units='px')
    message(woodpiece, ': coverage plot saved to ', plot_name)
  }
}


