#' Helper function to check if an innermost ring is incomplete
#'
#' Given a dataframe with cells data from a specific year (usually the innermost
#' one), this function checks whether the estimated inner ring border is very
#' close to 0 (i.e. the image border) and also very straight (i.e. very similar
#' at both sides of the image). This indicates that the ring is cut-off and
#' ROXAS used the image border instead of the true ring border to estimate MRW.
#'
#' @param cells.innermost a dataframe containing the cells data for the innermost ring
#' @param res the spatial resolution of the image in microns per pixel
#'
#' @return a logical indicating whether the true inner ring border lies outside
#' the image border and the ring is thus incomplete
#' @keywords internal
# TODO: compare my method with GvA's original one, check thresholds
check_incomplete_innermost <- function(cells.innermost, res){
  # first check that we do have cell data for the year in question,
  # if not, it means the cells were manually excluded and it is def incomplete
  if (nrow(cells.innermost) < 1 || all(is.na(cells.innermost$xpix))){
    return(list(mae = NA, medYleft = NA, medYright = NA, mindist = NA,
                incomplete_inner = TRUE, incomplete_innerv2 = TRUE))
  }

  # estimate the Y position of the inner border respective to cell based on
  # raddistr (converted from microns to pixels with the image's spatial resolution)


  cells.innermost$ringposY <- cells.innermost$ypix - cells.innermost$raddistr*res

  # sort cells by X coordinate
  cells.innermost <- cells.innermost[order(cells.innermost$xpix),]

  # MAE to the straight line at Y=0 (in microns)
  mae <- mean(abs(cells.innermost$ringposY/res), na.rm=TRUE)
  n_cells <- nrow(cells.innermost)
  # median distance to Y=0 for cells on LHS and RHS of image (in microns)
  medYleft <- median(cells.innermost$ringposY[1:round(n_cells/10)], na.rm=TRUE) / res
  medYright <- median(cells.innermost$ringposY[(round(n_cells/10*9)):n_cells], na.rm=TRUE) / res

  # check if the estimated position of the inner ring border is very close to
  # the image border (the thresholds are in microns)
  is_incomplete <- (mae < 20) & (abs(medYleft - medYright) < 15)

  # ALTERNATIVE CALC METHOD BY GvA:
  # IF the inner ring border corresponds to the image border, then
  # SCAL corresponds roughly to the actual spatial resolution, and there is not
  # much variation in the ratio. but if the true border is far away and not
  # straight then this estimate oscillates heavily around 0 (but I'm not quite sure what it would mean?)
  SCAL <- mean(cells.innermost$raddistr / cells.innermost$ypix, na.rm=TRUE)
  cells.innermost$ringposY2 <- cells.innermost$ypix - cells.innermost$raddistr/SCAL

  RP <- median(cells.innermost$ringposY2, na.rm=TRUE) * SCAL
  RP10 <- median(cells.innermost$ringposY2[1:round(n_cells/10)], na.rm=TRUE) * SCAL
  RP90 <- median(cells.innermost$ringposY2[(round(n_cells/10*9)):n_cells], na.rm=TRUE) * SCAL
  # NOTE: the thresholds of 10 microns are based on GvA's expertise
  is_incomplete_v2 <- (RP<=10 & abs(RP90-RP10)<=10)

  # ADDITIONAL TESTING
  # it seems that there are some cases at the border of a slice, where
  # the estimated border is quite far shifted (~50microns) from the image border
  # in these cases, there is a large gap btw the border and the topmost cells
  # the following might be useful for those special cases?
  # Find the top border points
  top_border_points <- cells.innermost |>
    dplyr::arrange(xpix) |>
    dplyr::mutate(xgroup = dplyr::ntile(xpix, max(round(n_cells/50), 5))) |>
    dplyr::group_by(xgroup) |>
    dplyr::slice(which.min(ypix)) |>
    dplyr::ungroup()

  mindist <- min(top_border_points$ypix - top_border_points$ringposY) / res
  meddist <- median(top_border_points$ypix - top_border_points$ringposY) / res

  if ((mae > 20) & (mae < 75) & (mindist > 100)){
    is_incomplete <- TRUE
  }

  # plot for testing purposes
  # p <- ggplot2::ggplot(cells.innermost, ggplot2::aes(x=xpix, y=ypix)) +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_line(ggplot2::aes(x=xpix, y = ringposY), color = 'red') +
  #   ggplot2::geom_line(ggplot2::aes(x=xpix, y = ringposY2), color = 'violet') +
  #   ggplot2::geom_line(ggplot2::aes(x=xpix, y = 0), color = 'orange') +
  #   ggplot2::geom_point(data = top_border_points, ggplot2::aes(x=xpix, y=ypix), color = 'blue') +
  #   ggplot2::scale_y_reverse()
  # print(p)

  list(
    mae = mae,
    medYleft = medYleft,
    medYright = medYright,
    mindist = mindist,
    #meddist = meddist,
    incomplete_inner = is_incomplete_v2, # USE GEORG'S METHOD
    incomplete_innerv2 = is_incomplete
  )
}


#' Flag incomplete rings at tangential image borders.
#'
#' This function determines which of the inner- and outermost rings of each
#' image are incomplete (i.e. extend across the image boundaries).
#' For the innermost rings, we rely on the position and shape of the estimated
#' inner ring border (see helper function `check_incomplete_innermost`). For the
#' outermost rings, we check the MRW value, since ROXAS only estimates an MRW if
#' it can detect an outer ring boundary.
#'
#' @param df_rings_log the dataframe containing the rings data
#' @param df_cells_all the dataframe containing all cells data
#' @param df_meta the dataframe containing the metadata of the images (needed
#' for the spatial resolution)
#'
#' @return the input dataframe with additional logical columns 'incomplete_ring'
#' @keywords internal
# TODO: additional columns
# TODO: what about circular samples?
flag_incomplete_rings <- function(df_rings_log, df_cells_all, df_meta){
  # identify inner- and outermost year per image
  df_rings_log <- df_rings_log |>
    dplyr::group_by(image_label) |>
    dplyr::mutate(innermost_ring = year == min(year),
                  outermost_ring = year == max(year)) |>
    dplyr::ungroup()

  # get the cells data for all innermost rings, nest by img and year, and
  # add spatial resolution from df_meta
  grouped_innermost_celldata <- df_cells_all |>
    dplyr::right_join(df_rings_log |> dplyr::filter(innermost_ring) |>
                        dplyr::select(image_label, year),
                      by = c('image_label', 'year')) |>
    tidyr::nest(.by = c(image_label, year)) |>
    dplyr::left_join(df_meta[c('image_label', 'spatial_resolution')],
                     by = c('image_label'))

  # apply the incompleteness-check function to the nested innermost cells data
  df_rings_innermost <- grouped_innermost_celldata |>
    dplyr::mutate(incomplete_inner = purrr::map2(data, spatial_resolution,
                                                 check_incomplete_innermost)) |>
    tidyr::unnest_wider(incomplete_inner) |>
    dplyr::select(-data, -spatial_resolution)

  # add results back to the df_rings_log
  df_rings_log <- df_rings_log |>
    dplyr::left_join(df_rings_innermost, by = c('image_label', 'year'))

  # add a column to flag the incomplete border rings
  df_rings_log |>
    dplyr::mutate(incomplete_ring = (outermost_ring & is.na(mrw)) | (incomplete_inner),
                  incomplete_fct_check = incomplete_inner != incomplete_innerv2, # TODO: check if this ever occurs and for what reason
                  incomplete_ring = dplyr::if_else(is.na(incomplete_ring), FALSE, incomplete_ring))
}


#' Flag duplicate rings due to overlapping images from the same woodpiece
#'
#' This function identifies duplicate rings, i.e. the same annual ring being
#' captured in two or more images from the same woodpiece due to overlaps.
#' It also ranks the duplicated rings by the number of cells they contain (in
#' decreasing order), since for chronologies, we generally want to keep only the
#' ring with the most cells (rank 1).
#'
#' @param df_rings_log the dataframe containing the rings data
#'
#' @return the input dataframe with additional logical column 'duplicate_ring'
#' and integer column 'duplicate_sel' with the selected ring among duplicates
#' @keywords internal
# TODO: avoid switching too often?
flag_duplicate_rings <- function(df_rings_log){
  # flag which rings are overlapping with others
  df_rings_log <- df_rings_log |>
    dplyr::group_by(woodpiece_label, year) |>
    dplyr::mutate(
      duplicate_ring = dplyr::n() > 1,
      duplicate_rank = dplyr::row_number(-cno), # rank by cell count descending
      duplicate_rank = dplyr::if_else(duplicate_ring, # set rank to NA if not duplicate
                                      duplicate_rank, NA_integer_),
      duplicate_rank = duplicate_rank + 100*as.numeric(missing_ring) + 100*as.numeric(incomplete_ring), # penalty for issues
      duplicate_sel = dplyr::if_else(!duplicate_ring | duplicate_rank == min(duplicate_rank), # select the best ranked duplicate
                                     TRUE, FALSE),
      exclude_dupl = !duplicate_sel) |> # the not-selected duplicates
    dplyr::ungroup() |>
    dplyr::select(-duplicate_rank, -duplicate_sel)
  df_rings_log
}