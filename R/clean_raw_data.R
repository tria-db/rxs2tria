

# # recalculate the missing cell measures
#   cell_meas_exist <- intersect(cell_meas_all, names(df_cells))
#   if (length(cell_meas_exist) < length(cell_meas_all)) {
#     df_cells <- df_cells |>
#     # remove any existing 
#     dplyr::select(!dplyr::any_of(cell_meas_exist)) |> 
#     dplyr::mutate(
#       # add total cell area and rwd2:
#       tca = .data$la + .data$cwa,
#       rwd2 = .data$cwtrad / .data$drad,
#       # add cwt-based density:
#       # assume a circular lumen area with homogenous cell wall thickness around it;
#       # for latewood-like cells, take overall average cwt,
#       # for earlywood-like cells, only consider cwttan, which avoids pit artefacts
#       # helper:
#       lr = sqrt(.data$la / pi),
#       wa = dplyr::if_else(.data$rtsr < 1, 
#                           (.data$lr + .data$cwttan)^2 * pi - .data$la,
#                           (.data$lr + .data$cwtall)^2 * pi - .data$la),
#       dcwt = .data$wa / (.data$la + .data$wa),
#       # standardized raddistr (by mrw):
#       raddistr.st = .data$rraddistr * .data$mrw / 100,
#       # add mean cwt: mean of radial and tangential cwt if Mork index latewood-like,
#       # in earlywood-like cells take cwttan
#       cwtall.adj = dplyr::if_else(.data$rtsr < 1, .data$cwttan, .data$cwtall),
#       cdrad = .data$drad + 2*.data$cwttan,
#       cdtan = .data$dtan + 2*.data$cwtrad,
#       cdratio = .data$cdrad / .data$cdtan,
#       # add sector100: 
#       sector100 = as.numeric(cut(.data$rraddistr,
#                                  b = seq(from=0, to=100, by= 1),
#                                  labels = 1:100,
#                                  include.lowest = TRUE)),
#       # round for data with rraddistr just above 100, otherwise leave NA
#       sector100 = dplyr::if_else(.data$rraddistr > 100 & .data$rraddistr <= 101, 
#                                  100, .data$sector100)
#     ) |> 
#     # remove helper columns and existing measures (take the originals instead)
#     dplyr::select(!"lr", !"wa") |> 
#     dplyr::select(!dplyr::any_of(cell_meas_exist))
#   }

  # mork <- 1
  # df_ewlw <- df_cells |> 
  #   dplyr::filter(!is.na(.data$rtsr), !is.na(.data$mrw)) |> # remove cells that do not have a measured CWT or MRW
  #   # TODO: check grouping - could also group by slide_label to treat overlaps / incomplete rings together?
  #   dplyr::group_by(.data$image_label, .data$year, .data$sector100) |> 
  #   dplyr::summarise(rtsr.mean = mean(.data$rtsr),
  #                    mrw = mean(.data$mrw), .groups = "drop_last") |> 
  #   # TODO: fill is 10 so the last 4 sectors are always LW -> very narrow incomplete rings seen as LW only
  #   dplyr::mutate(rollmean = zoo::rollmean(.data$rtsr.mean , 9, fill = c(NA, NA, 10))) |> 
  #   dplyr::summarise(
  #     mrw = mean(.data$mrw),
  #     # the boundary is set at the highest sector with a rolling mean <= mork
  #     # TODO: check edge cases
  #     max_EW_sector = max_na_inf(.data$sector100[.data$rollmean <= mork]),
  #     eww = dplyr::if_else(.data$max_EW_sector >= 0, .data$max_EW_sector*.data$mrw/100, 0),
  #     lww = .data$mrw - .data$eww,
  #     .groups = 'drop'
  #   )

  # df_cells <- df_cells |> 
  #   dplyr::left_join(df_ewlw[,c('image_label','year','max_EW_sector')],
  #                    by = c('image_label', 'year')) |> 
  #   dplyr::mutate(ew_lw = dplyr::if_else(.data$sector100 <= .data$max_EW_sector, "EW", "LW")) |> 
  #   dplyr::select(!"mrw", !"max_EW_sector")

  # df_rings <- QWA_data$rings |> 
  #   dplyr::left_join(
  #     df_ewlw |> dplyr::select(!"mrw", !"max_EW_sector"),
  #     by = c("image_label", "year"))

  # cli::cli_inform(c(
  #   "v" = "Added additional cell measures and EW / LW estimation."
  # ))

  # new_QWAdata(cells = df_cells, rings = df_rings)


# #' Complete the rings df with all years present in the cell data
# #'
# #' This function takes the QWA data (cells and rings dataframes) and returns an
# #' extended version of the rings dataframe, with additional rows for years that
# #' were only present in the cells data, and additional columns for the number
# #' of cells per ring (cno) and the mean cwttan per ring (mean_cwttan; this is
# #' later used to check if CWT estimates were made).
# #'
# #' @param QWA_data a list containing the cells and rings dataframes
# #'
# #' @return an extended dataframe of rings data
# #' @keywords internal
# complete_rings_log <- function(QWA_data){
#   # get a list of all annual rings in cells data (distinct image_label, year),
#   # with added cell counts and mean cwttan per ring
#   df_rings_log <- QWA_data$cells |>
#     dplyr::group_by(image_label, year) |>
#     dplyr::summarise(cno = dplyr::n(),
#                      mean_cwttan = mean(cwttan, na.rm = TRUE),
#                      .groups = 'drop')

#   # combine with rings data
#   df_rings_log <- df_rings_log |>
#     dplyr::full_join(QWA_data$rings,
#                      by = c('image_label', 'year')) |>
#     dplyr::select(woodpiece_label, slide_label, dplyr::everything()) |>
#     dplyr::group_by(image_label) |>  # fill any missing woodpiece/slide labels by image
#     tidyr::fill(woodpiece_label, slide_label, .direction = 'downup') |>
#     dplyr::ungroup() |>
#     dplyr::arrange(image_label, year) |>  # arrange by year within image bc missing rings can lead to disordered years
#     dplyr::mutate(cno = tidyr::replace_na(cno, 0)) # replace NA cno with 0

#   df_rings_log
# }


# #' Check whether the data have CWT estimates
# #'
# #' This function checks if the raw data include cell wall thickness estimates,
# #' since ROXAS analyses can be run with or without estimating CWT. We generally
# #' require CWT estimates in the case of conifer data, but not for angiosperms.
# #' Thus if any images with all NA cwttan are found, the function will issue a
# #' warning.
# #'
# #' @param df_rings_log the dataframe containing the rings data incl. mean_cwttan
# #'
# #' @return TRUE if CWT estimates are present for all images, FALSE otherwise
# #' @keywords internal
# check_cwt_estimates <- function(df_rings_log){
#   # check that the cell data include cell wall thickness estimates
#   # (i.e., at least some cells per image need to have a nonNAN cwt value)
#   no_cwt <- df_rings_log |>
#     dplyr::group_by(image_label) |>
#     dplyr::summarise(no_cwt = all(is.na(mean_cwttan))) |>
#     dplyr::filter(no_cwt)

#   if (nrow(no_cwt) > 0){
#     img_labels <- no_cwt$image_label[1:min(9, nrow(no_cwt))]
#     if (nrow(no_cwt) > 9) {img_labels <- c(img_labels, '...')}
#     cli::cli_warn(c(
#       "!" = "Missing cell wall thickness estimates detected",
#       "i" = "For conifer (but not angiosperm) data, performed CWT analysis is expected.",
#       "{nrow(no_cwt)} image{?s} found without cell wall thickness estimation:",
#       img_labels
#     ))
#     return(FALSE)
#   }

#   TRUE
# }


# #' Check that the ring data are properly dated
# #'
# #' This function checks that the ring data are properly dated, i.e. that the year
# #' variable is a sequence without gaps or duplicates within each image, is never
# #' NA, and does not contain values in the future, nor after the outmost year of
# #' a corresponding images metadata data frame (e.g. QWAimages object), if provided.
# #' If any invalid years are detected, the function will issue an error.
# #'
# #' @param df_rings_log the data frame containing the rings data with cell counts
# #' @param df_meta a data frame with image_label and outmost_year (optional).
# #'
# #' @return TRUE if all the dating checks are passed, aborts otherwise
# #' @keywords internal
# #' @noRd
# check_dating <- function(df_rings_log, df_meta = NULL){
#   current_year <- as.numeric(format(Sys.Date(), "%Y"))

#   # ensure we have complete year sequences per image: fill values get missing_year TRUE
#   df_rings_dating <- df_rings_log |>
#     dplyr::select(woodpiece_label, slide_label, image_label, year, cno) |>
#     dplyr::mutate(missing_year = FALSE) |>
#     dplyr::arrange(image_label, year) %>%
#     dplyr::group_by(image_label) %>%
#     tidyr::complete(year = tidyr::full_seq(year, 1),
#                     fill = list(cno = 0, missing_year = TRUE), explicit = FALSE) %>%
#     tidyr::fill(woodpiece_label, slide_label, .direction = 'downup') |>
#     dplyr::ungroup()

#   # find duplicate years within an image
#   df_rings_dating <- df_rings_dating |>
#     dplyr::group_by(image_label) |>
#     dplyr::mutate(dupl_year = duplicated(year))

#   # if df_meta (resp. $outmost_year) is not given, create dummy
#   if (is.null(df_meta$outmost_year)) {
#     df_meta <- data.frame(
#       image_label = unique(df_rings_log$image_label),
#       outmost_year = Inf
#     )
#   }

#   # join outmost year by image_label
#   df_rings_dating <- df_rings_dating |>
#     dplyr::left_join(
#       df_meta %>% dplyr::select(image_label, outmost_year),
#       by = 'image_label'
#     )

#   # check dating: year is not NA, not in future, not after outmost_year
#   df_rings_dating <- df_rings_dating |>
#     dplyr::mutate(
#       undated = is.na(year),
#       in_future = year > current_year,
#       after_outmost = year > outmost_year
#     )

#   dating_issues <- df_rings_dating |>
#     dplyr::filter(missing_year | dupl_year | undated | in_future| after_outmost) |>
#     dplyr::group_by(image_label) |>
#     dplyr::summarise(
#       dplyr::across(c(missing_year, dupl_year, undated, in_future, after_outmost),
#                     \(x) sum(x, na.rm = TRUE)), .groups = "drop") |> 
#     dplyr::mutate(
#       missing_year = dplyr::if_else(missing_year > 0, glue::glue("{missing_year} missing"), NA_character_),
#       dupl_year = dplyr::if_else(dupl_year > 0, glue::glue("{dupl_year} duplicates"), NA_character_),
#       undated = dplyr::if_else(undated > 0, glue::glue("{undated} undated"), NA_character_),
#       in_future = dplyr::if_else(in_future > 0, glue::glue("{in_future} in future"), NA_character_),
#       after_outmost = dplyr::if_else(after_outmost > 0, glue::glue("{after_outmost} after outmost"), NA_character_)
#     ) |> 
#     tidyr::unite("issues", missing_year, dupl_year,  undated, in_future, after_outmost,
#                  na.rm = TRUE, sep = ", ") |>
#     glue::glue_data("{image_label}: {issues}")

#   if (length(dating_issues) > 0){
#     img_labels <- dating_issues[1:min(9, length(dating_issues))]
#     if (length(dating_issues) > 9) {img_labels <- c(img_labels, '...')}
#     cli::cli_abort(c(
#       "x" = "Dating issues detected in rings data",
#       "i" = "The following images have problems with the dating:",
#       img_labels
#     ),
#     class = "rxs2tria_val_error",
#     issues = dating_issues)
#   }

#   invisible(TRUE)  
# }
# to access the issues:
# result <- tryCatch(
#   check_dating(df_rings_log, df_meta),
#   rxs2tria_val_error = function(e) e   # returns the condition object
# )


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
  top_border_points <- cells.innermost %>%
    dplyr::arrange(xpix) %>%
    dplyr::mutate(xgroup = dplyr::ntile(xpix, max(round(n_cells/50),5))) %>%
    dplyr::group_by(xgroup) %>%
    dplyr::slice(which.min(ypix)) %>%
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

  return(list(
    mae = mae,
    medYleft = medYleft,
    medYright = medYright,
    mindist = mindist,
    #meddist = meddist,
    incomplete_inner = is_incomplete_v2, # USE GEORG'S METHOD
    incomplete_innerv2 = is_incomplete
  ))
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
  df_rings_log <- df_rings_log %>%
    dplyr::group_by(image_label) %>%
    dplyr::mutate(innermost_ring = year == min(year),
                  outermost_ring = year == max(year)) %>%
    dplyr::ungroup()

  # get the cells data for all innermost rings, nest by img and year, and
  # add spatial resolution from df_meta
  grouped_innermost_celldata <- df_cells_all %>%
    dplyr::right_join(df_rings_log %>% dplyr::filter(innermost_ring) %>%
                        dplyr::select(image_label,year),
                      by = c('image_label','year')) %>%
    #dplyr::group_by(image_label, year) %>%
    tidyr::nest(.by = c(image_label, year)) %>%
    dplyr::left_join(df_meta[c('image_label','spatial_resolution')],
                     by = c('image_label'))

  # apply the incompleteness-check function to the nested innermost cells data
  df_rings_innermost <- grouped_innermost_celldata %>%
    dplyr::mutate(incomplete_inner = purrr::map2(data, spatial_resolution,
                                                 check_incomplete_innermost)) %>%
    tidyr::unnest_wider(incomplete_inner) %>%
    dplyr::select(-data, -spatial_resolution)

  # add results back to the df_rings_log
  df_rings_log <- df_rings_log %>%
    dplyr::left_join(df_rings_innermost, by = c('image_label','year'))

  # add a column to flag the incomplete border rings
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(incomplete_ring = (outermost_ring & is.na(mrw)) | (incomplete_inner),
                  incomplete_fct_check = incomplete_inner != incomplete_innerv2,# TODO: check if this ever occurs and for what reason
                  incomplete_ring = ifelse(is.na(incomplete_ring), FALSE, incomplete_ring))

  return(df_rings_log)
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
    dplyr::select(-duplicate_rank,-duplicate_sel)

  return(df_rings_log)
}


# #' Validate the raw QWA data
# #'
# #' Initial checks to ensure the quality of the raw QWA data.
# #' This function checks for the following issues:
# #' - undated images, i.e. YEAR is NA or in the future (raises error)
# #' - images without cell wall thickness estimates (raises warning, since CWT is
# #' required for conifers but may not be available in the case of angiosperms)
# #'
# #' Next, the function identifies the rings with the following issues:
# #' - incomplete rings
# #' - missing rings
# #' - duplicate rings
# #'
# #' Here, by **incomplete** rings we mean those at the inner (pith) and outer (bark)
# #' boundaries of an images, which are cut-off by the image or slide border.
# #' For these rings, some cells are usually recognized but the MRW can NOT
# #' (outer) or NOT ACCURATELY (inner) be estimated. In some cases, the user may
# #' have manually deleted the incomplete rings within ROXAS already, so it is not
# #' a priori clear that all inner- and outermost rings per image are incomplete.
# #'
# #' NOTE: Because ROXAS uses the outer ring boundary to estimate MRW, the
# #' innermost ring generally has an MRW estimate (that is not based on the true
# #' ring boundary but rather the image border), while the outermost ring has no
# #' MRW value (except if it is actually complete either because it is at the
# #' at the bark or because the user removed the incomplete ring manually in ROXAS).
# #' Therefore, we perform an additional check on the border shape and position to
# #' check if an innermost ring is incomplete,  while the outermost ring is flagged
# #' as incomplete if and only if it has no MRW.
# #'
# #' **Missing** rings are for years that have no discernible ring in the image, but
# #' have been manually added in ROXAS during cross-dating, leading to an entry
# #' in the rings data but no corresponding no entries (cells) in the cells data.
# #' This is usually the case with wedging rings.
# #'
# #' **Duplicate** rings are those that are present in multiple images due to them
# #' overlapping. All years which have cells in more than one image are flagged
# #' and ranked by their number of cells. The (complete) year with the highest
# #' number of cells for each overlap is the one that would then usually be
# #' selected for further analysis when building chronologies.
# #'
# #' @param QWA_data a `QWAdata` object containing the cells and rings dataframes
# #' @param rxs_meta a [QWAmetadata] or [QWAimages] object whose image-level
# #'   metadata provides `spatial_resolution` (required for the incomplete
# #'   innermost ring check) and `outmost_year`.
# #' @param exclude_mode how should the exclude_issues column be initialized, to
# #'   exclude any incomplete or missing rings (`"either"`, default) or only the 
# #'   incomplete rings (i.e. not flagging missing/wedging rings for exclusion
# #'   from analyses, `"incomplete_only"`).
# #' @returns A `QWAdata` object with the validated data: cells unchanged, rings
# #'   with added flag columns.
# #' @export
# #'
# validate_QWA_data <- function(QWA_data, rxs_meta, 
#                               exclude_mode = c("either", "incomplete_only"),
#                               verbose_flags = FALSE) { # for debugging only
#   checkmate::assert_class(QWA_data, "QWAdata")
#   checkmate::assert_subset(
#     c('image_label','year','cwttan'),
#     names(QWA_data$cells)
#   )
#   checkmate::assert_subset(
#     c("woodpiece_label","slide_label", "image_label", "year"),
#     names(QWA_data$rings)
#   )

#   checkmate::assert_class(rxs_meta, "QWAimages")
#   df_meta <- rxs_meta
#   checkmate::assert_subset(
#     c('image_label','spatial_resolution','outmost_year'),
#     names(df_meta)
#   )
#   checkmate::assert_flag(verbose_flags)

#   # get a complete list of all the annual rings (years) in rings AND cells data
#   # with the ring measurements and additional cell count per ring and mean cwttan
#   df_rings_log <- complete_rings_log(QWA_data)

#   # check that the data have cwt estimates
#   all_imgs_have_cwt <- check_cwt_estimates(df_rings_log)
#   # can remove mean_cwttan now
#   df_rings_log <- df_rings_log %>% dplyr::select(-mean_cwttan)

#   # check that the data are dated
#   all_imgs_dated <- check_dating(df_rings_log, df_meta)

#   # flag incomplete rings
#   df_rings_log <- flag_incomplete_rings(df_rings_log, QWA_data$cells, df_meta)

#   # flag missing rings
#   df_rings_log <- df_rings_log %>%
#     dplyr::mutate(missing_ring = is.na(cno) | (cno < 5) | dplyr::coalesce(mrw < 10, FALSE), # TODO: (should never have NA cno anymore because we replace with 0), but mrw might be NA for incomplete rings -> coalesce. make thresholds function params?
#                   no_MRW_other = is.na(mrw) & !(outermost_ring | innermost_ring)) # TODO: check if this ever occurs and for what reason

#   # flag duplicate rings: flag rings which are represented in more than one image per core
#   # add duplicate_sel column to indicate which of the duplicates to keep for chronology.
#   # default behavior is to select the year with the highest cell count (that isn't incomplete or missing)
#   df_rings_log <- flag_duplicate_rings(df_rings_log) # replace with duplciate_sel?

#   mode <- match.arg(exclude_mode)

#   df_rings_log <- df_rings_log |> dplyr::mutate(
#     mrw = dplyr::if_else(missing_ring & is.na(mrw) & cno < 5, 0, mrw),
#     ra = dplyr::if_else(missing_ring & is.na(ra) & cno < 5, 0, ra),
#     eww = dplyr::if_else(missing_ring & is.na(eww) & cno < 5, 0, eww), # TODO: requires ew/lw estimation - make flexible?
#     lww = dplyr::if_else(missing_ring & is.na(lww) & cno < 5, 0, lww),
#   )

#   df_rings_log <- df_rings_log %>%
#     dplyr::mutate(
#       exclude_issues = switch(
#         mode,
#         "either" = incomplete_ring | missing_ring,
#         "incomplete_only" = incomplete_ring
#       )
#     )

#   # TODO: finalize after checking, can already remove unnecessary cols in functions
#   # remove unwanted columns
#   if (!verbose_flags) {
#     df_rings_log <- df_rings_log |>
#       dplyr::select(-dplyr::any_of(c(
#         'innermost_ring','outermost_ring',
#         'mae','medYleft','medYright','mindist',
#         'incomplete_inner', 'incomplete_innerv2', 'incomplete_fct_check',
#         'no_MRW_other')))
#   }

#   # output summary
#   n_incomplete <- sum(df_rings_log$incomplete_ring)
#   n_missing <- sum(df_rings_log$missing_ring)
#   n_duplicate <- sum(df_rings_log$duplicate_ring)
#   n_dupl_years <- df_rings_log[df_rings_log$duplicate_ring,] |>
#     dplyr::distinct(woodpiece_label, year) |> nrow()

#   cli::cli_inform(c(
#     "i" = "The following issues were automatically detected:",
#     " " = "Rings flagged as incomplete: {n_incomplete}",
#     " " = "Rings flagged as missing: {n_missing}",
#     " " = "Rings flagged as duplicate: {n_duplicate} ({n_dupl_years} unique years)",
#     "v" = "QWA data have been validated!"
#   ))

#   new_QWAdata(cells = QWA_data$cells, rings = df_rings_log)
# }
  