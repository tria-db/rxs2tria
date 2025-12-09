#' Remove outliers in cells and rings data
#'
#' ROXAS does some automatic outlier detection and replaces the found outliers
#' in cell (and ring?) measures by their negative value. This function replaces
#' these 'negatives' with NA.
#'
#' @param QWA_data a list containing the cells and rings dataframes
#' @returns QWA_data with the 'negative' outliers replaced by NAs
#'
#' @export
# TODO: finalize: sel cols, warn about how many are replaced? sure that these cols cannot have neg values?
remove_outliers <- function(QWA_data){
  checkmate::assert_list(QWA_data, types = "data.frame", len = 2)
  checkmate::check_subset(names(QWA_data), c("cells", "rings"))

  outl_cols_cells <- c("raddistr", "rraddistr", "nbrno", "nbrid", "la", "asp",
                       "majax", "kh", "cwtpi", "cwtba", "cwtle", "cwtri",
                       "cwttan", "cwtrad", "cwtall", "rtsr", "ctsr", "dh",
                       "drad", "dtan", "tb2", "cwa", "rwd")
  outl_cols_rings <- c("ra", "mrw", "rvgi", "rvsf", "rgsgv", "aoiar",
                       "dh_w", "dh_m")

  # get outlier counts for reporting
  cell_outliers <- QWA_data$cells |>
    dplyr::select(dplyr::all_of(outl_cols_cells)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~sum(.x < 0, na.rm = TRUE))) |>
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::filter(value > 0) |>
    glue::glue_data("{name}: {value}")

  ring_outliers <- QWA_data$rings %>%
    dplyr::select(dplyr::all_of(outl_cols_rings)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~sum(.x < 0, na.rm = TRUE))) |>
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::filter(value > 0) |>
    glue::glue_data("{name}: {value}")

  if (length(cell_outliers) > 0){
    cli::cli_inform(c(
      "i" = 'Outliers (negative values) in cells data:',
      cell_outliers
    ))
  }
  if (length(ring_outliers) > 0){
    cli::cli_inform(c(
      "i" = 'Outliers (negative values) in rings data:',
      ring_outliers
    ))
  }

  QWA_data_rm <- list()
  QWA_data_rm$cells <- QWA_data$cells %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(outl_cols_cells),
                                ~ dplyr::if_else(.x < 0, NA_real_, .x)))
  QWA_data_rm$rings <- QWA_data$rings %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(outl_cols_rings),
                                ~ dplyr::if_else(.x < 0, NA_real_, .x)))

  cli::cli_alert_success(
    "Outliers (negative values) have been replaced with NA"
  )

  QWA_data_rm
}


#' Complete cell measures
#'
#' Some additional cell measures are calculated based on the existing data.
#' @keywords internal
max_na_inf <- function(x){
  x_na <- is.na(x)
  if(all(x_na)) -Inf else max(x[!x_na])
}

# calculate additional measures (see rxs complete)
# TODO: finalize
complete_cell_measures <- function(QWA_data){

  df_cells <- QWA_data$cells %>%
    # need MRW for some calculations
    dplyr::left_join(QWA_data$rings %>%
                       dplyr::select(image_label, year, mrw), by=c('image_label', 'year')) %>%
    dplyr::mutate(
      # new:
      tca = la + cwa,
      rwd2 = cwtrad/drad,
      # Add CWT-based density:
      # assume a circular lumen area with homogenous cell wall thickness around it;
      # for latewood-like cells, take overall average CWT,
      # for earlywood-like cells, only consider CWTTAN, which avoids pit artefacts
      # helper:
      lr = sqrt(la / pi),
      wa = ifelse(rtsr < 1, (lr + cwttan)^2 * pi - la, (lr + cwtall)^2 * pi - la),
      # new:
      dcwt = wa / (la + wa),
      # standardized RADDISTR (by MRW): new
      raddistr.st = rraddistr * mrw / 100,
      # Add mean CWT: mean of radial and tangential CWT if Mork index latewood-like,
      # in earlywood-like cells take CWTTAN
      # new:
      cwtall.adj = ifelse(rtsr < 1,  cwttan,  cwtall),
      cdrad = drad + 2*cwttan,
      cdtan = dtan + 2*cwtrad,
      cdratio = cdrad/cdtan
    ) %>%
    # remove helper parameters
    dplyr::select(-lr, -wa)

  df_cells <- df_cells %>%
    dplyr::mutate(
      sector100 = as.numeric(cut(rraddistr, # no grouping needed
                                 b = seq(from=0, to=100, by= 1),
                                 labels = 1:100,
                                 include.lowest = T))) %>%
    # round for data with rraddistr just above 100, otherwise leave NA
    dplyr::mutate(sector100 = dplyr::if_else(rraddistr > 100 & rraddistr <= 101, 100, sector100))



  # ggplot2::ggplot(df_cells %>% dplyr::filter(image_label == 'POG_PISY_02_B_4_2'),
  #                 ggplot2::aes(x = xpix, y = ypix, color = TO.EWLW)) +
  #   ggplot2::geom_point()

  mork <- 1
  df_ewlw <- df_cells %>%
    # need MRW for some calculations
    # dplyr::left_join(QWA_data$rings %>%
    #                    dplyr::select(woodpiece_label, slide_label, image_label, year, mrw), by=c('image_label', 'year')) %>%
    dplyr::filter(!is.na(rtsr), !is.na(mrw)) %>%  # remove cells that do not have a measured CWT or MRW
    # TODO: check grouping
    #dplyr::group_by(woodpiece_label, year, slide_label, sector100) %>%
    dplyr::group_by(image_label, year, sector100) %>%
    dplyr::summarise(RTSR.MEAN = mean(rtsr),
                     mrw = mean(mrw), .groups = 'drop') %>%
    dplyr::group_by(image_label, year) %>%
    dplyr::mutate(ROLLMEAN = zoo::rollmean(RTSR.MEAN, 9, fill = c(NA, NA, 10))) %>% # TODO: 10 so the last 4 are always LW?
    dplyr::summarise(
      mrw = mean(mrw),
      # the boundary is set at the highest sector with a rolling mean below mork
      # TODO: check edge cases
      max_EW_sector = max_na_inf(sector100[ROLLMEAN <= mork]),
      eww = ifelse(max_EW_sector >=0, max_EW_sector*mrw/100, 0),
      lww = mrw - eww, .groups = 'drop'
    )

  df_cells <- df_cells %>%
    dplyr::left_join(df_ewlw[,c('image_label','year','max_EW_sector')],
                     by = c('image_label', 'year')) %>%
    dplyr::mutate(ew_lw = ifelse(sector100 <= max_EW_sector, "EW", "LW")) %>%
    dplyr::select(-mrw,-max_EW_sector)

  df_rings <- QWA_data$rings %>%
    dplyr::left_join(df_ewlw %>% dplyr::select(-mrw), by = c("image_label", "year")) %>%
    dplyr::select(-max_EW_sector)

  return(
    stats::setNames(
      list(df_cells, df_rings),
      c('cells','rings')
    ))
}


#' Complete the rings df with all years present in the cell data
#'
#' This function takes the QWA data (cells and rings dataframes) and returns an
#' extended version of the rings dataframe, with additional rows for years that
#' were only present in the cells data, and additional columns for the number
#' of cells per ring (cno) and the mean cwttan per ring (mean_cwttan; this is
#' later used to check if CWT estimates were made).
#'
#' @param QWA_data a list containing the cells and rings dataframes
#'
#' @return an extended dataframe of rings data
#' @keywords internal
complete_rings_log <- function(QWA_data){
  # get a list of all annual rings in cells data (distinct image_label, year),
  # with added cell counts and mean cwttan per ring
  df_rings_log <- QWA_data$cells |>
    dplyr::group_by(image_label, year) |>
    dplyr::summarise(cno = dplyr::n(),
                     mean_cwttan = mean(cwttan, na.rm = TRUE),
                     .groups = 'drop')

  # combine with rings data
  df_rings_log <- df_rings_log |>
    dplyr::full_join(QWA_data$rings,
                     by = c('image_label', 'year')) |>
    dplyr::select(woodpiece_label, slide_label, dplyr::everything()) |>
    dplyr::group_by(image_label) |>  # fill any missing woodpiece/slide labels by image
    tidyr::fill(woodpiece_label, slide_label, .direction = 'downup') |>
    dplyr::ungroup() |>
    dplyr::arrange(image_label, year) |>  # arrange by year within image bc missing rings can lead to disordered years
    dplyr::mutate(cno = tidyr::replace_na(cno, 0)) # replace NA cno with 0

  df_rings_log
}


#' Check whether the data have CWT estimates
#'
#' This function checks if the raw data include cell wall thickness estimates,
#' since ROXAS analyses can be run with or without estimating CWT. We generally
#' require CWT estimates in the case of conifer data, but not for angiosperms.
#' Thus if any images with all NA cwttan are found, the function will issue a
#' warning.
#'
#' @param df_rings_log the dataframe containing the rings data incl. mean_cwttan
#'
#' @return TRUE if CWT estimates are present for all images, FALSE otherwise
#' @keywords internal
check_cwt_estimates <- function(df_rings_log){
  # check that the cell data include cell wall thickness estimates
  # (i.e., at least some cells per image need to have a nonNAN cwt value)
  no_cwt <- df_rings_log |>
    dplyr::group_by(image_label) |>
    dplyr::summarise(no_cwt = all(is.na(mean_cwttan))) |>
    dplyr::filter(no_cwt)

  if (nrow(no_cwt) > 0){
    img_labels <- no_cwt$image_label[1:min(9, nrow(no_cwt))]
    if (nrow(no_cwt) > 9) {img_labels <- c(img_labels, '...')}
    cli::cli_warn(c(
      "!" = "Missing cell wall thickness estimates detected",
      "i" = "For conifer (but not angiosperm) data, performed CWT analysis is expected.",
      "{nrow(no_cwt)} image{?s} found without cell wall thickness estimation:",
      img_labels
    ))
    return(FALSE)
  }

  TRUE
}


#' Check that the data are properly dated
#'
#' This function checks that the data are properly dated, i.e. that the year
#' variable is a sequence without gaps or duplicate within each image, is never
#' NA, and does not contain values in the future nor after the outmost year.
#' If any invalid years are detected, the function will issue an error.
#'
#' @param df_rings_log the dataframe containing the rings data with cell counts
#'
#' @return TRUE if all the dating checks are passed, FALSE otherwise
#' @keywords internal
check_dating <- function(df_rings_log, df_meta){
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # ensure we have complete year sequences per image
  df_rings_dating <- df_rings_log |>
    dplyr::select(woodpiece_label,slide_label, image_label, year, cno) |>
    dplyr::mutate(missing_year = FALSE) |>
    dplyr::arrange(image_label, year) %>%
    dplyr::group_by(image_label) %>%
    tidyr::complete(year = tidyr::full_seq(year, 1),
                    fill = list(cno = 0, missing_year = TRUE), explicit = FALSE) %>%
    tidyr::fill(woodpiece_label, slide_label, .direction = 'downup') |>
    dplyr::ungroup()

  # check that there are not duplicate years within an image
  df_rings_dating <- df_rings_dating |>
    dplyr::group_by(image_label) |>
    dplyr::mutate(dupl_year = duplicated(year)) # one instance is enough

  # check dating: year is not NA, not in future, not after outmost_year
  df_rings_dating <- df_rings_dating |>
    dplyr::left_join(
      df_meta %>% dplyr::select(image_label, outmost_year),
      by = 'image_label'
    ) |>
    dplyr::mutate(
      undated = is.na(year),
      in_future = year > current_year,
      after_outmost = year > outmost_year
    )

  dating_issues <- df_rings_dating |>
    dplyr::filter(missing_year|dupl_year|undated|in_future|after_outmost) |>
    dplyr::mutate(
      missing_year = dplyr::if_else(missing_year, "missing", NA_character_),
      dupl_year = dplyr::if_else(dupl_year, "duplicate", NA_character_),
      undated = dplyr::if_else(undated, "undated", NA_character_),
      in_future = dplyr::if_else(in_future, "in future", NA_character_),
      after_outmost = dplyr::if_else(after_outmost, "after outmost", NA_character_),
    ) |>
    tidyr::unite("issues",
                 missing_year, dupl_year, undated, in_future, after_outmost,
                 na.rm = TRUE, sep = ", ") |>
    glue::glue_data("{image_label}, {year}: {issues}")

  if (length(dating_issues) > 0){
    img_labels <- dating_issues[1:min(9, length(dating_issues))]
    if (length(dating_issues) > 9) {img_labels <- c(img_labels, '...')}
    cli::cli_abort(c(
      "x" = "Dating issues detected in rings data",
      "i" = "The following images/years have problems with the dating:",
      img_labels
    ))
    return(FALSE)
  }

  TRUE
}



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


#' Validate the raw QWA data
#'
#' Initial checks to ensure the quality of the raw QWA data.
#' This function checks for the following issues:
#' - undated images, i.e. YEAR is NA or in the future (raises error)
#' - images without cell wall thickness estimates (raises warning, since CWT is
#' required for conifers but may not be available in the case of angiosperms)
#'
#' Next, the function identifies the rings with the following issues:
#' - incomplete rings
#' - missing rings
#' - duplicate rings
#'
#' Here, by **incomplete** rings we mean those at the inner (pith) and outer (bark)
#' boundaries of an images, which are cut-off by the image or slide border.
#' For these rings, some cells are usually recognized but the MRW can NOT
#' (outer) or NOT ACCURATELY (inner) be estimated. In some cases, the user may
#' have manually deleted the incomplete rings within ROXAS already, so it is not
#' a priori clear that all inner- and outermost rings per image are incomplete.
#'
#' NOTE: Because ROXAS uses the outer ring boundary to estimate MRW, the
#' innermost ring generally has an MRW estimate (that is not based on the true
#' ring boundary but rather the image border), while the outermost ring has no
#' MRW value (except if it is actually complete either because it is at the
#' at the bark or because the user removed the incomplete ring manually in ROXAS).
#' Therefore, we perform an additional check on the border shape and position to
#' check if an innermost ring is incomplete,  while the outermost ring is flagged
#' as incomplete if and only if it has no MRW.
#'
#' **Missing** rings are for years that have no discernible ring in the image, but
#' have been manually added in ROXAS during cross-dating, leading to an entry
#' in the rings data but no corresponding no entries (cells) in the cells data.
#' This is usually the case with wedging rings.
#'
#' **Duplicate** rings are those that are present in multiple images due to them
#' overlapping. All years which have cells in more than one image are flagged
#' and ranked by their number of cells. The (complete) year with the highest
#' number of cells for each overlap is the one that would then usually be
#' selected for further analysis when building chronologies.
#'
#' @param QWA_data a list containing the cells and rings dataframes
#' @param df_meta a dataframe containing the metadata for the images
#' (spatial_resolution required for the incomplete innermost ring check)
#' @returns validated QWA_data (no changes to cells df, but added minimum required ring flag columns to rings df).
#' @export
#'
validate_QWA_data <- function(QWA_data, df_meta, verbose_flags = FALSE){
  checkmate::assert_list(QWA_data, types = "data.frame", len = 2)
  checkmate::check_subset(names(QWA_data), c("cells", "rings"))
  checkmate::assert_subset(
    c('image_label','year','cwttan'),
    names(QWA_data$cells)
  )
  checkmate::assert_subset(
    c("woodpiece_label","slide_label", "image_label", "year"),
    names(QWA_data$rings)
  )

  checkmate::assert_data_frame(df_meta)
  checkmate::assert_subset(
    c('image_label','spatial_resolution','outmost_year'),
    names(df_meta)
  )
  checkmate::assert_logical(verbose_flags, len = 1)

  # get a complete list of all the annual rings (years) in rings AND cells data
  # with the ring measurements and additional cell count per ring and mean cwttan
  df_rings_log <- complete_rings_log(QWA_data)

  # check that the data have cwt estimates
  all_imgs_have_cwt <- check_cwt_estimates(df_rings_log)
  # can remove mean_cwttan now
  df_rings_log <- df_rings_log %>% dplyr::select(-mean_cwttan)

  # check that the data are dated
  all_imgs_dated <- check_dating(df_rings_log, df_meta)

  # flag incomplete rings
  df_rings_log <- flag_incomplete_rings(df_rings_log, QWA_data$cells, df_meta)

  # flag missing rings
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(missing_ring = is.na(cno) | (cno < 5) | dplyr::coalesce(mrw < 10, FALSE), # TODO: (should never have NA cno anymore because we replace with 0), but mrw might be NA for incomplete rings -> coalesce. make thresholds function params?
                  no_MRW_other = is.na(mrw) & !(outermost_ring | innermost_ring)) # TODO: check if this ever occurs and for what reason

  # flag duplicate rings: flag rings which are represented in more than one image per core
  # add duplicate_sel column to indicate which of the duplicates to keep for chronology.
  # default behavior is to select the year with the highest cell count (that isn't incomplete or missing)
  df_rings_log <- flag_duplicate_rings(df_rings_log) # replace with duplciate_sel?

  # add automatic exclude_year flags: default behavior is to exclude any incomplete or missing years from analysis
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(exclude_issues = incomplete_ring | missing_ring)

  # TODO: affected_tissue: do we need to add a default? or we just leave it (-> leading to nulls in DB) unless user specifies manually?

  # TODO: finalize after checking, can already remove unnecessary cols in functions
  # remove unwanted columns
  if (!verbose_flags){
    df_rings_log <- df_rings_log |>
      dplyr::select(-dplyr::any_of(c(
        'innermost_ring','outermost_ring',
        'mae','medYleft','medYright','mindist',
        'incomplete_inner', 'incomplete_innerv2', 'incomplete_fct_check',
        'no_MRW_other')))
  }

  # output summary
  issue_counts <-  df_rings_log %>%
    dplyr::summarise(dplyr::across(c(incomplete_ring:duplicate_ring),
                                   ~sum(.x,na.rm=TRUE))) %>%
    dplyr::select(incomplete_ring, missing_ring, duplicate_ring)
  message('QWA data have been validated successfully!\n',
          'The following issues were found:')
  message(paste0(capture.output(print(as.data.frame(issue_counts),
                                row.names = FALSE)), collapse='\n'))

  return(
    stats::setNames(
      list(QWA_data$cells, df_rings_log),
      c('cells','rings'))
  )
}




# # Manual flags
# # TODO: finalize
# add_user_flags <- function(QWA_data, years_to_flag){
#   df_rings_log <- QWA_data$rings %>%
#     dplyr::left_join(years_to_flag %>% dplyr::mutate(manual_flag = TRUE),
#                     by = c('image_label','year')) %>%
#     dplyr::mutate(manual_flag = ifelse(is.na(manual_flag), FALSE, manual_flag))
#
#   return(
#     stats::setNames(
#       list(QWA_data$cells, df_rings_log),
#       c('cells','rings')
#     ))
# }
#
#
# finalize_flags <- function(QWA_data){
#   df_rings <- QWA_data$rings
#   if (!('other_issues' %in% colnames(df_rings))){
#     df_rings$other_issues <- FALSE
#   }
#   # TODO: fix the duplicate flags
#   # TODO: set incomplete rings measures to 0
#   # TODO: check missing ring has no cell data
#
#   return(
#     stats::setNames(
#       list(QWA_data$cells, df_rings_log),
#       c('cells','rings')
#     ))
# }






# tbl_rxs_hmgz <- tbl_rxs_v3 %>%
# dplyr::group_by(image_label, YEAR) %>%
#   dplyr::mutate(
#     # Add SECTORS100
#     # TO.SECTORS = as.numeric(cut(RADDISTR.ST, b=seq(from= 0, to=100, by= 100/(NSECTOR[1])), labels=1:(NSECTOR[1]))),
#     SECTOR100 = as.numeric(cut(RRADDISTR,
#                                b = seq(from=0, to=100, by= 1),
#                                labels = 1:100,
#                                include.lowest = T)))  %>%
#   dplyr::ungroup() %>%
#
#   # round for data just above the RRADDISTR of class 100 otherwise just leave NA
#   dplyr::mutate(SECTOR100 = if_else(RRADDISTR > 100 & RRADDISTR <= 101, 100, SECTOR100))

# tbl_YEAR <- tbl_rxs_hmgz %>%
#   dplyr::filter(!is.na(RTSR)) %>%  # remove cells that do not have a measured CWT
#   dplyr::group_by(plot_treecode, YEAR, slide, SECTOR100) %>%
#   dplyr::summarise(RTSR.MEAN = mean(RTSR, na.rm = TRUE),
#                    MRW = mean(MRW, na.rm = TRUE), .groups = 'drop') %>%
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::mutate(ROLLMEAN = zoo::rollmean(RTSR.MEAN, 9, fill = c(NA, NA, 10)),
#                 TO.EWLW = ifelse(SECTOR100 <= max_na_inf(SECTOR100[ROLLMEAN <= mork]), "EW", "LW")) %>%
#   # filter(plot_treecode=="YAM_2756", YEAR ==1940) %>%
#   # ggplot(aes(x=SECTOR100,y=ROLLMEAN, group = paste0(plot_treecode,YEAR), colour= TO.EWLW)) + geom_line()
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::summarise(MRW = mean(MRW),
#                    EWW = ifelse(any(TO.EWLW == "EW", na.rm = TRUE),
#                                 max_na_inf(SECTOR100[TO.EWLW == "EW"] * MRW/100),
#                                 0),
#                    LWW = MRW - EWW, .groups = 'drop')

# tbl_out <- tbl_rxs_hmgz %>%
#   dplyr::left_join(tbl_YEAR %>% select(-MRW),  by = c("plot_treecode", "YEAR", "slide")) %>%
#   dplyr::mutate(TO.EWLW = ifelse(RADDISTR.ST >= EWW, "LW", "EW"))


# TODO: set raddist and rraddist to NA for incomplete rings (are only already NA for outermost incomplete rings)


# TODO: add EW/LW estimation (standard implementation)

# Add SECTOR100
# TODO: does this work with the duplicates / flags?
# TODO: should be as.numeric(as.character()) ???
# dplyr::group_by(woodpiece_label, YEAR) %>%
#   dplyr::mutate(
#     # TO.SECTORS = as.numeric(cut(RADDISTR.ST, b=seq(from= 0, to=100, by= 100/(NSECTOR[1])), labels=1:(NSECTOR[1]))),
#     SECTOR100 = as.numeric(cut(RRADDISTR,
#                                b = seq(from=0, to=100, by= 1),
#                                labels = 1:100,
#                                include.lowest = T)))  %>%
#   dplyr::ungroup() %>%
#   # round for data just above the RRADDISTR of class 100 otherwise just leave NA
#   dplyr::mutate(SECTOR100 = if_else(RRADDISTR > 100 & RRADDISTR <= 101, 100, SECTOR100)) %>%

# tbl_YEAR <- tbl_rxs_hmgz %>%
#   dplyr::filter(!is.na(RTSR)) %>%  # remove cells that do not have a measured CWT
#   dplyr::group_by(plot_treecode, YEAR, slide, SECTOR100) %>%
#   dplyr::summarise(RTSR.MEAN = mean(RTSR, na.rm = TRUE),
#                    MRW = mean(MRW, na.rm = TRUE), .groups = 'drop') %>%
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::mutate(ROLLMEAN = zoo::rollmean(RTSR.MEAN, 9, fill = c(NA, NA, 10)),
#                 TO.EWLW = ifelse(SECTOR100 <= max_na_inf(SECTOR100[ROLLMEAN <= mork]), "EW", "LW")) %>%
#   # filter(plot_treecode=="YAM_2756", YEAR ==1940) %>%
#   # ggplot(aes(x=SECTOR100,y=ROLLMEAN, group = paste0(plot_treecode,YEAR), colour= TO.EWLW)) + geom_line()
#   dplyr::group_by(plot_treecode, YEAR, slide) %>%
#   dplyr::summarise(MRW = mean(MRW),
#                    EWW = ifelse(any(TO.EWLW == "EW", na.rm = TRUE),
#                                 max_na_inf(SECTOR100[TO.EWLW == "EW"] * MRW/100),
#                                 0),
#                    LWW = MRW - EWW, .groups = 'drop')
#
# tbl_out <- tbl_rxs_hmgz %>%
#   dplyr::left_join(tbl_YEAR %>% select(-MRW),  by = c("plot_treecode", "YEAR", "slide")) %>%
#   dplyr::mutate(TO.EWLW = ifelse(RADDISTR.ST >= EWW, "LW", "EW"))




# TODO: what to do with the faulty data: remove? flag only? tell to restart? allow for dating manually?
# saxor / homogenizer remove only the cells with no rings or na years (but should not exist anyway)
# warn about no dates, no CWT, ask to start over with updated output files or remove from sel treecodes for future steps
# homogenzier later removes any flagged cells
# track with flags in diagnostics table on a yearly basis



# in theory, after this, data could be uploaded
# however, we might want to add EWLW estimation based measures from Profile()
# additional Diagnostics, and manual YTE


# # find the max per overlap group (without missing, incomplete), flag only the others
# overlap_old <- function(df_rings_log){
#   df_rings_log <- df_rings_log %>%
#     dplyr::group_by(woodpiece_label, year) %>%
#     dplyr::mutate(overlap = dplyr::n() > 1) %>%
#     dplyr::ungroup()
#
#   df_overlap <- df_rings_log %>%
#     dplyr::filter(overlap) %>%
#     dplyr::arrange(woodpiece_label, year, desc(cno))
#
#   # find the duplicate rings with the max number of cells (while excluding
#   # incomplete or missing rings from possible candidates)
#   # TODO: if there are no max candidates (e.g all incomplete), then all are
#   # flagged as duplicates atm - is this the desired behavior?
#   df_overlap_max <- df_overlap %>%
#     dplyr::filter(!incomplete_ring, !missing_ring)  %>%
#     dplyr::group_by(woodpiece_label, year) %>%
#     dplyr::slice(1) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(duplicate_max_ncell = TRUE) %>%
#     dplyr::select(image_label, year, duplicate_max_ncell)
#
#   # the other duplicate rings are flagged as duplicates
#   df_overlap <- df_overlap %>%
#     dplyr::left_join(df_overlap_max, by = c('image_label', 'year')) %>%
#     dplyr::mutate(duplicate_ring = ifelse(is.na(duplicate_max_ncell), TRUE, FALSE)) %>%
#     dplyr::select(image_label, year, duplicate_max_ncell, duplicate_ring)
#
#   df_rings_log <- df_rings_log %>%
#     dplyr::left_join(df_overlap, by = c("image_label", "year")) %>%
#     dplyr::mutate(duplicate_ring = ifelse(is.na(duplicate_ring), FALSE, duplicate_ring))
#
#   return(df_rings_log)
# }


#-------------------------------------------------------------------------------
# # check that YEAR is consecutive sequence within each image
# # TODO: these checks are probably be overkill, since these issues should not arise
# # in ROXAS normally. BUT keep for now and see if we find any in existing datasets
# # TODO: should we arrange by year here? and also generally in QWA_data?
# additional_year_check <- function(df_rings_log){
#   df_rings_log <- df_rings_log %>%
#     dplyr::arrange(image_label, year) %>%
#     dplyr::group_by(image_label) %>%
#     dplyr::mutate(year_diff = year - dplyr::lag(year),
#                   year_diff = tidyr::replace_na(year_diff, 1)) %>%
#     dplyr::ungroup()
#
#   # any duplicated years in the rings files?
#   if (any(df_rings_log$year_diff == 0)) {
#     beepr::beep(sound = 2, expr = NULL)
#     stop('The following images have duplicate years:\n',
#          paste0(unique(df_rings_log[df_rings_log$year_diff == 0, 'image_label']), collapse=', '),
#          '\nPlease ensure that all included samples are properly dated, then restart the process.')
#   }
#
#   # any gaps in the dating?
#   if (any(df_rings_log$year_diff > 1)) {
#     # fill in missing years
#     df_rings_log <- df_rings_log %>%
#       dplyr::group_by(image_label) %>%
#       tidyr::complete(year = tidyr::full_seq(year, 1),
#                       fill = list(cno = 0, missing_ring = TRUE, incomplete_ring = FALSE), explicit = FALSE) %>%
#       tidyr::fill(tree_label, woodpiece_label, slide_label, .direction = 'downup') |>
#       dplyr::ungroup()
#     beepr::beep(sound = 2, expr = NULL)
#     warning('The following images have gaps in the dating, missing years were added:\n',
#             paste0(unique(df_rings_log[df_rings_log$year_diff > 1, 'image_label']), collapse=', '))
#   }
#
#   df_rings_log <- df_rings_log %>% dplyr::select(-year_diff)
#
#   return(df_rings_log)
# }


