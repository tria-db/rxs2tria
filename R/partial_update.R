update_rxsmeta <- function(df_rxsmeta, imgs_to_update, settings_date_orders,
                           roxas_version = 'classic'){
  checkmate::assert_subset(imgs_to_update, df_rxsmeta$image_label)

  files_to_update <- df_rxsmeta |>
    dplyr::filter(image_label %in% imgs_to_update)

  # read updated settings files (structure, image info and fnames should not be changed)
  df_settings_new <- collect_settings_data(files_to_update$fname_settings,
                                           roxas_version = roxas_version)

  # convert created at dates to POSIXct
  df_settings_new$rxs_created_at <- convert_settings_dates(
    df_settings_new$rxs_created_at,
    orders = settings_date_orders, # cf. lubridate::parse_date_time
    tz = Sys.timezone())

  # UPDATE THE df_rxsmeta DATAFRAME WITH THE NEW SETTINGS INFO
  df_updated <- df_rxsmeta |>
    dplyr::rows_update(df_settings_new, by = "fname_settings")

  df_updated
}

#' Function to update QWA data with new raw data for selected images
#' @export
update_QWAdata <- function(QWA_data, imgs_to_update, df_rxsmeta){

  files_to_update <- df_rxsmeta |>
    dplyr::filter(image_label %in% imgs_to_update)

  # read and preprocess raw cells/rings data for the updated images
  QWA_data_new <- collect_raw_data(files_to_update)
  QWA_data_new <- remove_outliers(QWA_data_new)
  QWA_data_new <- complete_cell_measures(QWA_data_new)

  # for the rings data, we still need to recalculate the automatic flags
  df_rings_log <- complete_rings_log(QWA_data_new)

  # check that the data have cwt estimates
  all_imgs_have_cwt <- check_cwt_estimates(df_rings_log)
  # can remove mean_cwttan now
  df_rings_log <- df_rings_log %>% dplyr::select(-mean_cwttan)

  # check that the data are dated
  all_imgs_dated <- check_dating(df_rings_log, df_rxsmeta)

  # flag incomplete rings
  df_rings_log <- flag_incomplete_rings(df_rings_log, QWA_data_new$cells, df_rxsmeta)

  # flag missing rings
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(missing_ring = is.na(cno) | (cno < 5) | dplyr::coalesce(mrw < 10, FALSE)) # TODO: (should never have NA cno anymore because we replace with 0), but mrw might be NA for incomplete rings -> coalesce. make thresholds function params?
                  #no_MRW_other = is.na(mrw) & !(outermost_ring | innermost_ring)) # TODO: check if this ever occurs and for what reason

  # add automatic exclude_year flags: default behavior is to exclude any incomplete or missing years from analysis
  df_rings_log <- df_rings_log %>%
    dplyr::mutate(exclude_issues = incomplete_ring | missing_ring)
  # remove any of the temp issue columns (TODO: clean up)
  df_rings_log <- df_rings_log |>
    dplyr::select(-dplyr::any_of(c(
      'innermost_ring','outermost_ring',
      'mae','medYleft','medYright','mindist',
      'incomplete_inner', 'incomplete_innerv2', 'incomplete_fct_check',
      'no_MRW_other')))

  # flag duplicate rings: HERE WE NEED TO CONSIDER OTHER IMAGES FROM SAME WOODPIECE
  # default behavior is to select the year with the highest cell count (that isn't incomplete or missing)
  affected_wps <- unique(files_to_update$woodpiece_label)
  df_rings_log <- QWA_data$rings |> # add old data for affected wps
    dplyr::filter(woodpiece_label %in% affected_wps) |>
    dplyr::filter(!image_label %in% imgs_to_update) |> # filter out old data for updated images
    dplyr::bind_rows(df_rings_log) # add new data

  df_rings_log <- flag_duplicate_rings(df_rings_log) # this affects only the duplicate_ring and exclude_dupl cols

  rings_updated <- QWA_data$rings |>
    dplyr::filter(!woodpiece_label %in% affected_wps) |> # filter out wps to update
    dplyr::bind_rows(df_rings_log) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~tidyr::replace_na(., FALSE))) |>
    dplyr::arrange(woodpiece_label, slide_label, image_label, year)

  cells_updated <- QWA_data$cells |>
    dplyr::filter(!image_label %in% imgs_to_update) |> # filter out old data
    dplyr::bind_rows(QWA_data_new$cells) |>
    dplyr::arrange(image_label, year)

  return(
    stats::setNames(
      list(cells_updated, rings_updated),
      c('cells','rings'))
  )
}
