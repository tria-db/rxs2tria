# remotes::install_github('tria-db/rxs2tria')
# library(rxs2tria)
devtools::load_all()

path_in <- '../../../QWAdata/LTAL_S22/rxs_out'

# 1 ----------------------------------------------------------------------------
# - extract data structure
files <- get_roxas_files(path_in)
df_structure <- extract_data_structure(files)
# - read available metadata
df_images <- collect_image_info(df_structure$fname_image)
df_settings <- collect_settings_data(files$fname_settings)
df_rxsmeta <- combine_rxs_metadata(df_structure, df_images, df_settings)
rm(df_images, df_settings, df_structure, files)
# - merge and clean raw data
QWA_data <- collect_raw_data(df_rxsmeta)
QWA_data <- remove_outliers(QWA_data)
QWA_data <- complete_cell_measures(QWA_data)
QWA_data <- validate_QWA_data(QWA_data, df_rxsmeta)

# 2 ----------------------------------------------------------------------------
# launch_metadata_app()

# 3 ----------------------------------------------------------------------------
# calculate statistical aggregates of the cell data for n sectors per ring
prf_data <- calculate_sector_profiles(
  QWA_data$cells,
  n_sectors = 5,
  sel_cell_params = c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan",
                      "cwa", "tca", "cdrad", "cdtan", "cdratio"),
  quant_probs = c(0.1, 0.5, 0.9)
)
launch_flags_app()

# prf_bands <- calculate_band_profiles(
#   QWA_data,
#   bandwidth = 30,
#   stepsize = 10,
#   sel_cell_params,
#   quant_probs,
#   band_rebound = TRUE)


