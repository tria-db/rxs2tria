## rxs2tria: Cheatsheet
pak::pak("tria-db/rxs2tria")

# or with another remote package installer, such as:
# devtools::install_github("tria-db/rxs2tria")
# remotes::install_github("tria-db/rxs2tria")

library(rxs2tria)

# TODO: adjust path
path_in <- "/Users/maranaegelin/Documents/QWAdata/example_dataset/roxas_out"
files <- get_roxas_files(path_in, roxas_version = "roxas")

# TODO: adjust pattern according to your labelling system (ask AI for help if needed)
pattern <- "(?<site>[:alnum:]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"
df_structure <- extract_data_structure(files, pattern)
df_settings <- collect_settings_data(df_structure)
# TODO: adjust date format (orders) if needed (optional)
df_settings$meas_created_at <- lubridate::parse_date_time(
  df_settings$meas_created_at,
  orders = "%m/%d/%Y %H:%M"
)
rxs_images <- build_QWAimages(df_structure, df_settings)
QWA_data <- collect_raw_data(rxs_images)
# TODO: use "either" instead of "incomplete_only" if missing rings should be excluded (set to NA)
QWA_data <- complete_QWAdata(QWA_data, rxs_images, "incomplete_only") 
# TODO: adjust the n sectors / parameters / quantiles you want
prf_sector <- calculate_sector_profiles(QWA_data, 5, c("la", "cwtrad"), quant_probs = c(0.1,0.5,0.9))

# TODO: set to your output folder, choose parameters, scaling options, adjust filenames
path_out <- "/Users/maranaegelin/Documents/QWAdata/example_dataset/test_out/"
mrw_rwl <- extract_rwl(QWA_data$rings, "mrw") |> 
  scale_for_tucson(prec = 0.001, scaling = 0.001) |> 
  rename_for_tucson(rxs_images, long.names = TRUE)
f <- dplR::write.tucson(
  mrw_rwl, paste0(path_out,"mrw.rwl"), prec = 0.001, long.names = TRUE
)

la_mean_rwl <- extract_rwl(QWA_data$rings, "la_mean", prf_sector, 5) |> 
  scale_for_tucson(prec = 0.001) |> 
  rename_for_tucson(rxs_images, long.names = TRUE)
f <- dplR::write.tucson(
  la_mean_rwl, paste0(path_out,"la_mean.rwl"), prec = 0.001, long.names = TRUE
)

# NOTE: to save / reload the images metadata and QWA measurements data
write_QWAimages(rxs_images, paste0(path_out,"rxs_images.csv"))
rxs_images <- read_QWAimages(paste0(path_out,"rxs_images.csv"))

write_QWAdata(QWA_data, dir = path_out)
read_QWAdata(dir = path_out)

# NOTE: to start the shiny apps
launch_flags_app()
launch_metadata_app()