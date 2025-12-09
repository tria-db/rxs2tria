################################################################################
# installation
library(devtools)
load_all()

#remotes::install_github('tria-db/rxs2tria')
#library(rxs2tria)

################################################################################
# set path to the input and output data
# where the ROXAS files are (can contain subfolders)
path_in <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs_out'
path_in <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs_out'
path_in <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs_out'

# where output files should be saved to
path_out <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs2tria_out'
path_out <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out'
path_out <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs2tria_out'

dataset_name <- 'POGSTO2024' # used to name the resulting output files
dataset_name <- 'LTAL_S22'
dataset_name <- 'YAM_1880'


################################################################################
# get overview of data to be read and extract data structure from filenames
files <- get_roxas_files(path_in)

# example: `{site}_{species}_{tree}{woodpiece}_{slide}_{image}` (with 2digit tree identifier and optional woodpiece)
#pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

# example: `{site}_{species}_{tree}_{slide}_{image}`, e.g. S22_LADE_L20_9_3
pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:]+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

# example2: `{site}_{species}_{tree}_{slide}_{image}`
#pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:].+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

df_structure <- extract_data_structure(files, pattern)
# TODO: visualize as data.tree?

################################################################################
# read available metadata
df_images <- collect_image_info(df_structure$fname_image)
df_settings <- collect_settings_data(files$fname_settings,#df_structure$fname_settings,
                                     roxas_version ='classic')
# glimpse the created at dates to check format
df_settings |>
  dplyr::select(created_at, fname_settings) |>
  dplyr::slice_sample(n=10)
# convert created at dates to POSIXct
df_settings$created_at <- convert_settings_dates(
  df_settings$created_at,
  orders = c("%d/%m/%Y %H:%M:%S","%d/%m/%Y %H:%M"), # cf. lubridate::parse_date_time
  tz = Sys.timezone())

df_rxsmeta <- combine_rxs_metadata(df_structure, df_images, df_settings)

rm(df_images, df_settings)
################################################################################
# complete the required metadata form via the Shiny app
launch_metadata_app()
# save the json metadata file from the app output once completed

################################################################################
# read raw cells/rings data
QWA_data <- collect_raw_data(df_structure)


################################################################################
# remove outliers
# NOTE: ROXAS does some threshold based outlier checks and assigns these a
# a negative value. Here, we replace these outliers with NAs)
QWA_data <- remove_outliers(QWA_data)


################################################################################
# complete measures (add EW/LW estimations)
QWA_data <- complete_cell_measures(QWA_data)


################################################################################
# clean raw data
QWA_data <- validate_QWA_data(QWA_data, df_rxsmeta)
# this creates some initial flags based on the data only, namely missing, incomplete and duplicate flags


################################################################################
# save preprocessed data to files
fname_out <- file.path(
  path_out,
  glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_{dataset_name}")
)
readr::write_csv(
  QWA_data$cells,
  paste0(fname_out, '_cells.csv.gz'))

readr::write_csv(QWA_data$rings,
                 paste0(fname_out, '_rings.csv'))


# TODO: missing also in case of mrw < 10 and/or cno < 5?
# TODO: should we calculate additional measures in case of incomplete rings?
# might still want to decide later on some issues?
# but what makes sense to calculate?


################################################################################
# provide user input on ring flags
# interactively in shiny app (NOT READY YET)
# TODO: adapt to new flag logic (need to calculate duplicate_rank in app)
# launch_coverage_app()

n_sectors <- 5
# divide cells into n_sectors based on position in ring (rraddsitr)
# calculate profiles for each sector and year
# i.e. aggregate over all cells per year/sector for selected parameters (this might take a while)
sel_cell_params <- c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan",
                     "cwa", "tca", "cdrad", "cdtan", "cdratio")

prf_data <- QWA_data$cells |>
  #dplyr::inner_join(complete_rings, by = c("image_label", "year")) |> # only use cells from valid rings
  dplyr::mutate(
  sector_n = as.numeric(cut(rraddistr, # no grouping needed
                            b = seq(from=0, to=100, by = 100/n_sectors),
                            labels = 1:n_sectors,
                            include.lowest = T))) %>%
  # round for data with rraddistr just above 100, otherwise leave NA
  dplyr::mutate(sector_n = dplyr::if_else(rraddistr > 100 & rraddistr <= 101, n_sectors, sector_n)) |>
  dplyr::filter(!is.na(sector_n)) |> # only use cells with valid sector as basis for calculation
  dplyr::select(image_label, year, sector_n, dplyr::all_of(sel_cell_params))


prf_data_quant <- prf_data |>
  collapse::fgroup_by(image_label, year, sector_n) |>
  collapse::BY(collapse::.quantile,
               probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
               expand.wide = TRUE) |>
  dplyr::rename_with(
    \(x) stringr::str_replace_all(x,
                                  c(".V1" = "_q10", ".V2" = "_q25", ".V3" = "_median",
                                    ".V4" = "_q75", ".V5" = "_q90"))
  )

prf_data_mean <- prf_data |>
  collapse::fgroup_by(image_label, year, sector_n) |>
  collapse::fmean() |>
  dplyr::rename_with(
    \(x) paste0(x, "_mean"),
    dplyr::all_of(sel_cell_params)
  )

prf_data <- prf_data_mean |>
  dplyr::full_join(prf_data_quant,
                   by = c("image_label", "year", "sector_n"))

rm(prf_data_mean, prf_data_quant)
# sel_ring_params <- c('mrw','cno','eww','lww')
# prf_data <- QWA_data$rings |>
#   dplyr::select(image_label, year, dplyr::all_of(sel_ring_params)) |>
#   dplyr::mutate(sector_n = NA) |>
#   dplyr::full_join(
#     prf_data,
#     by = c('image_label', 'year', 'sector_n')
#   )
#
# # add woodpiece and slide labels
# prf_data <- QWA_data$rings |>
#   dplyr::distinct(woodpiece_label, slide_label, image_label) |>
#   dplyr::right_join(
#     prf_data,
#     by = c('image_label')
#   )

readr::write_csv(prf_data,
                 paste0(fname_out, '_profiles.csv'))


# df_rings <- QWA_data$rings |>
#   dplyr::mutate(include_ring = dplyr::case_when(exclude_dupl | exclude_issues ~ 2,
#                                                 TRUE ~ 1)) |>
#   dplyr::select(-exclude_dupl, -exclude_issues)




launch_flags_app()


################################################################################
# READ QWA data
file_path <- "../example_data/rxs2tria_out"
QWA_data <- read_QWAdata(file_path)

file_path <- "../example_data/tria_download"
QWA_metadata <- read_QWA_metadata(file_path)
