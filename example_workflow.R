################################################################################
# installation
library(devtools)
load_all()

#remotes::install_github('tria-db/rxs2tria')
#library(rxs2tria)

################################################################################
# set path to the input and output data
# where the ROXAS files are (can contain subfolders)
# path_in <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs_out'
# path_in <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs_out'
# path_in <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs_out'
# path_in <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_LOTanatomy_2021_PF/S22/ROXAS/4_Roxas_final/S22_LADE_L01'
path_in <- '~/Desktop/Ltal_S22/4_Roxas_final'
path_in <- '~/Desktop/Ltal_S22/4_Roxas_final_AI'
path_in <- '~/Desktop/New Yamal 1880'
path_in <- '~/Desktop/New Yamal AI 1880'


# where output files should be saved to
# path_out <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs2tria_out'
path_out <- '~/Desktop/LtalS22_out'
path_out <- '~/Desktop/LtalS22_out_AI'
path_out <- '~/Desktop/New Yamal AI 1880_out'

# dataset_name <- 'POGSTO2024' # used to name the resulting output files
# dataset_name <- 'YAM_1880'
# dataset_name <- 'LTAL_S22_L01'
dataset_name <- 'LTAL_S22'
dataset_name <- 'LTAL_S22_AI'
dataset_name <- 'YAM_1880'
dataset_name <- 'YAM_AI_1880'


################################################################################
# get overview of data to be read and extract data structure from filenames
files <- get_roxas_files(path_in)

# example: `{site}_{species}_{tree}{woodpiece}_{slide}_{image}` (with 2digit tree identifier and optional woodpiece)
#pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

# example: `{site}_{species}_{tree}_{slide}_{image}`, e.g. S22_LADE_L20_9_3
# pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:]+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"
pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:].]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"


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
# launch_metadata_app()
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
# TODO: missing rings with 0 cells -> set mrw, cwt, la etc to 0
# this creates some initial flags based on the data only, namely missing, incomplete and duplicate flags

# add automatic exclude_year flags: default behavior is to exclude any incomplete or missing years from analysis
QWA_data$rings <- QWA_data$rings %>%
  dplyr::mutate(exclude_issues = incomplete_ring)
  #dplyr::mutate(exclude_issues = incomplete_ring | missing_ring)

################################################################################
# save preprocessed data to files
fname_out <- file.path(
  path_out,
  glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_{dataset_name}")
)

save_files <- TRUE
if (save_files) {
  readr::write_csv(
    QWA_data$cells,
    paste0(fname_out, '_cells.csv.gz'))

  readr::write_csv(QWA_data$rings,
                   paste0(fname_out, '_rings.csv'))
}


################################################################################
# provide user input on ring flags
# first, calculate profiles to explore cell measurements data

n_sectors <- 5
# divide cells into n_sectors based on position in ring (rraddsitr)
# calculate profiles for each sector and year
# i.e. aggregate over all cells per year/sector for selected parameters (this might take a while)
sel_cell_params <- c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan",
                     "cwa", "tca", "cdrad", "cdtan", "cdratio")
quant_probs <- c(0.1, 0.5, 0.9)

prf_data <- calculate_profiles(
  QWA_data$cells, n_sectors, sel_cell_params, quant_probs
)

prf_data2 <- calculate_sector_profiles(
  QWA_data, n_sectors, sel_cell_params, quant_probs
)
# TODO: handling missing rings: currently, if ring has few cells, they seem to fall in sector 5
# what if there are no cells? what about the other sectors?

if (save_files){
  readr::write_csv(prf_data,
                   paste0(fname_out, '_profiles.csv'))
  readr::write_csv(df_rxsmeta,
                   paste0(fname_out, '_df_rxsmeta.csv'))
}


################################################################################
# read in previously saved data for testing
prf_data <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251208_TRIA_LTAL_S22_profiles.csv",
                         col_types = c(image_label = "c", year = "i", sector_n = "i"))
QWA_data <- list()
QWA_data$rings <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251208_TRIA_LTAL_S22_rings.csv")
QWA_data$cells <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251208_TRIA_LTAL_S22_cells.csv.gz")
df_rxsmeta <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251125_TRIA_LTAL_S22_rxsmeta.csv")

# launch the shiny app to explore data and flag rings
launch_flags_app()



################################################################################
# moving band profiles with fixed bandwidth and stepsize
bandwidth <- 30
stepsize <- 10
sel_cell_params <- c("la", "cwttan", "cwtrad") #, "cwtall", "drad", "dtan",
                     #"cwa", "tca", "cdrad", "cdtan", "cdratio")
quant_probs <- c(0.1,  0.5,  0.9)
band_rebound <- TRUE

prf_bands <- calculate_band_profiles(
  QWA_data,
  bandwidth,
  stepsize,
  sel_cell_params,
  quant_probs,
  band_rebound
)
# TODO: if there are NO cells within a band, then that band is missing from the output df
# maybe add tidyr::complete step to have all bands present for all rings (with NA vals and 0 n cells)


## PLOTTING PROFILE
prf_wp <- QWA_data$rings |>
  # TODO: handling issues / duplicates before?
  dplyr::filter(!exclude_issues, !exclude_dupl) |>
  dplyr::select(woodpiece_label, slide_label, image_label, year) |>
  dplyr::filter(woodpiece_label == "S22_LADE_L01") |>  # select a core
  dplyr::left_join(prf_bands, by = c("image_label", "year"))

df_plot <- prf_wp %>%
  dplyr::select(woodpiece_label, slide_label, image_label, year, mrw, ew_band, start, end, cwttan_N, cwttan_mean) |> # CHOOSE PARAMETER
  dplyr::group_by(image_label, year) |>
  dplyr::mutate(band_pos = year + start/(max(start)+stepsize)) # relative band position within year (TODO: as date?)

df_plot |>
  ggplot2::ggplot(ggplot2::aes(x = band_pos, y = cwttan_mean)) +
  ggplot2::geom_line(col="darkgrey")
# TODO:
# - highlight gaps of missing rings / bands
# - add vertical lines to differentiate slides/images
# - add horizontal avg lines per slide/image to indicate angle issues
# - add smoothed / rolling average lines for ew and lw part in different colors
# - add points for outliers, for values with few cells, duplicate rings, excluded rings?
# cf saxor plt_info and prf_make_plt

################################################################################
# if one or more images have been rescanned and reprocessed in ROXAS,
# read in the new data and update the existing data accordingly

# ENSURE THAT THE NEW FILES ARE REPLACED IN THE INPUT FOLDER
imgs_to_update <- c("S22_LADE_L09_2_1", "S22_LADE_L01_1_3") # example
# check they are valid

settings_date_orders <- c("%d/%m/%Y %H:%M:%S","%d/%m/%Y %H:%M")
df_rxsmeta <- update_rxsmeta(df_rxsmeta, imgs_to_update, settings_date_orders)
# TODO: what if the user already ran the metadata app? allow to merge into json file?

QWA_data_updated <- update_QWAdata(QWA_data, imgs_to_update, df_rxsmeta)


################################################################################
# READ QWA data
# TODO: add the reading QWA data and metadat functionalities
# file_path <- "../example_data/rxs2tria_out"
# QWA_data <- read_QWAdata(file_path)
#
# file_path <- "../example_data/tria_download"
# QWA_metadata <- read_QWA_metadata(file_path)


################################################################################
# WRITE RWL
# Create an RWL file from mean ring width
create_rwl(prf_data = prf_data,
           df_rings = QWA_data$rings,
           PAR = "mrw",
           path_out = path_out,
           remove_excluded = TRUE)

# Create an RWL file from a profile-level parameter
create_rwl(prf_data = prf_data,
           PAR = "cwtrad_mean",
           df_rings = read.csv(paste0(path_out, "/", "20251230_TRIA_YAM_AI_1880_rings_edited.csv")),
           SECTOR = 5,
           path_out = path_out,
           remove_excluded = TRUE)

# 20260103_TRIA_S22_AI_rings_edited.csv
# 20251230_TRIA_YAM_AI_1880_rings_edited.csv
