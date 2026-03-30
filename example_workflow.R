################################################################################
# installation
library(devtools)
load_all()

#remotes::install_github('tria-db/rxs2tria')
#library(rxs2tria)

################################################################################
# set path to the input and output data,
# get overview of data to be read and extract data structure from filenames

# example 1:
path_in <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs_out'
# `{site}_{species}_{tree}{woodpiece}_{slide}_{image}` (with 2digit tree identifier and optional woodpiece)
pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

# example 2:
path_in <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs_out'
# `{site}_{species}_{tree}_{slide}_{image}`, e.g. S22_LADE_L20_9_3
pattern <- "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:]+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"

# example 3:
path_in <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs_out'
# `{site}_{species}_{tree}_{slide}_{image}`, e.g. YAM_LASI_T3.13_01_1
pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:].]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"

# path_in <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_LOTanatomy_2021_PF/S22/ROXAS/4_Roxas_final/S22_LADE_L01'
# path_in <- "/Volumes/Dendro/Dendrosciences_All/PatrickFonti_ALTAY_2020_PF/Box01-04/Box01/ROXAS/Edited/Box01/"
# path_in <- "/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAMAL CLEANED/New Yamal AI 1880"
# pattern <- "ALT_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"

# where output files should be saved to
# path_out <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs2tria_out'

# dataset_name <- 'POGSTO2024' # used to name the resulting output files
# dataset_name <- 'YAM_1880'
# dataset_name <- 'LTAL_S22_L01'

################################################################################
files <- get_roxas_files(path_in, roxas_version = "roxas", exclude_dirs = c("test", "unused"))
df_structure <- extract_data_structure(files, pattern) #, site_label = "ALTAY", species_code = "LASI")


################################################################################
# read available metadata
# ROXAS (image EXIF metadata is collected from files_images):
df_settings <- collect_settings_data(files_settings = df_structure$fname_settings,
                                     files_images = df_structure$fname_image,
                                     roxas_version = "roxas")

# ROXAS AI (image EXIF metadata is already part of the JSON files_settings):
# df_settings <- collect_settings_data(files_settings = df_structure$fname_settings,
#                                      roxas_version = "roxas_ai")

# To avoid conversion errors, datetime columns are originally read as pure
# character strings, and you need to explicitly convert them to POSIXct with
# the appropriate format(s) and timezone.
df_settings$img_created_at <- lubridate::parse_date_time(
  df_settings$img_created_at,
  orders = "%Y:%m:%d %H:%M:%S", # common EXIF format
  tz = "UTC" # commonly used in EXIF tags
)
df_settings$rxs_created_at <- lubridate::parse_date_time(
  df_settings$rxs_created_at,
  orders = c("%d.%m.%Y %H:%M:%S", "%m/%d/%Y %H:%M"), #"%d/%m/%Y %H:%M"
  tz = Sys.timezone()
)

rxs_images <- build_QWAimages(df_structure, df_settings)
rm(df_structure, df_settings)

# To save the QWAimages object to a file for later, use:
write_QWAimages(rxs_images, file.path(path_out, paste0(dataset_name, "_QWAimages.csv")))

# and read it in again
# rxs_images <- read_QWAimages("path/to/output_data/QWAimages.csv")


################################################################################
# complete the required metadata form via the Shiny app
# launch_metadata_app()
# save the json metadata file from the app output once completed


################################################################################
# read raw cells/rings data
QWA_data <- collect_raw_data(rxs_images)


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
QWA_data <- validate_QWA_data(QWA_data, rxs_images, exclude_mode = "either")
# TODO: missing rings with 0 cells -> set mrw, cwt, la etc to 0
# this creates some initial flags based on the data only, namely missing, incomplete and duplicate flags


# save preprocessed data to files
write_QWAdata(QWA_data, dir = path_out, dataset_name = dataset_name)



################################################################################
# provide user input on ring flags
# first, calculate profiles to explore cell measurements data
prf_sector <- calculate_sector_profiles(QWA_data,
                                        n_sectors = 5,
                                        sel_cell_params = c("la", "cwttan"), 
                                        quant_probs = c(0.1, 0.5, 0.9))

# band_profiles <- calculate_band_profiles(QWA_data,
#                                          bandwidth = 50, stepsize = 25,
#                                          sel_cell_params = c("la", "cwttan"))

write_QWAprofile(prf_sector,
                 file.path(path_out, paste0(dataset_name, "_profiles_sector5.csv")))
# TODO: handling missing rings: currently, if ring has few cells, they seem to fall in sector 5
# what if there are no cells? what about the other sectors?


# launch the shiny app to explore data and flag rings
launch_flags_app()


################################################################################
# read in previously saved data for testing
prf_sector <- read_QWAprofile(file.path(path_out, paste0(dataset_name, "_profiles_sector5.csv")))

QWA_data <- read_QWAdata(dir = path_out, dataset_name = dataset_name)









# work in progres.....
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



