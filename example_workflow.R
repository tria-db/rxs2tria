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
quant_probs <- c(0.1, 0.5, 0.9)

prf_data <- calculate_profiles(
  QWA_data$cells, n_sectors, sel_cell_params, quant_probs
)


sel_prm <- "la_mean"
sel_sect <- 5

df_crn <- QWA_data$rings |>
  dplyr::select(woodpiece_label, slide_label, image_label, year,
                exclude_issues, exclude_dupl,
                dplyr::any_of(sel_prm))
if (sel_prm %in% names(prf_data)){
  df_crn  <- prf_data |>
    dplyr::filter(sector_n == as.numeric(sel_sect)) |>
    dplyr::select(dplyr::all_of(c("image_label","year", sel_prm))) |>
    dplyr::right_join(df_crn, by = c("image_label","year"))
}

# make sure we only have one value per year/woodpiece
df_crn <- df_crn |> dplyr::filter(!exclude_dupl)





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
