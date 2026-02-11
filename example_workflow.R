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
path_in <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAMAL CLEANED/New Yamal AI 1880'
path_in <- '~/Desktop/New Ymal AI 1880_2'
path_in <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_QWAMENA_2024/TAZ/ROXAS/3_Roxas_dated AI'

# where output files should be saved to
# path_out <- '/Users/maranaegelin/Documents/QWAdata/QWA_Arzac2024/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out'
# path_out <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880/rxs2tria_out'
path_out <- '~/Desktop/LtalS22_out'
path_out <- '~/Desktop/LtalS22_out_AI'
path_out <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAMAL CLEANED/New Yamal AI 1880_out'
path_out <- '~/Desktop/New Yamal AI 1880_out_2'
path_out <- '~/Desktop/New TAZ_out'

# dataset_name <- 'POGSTO2024' # used to name the resulting output files
# dataset_name <- 'YAM_1880'
# dataset_name <- 'LTAL_S22_L01'
dataset_name <- 'LTAL_S22'
dataset_name <- 'LTAL_S22_AI'
dataset_name <- 'YAM_1880'
dataset_name <- 'YAM_AI_1880'
dataset_name <- 'YAM_AI_1880_2'
dataset_name <- 'TAZ_AI'


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
# this creates some initial flags based on the data only, namely missing, incomplete and duplicate flags


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
                     "cwa")
quant_probs <- c(0.1, 0.5, 0.75, 0.9)

prf_data <- calculate_sector_profiles(
  QWA_data$cells, n_sectors, sel_cell_params, quant_probs
) %>%
  dplyr::mutate(tree_id = stringr::str_c(stringr::word(image_label, 1, 3, sep = "_"), collapse = NULL))

if (save_files){
  readr::write_csv(prf_data,
                   paste0(fname_out, '_profiles.csv'))
  readr::write_csv(df_rxsmeta,
                   paste0(fname_out, '_df_rxsmeta.csv'))
}

# read in previously saved data for testing
# prf_data <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251208_TRIA_LTAL_S22_profiles.csv")
# QWA_data <- list()
# QWA_data$rings <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251208_TRIA_LTAL_S22_rings.csv")
# df_rxsmeta <- vroom::vroom("/Users/maranaegelin/Documents/QWAdata/LTAL_S22/rxs2tria_out/20251125_TRIA_LTAL_S22_rxsmeta.csv")

# launch the shiny app to explore data and flag rings
launch_flags_app()




################################################################################
# READ QWA data
# TODO: add the reading QWA data and metadata functionalities
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
library(dplyr)
library(stringr)
library(tidyr)
create_rwl(prf_data = prf_data, # %>%  mutate(cwt_mean = la_mean/100), # to ensure the parameter exists in prf_data),
           PAR = "cwtrad_mean",
           df_rings = read.csv(paste0(path_out, "/", "20260203_TRIA_YAM_AI_1880_rings_edited.csv")),
           SECTOR = 5,
           path_out = path_out,
           remove_excluded = TRUE)

# 20260103_TRIA_S22_AI_rings_edited.csv
# 20251230_TRIA_YAM_AI_1880_rings_edited.csv
# 20260127_TRIA_DATASETNAME_rings_edited.csv
# 20260112_TRIA_YAM_AI_1880_rings_edited.csv


################################################################################ new steps after cleaning!
################################################################################
################################################################################

# if starting from here, upload ...
# installation
library(devtools)
load_all()

path_in <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAMAL CLEANED/New Yamal AI 1880'
path_out <- '/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAMAL CLEANED/New Yamal AI 1880_out'
dataset_name <- 'YAM_AI_1880'
QWA_data <- list()
QWA_data$cells <- vroom::vroom(paste(path_out, "20260209_TRIA_YAM_AI_1880_cells.csv.gz", sep = "/"))
QWA_data$rings <- vroom::vroom(paste(path_out, "20260209_TRIA_YAM_AI_1880_rings.csv", sep = "/"))
save_files <- TRUE
fname_out <- file.path(
  path_out,
  glue::glue("{format(Sys.Date(), '%Y%m%d')}_TRIA_{dataset_name}")
)


################################################################################
# CALCULATE PROFILES

# SECTOR
sector_data <- calculate_sector_profiles(
  QWA_data %>% .$cells,
  n_sectors = 5,
  # sel_cell_params = c("la", "cwttan", "cwtrad"),
  # quant_probs = c(0.75)
  sel_cell_params = c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan", "cwa", "tca", "rwd2"),
  quant_probs = c(0.25, 0.75)
) %>%
  dplyr::mutate(tree_id = stringr::str_c(stringr::word(image_label, 1, 3, sep = "_"), collapse = NULL)) %>%
  dplyr::arrange(image_label, year, sector_n) %>%
  dplyr::left_join(QWA_data %>% .$rings %>% dplyr::select(image_label, year, mrw), by = c("image_label", "year"))

if (save_files){
  readr::write_csv(sector_data,
                   paste0(fname_out, '_sector_data.csv'))
}

# BAND only from 1850 onwards due to memory issue
band_data <- calculate_band_profiles(lapply(QWA_data, function(df) df %>% dplyr::filter(year >= 1850)),
                        bandwidth = 30,
                        stepsize = 10,
                        # sel_cell_params = c("la", "cwttan", "cwtrad"),
                        # quant_probs = c(0.75)
                        sel_cell_params = c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan", "cwa", "tca", "rwd2"),
                        quant_probs = c(0.25, 0.75),
                        band_rebound = TRUE) %>%
  dplyr::mutate(tree_id = stringr::str_c(stringr::word(image_label, 1, 3, sep = "_"), collapse = NULL)) %>%
  dplyr::arrange(image_label, year, start)

if (save_files){
  readr::write_csv(band_data,
                   paste0(fname_out, '_band_data.csv'))
}

################################################################################
# FILTERING UNWANTED RINGS (double rings, etc.) BASED ON FLAGS IN QWA_data$rings

# SECTOR
filter_rings_to_exclude <- function(sector_data,
                                    rings_data,
                                    flags = c("exclude_dupl", "exclude_issues", "exclude_issues_ring_edited")) {

  # Check that all flags exist in rings_data
  missing_flags <- setdiff(flags, names(rings_data))
  if(length(missing_flags) > 0) {
    stop("The following flags are missing in rings_data: ", paste(missing_flags, collapse = ", "))
  }

  # Vectorized OR across all flag columns
  to_exclude <- Reduce(`|`, rings_data[flags])

  rings_to_exclude <- rings_data[to_exclude, ] %>%
    dplyr::select(image_label, year) %>%
    dplyr::distinct()

  # Remove these rings from sector_data
  filtered_sector_data <- sector_data %>%
    dplyr::anti_join(rings_to_exclude, by = c("image_label", "year"))

  return(filtered_sector_data)
}

# Add your new edited flag first
rings_data <- read.csv("~/Desktop/New Yamal AI 1880_out/20260209_TRIA_YAM_AI_1880_rings_edited.csv") %>% tibble::tibble() %>%
  dplyr::mutate(
    exclude_issues_ring_edited =
      (!missing_ring & incomplete_ring) |
      (x_dating & exclude_scope %in% c("ALL", "EW", "LW")) |
      (decay    & exclude_scope %in% c("ALL", "EW", "LW")) |
      (paraffin & exclude_scope %in% c("ALL", "EW", "LW"))
  )



# rings_data = read.csv(paste0(path_out, "/", "20260204_TRIA_YAM_AI_1880_rings_edited.csv")) %>% tibble::tibble(), # QWA_data$rings,


# Filter the sector data
filtered_sector_data <- filter_rings_to_exclude(
  sector_data = sector_data,
  rings_data  = rings_data,
  flags       = c("exclude_dupl", "exclude_issues", "exclude_issues_ring_edited")
)

if (save_files){
  readr::write_csv(filtered_sector_data,
                   paste0(fname_out, '_filtered_sector_data.csv'))
}

# Filter the sector data
filtered_band_data <- filter_rings_to_exclude(
  sector_data = band_data,
  rings_data  = rings_data,
  flags       = c("exclude_dupl", "exclude_issues", "exclude_issues_ring_edited")
)

if (save_files){
  readr::write_csv(filtered_band_data,
                   paste0(fname_out, '_filtered_band_data.csv'))
}




################################################################################
# CALCULATE summary chronology and correlations with max cwtrad_mean

# SECTOR
summary_sector <- summary_sector_chronologies(
  filtered_sector_data,
  SECTOR = 1:5,
  # sel_params = c("la", "cwttan", "cwtrad", "mrw"),
  # quant_probs = c(0.75),
  sel_params = c("la", "cwttan", "cwtrad", "cwtall", "drad", "dtan", "cwa", "tca", "rwd2", "mrw"),
  quant_probs = c(0.25, 0.75),
  min_year_cor = 1850,
  write_csv = FALSE
)

summary_sector$timeseries
summary_sector$chronology
summary_sector$diagnostics %>% dplyr::filter(sector_n == 5)

if (save_files){
  readr::write_csv(summary_sector$timeseries,
                   paste0(fname_out, '_sector_data_timeseries.csv'))
}


# BAND
summary_band <- summary_band_chronologies(
  band_data = filtered_band_data,
  sel_params = c("la_q75", "cwttan_q75", "cwtrad_q75", "cwtall_q75", "drad_q75", "dtan_q75", "cwa_q75", "tca_q75", "rwd2_q75", "mrw"),
  min_year_cor = 1850,
  write_csv = FALSE
)

# Access results
summary_band$summary_data   # ring + EW + LW summaries
summary_band$timeseries      # per-tree × year × param
summary_band$chronology     # Tukey biweight mean chronologies
summary_band$diagnostics    # inter-tree correlations

if (save_files){
  readr::write_csv(summary_band$timeseries,
                   paste0(fname_out, '_band_data_timeseries.csv'))
}

# plot rbt sector
library(ggplot2)
ggplot(summary_sector$diagnostics, aes(x = stat, y = Rbar, fill = param)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_grid(~ sector_n) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs(
    title = "SECTOR: Inter-tree correlations by parameter and statistic",
    x = "Statistic",
    y = "rbt = Mean correlation",
    fill = "Parameter"
  )

# plot rbt band
ggplot(summary_band$diagnostics,
       aes(x = stat, y = Rbar, fill = param)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ ew_lw, scales = "free_x") +   # optionally facet by param
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "BAND: Inter-tree correlations by parameter and statistic",
    x = "Statistic",
    y = "rbt = Mean correlation",
    fill = "Parameter"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot chronology SECTOR
ts_sector <- summary_sector$timeseries %>%
  arrange(tree_id, sector_n, param, stat, year) %>%
  group_by(tree_id, sector_n, param, stat) %>%
  mutate(
    segment = cumsum(
      year - lag(year, default = first(year)) > 1
    )
  ) %>%
  ungroup()

ggplot() +
  # per-tree timeseries (broken at gaps)
  geom_line(
    data = ts_sector %>%
      filter(
        year >= 1850,
        sector_n == 5,
        stat == "mean"
      ),
    aes(
      x = year,
      y = value,
      group = interaction(tree_id, segment)
    ),
    color = "steelblue",
    alpha = 0.2
  ) +

  # chronology (Tukey biweight mean)
  geom_line(
    data = summary_sector$chronology %>%
      filter(
        year >= 1850,
        sector_n == 5,
        stat == "mean"
      ),
    aes(x = year, y = value),
    color = "black",
    linewidth = 0.5,
    alpha = 0.9
  ) +

  facet_wrap(~ param, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Sector 5: Tree-ring timeseries and chronology (mean)",
    x = "Year",
    y = "Value"
  )


# plot chronology BAND
ts <- summary_band$timeseries %>%
  filter(ew_lw == "all", stat == "mean_value") %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  mutate(
    segment = cumsum(
      year - lag(year, default = first(year)) > 1
    )
  ) %>%
  ungroup()

ggplot() +
  geom_line(
    data = ts,
    aes(
      x = year,
      y = value,
      group = interaction(tree_id, segment)
    ),
    color = "steelblue",
    alpha = 0.2
  ) +
  geom_line(
    data = summary_band$chronology %>%
      filter(ew_lw == "all", stat == "mean_value"),
    aes(x = year, y = value),
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(~ param, scales = "free_y") +
  theme_minimal()


################################################################################
# CALCULATE DETRENDED TIMESERIES AND CHRONOLOGIES



# SECTOR
t1 <- Sys.time()
summary_detrended_sector <- summary_det_sector(
  df = summary_sector$timeseries,
  spline_years = c(10, 30, 50, 150),
  do_zscore = TRUE,
  do_ar = TRUE,
  do_diff = TRUE,
  min_year_cor = 1850
)

# Access elements
summary_detrended_sector$det.timeseries    # per-tree detrended series
summary_detrended_sector$det.chronology    # per-year Tukey biweight chronologies
summary_detrended_sector$det.diagnostics   # mean Rbar correlations, EPS and n_trees
Sys.time() - t1


# BAND
t1 <- Sys.time()
summary_detrended_band <- summary_det_band(
  df = summary_band$timeseries,
  spline_years = c(10, 30, 50, 150),
  do_zscore = TRUE,
  do_ar = TRUE,
  do_diff = TRUE,
  min_year_cor = 1850
)

# Access results
summary_detrended_band$det.timeseries   # per-tree detrended series
summary_detrended_band$det.chronology   # per-year Tukey biweight chronologies
summary_detrended_band$det.diagnostics  # mean Rbar correlations, EPS and n_trees
Sys.time() - t1

# plot detrended rbt
library(ggplot2)
library(dplyr)

# -----------------------------
# 1. Prepare rbt for plotting sector
# -----------------------------
# Add a det_method column for undetrended
summary_sector$diagnostics <- summary_sector$diagnostics %>%
  mutate(det_method = "undetrended")

# Combine with detrended Rbt
rbt_all_sector <- bind_rows(
  summary_sector$diagnostics,
  summary_detrended_sector$det.diagnostics %>% tibble()
)

# Optional: filter by sector or param
sector_sel <- 1:5
param_sel <- "cwtrad"   # example
rbt_plot_sector <- rbt_all_sector %>%
  filter(sector_n %in% sector_sel, param == param_sel)


# -----------------------------
# 2. Plot: mean inter-tree correlations sector
# -----------------------------
ggplot(rbt_plot_sector, aes(x = stat, y = Rbar, fill = det_method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(sector_n ~ param) +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    title = paste("Sector", sector_sel, "- Inter-tree correlations by detrending method"),
    x = "Statistic",
    y = "Mean correlation (Rbt)",
    fill = "Detrending method"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



# -----------------------------
# 1. Prepare rbt for plotting band
# -----------------------------
# Add a det_method column for undetrended
summary_band$diagnostics <- summary_band$diagnostics %>%
  mutate(det_method = "undetrended")

# Combine with detrended Rbt
rbt_all_band <- bind_rows(
  summary_band$diagnostics,
  summary_detrended_band$det.diagnostics %>% tibble()
)

# Optional: filter by sector or param
ew_lw_sel <- c("all", "EW", "LW")
param_sel <- "cwtrad_q75"   # example
rbt_plot_band <- rbt_all_band %>%
  filter(ew_lw %in% ew_lw_sel, param == param_sel)


# -----------------------------
# 2. Plot: mean inter-tree correlations band
# -----------------------------
ggplot(rbt_plot_band, aes(x = stat, y = Rbar, fill = det_method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(ew_lw ~ param) +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    title = paste("Band", ew_lw_sel, "- Inter-tree correlations by detrending method"),
    x = "Statistic",
    y = "Mean correlation (Rbt)",
    fill = "Detrending method"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


# plot detrended chronology sector
param_sel <- "cwtrad"
stat_sel  <- "q75"
sector_sel <- 5
min_year <- 1850

# -----------------------------
# 1. Detrended per-tree series
# -----------------------------
ts_detrended <- summary_detrended_sector$det.timeseries %>%
  filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year) %>%
  select(tree_id, year, all_of(c("value", "detrended_zscore",
                                 "detrended_ar1", "detrended_diff",
                                 paste0("detrended_spline_", c(10,30,50,150))))) %>%
  pivot_longer(
    cols = -c(tree_id, year, value),
    names_to = "det_method",
    values_to = "detrended_value"
  ) %>%
  group_by(tree_id, det_method) %>%
  arrange(year) %>%
  mutate(segment = cumsum(year - lag(year, default = first(year)) > 1)) %>%
  ungroup()

# -----------------------------
# 2. Undetrended per-tree series
# -----------------------------
ts_undetrended <- summary_sector$timeseries %>%
  filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year) %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  mutate(segment = cumsum(year - lag(year, default = first(year)) > 1)) %>%
  ungroup() %>%
  mutate(det_method = "undetrended",
         detrended_value = value) %>%
  select(tree_id, year, segment, det_method, detrended_value, value)

# Combine
ts_long <- bind_rows(ts_undetrended, ts_detrended)

# -----------------------------
# 3. Chronologies
# -----------------------------
chron_detrended <- summary_detrended_sector$det.chronology %>%
  filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year)

chron_undetrended <- summary_sector$chronology %>%
  filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year) %>%
  mutate(det_method = "undetrended")

chronos_sector <- bind_rows(chron_undetrended, chron_detrended)

# -----------------------------
# 4. Plot
# -----------------------------
ggplot() +
  # Per-tree lines in steelblue
  geom_line(
    data = ts_long,
    aes(x = year, y = detrended_value, group = interaction(tree_id, segment)),
    color = "steelblue",
    alpha = 0.2
  ) +
  # Chronology lines colored by method
  geom_line(
    data = chronos_sector,
    aes(x = year, y = value, color = det_method),
    linewidth = 0.7
  ) +
  facet_wrap(~ det_method, scales = "free_y") +
  labs(
    title = paste("Sector", sector_sel, "chronology for parameter:", param_sel),
    x = "Year",
    y = "Value",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ### Plot comparison
# param_sel <- "cwtrad"
# stat_sel  <- "q75"
# sector_sel <- 5
# min_year <- 1850
#
# # Prepare chronologies
# chron_detrended <- summary_detrended_sector$det.chronology %>%
#   filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year)
#
# chron_undetrended <- summary_sector$chronology %>%
#   filter(param == param_sel, stat == stat_sel, sector_n == sector_sel, year > min_year) %>%
#   mutate(det_method = "undetrended")
#
# chronos_sector <- bind_rows(chron_undetrended, chron_detrended)
#
# # Plot only the chronologies
# ggplot(chronos_sector, aes(x = year, y = value, color = det_method)) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = paste("Sector", sector_sel, "chronology for parameter:", param_sel),
#     x = "Year",
#     y = "Value",
#     color = "Method"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")



# plot detrended band
param_sel <- "cwtrad_q75"
ew_type <- "all"
stat1 = "max_value"
min_year <- 1850

# Pivot per-tree series to long format
ts_undetrended <- summary_band$timeseries %>%
  filter(param == param_sel, ew_lw == ew_type, stat == stat1, year > min_year) %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  mutate(segment = cumsum(year - lag(year, default = first(year)) > 1)) %>%
  ungroup() %>%
  mutate(det_method = "undetrended",
         detrended_value = value) %>%
  select(tree_id, year, segment, det_method, detrended_value, value)

ts_detrended <- summary_detrended_band$det.timeseries %>%
  filter(param == param_sel, ew_lw == ew_type, stat == stat1, year > min_year) %>%
  select(tree_id, year, all_of(c("value", "detrended_zscore",
                                 "detrended_ar1", "detrended_diff",
                                 paste0("detrended_spline_", c(10,30,50, 150))))) %>%
  pivot_longer(
    cols = -c(tree_id, year, value),
    names_to = "det_method",
    values_to = "detrended_value"
  ) %>%
  group_by(tree_id, det_method) %>%
  arrange(year) %>%
  mutate(segment = cumsum(year - lag(year, default = first(year)) > 1)) %>%
  ungroup()

ts_long <- bind_rows(ts_undetrended, ts_long)

# Chronologies
chron_undetrended <- summary_band$timeseries %>%
  filter(param == param_sel, ew_lw == ew_type, stat == stat1, year > min_year) %>%
  group_by(year) %>%
  summarise(
    value = dplR::tbrm(value),   # mean over trees using tbrm
    .groups = "drop"
  ) %>%
  mutate(det_method = "undetrended")

chron_detrended <- summary_detrended_band$det.chronology %>%
  filter(param == param_sel, ew_lw == ew_type, stat == stat1, year > min_year)

chronos_band <- bind_rows(chron_undetrended, chron_detrended)

# Plot
ggplot() +
  # Per-tree lines in steelblue
  geom_line(
    data = ts_long,
    aes(x = year, y = detrended_value, group = interaction(tree_id, segment)),
    color = "steelblue",
    alpha = 0.2
  ) +
  # Chronology lines colored by detrending method
  geom_line(
    data = chronos_band,
    aes(x = year, y = value, color = det_method),
    linewidth = 0.5
  ) +
  facet_wrap(~ det_method, scales = "free_y") +
  labs(
    title = paste("Chronology for parameter:", param_sel),
    x = "Year",
    y = "Value",
    color = "Detrending method"
  ) +
  theme_minimal() + theme(legend.position = "none")









################################################################################
# CALCULATE CLIMATE GROWTH CORRELATION

# Load packages & world map

library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load clima data and world map
world <- ne_countries(scale = "medium", returnclass = "sf")# Load climate rasters (CRU data example)
tmp_rast <- rast("~/Desktop/New Yamal AI 1880_out/cru_ts4.08.1901.2023.tmp.dat_0-140E_20-80N_1901-2023.nc")
pre_rast <- rast("~/Desktop/New Yamal AI 1880_out/cru_ts4.08.1901.2023.pre.dat_0-140E_20-80N_1901-2023.nc")
clim_rasts <- list(temp = tmp_rast, pre = pre_rast)

clim_rasts <- list(temp = tmp_rast)

################################################
## Calculate the correlation for a single pixel at site location
# ===========================================
# Load libraries
# ===========================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(metR)
library(sf)

# ===========================================
# 1. Define detrending methods
# ===========================================
det_methods <- c("undetrended", "detrended_zscore", "detrended_ar1",
                 "detrended_diff", paste0("detrended_spline_", c(10,30,50, 150)))

# ===========================================
# 2. Combine all chronologies in one tibble
# ===========================================
# Add det_method column
chrono_list_sector <- lapply(det_methods, function(meth) {
  if(meth == "undetrended") {
    summary_sector$chronology %>%
      mutate(det_method = "undetrended")
  } else {
    summary_detrended_sector$det.chronology %>%
      filter(det_method == meth)
  }
})

chrono_all_sector <- bind_rows(chrono_list_sector)


chrono_list_band <- lapply(det_methods, function(meth) {
  if(meth == "undetrended") {
    summary_band$chronology %>%
      mutate(det_method = "undetrended")
  } else {
    summary_detrended_band$det.chronology %>%
      filter(det_method == meth)
  }
})

chrono_all_band <- bind_rows(chrono_list_band)


# ===========================================
# FOR SECTOR
xy_sel <- tibble(x = 67.25, y = 70.75)

screen_results <- compute_corr_sitegrid(
  clim_rasts = clim_rasts,
  chrono     = chrono_all_sector,
  xy_sel     = xy_sel,
  group_col  = "sector_n",
  groups     = 1:5,
  params     = c("cwtrad", "la"),
  stats      = c("q25", "mean", "q75"),
  seasons    = list(JJA = 6:8, JJ = 6:7, May = 5, Jun = 6, Jul = 7, Aug = 8),
  clim_var   = "temp",
  start_year = 1901,
  end_year   = 2023,
  min_years  = 20
)

season_order <- c("May", "Jun", "Jul", "Aug", "JJ", "JJA")
screen_results <- screen_results %>%
  mutate(season = factor(season, levels = season_order))

library(ggplot2)
library(plotly)

ggplot(screen_results,
       aes(x = group,
           y = r,
           color = det_method,
           group = det_method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_grid(param + stat ~ season) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Correlation screening at site (67.5E, 70.5N)",
    x = "Sector",
    y = "Correlation (r)",
    color = "Detrending method"
  ) +
  theme_minimal()

p <- ggplot(screen_results,
            aes(x = group,
                y = r,
                color = det_method,
                group = det_method,
                text = paste(
                  "Sector:", group,
                  "<br>r:", round(r, 3),
                  "<br>Param:", param,
                  "<br>Season:", season,
                  "<br>Method:", det_method
                ))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_grid(param + stat ~ season) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Correlation screening at site (67.5E, 70.5N)",
    x = "Sector",
    y = "Correlation (r)",
    color = "Detrending method"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")



# ===========================================
# FOR BAND
xy_sel <- tibble(x = 67.5, y = 70.5)

screen_results <- compute_corr_sitegrid(
  clim_rasts = clim_rasts,
  chrono     = chrono_all_band,
  xy_sel     = xy_sel,
  group_col  = "ew_lw",
  groups     =  c("all", "EW", "LW"),
  params     = c("cwtrad_q75", "la_q75"),
  stats      = c("max_value", "min_value", "delta"),
  seasons    = list(JJA = 6:8, JJ = 6:7, May = 5, Jun = 6, Jul = 7, Aug = 8),
  clim_var   = "temp",
  start_year = 1901,
  end_year   = 2023,
  min_years  = 20
)

season_order <- c("May", "Jun", "Jul", "Aug", "JJ", "JJA")
screen_results <- screen_results %>%
  mutate(season = factor(season, levels = season_order))

library(plotly)

library(ggplot2)
library(plotly)
library(dplyr)

# 1️⃣ Compute overall min/max for y-axis
y_range <- range(screen_results$r, na.rm = TRUE)

# 2️⃣ Find the maximum |r| for each param
max_points <- screen_results %>%
  group_by(param) %>%
  slice_max(order_by = abs(r), n = 1) %>%
  ungroup()

# 3️⃣ Base ggplot
p <- ggplot(screen_results,
            aes(x = group,
                y = r,
                color = det_method,
                group = det_method,
                text = paste(
                  "Sector:", group,
                  "<br>r:", round(r, 3),
                  "<br>Param:", param,
                  "<br>Season:", season,
                  "<br>Method:", det_method
                ))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  # Highlight max |r|
  geom_point(data = max_points, aes(x = group, y = r),
             color = "red", size = 4, shape = 21, fill = "yellow") +
  geom_text(data = max_points,
            aes(x = group, y = r, label = round(r, 2)),
            vjust = -1.2, color = "red", size = 3) +
  facet_grid(param + stat ~ season) +
  scale_y_continuous(limits = y_range) +
  labs(
    title = "Correlation screening at site (67.5E, 70.5N)",
    x = "Sector",
    y = "Correlation (r)",
    color = "Detrending method"
  ) +
  theme_minimal()

# 4️⃣ Convert to interactive
ggplotly(p, tooltip = "text")






################################################
## Calculate the correlation for correlation maps
# SECTOR chronology
t<- Sys.time()
r_sector <- compute_corr_rasters(clim_rasts, summary_sector$chronology, # summary_detrended_sector$det.chronology,
                                 group_col = "sector_n", groups = 5,
                                 params = "cwtrad", stat = "q75")
Sys.time() - t

# BAND chronology
t<- Sys.time()
r_band <- compute_corr_rasters(clim_rasts, summary_band$chronology,
                               group_col = "ew_lw", groups = c("LW"),
                               params = c("cwtrad_q75", "mrw"), stat = c("max_value"))
Sys.time() - t

r_all <- c(r_sector, r_band)

# Convert both rasters to data frames
corr_df <- lapply(names(r_all), function(nm) {
  raster_to_ggplot(r_all[[nm]]) %>%
    mutate(raster_name = nm)  # store full raster name temporarily
}) %>%
  bind_rows() %>%
  separate(
    col = raster_name,
    into = c("sector", "param", "stat", "climvar", "season"),
    sep = "\\."
  ) %>% tibble()



# ===============================
# Plot CORRELATION MAPS with ggplot and facets
# ===============================
library(metR)

library(dplyr)
library(ggplot2)
library(metR)
library(sf)

# Define isolines
isolines <- c(0.4, 0.6, 0.8)

# Find max correlation per facet
max_pixels <- corr_df %>%
  group_by(sector, param, season) %>%
  slice_max(r, n = 1, with_ties = FALSE) %>%
  ungroup()

# Compute top-right position per facet for the label
# Add a small offset to x and y
label_pos <- corr_df %>%
  group_by(sector, param, season) %>%
  summarise(
    x = max(x) - 0.5,
    y = max(y) - 0.5,
    r = paste0("rmax=", round(max(max_pixels$r[max_pixels$sector == unique(sector) &
                           max_pixels$param == unique(param) &
                           max_pixels$season == unique(season)]), 2)),
    .groups = "drop"
  )

#### Plotting
ggplot(corr_df) +

  geom_raster(aes(x = x, y = y, fill = r)) +

  # Contour lines
  geom_contour(aes(x = x, y = y, z = r),
               breaks = isolines,
               color = "grey20", linewidth = 0.3) +

  # Contour labels
  geom_text_contour(
    aes(x = x, y = y, z = r),
    breaks = isolines,
    stroke = 0.2,
    color = "grey20",
#    skip = 20,
    size = 1.8
  ) +

  geom_sf(data = world, fill = NA, color = "grey", linewidth = 0.3) +

  # Max correlation pixel
  geom_point(
    data = max_pixels,
    aes(x = x, y = y),
    color = "darkred",
    size = 1.5
  ) +

  # Max correlation value in top-right corner
  geom_text(
    data = label_pos,
    aes(x = x, y = y, label = r),
    color = "darkred",
    size = 2.5,
    hjust = 1,
    vjust = 1
  ) +

  coord_sf(
    xlim = range(corr_df$x),
    ylim = range(corr_df$y),
    expand = FALSE
  ) +

  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey80",
    name = "r"
  ) +

  facet_wrap(sector ~ interaction(param, season), ncol = 3) +

  theme_minimal() +
  labs(
    title = "Correlation with temperature",
    x = "Longitude",
    y = "Latitude"
  )




corr_df[which(corr_df$r == max(corr_df$r, na.rm = TRUE)), ]


# plot corrclima map with facets for all detrending methods
# ===========================================
# 3. Compute correlations
# ===========================================
# NOTE: compute_corr_rasters_det() is a modified version that
#       includes `det_method` in the raster name
r_all <- compute_corr_rasters_det(
  clim_rasts = clim_rasts,
  chrono     = chrono_all_sector,
  group_col  = "sector_n",
  groups     = 5,
  params     = "cwtrad",
  stat       = "q75"
)

# Flatten list if needed
r_all <- unlist(r_all, recursive = FALSE)

# ===========================================
# 4. Convert raster to ggplot-ready tibble
# ===========================================
corr_df <- lapply(names(r_all), function(nm) {
  raster_to_ggplot(r_all[[nm]]) %>%
    mutate(
      raster_name = nm,
      det_method  = attr(r_all[[nm]], "det_method")
    )
}) %>% bind_rows() %>%
  separate(col = raster_name,
           into = c("sector", "param", "stat", "det_method", "climvar", "season" ),
           sep = "\\.") %>%
  # select(-det_method2) %>%
  tibble()

# ===========================================
# 5. Compute max correlation for labels
# ===========================================
isolines <- c(0.4, 0.6, 0.8)

max_pixels <- corr_df %>%
  group_by(sector, param, season, det_method) %>%
  slice_max(r, n = 1, with_ties = FALSE) %>%
  ungroup()

label_pos <- corr_df %>%
  group_by(sector, param, season, det_method) %>%
  summarise(
    x = max(x) - 0.5,
    y = max(y) - 0.5,
    r = paste0("rmax=", round(max(max_pixels$r[max_pixels$sector == unique(sector) &
                                                 max_pixels$param == unique(param) &
                                                 max_pixels$season == unique(season) &
                                                 max_pixels$det_method == unique(det_method)]), 2)),
    .groups = "drop"
  )

# ===========================================
# 6. Plot correlation maps
# ===========================================
library(ggplot2)
library(metR)

# isolines <- seq(-1, 1, by = 0.2)

corr_df_plot <- corr_df %>%
  filter(!is.na(r), season == "JJA") %>%
  droplevels()   # drop unused factor levels
max_pixels1 <- max_pixels %>% filter(season == "JJA")
label_pos1 <- label_pos %>% filter(season == "JJA")

ggplot(corr_df_plot) +
  geom_raster(aes(x = x, y = y, fill = r)) +
  geom_contour(aes(x = x, y = y, z = r),
               breaks = isolines, color = "grey20", linewidth = 0.3) +
  geom_text_contour(aes(x = x, y = y, z = r),
                    breaks = isolines, stroke = 0.2,
                    color = "grey20", size = 1.5) +
  geom_sf(data = world, fill = NA, color = "grey", linewidth = 0.3) +
  geom_point(data = max_pixels1, aes(x = x, y = y),
             color = "darkred", size = 1.5) +
  geom_text(data = label_pos1, aes(x = x, y = y, label = r),
            color = "darkred", size = 2.5, hjust = 1, vjust = 1) +
  coord_sf(xlim = range(corr_df_plot$x), ylim = range(corr_df_plot$y), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1), na.value = "grey80",
                       name = "r") +
  facet_wrap(~ det_method + sector + param + season, nrow =2) +
  theme_minimal() +
  labs(title = "Correlation maps (cwtrad q75) for all detrending methods",
       x = "Longitude", y = "Latitude")



################################################################################
# END
################################################################################





######################## PARTIAL UPDATE

# Insert here the images to update
# imgs_to_update <- c("YAM_LASI_2770_3_4", "YAM_LASI_2770_5_3", "YAM_LASI_562_3_1")
settings_date_orders <- c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M")

df_rxsmeta_updated <- update_rxsmeta(df_rxsmeta, imgs_to_update, settings_date_orders,
                           roxas_version = 'classic')

QWAdata_updated <- update_QWAdata(QWA_data, imgs_to_update, df_rxsmeta_updated)

if (save_files){
  readr::write_csv(df_rxsmeta_updated,
                   paste0(fname_out, '_df_rxsmeta_up.csv'))
  readr::write_csv(QWAdata_updated$rings,
                   paste0(fname_out, '_rings_up.csv'))
}


# ######### Check edited versus original rings data
# df_rings_edited = read.csv(paste0(path_out, "/", "20260111_TRIA_YAM_AI_1880_rings_edited.csv"))
# df_rings = read.csv(paste0(path_out, "/", "20260111_TRIA_YAM_AI_1880_rings.csv"))
#
# key_cols <- c(
#   "woodpiece_label",
#   "slide_label",
#   "image_label",
#   "year"
# )
#
# missing_in_edited <- df_rings |>
#   dplyr::anti_join(
#     df_rings_edited,
#     by = key_cols
#   )
# missing_in_edited
#
# missing_in_original <- df_rings_edited |>
#   dplyr::anti_join(
#     df_rings,
#     by = key_cols
#   )
# missing_in_original
#
# list(
#   missing_in_edited  = nrow(missing_in_edited),
#   missing_in_original = nrow(missing_in_original)
# )
#
# changed_rows <- df_rings |>
#   dplyr::inner_join(
#     df_rings_edited,
#     by = key_cols,
#     suffix = c("_orig", "_edited")
#   ) |>
#   dplyr::filter(
#     incomplete_ring_orig != incomplete_ring_edited |
#       missing_ring_orig    != missing_ring_edited |
#       exclude_issues_orig  != exclude_issues_edited |
#       exclude_dupl_orig    != exclude_dupl_edited
#   )
#
# changed_rows
#
# comparison <- dplyr::full_join(
#   df_rings |> dplyr::mutate(source = "original"),
#   df_rings_edited |> dplyr::mutate(source = "edited"),
#   by = key_cols
# )
#
# comparison
#
# orig_seq <- df_rings |>
#   dplyr::mutate(seq_id = dplyr::row_number()) |>
#   dplyr::select(seq_id, dplyr::all_of(key_cols))
#
# edit_seq <- df_rings_edited |>
#   dplyr::mutate(seq_id = dplyr::row_number()) |>
#   dplyr::select(seq_id, dplyr::all_of(key_cols))
#
# seq_diff <- orig_seq |>
#   dplyr::rename(seq_orig = seq_id) |>
#   dplyr::inner_join(
#     edit_seq |> dplyr::rename(seq_edited = seq_id),
#     by = key_cols
#   ) |>
#   dplyr::filter(seq_orig != seq_edited)
# seq_diff
