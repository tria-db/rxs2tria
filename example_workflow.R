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
  mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL))

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

################################################################################
# CALCULATE PROFILES

# SECTOR
sector_data <- calculate_sector_profiles(
  QWA_data %>% .$cells,
  n_sectors = 5,
  sel_cell_params = c("la", "cwttan", "cwtrad"),
  quant_probs = c(0.75)
) %>%
  mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL)) %>%
  arrange(image_label, year, sector_n) %>%
  left_join(QWA_data %>% .$rings %>% select(image_label, year, mrw), by = c("image_label", "year"))

if (save_files){
  readr::write_csv(band,
                   paste0(fname_out, '_sector_data.csv'))
}

# BAND
band_data <- calculate_band_profiles(lapply(QWA_data, function(df) df %>% filter(year >= 1850)),
                        bandwidth = 30,
                        stepsize = 10,
                        sel_cell_params = c("la", "cwttan", "cwtrad"),
                        quant_probs = c(0.75),
                        band_rebound = TRUE) %>%
  mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL)) %>%
  arrange(image_label, year, start)

if (save_files){
  readr::write_csv(band,
                   paste0(fname_out, '_band_data.csv'))
}

################################################################################
# FILTERING UNWANTED RINGS (double rings, etc.) BASED ON FLAGS IN QWA_data$rings

# SECTOR
filter_rings_to_exclude <- function(sector_data,
                                    rings_data = QWA_data$rings,
                                    flags = c("exclude_dupl", "exclude_issues")) {

  rings_to_exclude <- QWA_data$rings %>%
    filter(!if_any(all_of(flags), ~ .x)) %>%
    select(image_label, year) %>%
    distinct() %>%
    arrange(image_label, year)

  filtered_sector_data <- sector_data %>%
    anti_join(exclude_pairs, by = c("image_label", "year"))
  return(filtered_sector_data)
}

# SECTOR
filtered_sector_data <- filter_rings_to_exclude(sector_data,
                     rings_data = QWA_data$rings,
                     flags = c("exclude_dupl", "exclude_issues"))

# BAND
filter_band_data <- filter_rings_to_exclude(band_data,
                                            rings_data = QWA_data$rings,
                                            flags = c("exclude_dupl", "exclude_issues"))


################################################################################
# CALCULATE summary chronology and correlations with max cwtrad_mean

# SECTOR
summary_sector <- summary_sector_chronologies(
  filtered_sector_data,
  SECTOR = 1:5,
  sel_params = c("la", "cwttan", "cwtrad", "mrw"),
  quant_probs = c(0.75),
  min_year_cor = 1900,
  write_csv = FALSE
)

summary_sector$timeseries
summary_sector$chronology
summary_sector$correlation %>% filter(sector_n == 5)

# plot rbt
ggplot(summary_sector$correlation, aes(x = stat, y = mean_cor, fill = param)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ sector_n) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs(
    title = "Inter-tree correlations by parameter and statistic",
    x = "Statistic",
    y = "Mean correlation",
    fill = "Parameter"
  )

# BAND
summary_band <- summary_band_chronologies(
  band_data = filter_band_data,
  sel_params = c("la_q75", "cwttan_q75", "cwtrad_q75", "mrw"),
  min_year_cor = 1900,
  write_csv = FALSE
)

# Access results
summary_band$summary_data   # ring + EW + LW summaries
summary_band$timeseries      # per-tree × year × param
summary_band$chronology     # Tukey biweight mean chronologies
summary_band$correlation    # inter-tree correlations

# plot chronology
ggplot() +
  # per-tree timeseries in semi-transparent colors
  geom_line(
    data = summary_band$timeseries %>% filter(ew_lw == "all", stat == stat_to_plot),
    aes(x = year, y = value, group = tree_id),
    color = "steelblue", alpha = 0.2
  ) +

  # chronology (Tukey biweight mean) as bold black line
  geom_line(
    data = summary_band$chronology %>% filter(ew_lw == "all", stat == stat_to_plot),
    aes(x = year, y = value),
    color = "black",
    linewidth = 1
  ) +

  facet_wrap(~ ew_lw + param, scales = "free_y") +

  theme_minimal() +
  labs(
    title = paste("Tree-ring timeseries and chronology:", stat_to_plot),
    x = "Year",
    y = "Value"
  )


# plot rbt
library(ggplot2)

ggplot(summary_band$correlation, aes(x = stat, y = mean_cor, fill = param)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ ew_lw) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs(
    title = "Inter-tree correlations by parameter and statistic",
    x = "Statistic",
    y = "Mean correlation",
    fill = "Parameter"
  )


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

## Calculate the correlation
# SECTOR chronology
t<- Sys.time()
r_sector <- compute_corr_rasters(clim_rasts, summary_sector$chronology,
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
# Plot with ggplot and facets
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


################################################################################
# END
################################################################################




# Exclude double rings
# rings_df <- readr::read_csv(paste0(fname_out, "_rings.csv"))
QWA_data$rings
# get the list of image_label + year to exclude
exclude_double <- QWA_data$rings %>%
  filter(exclude_dupl == TRUE) %>%
  select(image_label, year) %>%
  distinct()
# filter band to exclude these
band_clean <- band_data %>%
  anti_join(exclude_double, by = c("image_label", "year"))



max_cwtrad <- band_clean |>
  collapse::fgroup_by(tree_id, year) |>
  collapse::fsummarise(
    max_cwtrad_mean = collapse::fmax(cwtrad_mean, na.rm = TRUE)
  ) |> tibble()

# View result
head(max_cwtrad)

yearly_mean <- max_cwtrad %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    mean_max_cwtrad = mean(max_cwtrad_mean, na.rm = TRUE),
    .groups = "drop"
  )

yearly_mean
readr::write_csv(yearly_mean,
                 paste0(fname_out, '_max_cwtrad.csv'))

# Pivot once, then compute correlations per year
wide_max <- max_cwtrad %>%
  pivot_wider(names_from = tree_id, values_from = max_cwtrad_mean) %>%
  arrange(-year) %>% filter(year >=1850)


# correlation across all series
cor_mat <- cor(select(wide_max, -year), use = "pairwise.complete.obs")
mean_cor <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
mean_cor

################################################################################
# CALCULATE SECTOR5 chronology and correlations with max cwtrad_mean

# prf_data <- prf_data %>%
#   arrange(image_label, year, sector_n) %>%
#   mutate(
#     tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL)
#   )  %>%
#   left_join(QWA_data$rings %>% select(image_label, year, mrw), by = c("image_label", "year"))

## -------------------------------
## 2. Exclude double rings
## -------------------------------

exclude_pairs <- QWA_data$rings %>%
  filter(exclude_dupl == TRUE) %>%
  select(image_label, year) %>%
  distinct()

## -------------------------------
## 3. Filter sector 5 + clean
## -------------------------------

prf_s5_clean <- prf_data %>%
  left_join(QWA_data$rings %>% select(image_label, year, mrw), by = c("image_label", "year")) %>%
  filter(sector_n == 5) %>%
  anti_join(exclude_pairs, by = c("image_label", "year"))

## -------------------------------
## 4. Aggregate to tree × year
## -------------------------------
# Use fmean; replace with fmax if needed

la_tree_year <- prf_s5_clean %>%
  collapse::fgroup_by(tree_id, year) %>%
  collapse::fsummarise(
    la_mean = collapse::fmean(la_mean, na.rm = TRUE)
  ) %>%
  as.data.frame()

## -------------------------------
## 5. Yearly mean index (QWA-style)
## -------------------------------

la_yearly <- la_tree_year %>%
  group_by(year) %>%
  summarise(
    mean_cwtrad = mean(la_mean, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  la_yearly,
  paste0(fname_out, "_la_sector5_yearly.csv")
)

## -------------------------------
## 6. Pivot to wide (tree-wise)
## -------------------------------

wide_la <- la_tree_year %>%
  pivot_wider(
    names_from = tree_id,
    values_from = la_mean
  ) %>%
  arrange(year) %>%
  filter(year >= 1900)

## -------------------------------
## 7. Inter-tree correlations
## -------------------------------

cor_mat <- cor(
  select(wide_la, -year),
  use = "pairwise.complete.obs"
)

mean_cor <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
cat("Mean inter-tree correlation (raw):", mean_cor, "\n")




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
