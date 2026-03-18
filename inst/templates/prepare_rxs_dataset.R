# Prepare a QWA dataset from ROXAS output files
# Copy this template to your working directory with:
#   file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
# See vignette("rxs2tria") for a detailed explanation of each step.

library(rxs2tria)

# ------------------------------------------------------------------------------
# Step 1: Locate ROXAS files
# ------------------------------------------------------------------------------

path_in <- "./path/to/ROXAS_data"
files <- get_roxas_files(path_in)

# ------------------------------------------------------------------------------
# Step 2: Extract the data structure from file names
# ------------------------------------------------------------------------------
# Adapt the pattern to match your naming convention using named regex groups.
# Supported group names: site, species, tree, woodpiece, slide, image.
# Example below assumes the common convention {site}_{species}_{tree}_{slide}_{image}
# (one woodpiece per tree, so no woodpiece identifier in the pattern)

pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
df_structure <- extract_data_structure(files, pattern)

# Always check the result before proceeding:
# View(df_structure)

# ------------------------------------------------------------------------------
# Step 3: Collect metadata
# ------------------------------------------------------------------------------

# ROXAS (image EXIF metadata is collected from files_images):
df_settings <- collect_settings_data(files_settings = df_structure$fname_settings,
                                     files_images = df_structure$fname_image,
                                     roxas_version = "roxas")

# ROXAS AI (image EXIF metadata is already part of the JSON files_settings):
df_settings <- collect_settings_data(files_settings = df_structure$fname_settings,
                                     roxas_version = "roxas_ai")

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
  orders = c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M"),
  tz = Sys.timezone()
)

rxs_meta <- combine_rxs_metadata(df_structure, df_settings)
rm(df_settings)

# To save the QWAmetadata object to a file for later, use:
# write_QWAmetadata(rxs_meta, "path/to/output_data/QWAmetadata.json")

# ------------------------------------------------------------------------------
# Step 3b: Additional metadata (for TRIA submission)
# ------------------------------------------------------------------------------
# To provide the site-, tree-, ..., and dataset-level metadata required for a
# TRIA submission, use the interactive Shiny app.
# If run locally, the app can access objects such as `rxs_meta` from the current
# environment.
launch_metadata_app()


# ------------------------------------------------------------------------------
# Step 4: Read and clean the measurement data
# ------------------------------------------------------------------------------

# if you would like to read rxs_meta data from file:
# rxs_meta <- read_QWAmetadata('filename')

# NOTE: only the structure and filepath columns are required to collect the raw
# data, so df_structure also works as input for `collect_raw_data()`.
QWA_data <- collect_raw_data(rxs_meta$images)
QWA_data <- remove_outliers(QWA_data)
QWA_data <- complete_cell_measures(QWA_data)

# validate_QWA_data checks dating and CWT estimates, adds ring quality flags,
# and attaches rxs_meta as QWA_data$metadata
QWA_data <- validate_QWA_data(QWA_data, rxs_meta)

# Inspect the result:
# QWA_data
# launch_coverage_app()

# ------------------------------------------------------------------------------
# Step 5: Save the results
# ------------------------------------------------------------------------------

path_out     <- "./path/to/output"
dataset_name <- "my_dataset"

# TODO: write_QWAdata(QWA_data)

write.csv(QWA_data$cells,
          file.path(path_out, paste0(dataset_name, "_cells.csv")),
          row.names = FALSE)
write.csv(QWA_data$rings,
          file.path(path_out, paste0(dataset_name, "_rings.csv")),
          row.names = FALSE)
