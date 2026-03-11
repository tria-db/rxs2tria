# Prepare a QWA dataset from ROXAS output files
# Copy this template to your working directory with:
#   file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
# See vignette("rxs2tria") for a detailed explanation of each step.

library(rxs2tria)

# ------------------------------------------------------------------------------
# Step 1: Locate ROXAS files
# ------------------------------------------------------------------------------

path_in <- "./path/to/ROXAS_output_data"
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

df_images <- collect_image_info(df_structure$fname_image)
df_settings <- collect_settings_data(df_structure$fname_settings,
                                     roxas_version = "classic")

# To avoid conversion errors, datetime columns are originally read as pure
# character strings, and you need to explicitly convert them to POSIXct with
# the appropriate format(s) and timezone.
df_images$img_created_at <- lubridate::parse_date_time(
  df_images$img_created_at, 
  orders = "%Y:%m:%d %H:%M:%S", # common EXIF format
  tz = "UTC" # commonly used in EXIF tags
)
df_settings$rxs_created_at <- lubridate::parse_date_time(
  df_settings$rxs_created_at,
  orders = c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M"),
  tz = Sys.timezone()
)

rxs_meta <- combine_rxs_metadata(df_structure, df_images, df_settings)
rm(df_images, df_settings)

# ------------------------------------------------------------------------------
# Step 4: Read and clean the measurement data
# ------------------------------------------------------------------------------

QWA_data <- collect_raw_data(df_structure) 
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

write.csv(QWA_data$cells,
          file.path(path_out, paste0(dataset_name, "_cells.csv")),
          row.names = FALSE)
write.csv(QWA_data$rings,
          file.path(path_out, paste0(dataset_name, "_rings.csv")),
          row.names = FALSE)
