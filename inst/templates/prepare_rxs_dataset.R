# Prepare a QWA dataset from ROXAS output files
# Copy this template to your working directory with:
#   file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
# See vignette("rxs2tria") for a detailed explanation of each step.

library(rxs2tria)

# ------------------------------------------------------------------------------
# Step 1: Locate ROXAS files
# ------------------------------------------------------------------------------

path_in <- "./path/to/ROXAS_data"
files <- get_roxas_files(path_in, roxas_version = "roxas", exclude_dirs = c("test", "unused"))

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

rxs_images <- build_QWAimages(df_structure, df_settings)
rm(df_settings)

# To save the QWAimages object to a file for later, use:
# write_QWAimages(rxs_images, "path/to/output_data/QWAimages.csv.gz")

# and read it in again
# rxs_images <- read_QWAimages("path/to/output_data/QWAimages.csv.gz")

# ------------------------------------------------------------------------------
# Step 3b: Additional metadata (for TRIA submission)
# ------------------------------------------------------------------------------
# To provide the slide-, woodpiece-, ..., and dataset-level metadata required for a
# TRIA submission, use the interactive Shiny app.
# The app takes rxs_images as input and yields a full QWAmetadata object.
# If run locally, the app can access objects such as `rxs_images` from the
# current environment.
launch_metadata_app()


# ------------------------------------------------------------------------------
# Step 4: Read and clean the measurement data
# ------------------------------------------------------------------------------

# if you would like to read rxs_images data from file:
# rxs_images <- read_QWAimages("path/to/output_data/QWAimages.csv.gz")

QWA_data <- collect_raw_data(rxs_images)
QWA_data <- remove_outliers(QWA_data)
QWA_data <- complete_cell_measures(QWA_data)

# validate_QWA_data checks dating and CWT estimates and adds ring quality flags
QWA_data <- validate_QWA_data(QWA_data, rxs_images)

# ------------------------------------------------------------------------------
# Step 5: Save the results
# ------------------------------------------------------------------------------

path_out <- "./path/to/output"
dataset_name <- "my_dataset"

write_QWAdata(QWA_data, dir = path_out, dataset_name = dataset_name)

# ------------------------------------------------------------------------------
# Step 6a: Compute radial profiles (optional)
# ------------------------------------------------------------------------------
# Profiles are standalone QWAprofile objects, separate from QWAdata.

prf_sector <- calculate_sector_profiles(QWA_data,
                                        n_sectors = 5,
                                        sel_cell_params = c("la", "cwttan"),
                                        quant_probs = c(0.1, 0.5, 0.9)
                                       )

# band_profiles <- calculate_band_profiles(QWA_data,
#                                          bandwidth = 50, stepsize = 25,
#                                          sel_cell_params = c("la", "cwttan"))

write_QWAprofile(prf_sector,
                 file.path(path_out, paste0(dataset_name, "_profiles_sector5.csv.gz")))


# ------------------------------------------------------------------------------
# Step 6b: Assess data quality of annual rings
# ------------------------------------------------------------------------------
# can also be run from just ring measurements, without sector profiles
launch_flags_app()