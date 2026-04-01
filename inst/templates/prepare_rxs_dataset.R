# Prepare a QWA dataset from ROXAS output files
# Copy this template to your working directory with:
#   file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
# See vignette("rxs2tria") for a detailed explanation of each step.

library(rxs2tria)

# ------------------------------------------------------------------------------
# Step 1: Locate ROXAS data files
# ------------------------------------------------------------------------------

path_in <- "path/to/ROXAS_data"
files <- get_roxas_files(path_in, roxas_version = "roxas")

# ------------------------------------------------------------------------------
# Step 2: Extract the data structure from file names
# ------------------------------------------------------------------------------
# Adapt the pattern to match your naming convention using named regex groups.
# Supported group names: site, species, tree, woodpiece, slide, image.
# Example below assumes the common convention
#   {site}_{species}_{tree/woodpiece}_{slide}_{image}
# with alphanumeric components and one woodpiece per tree, so no separate 
# woodpiece identifier required.

pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
df_structure <- extract_data_structure(files, pattern)

# Always check the result before proceeding:
# View(df_structure)

# ------------------------------------------------------------------------------
# Step 3: Collect metadata from ROXAS files
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
rm(df_structure, df_settings)

# To save the QWAimages object to a file for later, use:
# write_QWAimages(rxs_images, "path/to/output_data/example_dataset_QWAimages.csv.gz")

# and read it in again
# rxs_images <- read_QWAimages("path/to/output_data/example_dataset_QWAimages.csv.gz")

# ------------------------------------------------------------------------------
# Step 3b: Additional metadata (for TRIA submission)
# ------------------------------------------------------------------------------
# To provide the slide-, woodpiece-, ..., and dataset-level metadata required  
# for a TRIA submission, use the interactive Shiny app.
# The starting point for the app is a QWAimages object such as the `rxs_images`
# derived above. The output from the app is a full QWAmetadata object, which can
# be exported to a .json file. It is also possible to re-load a (partially 
# completed) QWAmetadata object in the app and continue editing.
# If run locally, the app can access objects such as `rxs_images` from the
# current environment, in addition to loading input data from .csv or .json 
# files.
launch_metadata_app()

# after exporting the completed QWAmetadata and closing the Shiny app, you may
# read it in your R session with (not required for the steps below):
# QWA_metadata <- read_QWAmetadata("path/to/output_data/example_dataset_QWAmetadata.json")

# ------------------------------------------------------------------------------
# Step 4: Read and clean the measurement data
# ------------------------------------------------------------------------------
# Read, combine and preprocess the cells and rings measurement data from the
# raw ROXAS output files into a QWAdata object.

QWA_data <- collect_raw_data(rxs_images)
QWA_data <- remove_outliers(QWA_data)
QWA_data <- complete_cell_measures(QWA_data)

# validate_QWA_data checks dating and CWT estimates and adds ring quality flags
# for duplicate rings (overlapping years from separate images), radially 
# incomplete rings (at image borders) and missing/wedging rings.
QWA_data <- validate_QWA_data(QWA_data, rxs_images)

# ------------------------------------------------------------------------------
# Step 5: Save the QWA data
# ------------------------------------------------------------------------------
# QWAdata are saved to two (compressed) csv files with the naming convention
# "{dataset_name}_QWAdata_cells.csv(.gz)" and "{dataset_name}_QWAdata_rings.csv(.gz)"
# though explicit file names may also be provided (see ?write_QWAdata).
path_out <- "path/to/output_data"
dataset_name <- "example_dataset"
write_QWAdata(QWA_data, dir = path_out, dataset_name = dataset_name)

# and to read a QWAdata object again
# QWA_data <- read_QWAdata(dir = path_out, dataset_name = dataset_name)

# ------------------------------------------------------------------------------
# At this stage, you have the minimal components required for a TRIA submission,
# i.e. a QWAmetadata .json file (from the Shiny app), and QWAdata_cells and 
# QWAdata_rings .csv files with the preprocessed measurements data.
# The following optional steps can be completed to provide more in-depth
# qualtiy assessment and discrete features information of the annual rings (Step 6),
# and to compile a list of additional resources (e.g. original and/or annotated
# image files, un-cropped images of slides, .rwl files of the reference series, ...)
# that you want to include with your submission (Step 7).
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 6a: Compute radial profiles
# ------------------------------------------------------------------------------
# The cell measurements can be aggregated by radially splitting rings into 
# a) a fixed number of sectors of equal relative width, or b) a variable number
# of bands with fixed absolute bandwith and stepsize (in micrometers), and then
# calculating statistical aggregates (mean, and---optionally---quantiles).

prf_sector <- calculate_sector_profiles(QWA_data,
                                        n_sectors = 5,
                                        sel_cell_params = c("la", "cwttan"),
                                        quant_probs = c(0.1, 0.5, 0.9))

# prf_band <- calculate_band_profiles(QWA_data,
#                                     bandwidth = 50, stepsize = 25,
#                                     sel_cell_params = c("la", "cwttan"),
#                                     quant_probs = c(0.1, 0.5, 0.9))

# QWAprofile objects can be written to and read from (compressed) .csv files
# write_QWAprofile(prf_sector, "path/to/output_data/example_dataset_QWAprofile_sector5.csv.gz")
# prf_sector <- read_QWAprofile("path/to/output_data/example_dataset_QWAprofile_sector5.csv.gz")

# ------------------------------------------------------------------------------
# Step 6b: Assess data quality of annual rings
# ------------------------------------------------------------------------------
# The flags Shiny app plots the time series of the ring measurements (as well as
# cell measurements aggregated to sector profiles, if available) to give an 
# overview of the data at a glance and allow for easy quality assessemnt. 
# To provide ring-level information on data quality (e.g. compression wood, 
# cracks) and potential discrete features (e.g. blue rings, frost rings), you
# can then interactively select a specific annual ring and set the corresponding
# data quality flags.
# The input for the app (loaded from files or from the current R environemnt) are
# the rings component of a QWAdata object, and optionally a sector profile
# derived from cells measurements and a QWAimages object (used only to give 
# information on any image-level comments that were provided and to allow you to
# open the image files under $fname_images from within the app).
# The output (saved to file or fed back to the current R session) is the 
# QWAdata$rings component with additional columns capturing the provided flags
# information.
launch_flags_app()

# to read in and update the edited rings data from file
# QWA_data$rings <- read_QWAdata(
#   file_rings = "path/to/output_data/example_dataset_edited_QWAdata_rings.csv",
#   components = "rings")

# ------------------------------------------------------------------------------
# Step 7: Compile resources information
# ------------------------------------------------------------------------------
# A TRIA submission must comprise at least the QWAmetadata .json and the 
# QWAdata cells and rings .csv files. If you would like to provide additional
# files with your submission, these should be described in the resources 
# component of the QWAmetadata object, which you can compile as follows

QWA_metadata <- read_QWAmetadata("path/to/output_data/example_dataset_QWAmetadata.json")
QWA_metadata <- add_resources(QWA_metadata, dir = "path/to/submission_files")

write_QWAmetadata("path/to/output_data/example_dataset_QWAmetadata.json")