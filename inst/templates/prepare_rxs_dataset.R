# Prepare a QWA dataset from ROXAS output files
# Copy this template to your working directory with:
#   file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
# For more information, see https://tria-db.github.io/rxs2tria/articles/workflow.html,
# or consult the help page for each function.

library(rxs2tria)

# ------------------------------------------------------------------------------
# Step 1: Locate ROXAS data files
# ------------------------------------------------------------------------------

roxas_version <- "roxas" # or "roxas_ai"
path_in <- "path/to/ROXAS_data"
files <- get_roxas_files(path_in, roxas_version) 

# ------------------------------------------------------------------------------
# Step 2: Extract the data structure from file names
# ------------------------------------------------------------------------------
# Adapt the pattern to match your naming convention using named regex groups.
# Supported group names: site, species, tree, woodpiece, slide, image.
# Example below assumes the common convention
#   {site}_{species}_{tree}_{slide}_{image}
# with alphanumeric components and one woodpiece per tree (no separate woodpiece
# identifier). If your naming includes a woodpiece identifier, you can add it
# (e.g. by including "(?<woodpiece>[[:alnum:]]+)" between the tree and slide groups).
# See ?extract_data_structure for more information.

pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
df_structure <- extract_data_structure(files, pattern)

# If all images are from the same site and/or species, you can supply them as 
# the site_label/species_code arguments instead. 
# E.g. if all images are from site "ABC" and species "LADE":

# pattern <- "(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
# df_structure <- extract_data_structure(
#   files, pattern, site_label = "ABC", species_code = "LADE")

# Always check the result before proceeding:
# View(df_structure)

# ------------------------------------------------------------------------------
# Step 3: Collect metadata from ROXAS files into QWAimages object
# ------------------------------------------------------------------------------
# Read and extract the relevant info from the settings/metadata files 
# (df_structure$fname_settings) and the EXIF metadata of the image files 
# (df_structure$fname_image).
df_settings <- collect_settings_data(df_structure)

# For ROXAS only, make sure that the "Date created" timestamp from the ROXAS
# settings is converted correctly to POSIXct. Note that we use the convention
# of local clock time stored as UTC (so no need to set a timezone).
# (ROXAS AI meas_created_at and img_created_at are standardised timestamps,
# converted automatically).
settings_date_orders <- c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M") # adjust to your locale
if (roxas_version == "roxas") {
  meas_created_at_converted <- lubridate::parse_date_time(
    df_settings$meas_created_at,
    orders = settings_date_orders
  )
  df_settings$meas_created_at <- meas_created_at_converted
}

rxs_images <- build_QWAimages(df_structure, df_settings)
rm(df_structure, df_settings)

# To save the QWAimages object to a file for later, use:
# write_QWAimages(rxs_images, "path/to/output_data/example_dataset_QWAimages.csv.gz")

# and read it in again
# rxs_images <- read_QWAimages("path/to/output_data/example_dataset_QWAimages.csv.gz")

# ------------------------------------------------------------------------------
# Step 3b: Additional metadata (for TRIA submission)
# ------------------------------------------------------------------------------
# To provide the site-, tree-, woodpiece-, slide-, and dataset-level metadata
# required for a TRIA submission, use the interactive Shiny app.
# The starting point for the app is a QWAimages object such as `rxs_images`
# derived above. The output is a full QWAmetadata object, which can be exported
# to a .json file. It is also possible to re-load a (partially completed)
# QWAmetadata object in the app and continue editing.
# Since it's run locally, the app can access objects such as `rxs_images` from the
# current environment, in addition to loading input data from .csv or .json files.
launch_metadata_app()

# After exporting the completed QWAmetadata and closing the Shiny app, you may
# read it in your R session with (not required for the steps below):
# QWA_meta <- read_QWAmetadata("path/to/output_data/example_dataset_QWAmetadata.json")

# ------------------------------------------------------------------------------
# Step 4: Read and clean the measurement data
# ------------------------------------------------------------------------------
# Read, combine and preprocess the cells and rings measurement data from the
# raw ROXAS output files into a QWAdata object.
# Removes any outliers (negative values) and error codes in columns where they
# could occur. Checks if the dating is valid and if CWT analysis was performed.

QWA_data <- collect_raw_data(rxs_images)
# or if continuing from QWAmetadata object (generated via the metadata Shiny app):
# QWA_data <- collect_raw_data(QWA_meta$images)

# Add additional cell and ring measures, and ring quality flags
# for duplicate rings (overlapping years from separate images), radially
# incomplete rings (at image borders) and missing/wedging rings (complete rings
# with mrw < 10, or mrw = NA and < 5 cells). exclude_dupl is initialised to 
# TRUE for overlapping rings with non-maximum cell count, exclude_issues is
# initialised from the incomplete rings.
QWA_data <- complete_QWAdata(QWA_data, rxs_images)


# ------------------------------------------------------------------------------
# Step 5: Save the QWA data
# ------------------------------------------------------------------------------
# QWAdata are saved to two (compressed) csv files with the naming convention
# "{dataset_name}_QWAcells.csv(.gz)" and "{dataset_name}_QWArings.csv(.gz)"
# though explicit file names may also be provided (see ?write_QWAdata).
path_out <- "path/to/output_data"
dataset_name <- "example_dataset"
write_QWAdata(QWA_data, dir = path_out, dataset_name = dataset_name)

# and to read a QWAdata object again
# QWA_data <- read_QWAdata(dir = path_out, dataset_name = dataset_name)

# ------------------------------------------------------------------------------
# At this stage, you have the minimal components required for a TRIA submission,
# i.e. a QWAmetadata .json file (from the metadata Shiny app), and QWAcells and
# QWArings .csv files with the preprocessed measurements data.
# The following optional steps can be completed to provide more in-depth
# quality assessment and discrete features information of the annual rings (Step 6),
# and to compile a list of supplementary files (e.g. original and/or annotated
# image files, un-cropped images of slides, .rwl files of the reference series, ...)
# that you want to include with your submission (Step 7).
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Step 6a: Compute radial profiles
# ------------------------------------------------------------------------------
# Cell measurements can be aggregated by radially splitting rings into
# a) a fixed number of sectors of equal relative width, or b) bands of fixed
# absolute width and step size (in micrometres), and then calculating
# statistical aggregates (mean, and---optionally---quantiles).

prf_sector <- calculate_sector_profiles(QWA_data,
                                        n_sectors = 5,
                                        sel_cell_params = c("la", "cwttan"),
                                        quant_probs = c(0.1, 0.5, 0.9))

# prf_band <- calculate_band_profiles(QWA_data,
#                                     bandwidth = 50, stepsize = 25,
#                                     sel_cell_params = c("la", "cwttan"),
#                                     quant_probs = c(0.1, 0.5, 0.9))

# After flagging rings (Step 6b), recalculate the profiles with
# `apply_exclusions = TRUE` to remove the cells of excluded rings, and
# `tissue = "affected"` to remove only the affected earlywood/latewood cells
# (see ?calculate_sector_profiles).

# QWAprofile objects can be written to and read from (compressed) .csv files:
# write_QWAprofile(prf_sector, "path/to/output_data/example_dataset_QWAprofile_sector5.csv.gz")
# prf_sector <- read_QWAprofile("path/to/output_data/example_dataset_QWAprofile_sector5.csv.gz")


# ------------------------------------------------------------------------------
# Step 6b: Assess data quality of annual rings
# ------------------------------------------------------------------------------
# The flags Shiny app plots the time series of the ring measurements (as well as
# cell measurements aggregated to sector profiles, if available) to give an
# overview of the data at a glance and allow for easy quality assessment.
# To provide ring-level information on data quality (e.g. compression wood,
# cracks) and potential discrete features (e.g. blue rings, frost rings), you
# can then interactively select a specific annual ring and set the corresponding
# data quality flags.
# The input for the app (loaded from files or from the current R environment) are
# the rings component of a QWAdata object, and optionally: a derived sector 
# QWAprofile, and a QWAimages object (to open the (annotated) image files under
# via $fname_image from within the app, and to show any $img_comment provided).
# The output (saved to file or fed back to the current R session) is the
# QWAdata$rings component with additional columns for the flags, `comment` and
# `affected_tissue`. If image-level comments were reviewed, the app also saves
# the image metadata with the `comment_handled` column.
launch_flags_app()

# To read in and update the edited rings data from file or a data frame in the env:
# QWA_data$rings <- read_QWAdata(
#   file_rings = "path/to/output_data/example_dataset_edited_QWArings.csv")$rings
# QWA_data$rings <- df_rings_edited
# Save the edits to files:
# write_QWAdata(QWA_data, dir = path_out, dataset_name = dataset_name, overwrite = TRUE)

# The ring measurements without excluded rings, for further analyses:
# df_meas <- extract_ring_measures(QWA_data$rings)

# ------------------------------------------------------------------------------
# Step 6c: Update and combine data
# ------------------------------------------------------------------------------
# See https://tria-db.github.io/rxs2tria/articles/update-combine.html
# If the data processing steps above reveal issues in the raw data, it is
# possible to update the (meta-)data for a subset of images only, without having 
# to start fresh.

# Rerun the analyses with the QWA software, and replace the output files
# in `path_in` with the new files. Update the ROXAS settings / ROXAS AI metadata
# information with 
rxs_images <- update_QWAimages(rxs_images,
  imgs_to_reread = c("IMAGELABEL1", "IMAGELABEL2"),
  settings_date_orders = settings_date_orders) # same as defined in Step 3

# If a QWAmetadata object exists, update it as well (keeps the values entered
# in the metadata app), using update_QWAmetadata().

# update_QWAdata() then synchronises an exiting QWA_data with the new 
# rxs_images: new images are added, images no longer in rxs_images are removed, 
# and data for images in `imgs_to_reread` are read again. Duplicate-ring flags 
# are recalculated for all affected woodpiece-years. 
# Note: Added and re-read images are new data, so inspect their flags again 
# (Step 6b) or re-apply any custom exclude_issues rule.

# b) Combining datasets
# Two datasets prepared separately (same ROXAS version, distinct image labels)
# can be combined; for shared entries (e.g. sites, authors) the first object
# takes precedence:
# rxs_images <- combine_QWAimages(rxs_images_1, rxs_images_2)
# QWA_data <- combine_QWAdata(QWA_data_1, QWA_data_2)
# QWA_meta <- combine_QWAmetadata(QWA_meta_1, QWA_meta_2)


# ------------------------------------------------------------------------------
# Step 7: Compile supplementary files (optional)
# ------------------------------------------------------------------------------
# See https://tria-db.github.io/rxs2tria/articles/supplementary.html
# A TRIA submission must comprise at least the QWAmetadata .json and the
# QWAdata cells and rings .csv files. If you would like to provide additional
# (supplementary) files with your submission - e.g. original / annotated images,
# raw ROXAS output, reference series - these are listed in a standalone
# supplementary CSV, submitted alongside a zip of the files themselves.
# compile_supplementary() scans a directory (including nested subfolders) and
# returns a table with one row per file, its inferred type, hierarchy level,
# and a readiness status ("ok", "review", or "ignore"). The console reports
# whether the supplementary files are ready to submit, or which files still
# need attention. 

suppl_res <- compile_supplementary(path_in, rxs_images = rxs_images)
# or rxs_images = QWA_meta$images

suppl_res # review status "review" rows, make edits where needed, then (re-)compile
vroom::vroom_write(suppl_res, "path/to/output_data/example_dataset_supplementary_files.csv",
                   delim = ",", na = "")

# ------------------------------------------------------------------------------
# Step 8: Check and submit your data
# ------------------------------------------------------------------------------
# Before submitting, check the files as they will be submitted (after the
# metadata app and any edits in the flags app). Each check warns about issues
# the TRIA admins would ask you to fix and returns TRUE if none were found;
# with strict = TRUE, it aborts on the first issue instead.
QWA_meta <- read_QWAmetadata("path/to/output_data/example_dataset_QWAmetadata.json")
QWA_data <- read_QWAdata(dir = path_out, dataset_name = dataset_name)

check_QWAmetadata(QWA_meta) # includes the images, see ?check_QWAimages
check_QWAdata(QWA_data)
check_consistency(meta = QWA_meta, data = QWA_data)
# if submitting supplementary files (Step 7), against the final zip:
# check_supplementary(suppl_res, "path/to/supplementary_data.zip",
#                     rxs_images = QWA_meta$images)

# Then email tria@wsl.ch with a link to your files; see vignette("submission")
# for what to upload and what happens to your files once we receive them.
