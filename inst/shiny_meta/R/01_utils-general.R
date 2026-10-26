if (getRversion() < "4.4.0") {
  # For older R versions, import from backports
  `%||%` <- getFromNamespace("%||%", "backports")
} else {
  # For R >= 4.4.0, it's already available
  # No action needed
}
# if we absolutely need the magrittr pipe:
# `%>%` <- getFromNamespace("%>%", "magrittr")

# THEME VARS -------------------------------------------------------------------
# define the names of the tabs
tab_start <- '1: Start'
tab_general <- '2: General'
tab_site <- '3: Samples'
tab_summary <- '4: Summary'

# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

# params for dynamic ht heights
# TODO: maybe better use something like this (but need to have additional space after for dropdown overflows):
# hot_height <- min(nrow(df_rings) * 23L + 26L, 400L) + 5L
ht_row_height <- 28 # typical row height in px
ht_min_height <- 180 # min height of the ht
ht_max_height <- 750 # max height of the ht

# Field names for non-ht inputs
# need only the names of fields with validation checks,
# can take table column names from tbl_configs
input_field_names <- c(
  dataset = 'Dataset',
  ds_name = 'Dataset name',
  ds_title = 'Dataset title',
  description = 'Dataset description',
  ds_license = 'License',
  embargoed_until = 'Embargo date',
  authors = 'Authors',
  funding = 'Funding',
  related = "Related works",
  sites = 'Sites',
  trees = 'Trees',
  woodpieces = 'Woodpieces',
  slides = 'Slides')

# GLOBAL OPTIONS ---------------------------------------------------------------
# whether app should be run in example mode or not
# can be set via launch_metadata_app(example_run = ...), or directly
# via options(rxs2tria.shinymeta.example_run = ...) if running on server
#example_run <- getOption("rxs2tria.shinymeta.example_run", TRUE)
server_run <- getOption("rxs2tria.shinymeta.server_run", FALSE)


# UTILS -------------------------------------------------------------------------
and_collapse <- function(x, oxford_comma = FALSE) {
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  if (n == 2) return(paste(x, collapse = " and "))
  if (oxford_comma) {
    paste(paste(x[-n], collapse = ", "), ", and ", x[n])
  } else {
    paste(paste(x[-n], collapse = ", "), " and ", x[n])
  }
}

# COUNTRIES
# read the list of country codes and names
get_countries_info <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20250513.csv", package = "rxs2tria")
  df_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  return(df_countries)
}
countries_info <- get_countries_info()

# load the countries sf object
# country data downloaded from https://www.naturalearthdata.com/downloads/50m-cultural-vectors/
get_countries_shapes <- function(){
  file_path <- system.file("extdata/ne_countries_data", package="rxs2tria")
  countries_sf <- sf::read_sf(file_path)
  return(countries_sf)
}
countries_sf <- get_countries_shapes()

# function to get the country ISO2 code from given coordinates
country_from_coords <- function(lng, lat, countries_sf){
  point <- sf::st_as_sf(data.frame(lon = lng, lat = lat), coords = c("lon", "lat"), crs = 4326)
  country <- sf::st_join(point, countries_sf, join = sf::st_within)
  return(country$ISO_A2_EH)
}

# SPECIES
get_species_info <- function(){
  file_path <- system.file("extdata", "species_ITRDB_codes_20250701.csv", package = "rxs2tria")
  df_species <- vroom::vroom(file_path, show_col_types = FALSE) |>
    dplyr::select(species_name, itrdb_species_code, phylogenetic_group, leaf_habit, tree_ring_structure)
  return(df_species)
}
species_info <- get_species_info()

# convert ISO codes to combined display values (e.g. "CH" -> "Switzerland  (CH)")
iso_to_combined <- function(x) {
  result <- dplyr::left_join(
    data.frame(country_iso_code = x, stringsAsFactors = FALSE),
    countries_info[, c("country_iso_code", "combined")],
    by = "country_iso_code"
  )$combined
  dplyr::coalesce(result, x)
}

# convert combined display values back to ISO codes (e.g. "Switzerland  (CH)" -> "CH")
combined_to_iso <- function(x) {
  result <- dplyr::left_join(
    data.frame(combined = x, stringsAsFactors = FALSE),
    countries_info[, c("country_iso_code", "combined")],
    by = "combined"
  )$country_iso_code
  dplyr::coalesce(result, x)
}

# wrapper to get country or species info for the autocomplete/dropdowns etc
get_options <- function(options){
  if (is.list(options)) { # or by length?
    return(options) # convert to vector?
  } else {
    switch(
      options,
      country_codes_names = return(countries_info$combined),
      # TODO: species_codes_names = return(species_info$species_name)
      species_names = return(species_info$species_name),
      species_codes = return(species_info$itrdb_species_code),
    )
  }
}


# INPUT VALIDATION
# helper to generate max character limit warning message
max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}

get_shiny_schema <- function() {
  schema_path <- system.file(rxs2tria:::schema_rel_path("shiny_meta"), package = "rxs2tria")
  rxs2tria:::resolve_schema(schema_path)
}
full_schema <- get_shiny_schema()



# SHINY STUF -------------------------------------------------------------------
#' Show modal dialog to select input source
#' @param ns Namespace function for the module.
show_input_modal <- function(ns){
  shiny::showModal(
    shiny::modalDialog(
      title = "Select input source",
      shiny::tagList(
        shiny::radioButtons(
          ns("load_type"), "Choose input option:",
          choices = c(
            "From local R environment" = "env",
            "From file" = "file",
            "Load example data" = "example")
        ),
        shiny::hr(),
        shiny::uiOutput(ns("load_details_ui"))
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("confirm_input"), "Proceed")
      )
    )
  )
}

# ERRORS and WARNINGS ------------------------------
# wrapper for an error message
show_error_modal <- function(title, message) {
  if (shiny::isRunning()) {
    shiny::showModal(modalDialog(
      title = title,
      cli::ansi_strip(message),
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    message(sprintf("[ERROR] %s: %s", title, message))
  }
}

# wrapper for a warning notification
show_warning_notification <- function(message) {
  if (shiny::isRunning()) {
    shiny::showNotification(cli::ansi_strip(message), type = "warning")
  } else {
    message(sprintf("[WARNING] %s", message))
  }
}

show_ht_import_modal <- function(ns, confirm_id){
  shiny::showModal(modalDialog(
    title = "Confirm Import",
    tagList(
      span("Importing data will overwrite any changes already made in the current table.
            Are you sure you wish to continue?"),
      br(),
      checkboxInput(
        ns("col_names"),
        label = "Does the input data have column names?",
        value = TRUE
      ),
      numericInput(
        ns("skip"),
        label = "How many rows should be skipped (if any)?",
        value = 0,
        min = 0
      ),
    ),
    footer = tagList(
      actionButton(ns(confirm_id), "Import data"),
      modalButton("Cancel")
    )
  ))
}

# Wrapper around rxs2tria::complete_QWAmetadata() that muffles the
# "empty data frame created from schema" warning: expected/intentional noise
# whenever a component (dataset, authors, sites, ...) was not supplied and
# gets filled in with a schema skeleton by complete_QWAmetadata() itself.
# All other warnings (e.g. type conversion issues on already-supplied data)
# still propagate normally.
complete_QWAmetadata_quiet <- function(x) {
  withCallingHandlers(
    rxs2tria::complete_QWAmetadata(x),
    warning = function(w) {
      if (grepl("empty data frame created from schema", conditionMessage(w))) {
        rlang::cnd_muffle(w)
      }
    }
  )
}

# Safely try to run a given expression without crashing the app
# run the expr in the given block
# and catch errors and warnings, showing them in a modal dialog or notification
# in case of a warning, the rest of the block is still executed
# in case of an error, the block is exited, returning NULL, and the error is shown in a modal dialog
# if propagate_err = FALSE, the parent context will not receive an error and execution continues with the NULL return from the current block
# modal dialog is shown only if the error is not already a modal_shown class, so only for the lowest level
safe_block <- function(expr,
                       err_title = "Error", err_message = "An error occurred:",
                       propagate_err = TRUE,
                       warn_message = "A warning occurred:") {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        show_warning_notification(paste(warn_message, w$message))
        rlang::cnd_muffle(w) # muffle warnings so they don't propagate up
      }
    ),
    error = function(e) {
      if (!inherits(e, "modal_shown")) {
        show_error_modal(err_title, paste(err_message, e$message))
        e <- structure(e, class = c("modal_shown", class(e)))
      }
      if (propagate_err) {
        stop(e)  # re-throw error in parent context
      }
      return(NULL)
    }
  )
}









convert_ds_name_filename <- function(ds_name, max_length = 32) {

  clean_name <- trimws(ds_name)
  clean_name <- gsub("\\s+", "_", clean_name)
  # Replace any unwanted characters with underscores
  clean_name <- gsub("[^A-Za-z0-9_-]", "_", clean_name)

  # Remove any leading hyphens or underscores
  clean_name <- gsub("^[-_]+", "", clean_name)

  # Ensure it's not empty after sanitization
  if (nchar(clean_name) == 0) {
    clean_name <- "UnnamedDS"
  }

  # Limit length to prevent filesystem issues
  if (nchar(clean_name) > max_length) {
    clean_name <- substr(clean_name, 1, max_length)
  }

  clean_name
}

# text input with a small arrow button to its right (e.g. for search fields)
search_input_row <- function(text_input, btn_id, btn_title) {
  shiny::div(
    style = "display: flex; gap: 4px; align-items: flex-end;",
    shiny::div(style = "flex-grow: 1;", text_input),
    shiny::actionButton(btn_id, NULL, icon = shiny::icon("arrow-right"),
                        class = "btn btn-info btn-sm mb-3", title = btn_title)
  )
}
