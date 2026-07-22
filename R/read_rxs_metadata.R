#' Read and combine ROXAS (AI) settings and image metadata
#'
#' @description
#' Collect the TRIA-relevant attributes from a collection of ROXAS (AI) settings
#' and image EXIF metadata.
#'
#' The settings (and, for ROXAS, image) file names can be supplied either as a
#' data frame `df` with the columns `fname_settings` (and `fname_image`), such
#' as the output of [extract_data_structure()] or [get_roxas_files()], or as 
#' the individual file path vectors `files_settings` (and `files_images`).
#' Exactly one of the two input styles must be used.
#'
#' For ROXAS files, image EXIF metadata are extracted from the image files and
#' then joined to the settings data. For ROXAS AI files, the EXIF metadata are 
#' already embedded in the settings JSON files and read from there directly.
#'
#' Therefore, [collect_settings_data()] requires the image file paths (`files_images`
#' or the `fname_image` column of `df`) as input if `roxas_version = "roxas"`. 
#' When passed as a vector, they must be in the same order as `files_settings`.
#'
#' If `roxas_version` is not supplied, it is auto-detected from the settings
#' file names.
#'
#' @param df Data frame with the file name columns `fname_settings` (and
#'   `fname_image` for ROXAS), e.g. from [extract_data_structure()]. Provide
#'   this or `files_settings`/`files_images`, but not both.
#' @param files_settings Vector of ROXAS settings file names.
#' @param files_images Vector of image file names in the same order as
#'   `files_settings`. Required when `roxas_version = "roxas"`.
#' @param roxas_version The version of ROXAS used to create the files, either
#'   `"roxas"` or `"roxas_ai"`. Auto-detected from the settings file names if
#'   not supplied.
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_settings_data <- function(df = NULL,
                                  files_settings = NULL,
                                  files_images = NULL,
                                  roxas_version = NULL) {
  # resolve inputs: either the data frame `df` or the file path vectors
  if (!is.null(df)) {
    if (!is.null(files_settings) || !is.null(files_images)) {
      cli::cli_abort(
        "Supply either {.arg df} or {.arg files_settings}/{.arg files_images}, not both.")
    }
    checkmate::assert_data_frame(df)
    checkmate::assert_subset("fname_settings", colnames(df),
                             .var.name = "columns of `df`")
    files_settings <- df$fname_settings
    files_images <- df$fname_image # NULL if column absent (e.g. roxas_ai)
  } else if (is.null(files_settings)) {
    cli::cli_abort(
      "Supply either {.arg df} or {.arg files_settings}.")
  }
  checkmate::assert_character(
    files_settings, any.missing = FALSE, min.len = 1,
    .var.name = "df$fname_settings (or files_settings)")

  # auto-detect the roxas version from the settings file names if not supplied
  roxas_version <- roxas_version %||% detect_roxas_version(files_settings)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  if (roxas_version == "roxas") {
    checkmate::assert_character(
      files_images, len = length(files_settings), any.missing = FALSE,
      .var.name = "df$fname_image (or files_images)"
    )
  }

  rv <- rv_msg(roxas_version)
  results <- files_settings |>
    purrr::map(\(x) read_roxas_settings(x, roxas_version = roxas_version),
               .progress = list(name = glue::glue("Reading {rv} settings files..."), clear = TRUE))
  df_settings_all <- purrr::list_rbind(results)

  if (roxas_version == "roxas") {
    # collect image EXIF metadata and bind alongside settings columns
    # NOTE: files_images and files_settings need to be in the same order
    df_images <- collect_image_info(files_images)
    df_settings_all <- dplyr::bind_cols(df_settings_all,
                                        df_images |> dplyr::select(!"fname_image"))
  } else {
    cli::cli_warn("ROXAS AI support is still under development, not all metadata variables may be available.")
  }

  # coerce raw character columns to their target types (shared with update_QWAimages)
  df_settings_all <- cast_settings_types(df_settings_all, roxas_version)

  cli::cli_alert_success(
    "Extracted {rv} settings and image metadata from {nrow(df_settings_all)} file{?s}."
  )

  df_settings_all
}

#' Build a QWAimages object from raw ROXAS (AI) metadata
#'
#' Combines the extracted ROXAS / ROXAS AI file names plus data structure (from
#' [extract_data_structure()]) and ROXAS settings and image exif data (from
#' [collect_settings_data()]) into a [QWAimages] object.
#'
#' This is the typical starting point of the `prepare_rxs_dataset` workflow.
#' The resulting [QWAimages] can be passed directly to [collect_raw_data()] (to
#' read the cell and ring measurements) and to the metadata Shiny app
#' ([launch_metadata_app()], where one can enrich it with site-, tree-, and
#' dataset-level metadata to produce a full [QWAmetadata] object).
#'
#' @param df_structure Data frame with all input file names and data structure.
#' @param df_settings Data frame with extracted ROXAS (AI) settings data. The
#'   `img_created_at` and `rxs_created_at` columns (if present and not
#'   entirely `NA`) must already be converted to `POSIXct`, e.g. with
#'   [lubridate::parse_date_time()] (see the `workflow` vignette).
#'
#' @returns A [QWAimages] object with the joined image-level metadata.
#' @seealso [extract_data_structure()], [collect_settings_data()],
#'   [QWAimages()], [write_QWAimages()]
#' @export
build_QWAimages <- function(df_structure,
                            df_settings) {
  # assert the minimal required conditions to create a QWAimages data frame
  checkmate::assert_data_frame(df_structure,
     min.rows = 1, .var.name = "df_structure")
  checkmate::assert_subset(
    c("image_label", "slide_label", "woodpiece_label", "tree_label", "site_label",  
      "fname_image",'fname_cells','fname_rings','fname_settings'),
    colnames(df_structure))
  checkmate::assert_data_frame(df_settings)
  checkmate::assert_subset(c('fname_settings'), colnames(df_settings))
  checkmate::assert_permutation(
    df_structure$fname_settings,
    df_settings$fname_settings, na.ok = FALSE)
  # custom check of the hierarchy defined by the structure columns
  checkmate::assert_character(df_structure$image_label, unique = TRUE)
  check_structure(df_structure)

  # datetime columns must already be converted to POSIXct: ROXAS/EXIF
  # timestamp formats are not parsed automatically further down and would
  # otherwise fail with a cryptic error inside align_to_schema()
  datetime_cols <- c("img_created_at", "rxs_created_at")
  needs_conversion <- vapply(datetime_cols, function(col) {
    col %in% colnames(df_settings) &&
      is.character(df_settings[[col]]) &&
      !all(is.na(df_settings[[col]]))
  }, logical(1))
  if (any(needs_conversion)) {
    cli::cli_abort(c(
      "Datetime column{?s} {.field {datetime_cols[needs_conversion]}} in {.arg df_settings} must be converted to {.cls POSIXct} first.",
      "i" = "Convert with {.fun lubridate::parse_date_time}, using the format(s) and timezone appropriate for your data, before calling {.fn build_QWAimages}.",
      "i" = "See the {.emph workflow} vignette (Step 3) for an example."
    ))
  }

  df_rxsmeta <- df_structure |>
    dplyr::left_join(df_settings, by = 'fname_settings')

  # auto-detect schema version from the software column
  rv <- resolve_roxas_version(df_rxsmeta)

  # compliance with base schema
  schema_path <- system.file(schema_rel_path(rv), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  
  # align columns and types to schema (coerce with warnings)
  df_rxsmeta <- align_to_schema(df_rxsmeta, tbl_props, rv, add_opt = FALSE, mute_info = FALSE)

  # validate against the base schema, but warn only. user may want to fix
  # things manually in the metadata app
  check_schema(df_rxsmeta, schema_obj, rv, warn_only = TRUE, greedy = FALSE)

  cli::cli_alert_success(
    "Extracted available {rv_msg(rv)} metadata to {.var QWAimages} object."
  )

  new_QWAimages(df_rxsmeta, roxas_version = rv)
}

#' Detect the ROXAS version from settings file names
#'
#' Infer whether a set of settings files were produced by ROXAS or ROXAS AI
#' based on their (unambiguous) naming suffixes. Aborts if the files are mixed
#' or match neither pattern.
#' @param files_settings Vector of ROXAS settings file names.
#' @returns Either `"roxas"` or `"roxas_ai"`.
#' @noRd
detect_roxas_version <- function(files_settings) {
  is_ai <- stringr::str_detect(
    files_settings, stringr::regex("\\.metadata\\.json$", ignore_case = TRUE))
  is_roxas <- stringr::str_detect(
    files_settings, stringr::regex("_ROXAS_Settings\\.txt$", ignore_case = TRUE))
  if (all(is_ai)) return("roxas_ai")
  if (all(is_roxas)) return("roxas")
  cli::cli_abort(c(
    "Can't auto-detect {.arg roxas_version} from the settings file names.",
    "i" = "Supply {.arg roxas_version} explicitly ({.val roxas} or {.val roxas_ai})."
  ))
}

#' Read data from a ROXAS (AI) settings file
#'
#' Helper function to read and extract the relevant metadata from a single ROXAS
#' settings file
#'
#' @param file_settings The file to be read.
#' @param roxas_version The version of ROXAS used to create the file
#'
#' @returns A dataframe containing the extracted data.
#' @noRd
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
# TODO: finalize support for ROXAS AI
read_roxas_settings <- function(file_settings, roxas_version) {
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))
  
  if (roxas_version == "roxas") {
    # read from a single settings txt file
    df_settings <- vroom::vroom(file_settings,
                                delim = "\t",
                                col_types = c(.default = "c", RNUM = "d"),
                                progress = FALSE)

    # NOTE: this relies heavily on the consistent layout of the settings file
    # in particular, we need tab delimiters, columns RNUM and SETTING,
    # and the right values in the rows 8,9,10,12,13,17,20,31,33,166,203:208,247!
    df_settings <- df_settings |>
      dplyr::filter(.data$RNUM %in% c(8,9,10,
                                      12,13,
                                      17,18,19,20,
                                      31,33,166,
                                      203,204,205,206,207,208,
                                      247
                                      )) |>
      dplyr::mutate(new_names = c(
        "configuration_file", "rxs_created_at", "sw_version",
        "spatial_resolution", "origin_calibrated",
        "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
        "min_cell_area", "max_cell_area", "cluster_dbl_cwt_threshold",
        "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_window", "maxrel_opp_cwt",
        "max_cwttan_s", "max_cwttan_l",
        "rw_reference_file"
      )) |>
      dplyr::select("SETTING", "new_names") |>
      tidyr::pivot_wider(names_from = "new_names", values_from = "SETTING") |>
      # NOTE: columns are left as raw character here; type coercion and value
      # recodes are applied once on the combined frame via cast_settings_types()
      dplyr::mutate(
        fname_settings = file_settings,
        software = "roxas") |>
      tidyr::separate_wider_regex("origin_calibrated",
                                  patterns = c(origin_calibrated_x = "[^/]+",
                                               "[ ]*/[ ]*",
                                               origin_calibrated_y = ".+")) |>
      dplyr::relocate("fname_settings", "software", "sw_version")

  } else { # ROXAS AI
    # read from a single metadata json file
    raw <- jsonlite::read_json(file_settings, simplifyVector = TRUE)
    df_settings <- raw |> 
      purrr::map(\(x) ifelse(is.null(x) || length(x)>1, list(x), x)) |> 
      tibble::as_tibble() |> 
      tidyr::unnest_wider(col = "scan_size", names_sep = "_") |> 
      tidyr::unnest_wider(col = "scan_exif") |>
      dplyr::bind_rows(data.frame("Software" = character(0), # ensure we have all columns even if scan_exif is null
        "DateTimeOriginal" = character(0),
        "DateTimeDigitized"= character(0))) |> 
      dplyr::select(!"scan_info", !"scan_mode") |> # TODO: confirm that we can ignore these
      dplyr::rename(c(
        # config file: instead we have rings_segmentation_model, cells_segmentation_model
        # TODO: analysis created at will be implemented under meas_created_at
        # TODO: sw_version - will be implemented
        # sample_type: new
        "spatial_resolution" = "sample_scale", # TODO: will be renamed to fit
        # origin_calibrated: no calibrated origin atm, might be implemented later for circular samples
        "meas_geometry" = "sample_geometry",  # TODO: will be renamed to fit
        "outmost_year" = "rings_outmost_complete_year", # TODO: note somewhere that this only corresponds to the ROXAS var if outermost ring boundary drawn, else its -1
        # TODO: filtering attributes (similar but not identical to ROXAS vars) still to be implemented
        # e.g. will also have cluster_dbl_cwt_threshold
        # TODO: will there be a rwl reference file var?
        "img_filetype" = "scan_format", 
        # TODO: img_size - will be implemented 
        "img_width" = "scan_size_1", # TODO: these might be renamed to fit?
        "img_height" = "scan_size_2",
        "img_software" = "Software"
        # DateTimeOriginal, DateTimeDigitized -> coalesce to img_created_at
      )) |>
      dplyr::mutate(
        fname_settings = file_settings, 
        software = "roxas_ai", 
        sw_version = NA_character_, 
        rxs_created_at = NA_character_, 
        img_size = NA_integer_,
        img_created_at = dplyr::coalesce(.data$DateTimeOriginal, .data$DateTimeDigitized)
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "fname_settings", "sample_type", "meas_geometry", 
          "img_filetype", "img_width", "img_height", "img_size",
          "img_software", "img_created_at",
          "spatial_resolution",
          "software", "sw_version", "rxs_created_at", "outmost_year",
          "rings_segmentation_model", "cells_segmentation_model"
        ))
      )
  }

  df_settings
}

#' Read and combine image exif data
#'
#' Collect the exif data from all image files. Used preliminarily for data generated
#' with classic ROXAS, for ROXAS AI the image exif information is already stored in the 
#' settings files.
#'
#' @param files_images Vector/list of image filenames
#' @param batch_size Nr of images to read exif data from at a time. 
#'   Note that [exifr::read_exif()] is vectorized, so the only purpose for the 
#'   batching is to generate a progress bar. Increase for more speed.
#' @returns A dataframe containing the extracted data.
#' @noRd
collect_image_info <- function(files_images, batch_size = 50) {
  #checkmate::assert(all(fs::file_exists(files_images))) # too time consuming, fail later
  checkmate::assert_character(files_images, any.missing = FALSE)

  # split into batches for progress bar
  # (big batches are faster since read_exif is already vectorized)
  batches <- split(files_images, ceiling(seq_along(files_images) / batch_size))
  results <- lapply(
    cli::cli_progress_along(seq_along(batches),
                            "Reading image metadata..."),
    function(i) {
      # read exif data for each batch
      exifr::read_exif(batches[[i]],
                       tags = c("FileType", "FileSize",
                                "ImageWidth", "ImageHeight",
                                "Software",
                                "DateTimeOriginal", "DateCreated", # all potential date tags
                                "DateTimeDigitized", "CreateDate"))
  })

  df_image_meta <- dplyr::bind_rows(results)
  df_image_meta[setdiff(c("Software","DateTimeOriginal", "DateCreated",
                          "DateTimeDigitized", "CreateDate"),
                        names(df_image_meta))] <- NA_character_

  df_image_meta <- df_image_meta |>
    dplyr::mutate(
      img_created_at = dplyr::coalesce(
        .data$DateTimeOriginal, .data$DateCreated,
        .data$DateTimeDigitized, .data$CreateDate
      ),
      .keep = "unused" # remove the original exif date cols
    )

  df_image_meta |>
    dplyr::rename("fname_image" = "SourceFile",
                  "img_filetype" = "FileType",
                  "img_size" = "FileSize",
                  "img_width" = "ImageWidth",
                  "img_height" = "ImageHeight",
                  "img_software" = "Software")
}

#' Coerce raw ROXAS (AI) settings columns to their target types
#'
#' After collecting the sraw ettings info from multiple files, convert the 
#' columns to their respective type.
#' Note: Datetime columns are parsed separately by the callers, as they may
#' require different formats.
#' @param df Data frame of combined raw settings columns.
#' @param roxas_version Either `"roxas"` or `"roxas_ai"`.
#' @returns The data frame with coerced column types.
#' @noRd
# TODO: levarage schema for types?
cast_settings_types <- function(df, roxas_version) {
  if (roxas_version == "roxas") {
    df <- df |>
      dplyr::mutate(
        meas_geometry = dplyr::if_else(.data$meas_geometry == 1, "linear", "circular"),
        dplyr::across(c("cluster_dbl_cwt_threshold":"max_cwttan_l"), as.numeric),
        dplyr::across(c("origin_calibrated_x", "origin_calibrated_y", 
                        "circ_lower_limit":"max_cell_area"), as.integer)
      )
  } else {
    df <- df |>
      dplyr::mutate( # roxas ai creates standardized timestamps (?)
        rxs_created_at = lubridate::parse_date_time(.data$rxs_created_at, orders = "ymdHMS") 
      )
  }

  df <- df |> 
    dplyr::mutate(
      dplyr::across("spatial_resolution", as.numeric),
      dplyr::across(c("img_width", "img_height", "img_size", "outmost_year"), as.integer),
    )
  df
}