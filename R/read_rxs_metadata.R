#' Read and combine ROXAS (AI) settings and image metadata
#'
#' @description
#' Collect the TRIA-relevant attributes from a collection of ROXAS (AI) settings
#' and image EXIF metadata.
#'
#' The settings and image file names can be supplied either as a data frame
#' `df` with the columns `fname_settings` and `fname_image`, such as the output
#' of [extract_data_structure()] or [get_roxas_files()], or as the individual
#' file path vectors `files_settings` and `files_images`. When passed as 
#' vectors, `files_images` must be in the same order as `files_settings`.
#'
#' For ROXAS files, image EXIF metadata are extracted from the image files and
#' then joined to the settings data. For ROXAS AI files, the EXIF metadata are
#' already embedded in the settings JSON files and read from there directly;
#' the image files are only used to get the image file size, and for images
#' without a readable settings file.
#'
#' Missing (`NA`), unreadable or non-existing files leave the corresponding
#' metadata empty, with a warning.
#'
#' If `roxas_version` is not supplied, it is auto-detected from the settings
#' file names.
#'
#' @param df Data frame with the file name columns `fname_settings` and
#'   `fname_image`, e.g. from [extract_data_structure()]. Provide this or
#'   `files_settings`/`files_images`, but not both.
#' @param files_settings Vector of ROXAS settings file names (`NA` for
#'   missing settings files).
#' @param files_images Vector of image file names in the same order as
#'   `files_settings` (`NA` for missing images files).
#' @param roxas_version The version of ROXAS used to create the files, either
#'   `"roxas"` or `"roxas_ai"`. Auto-detected from the settings file names if
#'   not supplied.
#'
#' @returns A data frame containing the extracted data.
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
    checkmate::assert_names(names(df), 
      must.include = c("fname_settings", "fname_image"))
    files_settings <- df$fname_settings
    files_images <- df$fname_image
  } else if (is.null(files_settings) || is.null(files_images)) {
    cli::cli_abort(
      "Supply either {.arg df} or both {.arg files_settings} and {.arg files_images}.")
  }
  checkmate::assert_character(
    files_settings, min.len = 1, all.missing = FALSE,
    .var.name = "df$fname_settings (or files_settings)")
  checkmate::assert_character(
    files_images, len = length(files_settings),
    .var.name = "df$fname_image (or files_images)"
  )
  # all-NA vectors may come as logical; also drops the fs_path class
  files_settings <- as.character(files_settings)
  files_images <- as.character(files_images)
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"), null.ok = TRUE)
  # auto-detect the roxas version from the settings file names if not supplied
  roxas_version <- roxas_version %||%
    detect_roxas_version(files_settings[!is.na(files_settings)])
  rv <- rv_msg(roxas_version)

  # collect settings from files (except missing or non-existing)
  settings_exist <- !is.na(files_settings)
  settings_exist[settings_exist] <- fs::is_file(files_settings[settings_exist])
  df_settings_all <- tibble::tibble(
    fname_image = files_images,
    settings = purrr::map(dplyr::if_else(settings_exist, files_settings, NA),
      \(x) read_roxas_settings(x, roxas_version = roxas_version),
      .progress = list(name = glue::glue("Reading {rv} settings files..."), clear = TRUE))
  ) |>
    tidyr::unnest("settings")
  has_settings <- !is.na(df_settings_all$fname_settings) # NA = missing or unreadable
  if (!any(has_settings)) {
    cli::cli_abort("None of the {rv} settings files could be found or read.")
  }
  # unreadable files keep their path value
  df_settings_all$fname_settings <- files_settings

  # collect image exif data where needed (except missing or non-existing)
  has_img <- !is.na(files_images)
  has_img[has_img] <- fs::is_file(files_images[has_img])
  need_exif <- has_img & (roxas_version == "roxas" | !has_settings)
  df_exif <- collect_image_info(unique(df_settings_all$fname_image[need_exif]))
  df_settings_all <- df_settings_all |>
    dplyr::bind_rows(df_exif[0, ]) |> # add missing cols
    dplyr::rows_patch(df_exif, by = "fname_image", unmatched = "error")

  if (roxas_version == "roxas") {
    # convert encoded meas_geometry to actual values
    df_settings_all <- df_settings_all |> dplyr::mutate(
      meas_geometry = dplyr::if_else(.data$meas_geometry == 1, "linear", "circular")
    )
  } else {
    # for roxas ai, get an image size separately (rounded in metadata)
    # follow symlinks, as exiftool does for classic ROXAS
    df_settings_all <- df_settings_all |> dplyr::mutate(
      img_size = as.numeric(fs::file_info(.data[['fname_image']], follow = TRUE)$size)
    )
  }
  
  # coerce raw character columns to their target types
  df_settings_all <- cast_settings_types(df_settings_all, roxas_version)

  # grouped warnings for missing file names and non-existent files
  # (parse errors are raised by read_roxas_settings()/collect_image_info())
  # images are listed by their label, else settings file, else image file
  img_issue <- dplyr::case_when(
    is.na(files_images) ~ "image file name missing",
    !has_img            ~ "image file not found")
  settings_issue <- dplyr::case_when(
    is.na(files_settings) ~ "settings file name missing",
    !settings_exist       ~ "settings file not found")
  issue <- purrr::map2_chr(img_issue, settings_issue,
                           \(a, b) paste(stats::na.omit(c(a, b)), collapse = ", "))
  has_issue <- nzchar(issue)
  if (any(has_issue)) {
    # images listed by their label, else prefix, else settings file, else image file
    ids <- dplyr::coalesce(df[["image_label"]], df[["prefix"]],
                           files_settings, files_images,
                           paste("row", seq_along(files_images)))
    cli::cli_warn(c(
      "!" = "{sum(has_issue)} image{?s} with missing files:",
      cli_truncated_groups(split(ids[has_issue], issue[has_issue]),
                           max_show_out = 5, quote = FALSE),
      "i" = "Their image metadata and/or settings stay empty."
    ))
  }

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
#' @param df_settings Data frame with extracted ROXAS (AI) settings data from
#'   [collect_settings_data()], matched to `df_structure` by `fname_settings`
#'   (or `fname_image` for images without a settings file). Any datetime columns
#'   must already be converted to `POSIXct` (manual conversion is required for 
#'   `meas_created_at` in classic ROXAS, see the `workflow` vignette).
#'
#' @returns A [QWAimages] object with the joined image-level metadata.
#' @seealso [extract_data_structure()], [collect_settings_data()],
#'   [QWAimages()], [write_QWAimages()]
#' @export
build_QWAimages <- function(df_structure, df_settings) {
  # assert the minimal required conditions to create a QWAimages data frame
  checkmate::assert_data_frame(df_structure,
     min.rows = 1, .var.name = "df_structure")
  checkmate::assert_names(names(df_structure), 
    must.include = c("image_label", "slide_label", "woodpiece_label", 
      "tree_label", "site_label", "fname_image", "fname_cells", "fname_rings",
      "fname_settings"))
  checkmate::assert_data_frame(df_settings)
  checkmate::assert_names(names(df_settings), 
    must.include = c('fname_settings', 'fname_image'))
  checkmate::assert_permutation(
    df_structure$fname_settings[!is.na(df_structure$fname_settings)],
    df_settings$fname_settings[!is.na(df_settings$fname_settings)])
  # custom check of the hierarchy defined by the structure columns
  validate_keys(df_structure, key_props("images"), "df_structure", unique_by = "image_label")
  check_structure(df_structure, strict = TRUE)

  # datetime columns must already be converted to POSIXct: ROXAS classic
  # timestamp formats are not parsed automatically further down and would
  # otherwise fail with a cryptic error inside align_to_schema()
  datetime_cols <- c("img_created_at", "meas_created_at")
  needs_conversion <- vapply(datetime_cols, function(col) {
    col %in% colnames(df_settings) &&
      !checkmate::test_posixct(df_settings[[col]]) &&
      !all(is.na(df_settings[[col]]))
  }, logical(1))
  if (any(needs_conversion)) {
    cli::cli_abort(c(
      "Datetime column{?s} {.field {datetime_cols[needs_conversion]}} in {.arg df_settings} must be converted to {.cls POSIXct} first.",
      "i" = "Convert e.g. with {.fun lubridate::parse_date_time} (UTC as local clock time), see the {.emph workflow} vignette (Step 3)."
    ))
  }

  # join by the settings file, or the image file for images without one
  df_rxsmeta <- df_structure |>
    dplyr::mutate(.key = dplyr::coalesce(.data$fname_settings, .data$fname_image)) |> 
    dplyr::left_join(
      df_settings |>
        dplyr::mutate(.key = dplyr::coalesce(.data$fname_settings, .data$fname_image)) |>
        dplyr::select(!c("fname_settings", "fname_image")),
      by = ".key", na_matches = "never") |>
    dplyr::select(!".key")

  # auto-detect schema version from the software column, set it also for
  # images without a settings or image file
  rv <- resolve_roxas_version(df_rxsmeta)
  df_rxsmeta$software <- rv

  # compliance with the submission schema
  schema_path <- system.file(schema_rel_path(rv), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- schema_tbl_props(rv)

  # align columns and types to schema (coerce with warnings)
  df_rxsmeta <- align_to_schema(df_rxsmeta, tbl_props, rv, add_opt = FALSE)

  # validate against the schema, but warn only. user may want to fix
  # things manually in the metadata app
  check_schema(df_rxsmeta, schema_obj, rv, greedy = FALSE)

  # TODO: check outmost_year is valid?

  x <- new_QWAimages(df_rxsmeta, roxas_version = rv)
  validate_QWAimages(x)

  cli::cli_alert_success(
    "Extracted available {rv_msg(rv)} metadata to {.var QWAimages} object."
  )
  x
}

#' Detect the ROXAS version from settings file names
#'
#' Infer whether a set of settings files were produced by ROXAS or ROXAS AI
#' based on their (unambiguous) naming suffixes. Aborts if the files are mixed
#' or match neither pattern.
#' @param files_settings Vector of ROXAS settings file names, no missing values.
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
#' @param file_settings The file to be read (can be NA).
#' @param roxas_version The version of ROXAS used to create the file
#'
#' @returns A dataframe containing the extracted data.
#' @noRd
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
read_roxas_settings <- function(file_settings, roxas_version) {
  checkmate::assert_choice(roxas_version, c("roxas","roxas_ai"))

  if (is.na(file_settings)) {
    return(tibble::tibble(fname_settings = NA_character_, software = roxas_version))
  }
  
  tryCatch({
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
          "configuration_file", "meas_created_at", "sw_version",
          "spatial_resolution", "origin_calibrated",
          "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
          "min_cell_area", "max_cell_area", "cluster_dbl_cwt_threshold",
          "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_integration", "opposite_cwt_ratio_limit",
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
        dplyr::mutate(
          origin_calibrated_x = stringr::str_trim(.data$origin_calibrated_x),
          origin_calibrated_y = stringr::str_trim(.data$origin_calibrated_y),
          # some settings files wrap the file name in (many) double quotes
          rw_reference_file = stringr::str_remove_all(.data$rw_reference_file, '"') |>
            stringr::str_trim() |>
            dplyr::na_if("")) |>
        dplyr::relocate("fname_settings", "software", "sw_version") #

    } else { # ROXAS AI
      # read from a single metadata json file
      raw <- jsonlite::read_json(file_settings, simplifyVector = TRUE)
      df_settings <- raw |> 
        # reformat
        purrr::map(\(x) ifelse(is.null(x) || length(x)>1, list(x), x)) |> 
        tibble::as_tibble() |> 
        tidyr::unnest_wider(col = dplyr::any_of("scan_size"), names_sep = "_") |> 
        tidyr::unnest_wider(col = dplyr::any_of("scan_exif")) |>
        dplyr::bind_rows(data.frame("Software" = character(0), # if scan_exif is null: ensure we still have all columns
          "DateTimeOriginal" = character(0), "DateCreated" = character(0),
          "DateTimeDigitized"= character(0), "CreateDate" = character(0))) |> 
        dplyr::rename(dplyr::any_of(c(
          # keep as-is: meas_geometry, spatial_resolution, sw_version, 
          # meas_created_at, reference_series, 
          # rings_segmentation_model, rings_segmentation_datetime,
          # cells_segmentation_model, cells_segmentation_datetime,
          # cluster_dbl_cwt_threshold, relwidth_cwt_integration,
          # lower_limit_cwt_iqr_multiplier, upper_limit_cwt_iqr_multiplier,
          # opposite_cwt_ratio_limit, adjacent_cwt_ratio_limit
          "meas_geometry" = "sample_geometry", # back-comp with older ROXAS AI versions
          "spatial_resolution" = "sample_scale", # back-comp older ROXAS AI versions
          "outmost_year" = "rings_outmost_complete_year", 
          "img_filetype" = "scan_format", 
          "img_width" = "scan_size_1",
          "img_height" = "scan_size_2",
          "img_software" = "Software"
        ))) |>
        dplyr::mutate(
          fname_settings = file_settings, 
          software = "roxas_ai", 
          img_created_at = dplyr::coalesce(
            .data$DateTimeOriginal, .data$DateCreated,
            .data$DateTimeDigitized, .data$CreateDate)
        ) |>
        dplyr::select(
          dplyr::any_of(c(
            "fname_settings", "meas_geometry", 
            "img_filetype", "img_width", "img_height",
            "img_software", "img_created_at",
            "spatial_resolution",
            "software", "sw_version", "meas_created_at", "outmost_year",
            "rings_segmentation_model", "rings_segmentation_datetime",
            "cells_segmentation_model", "cells_segmentation_datetime",
            "cluster_dbl_cwt_threshold", "relwidth_cwt_integration",
            "lower_limit_cwt_iqr_multiplier", "upper_limit_cwt_iqr_multiplier",
            "opposite_cwt_ratio_limit", "adjacent_cwt_ratio_limit", "reference_series"
          ))
        )
    }

    df_settings
  }, 
  error = function(e) {
    cli::cli_warn(c("!" = "Could not read file {.file {file_settings}}:",
                    " " = "{conditionMessage(e)}")) # interpolated, as it may contain braces
    return(tibble::tibble(fname_settings = NA_character_, software = roxas_version))
  })
}

#' Read and combine image exif data
#'
#' Collect the exif data from all image files. Used primarily for data generated
#' with classic ROXAS, for ROXAS AI the image exif information is already stored
#' in the settings files.
#'
#' @param files_images Vector of image filenames, no missing.
#' @param batch_size Nr of images to read exif data from at a time. 
#'   Note that [exifr::read_exif()] is vectorized, so the only purpose for the 
#'   batching is to generate a progress bar. Increase for more speed.
#' @returns A dataframe containing the extracted data.
#' @noRd
collect_image_info <- function(files_images, batch_size = 50) {
  # split into batches for progress bar
  batches <- split(files_images, ceiling(seq_along(files_images) / batch_size))
  # exiftool outputs numeric-looking values as numbers and omits absent tags:
  # unify the types per batch and ensure all columns exist (also if no files)
  chr_tags <- c("SourceFile", "FileType", "Software",
                "DateTimeOriginal", "DateCreated", # all potential date tags
                "DateTimeDigitized", "CreateDate",
                "Error") # to see if read failed
  num_tags <- c("FileSize", "ImageWidth", "ImageHeight")
  template <- tibble::tibble(
    fname_image = character(0),
    !!!stats::setNames(rep(list(character(0)), length(chr_tags)), chr_tags),
    !!!stats::setNames(rep(list(numeric(0)), length(num_tags)), num_tags)
  )
  results <- lapply(
    cli::cli_progress_along(seq_along(batches),
                            "Reading image metadata..."),
    function(i) {
      # read exif data for each batch
      res <- exifr::read_exif(batches[[i]], tags = c(chr_tags, num_tags)) |>
        dplyr::mutate(dplyr::across(dplyr::any_of(chr_tags), as.character),
                      dplyr::across(dplyr::any_of(num_tags), as.numeric))
      # exiftool returns one entry per file in input order (also for files it
      # cannot read): keep the input paths rather than SourceFile, which may
      # be normalised
      if (nrow(res) != length(batches[[i]])) {
        cli::cli_abort("exiftool returned {nrow(res)} entries for {length(batches[[i]])} image files.")
      }
      res$fname_image <- batches[[i]]
      res
  })

  df_image_meta <- dplyr::bind_rows(template, results) 

  # files exiftool cannot read (e.g. empty, no permission) keep empty metadata
  is_err <- !is.na(df_image_meta$Error)
  purrr::walk2(df_image_meta$fname_image[is_err], df_image_meta$Error[is_err],
    \(f, err) cli::cli_warn(c("!" = "Could not read file {.file {f}}:",
                              " " = "{err}"))) # interpolated, as it may contain braces
  df_image_meta$Error <- NULL
  df_image_meta$SourceFile <- NULL

  df_image_meta <- df_image_meta |>
    dplyr::mutate(
      img_created_at = dplyr::coalesce(
        .data$DateTimeOriginal, .data$DateCreated,
        .data$DateTimeDigitized, .data$CreateDate
      ),
      .keep = "unused" # remove the original exif date cols
    )

  df_image_meta |>
    dplyr::rename("img_filetype" = "FileType",
                  "img_size" = "FileSize",
                  "img_width" = "ImageWidth",
                  "img_height" = "ImageHeight",
                  "img_software" = "Software")
}

#' Coerce raw ROXAS (AI) settings columns to their target types
#'
#' After collecting the raw settings info from multiple files, convert the 
#' columns to their respective type.
#' Datetime columns are parsed here, except `meas_created_at` of classic
#' ROXAS (non-standard formats), which is converted by the user.
#' @param df Data frame of combined raw settings columns.
#' @param roxas_version Either `"roxas"` or `"roxas_ai"`.
#' @returns The data frame with coerced column types.
#' @noRd
cast_settings_types <- function(df, roxas_version) {

  col_props <- schema_tbl_props(roxas_version)$properties

  int_cols <- col_props |> purrr::keep(function(x) x$type[1] == "integer") |> names()
  num_cols <- col_props |> purrr::keep(function(x) x$type[1] == "number") |> names()
  str_cols <- col_props |> purrr::keep(function(x) x$type[1] == "string") |> names()

  datetime_cols <- c("img_created_at",
    "rings_segmentation_datetime","cells_segmentation_datetime")
  # roxas_ai has standardized timestamps, but roxas classic does not
  if (roxas_version == "roxas_ai") {
    datetime_cols <- c(datetime_cols, "meas_created_at")
  }

  df <- df |>
    dplyr::mutate(
      # literal "NA" strings (e.g. unset reference_series) -> proper NA
      dplyr::across(dplyr::any_of(str_cols), ~ dplyr::na_if(.x, "NA")),
      dplyr::across(dplyr::any_of(num_cols), as.numeric),
      dplyr::across(dplyr::any_of(int_cols), as_whole_integer),
      dplyr::across(dplyr::any_of(datetime_cols), lubridate::ymd_hms),
    )
  df
}