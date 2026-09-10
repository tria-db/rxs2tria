#' Create a Tucson (.rwl) file from QWAdata$rings or QWAprofile data
#'
#' This function creates a dendrochronological Tucson-format (`.rwl`)
#' file from annualy resolved QWA data. Depending on the selected parameter, the
#' function either uses ring-level measurements (e.g. mean ring width, `mrw`)
#' or profile-level measurements (aggregated cell parameter such as lumenn area
#' `la`) at a given sector.
#'
#' Duplicate rings (`exclude_dupl`) and user-defined ring exclusions 
#' (`exclude_issues`) are automatically removed before constructing the final
#' time series. Since these columns are stored in the `QWAdata$rings` component,
#' it is a required parameter, while the `QWAprofile` is only required to 
#' extract an .rwl file if the selected parameter is from the profile data.
#'
#' Image identifiers are simplified to woodpiece-level series IDs before 
#' exporting, and the resulting data are written to a Tucson `.rwl` file
#' using `dplR::write.tucson()`.
#'
#' @param path_out Character string specifying the output directory where
#'   the \code{.rwl} file and mapping information will be written.
#' @param prf_data A data frame containing ROXAS profile-level measurements
#'   (e.g. sector-based anatomical parameters).
#' @param df_rings A data frame containing ROXAS ring-level measurements and
#'   logical flag columns (the `$rings` compnent of a `QWAdata` object, after
#'   calling `complete_QWAdata()` to instantiate the flag columns.)
#' @param PAR Character string specifying the parameter to export.
#'   Any measurements column name from `prf_data` or `df_rings` may be used.
#' @param SECTOR Integer specifying which sector to use when exporting
#'   profile-level parameters (optional).
#'
#' @return
#' The function is called for its side effects. It writes a Tucson-format
#' \code{.rwl} file to disk and returns the final data frame invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an RWL file from mean ring width
#' create_rwl(prf_data = prf_data,
#'            df_rings = QWA_data$rings,
#'            PAR = "mrw",
#'            path_out = tempdir())
#'
#' # Create an RWL file from a profile-level parameter
#' create_rwl(prf_data = prf_data,
#'            PAR = "cwtrad_mean",
#'            df_rings = QWA_data$rings, #df_rings = read.csv(paste0(path_out, "/", "20251230_TRIA_YAM_AI_1880_rings_edited.csv"))
#'            SECTOR = 5,
#'            path_out = tempdir())
#' }
#'
#' @export
create_rwl <- function(
  df_rings, param, 
  prf_data = NULL, sector = NULL, 
  dir_out = ".") {
  
  checkmate::assert_data_frame(df_rings)
  checkmate::assert_names(names(df_rings), 
    must.include = c("year", "image_label", "slide_label", "woodpiece_label"))
  checkmate::assert_string(param, min.chars = 1)
  checkmate::assert_string(dir_out)

  # TODO: if no exclude flags -> abort and say to run complete first
  # TODO: check rings df valid?

  # possible measurement params for the rings data:
  schema_path <- system.file(schema_rel_path("rings"), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_schema <- resolve_schema(schema_obj, schema_path)
  tbl_props <- get_tbl_props(tbl_schema)
  measure_cols_rings <- tbl_props$properties |>
    purrr::keep(\(x) x$colType %in% c("measure","derived")) |> names()

  meas_cols <- intersect(names(df_rings), measure_cols_rings)

  if (!param %in% meas_cols) {
    checkmate::assert_data_frame(prf_data)
    checkmate::assert_names(names(prf_data), 
      must.include = c("year", "image_label", "sector_n"))
    checkmate::assert_integerish(sector)

    # TODO: check valid, matching df_rings?

    # possible measurement params for the cells data:
    schema_path <- system.file(schema_rel_path("cells"), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
    tbl_schema <- resolve_schema(schema_obj, schema_path)
    tbl_props <- get_tbl_props(tbl_schema)
    measure_cols_cells <- tbl_props$properties |>
      purrr::keep(\(x) x$colType %in% c("measure","derived")) |> names()
    measure_cols_cells <- setdiff(measure_cols_cells, c("sector100", "ew_lw")) # not actually valid params

    meas_cols <- intersect(names(prf_data), measure_cols_cells)
    
    # TODO; check that param is in the possible measurement cols (with _ suffix)
    # TODO: check that sector is in sector_n values
  }

  # Extract selected and prepare for rwl ----
  df_data <- df_rings |> dplyr::filter(!exclude_dupl, !exclude_issues)

  df_data <- df_data |>
    dplyr::select(dplyr::any_of(c("woodpiece_label", "image_label", "year", param)))

  if (param %in% names(prf_data)) {
    df_data <- prf_data |>
      dplyr::filter(.data$sector_n == sector) |>
      dplyr::select(dplyr::all_of(c("image_label", "year", param))) |>
      dplyr::right_join(df_data, by = c("image_label", "year")) |>
      dplyr::select(-"image_label")
  }

  if (!is.null(scaling)){
    scale <- scaling
  } else {
    # to fit Tucson format requirements of max 5 digits
    max_digits <- 5
    vals <- df_data[[param]]
    vals <- vals[!is.na(vals)]

    if (length(vals[vals>0])>0){
      max_val_pos <- max(vals[vals>0], na.rm = TRUE)
    } else {
      max_val_pos <- 0
    }
    if (length(vals[vals<0])>0){
      max_val_neg <- abs(min(vals[vals<0], na.rm = TRUE))
    } else {
      max_val_neg <- 0
    }

    if (max_val_pos > max_val_neg){
      max_val <- max_val_pos
      max_representable <- 10^(max_digits) - 1
    } else {
      max_val <- max_val_neg
      max_representable <- 10^(max_digits-1) - 1 # leave space for negative sign (note: none of the measurements usually have neg values?)
    }

    # find power of 10 scale that fits
    optimal_scale <- max_representable / max_val
    scale <- 10^floor(log10(optimal_scale))
  }

  if (scale != 1){
    cli::cli_warn("Scaling parameter {.var {param}} by factor {.val {scale}}.")
  }
  df_data[[param]] <- df_data[[param]] * scale / 1000 # (the /1000 because write.tucson rescales again later)

  df_rwl <- df_data |>
    tidyr::pivot_wider(names_from = woodpiece_label, values_from = !!param) |>
    dplyr::arrange(year) |>
    tidyr::complete(year = seq(min(year), max(year), by = 1)) |>
    tibble::column_to_rownames("year") |>
    dplR::as.rwl()


  out_file <- file.path(path_out, paste0(param, ".rwl")) # add sector, scaling, allow for path_out
  # TODO: try to shorten names based on df_structure? what if multiple sites?
  # TODO: add warning about auto renaming of dplR if required

  f <- dplR::write.tucson(
    df_rwl,
    fname = out_file,
    prec = 0.001, # IMPORTANT!
    header = NULL,
    append = FALSE,
    long.names = FALSE,
    mapping.fname = "id_map.txt"
  )

  cli::cli_inform("saved file under ..., with .. scaling, ..names mapping")

  invisible(df_rwl)


  # ----------------------------
  # 2. Helper: standardize series ID
  # ----------------------------
  # make_series_id <- function(x) {
  #   paste0(
  #     stringr::str_split(x, "_") |> sapply(`[`, 1),
  #     stringr::str_split(x, "_") |> sapply(`[`, 3)
  #   )
  # }

  # ----------------------------
  # 3. RING-LEVEL PARAMETERS
  # ----------------------------
  # if (PAR %in% names(df_rings)) {

  #   rings_df <- df_rings |>
  #     tibble::as_tibble()

  #   if (remove_excluded && "exclude_issues" %in% names(rings_df)) {
  #     rings_df <- rings_df |> dplyr::filter(!exclude_issues)
  #   }

  #   rwl <- rings_df |>
  #     dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
  #     dplyr::filter(!is.na(.data[[PAR]])) |>
  #     dplyr::mutate(
  #       series = make_series_id(image_label),
  #       value  = .data[[PAR]] / 1000
  #     ) |>
  #     dplyr::group_by(year, series) |>
  #     dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  #     tidyr::pivot_wider(names_from = series, values_from = value) |>
  #     dplyr::arrange(desc(year)) |>
  #     tibble::column_to_rownames("year")

  # } else {

  #   # ----------------------------
  #   # 4. PROFILE-LEVEL PARAMETERS
  #   # ----------------------------
  #   if (is.null(SECTOR)) {
  #     stop("SECTOR must be provided for profile-level parameters.")
  #   }

  #   prf_df <- prf_data |>
  #     tibble::as_tibble() |>
  #     dplyr::filter(sector_n == SECTOR)

  #   # Ensure 'exclude_scope' exists and is character
  #   if (!"exclude_scope" %in% names(df_rings)) {
  #     df_rings$exclude_scope <- NA_character_
  #   }

  #   if (remove_excluded && all(c("image_label", "year") %in% names(df_rings))) {

  #     to_remove <- df_rings |>
  #       dplyr::filter(
  #         (exclude_issues %||% FALSE) |
  #           !is.na(exclude_scope)
  #       ) |>
  #       dplyr::select(image_label, year) |>
  #       dplyr::distinct()

  #     prf_df <- prf_df |>
  #       dplyr::mutate(
  #         image_label = stringr::str_replace(image_label, "_[0-9]+$", "")
  #       ) |>
  #       dplyr::anti_join(to_remove, by = c("image_label", "year"))
  #   }

  #   rwl <- prf_df |>
  #     dplyr::select(image_label, year, !!rlang::sym(PAR)) |>
  #     dplyr::filter(!is.na(.data[[PAR]])) |>
  #     dplyr::mutate(series = make_series_id(image_label)) |>
  #     dplyr::group_by(year, series) |>
  #     dplyr::summarise(value = mean(.data[[PAR]], na.rm = TRUE), .groups = "drop") |>
  #     tidyr::pivot_wider(names_from = series, values_from = value) |>
  #     dplyr::arrange(desc(year)) |>
  #     tibble::column_to_rownames("year")
  # }

  # ----------------------------
  # 5. Write RWL
  # ----------------------------
  # out_file <- file.path(path_out, paste0(PAR, "_roxas.rwl"))

  # # dplR::write.tucson(
  # #   rwl,
  # #   fname = out_file,
  # #   prec = 0.01,
  # #   header = NULL,
  # #   append = FALSE,
  # #   long.names = TRUE,
  # #   mapping.fname = file.path(path_out, "ROXAS.RWL_new.IDs")
  # # )

  # invisible(rwl)
}

save_rwl_file <- function(param, df_rings,
                          df_prf = NULL, sel_sect = NULL,
                          auto_scale = TRUE, scaling = NULL,
                          shorten_name = FALSE, df_structure = NULL,
                          exclude_issue_rings = TRUE, file="", path_out = "") {
  # check:
  # param in df_rings xor df_prf
  # df_rings has correct format, cols
  # df_prf has correct format, cols if provided, incl. sel_sect

  df_data <- df_rings |> dplyr::filter(!exclude_dupl)
  if (exclude_issue_rings){
    df_data <- df_data |> dplyr::filter(!exclude_issues)
  }

  df_data <- df_data |>
    dplyr::select(woodpiece_label, image_label, year, dplyr::any_of(param))
  # TODO: check not duplicates / max one value one per year/wp?

  if (param %in% names(df_prf)) {
    df_data <- df_prf |>
      dplyr::filter(sector_n == sel_sect) |>
      dplyr::select(image_label, year, dplyr::all_of(param)) |>
      dplyr::right_join(df_data, by = c("image_label", "year")) |>
      dplyr::select(-image_label)
  }

  scale <- 1

  if (auto_scale){
    # to fit Tucson format requirements of max 5 digits
    max_digits <- 5
    vals <- df_data[[param]]
    vals <- vals[!is.na(vals)]

    if (length(vals[vals>0])>0){
      max_val_pos <- max(vals[vals>0], na.rm = TRUE)
    } else {
      max_val_pos <- 0
    }
    if (length(vals[vals<0])>0){
      max_val_neg <- abs(min(vals[vals<0], na.rm = TRUE))
    } else {
      max_val_neg <- 0
    }

    if (max_val_pos > max_val_neg){
      max_val <- max_val_pos
      max_representable <- 10^(max_digits) - 1
    } else {
      max_val <- max_val_neg
      max_representable <- 10^(max_digits-1) - 1 # leave space for negative sign (note: none of the measurements usually have neg values?)
    }

    # find power of 10 scale that fits
    optimal_scale <- max_representable / max_val
    scale <- 10^floor(log10(optimal_scale))
  }

  if (!is.null(scaling)){
    scale <- scaling
  }

  if (scale != 1){
    cli::cli_warn("Scaling parameter {.var {param}} by factor {.val {scale}}.")
  }
  df_data[[param]] <- df_data[[param]] * scale / 1000 # (the /1000 because write.tucson rescales again later)

  df_rwl <- df_data |>
    tidyr::pivot_wider(names_from = woodpiece_label, values_from = !!param) |>
    dplyr::arrange(year) |>
    tidyr::complete(year = seq(min(year), max(year), by = 1)) |>
    tibble::column_to_rownames("year") |>
    dplR::as.rwl()

  if (!is.null(file)){
    fname <- file
  } else {
    fname <- paste0(param, ".rwl") # add site, sector, scaling, allow for path_out
  }

  # TODO: try to shorten names based on df_structure? what if multiple sites?
  # TODO: add warning about auto renaming of dplR if required

  f <- dplR::write.tucson(
    df_rwl,
    fname = fname,
    prec = 0.001, # IMPORTANT!
    header = NULL,
    append = FALSE,
    long.names = FALSE,
    mapping.fname = "id_map.txt"
  )

  cli::cli_inform("saved file under ..., with .. scaling, ..names mapping")
}


