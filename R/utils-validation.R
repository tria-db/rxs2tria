#' @noRd
rv_msg <- function(rv) {
  switch(rv,
    "roxas" = "ROXAS",
    "roxas_ai" = "ROXAS AI",
    cli::cli_abort("Invalid roxas version {.var {rv}}")
  )
}

#' @noRd
cli_truncated_list <- function(x, max_show = 5, bullet = " ") {
  shown <- x[seq_len(min(length(x), max_show))]
  n_more <- length(x) - length(shown)
  c(
    setNames(shown, rep(bullet, length(shown))),
    if (n_more > 0) setNames(glue::glue("... and {n_more} more"), bullet)
  )
}

#' @noRd
cli_truncated_groups <- function(
  groups, max_show_out = 3, max_show_in = 3,
  bullet_out = "*", bullet_in = " ") {
  shown_out <- groups[seq_len(min(length(groups), max_show_out))]
  hidden_out <- groups[seq_len(length(groups))[-seq_len(length(shown_out))]]
  n_more_groups <- length(hidden_out)
  n_more_files <- sum(lengths(hidden_out))
  inner <- lapply(names(shown_out), function(id) {
    files <- shown_out[[id]]
    header <- setNames(paste0("'", id, "' (n = ", length(files), ")"), bullet_out)
    c(header, cli_truncated_list(files, max_show = max_show_in, bullet = bullet_in))
  })
  c(
    unlist(inner, use.names = TRUE),
    if (n_more_groups > 0) 
      glue::glue("... and {n_more_groups} more group{ifelse(n_more_groups==1,'','s')} ({n_more_files} item{ifelse(n_more_files==1,'','s')})")
  )
}

#' Map a schema name to its package-relative schema file path
#'
#' Schema files are named after the schema. The stage overlay
#' (`submission_<schema>.json` or `tria_<schema>.json`) is used if it exists,
#' otherwise the definition shared by both stages (`core/<schema>.json`).
#'
#' @param schema Schema name: a QWAmetadata component (e.g. `"dataset"`,
#'   `"datafiles"`), `"supplementary"`, `"roxas"` / `"roxas_ai"` for images,
#'   `"cells"`, `"rings"`, `"QWAmetadata"` (full schema) or `"shiny_meta"`
#'   (metadata app).
#' @param submission Schema for submission files (`TRUE`) or TRIA dataset
#'   files (`FALSE`)?
#' @return Package-relative path to the JSON schema file.
#' @noRd
schema_rel_path <- function(schema, submission = TRUE) {
  if (schema == "shiny_meta") return("extdata/json_schema/app_QWAmetadata.json")
  stage <- if (submission) "submission" else "tria"
  candidates <- file.path("extdata/json_schema",
                          c(paste0(stage, "_", schema, ".json"),
                            paste0("core/", schema, ".json")))
  for (rel_path in candidates) {
    if (nzchar(system.file(rel_path, package = "rxs2tria"))) return(rel_path)
  }
  cli::cli_abort("No schema {.val {schema}} for {.val {stage}} files.")
}

#' Inline all references in a JSON schema list
#'
#' Recursively walks the schema list and replaces every node of the form
#' `{"$ref": "path/to/schema.json"}` with the content of the referenced file.
#' Relative paths are resolved against `base_path`; the base path is updated
#' when descending into a referenced file so nested refs resolve correctly.
#'
#' @param x A schema as a nested R list.
#' @param base_path Directory used to resolve relative `$ref` paths (from where `x` was loaded).
#' @returns A copy of `x` with all `$ref` nodes replaced by their referenced content.
#' @noRd
resolve_refs <- function(x, base_path){
  if (!is.list(x)) return(x)
  if (!is.null(x[["$ref"]])) {
    ref_file <- fs::path_real(fs::path(base_path, x[["$ref"]]))
    ref_raw  <- jsonlite::fromJSON(readLines(ref_file, warn = FALSE), simplifyDataFrame = FALSE)
    return(resolve_refs(ref_raw, fs::path_dir(ref_file))) # recurse with new base_path
  }
  purrr::map(x, resolve_refs, base_path = base_path)
}

#' @noRd
resolve_schema <- function(schema_obj, schema_path) {
  tbl_schema  <- jsonlite::fromJSON(schema_obj$schema$schema, simplifyDataFrame = FALSE)
  resolve_refs(tbl_schema, fs::path_dir(schema_path))
}

# "required" inside if/then/else applies conditionally only, so these
# branches are skipped by default
#' @noRd
extract_required <- function(x, skip = c("if", "then", "else")) {
  if (!is.list(x)) return(NULL)
  # collect "required" at this level
  current <- if (!is.null(x$required)) list(x$required) else list()
  # recurse into all children except the skipped ones and combine
  if (!is.null(names(x))) x <- x[setdiff(names(x), skip)]
  children <- purrr::map(x, extract_required, skip = skip)
  c(current, children) |> purrr::list_flatten() |> unname()
}
#' @noRd
merge_props <- function(a, b) {
  new_keys  <- setdiff(names(b), names(a))
  both_keys <- intersect(names(b), names(a))
  
  a[new_keys] <- b[new_keys]
  
  for (key in both_keys) {
    if (is.list(a[[key]]) && is.list(b[[key]])) {
      a[[key]] <- merge_props(a[[key]], b[[key]])  # recurse for sub-lists
    } else {
      a[[key]] <- c(a[[key]], b[[key]])  # concatenate atomics
    }
  }
  a
}
#' @noRd
extract_properties <- function(x) {
  if (!is.list(x)) return(NULL)

  current <- if (!is.null(x$properties)) x$properties else list()
  children <- purrr::map(x, extract_properties) |> purrr::compact()
  all_props <- c(list(current), children)
  purrr::reduce(all_props, merge_props, .init = list())
}

#' Helper to extract specific column definitions from a imported schema
#' @returns A list with the column definitions (`properties`), the required
#'   columns (`required`) and the columns required only under an
#'   `if`/`then`/`else` condition (`cond_required`).
#' @noRd
get_tbl_props <- function(tbl_schema) {
  required <- tbl_schema |> extract_required() |> unlist()
  cond_required <- tbl_schema |> extract_required(skip = NULL) |> unlist() |>
    setdiff(required)
  properties <- tbl_schema |> extract_properties()
  prop_names <- names(properties)
  properties <- lapply(prop_names, function(nm) {
    c(properties[[nm]], list(required = nm %in% required))
  })
  names(properties) <- prop_names
  list(properties = properties, required = required, cond_required = cond_required)
}

#' Names of the quality control columns set in the flags app (`colType`
#' `"qc_input"`) in table properties
#' @param tbl_props Output of `get_tbl_props()`.
#' @noRd
qc_input_cols <- function(tbl_props) {
  tbl_props$properties |>
    purrr::keep(\(x) isTRUE(x$colType == "qc_input")) |>
    names()
}

#' Names of the columns of a table schema with the given colType(s)
#' @param schema Schema name, see `schema_rel_path()`.
#' @param col_types Character vector of `colType` values.
#' @returns Character vector of column names, in schema order.
#' @noRd
schema_cols <- function(schema, col_types) {
  schema_path <- system.file(schema_rel_path(schema), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- get_tbl_props(resolve_schema(schema_obj, schema_path))
  tbl_props$properties |>
    purrr::keep(\(x) isTRUE(x$colType %in% col_types)) |>
    names()
}

#' Key columns of a table schema (`colType` `"key"`) and their JSON types
#'
#' Reads the schema with jsonlite only (no V8 context), as the validators call
#' it often.
#' @param schema Schema name, see `schema_rel_path()`.
#' @returns Named character vector: JSON type by key column, in schema order.
#' @noRd
key_props <- function(schema) {
  schema_path <- system.file(schema_rel_path(schema), package = "rxs2tria")
  tbl_schema <- jsonlite::read_json(schema_path, simplifyVector = TRUE, simplifyDataFrame = FALSE) |>
    resolve_refs(fs::path_dir(schema_path))
  extract_properties(tbl_schema) |>
    purrr::keep(\(x) "key" %in% x$colType) |>
    vapply(\(x) x$type[1], character(1))
}

#' Names of the cell measurement columns that can be aggregated
#'
#' All `measure` and `derived` columns of the cells schema except radial
#' positions, categorical columns and neighbour IDs.
#' @returns Character vector of column names, in schema order.
#' @noRd
cell_param_cols <- function() {
  setdiff(schema_cols("cells", c("measure", "derived")),
          c("raddistr", "rraddistr", "sector100", "ew_lw", "nbrid"))
}

# tbl_props <- get_tbl_props(full_schema$properties$tbl_name)
# tbl_props <- get_tbl_props(tbl_schema)


#' Create an empty dataframe conforming to a JSON schema
#'
#' @param tbl_props The properties from table schema (i.e. after
#'   resolve_refs() and get_tbl_props(), with `$properties`
#'   containing the column definitions, `$required` the required columns.
#' @param nrows Number of rows to create. If `> 0`, character columns are
#'   filled with `""` and logical columns with `FALSE`.
#' @returns A tibble with columns and R types matching the schema, with
#'   `nrows` rows.
#' @noRd
create_empty_df <- function(tbl_props, nrows = 0) {
  # Mapping JSON Schema types to R types
  json_to_r_types <- c(
    string = "c",
    integer = "i",
    boolean = "l",
    number = "d",
    date = "D",
    datetime = "T",
    null = "c" # or "NULL"?
  )

  # extract column names and types from schema
  cols_info <- tbl_props$properties
  col_names <- names(cols_info)
  col_types <- sapply(cols_info, function(x) x$type[1]) # NOTE: leverages that default type is in first position in schema!

  # special cases:
  col_formats <- sapply(cols_info, function(x) if (!is.null(x$format)) x$format else "none")
  col_types[which(col_formats == "date-time")] <- "datetime"
  col_types[which(col_formats == "date")] <- "date"

  r_col_types <- paste(json_to_r_types[col_types], collapse ="")
  empty_df <- vroom::vroom(I("\n"), col_names = col_names, col_types = r_col_types)

  # add dummy rows if nrows > 0
  if (nrows > 0) {
    char_cols <- col_names[col_types == "string"] # NOTE: we have at least one char column in all our tables
    bool_cols <- col_names[col_types == "boolean"]
    empty_df[1:nrows, char_cols] <- "" # dummy value to add char rows
    empty_df[1:nrows, bool_cols] <- FALSE # default value FALSE for logical cols bc of checkboxes in rhandsontable
  }

  empty_df
}

#' Helper to convert a column to the target class
#' @param x The column to convert.
#' @param target_class The target class to convert to
#' @noRd
convert_column <- function(x, target_class) {
  target_class <- target_class[1]  # use the first class name
  switch(target_class,
         "character" = as.character(x),
         "factor" = as.factor(x),
         "numeric" = as.numeric(x),
         "integer" = as.integer(x),
         "logical" = as.logical(x),
         "Date" = as.Date(x),
         "POSIXct" = as.POSIXct(x),
         x  # fallback: no conversion
  )
}

#' Helper to force a given df into alignment with a table schema:
#' ensure it has required columns, remove extra columns unless `keep_extra`,
#' force all columns to have the correct type
#' @param df  The data frame to be aligned
#' @param tbl_props  Table properties read from schema (output of get_tbl_props()) 
#' @param schema  Schema name, only used for warning messages
#' @param add_opt  Should missing optional columns be added as well, or only
#'   required? Columns with `colType` `"qc_input"` (set in the flags app) are
#'   never added.
#' @param mute_info  If `TRUE`, suppress the messages on missing required
#'   columns, dropped extra columns and `NA`s from type conversions
#' @param ignore_colnames  Should the column names of df be ignored
#'   in favor of position?
#' @param keep_extra  Keep columns not in the schema (appended after the schema
#'   columns, type unchanged) instead of removing them?
#' @noRd
align_to_schema <- function(df, tbl_props, schema = NULL, add_opt = FALSE,
                            mute_info = FALSE, ignore_colnames = FALSE,
                            keep_extra = FALSE
                            ) {
  # get target empty df to align data to
  target_structure <- create_empty_df(tbl_props, nrows = 0)
  target_cols <- names(target_structure)

  if (is.null(df)) {
    cli::cli_warn(c("!" = "{.field {schema}} empty data frame created from schema"))
    return(target_structure)
  }

  # replace empty strings with NA
  df <- df |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.,"")))

  aligned_data <- df

  # force target_cols as names for source_data if ignore_colnames is TRUE
  if (ignore_colnames){
    names(aligned_data)[1:min(length(aligned_data),length(target_cols))] <- target_cols
  }

  req_cols <- if (add_opt) {
    setdiff(target_cols, qc_input_cols(tbl_props))
  } else {
    tbl_props$required
  }
  source_cols <- names(aligned_data)
  common_cols <- intersect(target_cols, source_cols)
  extra_cols <- setdiff(source_cols, target_cols)

  # enforce the correct class for all columns in df
  for (col in common_cols) {
    aligned_data[[col]] <- convert_column(aligned_data[[col]], class(target_structure[[col]]))
  }
  source_data <- aligned_data

  # bind to target_structure to ensure required+common columns are present
  res_cols <- intersect(target_cols, c(req_cols, common_cols)) # ensures correct order
  aligned_data <- dplyr::bind_rows(target_structure[res_cols], aligned_data[common_cols])
  if (keep_extra && length(extra_cols) > 0) {
    aligned_data[extra_cols] <- source_data[extra_cols]
  }

  # if not muted, collect messages on changes made
  if (!mute_info) {
    msg <- c()
    
    # missing required columns are a warning, optional ones added on request
    # (add_opt) only an information
    missing_cols <- setdiff(tbl_props$required, source_cols)
    added_opt_cols <- setdiff(req_cols, c(source_cols, tbl_props$required))

    if (length(missing_cols) > 0) {
      msg <- c(msg,
        "i" = "Missing required columns added:",
        " " = "{missing_cols}"
      )
    }

    if (length(extra_cols) > 0 && !keep_extra) {
      msg <- c(msg,
        "i" = "Extra columns ignored:", 
        " " = "{extra_cols}"
      )
    }

    if (!is.null(df)) {
      # compare NA counts after conversion
      na_counts_org <- colSums(is.na(df[common_cols]))
      na_counts_aligned <- colSums(is.na(aligned_data[common_cols]))
      if (any(na_counts_aligned > na_counts_org)){
        NA_cols <- names(which(na_counts_aligned>na_counts_org))
        msg <- c(msg,
          "i" = "Type converted columns caused new NAs:", 
          " " = "{NA_cols}"
        )
      }
    }
    
    if (length(msg)>0) {
      cli::cli_warn(c("!" = "{.field {schema}} data frame aligned to schema with {length(msg)%/%2} warning{?s}", msg))
    } else {
      cli::cli_inform(c("i" = "{.field {schema}} data frame aligned with schema"))
    }
    if (length(added_opt_cols) > 0) {
      cli::cli_inform(c("i" = "{.field {schema}} optional columns added:", " " = "{added_opt_cols}"))
    }
    if (length(extra_cols) > 0 && keep_extra) {
      cli::cli_inform(c("i" = "{.field {schema}} extra columns kept:", " " = "{extra_cols}"))
    }
  }

  aligned_data
}

#' Report a check issue as a warning, or as an error with `strict`
#' @param msg Message for cli, interpolated in the caller's environment.
#' @param strict If `TRUE`, abort instead of warning.
#' @return `FALSE`, invisibly.
#' @noRd
report_issue <- function(msg, strict = FALSE, .envir = parent.frame()) {
  if (strict) {
    cli::cli_abort(msg, .envir = .envir)
  } else {
    cli::cli_warn(msg, .envir = .envir)
  }
  invisible(FALSE)
}

#' Does a column match a JSON schema type? Whole numbers count as integer;
#' unknown types pass.
#' @noRd
is_json_type <- function(x, type) {
  switch(type,
    string = is.character(x),
    boolean = is.logical(x),
    number = is.numeric(x),
    integer = is.numeric(x) && all(x == round(x), na.rm = TRUE),
    TRUE
  )
}

#' Abort if key columns are missing, of the wrong type or `NA`, or if rows are
#' not unique by `unique_by`
#' @param df Data frame.
#' @param keys Named character vector: JSON type by key column (`NA` skips the
#'   type check), see `key_props()`.
#' @param what Table name, for messages.
#' @param unique_by Columns that must identify the rows uniquely, or `NULL`.
#' @param hint Optional hint added to the messages on `NA` and duplicate keys.
#' @return `df`, invisibly.
#' @noRd
validate_keys <- function(df, keys, what, unique_by = NULL, hint = NULL) {
  key_cols <- names(keys)
  missing_cols <- setdiff(key_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c("x" = "{.field {what}}: missing key column{?s} {.field {missing_cols}}."))
  }

  wrong_type <- key_cols[!purrr::map2_lgl(df[key_cols], keys, \(x, t) is.na(t) || is_json_type(x, t))]
  if (length(wrong_type) > 0) {
    cli::cli_abort(c(
      "x" = "{.field {what}}: key column{?s} of the wrong type:",
      cli_truncated_list(paste0("`", wrong_type, "` must be ", keys[wrong_type]), max_show = Inf)
    ))
  }

  # images affected by the rows in `rows`, if known
  affected_imgs <- function(rows) {
    if (!"image_label" %in% names(df)) return(NULL)
    imgs <- unique(stats::na.omit(df$image_label[rows]))
    if (length(imgs) > 0) c("i" = "Affected images:", cli_truncated_list(imgs))
  }

  na_rows <- !stats::complete.cases(df[key_cols])
  if (any(na_rows)) {
    na_cols <- key_cols[vapply(df[key_cols], anyNA, logical(1))]
    cli::cli_abort(c(
      "x" = "{.field {what}}: {sum(na_rows)} row{?s} with missing values in key column{?s} {.field {na_cols}}.",
      affected_imgs(na_rows),
      "i" = hint
    ))
  }

  if (!is.null(unique_by)) {
    dupl_rows <- duplicated(df[unique_by]) | duplicated(df[unique_by], fromLast = TRUE)
    if (any(dupl_rows)) {
      cli::cli_abort(c(
        "x" = "{.field {what}}: {sum(dupl_rows)} row{?s} with duplicate {.field {unique_by}}.",
        affected_imgs(dupl_rows),
        "i" = hint
      ))
    }
  }

  invisible(df)
}

#' Helper to check df against schema
#' @return `TRUE` if valid, `FALSE` otherwise (invisibly).
#' @noRd
check_schema <- function(df, schema_obj, schema = NULL, strict = FALSE, greedy = FALSE) {
  # convert NA_character to empty strings for $validate to work correctly
  df_na <- df |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) tidyr::replace_na(x,"")))
  result <- schema_obj$validate(
    jsonlite::toJSON(df_na, na = "null", POSIXt = "ISO8601"),
    verbose = TRUE,
    greedy = greedy
  )

  # if validation resulted in any errors, format the issues into a single message
  if (!result) {
    errors <- attr(result, "errors")
    msg <- errors |>
      tibble::as_tibble() |> 
      dplyr::select(instancePath, keyword, params, message) |> 
      dplyr::mutate(
        last_seg = sub("^.*/", "", instancePath),
        full_message = dplyr::case_when(
          keyword == "type" ~ paste0(message, ": `", last_seg, "`"),
          keyword == "additionalProperties" ~ paste0(message, ": `", params$additionalProperty, "`"),
          keyword == "unevaluatedProperties" ~ paste0(message, ": `", params$unevaluatedProperty, "`"),
          keyword == "enum" ~ paste0(message, ": `", last_seg, "` (", params$allowedValues, ")"),
          .default = paste0(message, ": `", last_seg, "`")
        )
      ) |> dplyr::pull(full_message) |> unique()
    names(msg) <- rep("*", length(msg))
    return(report_issue(
      c("!" = "{.field {schema}} data frame failed validation checks:", msg),
      strict
    ))
  }
  invisible(TRUE)
}

#' Helper to check a large data frame against the column rules of its schema
#'
#' Vectorised alternative to `check_schema()` for the cells and rings tables:
#' checks `type`, `minimum`, `maximum` and `enum` of the schema columns, with
#' the messages of `check_schema()` plus the number of rows affected. Extra
#' columns are reported as information only.
#' @param df Data frame to check.
#' @param tbl_props Output of `get_tbl_props()`.
#' @param schema Schema name, only used for messages.
#' @param strict If `TRUE`, abort instead of warning.
#' @return `TRUE` if valid, `FALSE` otherwise (invisibly).
#' @noRd
check_data_schema <- function(df, tbl_props, schema = NULL, strict = FALSE) {
  fmt_n <- \(n) if (n == 1) "1 row" else paste(n, "rows")

  msg <- c()
  for (col in intersect(names(tbl_props$properties), names(df))) {
    prop <- tbl_props$properties[[col]]
    x <- df[[col]]
    if (all(is.na(x))) next
    type <- prop$type[1]
    if (!is_json_type(x, type)) {
      msg <- c(msg, paste0("must be ", type, ": `", col, "`"))
      next
    }
    if (!is.null(prop$minimum)) {
      n <- sum(x < prop$minimum, na.rm = TRUE)
      if (n > 0) msg <- c(msg, paste0("must be >= ", prop$minimum, ": `", col, "` (", fmt_n(n), ")"))
    }
    if (!is.null(prop$maximum)) {
      n <- sum(x > prop$maximum, na.rm = TRUE)
      if (n > 0) msg <- c(msg, paste0("must be <= ", prop$maximum, ": `", col, "` (", fmt_n(n), ")"))
    }
    if (!is.null(prop$enum)) {
      n <- sum(!is.na(x) & !x %in% unlist(prop$enum))
      if (n > 0) msg <- c(msg, paste0(
        "must be equal to one of the allowed values: `", col, "` (",
        paste(unlist(prop$enum), collapse = ", "), ") (", fmt_n(n), ")"
      ))
    }
  }

  extra_cols <- setdiff(names(df), names(tbl_props$properties))
  if (length(extra_cols) > 0) {
    cli::cli_inform(c(
      "i" = "{.field {schema}} extra columns (kept, not imported into TRIA by default):",
      " " = "{extra_cols}"
    ))
  }

  if (length(msg) > 0) {
    names(msg) <- rep("*", length(msg))
    return(report_issue(
      c("!" = "{.field {schema}} data frame failed validation checks:", msg),
      strict
    ))
  }
  invisible(TRUE)
}


#' Helper to check the structure columns define a sound hierarchy
#' @param df data frame containing the structure columns image_label, slide_label,
#'   woodpiece_label, tree_label and site_label, not `NA` (see
#'   `validate_QWAimages()`)
#' @param strict If `TRUE`, abort on label prefix mismatches instead of warning.
#' @return `TRUE` if the label prefixes match, `FALSE` otherwise (invisibly).
#' @noRd
check_structure <- function(df, strict = FALSE) {
  valid <- (
    stringr::str_starts(df$image_label, df$slide_label) &
      stringr::str_starts(df$slide_label, df$woodpiece_label) &
      stringr::str_starts(df$woodpiece_label, df$tree_label) &
      stringr::str_starts(df$tree_label, df$site_label)
  )
  if (any(!valid)) {
    return(report_issue(c(
      "!" = "{.cls QWAimages} structure check failed:",
      " " = "Invalid data structure or not using rxs2tria labels for:",
      " " = "{df$image_label[!valid]}"
    ), strict))
  }
  invisible(TRUE)
}

# report the missing optional columns that complete_*() would add (all except
# "qc_input" columns); conditionally required columns are left to the schema
# validation
#' @noRd
check_missing_opt <- function(df, tbl_props, schema = NULL) {
  missing_opt_cols <- setdiff(
    names(tbl_props$properties),
    c(tbl_props$required, tbl_props$cond_required, qc_input_cols(tbl_props), names(df))
  )
  if (length(missing_opt_cols)>0) {
    cli::cli_inform(c(
      "i" = "{.field {schema}} missing optional columns:",
      "i" = "{missing_opt_cols}"
    ))
  }
  invisible(TRUE)
}

#' Prepare a table for writing to file
#'
#' Converts empty strings to `NA`. With `drop_empty`, drops the optional
#' columns set in the apps (`colType` `"meta_input"` or `"qc_input"`) that are
#' entirely `NA` (or `NA`/`FALSE` for logical columns). A column that is required only under a schema condition
#' (e.g. `embargoed_until` for restricted datasets) is kept if the schema
#' validation reports it as missing without it.
#' @param df Data frame, or `NULL`.
#' @param schema Schema name, see `schema_rel_path()`.
#' @param drop_empty Drop all-empty optional app columns?
#' @param submission Schema for submission files (`TRUE`) or TRIA dataset
#'   files (`FALSE`)?
#' @returns `df` prepared for writing, `NULL` if `df` is `NULL`.
#' @noRd
prepare_write <- function(df, schema, drop_empty = TRUE, submission = TRUE) {
  if (is.null(df)) return(NULL)
  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, "")))
  if (!drop_empty) return(df)

  schema_path <- system.file(schema_rel_path(schema, submission), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- get_tbl_props(resolve_schema(schema_obj, schema_path))

  input_cols <- tbl_props$properties |>
    purrr::keep(\(x) isTRUE(x$colType %in% c("meta_input", "qc_input"))) |>
    names()
  # FALSE is the default of logical app columns (flags app), so counts as empty
  is_empty <- \(x) all(is.na(x) | (is.logical(x) & x %in% FALSE))
  empty_cols <- names(df)[vapply(df, is_empty, logical(1))]
  drop_cols <- setdiff(intersect(empty_cols, input_cols), tbl_props$required)

  # keep conditionally required columns the validation asks for
  cond_cols <- intersect(drop_cols, tbl_props$cond_required)
  if (length(cond_cols) > 0) {
    result <- schema_obj$validate(
      jsonlite::toJSON(df[setdiff(names(df), drop_cols)], na = "null", POSIXt = "ISO8601"),
      verbose = TRUE, greedy = TRUE
    )
    errors <- attr(result, "errors")
    missing_props <- if (is.null(errors)) character(0) else
      errors$params$missingProperty[errors$keyword == "required"]
    drop_cols <- setdiff(drop_cols, missing_props)
  }

  df[setdiff(names(df), drop_cols)]
}





# TODO: other additional validation checks beyond the schemas
# - example: check that outmost year in rxs_images is not in future
