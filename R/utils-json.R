#' Read and resolve JSON Schema from file
#'
#' @param schema_path Path to the JSON schema file
#' @return A list containing the fully resolved schema
#' @export
resolve_schema <- function(schema_path) {

  # Determine base path for resolving relative references
  base_path <- dirname(schema_path)

  # load schema from file
  obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  schema <- jsonlite::fromJSON(obj$schema$schema, simplifyDataFrame = FALSE)

  # helper function to extract table properties from base table schema
  extract_tbl_props <- function(tbl_schema){
    resolved_schema <- list()
    stopifnot(tbl_schema$type == "array")
    resolved_schema$minItems <- tbl_schema$minItems
    resolved_schema$maxItems <- tbl_schema$maxItems
    resolved_schema$additionalProperties <- tbl_schema$additionalProperties %||% TRUE
    resolved_schema$tbl_props <- tbl_schema$items$properties
    req_props <- tbl_schema$items$required %||% character(0)
    for (rp in req_props) {
      resolved_schema$tbl_props[[rp]][['required']] <- TRUE
    }
    resolved_schema
  }

  # if we are reading a table schema directly: extract info
  if (!is.null(schema$type) && schema$type == "array") {
    resolved_schema <- extract_tbl_props(schema)
    return(resolved_schema)
  }

  # if we are reading an extended schema: iterate over allOf
  if (!is.null(schema$allOf)) {
    resolved_schema <- list()
    for (k in seq_along(schema$allOf)) {
      subschema <- schema$allOf[[k]]
      if (is.null(subschema[['$ref']])) { # if no ref, extract directly
        res_subschema <- extract_tbl_props(subschema)
      } else { # else resolve ref
        ref_path <- subschema[["$ref"]]
        ref_file <- fs::path_real(file.path(base_path, ref_path))
        res_subschema <- resolve_schema(ref_file)
      }
      # combine the resolved subschemata into one
      resolved_schema <- modifyList(resolved_schema, res_subschema)
    }
    return(resolved_schema)
  }

  # if we are reading a combined schema: iterate over the properties (subschema refs)
  if (!is.null(schema$type) && schema$type == "object"){
    resolved_schema <- list()
    for (k in seq_along(schema$properties)) {
      subschema <- schema$properties[[k]]
      if (is.null(subschema[['$ref']])) { # not tested, current structure has only refs
        res_subschema <- extract_tbl_props(subschema)
      } else { # resolve the refs
        ref_path <- subschema[["$ref"]]
        ref_file <- fs::path_real(file.path(base_path, ref_path))
        res_subschema <- resolve_schema(ref_file)
      }
      # add subschema to main schema
      subschema_name <- names(schema$properties)[[k]]
      resolved_schema[[subschema_name]] <- res_subschema
    }
    # main schema props
    resolved_schema$additionalProperties <- schema$additionalProperties %||% TRUE
    req_tbls <- schema$required %||% character(0)
    for (rt in req_tbls) {
      resolved_schema[[rt]][['required']] <- TRUE
    }
    return(resolved_schema)
  }
}

#' Validate a dataframe against json schema
#' @param df the data frame to validate.
#' @param schema the name of the QWAmetadata element under question
#' @param warn_only warn or abort on errors
#' @param greedy stop on first error for faster eval
#' @return passes if validation ok
validate_schema <- function(df, schema = c("dataset", "authors", "funding", "relresources",
                                           "sites", "trees", "woodpieces", "slides", "images"),
                            warn_only = TRUE, greedy = TRUE){
  schema <- match.arg(schema)
  rel_schema_path <- switch(schema,
    "dataset" = "extdata/json_schema/base_schema/20251007_tria_ds_data_schema.json",
    "authors" = "extdata/json_schema/base_schema/20251007_tria_author_data_schema.json",
    "funding" = "extdata/json_schema/base_schema/20251007_tria_funding_data_schema.json",
    "relresources" = "extdata/json_schema/base_schema/20251007_tria_relresource_data_schema.json",
    "sites" = "extdata/json_schema/base_schema/20251007_tria_site_data_schema.json",
    "trees" = "extdata/json_schema/20251007_tria_tree_ext_schema.json",
    "woodpieces" = "extdata/json_schema/base_schema/20251007_tria_woodpiece_data_schema.json",
    "slides" = "extdata/json_schema/base_schema/20251007_tria_slide_data_schema.json",
    "images" = "extdata/json_schema/base_schema/20251007_tria_roxas_data_schema.json",
  )
  schema_path <- system.file(rel_schema_path, package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  result <- schema_obj$validate(
    jsonlite::toJSON(df, na = "null", POSIXt = "ISO8601"),
    verbose = TRUE,
    greedy = greedy
  )
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
          .default = message
        )
      ) |> dplyr::pull(full_message) |> unique()
    names(msg) <- rep("*", length(msg))
    if (warn_only) cli::cli_warn(c("!" = glue::glue("QWAmetadata${schema} validation checks failed:"), msg))
    else cli::cli_abort(c("x" = glue::glue("QWAmetadata${schema} validation checks failed:"), msg))
  }
  invisible(result)
}


