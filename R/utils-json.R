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



