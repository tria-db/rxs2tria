

# validator functions for different column types
# cf. the corresponding JS renderers in ht_render_utils.R
validate_char_column <- function(column, col_config) {
  # Default values for validation rules
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  minl <- ifelse(is.null(col_config$minLength), -1, col_config$minLength)
  maxl <- ifelse(is.null(col_config$maxLength), 10000, col_config$maxLength)
  check_regex <- !is.null(col_config$pattern)
  regp <- ifelse(is.null(col_config$pattern), "", col_config$pattern)
  check_unique <- ifelse(is.null(col_config$unqiue), FALSE, col_config$unique)

  # Initialize the result list
  #validation_results <- vector("list", length(column))
  validation_results <- c()

  if (check_required && any(is.na(column) | column == "")) {
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (any(nchar(column_nonmissing) < minl)) {
    validation_results <- c(validation_results, "Invalid lengths")
  } else if (any(nchar(column_nonmissing) > maxl)) {
    validation_results <- c(validation_results, "Invalid lengths")
  }

  if (check_regex && any(!grepl(regp, column_nonmissing))) {
    validation_results <- c(validation_results, "Invalid formats")
  }

  if (check_unique && any(duplicated(column_nonmissing))) {
    validation_results <- c(validation_results, "Non-unique values")
  }

  return(validation_results)
}

validate_num_column <- function(column, col_config) {
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  check_min_val <- ifelse(is.null(col_config$minimum), FALSE, TRUE)
  minv <- col_config$minimum
  check_max_val <- ifelse(is.null(col_config$maximum), FALSE, TRUE)
  maxv <- col_config$maximum

  validation_results <- c()
  column_numeric <- suppressWarnings(as.numeric(column))

  if (check_required && any(is.na(column_numeric))) {
    validation_results <- c(validation_results, "Required")
  }

  if (check_min_val && any(column_numeric < minv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "Out of range values")
  } else if (check_max_val && any(column_numeric > maxv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "Out of range values")
  }

  return(validation_results)
}

validate_drop_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  if (col_config$htType == 'autocomplete'){
    val_options <- get_options(col_config$options)
  } else {
    val_options <- col_config$enum
  }

  validation_results <- c()

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (!all(column_nonmissing %in% val_options)) {
    validation_results <- c(validation_results, "Invalid options")
  }

  return(validation_results)
}

validate_cb_column <- function(column, col_config){
  # if required, we need at least one checkbox checked, else 0
  mincb <- ifelse(is.null(col_config$required), 0, ifelse(col_config$required, 1, 0))
  # if min_checks is given, we update mincb
  mincb <- ifelse(is.null(col_config$min_checks), mincb, col_config$min_checks)
  maxcb <- ifelse(is.null(col_config$max_checks), 10000, col_config$max_checks)

  validation_results <- c()

  true_count <- sum(column, na.rm = TRUE) # NA count a s FALSE

  if (true_count < mincb) {
    validation_results <- c(validation_results, "Too few checked")
  } else if (true_count > maxcb) {
    validation_results <- c(validation_results, "Too many checked")
  }

  return(validation_results)
}

validate_date_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)

  validation_results <- c()

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]
  column_date <- as.Date(column_nonmissing, format = "%Y-%m-%d")

  if (any(is.na(column_date))) {
    validation_results <- c(validation_results, "Invalid date formats")
  }

  return(validation_results)
}

validate_column <- function(column, col_config){
  switch(col_config$htType,
         "character" = validate_char_column(column, col_config),
         "text" = validate_char_column(column, col_config),
         "numeric" = validate_num_column(column, col_config),
         "dropdown" = validate_drop_column(column, col_config),
         "autocomplete" = validate_drop_column(column, col_config),
         "checkbox" = validate_cb_column(column, col_config),
         "date" = validate_date_column(column, col_config),
         stop("Unknown column type")
  )
}


# collect results from dataframes (output of rhandsontables)
collect_hot_val_results <- function(df, tbl_props){
  results <- sapply(
    colnames(df),
    function(col_name) {
      val_check <- validate_column(df[[col_name]], tbl_props[[col_name]])
      if (length(val_check) > 0) {
        list(
          field = tbl_props[[col_name]]$title,
          type = 'error',
          message = paste(val_check, collapse = ', ')
        )
      } else {NULL}
    }, simplify = FALSE, USE.NAMES = TRUE)
  return(results)
}

# collect results from shiny input validators
collect_validator_results <- function(iv_validated, input_field_names, ns_prefix){
  results <- list()
  for (val_item in names(iv_validated)){
    if (!is.null(iv_validated[[val_item]])){
      item_name <- gsub(paste0(ns_prefix,'-'),'',val_item) # undo the namespace
      results[[item_name]] <- list(
        field = input_field_names[[item_name]],
        type = iv_validated[[val_item]]$type,
        message = iv_validated[[val_item]]$message
      )
    }
  }
  return(results)
}
