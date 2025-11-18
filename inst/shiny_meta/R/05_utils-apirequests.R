## DATASET SERVER: API reqeusts -----

# helper to create a numbered list of authors for modal dialog with checkboxes
get_author_choices <- function(author_df){
  cb_names <- paste0("Author Nr. ", author_df$author_nr, ": ",
                     author_df$last_name, ", ",
                     author_df$first_name)
  cb_names <- gsub(": ,", ": [last name],", cb_names)
  cb_names <- gsub(", $", ", [first name]", cb_names)
  cb_vals <- rownames(author_df)

  return(stats::setNames(cb_vals, cb_names))
}

get_funding_choices <- function(funding_df){
  cb_names <- paste0("Funding Inst. Nr. ", rownames(funding_df))
  cb_vals <- rownames(funding_df)

  return(stats::setNames(cb_vals, cb_names))
}

ror_api_request <- function(search_string, country_code){
  search_url <- sprintf(
    'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
    URLencode(search_string), country_code)
  ror_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(ror_res) == 200) {
    ror_data <- jsonlite::fromJSON(rawToChar(ror_res$content))

    if (ror_data$number_of_results > 0) {
      # get the names (assuming that there is always exactly one ror_display name)
      res_names <- ror_data$items$names %>%
        dplyr::bind_rows() %>%
        dplyr::filter(grepl('ror_display', types)) %>%
        dplyr::rename(Name = value) %>%
        dplyr::select(Name)

      # get the locations
      res_locs <- ror_data$items$locations %>%
        dplyr::bind_rows() %>%
        dplyr::pull(geonames_details) %>%
        tidyr::unite(col = 'Location', name, country_name, sep = ', ', remove = FALSE) %>%
        dplyr::rename(city = name) %>%
        dplyr::select(Location, country_code, city)

      res_df <- cbind(res_names, res_locs)

      # get the ror ids and corresponding hyperlinks
      res_df <- res_df %>%
        dplyr::mutate(
          RORID = gsub('https://ror.org/', '', ror_data$items$id),
          Link = paste0("<a href='",ror_data$items$id,"' target='_blank'>",ror_data$items$id,"</a>")
        )
      return(res_df)

    } else {
      showNotification("No ROR results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ROR API request failed. Try again.", type = "error")
    return(NULL)
  }
}



orcid_api_request <- function(search_string = NULL, last_name = NULL, first_name = NULL){
  # EITHER query by search string
  if (!is.null(search_string)) {
    # query either orcid or by names, depending on the format of the search_string
    if (grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", search_string)) {
      # query by orcid
      query <- sprintf('?q=orcid:%s',search_string)
    } else {
      # query by names
      search_terms <- URLencode(gsub(" ", "+AND+", search_string))
      query <- sprintf('?q=given-and-family-names:(%s)+OR+other-names:(%s)',
                       search_terms, search_terms)
    }

    # OR query by first and last names explicitly
  } else if (!(is.null(last_name) || last_name == "" || is.na(last_name)) ||
             !(is.null(first_name) || first_name == "" || is.na(first_name))) {

    query_ln <- ifelse(!(is.null(last_name) || last_name == "" || is.na(last_name)),
                       sprintf('(family-name:(%s))', URLencode(gsub(" ", "+AND+", last_name))),
                       "")
    query_fn <- ifelse(!(is.null(first_name) || first_name == "" || is.na(first_name)),
                       sprintf('(given-names:(%s))', URLencode(gsub(" ", "+AND+", first_name))),
                       "")
    query <- paste0('?q=', query_ln, ifelse(nchar(query_ln)>0&&nchar(query_fn)>0, '+AND+', ""), query_fn)

  } else {
    # no valid search terms provided, abort
    # showNotification("Could not generate query. Provide either search_string or last_name, first_name arguments.", type = "error")
    return(NULL)
  }

  search_url <- paste0(
    'https://pub.orcid.org/v3.0/csv-search/', query,
    '&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names', # the fields we want
    '&rows=50') # limit to 50 results

  # GET request
  orcid_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(orcid_res) == 200) {
    orcid_data <- read.table(text = rawToChar(orcid_res$content),
                             sep =",", header = TRUE,
                             stringsAsFactors = FALSE, allowEscapes = TRUE)

    if (nrow(orcid_data) > 0) {
      orcid_data <- orcid_data %>%
        dplyr::rename(
          last_name = 'family.name',
          first_name = 'given.names',
          orcid_id = 'orcid',
          org_name = 'current.institution.affiliation.name',
          other_names = 'other.names') %>%
        # only use the first entry for email and affiliation
        tidyr::separate(email, into = c("email"), sep = ",(?!\\s)", extra = "drop") %>%
        tidyr::separate(org_name, into = c("org_name"), sep = ",(?!\\s)", extra = "drop") %>%
        dplyr::mutate(
          # create orcid hyperlinks
          orcid = paste0("<a href='https://orcid.org/", orcid_id, "' target='_blank'>",orcid_id,"</a>"))

      return(orcid_data)

    } else {
      showNotification("No ORCID results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ORCID API request failed. Try again.", type = "error")
    return(NULL)
  }

}

doi_api_request <- function(doi_search_string) {
  # cf. https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
  doi_pattern <- "10[.][0-9]{4,9}(?:[.][0-9]+)*/[-._;:/()\\[\\]<>A-Za-z0-9]+"
  clean_doi <- stringr::str_extract(doi_search_string, doi_pattern)

  safe_block({
    if (is.na(clean_doi)) {
      warning("Invalid DOI format. Please check the DOI.")
      return(NULL)
    }
    search_url <- sprintf(
      'https://citation.doi.org/format?doi=%s&style=apa&lang=en-US',
      URLencode(clean_doi)
    )
    doi_res <- httr::GET(search_url, httr::timeout(5))
    if (httr::status_code(doi_res) != 200) {
      warning("DOI API request failed with status code: ", httr::status_code(doi_res))
      return(NULL)
    }
    doi_content <- trimws(httr::content(doi_res, as = "text", encoding = "UTF-8"))

    tibble::tibble(doi = clean_doi, citation = doi_content)
  },
  err_title = "DOI API Request Error", err_message = "An error occurred while resolving the DOI:", propagate_err = FALSE
  )
}

cr_api_request <- function(search_string){
  query <- trimws(search_string)

  safe_block({
    if (query == "") {
      warning("Empty search string provided. Please check query input.")
      return(NULL)
    }

    cr_works <- rcrossref::cr_works(flq = c(query.bibliographic = query), limit = 3)

    if (cr_works$meta$total_results == 0) {
      warning("No CrossRef results found. Try again.")
      return(NULL)
    }

    cr_citations <- rcrossref::cr_cn(cr_works$data$doi, format = "text", style = "apa")

    tibble::tibble(
      doi = cr_works$data$doi,
      citation = unlist(cr_citations)
    )
  },
  err_title = "Crossref API Request Error", err_message = "An error occurred while querying Crossref:", propagate_err = FALSE
  )

}
