#' Internal function to process TS codelists
#'
#' This function fetches TS codelists from the CDISC Library API,
#' processes them, and returns a data frame with the formatted information.
#'
#' @param api_key Character string containing the CDISC Library API key
#' @param study_start_date Date string representing the study start date
#' @return A data frame containing processed TS codelist information
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle setColWidths setRowHeights saveWorkbook
#' @noRd
process_ts_codelists <- function(api_key, study_start_date) {
  base_url <- "https://library.cdisc.org/api"
  
  make_api_request <- function(endpoint) {
    url <- paste0(base_url, endpoint)
    response <- httr::GET(url, httr::add_headers("api-key" = api_key))
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text")
      return(jsonlite::fromJSON(content))
    } else {
      stop("Error: ", httr::status_code(response), " - ", substr(httr::content(response, "text"), 1, 200))
    }
  }

  # Determine the correct CT version based on study start date
  get_ct_version <- function(date) {
    year <- format(as.Date(date), "%Y")
    quarter <- ceiling(as.numeric(format(as.Date(date), "%m")) / 3)
    return(paste0("CT", year, " Q", quarter))
  }

  ct_version <- get_ct_version(study_start_date)

  # Fetch CT packages and find the correct version
  packages <- make_api_request("/mdr/ct/packages")
  packages_df <- packages$`_links`$packages
  target_package <- packages_df[grepl(ct_version, packages_df$title), ]
  
  if (nrow(target_package) == 0) {
    stop("Error: Could not find CT package for version ", ct_version)
  }

  # Extract TS-related controlled terminology
  ct_content <- make_api_request(target_package$href[1])
  ts_codelists <- ct_content$codelists[grepl("^TS", ct_content$codelists$name) | 
                                       grepl("TRIAL", toupper(ct_content$codelists$name)), ]
  
  # Flatten the terms from the codelists
  flatten_terms <- function(codelist) {
    terms <- codelist$terms
    data.frame(
      TSPARMCD = codelist$name,
      TSVAL = sapply(terms, function(term) term$submissionValue),
      TSVALCD = sapply(terms, function(term) term$conceptId),
      stringsAsFactors = FALSE
    )
  }
  
  flattened_terms <- do.call(rbind, lapply(ts_codelists, flatten_terms))
  
  return(flattened_terms)
}