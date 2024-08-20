#' Fetch Study Information
#'
#' This function fetches study information from ClinicalTrials.gov API.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @param debug Logical, if TRUE, print debug information. Default is FALSE.
#' @return A list containing study information for each NCT ID.
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
fetch_study_info <- function(nct_ids, debug = FALSE) {
  if(debug) cat("fetch_study_info called with NCT IDs:", paste(nct_ids, collapse = ", "), "\n")

  base_url <- "https://clinicaltrials.gov/api/v2/studies/"

  study_info <- lapply(nct_ids, function(nct_id) {
    if(debug) cat("Processing NCT ID:", nct_id, "\n")

    url <- paste0(base_url, nct_id)

    response <- httr::GET(url)
    if (httr::status_code(response) != 200) {
      warning("Failed to fetch data for NCT ID: ", nct_id)
      return(NULL)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(content)

    if(debug) {
      cat("API Response Structure for NCT ID:", nct_id, "\n")
      print(str(json_data, max.level = 2))
    }

    # Save JSON data
    filename <- paste0("study_info_", nct_id, ".json")
    save_json(json_data, filename)

    if(debug) cat("JSON data saved for NCT ID:", nct_id, "\n")

    return(json_data)
  })

  names(study_info) <- nct_ids

  if(debug) cat("Finished fetching study information for all NCT IDs\n")

  return(study_info)
}
