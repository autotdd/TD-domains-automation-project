#' Save JSON data to a file
#'
#' This function saves JSON data to a specified file within the package directory.
#'
#' @param data A list or data frame to be saved as JSON.
#' @param filename A character string representing the name of the JSON file.
#' @return NULL
#' @examples
#' \dontrun{
#' save_json(data, "example.json")
#' }
#' @export
save_json <- function(data, filename) {
  json_dir <- system.file("json", package = "autoTDD")
  if (json_dir == "") {
    json_dir <- file.path(getwd(), "inst/json")
  }
  if (!dir.exists(json_dir)) {
    dir.create(json_dir, recursive = TRUE)
  }
  json_file <- file.path(json_dir, filename)
  write(jsonlite::toJSON(data, pretty = TRUE), json_file)
}

#' Load JSON data from a file
#'
#' This function loads JSON data from a specified file within the package directory.
#'
#' @param filename A character string representing the name of the JSON file.
#' @return A list or data frame containing the loaded JSON data.
#' @examples
#' \dontrun{
#' data <- load_json("example.json")
#' }
#' @export
load_json <- function(filename) {
  json_dir <- system.file("json", package = "autoTDD")
  if (json_dir == "") {
    json_dir <- file.path(getwd(), "inst/json")
  }
  json_file <- file.path(json_dir, filename)
  if (!file.exists(json_file)) {
    stop("File does not exist: ", json_file)
  }
  data <- jsonlite::fromJSON(json_file)
  return(data)
}

#' Fetch study information from ClinicalTrials.gov
#'
#' This function fetches study information from ClinicalTrials.gov for the given NCT IDs.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame containing the study information.
#' @examples
#' \dontrun{
#' data <- get_study_info("NCT12345678")
#' }
#' @export
get_study_info <- function(nct_ids) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  url <- paste0(base_url, nct_ids)
  
  response <- httr::GET(url)
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data from ClinicalTrials.gov API")
  }
  
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(content)
  
  # Save JSON data
  filename <- paste0("study_info_", nct_ids, ".json")
  save_json(json_data, filename)
  
  # Load JSON data back into a data frame
  data <- load_json(filename)
  
  return(data)
}
