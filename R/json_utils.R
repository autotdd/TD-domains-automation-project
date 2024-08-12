# File: R/json_utils.R

#' Save JSON Data to File
#'
#' This function saves JSON data to a specified file within the package directory.
#'
#' @param data A list or data frame to be saved as JSON.
#' @param filename A character string representing the name of the JSON file.
#' @param dir A character string representing the directory to save the file. Defaults to "json".
#' @return NULL
#' @export
#' @importFrom jsonlite toJSON
#' @examples
#' \dontrun{
#' data <- list(name = "John Doe", age = 30)
#' save_json(data, "example.json")
#' }
save_json <- function(data, filename, dir = "json") {
  json_dir <- file.path(getwd(), dir)
  if (!dir.exists(json_dir)) {
    dir.create(json_dir, recursive = TRUE)
  }
  
  json_file <- file.path(json_dir, filename)
  write(toJSON(data, pretty = TRUE), json_file)
}

#' Load JSON Data from File
#'
#' This function loads JSON data from a specified file within the package directory.
#'
#' @param filename A character string representing the name of the JSON file.
#' @param dir A character string representing the directory where the file is located. Defaults to "json".
#' @return A list or data frame containing the loaded JSON data.
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' data <- load_json("example.json")
#' print(data)
#' }
load_json <- function(filename, dir = "json") {
  json_dir <- file.path(getwd(), dir)
  json_file <- file.path(json_dir, filename)
  if (!file.exists(json_file)) {
    stop("File does not exist: ", json_file)
  }
  data <- fromJSON(json_file)
  return(data)
}

#' Fetch Study Information from ClinicalTrials.gov
#'
#' This function fetches study information from ClinicalTrials.gov for the given NCT IDs.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @return A list containing the study information for each NCT ID.
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' study_info <- fetch_study_info(c("NCT00000419", "NCT00000102"))
#' print(names(study_info))
#' }
fetch_study_info <- function(nct_ids) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  
  study_info <- lapply(nct_ids, function(nct_id) {
    url <- paste0(base_url, nct_id)
    
    response <- GET(url)
    if (httr::status_code(response) != 200) {
      warning("Failed to fetch data for NCT ID: ", nct_id)
      return(NULL)
    }
    
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
    
    # Save JSON data
    filename <- paste0("study_info_", nct_id, ".json")
    save_json(json_data, filename)
    
    return(json_data)
  })
  
  names(study_info) <- nct_ids
  return(study_info)
}

#' Convert JSON Study Information to Data Frame
#'
#' This function converts study information from JSON format to a data frame.
#'
#' @param json_data A list containing study information in JSON format.
#' @return A data frame containing study information.
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' study_info <- fetch_study_info("NCT00000419")
#' df <- json_to_dataframe(study_info)
#' print(head(df))
#' }
json_to_dataframe <- function(json_data) {
  if (is.list(json_data) && !is.data.frame(json_data)) {
    df_list <- lapply(json_data, function(study) {
      protocol <- study$protocolSection
      
      data.frame(
        nctId = protocol$identificationModule$nctId,
        briefTitle = protocol$identificationModule$briefTitle,
        officialTitle = protocol$identificationModule$officialTitle,
        overallStatus = protocol$statusModule$overallStatus,
        startDate = protocol$statusModule$startDateStruct$date,
        completionDate = protocol$statusModule$completionDateStruct$date,
        studyType = protocol$designModule$studyType,
        phase = paste(protocol$designModule$phases, collapse = ", "),
        enrollment = protocol$designModule$enrollmentInfo$count,
        condition = paste(protocol$conditionsModule$conditions, collapse = ", "),
        stringsAsFactors = FALSE
      )
    })
    return(bind_rows(df_list))
  } else {
    stop("Input must be a list of JSON data")
  }
}