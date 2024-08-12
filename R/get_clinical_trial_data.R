# File: R/get_clinical_trial_data.R

#' Get Clinical Trial Data
#'
#' This function retrieves and processes clinical trials data from ClinicalTrials.gov.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame with the processed clinical trials data.
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' # Fetch data for a single trial
#' trial_data <- get_clinical_trial_data("NCT05112965")
#' print(trial_data)
#' 
#' # Fetch data for multiple trials
#' trial_data <- get_clinical_trial_data(c("NCT05112965", "NCT04647669"))
#' print(trial_data)
#' }
get_clinical_trial_data <- function(nct_ids) {
  data <- lapply(nct_ids, function(nct_id) {
    study_info <- get_study_info(nct_id)
    study_info_to_df(study_info)
  })
  
  # Combine all fetched trial data into a single data frame
  trial_df <- bind_rows(data)
  
  return(trial_df)
}

#' Fetch Study Information from ClinicalTrials.gov
#'
#' This function fetches study information from ClinicalTrials.gov for a given NCT ID.
#'
#' @param nct_id A character string representing the NCT ID.
#' @return A list containing the study information.
#' @keywords internal
get_study_info <- function(nct_id) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  url <- paste0(base_url, nct_id)
  
  response <- GET(url)
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data from ClinicalTrials.gov API for NCT ID: ", nct_id)
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content)
  
  return(json_data)
}

#' Convert JSON Study Information to Data Frame
#'
#' This function converts study information from JSON format to a data frame.
#'
#' @param json_data A list containing study information in JSON format.
#' @return A data frame containing study information.
#' @keywords internal
study_info_to_df <- function(json_data) {
  protocol <- json_data$protocolSection
  
  primary_outcome_measure <- tryCatch({
    if (!is.null(protocol$outcomesModule$primaryOutcomes) && length(protocol$outcomesModule$primaryOutcomes) > 0) {
      paste(protocol$outcomesModule$primaryOutcomes$measure, collapse = "; ")
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  primary_outcome_timeframe <- tryCatch({
    if (!is.null(protocol$outcomesModule$primaryOutcomes) && length(protocol$outcomesModule$primaryOutcomes) > 0) {
      paste(protocol$outcomesModule$primaryOutcomes$timeFrame, collapse = "; ")
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  secondary_outcome_measure <- tryCatch({
    if (!is.null(protocol$outcomesModule$secondaryOutcomes) && length(protocol$outcomesModule$secondaryOutcomes) > 0) {
      paste(protocol$outcomesModule$secondaryOutcomes$measure, collapse = "; ")
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  secondary_outcome_timeframe <- tryCatch({
    if (!is.null(protocol$outcomesModule$secondaryOutcomes) && length(protocol$outcomesModule$secondaryOutcomes) > 0) {
      paste(protocol$outcomesModule$secondaryOutcomes$timeFrame, collapse = "; ")
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  df <- data.frame(
    nctId = if (!is.null(protocol$identificationModule$nctId)) protocol$identificationModule$nctId else NA,
    briefTitle = if (!is.null(protocol$identificationModule$briefTitle)) protocol$identificationModule$briefTitle else NA,
    officialTitle = if (!is.null(protocol$identificationModule$officialTitle)) protocol$identificationModule$officialTitle else NA,
    overallStatus = if (!is.null(protocol$statusModule$overallStatus)) protocol$statusModule$overallStatus else NA,
    startDate = if (!is.null(protocol$statusModule$startDateStruct$date)) protocol$statusModule$startDateStruct$date else NA,
    completionDate = if (!is.null(protocol$statusModule$completionDateStruct$date)) protocol$statusModule$completionDateStruct$date else NA,
    studyType = if (!is.null(protocol$designModule$studyType)) protocol$designModule$studyType else NA,
    phase = if (!is.null(protocol$designModule$phases)) paste(protocol$designModule$phases, collapse = "; ") else NA,
    primaryOutcomeMeasure = primary_outcome_measure,
    primaryOutcomeTimeFrame = primary_outcome_timeframe,
    secondaryOutcomeMeasure = secondary_outcome_measure,
    secondaryOutcomeTimeFrame = secondary_outcome_timeframe,
    enrollment = if (!is.null(protocol$designModule$enrollmentInfo$count)) protocol$designModule$enrollmentInfo$count else NA,
    condition = if (!is.null(protocol$conditionsModule$conditions)) paste(protocol$conditionsModule$conditions, collapse = "; ") else NA,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

#' View JSON Structure
#'
#' This function prints the structure of a JSON object for debugging purposes.
#'
#' @param json_data A list representing a JSON object.
#' @keywords internal
view_json_structure <- function(json_data) {
  print(names(json_data))
  print(str(json_data))
}