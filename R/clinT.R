library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(openxlsx)

#' Clinical Trials Function
#'
#' This function performs clinical trials data processing.
#'
#' @param param1 Description of the first parameter.
#' @param param2 Description of the second parameter.
#' @return A data frame with the processed clinical trials data.
#' @examples
#' clinT(param1, param2)
#' @export


get_study_info <- function(nctId) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  url <- paste0(base_url, nctId)
  
  response <- GET(url)
  
  if (status_code(response) != 200) {
    stop("Failed to fetch data from ClinicalTrials.gov API")
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content)
  
  return(json_data)
}

study_info_to_df <- function(json_data) {
  protocol <- json_data$protocolSection
  
  primary_outcome_measure <- tryCatch({
    if (!is.null(protocol$outcomesModule$primaryOutcomes) && length(protocol$outcomesModule$primaryOutcomes) > 0) {
      protocol$outcomesModule$primaryOutcomes[[1]]$measure
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  primary_outcome_timeframe <- tryCatch({
    if (!is.null(protocol$outcomesModule$primaryOutcomes) && length(protocol$outcomesModule$primaryOutcomes) > 0) {
      protocol$outcomesModule$primaryOutcomes[[1]]$timeFrame
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  secondary_outcome_measure <- tryCatch({
    if (!is.null(protocol$outcomesModule$secondaryOutcomes) && length(protocol$outcomesModule$secondaryOutcomes) > 0) {
      protocol$outcomesModule$secondaryOutcomes[[1]]$measure
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  secondary_outcome_timeframe <- tryCatch({
    if (!is.null(protocol$outcomesModule$secondaryOutcomes) && length(protocol$outcomesModule$secondaryOutcomes) > 0) {
      protocol$outcomesModule$secondaryOutcomes[[1]]$timeFrame
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  df <- data.frame(
    nctId = protocol$identificationModule$nctId,
    briefTitle = protocol$identificationModule$briefTitle,
    officialTitle = protocol$identificationModule$officialTitle,
    overallStatus = protocol$statusModule$overallStatus,
    startDate = protocol$statusModule$startDateStruct$date,
    completionDate = protocol$statusModule$completionDateStruct$date,
    studyType = protocol$designModule$studyType,
    phase = protocol$designModule$phases,
    primaryOutcomeMeasure = primary_outcome_measure,
    primaryOutcomeTimeFrame = primary_outcome_timeframe,
    secondaryOutcomeMeasure = secondary_outcome_measure,
    secondaryOutcomeTimeFrame = secondary_outcome_timeframe,
    enrollment = protocol$designModule$enrollmentInfo$count,
    condition = protocol$conditionsModule$conditions,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

view_json_structure <- function(json_data) {
  print(names(json_data))
  print(str(json_data))
}

nctId <- "NCT05112965"  # Replace with the desired NCT ID
study_info <- get_study_info(nctId)
view_json_structure(study_info)  # View the structure of the JSON data
study_df <- study_info_to_df(study_info)

print(study_df)

# Display the structure of the dataframe
str(study_df)

# Display summary statistics of the dataframe
summary(study_df)

# Display the first few rows of the dataframe
print(study_df)
