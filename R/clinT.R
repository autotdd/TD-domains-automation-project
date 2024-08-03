library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(openxlsx)

source("R/json_utils.R")

#' Clinical Trials Function
#'
#' This function performs clinical trials data processing.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame with the processed clinical trials data.
#' @examples
#' clinT("NCT00000000")
#' @export
clinT <- function(nct_ids) {
  data <- get_study_info(nct_ids)
  # Further processing of data if needed
  return(data)
}

# Helper function to convert JSON data to a data frame
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

# Helper function to view JSON structure
view_json_structure <- function(json_data) {
  print(names(json_data))
  print(str(json_data))
}

# Example usage of the clinT function (this part should not be included in the package source code)
# nctId <- "NCT05112965"  # Replace with the desired NCT ID
# study_info <- clinT(nctId)
# view_json_structure(study_info)  # View the structure of the JSON data
# study_df <- study_info_to_df(study_info)
# 
# print(study_df)
# 
# # Display the structure of the dataframe
# str(study_df)
# 
# # Display summary statistics of the dataframe
# summary(study_df)
# 
# # Display the first few rows of the dataframe
# print(study_df)
