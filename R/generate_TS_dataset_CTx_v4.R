library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)
source("R/json_utils.R")

#' Generate TS Dataset using clintrialx (Version 4)
#'
#' This function generates the TS dataset using clintrialx and other dependencies.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame representing the TS dataset.
#' @examples
#' generate_TS_dataset_CTx_v4("STUDY123", 5, "NCT00000000")
#' @export
generate_TS_dataset_CTx_v4 <- function(study_id, num_rows, nct_ids) {
  # Path to the Trial_Summary.xlsx file within the package
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  # Check if the file exists before attempting to read it
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read the Excel file
  Trial_Summary <- read.xlsx(file_path)
  
  # Fetch study information using the utility function
  study_info <- get_study_info(nct_ids)
  
  # Generate the TS dataset 
  dataset <- data.frame(
    STUDYID = rep(study_id, num_rows),
    DOMAIN = rep("TS", num_rows),
    TSSEQ = integer(num_rows),
    TSGRPID = character(num_rows),
    TSPARMCD = character(num_rows),
    TSPARM = character(num_rows),
    TSVAL = character(num_rows),
    TSVALNF = character(num_rows),
    TSVALCD = character(num_rows),
    TSVCDREF = character(num_rows),
    TSVCDVER = character(num_rows),
    stringsAsFactors = FALSE
  )
  
  return(dataset)
}

# Function to format dates in ISO 8601 format
format_date_iso8601 <- function(date) {
  if (!is.na(date) && !is.null(date)) {
    return(as.character(as.Date(date, format="%Y-%m-%d")))
  } else {
    return(NA)
  }
}

# Function to convert numeric phase to Roman numeral
convert_to_roman <- function(phase) {
  phase <- toupper(phase)
  roman_phases <- c("PHASE1" = "Phase I", "PHASE2" = "Phase II", "PHASE3" = "Phase III", "PHASE4" = "Phase IV")
  return(ifelse(phase %in% names(roman_phases), roman_phases[phase], phase))
}

# Function to clean TSVAL values
clean_tsval <- function(tsval) {
  if (is.na(tsval)) return(NA)
  tsval <- gsub("Drug: |DRUG: |Allocation: |Intervention Model: |Masking: |Primary Purpose: ", "", tsval)
  tsval <- gsub(" ; ", "; ", tsval) # Remove spaces before semicolons
  tsval <- gsub("^\\s+|\\s+$", "", tsval) # Trim leading/trailing whitespace
  tsval <- gsub("[;]+", ";", tsval) # Remove multiple semicolons
  tsval <- unique(unlist(strsplit(tsval, ";"))) # Remove duplicate values
  tsval <- paste(tsval, collapse = "; ")
  return(tsval)
}

# Example usage
# nct_ids <- c("NCT05789082")
# study_id <- "BO44426"
# input_file <- "/cloud/project/TD-domains-automation-project/Trial_Summary.xlsx"
# final_df <- generate_TS_dataset_CTx_v4(nct_ids, study_id, input_file)
# print(paste("Output written to:", paste0(study_id, "_TS.xlsx")))
