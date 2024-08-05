library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)
source("R/json_utils.R")

#' Generate TS Dataset using clintrialx (Version 2)
#'
#' This function generates the TS dataset using clintrialx and other dependencies.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame representing the TS dataset.
#' @examples
#' generate_TS_dataset_CTx_v2("STUDY123", 5, "NCT00000000")
#' @export
generate_TS_dataset_CTx_v2 <- function(study_id, num_rows, nct_ids) {
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  data <- read.xlsx(file_path)
  study_info <- get_study_info(nct_ids)
  
  return(data)
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

# Function to extract specific information from complex fields
extract_info <- function(data, pattern) {
  match <- regmatches(data, regexpr(pattern, data, perl = TRUE))
  return(ifelse(length(match) > 0, match, NA))
}

# Function to preprocess clintrialx data
preprocess_clintrialx_data <- function(trial_df) {
  fields_to_split <- c("Interventions", "Primary Outcome Measures", "Secondary Outcome Measures", 
                       "Study Design", "Locations", "Collaborators", "Study Documents")
  for (field in fields_to_split) {
    if (field %in% colnames(trial_df)) {
      trial_df[[field]] <- strsplit(as.character(trial_df[[field]]), "|", fixed = TRUE)
    }
  }
  return(trial_df)
}

# Function to clean TSVAL values
clean_tsval <- function(tsval) {
  if (is.na(tsval)) return(NA)
  tsval <- gsub("Allocation: ", "", tsval)
  tsval <- gsub("Intervention Model: ", "", tsval)
  tsval <- gsub("Masking: ", "", tsval)
  tsval <- gsub("Primary Purpose: ", "", tsval)
  tsval <- gsub("DRUG: ", "", tsval)
  tsval <- gsub(" ; ", "; ", tsval) # Remove spaces before semicolons
  tsval <- gsub("^\\s+|\\s+$", "", tsval) # Trim leading/trailing whitespace
  tsval <- gsub("[;]+", ";", tsval) # Remove multiple semicolons
  tsval <- unique(unlist(strsplit(tsval, ";"))) # Remove duplicate values
  tsval <- paste(tsval, collapse = "; ")
  return(tsval)
}

create_ts_domain <- function(nct_ids, study_id, input_file) {
  # Read the TS summary file
  ts_summary <- read.xlsx(input_file, sheet = "TS")
  
  # Fetch the trial data from ClinicalTrials.gov using the provided NCT IDs
  trial_data <- ctg_get_nct(nct_ids, fields = c("NCT Number", "Study Title", "Study Status", "Sponsor", 
                                                "Conditions", "Phases", "Enrollment", "Start Date", 
                                                "Primary Completion Date", "Completion Date", "Brief Summary", 
                                                "Study Design", "Age", "Interventions", "Primary Outcome Measures", 
                                                "Secondary Outcome Measures", "Study Documents"))
  
  # Convert fetched data to a dataframe
  trial_df <- as.data.frame(trial_data)
  
  # Preprocess the clintrialx data for better readability
  trial_df <- preprocess_clintrialx_data(trial_df)
  
  # Print the preprocessed trial data to check its structure and contents
  # print("Preprocessed trial data from clintrialx:")
  # print(trial_df)
  
  # Define a mapping based on the dynamic extraction logic
  ts_mapping <- list(
    `NCTID` = "NCT Number",
    `TITLE` = "Study Title",
    `SPONSOR` = "Sponsor",
    `PHASE` = "Phases",
    `ENROLL` = "Enrollment",
    `SSTDTC` = "Start Date",
    `PCDTC` = "Primary Completion Date",
    `CMPDTC` = "Completion Date",
    `COND` = "Conditions",
    `STATUS` = "Study Status",
    `SUMM` = "Brief Summary",
    `OBJPRIM` = "Primary Outcome Measures",
    `OBJSEC` = "Secondary Outcome Measures",
    `PROTVERS` = "Study Documents",
    `PROTAMDT` = "Study Documents",
    `IPROD` = "Interventions",
    `CPROD` = "Interventions",
    `AGEMAX` = "Age",
    `AGEMIN` = "Age",
    `BLIND` = "Study Design",
    `PARALLEL` = "Study Design",
    `ALLOC` = "Study Design",
    `INTMODEL` = "Study Design",
    `MASK` = "Study Design",
    `PURPOSE` = "Study Design",
    `ACTSUB` = "Enrollment",
    `ADAPT` = "Study Design",
    `ADDON` = "Interventions",
    `COMPTRT` = "Interventions",
    `CRMINDUR` = NA,
    `CDISCUG` = NA,
    `CURTRT` = "Interventions",
    `DCUTDESC` = "Study Design",
    `DCUTDTC` = NA,
    `ECGBLIND` = "Study Design",
    `ECGCMON` = "Study Design",
    `ECGPRIM` = NA,
    `ECGSAME` = NA,
    `ECGRMETH` = NA,
    `ECGRBL` = NA,
    `ECGRTRT` = NA,
    `ECGTWALG` = NA,
    `EXTTIND` = "Study Design",
    `FCNTRY` = "Locations",
    `FDATSPEC` = NA,
    `HLTSUBJ` = "Study Design",
    `INTTYPE` = "Interventions",
    `LENGTH` = NA,
    `NARMS` = "Study Design",
    `NCOHORT` = "Study Design",
    `ONGOSIND` = "Study Status",
    `OUTMSEXP` = "Primary Outcome Measures",
    `OUTMSPRI` = "Primary Outcome Measures",
    `OUTMSSEC` = "Secondary Outcome Measures",
    `PCLAS` = "Interventions",
    `PEDPOST` = "Study Design",
    `PEDSTIND` = "Study Design",
    `PEDPIP` = "Study Design",
    `PLSUBJ` = "Enrollment",
    `RANDQ` = "Study Design",
    `RAREDIND` = "Conditions",
    `REGID` = "NCT Number",
    `RELCRIT` = "Study Design",
    `STABMIN` = "Study Design",
    `STENDTC` = "Completion Date",
    `SEX` = "Age",
    `SPREFID` = "Study Documents",
    `STOPRULES` = "Study Documents",
    `STRATFCT` = "Study Design",
    `STYPE` = "Study Type",
    `TRBLIND` = "Study Design",
    `CONTROL` = "Study Design",
    `DIAGGRP` = "Conditions",
    `THERAREA` = "Conditions",
    `TITLE` = "Study Title",
    `PHASE` = "Phases",
    `INVTHERAPY` = "Interventions",
    `TRTYPE` = "Study Type"
  )
  
  # Map the fetched data to TSVAL based on TSPARM labels
  for (i in 1:nrow(ts_summary)) {
    param <- ts_summary$TSPARMCD[i]
    tsval_field <- ts_mapping[[param]]
    
    if (!is.null(tsval_field) && tsval_field %in% colnames(trial_df)) {
      if (grepl("Date", param, ignore.case = TRUE)) {
        ts_summary$TSVAL[i] <- format_date_iso8601(trial_df[[tsval_field]])
      } else if (param == "PHASE") {
        ts_summary$TSVAL[i] <- convert_to_roman(trial_df[[tsval_field]])
      } else if (param == "PROTVERS") {
        ts_summary$TSVAL[i] <- extract_info(trial_df[[tsval_field]], "Protocol, ([^,]+)")
      } else if (param == "PROTAMDT") {
        ts_summary$TSVAL[i] <- extract_info(trial_df[[tsval_field]], "Amendment, ([^,]+)")
      } else if (param == "AGEMAX") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Age, "Maximum Age: (\\d+)")
      } else if (param == "AGEMIN") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Age, "Minimum Age: (\\d+)")
      } else if (param == "OBJPRIM") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Primary Outcome Measures`), collapse = "; ")
      } else if (param == "OBJSEC") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Secondary Outcome Measures`), collapse = ";")
      } else {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df[[tsval_field]]), collapse = "; ")
      }
    } else {
      # Handling complex fields not directly mapped
      if (param == "BLIND") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Masking: ([^|]+)")
      } else if (param == "PARALLEL") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Intervention Model: ([^|]+)")
      } else if (param == "ALLOC") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Allocation: ([^|]+)")
      } else if (param == "INTMODEL") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Intervention Model: ([^|]+)")
      } else if (param == "MASK") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Masking: ([^|]+)")
      } else if (param == "PURPOSE") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Primary Purpose: ([^|]+)")
      } else if (param == "OUTMSEXP") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Primary Outcome Measures`), collapse = "; ")
      } else if (param == "OUTMSPRI") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Primary Outcome Measures`), collapse = "; ")
      } else if (param == "OUTMSSEC") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Secondary Outcome Measures`), collapse = "; ")
      } else if (param == "PCLAS") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Interventions, "Drug: ([^|]+)")
      } else if (param == "PEDPOST") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Pediatric Postmarket: ([^|]+)")
      } else if (param == "PEDSTIND") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Pediatric Study: ([^|]+)")
      } else if (param == "PEDPIP") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Pediatric Investigation Plan: ([^|]+)")
      } else if (param == "RANDQ") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Randomization: ([^|]+)")
      } else if (param == "RAREDIND") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Conditions, "Rare Disease: ([^|]+)")
      } else if (param == "RELCRIT") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Relapse Criteria: ([^|]+)")
      } else if (param == "STABMIN") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Stable Disease Minimum Duration: ([^|]+)")
      } else if (param == "SEX") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Age, "Sex: ([^|]+)")
      } else if (param == "SPREFID") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Documents`, "Sponsor's Study Reference ID: ([^|]+)")
      } else if (param == "STOPRULES") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Documents`, "Study Stop Rules: ([^|]+)")
      } else if (param == "STRATFCT") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Stratification Factor: ([^|]+)")
      } else if (param == "TRBLIND") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Trial Blinding Scheme: ([^|]+)")
      } else if (param == "CONTROL") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Control Type: ([^|]+)")
      } else if (param == "DIAGGRP") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Conditions, "Diagnosis Group: ([^|]+)")
      } else if (param == "THERAREA") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Conditions, "Therapeutic Area: ([^|]+)")
      } else if (param == "INVTHERAPY") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Interventions, "Investigational Therapy: ([^|]+)")
      } else if (param == "TRTYPE") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Type`, "Trial Type: ([^|]+)")
      } else {
        ts_summary$TSVAL[i] <- NA  # No direct mapping available for other parameters
      }
    }
    # Clean the TSVAL values to remove redundant text and duplicates
    ts_summary$TSVAL[i] <- clean_tsval(ts_summary$TSVAL[i])
    print(paste("TSVAL for", param, ":", ts_summary$TSVAL[i]))
  }
  
  # Fill in the STUDYID and DOMAIN columns
  ts_summary$STUDYID <- study_id
  ts_summary$DOMAIN <- "TS"
  
  # Save the final output as a data frame
  final_df <- ts_summary
  assign("trial_df", trial_df, envir = .GlobalEnv)
  
  # Write the output to an Excel file
  output_file <- paste0(study_id, "_TS.xlsx")
  write.xlsx(final_df, output_file)
  
  # Print the generated TS domain
  # print("Generated TS domain:")
  # print(final_df)
  
  return(final_df)
}

# Example usage
# nct_ids <- c("NCT05112965")
# study_id <- "YO42713"
# input_file <- "Trial_Summary.xlsx"
# final_df <- create_ts_domain(nct_ids, study_id, input_file)
# print(paste("Output written to:", paste0(study_id, "_TS.xlsx")))
