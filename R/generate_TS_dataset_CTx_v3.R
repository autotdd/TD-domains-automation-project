library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)
source("R/json_utils.R")

#' Generate TS Dataset using clintrialx (Version 3)
#'
#' This function generates the TS dataset using clintrialx and other dependencies.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame representing the TS dataset.
#' @examples
#' generate_TS_dataset_CTx_v3("STUDY123", 5, "NCT00000000")
#' @export
generate_TS_dataset_CTx_v3 <- function(study_id, num_rows, nct_ids) {
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

# Function to calculate trial length
calculate_trial_length <- function(start_date, end_date) {
  if (!is.na(start_date) && !is.na(end_date)) {
    start <- as.Date(start_date, format="%Y-%m-%d")
    end <- as.Date(end_date, format="%Y-%m-%d")
    length <- as.numeric(difftime(end, start, units = "days"))
    return(paste0("P", length, "D")) # ISO 8601 duration format
  } else {
    return(NA)
  }
}

# Function to fetch study information from ClinicalTrials.gov
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

# Function to convert JSON study information to a dataframe
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
  
  # Count the number of arms
  number_of_arms <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$armGroups)) {
      nrow(protocol$armsInterventionsModule$armGroups)
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  # Get comparative treatment information
  comparative_treatment <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$armGroups)) {
      comp_trt <- protocol$armsInterventionsModule$armGroups %>%
        filter(type == "ACTIVE_COMPARATOR") %>%
        select(interventionNames) %>%
        unlist() %>%
        unique() %>%
        paste(collapse = "; ")
      if (comp_trt == "") NA else comp_trt
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  # Get current therapy or treatment information
  current_therapy <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$interventions)) {
      curr_trt <- protocol$armsInterventionsModule$interventions %>%
        select(name) %>%
        unlist() %>%
        unique() %>%
        paste(collapse = "; ")
      if (curr_trt == "") NA else curr_trt
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  # Get unique countries
  unique_countries <- tryCatch({
    if (!is.null(protocol$contactsLocationsModule$locations)) {
      unique(protocol$contactsLocationsModule$locations$country) %>%
        paste(collapse = "; ")
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
    phase = if (!is.null(protocol$designModule$phases)) protocol$designModule$phases else NA,
    primaryOutcomeMeasure = primary_outcome_measure,
    primaryOutcomeTimeFrame = primary_outcome_timeframe,
    secondaryOutcomeMeasure = secondary_outcome_measure,
    secondaryOutcomeTimeFrame = secondary_outcome_timeframe,
    enrollment = if (!is.null(protocol$designModule$enrollmentInfo$count)) protocol$designModule$enrollmentInfo$count else NA,
    condition = if (!is.null(protocol$conditionsModule$conditions)) protocol$conditionsModule$conditions else NA,
    maximumAge = if (!is.null(protocol$eligibilityModule$maximumAge)) protocol$eligibilityModule$maximumAge else NA,
    minimumAge = if (!is.null(protocol$eligibilityModule$minimumAge)) protocol$eligibilityModule$minimumAge else NA,
    sponsor = if (!is.null(protocol$sponsorCollaboratorsModule$leadSponsor$name)) protocol$sponsorCollaboratorsModule$leadSponsor$name else NA,
    interventionModel = if (!is.null(protocol$designModule$designInfo$interventionModel)) protocol$designModule$designInfo$interventionModel else NA,
    masking = if (!is.null(protocol$designModule$designInfo$maskingInfo$masking)) protocol$designModule$designInfo$maskingInfo$masking else NA,
    allocation = if (!is.null(protocol$designModule$designInfo$allocation)) protocol$designModule$designInfo$allocation else NA,
    primaryPurpose = if (!is.null(protocol$designModule$designInfo$primaryPurpose)) protocol$designModule$designInfo$primaryPurpose else NA,
    interventionNames = if (!is.null(protocol$armsInterventionsModule$interventions$name)) paste(protocol$armsInterventionsModule$interventions$name, collapse = "; ") else NA,
    trialLength = calculate_trial_length(protocol$statusModule$startDateStruct$date, protocol$statusModule$completionDateStruct$date),
    numberOfArms = number_of_arms,
    sex = if (!is.null(protocol$eligibilityModule$sex)) protocol$eligibilityModule$sex else NA,
    comparativeTreatment = comparative_treatment,
    currentTherapy = current_therapy,
    countries = unique_countries,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

create_ts_domain <- function(nct_ids, study_id, input_file) {
  # Read the TS summary file
  ts_summary <- read.xlsx(input_file, sheet = "TS")
  
  # Fetch and process trial data for each NCT ID
  trial_data <- lapply(nct_ids, function(nct_id) {
    study_info <- get_study_info(nct_id)
    
    # Save the JSON file with the study name prefix
    json_file_path <- file.path("/cloud/project/", paste0(study_id, "_", nct_id, ".json"))
    write_json(study_info, path = json_file_path, pretty = TRUE)
    
    study_info_to_df(study_info)
  })
  
  # Combine all fetched trial data into a single dataframe
  trial_df <- bind_rows(trial_data)
  
  # Print the processed trial data to check its structure and contents
  print("Processed trial data from ClinicalTrials.gov API:")
  print(trial_df)
  
  # Define a mapping based on the dynamic extraction logic
  ts_mapping <- list(
    `ACTSUB` = "enrollment",
    `ADAPT` = NA,  # No direct mapping available
    `ADDON` = NA,  # No direct mapping available
    `AGEMAX` = "maximumAge",
    `AGEMIN` = "minimumAge",
    `COMPTRT` = "comparativeTreatment",
    `CRMDUR` = NA,  # No direct mapping available
    `CTAUG` = NA,  # No direct mapping available
    `CURTRT` = "currentTherapy",
    `DCUTDESC` = NA,  # No direct mapping available
    `DCUTDTC` = NA,  # No direct mapping available
    `EGBLIND` = NA,  # No direct mapping available
    `EGCTMON` = NA,  # No direct mapping available
    `EGLEADPR` = NA,  # No direct mapping available
    `EGLEADSM` = NA,  # No direct mapping available
    `EGRDMETH` = NA,  # No direct mapping available
    `EGREPLBL` = NA,  # No direct mapping available
    `EGREPLTR` = NA,  # No direct mapping available
    `EGTWVALG` = NA,  # No direct mapping available
    `EXTTIND` = NA,  # No direct mapping available
    `FCNTRY` = "countries",
    `FDATCHSP` = NA,  # No direct mapping available
    `HLTSUBJI` = NA,  # No direct mapping available
    `INDIC` = "condition",
    `INTMODEL` = "interventionModel",
    `INTTYPE` = "studyType",
    `LENGTH` = "trialLength",
    `NARMS` = "numberOfArms",
    `NCOHORT` = NA,  # No direct mapping available
    `OBJPRIM` = "primaryOutcomeMeasure",
    `OBJSEC` = "secondaryOutcomeMeasure",
    `ONGOSIND` = "overallStatus",
    `OUTMSEXP` = NA,  # No direct mapping available
    `OUTMSPRI` = "primaryOutcomeMeasure",
    `OUTMSSEC` = "secondaryOutcomeMeasure",
    `PCLAS` = NA,  # No direct mapping available
    `PDPSTIND` = NA,  # No direct mapping available
    `PDSTIND` = NA,  # No direct mapping available
    `PIPIND` = NA,  # No direct mapping available
    `PLANSUB` = "enrollment",
    `RANDQT` = NA,  # No direct mapping available
    `RDIND` = "condition",
    `REGID` = "nctId",
    `RLPSCRIT` = NA,  # No direct mapping available
    `SDMDUR` = NA,  # No direct mapping available
    `SENDTC` = "completionDate",
    `SEXPOP` = "sex",
    `SPONSOR` = "sponsor",
    `SDTMVER` = NA,  # No direct mapping available
    `SDTIGVER` = NA,  # No direct mapping available
    `SPREFID` = "protocolSection.identificationModule.orgStudyIdInfo.id",
    `STOPRULE` = NA,  # No direct mapping available
    `STRATFCT` = NA,  # No direct mapping available
    `SSTDTC` = "startDate",
    `STYPE` = "studyType",
    `TBLIND` = "masking",
    `TCNTRL` = "allocation",
    `TDIGRP` = "condition",
    `THERAREA` = "condition",
    `TITLE` = "officialTitle", # Use official title for Trial Title
    `TPHASE` = "phase",
    `TRT` = "interventionNames",
    `TTYPE` = "studyType"
  )
  
  # Map the fetched data to TSVAL based on TSPARM labels
  for (i in 1:nrow(ts_summary)) {
    param <- ts_summary$TSPARMCD[i]
    tsval_field <- ts_mapping[[param]]
    
    if (!is.null(tsval_field) && tsval_field %in% colnames(trial_df)) {
      tsval <- trial_df[[tsval_field]]
      if (length(tsval) == 0 || all(is.na(tsval))) {
        tsval <- NA
      }
      if (grepl("Date", param, ignore.case = TRUE)) {
        ts_summary$TSVAL[i] <- format_date_iso8601(tsval)
      } else if (param == "TPHASE") {
        ts_summary$TSVAL[i] <- convert_to_roman(tsval)
      } else if (param %in% c("PROTVERS", "PROTAMDT", "AGEMAX", "AGEMIN", "OBJPRIM", "OBJSEC", "OUTMSEXP", "OUTMSPRI", "OUTMSSEC")) {
        ts_summary$TSVAL[i] <- tsval
      } else {
        ts_summary$TSVAL[i] <- paste(unlist(tsval), collapse = "; ")
      }
    } else {
      ts_summary$TSVAL[i] <- NA
    }
    
    # Clean the TSVAL values to remove redundant text and duplicates
    ts_summary$TSVAL[i] <- clean_tsval(ts_summary$TSVAL[i])
    if (is.na(ts_summary$TSVAL[i]) || ts_summary$TSVAL[i] == "") {
      warning(paste("TSVAL for", param, "is missing or empty"))
    } else {
      print(paste("TSVAL for", param, ":", ts_summary$TSVAL[i]))
    }
  }
  
  # Fill in the STUDYID and DOMAIN columns
  ts_summary$STUDYID <- study_id
  ts_summary$DOMAIN <- "TS"
  
  # Save the final output as a data frame
  final_df <- ts_summary
  
  # Write the output to an Excel file
  output_file <- paste0(study_id, "_TS.xlsx")
  write.xlsx(final_df, output_file)
  
  # Print the generated TS domain
  # print("Generated TS domain:")
  # print(final_df)
  
  return(final_df)
}

# Example usage
# nct_ids <- c("NCT05789082")
# study_id <- "BO44426"
# input_file <- "/cloud/project/TD-domains-automation-project/Trial_Summary.xlsx"
# final_df <- create_ts_domain(nct_ids, study_id, input_file)
# print(paste("Output written to:", paste0(study_id, "_TS.xlsx")))
