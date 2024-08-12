#' Format Dates in ISO 8601 Format
#'
#' This function formats a given date into the ISO 8601 format (YYYY-MM-DD).
#'
#' @param date A character string representing a date.
#' @return A character string formatted as YYYY-MM-DD or NA if input is invalid.
#' @export
format_date_iso8601 <- function(date) {
  if (!is.na(date) && !is.null(date)) {
    return(as.character(as.Date(date, format = "%Y-%m-%d")))
  } else {
    return(NA)
  }
}

#' Convert Numeric Phase to Roman Numeral
#'
#' This function converts a given phase from numeric format to Roman numeral format.
#'
#' @param phase A character string representing the phase.
#' @return A character string with the phase converted to Roman numeral.
#' @export
convert_to_roman <- function(phase) {
  phase <- toupper(phase)
  roman_phases <- c("PHASE1" = "Phase I", "PHASE2" = "Phase II", "PHASE3" = "Phase III", "PHASE4" = "Phase IV")
  if (is.vector(phase)) {
    return(paste(sapply(phase, function(p) ifelse(p %in% names(roman_phases), roman_phases[p], p)), collapse = " / "))
  } else {
    return(ifelse(phase %in% names(roman_phases), roman_phases[phase], phase))
  }
}

#' Clean TSVAL Values
#'
#' This function cleans TSVAL values by removing redundant text, trimming whitespace, and eliminating duplicates.
#'
#' @param tsval A character string representing TSVAL.
#' @return A cleaned character string with unique TSVAL values.
#' @export
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

#' Calculate Trial Length in ISO 8601 Format
#'
#' This function calculates the length of a trial and returns it in ISO 8601 format.
#'
#' @param start_date A character string representing the start date (YYYY-MM-DD).
#' @param end_date A character string representing the end date (YYYY-MM-DD).
#' @return A character string representing the trial length in ISO 8601 format or NA if input is invalid.
#' @export
calculate_trial_length <- function(start_date, end_date) {
  if (length(start_date) == 1 && length(end_date) == 1 && !is.na(start_date) && !is.na(end_date)) {
    start <- as.Date(start_date, format = "%Y-%m-%d")
    end <- as.Date(end_date, format = "%Y-%m-%d")
    length <- as.numeric(difftime(end, start, units = "days"))
    years <- floor(length / 365)
    days <- length %% 365
    iso_length <- paste0("P", ifelse(years > 0, paste0(years, "Y"), ""), ifelse(days > 0, paste0(days, "D"), ""))
    return(iso_length)
  } else {
    return(NA)
  }
}

#' Format Age in ISO 8601 Format
#'
#' This function formats a given age into ISO 8601 format.
#'
#' @param age A character string representing age.
#' @return A character string representing age in ISO 8601 format or NA if input is invalid.
#' @export
format_age_iso8601 <- function(age) {
  if (!is.na(age) && !is.null(age) && length(age) > 0) {
    if (grepl("P\\d+Y", age, ignore.case = TRUE)) {
      return(age)
    } else if (grepl("Year|Years", age, ignore.case = TRUE)) {
      years <- as.numeric(gsub("[^0-9]", "", age))
      return(paste0("P", years, "Y"))
    } else if (grepl("Month|Months", age, ignore.case = TRUE)) {
      months <- as.numeric(gsub("[^0-9]", "", age))
      return(paste0("P", months, "M"))
    } else if (grepl("Day|Days", age, ignore.case = TRUE)) {
      days <- as.numeric(gsub("[^0-9]", "", age))
      return(paste0("P", days, "D"))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

#' Convert JSON Study Information to DataFrame
#'
#' This function converts study information from JSON format to a data frame.
#'
#' @param json_data A list containing study information in JSON format.
#' @return A data frame containing study information.
#' @export
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
  
  number_of_arms <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$armGroups)) {
      nrow(protocol$armsInterventionsModule$armGroups)
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  comparative_treatment <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$armGroups)) {
      comp_trt <- dplyr::filter(protocol$armsInterventionsModule$armGroups, type == "ACTIVE_COMPARATOR") %>%
        dplyr::select(interventionNames) %>%
        unlist() %>%
        unique() %>%
        paste(collapse = "; ")
      if (comp_trt == "") NA else comp_trt
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  current_therapy <- tryCatch({
    if (!is.null(protocol$armsInterventionsModule$interventions)) {
      curr_trt <- dplyr::select(protocol$armsInterventionsModule$interventions, name) %>%
        unlist() %>%
        unique() %>%
        paste(collapse = "; ")
      if (curr_trt == "") NA else curr_trt
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  unique_countries <- tryCatch({
    if (!is.null(protocol$contactsLocationsModule$locations)) {
      unique(protocol$contactsLocationsModule$locations$country) %>%
        paste(collapse = "; ")
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  phases <- tryCatch({
    if (!is.null(protocol$designModule$phases)) {
      convert_to_roman(protocol$designModule$phases)
    } else {
      NA
    }
  }, error = function(e) { NA })
  
  df <- data.frame(
    nctId = if (!is.null(protocol$identificationModule$nctId)) protocol$identificationModule$nctId else NA,
    briefTitle = if (!is.null(protocol$identificationModule$briefTitle)) protocol$identificationModule$briefTitle else NA,
    officialTitle = if (!is.null(protocol$identificationModule$officialTitle)) protocol$identificationModule$officialTitle else NA,
    overallStatus = if (!is.null(protocol$statusModule$overallStatus)) protocol$statusModule$overallStatus else NA,
    startDate = if (!is.null(protocol$statusModule$startDateStruct$date)) format_date_iso8601(protocol$statusModule$startDateStruct$date) else NA,
    completionDate = if (!is.null(protocol$statusModule$completionDateStruct$date)) format_date_iso8601(protocol$statusModule$completionDateStruct$date) else NA,
    studyType = if (!is.null(protocol$designModule$studyType)) protocol$designModule$studyType else NA,
    phase = phases,
    primaryOutcomeMeasure = primary_outcome_measure,
    primaryOutcomeTimeFrame = primary_outcome_timeframe,
    secondaryOutcomeMeasure = secondary_outcome_measure,
    secondaryOutcomeTimeFrame = secondary_outcome_timeframe,
    enrollment = if (!is.null(protocol$designModule$enrollmentInfo$count)) protocol$designModule$enrollmentInfo$count else NA,
    condition = if (!is.null(protocol$conditionsModule$conditions)) protocol$conditionsModule$conditions else NA,
    maximumAge = if (!is.null(protocol$eligibilityModule$maximumAge)) format_age_iso8601(protocol$eligibilityModule$maximumAge) else NA,
    minimumAge = if (!is.null(protocol$eligibilityModule$minimumAge)) format_age_iso8601(protocol$eligibilityModule$minimumAge) else NA,
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

#' Create TS Domain
#'
#' This function creates the TS domain by fetching and processing trial data from ClinicalTrials.gov,
#' mapping the data to appropriate TS parameters, and saving the results as an Excel file.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @param study_id A character string representing the study ID.
#' @return A data frame containing the TS domain data.
#' @export
create_ts_domain <- function(nct_ids, study_id) {
  # Locate the TS summary file within the package
  input_file <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  # Read the TS summary file
  ts_summary <- openxlsx::read.xlsx(input_file, sheet = "TS")
  
  # Fetch and process trial data for each NCT ID
  trial_data <- lapply(nct_ids, function(nct_id) {
    study_info <- get_study_info(nct_id)
    
    # Save the JSON file with the study name prefix
    json_file_path <- paste0("json/", study_id, "_", nct_id, ".json")
    jsonlite::write_json(study_info, path = json_file_path)
    
    study_info_to_df(study_info)
  })
  
  # Combine all fetched trial data into a single data frame
  trial_df <- dplyr::bind_rows(trial_data)
  
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
    
    print(paste("Processing TSPARMCD:", param))  # Debug statement
    
    if (!is.null(tsval_field) && tsval_field %in% colnames(trial_df)) {
      tsval <- trial_df[[tsval_field]]
      print(paste("Initial tsval:", tsval))  # Debug statement
      if (length(tsval) == 0 || all(is.na(tsval))) {
        tsval <- NA
      }
      if (grepl("Date", param, ignore.case = TRUE)) {
        ts_summary$TSVAL[i] <- format_date_iso8601(tsval)
      } else if (param == "TPHASE") {
        ts_summary$TSVAL[i] <- convert_to_roman(tsval)
      } else if (param %in% c("PROTVERS", "PROTAMDT", "OBJPRIM", "OBJSEC", "OUTMSEXP", "OUTMSPRI", "OUTMSSEC")) {
        ts_summary$TSVAL[i] <- tsval
      } else if (param == "LENGTH") {
        ts_summary$TSVAL[i] <- calculate_trial_length(trial_df$startDate[1], trial_df$completionDate[1])
      } else if (param %in% c("AGEMAX", "AGEMIN")) {
        if (!is.null(tsval) && length(tsval) > 0 && !is.na(tsval)) {
          print(paste("Formatting age for:", tsval))  # Debug statement
          ts_summary$TSVAL[i] <- format_age_iso8601(tsval)
        } else {
          ts_summary$TSVAL[i] <- NA
        }
      } else {
        ts_summary$TSVAL[i] <- paste(unique(unlist(tsval)), collapse = "; ")
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
  output_file <- paste0("output/", study_id, "_TS.xlsx")
  openxlsx::write.xlsx(final_df, output_file)
  
  # Print the generated TS domain
  print("Generated TS domain:")
  print(final_df)
  
  return(final_df)
}

# Example usage:
# nct_ids <- c("NCT05789082")
# study_id <- "BO44426"
# final_df <- create_ts_domain(nct_ids, study_id)
# print(paste("Output written to:", paste0(study_id, "_TS.xlsx")))
