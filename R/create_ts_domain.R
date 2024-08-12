# File: R/create_ts_domain.R

#' Create TS Domain
#'
#' This function creates the TS (Trial Summary) domain by fetching and processing 
#' trial data from ClinicalTrials.gov, mapping the data to appropriate TS parameters, 
#' and saving the results as an Excel file.
#'
#' @param nct_ids A character vector of NCT IDs.
#' @param study_id A character string representing the study ID.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A data frame containing the TS domain data.
#' @export
#' @importFrom openxlsx read.xlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @importFrom dplyr bind_rows left_join select distinct
#' @examples
#' \dontrun{
#' nct_ids <- c("NCT05789082")
#' study_id <- "STUDY001"
#' ts_domain <- create_ts_domain(nct_ids, study_id)
#' print(head(ts_domain))
#' }
create_ts_domain <- function(nct_ids, study_id, output_dir = getwd()) {
  # Locate the TS summary file within the package
  input_file <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  if (!file.exists(input_file)) {
    stop("File does not exist: ", input_file)
  }
  
  # Read the TS summary file
  ts_summary <- read.xlsx(input_file, sheet = "TS")
  
  # Fetch and process trial data for each NCT ID
  study_info <- fetch_study_info(nct_ids)
  trial_df <- json_to_dataframe(study_info)
  
  # Define the mapping for TS parameters
  ts_mapping <- list(
    ACTSUB = "enrollment",
    AGEMAX = "maximumAge",
    AGEMIN = "minimumAge",
    COMPTRT = "comparativeTreatment",
    CURTRT = "currentTherapy",
    FCNTRY = "countries",
    INDIC = "condition",
    INTMODEL = "interventionModel",
    INTTYPE = "studyType",
    LENGTH = "trialLength",
    NARMS = "numberOfArms",
    OBJPRIM = "primaryOutcomeMeasure",
    OBJSEC = "secondaryOutcomeMeasure",
    ONGOSIND = "overallStatus",
    OUTMSPRI = "primaryOutcomeMeasure",
    OUTMSSEC = "secondaryOutcomeMeasure",
    PLANSUB = "enrollment",
    REGID = "nctId",
    SENDTC = "completionDate",
    SEXPOP = "sex",
    SPONSOR = "sponsor",
    SSTDTC = "startDate",
    STYPE = "studyType",
    TBLIND = "masking",
    TCNTRL = "allocation",
    TDIGRP = "condition",
    THERAREA = "condition",
    TITLE = "officialTitle",
    TPHASE = "phase",
    TRT = "interventionNames",
    TTYPE = "studyType"
  )
  
  # Map the fetched data to TSVAL based on TSPARM labels
  for (i in seq_len(nrow(ts_summary))) {
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
      } else if (param %in% c("PROTVERS", "PROTAMDT", "OBJPRIM", "OBJSEC", "OUTMSEXP", "OUTMSPRI", "OUTMSSEC")) {
        ts_summary$TSVAL[i] <- tsval
      } else if (param == "LENGTH") {
        ts_summary$TSVAL[i] <- calculate_trial_length(trial_df$startDate[1], trial_df$completionDate[1])
      } else if (param %in% c("AGEMAX", "AGEMIN")) {
        ts_summary$TSVAL[i] <- format_age_iso8601(tsval)
      } else {
        ts_summary$TSVAL[i] <- paste(unique(unlist(tsval)), collapse = "; ")
      }
    } else {
      ts_summary$TSVAL[i] <- NA
    }
    
    # Clean the TSVAL values
    ts_summary$TSVAL[i] <- clean_tsval(ts_summary$TSVAL[i])
  }
  
  # Fill in the STUDYID and DOMAIN columns
  ts_summary$STUDYID <- study_id
  ts_summary$DOMAIN <- "TS"
  
  # Save the final output as a data frame
  final_df <- ts_summary
  
  # Write the output to an Excel file
  output_file <- file.path(output_dir, paste0(study_id, "_TS.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TS")
  writeData(wb, "TS", final_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(final_df)
}

# Helper functions

#' Format Dates in ISO 8601 Format
#'
#' @param date A character string representing a date.
#' @return A character string formatted as YYYY-MM-DD or NA if input is invalid.
#' @keywords internal
format_date_iso8601 <- function(date) {
  if (!is.na(date) && !is.null(date)) {
    return(as.character(as.Date(date, format = "%Y-%m-%d")))
  } else {
    return(NA)
  }
}

#' Convert Numeric Phase to Roman Numeral
#'
#' @param phase A character string representing the phase.
#' @return A character string with the phase converted to Roman numeral.
#' @keywords internal
convert_to_roman <- function(phase) {
  phase <- toupper(phase)
  roman_phases <- c("PHASE1" = "Phase I", "PHASE2" = "Phase II", "PHASE3" = "Phase III", "PHASE4" = "Phase IV")
  if (is.vector(phase)) {
    return(paste(sapply(phase, function(p) ifelse(p %in% names(roman_phases), roman_phases[p], p)), collapse = " / "))
  } else {
    return(ifelse(phase %in% names(roman_phases), roman_phases[phase], phase))
  }
}

#' Calculate Trial Length in ISO 8601 Format
#'
#' @param start_date A character string representing the start date (YYYY-MM-DD).
#' @param end_date A character string representing the end date (YYYY-MM-DD).
#' @return A character string representing the trial length in ISO 8601 format or NA if input is invalid.
#' @keywords internal
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
#' @param age A character string representing age.
#' @return A character string representing age in ISO 8601 format or NA if input is invalid.
#' @keywords internal
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

#' Clean TSVAL Values
#'
#' @param tsval A character string representing TSVAL.
#' @return A cleaned character string with unique TSVAL values.
#' @keywords internal
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