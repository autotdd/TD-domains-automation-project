library(clintrialx)
library(dplyr)
library(openxlsx)
source("R/json_utils.R")

#' Generate TS Dataset using clintrialx
#'
#' This function generates the TS dataset using clintrialx and other dependencies.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame representing the TS dataset.
#' @examples
#' generate_TS_dataset_CTx("STUDY123", 5, "NCT00000000")
#' @export
generate_TS_dataset_CTx <- function(study_id, num_rows, nct_ids) {
  # Fetch the study info using clintrialx
  study_info <- lapply(nct_ids, get_study_info)
  study_info <- bind_rows(study_info)
  
  # Fetch the Trial_Summary.xlsx from the package
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  data <- read.xlsx(file_path)
  
  # Process study_info and data
  # Your implementation here using data and study_info
  
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

create_ts_domain <- function(nct_ids, study_id, input_file) {
  # Read the TS summary file
  ts_summary <- read.xlsx(input_file, sheet = "TS")
  
  # Fetch and process trial data for each NCT ID
  trial_data <- lapply(nct_ids, function(nct_id) {
    study_info <- get_study_info(nct_id)
    
    # Save the JSON file with the study name prefix
    json_file_path <- tempfile(fileext = ".json")
    write_json(study_info, path = json_file_path)
    
    study_info_to_df(study_info)
  })
  
  # Combine all fetched trial data into a single dataframe
  trial_df <- bind_rows(trial_data)
  
  # Print the processed trial data to check its structure and contents
  # print("Preprocessed trial data from clintrialx:")
  # print(trial_df)
  
  # Define a mapping based on the dynamic extraction logic
  ts_mapping <- list(
    `NCT Number` = "NCT Number",
    `Trial Title` = "Study Title",
    `Clinical Study Sponsor` = "Sponsor",
    `Trial Phase Classification` = "Phases",
    `Actual number of Subjects` = "Enrollment",
    `Study Start Date` = "Start Date",
    `Primary Completion Date` = "Primary Completion Date",
    `Completion Date` = "Completion Date",
    `Trial Disease/Condition Indication` = "Conditions",
    `Study Status` = "Study Status",
    `Brief Summary` = "Brief Summary",
    `Age` = "Age",
    `Study Design` = "Study Design",
    `Interventions` = "Interventions",
    `Primary Outcome Measure` = "Primary Outcome Measures",
    `Secondary Outcome Measure` = "Secondary Outcome Measures",
    `Trial Primary Objective` = "Primary Outcome Measures",
    `Trial Secondary Objective` = "Secondary Outcome Measures",
    `Protocol Version` = "Study Documents",
    `Protocol Amendment Number` = "Study Documents",
    `Investigational Product` = "Interventions",
    `Comparator Product` = "Interventions",
    `Planned Maximum Age of Subjects` = "Age",
    `Planned Minimum Age of Subjects` = "Age"
  )
  
  # Map the fetched data to TSVAL based on TSPARM labels
  for (i in 1:nrow(ts_summary)) {
    param <- ts_summary$TSPARM[i]
    tsval_field <- ts_mapping[[param]]
    
    if (!is.null(tsval_field) && tsval_field %in% colnames(trial_df)) {
      if (grepl("Date", param, ignore.case = TRUE)) {
        ts_summary$TSVAL[i] <- format_date_iso8601(trial_df[[tsval_field]])
      } else if (param == "Trial Phase Classification") {
        ts_summary$TSVAL[i] <- convert_to_roman(trial_df[[tsval_field]])
      } else if (param == "Protocol Version") {
        ts_summary$TSVAL[i] <- extract_info(trial_df[[tsval_field]], "Protocol, ([^,]+)")
      } else if (param == "Protocol Amendment Number") {
        ts_summary$TSVAL[i] <- extract_info(trial_df[[tsval_field]], "Amendment, ([^,]+)")
      } else if (param == "Planned Maximum Age of Subjects") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Age, "Maximum Age: (\\d+)")
      } else if (param == "Planned Minimum Age of Subjects") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Age, "Minimum Age: (\\d+)")
      } else if (param == "Trial Primary Objective") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Primary Outcome Measures`), collapse = "; ")
      } else if (param == "Trial Secondary Objective") {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df$`Secondary Outcome Measures`), collapse = "; ")
      } else {
        ts_summary$TSVAL[i] <- paste(unlist(trial_df[[tsval_field]]), collapse = "; ")
      }
    } else {
      # Handling complex fields not directly mapped
      if (param == "Blinded Study") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Masking: ([^|]+)")
      } else if (param == "Parallel Study") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Intervention Model: ([^|]+)")
      } else if (param == "Allocation") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Allocation: ([^|]+)")
      } else if (param == "Intervention Model") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Intervention Model: ([^|]+)")
      } else if (param == "Masking") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Masking: ([^|]+)")
      } else if (param == "Primary Purpose") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$`Study Design`, "Primary Purpose: ([^|]+)")
      } else if (param == "Investigational Product") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Interventions, "DRUG: ([^|]+)")
      } else if (param == "Comparator Product") {
        ts_summary$TSVAL[i] <- extract_info(trial_df$Interventions, "DRUG: ([^|]+)")
      } else {
        ts_summary$TSVAL[i] <- NA  # No direct mapping available for other parameters
      }
    }
    # print(paste("TSVAL for", param, ":", ts_summary$TSVAL[i]))
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
# nct_ids <- c("NCT05112965")
# study_id <- "YO42713"
# input_file <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
# final_df <- create_ts_domain(nct_ids, study_id, input_file)
# print(paste("Output written to:", paste0(study_id, "_TS.xlsx")))
