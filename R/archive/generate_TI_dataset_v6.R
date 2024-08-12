library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)
# source("R/json_utils.R")

#' Generate TI Dataset (Version 6)
#'
#' This function generates the TI dataset for a given study ID and number of rows. This is based on the Clintrails data.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @param nct_ids A character vector of NCT IDs.
#' @return A data frame representing the TI dataset.
#' @examples
#' generate_TI_dataset_v6("STUDY123", 5, "NCT00000000")
#' @export
generate_TI_dataset_v6 <- function(study_id, num_rows, nct_ids) {
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  data <- read.xlsx(file_path)
  study_info <- get_study_info(nct_ids)
  
  return(data)
}


# Function to handle text exceeding 200 characters
handle_text_length <- function(text, max_length = 200) {
  suffix <- "(As per the protocol)"
  suffix_length <- nchar(suffix)
  
  if (nchar(text) > (max_length - suffix_length)) {
    truncated_text <- str_sub(text, 1, max_length - suffix_length - 1)
    last_space <- str_locate_all(truncated_text, " ")[[1]]
    if (!is.null(last_space) && nrow(last_space) > 0) {
      last_space_position <- last(last_space[, 1])
      text <- str_sub(truncated_text, 1, last_space_position - 1)
    }
    text <- paste0(text, " ", suffix)
  }
  return(text)
}

# Function to extract and clean inclusion and exclusion criteria from eligibility text
extract_criteria <- function(text, type) {
  if (is.na(text) || is.null(text)) {
    return(data.frame(IETESTCD = character(0), IETEST = character(0), IECAT = character(0), stringsAsFactors = FALSE))
  }
  
  # Split the text by newlines and bullets
  criteria <- unlist(str_split(text, "\\n\\*\\s|\\n\\*|\\n\\n\\*\\s|\\*\\s"))
  
  # Remove any empty strings and NA values
  criteria <- criteria[criteria != "" & !is.na(criteria)]
  
  # Remove initial descriptive text if present
  criteria <- criteria[!grepl("Inclusion Criteria|Exclusion Criteria", criteria, ignore.case = TRUE)]
  
  # Handle text length exceeding 200 characters
  criteria <- sapply(criteria, handle_text_length, max_length = 200)
  
  # Create a data frame for the criteria
  criteria_df <- data.frame(
    IETESTCD = paste0(type, seq_along(criteria)),
    IETEST = criteria,
    IECAT = type,
    stringsAsFactors = FALSE
  )
  
  return(criteria_df)
}

# Main function to create the TI domain
create_ti_domain <- function(nct_ids, study_id, study_info) {
  # Extract the eligibility criteria text
  eligibility_text <- study_info[["protocolSection"]][["eligibilityModule"]][["eligibilityCriteria"]]
  
  # Split the eligibility text into inclusion and exclusion sections
  inclusion_text <- str_extract(eligibility_text, "(?s)(?<=Inclusion Criteria:).*?(?=Exclusion Criteria:)")
  exclusion_text <- str_extract(eligibility_text, "(?s)(?<=Exclusion Criteria:).*")
  
  # Debugging information
  # print("Inclusion text:")
  # print(inclusion_text)
  # print("Exclusion text:")
  # print(exclusion_text)
  
  # Extract and clean the inclusion and exclusion criteria
  inclusion_criteria_df <- extract_criteria(inclusion_text, "INCL")
  exclusion_criteria_df <- extract_criteria(exclusion_text, "EXCL")
  
  # Combine the inclusion and exclusion criteria into a single data frame
  ti_domain <- bind_rows(inclusion_criteria_df, exclusion_criteria_df)
  
  # Remove any rows with empty IETEST
  ti_domain <- ti_domain[ti_domain$IETEST != "", ]
  
  # Re-number the INCL and EXCL items
  ti_domain <- ti_domain %>%
    group_by(IECAT) %>%
    mutate(IETESTCD = paste0(IECAT, row_number())) %>%
    ungroup()
  
  # Add the fixed columns to the TI domain
  ti_domain <- ti_domain %>%
    mutate(
      STUDYID = study_id,
      DOMAIN = "TI",
      IESCAT = NA,
      TIRL = NA,
      TIVERS = 1
    ) %>%
    select(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT, IESCAT, TIRL, TIVERS)
  
  # Save the data frame to an xlsx file with formatting
  file_name <- paste0(study_id, "_TI.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "TI_Domain")
  writeData(wb, "TI_Domain", ti_domain)
  
  # Set column width for IETEST to show at least 100 characters
  setColWidths(wb, "TI_Domain", cols = 4, widths = 100)
  
  # Apply text wrapping to all columns
  wrapStyle <- createStyle(wrapText = TRUE)
  addStyle(wb, "TI_Domain", style = wrapStyle, rows = 1:(nrow(ti_domain) + 1), cols = 1:ncol(ti_domain), gridExpand = TRUE)
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  return(ti_domain)
}

# Example usage
# nct_ids <- "NCT05789082"
# study_id <- "BO44426"

# Fetch study information
# study_info <- get_study_info(nct_ids)

# Create the TI domain
# ti_domain <- create_ti_domain(nct_ids, study_id, study_info)
# print(ti_domain)
