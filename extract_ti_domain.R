library(pdftools)
library(dplyr)
library(stringr)

# Function to replace special characters and remove unnecessary spaces
replace_special_chars_and_trim <- function(text) {
  text <- str_replace_all(text, "", ">=")
  text <- str_replace_all(text, "", "<=")
  text <- str_replace_all(text, "", "<")
  text <- str_replace_all(text, "", ">")
  text <- str_replace_all(text, "", "-")
  text <- str_replace_all(text, "\\s{2,}", " ") # Replace multiple spaces with a single space
  text <- str_trim(text) # Trim leading and trailing spaces
  return(text)
}

extract_ti_domain <- function(pdf_path, study_id, incl_range = 64:67, excl_range = 67:71, incl_section = "4.1.1", excl_section = "4.1.2") {
  # Extract text from the specified page ranges of the PDF
  pdf_text <- pdf_text(pdf_path)
  inclusion_text <- paste(pdf_text[incl_range], collapse = "\n")
  exclusion_text <- paste(pdf_text[excl_range], collapse = "\n")
  
  # Remove common headers and footers
  header_footer_pattern <- "Tiragolumab and Atezolizumab—F. Hoffmann-La Roche Ltd.*|\\d+/Protocol GO41854, Version 1"
  inclusion_text <- str_replace_all(inclusion_text, header_footer_pattern, "")
  exclusion_text <- str_replace_all(exclusion_text, header_footer_pattern, "")
  
  # Replace special characters and trim spaces
  inclusion_text <- replace_special_chars_and_trim(inclusion_text)
  exclusion_text <- replace_special_chars_and_trim(exclusion_text)
  
  # Extract Inclusion Criteria
  inclusion_start <- str_locate(inclusion_text, incl_section)[1, 2]
  exclusion_start <- str_locate(inclusion_text, excl_section)[1, 1]
  inclusion_text <- str_sub(inclusion_text, inclusion_start + 1, exclusion_start - 1)
  
  # Extract Exclusion Criteria
  exclusion_start <- str_locate(exclusion_text, excl_section)[1, 2]
  exclusion_end <- str_locate(exclusion_text, "Method of Treatment Assignment and Blinding|4.2")[1, 1]
  exclusion_text <- str_sub(exclusion_text, exclusion_start + 1, exclusion_end - 1)
  
  # Add a newline before each bullet point to split correctly
  inclusion_text <- str_replace_all(inclusion_text, "", "\n")
  exclusion_text <- str_replace_all(exclusion_text, "", "\n")
  
  # Split criteria into individual points
  inclusion_criteria <- unlist(str_split(inclusion_text, "\n"))
  exclusion_criteria <- unlist(str_split(exclusion_text, "\n"))
  
  # Clean up criteria text
  inclusion_criteria <- str_trim(inclusion_criteria)
  exclusion_criteria <- str_trim(exclusion_criteria)
  
  # Remove any empty strings and initial descriptive text
  inclusion_criteria <- inclusion_criteria[inclusion_criteria != ""]
  exclusion_criteria <- exclusion_criteria[exclusion_criteria != ""]
  inclusion_criteria <- inclusion_criteria[!str_detect(inclusion_criteria, "^Inclusion Criteria Patients must meet the following criteria for study entry:")]
  
  # Create a data frame for the TI domain
  ti_domain <- data.frame(
    STUDYID = study_id,
    DOMAIN = "TI",
    IETESTCD = c(paste0("INCL", seq_along(inclusion_criteria)), paste0("EXCL", seq_along(exclusion_criteria))),
    IETEST = c(inclusion_criteria, exclusion_criteria),
    IECAT = c(rep("INCLUSION", length(inclusion_criteria)), rep("EXCLUSION", length(exclusion_criteria))),
    IESCAT = NA,
    TIRL = NA,
    TIVERS = 1,
    stringsAsFactors = FALSE
  )
  
  return(ti_domain)
}

# Example usage
pdf_path <- "/cloud/project/Protocol GO41854 tiragolumab v1, Published Output-1_2.pdf"
study_id <- "BO42592"

ti_domain <- extract_ti_domain(pdf_path, study_id)
print(ti_domain)
