library(pdftools)
library(dplyr)
library(stringr)
library(openxlsx)

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

# Function to handle text exceeding 200 characters
handle_text_length <- function(text, max_length = 200) {
  suffix <- "(As per the protocol)"
  suffix_length <- nchar(suffix)
  
  if (nchar(text) > (max_length - suffix_length)) {
    # Ensure we don't cut off in the middle of a word
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

# Function to extract common header and footer pattern
extract_header_footer_pattern <- function(pdf_text) {
  common_patterns <- c()
  for (page_text in pdf_text) {
    lines <- str_split(page_text, "\n")[[1]]
    header <- lines[1]
    footer <- lines[length(lines)]
    common_patterns <- c(common_patterns, header, footer)
  }
  common_patterns <- unique(common_patterns)
  return(common_patterns)
}

extract_ti_domain <- function(study_id = "AB64554", pdf_path = "592_Protocol.pdf", incl_range = 51:54, excl_range = 54:57, incl_section = "4.1.1", excl_section = "4.1.2", end_section = "4.2") {
  # Check if the protocol PDF file exists
  if (!file.exists(pdf_path)) {
    warning("The protocol PDF file is missing. Please ensure the file is named 'protocol.pdf' and located in the working directory.")
    return(NULL)
  }
  
  # Extract text from the specified page ranges of the PDF
  pdf_text <- pdf_text(pdf_path)
  inclusion_text <- paste(pdf_text[incl_range], collapse = "\n")
  exclusion_text <- paste(pdf_text[excl_range], collapse = "\n")
  
  # Extract header and footer patterns
  header_footer_patterns <- extract_header_footer_pattern(pdf_text)
  header_footer_pattern <- paste(header_footer_patterns, collapse = "|")
  
  # Remove common headers and footers
  inclusion_text <- str_replace_all(inclusion_text, fixed(header_footer_pattern), "")
  exclusion_text <- str_replace_all(exclusion_text, fixed(header_footer_pattern), "")
  
  # Replace special characters and trim spaces
  inclusion_text <- replace_special_chars_and_trim(inclusion_text)
  exclusion_text <- replace_special_chars_and_trim(exclusion_text)
  
  # Extract Inclusion Criteria
  inclusion_start <- str_locate(inclusion_text, incl_section)[1, 2]
  exclusion_start <- str_locate(inclusion_text, excl_section)[1, 1]
  inclusion_text <- str_sub(inclusion_text, inclusion_start + 1, exclusion_start - 1)
  
  # Extract Exclusion Criteria
  exclusion_start <- str_locate(exclusion_text, excl_section)[1, 2]
  end_section_pattern <- paste0("\\n", gsub("\\.", "\\\\.", end_section))
  exclusion_end <- str_locate(exclusion_text, end_section_pattern)[1, 1]
  if (!is.na(exclusion_end)) {
    exclusion_text <- str_sub(exclusion_text, exclusion_start + 1, exclusion_end - 1)
  } else {
    exclusion_text <- str_sub(exclusion_text, exclusion_start + 1)
  }
  
  # Add a newline before each bullet point to split correctly
  inclusion_text <- str_replace_all(inclusion_text, "", "\n")
  exclusion_text <- str_replace_all(exclusion_text, "", "\n")
  
  # Split criteria into individual points
  inclusion_criteria <- unlist(str_split(inclusion_text, "\n"))
  exclusion_criteria <- unlist(str_split(exclusion_text, "\n"))
  
  # Clean up criteria text
  inclusion_criteria <- str_trim(inclusion_criteria)
  exclusion_criteria <- str_trim(exclusion_criteria)
  
  # Remove any empty strings and initial descriptive text that is showing up in the output
  inclusion_criteria <- inclusion_criteria[inclusion_criteria != ""]
  exclusion_criteria <- exclusion_criteria[exclusion_criteria != ""]
  inclusion_criteria <- inclusion_criteria[-1]
  exclusion_criteria <- exclusion_criteria[-1]
  
  # Handle text length exceeding 200 characters
  inclusion_criteria <- sapply(inclusion_criteria, handle_text_length, max_length = 200)
  exclusion_criteria <- sapply(exclusion_criteria, handle_text_length, max_length = 200)
  
  # Create a data frame for the TI domain
  # ti_domain <- data.frame(
  #   STUDYID = study_id,
  #   DOMAIN = "TI",
  #   IETESTCD = c(paste0("INCL", seq_along(inclusion_criteria)), paste0("EXCL", seq_along(exclusion_criteria))),
  #   IETEST = c(inclusion_criteria, exclusion_criteria),
  #   IECAT = c(rep("INCLUSION", length(inclusion_criteria)), rep("EXCLUSION", length(exclusion_criteria))),
  #   IESCAT = NA,
  #   TIRL = NA,
  #   TIVERS = 1,
  #   stringsAsFactors = FALSE
  # )
  
    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", sprintf("%03d", seq_along(inclusion_result$criteria))),
                   paste0("EXCL", sprintf("%03d", seq_along(exclusion_result$criteria)))),
      IETEST = c(rep("Inclusion Criteria", length(inclusion_result$criteria)),
                 rep("Exclusion Criteria", length(exclusion_result$criteria))),
      IECAT = c(rep("Inclusion", length(inclusion_result$criteria)),
                rep("Exclusion", length(exclusion_result$criteria))),
      IESCAT = ifelse(c(inclusion_result$subcategories, exclusion_result$subcategories) == "", NA, c(inclusion_result$subcategories, exclusion_result$subcategories)),
      IEORRES = c(inclusion_result$criteria, exclusion_result$criteria),
      stringsAsFactors = FALSE
    )
    
  # Save the dataframe to an xlsx file with formatting
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
ti_domain <- extract_ti_domain()
# if (!is.null(ti_domain)) {
#   print(ti_domain)
# }

study_id <- "STUDY001"
pdf_path <- "/Users/siddharthlokineni/Desktop/autotdd/TD-domains-automation-project/592_Protocol.pdf"

result <- extract_ti_domain(
  study_id = study_id,
  pdf_path = pdf_path,
  incl_range = 51:54,
  excl_range = 54:58,
  incl_section = "4.1.1",
  excl_section = "4.1.2",
  end_section = "4.2"
)

# Print the TI domain data
print(result)

