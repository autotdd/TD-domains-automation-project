#' Create TI Domain
#'
#' This function creates the TI (Trial Inclusion/Exclusion Criteria) domain 
#' by extracting inclusion and exclusion criteria either from a PDF protocol or from ClinicalTrials.gov API.
#'
#' @param study_id A character string representing the Study ID.
#' @param method A character string, either "pdf" or "api", specifying the method to use for extraction.
#' @param pdf_path A character string representing the path to the protocol PDF. Required if method is "pdf".
#' @param nct_id A character string representing the NCT ID. Required if method is "api".
#' @param incl_range A numeric vector representing the page range for inclusion criteria. Required if method is "pdf".
#' @param excl_range A numeric vector representing the page range for exclusion criteria. Required if method is "pdf".
#' @param incl_section A character string representing the section identifier for inclusion criteria. Required if method is "pdf".
#' @param excl_section A character string representing the section identifier for exclusion criteria. Required if method is "pdf".
#' @param end_section A character string representing the section identifier for the end of criteria. Required if method is "pdf".
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A data frame representing the TI domain.
#' @export
#' @importFrom dplyr mutate select bind_rows group_by ungroup
#' @importFrom stringr str_split str_extract str_replace_all str_trim str_locate_all str_sub
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook
#' @examples
#' \dontrun{
#' # Example using PDF method
#' ti_domain_pdf <- create_ti_domain(
#'   study_id = "STUDY005", 
#'   method = "pdf",
#'   pdf_path = "path/to/protocol.pdf",
#'   incl_range = 64:67,
#'   excl_range = 67:71,
#'   incl_section = "4.1.1",
#'   excl_section = "4.1.2",
#'   end_section = "4.2"
#' )
#' 
#' # Example using API method
#' ti_domain_api <- create_ti_domain(
#'   study_id = "STUDY005", 
#'   method = "api",
#'   nct_id = "NCT05789082"
#' )
#' 
#' # Both methods will save an Excel file named "STUDY005_TI.xlsx" in the specified output directory
#' }
create_ti_domain <- function(study_id, method, pdf_path = NULL, nct_id = NULL, 
                             incl_range = NULL, excl_range = NULL, 
                             incl_section = NULL, excl_section = NULL, end_section = NULL,
                             output_dir = getwd()) {
  if (method == "pdf") {
    if (is.null(pdf_path) || is.null(incl_range) || is.null(excl_range) || 
        is.null(incl_section) || is.null(excl_section) || is.null(end_section)) {
      stop("For PDF method, pdf_path, incl_range, excl_range, incl_section, excl_section, and end_section must be provided.")
    }
    ti_domain <- create_ti_domain_pdf(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir)
  } else if (method == "api") {
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    ti_domain <- create_ti_domain_api(study_id, nct_id, output_dir)
  } else {
    stop("Invalid method. Choose either 'pdf' or 'api'.")
  }
  
  return(ti_domain)
}

#' Create TI Domain from PDF
#'
#' This function creates the TI domain by extracting inclusion and exclusion criteria from a PDF protocol.
#'
#' @param study_id A character string representing the Study ID.
#' @param pdf_path A character string representing the path to the protocol PDF.
#' @param incl_range A numeric vector representing the page range for inclusion criteria.
#' @param excl_range A numeric vector representing the page range for exclusion criteria.
#' @param incl_section A character string representing the section identifier for inclusion criteria.
#' @param excl_section A character string representing the section identifier for exclusion criteria.
#' @param end_section A character string representing the section identifier for the end of criteria.
#' @return A data frame representing the TI domain.
#' @importFrom pdftools pdf_text
#' @keywords internal
create_ti_domain_pdf <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir) {
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
  
  # Extract and clean the inclusion and exclusion criteria
  inclusion_criteria_df <- extract_criteria(inclusion_text, "INCL")
  exclusion_criteria_df <- extract_criteria(exclusion_text, "EXCL")
  
  # Combine and process the criteria
  ti_domain <- process_ti_domain(inclusion_criteria_df, exclusion_criteria_df, study_id, output_dir)
  
  return(ti_domain)
}

#' Create TI Domain from API
#'
#' This function creates the TI domain by extracting inclusion and exclusion criteria from ClinicalTrials.gov API.
#'
#' @param study_id A character string representing the Study ID.
#' @param nct_id A character string representing the NCT ID.
#' @return A data frame representing the TI domain.
#' @keywords internal
create_ti_domain_api <- function(study_id, nct_id, output_dir) {
  # Fetch study information from the API
  study_info <- get_study_info(nct_id)
  
  # Extract the eligibility criteria text
  eligibility_text <- study_info[["protocolSection"]][["eligibilityModule"]][["eligibilityCriteria"]]
  
  # Split the eligibility text into inclusion and exclusion sections
  inclusion_text <- str_extract(eligibility_text, "(?s)(?<=Inclusion Criteria:).*?(?=Exclusion Criteria:)")
  exclusion_text <- str_extract(eligibility_text, "(?s)(?<=Exclusion Criteria:).*")
  
  # Extract and clean the inclusion and exclusion criteria
  inclusion_criteria_df <- extract_criteria(inclusion_text, "INCL")
  exclusion_criteria_df <- extract_criteria(exclusion_text, "EXCL")
  
  # Combine and process the criteria
  ti_domain <- process_ti_domain(inclusion_criteria_df, exclusion_criteria_df, study_id, output_dir)
  
  return(ti_domain)
}

#' Process TI Domain
#'
#' This function processes the extracted criteria and creates the final TI domain data frame.
#'
#' @param inclusion_criteria_df A data frame of inclusion criteria.
#' @param exclusion_criteria_df A data frame of exclusion criteria.
#' @param study_id A character string representing the Study ID.
#' @return A data frame representing the processed TI domain.
#' @keywords internal
process_ti_domain <- function(inclusion_criteria_df, exclusion_criteria_df, study_id, output_dir) {
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
  save_ti_domain_to_excel(ti_domain, study_id, output_dir)
  
  return(ti_domain)
}

# Helper functions (extract_criteria, handle_text_length, etc.) remain the same as in the previous version

#' Save TI Domain to Excel
#'
#' This function saves the TI domain data frame to an Excel file with formatting.
#'
#' @param ti_domain A data frame representing the TI domain.
#' @param study_id A character string representing the Study ID.
#' @keywords internal
save_ti_domain_to_excel <- function(ti_domain, study_id, output_dir) {
  file_name <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TI_Domain")
  writeData(wb, "TI_Domain", ti_domain)
  
  # Set column width for IETEST to show at least 100 characters
  setColWidths(wb, "TI_Domain", cols = 4, widths = 100)
  
  # Apply text wrapping to all columns
  wrapStyle <- createStyle(wrapText = TRUE)
  addStyle(wb, "TI_Domain", style = wrapStyle, rows = 1:(nrow(ti_domain) + 1), cols = 1:ncol(ti_domain), gridExpand = TRUE)
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
}