# File: R/create_ti_domain.R

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
#' @importFrom stringr str_split str_extract str_replace_all str_trim str_locate_all str_sub str_locate fixed
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook freezePane
#' @examples
#' \dontrun{
#' # Get the path to the example PDF file
#' pdf_path <- system.file("extdata", "Temp_Protocol_v1.pdf", package = "autoTDD")
#' 
#' # Example using PDF method
#' ti_domain_pdf <- create_ti_domain(
#'   study_id = "STUDY001",
#'   method = "pdf",
#'   pdf_path = pdf_path,
#'   incl_range = 6:9,
#'   excl_range = 9:13,
#'   incl_section = "4.1.1",
#'   excl_section = "4.1.2",
#'   end_section = "4.2"
#' )
#'
#' # Example using API method
#' ti_domain_api <- create_ti_domain(
#'   study_id = "STUDY001",
#'   method = "api",
#'   nct_id = "NCT00000419"
#' )
#' }
create_ti_domain <- function(study_id, method, pdf_path = NULL, nct_id = NULL,
                             incl_range = NULL, excl_range = NULL,
                             incl_section = NULL, excl_section = NULL, end_section = NULL,
                             output_dir = getwd()) {
  # Ensure output_dir exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (method == "pdf") {
    if (is.null(pdf_path) || is.null(incl_range) || is.null(excl_range) ||
        is.null(incl_section) || is.null(excl_section) || is.null(end_section)) {
      stop("For PDF method, pdf_path, incl_range, excl_range, incl_section, excl_section, and end_section must be provided.")
    }
    if (!file.exists(pdf_path)) {
      stop("The specified PDF file does not exist: ", pdf_path)
    }
    result <- create_ti_domain_pdf(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir)
    
    # Print debugging information
    cat(result$debug_info)
    
    if (nrow(result$ti_domain) == 0) {
      stop("No inclusion or exclusion criteria found.")
    }
    
    return(result$ti_domain)
  } else if (method == "api") {
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    tryCatch({
      ti_domain <- create_ti_domain_api(study_id, nct_id, output_dir)
      if (nrow(ti_domain) == 0) {
        stop("create_ti_domain_api returned an empty data frame")
      }
      return(ti_domain)
    }, error = function(e) {
      cat("Error in create_ti_domain_api:", conditionMessage(e), "\n")
      return(data.frame(
        STUDYID = character(),
        DOMAIN = character(),
        IETESTCD = character(),
        IETEST = character(),
        IECAT = character(),
        IESCAT = character(),
        IEORRES = character(),
        stringsAsFactors = FALSE
      ))
    })
  } else {
    stop("Invalid method. Choose either 'pdf' or 'api'.")
  }
}

create_ti_domain_pdf <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir) {
  debug_info <- ""
  
  tryCatch({
    pdf_text <- pdftools::pdf_text(pdf_path)
    debug_info <- paste0(debug_info, sprintf("Number of pages in PDF: %d\n", length(pdf_text)))
    
    cleaned_result <- remove_footnotes_and_headers(pdf_text)
    cleaned_text <- cleaned_result$cleaned_pages
    debug_info <- paste0(debug_info, cleaned_result$debug_info)
    
    footnotes <- cleaned_result$footnotes
    debug_info <- paste0(debug_info, "Extracted footnotes:\n")
    for (i in seq_along(footnotes)) {
      debug_info <- paste0(debug_info, sprintf("%d. %s\n", i, footnotes[i]))
    }
    
    if (max(incl_range) > length(cleaned_text) || max(excl_range) > length(cleaned_text)) {
      stop(sprintf("Specified page ranges are out of bounds. Cleaned PDF has %d pages, incl_range: %s, excl_range: %s", 
                   length(cleaned_text), paste(incl_range, collapse = "-"), paste(excl_range, collapse = "-")))
    }
    
    inclusion_text <- cleaned_text[incl_range]
    exclusion_text <- cleaned_text[excl_range]
    
    debug_info <- paste0(debug_info, "Inclusion text sample: ", substr(inclusion_text[[1]]$text, 1, 500), "...\n")
    debug_info <- paste0(debug_info, "Exclusion text sample: ", substr(exclusion_text[[1]]$text, 1, 500), "...\n")
    
    inclusion_result <- extract_criteria(inclusion_text, incl_section, footnotes)
    exclusion_result <- extract_criteria(exclusion_text, excl_section, footnotes)
    
    debug_info <- paste0(debug_info, sprintf("Number of inclusion criteria: %d\n", length(inclusion_result$criteria)))
    debug_info <- paste0(debug_info, sprintf("Number of exclusion criteria: %d\n", length(exclusion_result$criteria)))
    
    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", sprintf("%03d", seq_along(inclusion_result$criteria))),
                   paste0("EXCL", sprintf("%03d", seq_along(exclusion_result$criteria)))),
      IETEST = c(rep("Inclusion Criteria", length(inclusion_result$criteria)),
                 rep("Exclusion Criteria", length(exclusion_result$criteria))),
      IECAT = "",
      IESCAT = "",
      IEORRES = c(inclusion_result$criteria, exclusion_result$criteria),
      stringsAsFactors = FALSE
    )
    
    if (nrow(ti_domain) > 0) {
      excel_file <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
      openxlsx::write.xlsx(ti_domain, excel_file)
      debug_info <- paste0(debug_info, "Excel file saved: ", excel_file, "\n")
    } else {
      debug_info <- paste0(debug_info, "No data to save to Excel.\n")
    }
    
    return(list(ti_domain = ti_domain, debug_info = debug_info))
  }, error = function(e) {
    error_msg <- paste("Error in create_ti_domain_pdf:", e$message, "\n")
    return(list(ti_domain = data.frame(), debug_info = paste0(debug_info, error_msg)))
  })
}

trim_and_clean <- function(text, max_length = 200) {
  text <- gsub("^\\s+|\\s+$", "", text)  # Remove leading and trailing whitespace
  text <- gsub("\\s+", " ", text)  # Replace multiple spaces with a single space
  if (nchar(text) > max_length) {
    text <- substr(text, 1, max_length - 23)  # Truncate to 177 characters (200 - 23 for the added text)
    text <- paste0(trimws(text), "... (As per the protocol)")
  }
  return(text)
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
  tryCatch({
    # Fetch study information
    study_info <- fetch_study_info(nct_id)
    
    # Process study information
    processed_info <- process_study_info(study_info, debug = TRUE)
    
    if (is.null(processed_info) || !is.list(processed_info)) {
      stop("Invalid or empty study information received from API")
    }
    
    # Extract eligibility criteria
    eligibility_module <- processed_info$protocolSection$eligibilityModule
    
    if (is.null(eligibility_module) || !is.list(eligibility_module)) {
      stop("Eligibility information not found in API response")
    }
    
    eligibility_criteria <- eligibility_module$eligibilityCriteria
    
    if (is.null(eligibility_criteria) || !is.character(eligibility_criteria)) {
      stop("Eligibility criteria not found in API response")
    } 
    
    # Split criteria into inclusion and exclusion
    criteria_split <- strsplit(eligibility_criteria, "Exclusion Criteria:")[[1]]
    
    if (length(criteria_split) < 2) {
      # If "Exclusion Criteria:" is not found, try to split by "Inclusion Criteria:"
      criteria_split <- strsplit(eligibility_criteria, "Inclusion Criteria:")[[1]]
      if (length(criteria_split) < 2) {
        stop("Unable to separate inclusion and exclusion criteria")
      }
      inclusion_text <- criteria_split[2]
      exclusion_text <- ""
    } else {
      inclusion_text <- sub("Inclusion Criteria:", "", criteria_split[1])
      exclusion_text <- criteria_split[2]
    }
    
    # Extract criteria
    inclusion_criteria <- extract_criteria_from_text(inclusion_text)
    exclusion_criteria <- extract_criteria_from_text(exclusion_text)
    
    # Ensure at least one criterion exists
    if (length(inclusion_criteria) == 0 && length(exclusion_criteria) == 0) {
      inclusion_criteria <- c("No specific inclusion criteria provided")
      exclusion_criteria <- c("No specific exclusion criteria provided")
    }
    
    # Create TI domain dataframe
    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", sprintf("%03d", seq_along(inclusion_criteria))),
                   paste0("EXCL", sprintf("%03d", seq_along(exclusion_criteria)))),
      IETEST = c(rep("Inclusion Criteria", length(inclusion_criteria)),
                 rep("Exclusion Criteria", length(exclusion_criteria))),
      IECAT = "",
      IESCAT = "",
      IEORRES = c(inclusion_criteria, exclusion_criteria),
      stringsAsFactors = FALSE
    )
    
    # Save to Excel
    save_ti_domain_to_excel(ti_domain, study_id, output_dir)
    
    return(ti_domain)
  }, error = function(e) {
    cat("Error in create_ti_domain_api:", conditionMessage(e), "\n")
    # Instead of returning NULL, return an empty data frame with the correct structure
    return(data.frame(
      STUDYID = character(),
      DOMAIN = character(),
      IETESTCD = character(),
      IETEST = character(),
      IECAT = character(),
      IESCAT = character(),
      IEORRES = character(),
      stringsAsFactors = FALSE
    ))
  })
}

# Helper function to extract criteria from text
extract_criteria_from_text <- function(text, max_length = 200) {
  if (is.null(text) || is.na(text) || nchar(trimws(text)) == 0) {
    return(character(0))
  }
  
  # Split the text into lines
  lines <- strsplit(trimws(text), "\n")[[1]]
  
  # Remove empty lines and trim whitespace
  lines <- trimws(lines[nchar(lines) > 0])
  
  # Combine lines that don't start with a number, bullet point, or asterisk
  criteria <- character(0)
  current_criterion <- ""
  
  for (line in lines) {
    if (grepl("^\\d+\\.\\s|^-\\s|^•\\s|^\\*\\s", line)) {
      if (nchar(current_criterion) > 0) {
        criteria <- c(criteria, trim_and_clean(current_criterion, max_length))
      }
      current_criterion <- sub("^\\d+\\.\\s|^-\\s|^•\\s|^\\*\\s", "", line)
    } else {
      current_criterion <- paste(current_criterion, line)
    }
  }
  
  if (nchar(current_criterion) > 0) {
    criteria <- c(criteria, trim_and_clean(current_criterion, max_length))
  }
  
  return(criteria)
}

#' Save TI Domain to Excel
#'
#' This function saves the TI domain data frame to an Excel file with formatting.
#'
#' @param ti_domain A data frame representing the TI domain.
#' @param study_id A character string representing the Study ID.
#' @param output_dir A character string representing the output directory.
#' @keywords internal
save_ti_domain_to_excel <- function(ti_domain, study_id, output_dir) {
  file_name <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TI_Domain")
  writeData(wb, "TI_Domain", ti_domain)

  # Set column widths
  setColWidths(wb, "TI_Domain", cols = 1:ncol(ti_domain), widths = "auto")

  # Define styles
  header_style <- createStyle(
    fontSize = 11,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  
  content_style <- createStyle(
    fontSize = 10,
    halign = "left",
    valign = "center",
    wrapText = TRUE
  )

  # Apply styles
  addStyle(wb, "TI_Domain", style = header_style, rows = 1, cols = 1:ncol(ti_domain), gridExpand = TRUE)
  addStyle(wb, "TI_Domain", style = content_style, rows = 2:(nrow(ti_domain) + 1), cols = 1:ncol(ti_domain), gridExpand = TRUE)

  # Freeze the top row
  freezePane(wb, "TI_Domain", firstRow = TRUE)

  tryCatch({
    saveWorkbook(wb, file_name, overwrite = TRUE)
    cat(sprintf("Excel file saved successfully: %s\n", file_name))
  }, error = function(e) {
    cat(sprintf("Error saving Excel file: %s\n", e$message))
  })

  return(file_name)
}
