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
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook
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
#' # This will save an Excel file in the current working directory
#' # named "STUDY001_TI.xlsx"
#' }
#' @importFrom pdftools pdf_text
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all str_trim str_split str_locate fixed str_sub
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook
#' @export
create_ti_domain <- function(study_id, method, pdf_path = NULL, nct_id = NULL,
                             incl_range = NULL, excl_range = NULL,
                             incl_section = NULL, excl_section = NULL, end_section = NULL,
                             output_dir = getwd()) {
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
    
    if (length(result$ti_domain) == 0) {
      stop("No inclusion or exclusion criteria found.")
    }
    
    return(result$ti_domain)
  } else if (method == "api") {
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    ti_domain <- create_ti_domain_api(study_id, nct_id, output_dir)
    return(ti_domain)
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
                   length(cleaned_text), paste(incl_range, collapse="-"), paste(excl_range, collapse="-")))
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

extract_criteria <- function(text_with_pages, section_header, footnotes) {
  all_lines <- unlist(lapply(text_with_pages, function(page) strsplit(page$text, "\n")[[1]]))
  all_page_nums <- unlist(lapply(text_with_pages, function(page) rep(page$page_num, length(strsplit(page$text, "\n")[[1]]))))
  
  start_index <- which(grepl(section_header, all_lines, ignore.case = TRUE))
  
  if (length(start_index) == 0) {
    warning(sprintf("Section header '%s' not found in the text. Searching for alternatives.", section_header))
    alternative_headers <- c("Inclusion Criteria", "Exclusion Criteria", "Eligibility Criteria", "Inclusion criteria", "Exclusion criteria")
    for (alt_header in alternative_headers) {
      start_index <- which(grepl(alt_header, all_lines, ignore.case = TRUE))
      if (length(start_index) > 0) {
        warning(sprintf("Alternative header '%s' found.", alt_header))
        break
      }
    }
    if (length(start_index) == 0) {
      warning("No criteria section found. Returning all text as a single criterion.")
      return(list(criteria = text_with_pages[[1]]$text, pages = text_with_pages[[1]]$page_num))
    }
  }
  
  criteria <- character(0)
  criteria_pages <- integer(0)
  current_criterion <- ""
  current_page <- all_page_nums[start_index[1]]
  
  # Define patterns to ignore
  ignore_patterns <- c(
    "^\\d+(\\.\\d+)*\\s+[A-Z]",  # Section numbers
    "^Patients must meet",       # Introductory text
    "^The following criteria",   # Introductory text
    "^Inclusion criteria:",      # Section header
    "^Exclusion criteria:",      # Section header
    "^Eligibility criteria:"     # Section header
  )
  
  # Define valid criterion start patterns
  valid_start_patterns <- paste0("^\\s*(",
    "•|",                  # Bullet point
    "\\*|",                # Asterisk
    "\\d+\\.\\d*|",        # Numbered (e.g., 1. or 1.1)
    "\\d+\\)|",            # Numbered with parenthesis (e.g., 1))
    "[a-z]\\)|",           # Lowercase letter with parenthesis (e.g., a))
    "[A-Z]\\)|",           # Uppercase letter with parenthesis (e.g., A))
    "\\([a-z]\\)|",        # Lowercase letter in parentheses (e.g., (a))
    "\\([A-Z]\\)|",        # Uppercase letter in parentheses (e.g., (A))
    "\\([ivx]+\\)|",       # Lowercase Roman numerals in parentheses (e.g., (i), (iv))
    "\\([IVX]+\\)|",       # Uppercase Roman numerals in parentheses (e.g., (I), (IV))
    "-|",                  # Dash
    "□|",                  # Empty checkbox
    "☐|",                  # Another empty checkbox unicode
    "■|",                  # Filled checkbox
    "☑"                    # Checked checkbox unicode
  , ")")
  
  for (i in (start_index[1] + 1):length(all_lines)) {
    line <- trimws(all_lines[i])
    if (nchar(line) == 0) next
    
    # Remove footnotes and protocol version information
    for (footnote in footnotes) {
      if (nchar(footnote) > 0) {
        line <- gsub(footnote, "", line, fixed = TRUE)
      }
    }
    line <- gsub("\\|?\\s*Protocol\\s+[A-Za-z0-9]+,?\\s*Version\\s+\\d+(\\.\\d+)?", "", line)
    
    # Remove page numbers from the line
    line <- gsub(paste0("\\b", all_page_nums[i], "\\b"), "", line)
    
    if (nchar(trimws(line)) == 0) next
    
    # Skip lines matching ignore patterns
    if (any(sapply(ignore_patterns, function(pattern) grepl(pattern, line)))) {
      next
    }
    
    # Start a new criterion if we encounter a valid start pattern
    if (grepl(valid_start_patterns, line)) {
      if (nchar(current_criterion) > 0) {
        criteria <- c(criteria, trim_and_clean(current_criterion))
        criteria_pages <- c(criteria_pages, current_page)
      }
      current_criterion <- gsub(valid_start_patterns, "", line)
      current_page <- all_page_nums[i]
    } else if (nchar(current_criterion) > 0) {
      # Append to the current criterion if we're in the middle of one
      current_criterion <- paste(current_criterion, line)
    }
    
    if (grepl("(Exclusion Criteria|Study Procedures|Investigational Medicinal Products)", line, ignore.case = TRUE)) {
      break
    }
  }
  
  if (nchar(current_criterion) > 0) {
    criteria <- c(criteria, trim_and_clean(current_criterion))
    criteria_pages <- c(criteria_pages, current_page)
  }
  
  if (length(criteria) == 0) {
    warning("No criteria extracted. Returning all text as a single criterion.")
    return(list(criteria = text_with_pages[[1]]$text, pages = text_with_pages[[1]]$page_num))
  }
  
  return(list(criteria = criteria, pages = criteria_pages))
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

library(stringdist)

remove_footnotes_and_headers <- function(pages) {
  debug_info <- ""
  
  tryCatch({
    # Split each page into lines and add page numbers
    page_lines <- lapply(seq_along(pages), function(page_num) {
      lines <- strsplit(pages[page_num], "\n")[[1]]
      list(lines = lines, page_num = page_num)
    })
    debug_info <- paste0(debug_info, sprintf("Number of pages: %d\n", length(page_lines)))
    
    # Check for empty pages
    page_lengths <- sapply(page_lines, function(page) length(page$lines))
    debug_info <- paste0(debug_info, "Page lengths: ", paste(page_lengths, collapse = ", "), "\n")
    
    # Remove empty pages
    non_empty_pages <- page_lines[page_lengths > 0]
    debug_info <- paste0(debug_info, sprintf("Number of non-empty pages: %d\n", length(non_empty_pages)))
    
    if (length(non_empty_pages) == 0) {
      stop("All pages are empty")
    }
    
    # Identify potential footnotes
    potential_footnotes <- character(0)
    for (i in 1:length(non_empty_pages)) {
      last_two_lines <- tail(non_empty_pages[[i]]$lines, 2)
      if (length(last_two_lines) == 2 && all(nchar(trimws(last_two_lines)) > 0)) {
        combined_line <- paste(last_two_lines, collapse = " ")
        # Check if this combined line (or a similar one) appears at the bottom of other pages
        similar_lines <- sapply(non_empty_pages, function(page) {
          page_last_two_lines <- tail(page$lines, 2)
          page_combined_line <- paste(page_last_two_lines, collapse = " ")
          stringdist::stringdist(combined_line, page_combined_line) <= 10  # Allow for small differences
        })
        if (sum(similar_lines) > length(non_empty_pages) * 0.2) {  # If it appears on more than 20% of pages
          potential_footnotes <- c(potential_footnotes, combined_line)
        }
      }
    }
    
    potential_footnotes <- unique(potential_footnotes)
    
    debug_info <- paste0(debug_info, "Potential footnotes:\n")
    for (i in seq_along(potential_footnotes)) {
      debug_info <- paste0(debug_info, sprintf("%d. %s\n", i, potential_footnotes[i]))
    }
    
    # Clean pages while preserving page numbers
    cleaned_pages <- lapply(non_empty_pages, function(page) {
      cleaned_lines <- page$lines
      for (footnote in potential_footnotes) {
        footnote_parts <- strsplit(footnote, " ")[[1]]
        for (i in 1:(length(cleaned_lines) - 1)) {
          if (paste(cleaned_lines[i], cleaned_lines[i+1], collapse = " ") == footnote ||
              stringdist::stringdist(paste(cleaned_lines[i], cleaned_lines[i+1], collapse = " "), footnote) <= 10) {
            cleaned_lines[i] <- cleaned_lines[i+1] <- ""
          }
        }
      }
      cleaned_lines <- cleaned_lines[nchar(trimws(cleaned_lines)) > 0]
      list(text = paste(cleaned_lines, collapse = "\n"), page_num = page$page_num)
    })
    
    debug_info <- paste0(debug_info, sprintf("Number of pages after cleaning: %d\n", length(cleaned_pages)))
    
    return(list(cleaned_pages = cleaned_pages, debug_info = debug_info, footnotes = potential_footnotes))
  }, error = function(e) {
    error_msg <- paste("Error in remove_footnotes_and_headers:", e$message, "\n")
    return(list(cleaned_pages = pages, debug_info = paste0(debug_info, error_msg), footnotes = character(0)))
  })
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
    select(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT, IESCAT, IEORRES, TIRL, TIVERS)

  # Save the data frame to an xlsx file with formatting
  save_ti_domain_to_excel(ti_domain, study_id, output_dir)

   return(ti_domain)
}

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

  # Set column widths
  setColWidths(wb, "TI_Domain", cols = 1:6, widths = c(10, 10, 10, 20, 10, 10))
  setColWidths(wb, "TI_Domain", cols = 7, widths = 150)  # IEORRES column

  # Apply text wrapping to all columns
  wrapStyle <- createStyle(wrapText = TRUE, valign = "top")
  addStyle(wb, "TI_Domain", style = wrapStyle, rows = 1:(nrow(ti_domain) + 1), cols = 1:ncol(ti_domain), gridExpand = TRUE)

  # Set row height to accommodate wrapped text
  setRowHeights(wb, "TI_Domain", rows = 2:(nrow(ti_domain) + 1), heights = 60)

  debug_info <- sprintf("Attempting to save Excel file: %s\n", file_name)

  tryCatch({
    saveWorkbook(wb, file_name, overwrite = TRUE)
    debug_info <- paste0(debug_info, sprintf("Excel file saved successfully: %s\n", file_name))
  }, error = function(e) {
    debug_info <- paste0(debug_info, sprintf("Error saving Excel file: %s\n", e$message))
  })

  return(debug_info)
}

# Helper function to extract footnotes from debug info
extract_footnotes_from_debug <- function(debug_info) {
  footnote_lines <- grep("^\\d+\\.", strsplit(debug_info, "\n")[[1]], value = TRUE)
  footnotes <- sub("^\\d+\\.\\s*", "", footnote_lines)
  # Remove any empty footnotes
  footnotes <- footnotes[nchar(trimws(footnotes)) > 0]
  return(footnotes)
}