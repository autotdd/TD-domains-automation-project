# File: R/create_ti_domain.R

#' Create TI Domain
#'
#' This function creates the TI (Trial Inclusion/Exclusion Criteria) domain using either PDF or API method.
#'
#' @param study_id A character string specifying the study ID.
#' @param method A character string specifying the method to use ("pdf" or "api").
#' @param pdf_path Path to the PDF file (required if method is "pdf").
#' @param nct_id NCT ID for the study (required if method is "api").
#' @param incl_range Page range for inclusion criteria (required if method is "pdf").
#' @param excl_range Page range for exclusion criteria (required if method is "pdf").
#' @param incl_section Section identifier for inclusion criteria (required if method is "pdf").
#' @param excl_section Section identifier for exclusion criteria (required if method is "pdf").
#' @param end_section Section identifier for the end of criteria (required if method is "pdf").
#' @param output_dir Directory to save the output Excel file.
#'
#' @return A data frame containing the TI domain information.
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
    return(create_ti_domain_pdf(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir))
  } else if (method == "api") {
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    return(create_ti_domain_api(study_id, nct_id, output_dir))
  } else {
    stop("Invalid method. Choose either 'pdf' or 'api'.")
  }
}

# Helper functions
trim_and_clean <- function(text, max_length = 200) {
  text <- gsub("^\\s+|\\s+$", "", text)  # Remove leading and trailing whitespace
  text <- gsub("\\s+", " ", text)  # Replace multiple spaces with a single space
  if (nchar(text) > max_length) {
    text <- substr(text, 1, max_length - 23)  # Truncate to 177 characters (200 - 23 for the added text)
    text <- paste0(trimws(text), "... (As per the protocol)")
  }
  return(text)
}

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

extract_criteria <- function(text_with_pages, section_header, end_section, footnotes) {
  all_lines <- unlist(lapply(text_with_pages, function(page) strsplit(page$text, "\n")[[1]]))
  all_page_nums <- unlist(lapply(text_with_pages, function(page) rep(page$page_num, length(strsplit(page$text, "\n")[[1]]))))
  
  start_index <- which(grepl(section_header, all_lines, ignore.case = TRUE))
  end_index <- which(grepl(end_section, all_lines, ignore.case = TRUE))
  
  if (length(start_index) == 0) {
    warning(sprintf("Section header '%s' not found in the text.", section_header))
    return(list(criteria = character(0), pages = integer(0), subcategories = character(0)))
  }
  
  if (length(end_index) == 0 || end_index[1] <= start_index[1]) {
    end_index <- length(all_lines)
  } else {
    end_index <- end_index[1] - 1
  }
  
  criteria <- character(0)
  criteria_pages <- integer(0)
  criteria_subcategories <- character(0)
  current_criterion <- ""
  current_page <- all_page_nums[start_index[1]]
  current_subcategory <- ""
  
  # Pattern to match subsection headers (e.g., "4.1.1.1 Patients")
  subsection_pattern <- "^\\d+(\\.\\d+)+\\s+(.+)$"
  
  # Pattern to match criteria start
  criterion_start_pattern <- "^\\s*(•|\\*|\\d+\\.|-|[a-z]\\)|[A-Z]\\)|\\([a-z]\\)|\\([A-Z]\\))\\s+"
  
  for (i in (start_index[1] + 1):end_index) {
    line <- trimws(all_lines[i])
    if (nchar(line) == 0) next
    
    # Check for subsection headers
    if (grepl(subsection_pattern, line)) {
      current_subcategory <- gsub(subsection_pattern, "\\2", line)
      next
    }
    
    # Check for new criterion
    if (grepl(criterion_start_pattern, line) || nchar(current_criterion) == 0) {
      if (nchar(current_criterion) > 0) {
        criteria <- c(criteria, trimws(current_criterion))
        criteria_pages <- c(criteria_pages, current_page)
        criteria_subcategories <- c(criteria_subcategories, current_subcategory)
      }
      current_criterion <- gsub(criterion_start_pattern, "", line)
      current_page <- all_page_nums[i]
    } else {
      current_criterion <- paste(current_criterion, line)
    }
  }
  
  # Add the last criterion if exists
  if (nchar(current_criterion) > 0) {
    criteria <- c(criteria, trimws(current_criterion))
    criteria_pages <- c(criteria_pages, current_page)
    criteria_subcategories <- c(criteria_subcategories, current_subcategory)
  }
  
  return(list(criteria = criteria, pages = criteria_pages, subcategories = criteria_subcategories))
}

# Main function
create_ti_domain <- function(study_id, method, pdf_path = NULL, nct_id = NULL,
                             incl_range = NULL, excl_range = NULL,
                             incl_section = NULL, excl_section = NULL, end_section = NULL,
                             output_dir = getwd()) {
  if (method == "pdf") {
    if (is.null(pdf_path) || is.null(incl_range) || is.null(excl_range) ||
        is.null(incl_section) || is.null(excl_section) || is.null(end_section)) {
      stop("For PDF method, pdf_path, incl_range, excl_range, incl_section, excl_section, and end_section must be provided.")
    }
    return(create_ti_domain_pdf(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir))
  } else if (method == "api") {
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    return(create_ti_domain_api(study_id, nct_id, output_dir))
  } else {
    stop("Invalid method. Choose either 'pdf' or 'api'.")
  }
}

create_ti_domain_pdf <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir) {
  tryCatch({
    pdf_text <- pdftools::pdf_text(pdf_path)
    cleaned_text <- pdf_text[incl_range[1]:excl_range[length(excl_range)]]
    
    inclusion_text <- list(text = paste(cleaned_text[1:(excl_range[1] - incl_range[1])], collapse = "\n"), page_num = incl_range[1])
    exclusion_text <- list(text = paste(cleaned_text[(excl_range[1] - incl_range[1] + 1):length(cleaned_text)], collapse = "\n"), page_num = excl_range[1])
    
    inclusion_result <- extract_criteria(list(inclusion_text), incl_section, excl_section, character(0))
    exclusion_result <- extract_criteria(list(exclusion_text), excl_section, end_section, character(0))
    
    # Function to truncate IEORRES
    truncate_ieorres <- function(text) {
      if (nchar(text) > 200) {
        return(paste0(substr(text, 1, 196), "... (As per the protocol)"))
      }
      return(text)
    }

    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", sprintf("%03d", seq_along(inclusion_result$criteria))),
                   paste0("EXCL", sprintf("%03d", seq_along(exclusion_result$criteria)))),
      IETEST = c(rep("Inclusion Criteria", length(inclusion_result$criteria)),
                 rep("Exclusion Criteria", length(exclusion_result$criteria))),
      IECAT = c(inclusion_result$subcategories, exclusion_result$subcategories),
      IESCAT = "",
      IEORRES = sapply(c(inclusion_result$criteria, exclusion_result$criteria), truncate_ieorres),
      stringsAsFactors = FALSE
    )
    
    excel_file <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
    openxlsx::write.xlsx(ti_domain, excel_file)
    
    return(ti_domain)
  }, error = function(e) {
    message("Error in create_ti_domain_pdf: ", e$message)
    return(NULL)
  })
}

#' Create TI Domain using API method
#'
#' @param study_id A character string specifying the study ID.
#' @param nct_id NCT ID for the study.
#' @param output_dir Directory to save the output Excel file.
#'
#' @return A data frame containing the TI domain information.
#' @importFrom openxlsx write.xlsx
create_ti_domain_api <- function(study_id, nct_id, output_dir) {
  print("Starting create_ti_domain_api function")
  
  tryCatch({
    # Load required libraries
    library(httr)
    library(jsonlite)
    
    # Construct the API URL
    api_url <- paste0("https://clinicaltrials.gov/api/v2/studies/", nct_id)
    print(paste("API URL:", api_url))
    
    # Make the API request
    response <- GET(api_url, add_headers("Accept" = "application/json"))
    print(paste("API response status:", status_code(response)))
    
    # Check if the request was successful
    if (status_code(response) != 200) {
      stop(paste("API request failed with status code:", status_code(response)))
    }
    
    # Parse the JSON response
    content <- content(response, "text", encoding = "UTF-8")
    study_data <- fromJSON(content)
    
    # Extract eligibility criteria
    eligibility <- study_data$protocolSection$eligibilityModule$eligibilityCriteria
    
    if (is.null(eligibility) || length(eligibility) == 0) {
      stop("No eligibility criteria found in the API response.")
    }
    
    print("Eligibility criteria found. Processing...")
    
    # Process criteria
    criteria <- unlist(strsplit(eligibility, "\n"))
    
    # Identify inclusion and exclusion criteria
    incl_start <- which(grepl("Inclusion Criteria:", criteria, ignore.case = TRUE))
    excl_start <- which(grepl("Exclusion Criteria:", criteria, ignore.case = TRUE))
    
    if (length(incl_start) == 0 || length(excl_start) == 0) {
      print("Could not identify standard inclusion/exclusion headers. Using alternative method...")
      # Alternative method: Split criteria in half if headers not found
      mid_point <- ceiling(length(criteria) / 2)
      inclusion_criteria <- criteria[1:mid_point]
      exclusion_criteria <- criteria[(mid_point + 1):length(criteria)]
    } else {
      inclusion_criteria <- criteria[(incl_start + 1):(excl_start - 1)]
      exclusion_criteria <- criteria[(excl_start + 1):length(criteria)]
    }
    
    # Remove empty lines and trim whitespace
    inclusion_criteria <- trimws(inclusion_criteria[nzchar(inclusion_criteria)])
    exclusion_criteria <- trimws(exclusion_criteria[nzchar(exclusion_criteria)])
    
    print(paste("Number of inclusion criteria:", length(inclusion_criteria)))
    print(paste("Number of exclusion criteria:", length(exclusion_criteria)))
    
    # Function to truncate IEORRES and remove leading asterisk
    truncate_and_clean_ieorres <- function(text) {
      # Remove leading asterisk and whitespace
      text <- sub("^\\s*\\*\\s*", "", text)
      if (nchar(text) > 200) {
        return(paste0(substr(text, 1, 196), "... (As per the protocol)"))
      }
      return(text)
    }

    # Create the TI domain data frame
    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", sprintf("%03d", seq_along(inclusion_criteria))),
                   paste0("EXCL", sprintf("%03d", seq_along(exclusion_criteria)))),
      IETEST = c(rep("Inclusion Criteria", length(inclusion_criteria)),
                 rep("Exclusion Criteria", length(exclusion_criteria))),
      IECAT = "",
      IESCAT = "",
      IEORRES = sapply(c(inclusion_criteria, exclusion_criteria), truncate_and_clean_ieorres),
      stringsAsFactors = FALSE
    )
    
    # Save to Excel
    excel_file <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
    openxlsx::write.xlsx(ti_domain, excel_file)
    print(paste("Excel file saved:", excel_file))
    
    return(ti_domain)
  }, error = function(e) {
    print(paste("Error in create_ti_domain_api:", e$message))
    print("API response content:")
    print(content(response, "text", encoding = "UTF-8"))
    stop(e)
  })
}
