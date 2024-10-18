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
#' @param debug Logical, if TRUE, print debug messages. Default is FALSE.
#' @return A list containing the number of inclusion and exclusion criteria, first criteria of each, and output file location.
#' @export
#' @importFrom dplyr mutate select bind_rows group_by ungroup last
#' @importFrom stringr str_split str_extract str_replace_all str_trim str_locate_all str_sub str_locate fixed str_detect str_replace
#' @importFrom openxlsx write.xlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook
#' @importFrom pdftools pdf_text
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
create_ti_domain <- function(study_id, method, pdf_path = NULL, nct_id = NULL,
                             incl_range = NULL, excl_range = NULL,
                             incl_section = NULL, excl_section = NULL, end_section = NULL,
                             output_dir = getwd(), debug = FALSE) {
  # Debug: Print function entry
  if(debug) cat("Entering create_ti_domain function\n")
  
  # Validate input parameters
  if(debug) cat("Validating input parameters\n")
  validate_input(method, pdf_path, nct_id, incl_range, excl_range, incl_section, excl_section, end_section, debug)
  
  # Process based on method
  if (method == "pdf") {
    if(debug) cat("Processing PDF method\n")
    result <- process_pdf_method(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, debug)
    pdf_text <- pdftools::pdf_text(pdf_path)
  } else if (method == "api") {
    if(debug) cat("Processing API method\n")
    result <- process_api_method(study_id, nct_id, debug)
    pdf_text <- NULL
  }
  
  # Generate TI domain data frame
  if(debug) cat("Generating TI domain data frame\n")
  ti_domain <- generate_ti_domain(study_id, result$inclusion, result$exclusion, pdf_text, debug)
  
  # Save to Excel
  if(debug) cat("Saving to Excel\n")
  excel_file <- save_to_excel(ti_domain, study_id, output_dir, debug)
  
  # Prepare summary
  if(debug) cat("Preparing summary\n")
  summary <- prepare_summary(result, excel_file, debug)
  
  # Debug: Print function exit
  if(debug) cat("Exiting create_ti_domain function\n")
  
  # Return summary
  return(summary)
}

#' Extract Version Information
#'
#' This function extracts version information from the PDF text.
#'
#' @param text A character string containing the PDF text.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character string representing the version, or NA if not found.
#' @keywords internal
extract_version <- function(text, debug = FALSE) {
  if(debug) cat("Entering extract_version function\n")
  
  # Define patterns to match various version formats
  patterns <- c(
    "Protocol\\s+\\w+,\\s*(Version\\s*\\d+(\\.\\d+)*)",
    "(Version\\s*\\d+(\\.\\d+)*)",
    "(V\\s*\\d+(\\.\\d+)*)"
  )
  
  # Join all lines of text
  full_text <- paste(text, collapse = " ")
  
  for (pattern in patterns) {
    match <- stringr::str_match(full_text, pattern)
    if (!is.na(match[1,2])) {
      version <- match[1,2]
      # Remove "Version" or "V" prefix and any leading/trailing whitespace
      version <- gsub("^(Version|V)\\s*", "", version)
      version <- trimws(version)
      if(debug) cat("Version found:", version, "\n")
      return(version)
    }
  }
  
  if(debug) cat("No version information found\n")
  return(NA)
}

#' Validate Input Parameters
#'
#' This function validates the input parameters for the create_ti_domain function.
#'
#' @param method A character string, either "pdf" or "api", specifying the method to use for extraction.
#' @param pdf_path A character string representing the path to the protocol PDF.
#' @param nct_id A character string representing the NCT ID.
#' @param incl_range A numeric vector representing the page range for inclusion criteria.
#' @param excl_range A numeric vector representing the page range for exclusion criteria.
#' @param incl_section A character string representing the section identifier for inclusion criteria.
#' @param excl_section A character string representing the section identifier for exclusion criteria.
#' @param end_section A character string representing the section identifier for the end of criteria.
#' @param debug Logical, if TRUE, print debug messages.
#' @return NULL invisibly. Throws an error if validation fails.
#' @keywords internal
validate_input <- function(method, pdf_path, nct_id, incl_range, excl_range, 
                           incl_section, excl_section, end_section, debug = FALSE) {
  if(debug) cat("Entering validate_input function\n")
  
  # Validate method
  if(debug) cat("Validating method\n")
  if (!method %in% c("pdf", "api")) {
    stop("Invalid method. Use 'pdf' or 'api'.")
  }
  
  if (method == "pdf") {
    if(debug) cat("Validating PDF-specific parameters\n")
    
    # Check if all required PDF parameters are provided
    if (is.null(pdf_path) || is.null(incl_range) || is.null(excl_range) ||
        is.null(incl_section) || is.null(excl_section) || is.null(end_section)) {
      stop("For PDF method, pdf_path, incl_range, excl_range, incl_section, excl_section, and end_section must be provided.")
    }
    
    # Check if PDF file exists
    if (!file.exists(pdf_path)) {
      stop(sprintf("The specified PDF file does not exist: %s", pdf_path))
    }
    
    # Validate page ranges
    if (!is.numeric(incl_range) || !is.numeric(excl_range)) {
      stop("incl_range and excl_range must be numeric vectors.")
    }
    
    if (any(incl_range < 1) || any(excl_range < 1)) {
      stop("Page numbers in incl_range and excl_range must be positive integers.")
    }
    
    # Validate section identifiers
    if (!is.character(incl_section) || !is.character(excl_section) || !is.character(end_section)) {
      stop("incl_section, excl_section, and end_section must be character strings.")
    }
    
  } else if (method == "api") {
    if(debug) cat("Validating API-specific parameters\n")
    
    # Check if NCT ID is provided for API method
    if (is.null(nct_id)) {
      stop("For API method, nct_id must be provided.")
    }
    
    # Validate NCT ID format (simple check)
    if (!grepl("^NCT\\d{8}$", nct_id)) {
      stop(sprintf("Invalid NCT ID format: %s. Expected format: NCTxxxxxxxx", nct_id))
    }
  }
  
  if(debug) cat("Input validation completed successfully\n")
  if(debug) cat("Exiting validate_input function\n")
  
  invisible(NULL)
}

#' Clean and Preprocess Text
#'
#' This function cleans and preprocesses the extracted text.
#'
#' @param text A character string to be cleaned and preprocessed.
#' @return A cleaned and preprocessed character string.
#' @keywords internal
clean_and_preprocess_text <- function(text) {
  # Ensure text is an atomic vector
  if (!is.atomic(text)) {
    warning("Input text is not an atomic vector. Coercing to character.")
    text <- paste(unlist(text), collapse = "\n")
  }
  
  # Convert various bullet points and dashes to a standard bullet only at the start of lines
  text <- gsub("(?m)^\\s*[\u2022\u2023\u25E6\u2043\u2219\u2013\u2014-]", "\u2022 ", text, perl = TRUE)
  
  # Other replacements
  text <- gsub("\u2265", ">=", text, fixed = TRUE)
  text <- gsub("\u2264", "<=", text, fixed = TRUE)
  text <- gsub("\u2212", "-", text, fixed = TRUE)
  text <- gsub("\u00B1", "+/-", text, fixed = TRUE)
  
  # Ensure there's always a space after a bullet point
  text <- gsub("\u2022(?!\\s)", "\u2022 ", text, perl = TRUE)
  
  # Trim each line while preserving line breaks and indentation
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  lines <- sub("^\\s*(\\S.*?)\\s*$", "\\1", lines)
  text <- paste(lines, collapse = "\n")
  
  return(text)
}


#' Process PDF Method
#'
#' This function extracts inclusion and exclusion criteria from a PDF protocol.
#'
#' @param study_id A character string representing the Study ID.
#' @param pdf_path A character string representing the path to the protocol PDF.
#' @param incl_range A numeric vector representing the page range for inclusion criteria.
#' @param excl_range A numeric vector representing the page range for exclusion criteria.
#' @param incl_section A character string representing the section identifier for inclusion criteria.
#' @param excl_section A character string representing the section identifier for exclusion criteria.
#' @param end_section A character string representing the section identifier for the end of criteria.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list containing two elements: 'inclusion' and 'exclusion', each a character vector of criteria.
#' @importFrom pdftools pdf_text
#' @keywords internal
process_pdf_method <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, debug = FALSE) {
  if(debug) cat("Entering process_pdf_method function\n")
  
  # Extract text from the specified page ranges of the PDF
  pdf_text <- pdftools::pdf_text(pdf_path)

    # Ensure pdf_text is an atomic vector
  if (!is.atomic(pdf_text)) {
    warning("PDF text is not an atomic vector. Coercing to character.")
    pdf_text <- unlist(pdf_text)
  }
  
  inclusion_text <- paste(pdf_text[incl_range], collapse = "\n")
  exclusion_text <- paste(pdf_text[excl_range], collapse = "\n")
  
  # Clean and preprocess text
  inclusion_text <- clean_and_preprocess_text(inclusion_text)
  exclusion_text <- clean_and_preprocess_text(exclusion_text)
  
  if(debug) {
    cat("Preprocessed inclusion text:\n", substr(inclusion_text, 1, 500), "...\n")
    cat("Preprocessed exclusion text:\n", substr(exclusion_text, 1, 500), "...\n")
    cat("Inclusion section identifier:", incl_section, "\n")
    cat("Exclusion section identifier:", excl_section, "\n")
    cat("End section identifier:", end_section, "\n")
  }
  
  # Extract inclusion criteria
  if(debug) cat("Extracting inclusion criteria\n")
  inclusion_criteria <- extract_criteria(inclusion_text, incl_section, excl_section, debug)
  
  # Extract exclusion criteria
  if(debug) cat("Extracting exclusion criteria\n")
  exclusion_criteria <- extract_criteria(exclusion_text, excl_section, end_section, debug)
  
  if(debug) {
    cat(sprintf("Extracted %d inclusion criteria\n", length(inclusion_criteria)))
    cat(sprintf("Extracted %d exclusion criteria\n", length(exclusion_criteria)))
  }
  
  if(debug) cat("Exiting process_pdf_method function\n")
  
  return(list(inclusion = inclusion_criteria, exclusion = exclusion_criteria))
}

#' Extract Header and Footer Pattern
#'
#' This function extracts common header and footer patterns from PDF text.
#'
#' @param pdf_text A character vector containing the text of each page in the PDF.
#' @return A character vector of common patterns.
#' @keywords internal
extract_header_footer_pattern <- function(pdf_text) {
  common_patterns <- c()
  for (page_text in pdf_text) {
    lines <- stringr::str_split(page_text, "\n")[[1]]
    header <- lines[1]
    footer <- lines[length(lines)]
    common_patterns <- c(common_patterns, header, footer)
  }
  common_patterns <- unique(common_patterns)
  return(common_patterns)
}

#' Extract Section Text
#'
#' This helper function extracts text for a specific section from PDF text.
#'
#' @param pdf_text A character vector containing the text of each page in the PDF.
#' @param page_range A numeric vector specifying the page range to search.
#' @param start_section A character string specifying the start of the section.
#' @param end_section A character string specifying the end of the section.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character string containing the extracted section text.
#' @keywords internal
extract_section_text <- function(cleaned_pages, start_section, end_section, debug = FALSE) {
  if(debug) cat("Entering extract_section_text function\n")
  
  # Combine text from cleaned pages
  section_text <- paste(sapply(cleaned_pages, function(page) page$text), collapse = "\n")
  
  # Find start of section
  start_pos <- regexpr(start_section, section_text, ignore.case = TRUE)
  if(start_pos == -1) {
    stop(sprintf("Start section '%s' not found in specified page range", start_section))
  }
  
  # Find end of section
  end_pos <- regexpr(end_section, section_text, ignore.case = TRUE)
  if(end_pos == -1) {
    # If end section not found, use the end of the text
    end_pos <- nchar(section_text)
  }
  
  # Extract text between start and end positions
  extracted_text <- substr(section_text, start_pos, end_pos - 1)
  
  if(debug) {
    cat(sprintf("Extracted %d characters of text\n", nchar(extracted_text)))
    cat("First 10 lines of extracted text:\n")
    cat(paste(head(strsplit(extracted_text, "\n")[[1]], 10), collapse = "\n"), "\n...")
  }
  
  if(debug) cat("Exiting extract_section_text function\n")
  
  return(extracted_text)
}

#' Extract Criteria
#'
#' This function extracts criteria from the preprocessed text.
#'
#' @param text A character string containing the preprocessed text.
#' @param start_section A character string representing the start section identifier.
#' @param end_section A character string representing the end section identifier.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character vector of extracted criteria.
#' @keywords internal
extract_criteria <- function(text, start_section, end_section, debug = FALSE) {
  if(debug) cat("Entering extract_criteria function\n")
  
  # Ensure text is an atomic vector
  if (!is.atomic(text)) {
    if(debug) cat("Warning: text is not an atomic vector. Attempting to coerce.\n")
    text <- paste(unlist(text), collapse = "\n")
  }
  
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  lines <- trimws(lines)
  
  start_pattern <- stringr::regex(start_section, ignore_case = TRUE)
  end_pattern <- stringr::regex(end_section, ignore_case = TRUE)
  subsection_pattern <- "^\\d+\\.\\d+(\\.\\d+)*\\s+"
  
  # Define states
  BEFORE_START <- "before_start"
  IN_SECTION <- "in_section"
  IN_CRITERION <- "in_criterion"
  
  state <- BEFORE_START
  criteria <- list()
  current_criterion <- NULL
  current_subsection <- NULL
  main_marker_pattern <- NULL
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    if(debug) cat("Processing line", i, ":", stringi::stri_escape_unicode(line), "\n")
    
    if (state == BEFORE_START) {
      if (stringr::str_detect(line, start_pattern)) {
        state <- IN_SECTION
        if(debug) cat("Found start of section:", stringi::stri_escape_unicode(line), "\n")
      }
    } else if (state == IN_SECTION || state == IN_CRITERION) {
      if (stringr::str_detect(line, end_pattern)) {
        if(debug) cat("Found end of section:", stringi::stri_escape_unicode(line), "\n")
        break
      }
      
      # Check for subsection
      if (stringr::str_detect(line, subsection_pattern)) {
        current_subsection <- stringr::str_replace(line, subsection_pattern, "")
        if(debug) cat("Detected subsection:", current_subsection, "\n")
        next
      }
      
      # Detect main marker pattern if not already set
      if (is.null(main_marker_pattern)) {
        if (stringr::str_detect(line, "^\\s*[\uf0b7\u2022]")) {
          main_marker_pattern <- "^\\s*[\uf0b7\u2022]"
          if(debug) cat("Detected bullet point as main marker\n")
        } else if (stringr::str_detect(line, "^\\s*\\d+\\.")) {
          main_marker_pattern <- "^\\s*\\d+\\."
          if(debug) cat("Detected numerical list as main marker\n")
        }
      }
      
      # Check for new main criterion
      if (!is.null(main_marker_pattern) && stringr::str_detect(line, main_marker_pattern)) {
        if (!is.null(current_criterion)) {
          criteria <- c(criteria, list(current_criterion))
        }
        current_criterion <- list(
          text = stringr::str_replace(line, main_marker_pattern, ""),
          subsection = current_subsection
        )
        state <- IN_CRITERION
        if(debug) cat("New main criterion detected:", stringi::stri_escape_unicode(substr(current_criterion$text, 1, 50)), "...\n")
      } else if (stringr::str_detect(line, "^\\s*\u2013")) {
        # This is a sub-point, append to current criterion
        current_criterion$text <- paste(current_criterion$text, stringr::str_replace(line, "^\\s*\u2013\\s*", " - "))
        if(debug) cat("Sub-point detected:", stringi::stri_escape_unicode(substr(line, 1, 50)), "...\n")
      } else if (state == IN_CRITERION) {
        # If in a criterion, append the line
        current_criterion$text <- paste(current_criterion$text, line)
      }
    }
  }
  
  # Add the last criterion
  if (!is.null(current_criterion)) {
    criteria <- c(criteria, list(current_criterion))
  }
  
  # Handle text length
  criteria <- lapply(criteria, function(crit) {
    crit$text <- handle_text_length(crit$text, max_length = 200)
    return(crit)
  })
  
  if(debug) {
    cat("Extracted", length(criteria), "criteria\n")
    for(i in seq_along(criteria)) {
      cat("Criterion", i, ":", stringi::stri_escape_unicode(substr(criteria[[i]]$text, 1, 50)), "...\n")
      cat("Subsection:", criteria[[i]]$subsection, "\n")
    }
  }
  
  if(debug) cat("Exiting extract_criteria function\n")
  
  return(criteria)
}

#' Handle Text Length
#'
#' This function handles text exceeding a specified maximum length.
#'
#' @param text A character string to be processed.
#' @param max_length The maximum allowed length for the text.
#' @return A processed character string.
#' @keywords internal
handle_text_length <- function(text, max_length = 200) {
  suffix <- "(As per the protocol)"
  suffix_length <- nchar(suffix)
  
  if (nchar(text) > (max_length - suffix_length)) {
    truncated_text <- stringr::str_sub(text, 1, max_length - suffix_length - 1)
    last_space <- stringr::str_locate_all(truncated_text, " ")[[1]]
    if (!is.null(last_space) && nrow(last_space) > 0) {
      last_space_position <- last_space[nrow(last_space), 1]
      text <- stringr::str_sub(truncated_text, 1, last_space_position - 1)
    }
    text <- paste0(text, " ", suffix)
  }
  return(text)
}

#' Clean Criterion Text
#'
#' This function cleans and truncates the criterion text.
#'
#' @param text A character string of the criterion text.
#' @param max_length The maximum allowed length for the text. Default is 200.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A cleaned and potentially truncated character string.
#' @keywords internal
clean_criterion_text <- function(text, max_length = 200, debug = FALSE) {
  text <- gsub("^\\s*(\\d+\\.?|[a-z]\\.?|\\*|\\-|•)\\s*", "", text)

  # Remove any leading/trailing whitespace
  text <- trimws(text)
  
  # Replace multiple spaces with a single space
  text <- gsub("\\s+", " ", text)
  
  # Truncate if necessary
  if (nchar(text) > max_length) {
    if(debug) cat("Cleaning and truncating criterion\n")
    suffix <- " As Per Protocol"
    truncated <- substr(text, 1, max_length - nchar(suffix))
    text <- paste0(truncated, suffix)
    if(debug) cat(sprintf("Final criterion length: %d characters\n", nchar(text)))
  }
  
  return(text)
}

#' Remove Introductory Text
#'
#' This helper function removes common introductory phrases from criteria text.
#'
#' @param text A character string to process.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character string with introductory text removed.
#' @keywords internal
remove_introductory_text <- function(text, debug = FALSE) {
  if(debug) cat("Entering remove_introductory_text function\n")
  
  # Define flexible patterns for introductory text
  intro_patterns <- c(
    "(?i)^.*?inclusion\\s*criteria\\s*:?\\s*$",
    "(?i)^.*?exclusion\\s*criteria\\s*:?\\s*$"
  )
  
  # Combine all patterns into a single regex
  combined_pattern <- paste(intro_patterns, collapse="|")
  
  # Remove introductory text
  new_text <- gsub(combined_pattern, "", text, ignore.case = TRUE)
  
  if (debug) {
    cat("Original text (first 200 chars):\n", substr(text, 1, 200), "...\n")
    cat("Text after removing introductory phrases (first 200 chars):\n", substr(new_text, 1, 200), "...\n")
  }
  
  if(debug) cat("Exiting remove_introductory_text function\n")
  
  return(trimws(new_text))
}

#' Process API Method
#'
#' This function extracts inclusion and exclusion criteria from ClinicalTrials.gov API.
#'
#' @param study_id A character string representing the Study ID.
#' @param nct_id A character string representing the NCT ID.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list containing two elements: 'inclusion' and 'exclusion', each a character vector of criteria.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @keywords internal
process_api_method <- function(study_id, nct_id, debug = TRUE) {
  if(debug) cat("Entering process_api_method function\n")
  
  # Fetch data from ClinicalTrials.gov API
  if(debug) cat("Fetching data from ClinicalTrials.gov API\n")
  api_data <- tryCatch({
    fetch_clinicaltrials_data(nct_id, debug = debug, save_json = TRUE)
  }, error = function(e) {
    warning(sprintf("Failed to fetch data from ClinicalTrials.gov API: %s", e$message))
    return(NULL)
  })
  
  if(is.null(api_data)) {
    if(debug) cat("API data fetch failed. Returning empty criteria list.\n")
    return(list(inclusion = list(), exclusion = list()))
  }
  
  # Print the structure of api_data
  if(debug) {
    cat("Structure of api_data:\n")
    print(str(api_data, max.level = 2))
  }
  
  # Extract eligibility criteria
  if(debug) cat("Extracting eligibility criteria from API data\n")
  eligibility_module <- api_data$protocolSection$eligibilityModule
  
  if(is.null(eligibility_module)) {
    if(debug) cat("No eligibility module found. Returning empty criteria list.\n")
    return(list(inclusion = list(), exclusion = list()))
  }
  
  # Print the structure of eligibility_module
  if(debug) {
    cat("Structure of eligibility_module:\n")
    print(str(eligibility_module))
  }
  
  eligibility_criteria <- eligibility_module$eligibilityCriteria
  
  if(is.null(eligibility_criteria) || eligibility_criteria == "") {
    if(debug) cat("No eligibility criteria found. Returning empty criteria list.\n")
    return(list(inclusion = list(), exclusion = list()))
  }
  
  # Print the eligibility criteria
  if(debug) {
    cat("Eligibility criteria:\n")
    cat(eligibility_criteria, "\n")
  }
  
  # Separate inclusion and exclusion criteria
  if(debug) cat("Separating inclusion and exclusion criteria\n")
  criteria_list <- separate_criteria(eligibility_criteria, debug)
  
  if(debug) {
    cat(sprintf("Extracted %d inclusion criteria\n", length(criteria_list$inclusion)))
    cat(sprintf("Extracted %d exclusion criteria\n", length(criteria_list$exclusion)))
  }
  
  if(debug) cat("Exiting process_api_method function\n")
  
  return(criteria_list)
}

#' Fetch ClinicalTrials.gov Data
#'
#' This helper function fetches study data from the ClinicalTrials.gov API.
#'
#' @param nct_id A character string representing the NCT ID.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list containing the API response data.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @keywords internal
fetch_clinicaltrials_data <- function(nct_id, debug = FALSE, save_json = TRUE) {
  if(debug) cat("Entering fetch_clinicaltrials_data function\n")
  
  api_url <- paste0("https://clinicaltrials.gov/api/v2/studies/", nct_id)
  
  if(debug) cat(sprintf("Sending GET request to: %s\n", api_url))
  
  response <- tryCatch({
    httr::GET(api_url, httr::add_headers("Accept" = "application/json"))
  }, error = function(e) {
    stop(sprintf("Error fetching data from ClinicalTrials.gov API: %s", e$message))
  })
  
  if(httr::status_code(response) != 200) {
    if(httr::status_code(response) == 404) {
      stop(sprintf("Study with NCT ID %s not found. Please check if the NCT ID is correct.", nct_id))
    } else {
      stop(sprintf("API request failed with status code: %d. Please try again later or check your internet connection.", httr::status_code(response)))
    }
  }
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Save JSON to file
  if(save_json) {
    json_file <- paste0(nct_id, "_data.json")
    writeLines(content, json_file)
    if(debug) cat(sprintf("JSON data saved to file: %s\n", json_file))
  }
  
  data <- jsonlite::fromJSON(content)
  
  if(debug) cat("Successfully fetched and parsed API data\n")
  if(debug) cat("Exiting fetch_clinicaltrials_data function\n")
  
  return(data)
}

#' Extract Eligibility Criteria
#'
#' This helper function extracts eligibility criteria from the API response data.
#'
#' @param api_data A list containing the API response data.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character string containing the eligibility criteria.
#' @keywords internal
extract_eligibility_criteria <- function(api_data, debug = FALSE) {
  if(debug) cat("Entering extract_eligibility_criteria function\n")
  
  eligibility_module <- api_data$FullStudiesResponse$FullStudies$Study$ProtocolSection$EligibilityModule
  
  if(is.null(eligibility_module) || is.null(eligibility_module$EligibilityCriteria)) {
    stop("Eligibility criteria not found in API response")
  }
  
  criteria <- eligibility_module$EligibilityCriteria
  
  if(debug) cat(sprintf("Extracted %d characters of eligibility criteria\n", nchar(criteria)))
  if(debug) cat("Exiting extract_eligibility_criteria function\n")
  
  return(criteria)
}

#' Separate Criteria
#'
#' This helper function separates the eligibility criteria into inclusion and exclusion criteria.
#'
#' @param criteria A character string containing the eligibility criteria.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list with two elements: 'inclusion' and 'exclusion', each a character vector of criteria.
#' @keywords internal
separate_criteria <- function(eligibility_text, debug = TRUE) {
  if(debug) cat("Entering separate_criteria function\n")
  
  # Split the text into lines
  lines <- unlist(strsplit(eligibility_text, "\n"))
  lines <- trimws(lines)
  
  # Initialize variables
  inclusion <- character()
  exclusion <- character()
  current_list <- NULL
  current_criterion <- NULL
  
  # Regular expression to match various list formats
  list_pattern <- "^\\s*(\\d+\\.?|[a-z]\\.?|\\*|\\-|•)\\s*"
  
  for(i in seq_along(lines)) {
    line <- lines[i]
    if(debug) cat("Processing line", i, ":", line, "\n")
    
    if(grepl("Inclusion Criteria", line, ignore.case = TRUE)) {
      current_list <- "inclusion"
      if(debug) cat("Switched to inclusion criteria\n")
    } else if(grepl("Exclusion Criteria", line, ignore.case = TRUE)) {
      current_list <- "exclusion"
      if(debug) cat("Switched to exclusion criteria\n")
    } else if(!is.null(current_list) && nchar(line) > 0) {
      # Remove any list markers at the beginning of the line
      clean_line <- gsub(list_pattern, "", line)
      
      if(grepl(list_pattern, line) || is.null(current_criterion)) {
        # This is a new criterion
        if(!is.null(current_criterion)) {
          if(current_list == "inclusion") {
            inclusion <- c(inclusion, current_criterion)
          } else {
            exclusion <- c(exclusion, current_criterion)
          }
        }
        current_criterion <- clean_line
        if(debug) cat("New criterion added:", current_criterion, "\n")
      } else {
        # This is a continuation of the previous criterion
        current_criterion <- paste(current_criterion, clean_line)
        if(debug) cat("Appended to previous criterion:", clean_line, "\n")
      }
    }
  }
  
  # Add the last criterion
  if(!is.null(current_criterion)) {
    if(current_list == "inclusion") {
      inclusion <- c(inclusion, current_criterion)
    } else {
      exclusion <- c(exclusion, current_criterion)
    }
  }
  
  # Clean up criteria
  inclusion <- sapply(inclusion, clean_criterion_text)
  exclusion <- sapply(exclusion, clean_criterion_text)
  
  if(debug) {
    cat(sprintf("Separated %d inclusion criteria\n", length(inclusion)))
    cat(sprintf("Separated %d exclusion criteria\n", length(exclusion)))
  }
  
  if(debug) cat("Exiting separate_criteria function\n")
  
  return(list(inclusion = inclusion, exclusion = exclusion))
}


#' Clean Criteria List
#'
#' This helper function cleans a list of criteria.
#'
#' @param criteria_list A character vector of criteria.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A cleaned character vector of criteria.
#' @keywords internal
clean_criteria_list <- function(criteria_list, debug = FALSE) {
  if(debug) cat("Entering clean_criteria_list function\n")
  
  # Remove empty lines
  criteria_list <- criteria_list[nzchar(trimws(criteria_list))]
  
  # Remove bullet points and leading/trailing whitespace
  criteria_list <- sapply(criteria_list, function(x) {
    x <- sub("^\\s*[•\\-–]|^\\s*\\d+\\.\\s*", "", x)
    trimws(x)
  })
  
  # Combine lines that don't start with a bullet point or number
  combined_criteria <- character(0)
  current_criterion <- ""
  
  for(line in criteria_list) {
    if(grepl("^[•\\-–]|^\\d+\\.", line)) {
      if(nzchar(current_criterion)) {
        combined_criteria <- c(combined_criteria, current_criterion)
      }
      current_criterion <- line
    } else {
      current_criterion <- paste(current_criterion, line)
    }
  }
  
  if(nzchar(current_criterion)) {
    combined_criteria <- c(combined_criteria, current_criterion)
  }
  
  if(debug) cat(sprintf("Cleaned and combined %d criteria\n", length(combined_criteria)))
  if(debug) cat("Exiting clean_criteria_list function\n")
  
  return(combined_criteria)
}

#' Generate TI Domain
#'
#' This function generates the TI (Trial Inclusion/Exclusion Criteria) domain data frame
#' from the extracted inclusion and exclusion criteria.
#'
#' @param study_id A character string representing the Study ID.
#' @param inclusion A list or vector of inclusion criteria.
#' @param exclusion A list or vector of exclusion criteria.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A data frame representing the TI domain.
#' @importFrom dplyr mutate
#' @keywords internal
generate_ti_domain <- function(study_id, inclusion_criteria, exclusion_criteria, pdf_text = NULL, debug = FALSE) {
  if(debug) cat("Entering generate_ti_domain function\n")
  
  # Extract version information if pdf_text is provided
  tivers <- if (!is.null(pdf_text)) extract_version(pdf_text, debug) else NA
  
  # Debug: Print the lengths of inclusion and exclusion criteria
  if(debug) {
    cat("Number of inclusion criteria:", length(inclusion_criteria), "\n")
    cat("Number of exclusion criteria:", length(exclusion_criteria), "\n")
    cat("TIVERS:", tivers, "\n")
    
    # Print the structure of the first inclusion and exclusion criteria
    if(length(inclusion_criteria) > 0) {
      cat("Structure of first inclusion criterion:\n")
      print(str(inclusion_criteria[[1]]))
    }
    if(length(exclusion_criteria) > 0) {
      cat("Structure of first exclusion criterion:\n")
      print(str(exclusion_criteria[[1]]))
    }
  }
  
  # Create empty data frame
  ti_domain <- data.frame(
    STUDYID = character(),
    DOMAIN = character(),
    IETESTCD = character(),
    IETEST = character(),
    IECAT = character(),
    IESCAT = character(),
    TIRL = character(),
    TIVERS = character(),
    stringsAsFactors = FALSE
  )
  
  # Add inclusion criteria
  for (i in seq_along(inclusion_criteria)) {
    criterion <- inclusion_criteria[[i]]
    ti_domain <- rbind(ti_domain, data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = paste0("INCL", sprintf("%02d", i)),
      IETEST = if(is.character(criterion)) criterion else criterion$text,
      IECAT = "INCLUSION",
      IESCAT = if(is.list(criterion) && !is.null(criterion$subsection)) criterion$subsection else "",
      TIRL = "",
      TIVERS = tivers,
      stringsAsFactors = FALSE
    ))
  }
  
  # Add exclusion criteria
  for (i in seq_along(exclusion_criteria)) {
    criterion <- exclusion_criteria[[i]]
    ti_domain <- rbind(ti_domain, data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = paste0("EXCL", sprintf("%02d", i)),
      IETEST = if(is.character(criterion)) criterion else criterion$text,
      IECAT = "EXCLUSION",
      IESCAT = if(is.list(criterion) && !is.null(criterion$subsection)) criterion$subsection else "",
      TIRL = "",
      TIVERS = tivers,
      stringsAsFactors = FALSE
    ))
  }
  
  if(debug) {
    cat("Generated TI domain with", nrow(ti_domain), "rows\n")
    print(head(ti_domain))
  }
  
  if(debug) cat("Exiting generate_ti_domain function\n")
  
  return(ti_domain)
}

#' Save TI Domain to Excel
#'
#' This function saves the TI domain data frame to an Excel file with proper formatting.
#'
#' @param ti_domain A data frame containing the TI domain data.
#' @param study_id A character string representing the Study ID.
#' @param output_dir A character string representing the output directory.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A character string representing the path to the saved Excel file.
#' @importFrom openxlsx createWorkbook addWorksheet writeData setColWidths createStyle addStyle saveWorkbook
#' @keywords internal
save_to_excel <- function(ti_domain, study_id, output_dir, debug = FALSE) {
  if(debug) cat("Entering save_to_excel function\n")
  
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Add a worksheet
  openxlsx::addWorksheet(wb, "TI")
  
  # Trim leading and trailing whitespace from all character columns
  ti_domain[] <- lapply(ti_domain, function(x) if(is.character(x)) trimws(x) else x)
  
  # Write the data to the worksheet
  openxlsx::writeData(wb, "TI", ti_domain)
  
  # Create a style for left alignment
  left_style <- openxlsx::createStyle(halign = "left")
  
  # Apply the left-aligned style to all cells
  openxlsx::addStyle(wb, "TI", style = left_style, rows = 1:(nrow(ti_domain) + 1), cols = 1:ncol(ti_domain), gridExpand = TRUE)
  
  # Set column widths (adjust as needed)
  openxlsx::setColWidths(wb, "TI", cols = 1:ncol(ti_domain), widths = "auto")
  
  # Create the file name
  file_name <- paste0(study_id, "_TI.xlsx")
  file_path <- file.path(output_dir, file_name)
  
  # Save the workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  if(debug) cat("Excel file saved:", file_path, "\n")
  if(debug) cat("Exiting save_to_excel function\n")
  
  return(file_path)
}

#' Create Header Style
#'
#' This helper function creates a style for the header row in the Excel file.
#'
#' @param wb An openxlsx workbook object.
#' @return An openxlsx style object.
#' @keywords internal
create_header_style <- function(wb) {
  openxlsx::createStyle(
    wb,
    fontSize = 12,
    fontColour = "#FFFFFF",
    halign = "center",
    fgFill = "#4F81BD",
    border = "TopBottom",
    borderColour = "#4F81BD",
    wrapText = TRUE
  )
}

#' Create Data Style
#'
#' This helper function creates a style for the data rows in the Excel file.
#'
#' @param wb An openxlsx workbook object.
#' @return An openxlsx style object.
#' @keywords internal
create_data_style <- function(wb) {
  openxlsx::createStyle(
    wb,
    fontSize = 11,
    halign = "left",
    border = "TopBottomLeftRight",
    borderColour = "#D9D9D9",
    wrapText = TRUE
  )
}

#' Prepare Summary of TI Domain Creation
#'
#' This function prepares a summary of the TI domain creation process, including
#' key statistics, the output file location, and the first 10 lines of inclusion criteria.
#'
#' @param result A list containing the extracted inclusion and exclusion criteria.
#' @param excel_file A character string representing the path to the saved Excel file.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list containing summary information.
#' @importFrom stringr str_split
#' @keywords internal
prepare_summary <- function(result, excel_file, debug = FALSE) {
  if(debug) cat("Entering prepare_summary function\n")
  
  summary <- list(
    num_inclusion = length(result$inclusion),
    num_exclusion = length(result$exclusion),
    first_inclusion = if(length(result$inclusion) > 0) {
      if(is.list(result$inclusion[[1]])) result$inclusion[[1]]$text else result$inclusion[[1]]
    } else "No inclusion criteria found",
    first_exclusion = if(length(result$exclusion) > 0) {
      if(is.list(result$exclusion[[1]])) result$exclusion[[1]]$text else result$exclusion[[1]]
    } else "No exclusion criteria found",
    inclusion_lines = paste(sapply(result$inclusion[1:min(10, length(result$inclusion))], function(x) {
      if(is.list(x)) x$text else x
    }), collapse = "\n"),
    output_location = excel_file
  )
  
  if(debug) {
    cat("Summary prepared:\n")
    cat("Number of inclusion criteria:", summary$num_inclusion, "\n")
    cat("Number of exclusion criteria:", summary$num_exclusion, "\n")
    cat("First inclusion criterion:", summary$first_inclusion, "\n")
    cat("First exclusion criterion:", summary$first_exclusion, "\n")
    cat("First 10 lines of inclusion criteria:\n", summary$inclusion_lines, "\n")
    cat("Output file location:", summary$output_location, "\n")
  }
  
  if(debug) cat("Exiting prepare_summary function\n")
  
  return(summary)
}

#' Truncate Text
#'
#' This helper function truncates text to a specified length, adding an ellipsis if truncated.
#'
#' @param text A character string to be truncated.
#' @param max_length The maximum allowed length for the text.
#' @return A truncated character string.
#' @keywords internal
truncate_text <- function(text, max_length) {
  if (nchar(text) <= max_length) {
    return(text)
  } else {
    truncated <- substr(text, 1, max_length - 3)
    return(paste0(truncated, "..."))
  }
}

#' Remove Footnotes and Headers
#'
#' This function removes footnotes and headers from the extracted PDF text.
#'
#' @param pages A character vector of page contents.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A list containing cleaned pages and debug information.
#' @keywords internal
remove_footnotes_and_headers <- function(pages, debug = FALSE) {
  debug_info <- ""
  
  tryCatch({
    # Split each page into lines and add page numbers
    page_lines <- lapply(seq_along(pages), function(page_num) {
      lines <- strsplit(pages[page_num], "\n")[[1]]
      list(lines = lines, page_num = page_num)
    })
    
    if(debug) {
      debug_info <- paste0(debug_info, sprintf("Number of pages: %d\n", length(page_lines)))
    }
    
    # Remove empty pages
    non_empty_pages <- page_lines[sapply(page_lines, function(page) length(page$lines) > 0)]
    
    if(debug) {
      debug_info <- paste0(debug_info, sprintf("Number of non-empty pages: %d\n", length(non_empty_pages)))
    }
    
    if (length(non_empty_pages) == 0) {
      stop("All pages are empty")
    }
    
    # Identify and remove potential footnotes and headers
    cleaned_pages <- lapply(non_empty_pages, function(page) {
      cleaned_lines <- page$lines
      # Remove first line (potential header) and last two lines (potential footnotes)
      if(length(cleaned_lines) > 3) {
        cleaned_lines <- cleaned_lines[2:(length(cleaned_lines)-2)]
      }
      list(text = paste(cleaned_lines, collapse = "\n"), page_num = page$page_num)
    })
    
    if(debug) {
      debug_info <- paste0(debug_info, sprintf("Number of pages after cleaning: %d\n", length(cleaned_pages)))
    }
    
    return(list(cleaned_pages = cleaned_pages, debug_info = debug_info))
  }, error = function(e) {
    error_msg <- paste("Error in remove_footnotes_and_headers:", e$message, "\n")
    return(list(cleaned_pages = pages, debug_info = paste0(debug_info, error_msg)))
  })
}

#' Identify and Extract Criteria
#'
#' This function identifies and extracts criteria from the PDF text.
#'
#' @param pdf_path A character string representing the path to the PDF file.
#' @param page_range A numeric vector specifying the page range to extract.
#' @param start_section A character string representing the start section marker.
#' @param end_section A character string representing the end section marker.
#' @param debug Logical, if TRUE, print debug messages.
#' @importFrom stringr str_split str_extract str_replace_all str_trim str_locate_all str_sub str_locate fixed str_detect str_replace
#' @importFrom pdftools pdf_text
identify_and_extract_criteria <- function(pdf_path, page_range, start_section, end_section, debug = FALSE) {
  if(debug) cat("Entering identify_and_extract_criteria function\n")
  
  # Extract text from specified pages
  pdf_text <- pdftools::pdf_text(pdf_path)[page_range]
  full_text <- paste(pdf_text, collapse = "\n")
  
  # Find the start and end of the relevant section
  start_pos <- stringr::str_locate(full_text, start_section)
  end_pos <- stringr::str_locate(full_text, end_section)
  
  if(is.na(start_pos[1]) || is.na(end_pos[1])) {
    stop("Could not find start or end section")
  }
  
  relevant_text <- stringr::str_sub(full_text, start_pos[1], end_pos[1] - 1)
  
  # Split text into lines
  lines <- stringr::str_split(relevant_text, "\n")[[1]]
  
  # Identify potential list markers
  numbered_pattern <- "^\\s*(\\d+\\.)"
  bullet_pattern <- "^\\s*(•|-)"
  alpha_pattern <- "^\\s*([a-z]\\))"
  
  # Count occurrences of each type of marker
  numbered_count <- sum(stringr::str_detect(lines, numbered_pattern))
  bullet_count <- sum(stringr::str_detect(lines, bullet_pattern))
  alpha_count <- sum(stringr::str_detect(lines, alpha_pattern))
  
  # Determine the most common marker type
  marker_counts <- c(numbered = numbered_count, bullet = bullet_count, alpha = alpha_count)
  most_common_marker <- names(which.max(marker_counts))
  
  if(debug) cat("Most common marker type:", most_common_marker, "\n")
  
  # Set the appropriate pattern based on the most common marker
  if(most_common_marker == "numbered") {
    pattern <- numbered_pattern
  } else if(most_common_marker == "bullet") {
    pattern <- bullet_pattern
  } else {
    pattern <- alpha_pattern
  }
  
  # Extract criteria using the identified marker
  criteria <- list()
  current_criterion <- NULL
  
  for(line in lines) {
    if(stringr::str_detect(line, pattern)) {
      if(!is.null(current_criterion)) {
        criteria <- c(criteria, list(clean_criterion_text(current_criterion)))
      }
      current_criterion <- stringr::str_replace(line, pattern, "")
    } else if(!is.null(current_criterion)) {
      current_criterion <- paste(current_criterion, line)
    }
  }
  
  # Add the last criterion
  if(!is.null(current_criterion)) {
    criteria <- c(criteria, list(clean_criterion_text(current_criterion)))
  }
  
  if(debug) {
    cat("Extracted", length(criteria), "criteria\n")
    for(i in seq_along(criteria)) {
      cat("Criterion", i, ":", substr(criteria[[i]], 1, 50), "...\n")
    }
  }
  
  if(debug) cat("Exiting identify_and_extract_criteria function\n")
  
  return(criteria)
}
#'
#' This function cleans and truncates the criterion text.
#'
#' @param text A character string of the criterion text.
#' @param max_length The maximum allowed length for the text. Default is 200.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A cleaned and potentially truncated character string.
#' @keywords internal
clean_criterion_text <- function(text, max_length = 200, debug = FALSE) {
  # Remove section numbers
  text <- gsub("^\\s*(\\d+\\.?|[a-z]\\.?|\\*|\\-|•)\\s*", "", text)
  
  # Remove any leading/trailing whitespace
  text <- trimws(text)
  
  # Replace multiple spaces with a single space
  text <- gsub("\\s+", " ", text)
  
  # Truncate if necessary
  if (nchar(text) > max_length) {
    if(debug) cat("Cleaning and truncating criterion\n")
    truncated <- substr(text, 1, max_length - 3)
    text <- paste0(truncated, "...")
    if(debug) cat(sprintf("Final criterion length: %d characters\n", nchar(text)))
  }
  
  return(text)
}

#' Clean and Truncate Criterion
#'
#' This function cleans and truncates the criterion text.
#'
#' @param text A character string of the criterion text.
#' @param max_length The maximum allowed length for the text. Default is 200.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A cleaned and potentially truncated character string.
#' @keywords internal
clean_and_truncate_criterion <- function(text, max_length = 200, debug = FALSE) {
  if(debug) cat("Cleaning and truncating criterion\n")
  
  # Remove any leading/trailing whitespace
  text <- trimws(text)
  
  # Replace multiple spaces with a single space
  text <- gsub("\\s+", " ", text)
  
  # Truncate if necessary
  if (nchar(text) > max_length) {
    truncated <- substr(text, 1, max_length - 3)
    text <- paste0(truncated, "...")
    if(debug) cat(sprintf("Truncated criterion to %d characters\n", nchar(text)))
  }
  
  if(debug) cat(sprintf("Final criterion length: %d characters\n", nchar(text)))
  
  return(text)
}

#' Clean Criterion Text
#'
#' This function cleans the criterion text.
#'
#' @param text A character string of the criterion text.
#' @param debug Logical, if TRUE, print debug messages.
#' @return A cleaned character string.
#' @keywords internal
clean_criterion <- function(text, debug = FALSE) {
  if(debug) cat("Cleaning criterion text\n")
  
  # Remove any leading/trailing whitespace
  text <- trimws(text)
  
  # Replace multiple spaces with a single space
  text <- gsub("\\s+", " ", text)
  
  if(debug) cat(sprintf("Cleaned criterion length: %d characters\n", nchar(text)))
  
  return(text)
}

#' Process Criterion Text
#'
#' This helper function processes the criterion text to fit within 200 characters
#' and removes any remaining section or subsection information.
#'
#' @param text A character string containing the criterion text.
#' @return A processed character string.
#' @keywords internal
process_criterion_text <- function(text) {
  # Clean and truncate the text if necessary
  text <- clean_criterion_text(text)
  if (nchar(text) > 200) {
    text <- paste0(substr(text, 1, 178), "(As per the protocol)")
  }
  return(text)
}