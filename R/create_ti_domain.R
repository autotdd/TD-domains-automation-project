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

  remove_introductory_text <- function(text) {
  # Define dynamic patterns for both inclusion and exclusion criteria introductory text
  intro_patterns <- c(
    "(?i)Inclusion\\s*Criteria", 
    "(?i)Exclusion\\s*Criteria", 
    "(?i)Patients\\s*(must|should)\\s*(meet|satisfy|fulfill)\\s*the\\s*following\\s*(criteria|requirements)\\s*(for\\s*study\\s*entry|to\\s*participate):?",
    "(?i)Eligibility\\s*(Criteria|Requirements)\\s*(for|to)\\s*(Study\\s*Entry|Participation):?",
    "(?i)Patients\\s*(who\\s*meet\\s*any|that\\s*meet\\s*the\\s*following|eligible\\s*for|who\\s*meet\\s*the\\s*criteria)\\s*will\\s*be\\s*(excluded\\s*from|included\\s*in)\\s*the\\s*study\\s*(entry|participation):?",
    "(?i)To\\s*be\\s*(eligible|included),\\s*patients\\s*must\\s*(meet|satisfy|fulfill)\\s*the\\s*(following\\s*)?(criteria|requirements):?",
    "(?i)(Key|Main|Primary)\\s*(inclusion|exclusion)\\s*(criteria|requirements):?"
  )
  
  # Dynamically identify and remove any introductory text matching the above patterns
  for (pattern in intro_patterns) {
    text <- gsub(pattern, "", text)
  }
  
  # Preserve the newlines and original structure of the text
  text <- gsub("\\s*\\n\\s*", "\n", text)  # Ensure newlines are preserved between criteria
  
  # Remove any redundant leading punctuation or spaces
  text <- gsub("^[\\s,;:.\\-]+", "", text)  # Clean up leading punctuation
  
  # Remove any redundant spaces within the text (but not newlines)
  text <- str_replace_all(text, "\\s{2,}", " ") # Replace multiple spaces with a single space
  text <- str_trim(text) # Trim leading and trailing spaces

  return(text)
}
remove_bullet_points <- function(text) {
  # Remove common bullet points or similar characters like • or 
  text <- gsub("^[•-]+\\s*", "", text)
  
  # You can add other special characters to remove if necessary
  return(text)
}


  # Internal function: extract_ti_domain
  extract_ti_domain <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section) {
    # Check if the protocol PDF file exists
    if (!file.exists(pdf_path)) {
      warning("The protocol PDF file is missing. Please ensure the file is named correctly.")
      return(NULL)
    }

    # Extract text from the specified page ranges of the PDF
    pdf_text <- pdftools::pdf_text(pdf_path)
    inclusion_text <- paste(pdf_text[incl_range], collapse = "\n")
    exclusion_text <- paste(pdf_text[excl_range], collapse = "\n")

    # Clean the inclusion and exclusion text
    inclusion_text <- replace_special_chars_and_trim(inclusion_text)
    exclusion_text <- replace_special_chars_and_trim(exclusion_text)

    # Extract Inclusion and Exclusion Criteria
    inclusion_start <- str_locate(inclusion_text, incl_section)[1, 2]
    exclusion_start <- str_locate(inclusion_text, excl_section)[1, 1]
    
    if (is.na(inclusion_start) | is.na(exclusion_start)) {
      stop("Section headers for inclusion or exclusion were not found in the text.")
    }
    
    inclusion_text <- str_sub(inclusion_text, inclusion_start + 1, exclusion_start - 1)

    exclusion_start <- str_locate(exclusion_text, excl_section)[1, 2]
    end_section_pattern <- paste0("\\n", gsub("\\.", "\\\\.", end_section))
    exclusion_end <- str_locate(exclusion_text, end_section_pattern)[1, 1]
    exclusion_text <- if (!is.na(exclusion_end)) {
      str_sub(exclusion_text, exclusion_start + 1, exclusion_end - 1)
    } else {
      str_sub(exclusion_text, exclusion_start + 1)
    }

    # Handle bullet points and split criteria
    inclusion_text <- str_replace_all(inclusion_text, "", "\n")
    exclusion_text <- str_replace_all(exclusion_text, "", "\n")

    # After extracting the inclusion and exclusion text
    inclusion_text <- remove_introductory_text(inclusion_text)
    inclusion_text <- remove_bullet_points(inclusion_text)

    exclusion_text <- remove_introductory_text(exclusion_text)
    exclusion_text <- remove_bullet_points(exclusion_text)

    inclusion_criteria <- str_trim(unlist(str_split(inclusion_text, "\n")))
    exclusion_criteria <- str_trim(unlist(str_split(exclusion_text, "\n")))

    # Remove empty strings
    inclusion_criteria <- inclusion_criteria[inclusion_criteria != ""]
    exclusion_criteria <- exclusion_criteria[exclusion_criteria != ""]
    
    # Handle text length exceeding 200 characters
    inclusion_criteria <- sapply(inclusion_criteria, handle_text_length, max_length = 200)
    exclusion_criteria <- sapply(exclusion_criteria, handle_text_length, max_length = 200)
    
    # Return the results
    return(list(inclusion = inclusion_criteria, exclusion = exclusion_criteria))
  }

  # Internal helper function: replace_special_chars_and_trim
replace_special_chars_and_trim <- function(text) {
    # Replace common scientific symbols
    text <- str_replace_all(text, "", ">=")    # Greater than or equal to
    text <- str_replace_all(text, "", "<=")    # Less than or equal to
    text <- str_replace_all(text, "", "<")     # Less than
    text <- str_replace_all(text, "", ">")     # Greater than
    text <- str_replace_all(text, "", "-")     # Dash
    text <- str_replace_all(text, "×", "x")     # Multiplication sign
    text <- str_replace_all(text, "−", "-")     # Minus sign
    text <- str_replace_all(text, "°", " degrees")  # Degrees symbol
    text <- str_replace_all(text, "µ", "u")     # Micro symbol (µ -> u)
    text <- str_replace_all(text, "±", "+/-")
    text <- str_replace_all(text, "≥", ">=")
    text <- str_replace_all(text, "≤", "<=")
    text <- str_replace_all(text, "≠", "!=")
    text <- str_replace_all(text, "µ", "micro")
    text <- str_replace_all(text, "Δ", "Delta")
    text <- str_replace_all(text, "≈", "~")
    text <- str_replace_all(text, "∞", "infinity")
    text <- str_replace_all(text, "×", "*")
    text <- str_replace_all(text, "√", "sqrt")
    text <- str_replace_all(text, "∑", "sum")
    text <- str_replace_all(text, "∫", "integral")
    text <- str_replace_all(text, "ℓ", "L")  # For liter
      # Replace superscripts (¹²³ etc.)
    text <- str_replace_all(text, "¹", "^1")
    text <- str_replace_all(text, "²", "^2")
    text <- str_replace_all(text, "³", "^3")
    
    # Replace subscripts (₀₁₂ etc.)
    text <- str_replace_all(text, "₀", "_0")
    text <- str_replace_all(text, "₁", "_1")
    text <- str_replace_all(text, "₂", "_2")
    text <- str_replace_all(text, "₃", "_3")
    text <- str_replace_all(text, "₄", "_4")
    text <- str_replace_all(text, "₅", "_5")
    text <- str_replace_all(text, "₆", "_6")
    text <- str_replace_all(text, "₇", "_7")
    text <- str_replace_all(text, "₈", "_8")
    text <- str_replace_all(text, "₉", "_9")
    
    # Handle scientific notation (e.g., 1 × 10^6)
    text <- str_replace_all(text, "×\\s*10\\^", "x10^")
    
    # Clean spaces
    text <- str_replace_all(text, "\\s{2,}", " ") # Replace multiple spaces with a single space
    text <- str_trim(text) # Trim leading and trailing spaces
    
    return(text)
}


  # Internal helper function: handle_text_length
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

  # Main logic of create_ti_domain continues here...
  
  if (method == "pdf") {
    if (is.null(pdf_path) || is.null(incl_range) || is.null(excl_range) ||
        is.null(incl_section) || is.null(excl_section) || is.null(end_section)) {
      stop("For PDF method, pdf_path, incl_range, excl_range, incl_section, excl_section, and end_section must be provided.")
    }
    if (!file.exists(pdf_path)) {
      stop("The specified PDF file does not exist: ", pdf_path)
    }
    
    # Call internal extract_ti_domain when necessary
    result <- extract_ti_domain(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section)
    
    if (is.null(result$inclusion) || is.null(result$exclusion)) {
      stop("No inclusion or exclusion criteria found.")
    }

    # Process the result and generate the TI domain as before
    ti_domain <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TI",
      IETESTCD = c(paste0("INCL", seq_along(result$inclusion)), paste0("EXCL", seq_along(result$exclusion))),
      IETEST = c(rep("Inclusion Criteria", length(result$inclusion)), rep("Exclusion Criteria", length(result$exclusion))),
      # IECAT = c(rep("Inclusion", length(result$inclusion)), rep("Exclusion", length(result$exclusion))),
      IECAT = " ",
      IESCAT = " ",
      IETEST = c(result$inclusion, result$exclusion),
      stringsAsFactors = FALSE
    )
    
    # Save to Excel
    excel_file <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
    openxlsx::write.xlsx(ti_domain, excel_file)
    
    return(ti_domain)
  }
  
  # API method logic can go here if needed...
}


create_ti_domain_pdf <- function(study_id, pdf_path, incl_range, excl_range, incl_section, excl_section, end_section, output_dir) {
  debug_info <- ""
  
  tryCatch({
    pdf_text <- pdftools::pdf_text(pdf_path)
    debug_info <- paste0(debug_info, sprintf("Number of pages in PDF: %d\n", length(pdf_text)))
    
    pages_to_process <- unique(c(incl_range, excl_range))
    cleaned_result <- remove_footnotes_and_headers(pdf_text[pages_to_process])
    cleaned_text <- cleaned_result$cleaned_pages
    debug_info <- paste0(debug_info, cleaned_result$debug_info)
    
    footnotes <- cleaned_result$footnotes
    debug_info <- paste0(debug_info, "Extracted footnotes:\n")
    for (i in seq_along(footnotes)) {
      debug_info <- paste0(debug_info, sprintf("%d. %s\n", i, footnotes[i]))
    }
    
# Detect bullet points or numbered lists for inclusion criteria
    inclusion_result <- if (grepl("•|\\d+\\.|–", inclusion_text)) {
      extract_ti_domain(
        study_id = study_id, 
        pdf_path = pdf_path, 
        incl_range = incl_range, 
        excl_range = excl_range, 
        incl_section = incl_section, 
        excl_section = excl_section, 
        end_section = end_section
      )
    } else {
      extract_criteria(inclusion_text, incl_section, excl_section, footnotes)
    }

    # Detect bullet points or numbered lists for exclusion criteria
    exclusion_result <- if (grepl("•|\\d+\\.|–", exclusion_text)) {
      extract_ti_domain(
        study_id = study_id, 
        pdf_path = pdf_path, 
        incl_range = incl_range, 
        excl_range = excl_range, 
        incl_section = incl_section, 
        excl_section = excl_section, 
        end_section = end_section
      )
    } else {
      extract_criteria(exclusion_text, excl_section, end_section, footnotes)
    }

    debug_info <- paste0(debug_info, "Inclusion criteria extraction debug info:\n", inclusion_result$debug_info)
    debug_info <- paste0(debug_info, "Exclusion criteria extraction debug info:\n", exclusion_result$debug_info)
    
    inclusion_result <- extract_criteria(inclusion_text, incl_section, excl_section, footnotes)
    exclusion_result <- extract_criteria(exclusion_text, excl_section, end_section, footnotes)
    
    debug_info <- paste0(debug_info, "Inclusion criteria extraction debug info:\n", inclusion_result$debug_info)
    debug_info <- paste0(debug_info, "Exclusion criteria extraction debug info:\n", exclusion_result$debug_info)
    
    if (length(inclusion_result$criteria) == 0 && length(exclusion_result$criteria) == 0) {
      warning("No inclusion or exclusion criteria found.")
      return(list(ti_domain = data.frame(), debug_info = debug_info))
    }
    
    # Process the result and generate the TI domain
ti_domain <- data.frame(
  STUDYID = study_id,
  DOMAIN = "TI",
  IETESTCD = c(paste0("INCL", seq_along(result$inclusion)), paste0("EXCL", seq_along(result$exclusion))),
  IETEST = c(rep("Inclusion Criteria", length(inclusion_result$criteria)),
             rep("Exclusion Criteria", length(exclusion_result$criteria))),
  IECAT = ifelse(c(inclusion_result$subcategories, exclusion_result$subcategories) == "", "", 
                 c(rep("Inclusion", length(inclusion_result$criteria)), 
                   rep("Exclusion", length(exclusion_result$criteria)))),
  IESCAT = ifelse(c(inclusion_result$subcategories, exclusion_result$subcategories) == "", NA, 
                  c(inclusion_result$subcategories, exclusion_result$subcategories)),
  IETEST = c(inclusion_result$criteria, exclusion_result$criteria),
  stringsAsFactors = FALSE
)

    
    # Remove footnote patterns from IETEST
    ti_domain$IETEST <- remove_footnote_patterns(ti_domain$IETEST, footnotes)
    
    # Truncate IETEST and remove introduction text
    truncate_and_clean_IETEST <- function(text, criteria_type) {
      # Remove introduction text dynamically
      intro_patterns <- c(
        paste0("(?i)patients\\s+(must|should)\\s+(meet|satisfy|fulfill|be excluded if|not be eligible if)\\s+(the\\s+)?(following|these)\\s+", criteria_type, "\\s+(criteria|requirements).*?:"),
        paste0("(?i)(the\\s+)?(following|these)\\s+", criteria_type, "\\s+(criteria|requirements)\\s+(must|should)\\s+be\\s+(met|satisfied|fulfilled).*?:"),
        paste0("(?i)", criteria_type, "\\s+(criteria|requirements).*?:")
      )
      for (pattern in intro_patterns) {
        text <- gsub(pattern, "", text)
      }
      
      # Truncate if necessary
      if (nchar(text) > 200) {
        return(paste0(substr(text, 1, 196), "... (As per the protocol)"))
      }
      return(trimws(text))
    }
    
    ti_domain$IETEST <- sapply(ti_domain$IETEST, function(x) {
      criteria_type <- ifelse(grepl("^INCL", x), "inclusion", "exclusion")
      truncate_and_clean_IETEST(x, criteria_type)
    })
    
    if (nrow(ti_domain) > 0) {
      excel_file <- file.path(output_dir, paste0(study_id, "_TI.xlsx"))
      openxlsx::write.xlsx(ti_domain, excel_file)
      debug_info <- paste0(debug_info, "Excel file saved: ", excel_file, "\n")
    } else {
      debug_info <- paste0(debug_info, "No data to save to Excel.\n")
    }
    
    return(list(ti_domain = ti_domain, debug_info = debug_info, footnotes = footnotes))
  }, error = function(e) {
    error_msg <- paste("Error in create_ti_domain_pdf:", e$message, "\n")
    return(list(ti_domain = data.frame(), debug_info = paste0(debug_info, error_msg)))
  })
}

# Enhanced function to filter criteria
filter_criteria <- function(criteria) {
  # Expanded patterns to identify and remove
  patterns_to_remove <- c(
    # General introductory phrases
    "^\\s*(Patients|Subjects|Participants)\\s+(must|should)\\s+(meet|satisfy|fulfill|be excluded if|not be eligible if)\\s+(the\\s+)?(following|these)\\s+(criteria|requirements).*$",
    "^\\s*(The\\s+)?(following|these)\\s+(criteria|requirements)\\s+(must|should)\\s+be\\s+(met|satisfied|fulfilled).*$",
    "^\\s*(Key|Main|Principal)\\s+(inclusion|exclusion)\\s+(criteria|requirements).*$",
    "^\\s*(Inclusion|Exclusion)\\s+(Criteria|Requirements).*$",
    
    # Specific phrases
    "^\\s*For\\s+inclusion\\s+in\\s+(the|this)\\s*study.*$",
    "^\\s*To\\s+be\\s+eligible\\s+(to|for)\\s+participate.*$",
    "^\\s*Eligibility\\s+(for|to)\\s+(treatment|participation).*$",
    "^\\s*Study\\s+population\\s+(will\\s+|must\\s+|should\\s+)?include.*$",
    "^\\s*(Patients|Subjects|Participants)\\s+(will|must|should)\\s+be\\s+excluded\\s+if.*$",
    
    # Numbered or bulleted introductions
    "^\\s*\\d+\\.?\\s*(Inclusion|Exclusion)\\s+(Criteria|Requirements).*$",
    "^\\s*[•\\*-]\\s*(Inclusion|Exclusion)\\s+(Criteria|Requirements).*$",
    
    # Additional patterns
    "^\\s*A\\s+(patient|subject|participant)\\s+(is|will be)\\s+eligible\\s+(for|to)\\s+(inclusion|participate).*$",
    "^\\s*(Patients|Subjects|Participants)\\s+with\\s+(any\\s+of\\s+)?(the\\s+)?(following|these)\\s+(will|must|should)\\s+(be\\s+excluded|not\\s+be\\s+included).*$"
  )
  
  # Function to check if a line should be removed
  should_remove <- function(line) {
    any(sapply(patterns_to_remove, function(pattern) grepl(pattern, line, ignore.case = TRUE)))
  }
  
  # Filter out lines that match the patterns
  filtered_criteria <- criteria[!sapply(criteria, should_remove)]
  
  # Remove any remaining empty lines and trim whitespace
  filtered_criteria <- trimws(filtered_criteria[nzchar(trimws(filtered_criteria))])
  
  # Remove leading numbers, bullets, or dashes
  filtered_criteria <- gsub("^\\s*([0-9]+\\.|-|•|\\*)\\s*", "", filtered_criteria)
  
  return(filtered_criteria)
}

remove_footnote_patterns <- function(text, footnotes) {
  # Extract common patterns from footnotes
  extract_patterns <- function(footnotes) {
    patterns <- c()
    for (footnote in footnotes) {
      # Extract company names (assumed to be capitalized words)
      company_names <- unlist(regmatches(footnote, gregexpr("[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)+", footnote)))
      patterns <- c(patterns, company_names)
      
      # Extract protocol numbers
      protocol_numbers <- unlist(regmatches(footnote, gregexpr("Protocol\\s+[A-Za-z0-9]+", footnote)))
      patterns <- c(patterns, protocol_numbers)
      
      # Extract version numbers
      version_numbers <- unlist(regmatches(footnote, gregexpr("Version\\s+\\d+", footnote)))
      patterns <- c(patterns, version_numbers)
      
      # Extract page numbers (assuming they are at the end of the footnote)
      page_numbers <- unlist(regmatches(footnote, gregexpr("\\d+\\s*$", footnote)))
      patterns <- c(patterns, page_numbers)
    }
    
    # Remove duplicates and empty strings
    unique(patterns[nzchar(patterns)])
  }
  
  # Get dynamic patterns from footnotes
  dynamic_patterns <- extract_patterns(footnotes)
  
  # Remove dynamic patterns
  for (pattern in dynamic_patterns) {
    text <- gsub(pattern, "", text, fixed = TRUE)
  }
  
  # Trim whitespace and remove any resulting double spaces
  text <- gsub("\\s+", " ", trimws(text))
  
  return(text)
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
  subsection_pattern <- "^\\d+(\\.\\d+){2,}\\s+(.+)$"
  
  # Pattern to match main criteria start (various bullet point styles and numbering)
  main_criterion_pattern <- "^\\s*(•|\\*|–|-|\\d+\\.?|[a-z]\\)|[A-Z]\\)|\\([a-z]\\)|\\([A-Z]\\))\\s+"
  
  # Pattern to match introductory text (expanded to catch more variations)
  intro_pattern <- paste0(
    "(?i)(",
    "patients\\s+(must|should|will)\\s+(meet|satisfy|fulfill|be excluded if|not be eligible if)|",
    "(inclusion|exclusion)\\s+criteria|",
    "for\\s+study\\s+entry|",
    "to\\s+be\\s+eligible\\s+(for|to)\\s+participate|",
    "eligibility\\s+(for|to)\\s+(treatment|participation)|",
    "study\\s+population\\s+(will|must|should)\\s+include",
    ").*?:"
  )
  
  in_criteria <- FALSE  # Flag to indicate we've started processing actual criteria
  skip_intro <- TRUE  # Flag to skip introductory text
  
  for (i in (start_index[1] + 1):end_index) {
    line <- trimws(all_lines[i])
    if (nchar(line) == 0) next
    
    # Clean special characters for each line
    line <- clean_special_characters(line)
    
    # Check for subsection headers
    if (grepl(subsection_pattern, line)) {
      current_subcategory <- gsub(subsection_pattern, "\\2", line)
      in_criteria <- FALSE  # Reset flag after subsection header
      skip_intro <- TRUE  # Reset flag to skip intro text
      next
    }
    
    # Skip introductory text
    if (skip_intro && grepl(intro_pattern, line, perl = TRUE)) {
      skip_intro <- FALSE  # Reset flag after skipping intro
      next
    }
    
    # Check for new criterion (main or sub)
    if (grepl(main_criterion_pattern, line)) {
      if (nchar(current_criterion) > 0) {
        criteria <- c(criteria, trimws(current_criterion))
        criteria_pages <- c(criteria_pages, current_page)
        criteria_subcategories <- c(criteria_subcategories, current_subcategory)
      }
      current_criterion <- gsub(main_criterion_pattern, "", line)
      current_page <- all_page_nums[i]
      in_criteria <- TRUE
    } else if (in_criteria) {
      # This is a continuation of the current criterion
      current_criterion <- paste(current_criterion, line)
    }
    
    # Truncate if exceeds 200 characters
    if (nchar(current_criterion) > 200) {
      current_criterion <- paste0(substr(current_criterion, 1, 196), "... (As per the protocol)")
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

# Function to clean up special characters
clean_special_characters <- function(text) {
  # Replace common special characters
  text <- gsub("≥", ">=", text)
  text <- gsub("≤", "<=", text)
  text <- gsub("×", "x", text)
  text <- gsub("–", "-", text)
  text <- gsub("−", "-", text)
  text <- gsub("µ", "u", text)
  text <- gsub("°", " degrees ", text)
  
  # Handle superscripts and subscripts
  text <- gsub("(\\d+)\\s*([⁰¹²³⁴⁵⁶⁷⁸⁹]+)", "\\1^\\2", text)
  text <- gsub("(\\d+)\\s*([₀₁₂₃₄₅₆₇₈₉]+)", "\\1_\\2", text)
  
  # Replace superscript and subscript characters
  superscripts <- c("⁰" = "0", "¹" = "1", "²" = "2", "³" = "3", "⁴" = "4", "⁵" = "5", "⁶" = "6", "⁷" = "7", "⁸" = "8", "⁹" = "9")
  subscripts <- c("₀" = "0", "₁" = "1", "₂" = "2", "₃" = "3", "₄" = "4", "₅" = "5", "₆" = "6", "₇" = "7", "₈" = "8", "₉" = "9")
  
  for (i in seq_along(superscripts)) {
    text <- gsub(names(superscripts)[i], superscripts[i], text, fixed = TRUE)
  }
  for (i in seq_along(subscripts)) {
    text <- gsub(names(subscripts)[i], subscripts[i], text, fixed = TRUE)
  }
  
  # Handle scientific notation and units
  text <- gsub("(\\d+(?:\\.\\d+)?)\\s*[×x]\\s*10\\s*/?\\s*(\\d+)\\s*/?(L|mL|dL)?", "\\1 x 10^\\2 /\\3", text)
  
  # Clean up spaces around operators and parentheses
  text <- gsub("\\s*([<>≤≥=])\\s*", " \\1 ", text)
  text <- gsub("\\s*([()])", "\\1", text)
  text <- gsub("\\(\\s+", "(", text)
  text <- gsub("\\s+\\)", ")", text)
  
  # Clean up multiple spaces
  text <- gsub("\\s+", " ", text)
  
  return(trimws(text))
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
    select(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT, IESCAT, IETEST, TIRL, TIVERS)

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
  setColWidths(wb, "TI_Domain", cols = 7, widths = 150)  # IETEST column

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