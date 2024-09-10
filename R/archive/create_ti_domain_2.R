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
    
    pages_to_process <- unique(c(incl_range, excl_range))
    cleaned_result <- remove_footnotes_and_headers(pdf_text[pages_to_process])
    cleaned_text <- cleaned_result$cleaned_pages
    debug_info <- paste0(debug_info, cleaned_result$debug_info)
    
    footnotes <- cleaned_result$footnotes
    debug_info <- paste0(debug_info, "Extracted footnotes:\n")
    for (i in seq_along(footnotes)) {
      debug_info <- paste0(debug_info, sprintf("%d. %s\n", i, footnotes[i]))
    }
    
    inclusion_text <- cleaned_text[which(pages_to_process %in% incl_range)]
    exclusion_text <- cleaned_text[which(pages_to_process %in% excl_range)]
    
    debug_info <- paste0(debug_info, "Inclusion text sample: ", substr(inclusion_text[[1]]$text, 1, 500), "...\n")
    debug_info <- paste0(debug_info, "Exclusion text sample: ", substr(exclusion_text[[1]]$text, 1, 500), "...\n")
    
    inclusion_result <- extract_criteria(inclusion_text, incl_section, excl_section, footnotes)
    exclusion_result <- extract_criteria(exclusion_text, excl_section, end_section, footnotes)
    
    inclusion_result$criteria <- filter_criteria(inclusion_result$criteria)
    exclusion_result$criteria <- filter_criteria(exclusion_result$criteria)
    
    debug_info <- paste0(debug_info, "Inclusion criteria extraction debug info:\n", inclusion_result$debug_info)
    debug_info <- paste0(debug_info, "Exclusion criteria extraction debug info:\n", exclusion_result$debug_info)
    
    if (length(inclusion_result$criteria) == 0 && length(exclusion_result$criteria) == 0) {
      warning("No inclusion or exclusion criteria found.")
      return(list(ti_domain = data.frame(), debug_info = debug_info))
    }
    
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
    
    # Remove footnote patterns from IEORRES
    ti_domain$IEORRES <- remove_footnote_patterns(ti_domain$IEORRES, footnotes)
    
    # Truncate IEORRES and remove introduction text
    truncate_and_clean_ieorres <- function(text, criteria_type) {
  # Dynamically identify and remove any introductory text matching the above patterns
  for (pattern in intro_patterns) {
    text <- gsub(pattern, "", text)
  }
      
      # Truncate if necessary
      if (nchar(text) > 200) {
        return(paste0(substr(text, 1, 196), "... (As per the protocol)"))
      }
      return(trimws(text))
    }
    
    ti_domain$IEORRES <- sapply(ti_domain$IEORRES, function(x) {
      criteria_type <- ifelse(grepl("^INCL", x), "inclusion", "exclusion")
      truncate_and_clean_ieorres(x, criteria_type)
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
    "(?i)Inclusion\\s*Criteria",
    "(?i)Exclusion\\s*Criteria",
    "(?i)Patients\\s*(must|should)\\s*(meet|satisfy|fulfill)\\s*the\\s*following\\s*(criteria|requirements)\\s*(for\\s*study\\s*entry|to\\s*participate):?",
    "(?i)Eligibility\\s*(Criteria|Requirements)\\s*(for|to)\\s*(Study\\s*Entry|Participation):?",
    "(?i)Patients\\s*(who\\s*meet\\s*any|that\\s*meet\\s*the\\s*following|eligible\\s*for|who\\s*meet\\s*the\\s*criteria)\\s*will\\s*be\\s*(excluded\\s*from|included\\s*in)\\s*the\\s*study\\s*(entry|participation):?",
    "(?i)To\\s*be\\s*(eligible|included),\\s*patients\\s*must\\s*(meet|satisfy|fulfill)\\s*the\\s*(following\\s*)?(criteria|requirements):?",
    "(?i)(Key|Main|Primary)\\s*(inclusion|exclusion)\\s*(criteria|requirements):?",
    "(?i)Potential\\s*participants\\s*are\\s*(excluded\\s*from|eligible\\s*to\\s*be\\s*included\\s*in)\\s*the\\s*study\\s*(if|only\\s*if)\\s*(any\\s*of|all\\s*of)\\s*the\\s*following\\s*criteria\\s*(apply|are\\s*met):?",
    "(?i)(Subjects|Individuals|Participants)\\s*(will\\s*be|are)\\s*(eligible|ineligible)\\s*(for|to)\\s*(inclusion|exclusion|participate)\\s*(in\\s*the\\s*study|if)\\s*(they\\s*meet|any\\s*of)\\s*the\\s*following\\s*(criteria|conditions):?",
    "(?i)The\\s*(inclusion|exclusion)\\s*criteria\\s*(for\\s*this\\s*study|are\\s*as\\s*follows):?",
    "(?i)(Study|Trial)\\s*(candidates|subjects)\\s*must\\s*(fulfill|meet)\\s*all\\s*of\\s*the\\s*following\\s*criteria\\s*to\\s*be\\s*(eligible|included):?",
    "(?i)(Subjects|Patients)\\s*(will\\s*not\\s*be\\s*eligible|are\\s*not\\s*suitable)\\s*for\\s*(enrollment|inclusion)\\s*if\\s*any\\s*of\\s*the\\s*following\\s*(apply|are\\s*present):?",
    "(?i)Patients\\s*who\\s*meet\\s*any\\s*of\\s*the\\s*following\\s*criteria\\s*will\\s*be\\s*excluded\\s*from\\s*study\\s*entry:",
    "(?i)(Subjects|Patients|Participants)\\s*(who\\s*meet|meeting)\\s*(any|all)\\s*of\\s*the\\s*following\\s*criteria\\s*(will|shall)\\s*be\\s*(excluded|included)\\s*(from|in)\\s*(the\\s*study|study\\s*entry):?",
    "(?i)(Subjects|Patients|Participants)\\s*(with|having)\\s*(any|all)\\s*of\\s*the\\s*following\\s*(will|shall)\\s*(not\\s*be\\s*eligible|be\\s*excluded)\\s*(for|from)\\s*(the\\s*study|study\\s*entry):?",
    "(?i)The\\s*following\\s*criteria\\s*(will|shall)\\s*(exclude|include)\\s*(a\\s*patient|patients|subjects|participants)\\s*from\\s*(the\\s*study|study\\s*entry):?",

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
  
  # Updated pattern to match subsection headers (e.g., "4.1.1.1 Patients")
  subsection_pattern <- "^\\d+(\\.\\d+)*\\s+(.+)$"
  
  # Updated pattern to match criteria start
  criterion_start_pattern <- "^\\s*(•|\\*|\\d+\\.|-|[a-z]\\)|[A-Z]\\)|\\([a-z]\\)|\\([A-Z]\\)|–|▪|○|·|■|□|◦|►|▻|▼|▽|◆|◇|✓|✔|⚫|⚪)\\s+"
  
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
      IECAT = c(rep("Inclusion", length(inclusion_result$criteria)),
                rep("Exclusion", length(exclusion_result$criteria))),
      IESCAT = c(inclusion_result$subcategories, exclusion_result$subcategories),
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