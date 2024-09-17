#' Create Default TS Domain Structure
#'
#' This function creates a default TS domain structure when the Trial_Summary.xlsx file is not available.
#'
#' @return A data frame with default TS domain structure.
#' @keywords internal
create_default_ts_structure <- function() {
  data.frame(
    STUDYID = character(10),
    DOMAIN = character(10),
    TSSEQ = 1:10,
    TSPARMCD = c("STUDYID", "TTYPE", "STYPE", "TPHASE", "TSUBJECT", "TDURPLA", "TITLE", "TSPOAPL", "TSPOTRT", "TCNTRL"),
    TSPARM = c("Study Identifier", "Trial Type", "Study Type", "Trial Phase", "Trial Subject", "Duration of Planned Treatment", "Trial Title", "Planned Sponsor of Analysis", "Planned Sponsor of Treatment", "Control Type"),
    TSVAL = character(10),
    stringsAsFactors = FALSE
  )
}

fetch_study_info_v2 <- function(nct_ids, debug = FALSE) {
  if(debug) cat("fetch_study_info called with NCT IDs:", paste(nct_ids, collapse = ", "), "\n")

  base_url <- "https://clinicaltrials.gov/api/v2/studies/"

  study_info <- lapply(nct_ids, function(nct_id) {
    if(debug) cat("Processing NCT ID:", nct_id, "\n")

    url <- paste0(base_url, nct_id)

    response <- httr::GET(url)
    if (httr::status_code(response) != 200) {
      warning("Failed to fetch data for NCT ID: ", nct_id)
      return(NULL)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(content)

    if(debug) {
      cat("API Response Structure for NCT ID:", nct_id, "\n")
      print(str(json_data, max.level = 2))
    }

    # Save JSON data
    filename <- paste0("study_info_", nct_id, ".json")
    save_json(json_data, filename)

    if(debug) cat("JSON data saved for NCT ID:", nct_id, "\n")

    return(json_data)
  })

  names(study_info) <- nct_ids

  if(debug) cat("Finished fetching study information for all NCT IDs\n")

  return(study_info)
}

# Update the split_text function to handle vectors
split_text <- function(text, max_length = 200) {
  if (length(text) > 1) {
    return(lapply(text, function(t) split_text(t, max_length)))
  }
  
  if (is.na(text) || is.null(text) || nchar(text) <= max_length) {
    return(list(TSVAL = text))
  }
  
  result <- list()
  remaining_text <- text
  col_index <- 0
  
  while (nchar(remaining_text) > 0) {
    col_name <- if (col_index == 0) "TSVAL" else paste0("TSVAL", col_index)
    if (nchar(remaining_text) <= max_length) {
      result[[col_name]] <- remaining_text
      break
    } else {
      split_point <- max(1, regexpr("\\s+", substr(remaining_text, max_length, nchar(remaining_text)))[1] + max_length - 1)
      result[[col_name]] <- substr(remaining_text, 1, split_point)
      remaining_text <- substr(remaining_text, split_point + 1, nchar(remaining_text))
    }
    col_index <- col_index + 1
  }
  
  return(result)
}

#' Create TS Domain
#'
#' This function creates the TS domain based on the provided NCT IDs and study ID.
#'
#' @param nct_ids A vector of NCT IDs
#' @param study_id The study ID
#' @param output_dir The output directory (default: current working directory)
#' @param debug Boolean flag to enable debug mode (default: FALSE)
#' @import dplyr
#' @import openxlsx
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @return A list containing the TS domain data frame and count information
#' @export
create_ts_domain <- function(nct_ids, study_id, output_dir = getwd(), debug = FALSE) {
  tryCatch({
    # Read the template file
    ts_file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
    ts_template <- openxlsx::read.xlsx(ts_file_path, sheet = "TS")
    
    # Count the number of rows in the template
    template_row_count <- nrow(ts_template)
    
    # Create a base ts_summary with all TSPARAM/TSPARAMCD from the template
    base_ts_summary <- ts_template %>%
      select(TSPARMCD, TSPARM) %>%
      mutate(
        STUDYID = study_id,
        DOMAIN = "TS",
        TSSEQ = row_number(),
        TSGRPID = NA_character_,
        TSVAL = NA_character_,
        TSVALNF = NA_character_,
        TSVALCD = NA_character_,
        TSVCDREF = NA_character_,
        TSVCDVER = NA_character_
      )

    if(debug) cat("Starting create_ts_domain function\n")

    local_fetch_study_info <- fetch_study_info_v2

    trial_data <- lapply(nct_ids, function(nct_id) {
      if(debug) cat("Processing NCT ID:", nct_id, "\n")
      study_info <- local_fetch_study_info(nct_ids = nct_id, debug = debug)
      if(debug) cat("Study info fetched for NCT ID:", nct_id, "\n")
      processed_info <- process_study_info(study_info, debug = debug)
      if(debug) cat("Processed study info for NCT ID:", nct_id, "\n")
      return(processed_info)
    })

    trial_data <- trial_data[!sapply(trial_data, is.null)]
    if (length(trial_data) == 0) stop("No valid data could be retrieved for the provided NCT IDs.")

    if(debug) cat("Trial data processed\n")

    ts_mapping <- define_ts_mapping(debug)
    if(debug) cat("TS mapping defined\n")

    if(debug) cat("Starting to create mapped_data\n")
    
    # Create a data frame with mapped values
    mapped_data <- data.frame(TSPARMCD = character(), TSVAL = character(), stringsAsFactors = FALSE)
    
    for (param in names(ts_mapping)) {
      if(debug) cat("Processing parameter:", param, "\n")
      mapped_values <- ts_mapping[[param]](trial_data)
      if (length(mapped_values) > 0 && !all(is.na(mapped_values))) {
        if(debug) cat("  Found", length(mapped_values), "values for", param, "\n")
        for (value in mapped_values) {
          tryCatch({
            split_values <- split_text(value)
            if(debug) cat("  Split into", length(split_values), "parts\n")
            new_row <- data.frame(TSPARMCD = param, stringsAsFactors = FALSE)
            for (col_name in names(split_values)) {
              new_row[[col_name]] <- split_values[[col_name]]
            }
            
            # Ensure all columns are present in mapped_data
            for (col in names(new_row)) {
              if (!(col %in% names(mapped_data))) {
                mapped_data[[col]] <- character(nrow(mapped_data))
              }
            }
            
            # Ensure new_row has all columns present in mapped_data
            for (col in names(mapped_data)) {
              if (!(col %in% names(new_row))) {
                new_row[[col]] <- NA_character_
              }
            }
            
            mapped_data <- rbind(mapped_data, new_row)
            
            if(debug) cat("  Added new row. mapped_data now has", nrow(mapped_data), "rows and", ncol(mapped_data), "columns\n")
          }, error = function(e) {
            if(debug) cat("Error processing value:", value, "\nError:", conditionMessage(e), "\n")
          })
        }
      }
    }
    
    if(debug) cat("Finished creating mapped_data. Final dimensions:", nrow(mapped_data), "rows and", ncol(mapped_data), "columns\n")
    
    # Ensure all necessary columns are present in mapped_data
    required_columns <- c("STUDYID", "DOMAIN", "TSSEQ", "TSGRPID", "TSPARMCD", "TSPARM", "TSVAL", "TSVALNF", "TSVALCD", "TSVCDREF", "TSVCDVER")
    for (col in required_columns) {
      if (!(col %in% colnames(mapped_data))) {
        mapped_data[[col]] <- NA_character_
      }
    }
    
    # Add TSVAL1, TSVAL2, etc. to required_columns if they exist in mapped_data
    extra_tsval_columns <- grep("^TSVAL\\d+$", colnames(mapped_data), value = TRUE)
    required_columns <- c(required_columns, extra_tsval_columns)
    
    if(debug) cat("Added required columns. mapped_data now has", ncol(mapped_data), "columns\n")
    
    # Ensure base_ts_summary has all the columns from mapped_data
    for (col in colnames(mapped_data)) {
      if (!(col %in% colnames(base_ts_summary))) {
        base_ts_summary[[col]] <- NA_character_
      }
    }
    
    # Merge mapped data with base_ts_summary
    ts_summary <- merge(base_ts_summary, mapped_data, by = "TSPARMCD", all.x = TRUE, suffixes = c(".base", ""))
    
    if(debug) cat("Merged with base_ts_summary. ts_summary now has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    
    # Use mapped values if available, otherwise keep base values
    for (col in setdiff(names(mapped_data), "TSPARMCD")) {
      base_col <- paste0(col, ".base")
      if (base_col %in% names(ts_summary)) {
        ts_summary[[col]] <- ifelse(is.na(ts_summary[[col]]), ts_summary[[base_col]], ts_summary[[col]])
        ts_summary[[base_col]] <- NULL
      }
    }
    
    if(debug) cat("Cleaned up merged data. ts_summary now has", ncol(ts_summary), "columns\n")
    
    # Ensure only required columns are present in the final output
    ts_summary <- ts_summary[, required_columns]
    
    if(debug) cat("Final ts_summary has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    
    # Create a mapping of TSPARMCD to row numbers in base_ts_summary
    base_order <- data.frame(
      TSPARMCD = base_ts_summary$TSPARMCD,
      original_order = seq_along(base_ts_summary$TSPARMCD),
      stringsAsFactors = FALSE
    )
    
    # Merge this ordering information with ts_summary
    ts_summary <- merge(ts_summary, base_order, by = "TSPARMCD", all.x = TRUE)
    
    # Sort by the original order
    ts_summary <- ts_summary[order(ts_summary$original_order), ]
    
    # Remove the temporary column
    ts_summary$original_order <- NULL
    
    if(debug) cat("After reordering, ts_summary has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    if(debug) cat("Number of unique TSPARAMCD values:", length(unique(ts_summary$TSPARMCD)), "\n")
    
    # Remove rows with NA TSPARMCD (if any)
    ts_summary <- ts_summary[!is.na(ts_summary$TSPARMCD), ]
    
    if(debug) cat("After removing NA TSPARMCD, ts_summary has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    
    # Add debug print before modifications
    if(debug) {
      cat("Before modifications:\n")
      print(table(ts_summary$TSVALNF, useNA = "ifany"))
      print(table(ts_summary$TSVCDREF, useNA = "ifany"))
      print(table(ts_summary$TSVALCD, useNA = "ifany"))
    }

    # Update TSVALNF and TSVCDREF based on TSVAL
    ts_summary$TSVALNF <- ifelse(is.na(ts_summary$TSVAL), "NI", ts_summary$TSVALNF)
    if(debug) cat("Rows where TSVALNF was set to 'NI':", sum(ts_summary$TSVALNF == "NI", na.rm = TRUE), "\n")
    
    ts_summary$TSVCDREF <- ifelse(!is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "" , "ClinicalTrials.gov", ts_summary$TSVCDREF)
    if(debug) cat("Rows where TSVCDREF was set to 'ClinicalTrials.gov':", sum(ts_summary$TSVCDREF == "ClinicalTrials.gov", na.rm = TRUE), "\n")
    
    # Update TSVALCD and TSVCDREF based on specific TSVAL content
    ts_summary$TSVALCD <- ifelse(grepl("Hoffmann-La Roche", ts_summary$TSVAL, ignore.case = TRUE), "480008226", ts_summary$TSVALCD)
    if(debug) cat("Rows where TSVALCD was set to '480008226':", sum(ts_summary$TSVALCD == "480008226", na.rm = TRUE), "\n")
    
    ts_summary$TSVCDREF <- ifelse(grepl("Hoffmann-La Roche", ts_summary$TSVAL, ignore.case = TRUE), "DUNS", ts_summary$TSVCDREF)
    if(debug) cat("Rows where TSVCDREF was set to 'DUNS':", sum(ts_summary$TSVCDREF == "DUNS", na.rm = TRUE), "\n")

    # Add debug print after modifications
    if(debug) {
      cat("After modifications:\n")
      print(table(ts_summary$TSVALNF, useNA = "ifany"))
      print(table(ts_summary$TSVCDREF, useNA = "ifany"))
      print(table(ts_summary$TSVALCD, useNA = "ifany"))
      
      cat("Number of NA TSVAL:", sum(is.na(ts_summary$TSVAL)), "\n")
      cat("Number of rows with 'Hoffmann-La Roche' in TSVAL:", sum(grepl("Hoffmann-La Roche", ts_summary$TSVAL, ignore.case = TRUE), na.rm = TRUE), "\n")
    }
    
    if(debug) cat("Final ts_summary after reordering and TSSEQ assignment has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    if(debug) print(head(ts_summary))
    if(debug) print(tail(ts_summary))
    
    # Identify extra TSVAL columns
    extra_tsval_columns <- grep("^TSVAL\\d+$", colnames(ts_summary), value = TRUE)
    
    # Define the desired column order
    base_columns <- c("STUDYID", "DOMAIN", "TSSEQ", "TSGRPID", "TSPARMCD", "TSPARM", "TSVAL")
    end_columns <- c("TSVALNF", "TSVALCD", "TSVCDREF", "TSVCDVER")
    
    # Combine all columns in the desired order
    desired_columns <- c(base_columns, extra_tsval_columns, end_columns)
    
    # Ensure all desired columns exist in ts_summary
    for (col in desired_columns) {
      if (!(col %in% colnames(ts_summary))) {
        ts_summary[[col]] <- NA_character_
      }
    }
    
    # Select only the desired columns in the specified order
    ts_summary <- ts_summary[, desired_columns]
    
    if(debug) cat("Final ts_summary after column reordering has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    if(debug) print(colnames(ts_summary))
    
    # Process treatments
    all_treatments <- unique(unlist(strsplit(ts_summary$TSVAL[ts_summary$TSPARMCD == "TRT"], ", ")))
    treatments <- all_treatments[all_treatments != ""]  # Remove any empty strings
    trt_tsparm <- ts_template$TSPARM[ts_template$TSPARMCD == "TRT"][1]  # Get TSPARM for TRT from template
    treatment_rows <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TS",
      TSSEQ = seq_along(treatments),
      TSGRPID = NA_character_,
      TSPARMCD = "TRT",
      TSPARM = trt_tsparm,
      TSVAL = toupper(treatments),
      TSVALNF = NA_character_,
      TSVALCD = NA_character_,
      TSVCDREF = "ClinicalTrials.gov",
      TSVCDVER = NA_character_,
      stringsAsFactors = FALSE
    )

    # Process countries
    countries <- unique(unlist(strsplit(ts_summary$TSVAL[ts_summary$TSPARMCD == "FCNTRY"], ", ")))
    fcntry_tsparm <- ts_template$TSPARM[ts_template$TSPARMCD == "FCNTRY"][1]  # Get TSPARM for FCNTRY from template
    country_rows <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TS",
      TSSEQ = seq_along(countries),
      TSGRPID = NA_character_,
      TSPARMCD = "FCNTRY",
      TSPARM = fcntry_tsparm,
      TSVAL = toupper(countries),
      TSVALNF = NA_character_,
      TSVALCD = NA_character_,
      TSVCDREF = NA_character_,
      TSVCDVER = NA_character_,
      stringsAsFactors = FALSE
    )


    # Add all columns from desired_columns to treatment_rows and country_rows
    for (col in setdiff(desired_columns, colnames(treatment_rows))) {
      treatment_rows[[col]] <- NA_character_
    }
    for (col in setdiff(desired_columns, colnames(country_rows))) {
      country_rows[[col]] <- NA_character_
    }

    # Handle INTTYPE
    if(debug) cat("Processing INTTYPE\n")
    int_types <- ts_mapping$INTTYPE(trial_data)
    if(length(int_types) > 0) {
      inttype_df <- data.frame(
        STUDYID = study_id,
        DOMAIN = "TS",
        TSSEQ = NA,  # We'll set this later
        TSPARMCD = "INTTYPE",
        TSPARM = "Intervention Type",
        TSVAL = int_types[1],  # Take only the first (and likely only) value
        TSVALNF = NA_character_,
        TSVALCD = NA_character_,
        TSVCDREF = "ClinicalTrials.gov",
        TSVCDVER = NA_character_,
        stringsAsFactors = FALSE
      )
      
      if(debug) {
        cat("INTTYPE data:\n")
        print(inttype_df)
      }
      
      # Ensure inttype_df has the same columns as ts_summary
      missing_cols <- setdiff(colnames(ts_summary), colnames(inttype_df))
      for (col in missing_cols) {
        inttype_df[[col]] <- NA_character_
      }
      
      # Combine with existing ts_summary
      ts_summary <- rbind(ts_summary, inttype_df)
    } else {
      if(debug) cat("No valid INTTYPE values found\n")
    }
    # Populate TSVCDREF
    ts_summary$TSVCDREF <- ifelse(!is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "" , "ClinicalTrials.gov", ts_summary$TSVCDREF)
    
    # Combine all rows
    ts_summary <- rbind(ts_summary, treatment_rows, country_rows)

    # # Ensure INTMODEL and INTTYPE values are valid
    # valid_intmodel_values <- c("CROSSOVER", "FACTORIAL", "PARALLEL", "SEQUENTIAL", "SINGLE GROUP")
    # valid_inttype_values <- c("DRUG", "DEVICE", "BIOLOGIC", "PROCEDURE", "RADIATION", "BEHAVIORAL", "DIETARY SUPPLEMENT", "GENETIC", "COMBINATION PRODUCT", "OTHER")

    # ts_summary$INTMODEL <- toupper(gsub("_", " ", ts_summary$INTMODEL))
    # ts_summary$INTTYPE <- toupper(gsub("_", " ", ts_summary$INTTYPE))

    # ts_summary$INTMODEL <- ifelse(ts_summary$INTMODEL %in% valid_intmodel_values, ts_summary$INTMODEL, NA)
    # ts_summary$INTTYPE <- ifelse(ts_summary$INTTYPE %in% valid_inttype_values, ts_summary$INTTYPE, NA)

    # if(debug) {
    #   cat("INTMODEL values after validation:\n")
    #   print(table(ts_summary$INTMODEL, useNA = "ifany"))
    #   cat("INTTYPE values after validation:\n")
    #   print(table(ts_summary$INTTYPE, useNA = "ifany"))
    # }
    
    # Reorder columns again to ensure TSVAL1, TSVAL2, etc. are next to TSVAL
    ts_summary <- ts_summary[, desired_columns]

    if(debug) cat("Final ts_summary after adding treatments and countries has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
    if(debug) print(colnames(ts_summary))
    
    # Extract lastUpdatePostDateStruct from JSON
    json_file <- file.path(output_dir, "json", paste0("study_info_", nct_ids[1], ".json"))
    if (file.exists(json_file)) {
      study_info <- jsonlite::fromJSON(json_file)
      last_update_date <- study_info$protocolSection$statusModule$lastUpdatePostDateStruct$date[1]
    } else {
      last_update_date <- NA
      if(debug) cat("JSON file not found:", json_file, "\n")
    }

    # Convert TSVAL and TSVALCD to uppercase for comparison
    ts_summary$TSVAL_upper <- toupper(ts_summary$TSVAL)
    ts_summary$TSVALCD_upper <- toupper(ts_summary$TSVALCD)

    # Remove duplicates based on uppercase TSVAL, TSVALCD, and TSPARMCD
    ts_summary <- ts_summary %>%
      distinct(TSVAL_upper, TSVALCD_upper, TSPARMCD, .keep_all = TRUE) %>%
      select(-TSVAL_upper, -TSVALCD_upper)

    # Make missing, NA, or <NA> values all missing
    ts_summary$TSVAL[ts_summary$TSVAL %in% c("", "NA", "<NA>")] <- NA
    ts_summary$TSVALCD[ts_summary$TSVALCD %in% c("", "NA", "<NA>")] <- NA
    ts_summary$TSVALNF[ts_summary$TSVALNF %in% c("", "NA", "<NA>")] <- NA
    ts_summary$TSVCDREF[ts_summary$TSVCDREF %in% c("", "NA", "<NA>")] <- NA
    ts_summary$TSVCDVER[ts_summary$TSVCDVER %in% c("", "NA", "<NA>")] <- NA

    # Remove the original horizontal TRT record
    ts_summary <- ts_summary[!(ts_summary$TSPARMCD == "TRT" & grepl(",", ts_summary$TSVAL)), ]

    # Populate TSVCDVER with the lastUpdatePostDateStruct date when TSVAL is not empty or NA
    ts_summary <- ts_summary %>%
      mutate(TSVCDVER = ifelse(!is.na(TSVAL) & TSVAL != "" , last_update_date, NA))

    # Set TSVALNF to 'PINF' when TSPARMCD is AGEMAX and TSVAL is missing
    ts_summary <- ts_summary %>%
      mutate(TSVALNF = ifelse(TSPARMCD == "AGEMAX" & (is.na(TSVAL) | TSVAL == ""), "PINF", TSVALNF))

  #   # New condition for REGID
  #   ts_summary$TSVCDREF <- ifelse(ts_summary$TSPARMCD == "REGID" & !is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "", "EUDRACT", ts_summary$TSVCDREF)
  #   ts_summary$TSVALCD <- ifelse(ts_summary$TSPARMCD == "REGID" & !is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "", ts_summary$TSVAL, ts_summary$TSVALCD)
  #   if(debug) cat("Rows where TSVCDREF was set to 'EUDRACT' and TSVALCD was set to TSVAL for REGID:", sum(ts_summary$TSPARMCD == "REGID" & !is.na(ts_summary$TSVAL) & ts_summary$TSVAL != ""), "\n")

  #   # Update TSVALNF and TSVCDREF based on TSVAL
  #   ts_summary$TSVALNF <- ifelse(is.na(ts_summary$TSVAL), "NI", ts_summary$TSVALNF)
  #   if(debug) cat("Rows where TSVALNF was set to 'NI':", sum(ts_summary$TSVALNF == "NI", na.rm = TRUE), "\n")
    
  #   ts_summary$TSVCDREF <- ifelse(!is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "" , "ClinicalTrials.gov", ts_summary$TSVCDREF)
  #   if(debug) cat("Rows where TSVCDREF was set to 'ClinicalTrials.gov':", sum(ts_summary$TSVCDREF == "ClinicalTrials.gov", na.rm = TRUE), "\n")

  #  # New condition for TSVALNF = 'NI'
  #   ts_summary$TSVCDREF <- ifelse(ts_summary$TSVALNF == "NI", "ISO 21090", ts_summary$TSVCDREF)
  #   if(debug) cat("Rows where TSVCDREF was set to 'ISO 21090' for TSVALNF = 'NI':", sum(ts_summary$TSVALNF == "NI", na.rm = TRUE), "\n")

    
    # Extract EUDRACT number
    eudract_number <- study_info$protocolSection$identificationModule$secondaryIdInfos %>%
      filter(type == "EUDRACT_NUMBER") %>%
      pull(id)
    
    # Create additional rows for REGID with NCTID and EUDRACT number
    regid_rows <- ts_summary %>%
      filter(TSPARMCD == "REGID")
    
    regid_nctid_rows <- regid_rows %>%
      slice(rep(1:n(), each = length(nct_ids))) %>%
      mutate(TSVAL = rep(nct_ids, times = nrow(regid_rows)), 
             TSVCDREF = "ClinicalTrials.gov", 
             TSVALCD = TSVAL)
    
    regid_eudract_rows <- regid_rows %>%
      slice(rep(1:n(), each = length(eudract_number))) %>%
      mutate(TSVAL = rep(eudract_number, times = nrow(regid_rows)), 
             TSVCDREF = "EUDRACT", 
             TSVALCD = TSVAL)
    
    # Remove existing REGID rows
    ts_summary <- ts_summary %>%
      filter(TSPARMCD != "REGID")
    
    # Combine the original ts_summary with the new REGID rows
    ts_summary <- bind_rows(ts_summary, regid_nctid_rows, regid_eudract_rows)
    
    # Combined condition for TSVCDREF and TSVALCD
    ts_summary <- ts_summary %>%
      mutate(
        TSVCDREF = case_when(
          TSPARMCD == "REGID" & grepl("^NCT", TSVAL) ~ "ClinicalTrials.gov",
          TSPARMCD == "REGID" & !is.na(TSVAL) & TSVAL != "" ~ "EUDRACT",
          TSVALNF == "NI" ~ "ISO 21090",
          !is.na(TSVAL) & TSVAL != "" ~ "ClinicalTrials.gov",
          TRUE ~ TSVCDREF
        ),
        TSVALCD = ifelse(TSPARMCD == "REGID" & !is.na(TSVAL) & TSVAL != "", TSVAL, TSVALCD)
      )
    
    if(debug) {
      cat("Rows where TSVCDREF was set to 'EUDRACT' and TSVALCD was set to TSVAL for REGID:", sum(ts_summary$TSPARMCD == "REGID" & ts_summary$TSVAL == eudract_number), "\n")
      cat("Rows where TSVCDREF was set to 'ClinicalTrials.gov' for REGID with TSVAL starting with 'NCT':", sum(ts_summary$TSPARMCD == "REGID" & grepl("^NCT", ts_summary$TSVAL)), "\n")
      cat("Rows where TSVALNF was set to 'NI':", sum(ts_summary$TSVALNF == "NI", na.rm = TRUE), "\n")
      cat("Rows where TSVCDREF was set to 'ISO 21090' for TSVALNF = 'NI':", sum(ts_summary$TSVALNF == "NI", na.rm = TRUE), "\n")
      cat("Rows where TSVCDREF was set to 'ClinicalTrials.gov':", sum(ts_summary$TSVCDREF == "ClinicalTrials.gov", na.rm = TRUE), "\n")
    }
    # Ensure only desired columns are present and in the correct order
    desired_columns <- c("STUDYID", "DOMAIN", "TSSEQ", "TSGRPID", "TSPARMCD", "TSPARM", "TSVAL", "TSVALNF", "TSVALCD", "TSVCDREF", "TSVCDVER")
    extra_tsval_columns <- grep("^TSVAL\\d+$", colnames(ts_summary), value = TRUE)
    desiredx_columns <- c(desired_columns, extra_tsval_columns)
    ts_summary <- ts_summary[, desiredx_columns]

    # Count unique TSPARAMCD's
    unique_tsparmcd_count <- length(unique(ts_summary$TSPARMCD))

    # Count non-missing TSVAL's
    non_missing_tsval_count <- sum(!is.na(ts_summary$TSVAL) & ts_summary$TSVAL != "")


    # Print summary to console
    cat("\nTS Domain Summary:\n")
    cat("Total rows in Trial_Summary.xlsx:", template_row_count, "\n")
    cat("Total rows in output:", nrow(ts_summary), "\n")
    cat("Unique TSPARMCD count:", unique_tsparmcd_count, "\n")
    cat("Non-missing TSVAL count:", non_missing_tsval_count, "\n")
    cat("Columns in output:", paste(colnames(ts_summary), collapse = ", "), "\n")

    # Compare TSPARAM/TSPARAMCD between template and output
    template_params <- unique(ts_template$TSPARMCD)
    output_params <- unique(ts_summary$TSPARMCD)
    missing_params <- setdiff(template_params, output_params)
    extra_params <- setdiff(output_params, template_params)

    cat("\nComparison of TSPARAM/TSPARAMCD:\n")
    cat("Total unique TSPARAM/TSPARAMCD in Trial_Summary.xlsx:", length(template_params), "\n")
    cat("Total unique TSPARAM/TSPARAMCD in output:", length(output_params), "\n")
    
    if (length(missing_params) > 0) {
      cat("Missing TSPARAM/TSPARAMCD in output:", paste(missing_params, collapse = ", "), "\n")
    } else {
      cat("No missing TSPARAM/TSPARAMCD in output.\n")
    }
    
    if (length(extra_params) > 0) {
      cat("Extra TSPARAM/TSPARAMCD in output:", paste(extra_params, collapse = ", "), "\n")
    } else {
      cat("No extra TSPARAM/TSPARAMCD in output.\n")
    }

    # Reset TSSEQ for each TSPARMCD
    ts_summary <- ts_summary[order(ts_summary$TSPARMCD), ]
    ts_summary$TSSEQ <- ave(seq_along(ts_summary$TSPARMCD), ts_summary$TSPARMCD, FUN = seq_along)

    if(debug) {
      cat("\nTSSEQ reset check:\n")
      tsseq_check <- aggregate(
        TSSEQ ~ TSPARMCD, 
        data = ts_summary, 
        FUN = function(x) c(min = min(x), max = max(x), count = length(x))
      )
      print(tsseq_check)
    }

    # Arrange TSVAL1 column next to TSVAL
    ts_summary <- ts_summary %>%
      select(STUDYID, DOMAIN, TSSEQ, TSGRPID, TSPARMCD, TSPARM, TSVAL, starts_with("TSVAL1"), everything())
    
    if(debug) {
      cat("Final ts_summary after arranging TSVAL1 column has", nrow(ts_summary), "rows and", ncol(ts_summary), "columns\n")
      print(colnames(ts_summary))
    }

    # Write to Excel with minimal formatting
    output_file <- file.path(output_dir, paste0(study_id, "_TS.xlsx"))
    
    if(debug) cat("Attempting to write to:", output_file, "\n")

    tryCatch({
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "TS")
      
      if(debug) cat("Worksheet added\n")

      # Write data
      openxlsx::writeData(wb, "TS", ts_summary)
      
      if(debug) cat("Data written to worksheet\n")

      # Set column widths
      openxlsx::setColWidths(wb, "TS", cols = 1:ncol(ts_summary), widths = "auto")
      
      if(debug) cat("Column widths set\n")

      # Save workbook
      openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
      
      if(debug) cat("Workbook saved\n")
    }, error = function(e) {
      cat("Error in Excel writing process:", conditionMessage(e), "\n")
    })

    if(debug) {
      if(file.exists(output_file)) {
        cat("File successfully created at:", output_file, "\n")
        cat("File size:", file.size(output_file), "bytes\n")
      } else {
        cat("File was not created at:", output_file, "\n")
      }
    }

    cat("\nTS domain data written to:", output_file, "\n")

    # After creating the initial TS domain data frame
    ts_summary <- integrate_controlled_terminology(ts_summary, debug)

    return(list(
      ts_summary = ts_summary,
      template_row_count = template_row_count,
      output_row_count = nrow(ts_summary),
      unique_tsparmcd_count = unique_tsparmcd_count,
      non_missing_tsval_count = non_missing_tsval_count,
      missing_params = missing_params,
      extra_params = extra_params
    ))
  }, error = function(e) {
    cat("Error in create_ts_domain:", conditionMessage(e), "\n")
    print(e)
    return(NULL)
  })
}

#' Integrate Controlled Terminology
#'
#' This function integrates controlled terminology into the TS domain's TSVALCD variable.
#'
#' @param ts_summary The TS domain data frame
#' @param debug Boolean flag to enable debug mode
#'
#' @return Updated TS domain data frame with TSVALCD populated
#' @keywords internal
integrate_controlled_terminology <- function(ts_summary, debug = FALSE) {
  if(debug) cat("Starting integration of controlled terminology\n")

  # Step 1: Extract Study Start Date (SSTDTC)
  sstdtc <- ts_summary$TSVAL[ts_summary$TSPARMCD == "SSTDTC"]
  if(length(sstdtc) == 0 || is.na(sstdtc)) {
    if(debug) cat("SSTDTC not found in TS domain\n")
    return(ts_summary)
  }

  # Step 2: Determine the Correct CT Version
  ct_version <- determine_ct_version(sstdtc)
  if(debug) cat("Determined CT version:", ct_version, "\n")

  # Step 3: Fetch the CT Package from the CDISC API
  ct_package <- fetch_ct_package(ct_version, debug)
  if(is.null(ct_package)) {
    if(debug) cat("Failed to fetch CT package\n")
    return(ts_summary)
  }

  # Step 4: Extract TS-Related Controlled Terminology
  ts_terminology <- extract_ts_terminology(ct_package, debug)

  # Step 5: Flatten the Terms from the Codelists
  flat_terminology <- flatten_terminology(ts_terminology, debug)

  # Step 6: Merge the CT Data with the TS Data
  ts_summary <- merge_ct_data(ts_summary, flat_terminology, debug)

  # Step 7: Handle Numeric TSVAL Values
  ts_summary <- handle_numeric_tsval(ts_summary, debug)

  if(debug) cat("Finished integrating controlled terminology\n")

  return(ts_summary)
}

#' Determine CT Version
#'
#' @param sstdtc Study start date
#' @return CT version string
#' @keywords internal
determine_ct_version <- function(sstdtc) {
  date <- as.Date(sstdtc)
  # Format the date as required by the CDISC API
  return(paste0("ct-", format(date, "%Y-%m-%d")))
}

#' Fetch CT Package from CDISC API
#'
#' @param ct_version CT version to fetch
#' @param debug Debug flag
#' @return CT package data
#' @keywords internal
fetch_ct_package <- function(ct_version, debug = FALSE) {
  base_url <- "https://library.cdisc.org/api"
  
  # Read API key from JSON file
  api_key_path <- system.file("extdata", "api_key.json", package = "autoTDD")
  if (!file.exists(api_key_path)) {
    if(debug) cat("API key file not found\n")
    return(NULL)
  }
  
  api_key <- jsonlite::fromJSON(api_key_path)
  if (!"api_key" %in% names(api_key)) {
    if(debug) cat("API key not found in JSON file\n")
    return(NULL)
  }
  
  # Set up headers with API key
  headers <- httr::add_headers(`api-key` = api_key$api_key)
  
  # Construct the URL for the specific CT package
  package_url <- paste0(base_url, "/mdr/ct/packages/", ct_version)
  
  if(debug) cat("Fetching CT package from:", package_url, "\n")
  
  # Fetch the package contents
  package_response <- httr::GET(package_url, headers)
  
  if(httr::status_code(package_response) != 200) {
    if(debug) cat("Failed to fetch CT package. Status code:", httr::status_code(package_response), "\n")
    return(NULL)
  }
  
  ct_package <- httr::content(package_response, "parsed")
  
  if(debug) cat("Successfully fetched CT package:", ct_version, "\n")
  
  return(ct_package)
}

#' Extract TS-Related Controlled Terminology
#'
#' @param ct_package CT package data
#' @param debug Debug flag
#' @return TS-related terminology
#' @keywords internal
extract_ts_terminology <- function(ct_package, debug = FALSE) {
  ts_terminology <- list()
  
  for(codelist in ct_package$codelists) {
    if(grepl("^TS", codelist$name) || grepl("TRIAL", codelist$name)) {
      ts_terminology[[codelist$name]] <- codelist$terms
    }
  }
  
  if(debug) cat("Extracted", length(ts_terminology), "TS-related codelists\n")
  
  return(ts_terminology)
}

#' Flatten Terminology
#'
#' @param ts_terminology TS-related terminology
#' @param debug Debug flag
#' @return Flattened terminology data frame
#' @importFrom dplyr filter mutate select slice bind_rows case_when
#' @keywords internal
flatten_terminology <- function(ts_terminology, debug = FALSE) {
  flat_terms <- data.frame(
    TSPARMCD = character(),
    TSVAL = character(),
    TSVALCD = character(),
    stringsAsFactors = FALSE
  )
  
  for(codelist_name in names(ts_terminology)) {
    for(term in ts_terminology[[codelist_name]]) {
      flat_terms <- rbind(flat_terms, data.frame(
        TSPARMCD = codelist_name,
        TSVAL = term$submissionValue,
        TSVALCD = term$conceptId,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if(debug) cat("Flattened terminology contains", nrow(flat_terms), "terms\n")
  
  return(flat_terms)
}

#' Merge CT Data with TS Data
#'
#' @param ts_summary TS domain data frame
#' @param flat_terminology Flattened terminology data frame
#' @param debug Debug flag
#' @return Updated TS domain data frame
#' @keywords internal
merge_ct_data <- function(ts_summary, flat_terminology, debug = FALSE) {
  merged_data <- merge(ts_summary, flat_terminology, 
                       by = c("TSPARMCD", "TSVAL"), 
                       all.x = TRUE)
  
  # Update TSVALCD where a match was found
  ts_summary$TSVALCD <- ifelse(!is.na(merged_data$TSVALCD.y), 
                               merged_data$TSVALCD.y, 
                               ts_summary$TSVALCD)
  
  if(debug) cat("Merged CT data with TS data\n")
  
  return(ts_summary)
}

#' Handle Numeric TSVAL Values
#'
#' @param ts_summary TS domain data frame
#' @param debug Debug flag
#' @return Updated TS domain data frame
#' @keywords internal
handle_numeric_tsval <- function(ts_summary, debug = FALSE) {
  numeric_rows <- which(grepl("^\\d+$", ts_summary$TSVAL))
  
  for(row in numeric_rows) {
    tsparmcd <- ts_summary$TSPARMCD[row]
    tsval <- ts_summary$TSVAL[row]
    
    # Here you would implement logic to assign the correct TSVALCD
    # This might involve looking up the appropriate codelist and finding the matching conceptId
    # For now, we'll just assign the TSVAL as the TSVALCD
    ts_summary$TSVALCD[row] <- tsval
  }
  
  if(debug) cat("Handled", length(numeric_rows), "numeric TSVAL values\n")
  
  return(ts_summary)
}
#' @export
process_study_info <- function(study_info, debug = FALSE) {
  if(debug) cat("Starting process_study_info function\n")

  if(is.null(study_info) || length(study_info) == 0) {
    if(debug) cat("Error: study_info is null or empty\n")
    return(NULL)
  }

  tryCatch({
    # Preserve the entire structure
    result <- study_info[[1]]

    if(debug) {
      cat("Processed study info structure:\n")
      print(str(result, max.level = 2))
    }

    return(result)
  }, error = function(e) {
    if(debug) {
      cat("Error in process_study_info:", conditionMessage(e), "\n")
      print(e)
    }
    return(NULL)  # Return NULL on error
  })
}


format_date_iso8601 <- function(date) {
  if (is.na(date) || is.null(date)) return(NA_character_)
  tryCatch({
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      return(date)  # Already in YYYY-MM-DD format
    } else if (grepl("^\\d{4}-\\d{2}$", date)) {
      return(paste0(date, "-01"))  # Convert YYYY-MM to YYYY-MM-01
    } else if (grepl("^\\d{4}$", date)) {
      return(paste0(date, "-01-01"))  # Convert YYYY to YYYY-01-01
    } else {
      formatted_date <- as.Date(date)
      return(format(formatted_date, "%Y-%m-%d"))
    }
  }, error = function(e) {
    warning("Error formatting date: ", e$message)
    return(NA_character_)
  })
}

#' Define TS Mapping
#'
#' This function defines the mapping between TS parameters and extracted fields.
#'
#' @param debug Boolean flag to enable debug mode
#' @return A list containing the TS mapping.
#' @keywords internal
define_ts_mapping <- function(debug = FALSE) {
  mapping <- list(
    STUDYID = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$identificationModule$nctId else NA_character_,
    DOMAIN = function(df) "TS",
    TSSEQ = function(df) NA_integer_,  # We'll populate this later in create_ts_domain
    TSPARMCD = function(df) NA_character_,  # This will be set in create_ts_domain
    TSPARM = function(df) NA_character_,  # This will be set in create_ts_domain
    TSVAL = function(df) NA_character_,  # This will be populated based on other mappings
    TSVALNF = function(df) NA_character_,
    TSVALCD = function(df) NA_character_,
    TSVCDREF = function(df) NA_character_,
    TSVCDVER = function(df) NA_character_,
    TITLE = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$identificationModule$officialTitle else NA_character_,
    TPHASE = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$phases)) {
          phases <- df[[1]]$protocolSection$designModule$phases
          
roman_phases <- c(
            "PHASE0" = "PHASE 0 TRIAL", "PHASE 0" = "PHASE 0 TRIAL",
            "PHASE1" = "PHASE I TRIAL", "PHASE 1" = "PHASE I TRIAL", "PHASE I" = "PHASE I TRIAL",
            "PHASE2" = "PHASE II TRIAL", "PHASE 2" = "PHASE II TRIAL", "PHASE II" = "PHASE II TRIAL",
            "PHASE3" = "PHASE III TRIAL", "PHASE 3" = "PHASE III TRIAL", "PHASE III" = "PHASE III TRIAL",
            "PHASE4" = "PHASE IV TRIAL", "PHASE 4" = "PHASE IV TRIAL", "PHASE IV" = "PHASE IV TRIAL",
            "PHASE5" = "PHASE V TRIAL", "PHASE 5" = "PHASE V TRIAL", "PHASE V" = "PHASE V TRIAL",
            "PHASE1/PHASE2" = "PHASE I/II TRIAL", "PHASE 1/PHASE 2" = "PHASE I/II TRIAL", "PHASE I/II" = "PHASE I/II TRIAL",
            "PHASE1/PHASE2/PHASE3" = "PHASE I/II/III TRIAL", "PHASE 1/PHASE 2/PHASE 3" = "PHASE I/II/III TRIAL", "PHASE I/II/III" = "PHASE I/II/III TRIAL",
            "PHASE1/PHASE3" = "PHASE I/III TRIAL", "PHASE 1/PHASE 3" = "PHASE I/III TRIAL", "PHASE I/III" = "PHASE I/III TRIAL",
            "PHASE2/PHASE3" = "PHASE II/III TRIAL", "PHASE 2/PHASE 3" = "PHASE II/III TRIAL", "PHASE II/III" = "PHASE II/III TRIAL",
            "PHASE1A" = "PHASE IA TRIAL", "PHASE 1A" = "PHASE IA TRIAL", "PHASE IA" = "PHASE IA TRIAL",
            "PHASE1B" = "PHASE IB TRIAL", "PHASE 1B" = "PHASE IB TRIAL", "PHASE IB" = "PHASE IB TRIAL",
            "PHASE2A" = "PHASE IIA TRIAL", "PHASE 2A" = "PHASE IIA TRIAL", "PHASE IIA" = "PHASE IIA TRIAL",
            "PHASE2B" = "PHASE IIB TRIAL", "PHASE 2B" = "PHASE IIB TRIAL", "PHASE IIB" = "PHASE IIB TRIAL",
            "PHASE3A" = "PHASE IIIA TRIAL", "PHASE 3A" = "PHASE IIIA TRIAL", "PHASE IIIA" = "PHASE IIIA TRIAL",
            "PHASE3B" = "PHASE IIIB TRIAL", "PHASE 3B" = "PHASE IIIB TRIAL", "PHASE IIIB" = "PHASE IIIB TRIAL",
            "EARLYPHASE1" = "PHASE Early I TRIAL", "EARLY PHASE 1" = "PHASE Early I TRIAL",
            "NOT APPLICABLE" = "N/A"
          )
          
          mapped_phases <- sapply(phases, function(phase) {
            phase <- toupper(gsub(" ", "", phase))
            if (phase %in% names(roman_phases)) {
              return(roman_phases[phase])
            } else {
              # If no exact match, return original with "TRIAL" suffix
              return(paste(phase, "TRIAL"))
            }
          })
          
          return(unique(mapped_phases))  # Return unique phases
        } else {
          return(NA_character_)
        }
      }, error = function(e) {
        warning("Error in TPHASE mapping: ", e$message)
        return(NA_character_)
      })
    },
    INDIC = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$conditionsModule$conditions[[1]] else NA_character_,
    TDIGRP = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$conditionsModule$conditions[[1]] else NA_character_,
    THERAREA = function(df) NA_character_,
    REGID = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$secondaryIdInfos)) {
          secondary_ids <- df[[1]]$protocolSection$identificationModule$secondaryIdInfos
          eudract_row <- secondary_ids[secondary_ids$type == "EUDRACT_NUMBER", ]
          if (nrow(eudract_row) > 0) {
            return(eudract_row$id)
          }
        }
        return("NA")
      }, error = function(e) {
        warning("Error in REGID mapping: ", e$message)
        return(NA_character_)
      })
    },
    INTTYPE = function(df) { 
      if(debug) cat("Processing INTTYPE\n")
      interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
      
      if(debug) {
        cat("Interventions structure:\n")
        print(str(interventions))
      }
      
      if (is.data.frame(interventions) && "type" %in% colnames(interventions)) {
          int_types <- unique(interventions$type)
          if(debug) cat("Unique INTTYPE values:", paste(int_types, collapse = ", "), "\n")
          return(int_types)
        } else {
        if(debug) cat("Unexpected interventions format\n")
        return(NA_character_)
        }
      },
    STYPE = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$designModule$studyType else NA_character_,
    TTYPE = function(trial_data) {
      primary_purpose <- unlist(lapply(trial_data, function(x) x$protocolSection$designModule$designInfo$primaryPurpose))
      if (length(primary_purpose) > 0) {
        # Convert to title case and return the first value (in case of multiple values)
        return(tools::toTitleCase(toupper(primary_purpose[1])))
      }
      return(NA_character_)
    },
    ACTSUB = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$enrollmentInfo)) {
          enrollment_info <- df[[1]]$protocolSection$designModule$enrollmentInfo
          if(!is.null(enrollment_info$type) && !is.null(enrollment_info$count)) {
            if(enrollment_info$type[1] == "ACTUAL") {
              return(as.character(enrollment_info$count[1]))
            }
          }
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in ACTSUB mapping: ", e$message)
        return(NA_character_)
      })
    },
    PLANSUB = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$enrollmentInfo$count)) {
          return(as.character(df[[1]]$protocolSection$designModule$enrollmentInfo$count))
        }
        return("NA")
      }, error = function(e) {
        warning("Error in PLANSUB mapping: ", e$message)
        return(NA_character_)
      })
    },
    SENDTC = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$completionDateStruct$date)) {
          return(format_date_iso8601(df[[1]]$protocolSection$statusModule$completionDateStruct$date))
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in SENDTC mapping: ", e$message)
        return(NA_character_)
      })
    },
    SSTDTC = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$startDateStruct$date)) {
          return(format_date_iso8601(df[[1]]$protocolSection$statusModule$startDateStruct$date))
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in SSTDTC mapping: ", e$message)
        return(NA_character_)
      })
    },
    LENGTH = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$startDateStruct$date) && !is.null(df[[1]]$protocolSection$statusModule$completionDateStruct$date)) {
          start <- as.Date(format_date_iso8601(df[[1]]$protocolSection$statusModule$startDateStruct$date))
          end <- as.Date(format_date_iso8601(df[[1]]$protocolSection$statusModule$completionDateStruct$date))
          duration <- as.numeric(end - start)
          return(paste0("P", duration, "D"))  # ISO8601 duration format
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in LENGTH mapping: ", e$message)
        return(NA_character_)
      })
    },
    ADAPT = function(df) derive_adaptive_design(df),
    ADDON = function(df) derive_addon_treatment(df),
    AGEMIN = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$minimumAge)) {
          age_min <- df[[1]]$protocolSection$eligibilityModule$minimumAge
          
          age_parts <- strsplit(trimws(age_min), " ")[[1]]
          if(length(age_parts) != 2) return(NA_character_)
          
          age_value <- as.numeric(age_parts[1])
          age_unit <- tolower(age_parts[2])
          
          if(grepl("year", age_unit)) {
            return(sprintf("P%dY", age_value))
          } else if(grepl("month", age_unit)) {
            return(sprintf("P%dM", age_value))
          } else if(grepl("week", age_unit)) {
            return(sprintf("P%dW", age_value))
          } else if(grepl("day", age_unit)) {
            return(sprintf("P%dD", age_value))
          } else {
            return(NA_character_)
          }
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in AGEMIN mapping: ", e$message)
        return(NA_character_)
      })
    },
    AGEMAX = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$maximumAge)) {
          age_max <- df[[1]]$protocolSection$eligibilityModule$maximumAge
          
          age_parts <- strsplit(trimws(age_max), " ")[[1]]
          if(length(age_parts) != 2) return(NA_character_)
          
          age_value <- as.numeric(age_parts[1])
          age_unit <- tolower(age_parts[2])
          
          if(grepl("year", age_unit)) {
            return(sprintf("P%dY", age_value))
          } else if(grepl("month", age_unit)) {
            return(sprintf("P%dM", age_value))
          } else if(grepl("week", age_unit)) {
            return(sprintf("P%dW", age_value))
          } else if(grepl("day", age_unit)) {
            return(sprintf("P%dD", age_value))
          } else {
            return(NA_character_)
          }
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in AGEMAX mapping: ", e$message)
        return(NA_character_)
      })
    },
    COMPTRT = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$armsInterventionsModule$interventions)) {
          interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
          comp_treatments <- interventions[grepl("carboplatin|cisplatin|pemetrexed", interventions$name, ignore.case = TRUE), ]
          if (nrow(comp_treatments) > 0) {
            return(paste(comp_treatments$name, collapse = "; "))
          }
        }
        return("NA")
      }, error = function(e) {
        warning("Error in COMPTRT mapping: ", e$message)
        return(NA_character_)
      })
    },
    CURTRT = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$armsInterventionsModule$interventions)) {
          interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
          cur_treatments <- interventions[grepl("divarasib|pembrolizumab", interventions$name, ignore.case = TRUE), ]
          if (nrow(cur_treatments) > 0) {
            return(paste(cur_treatments$name, collapse = "; "))
          }
        }
        return("NA")
      }, error = function(e) {
        warning("Error in CURTRT mapping: ", e$message)
        return(NA_character_)
      })
    },
    NARMS = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$armsInterventionsModule$interventions) && "armGroupLabels" %in% names(df[[1]]$protocolSection$armsInterventionsModule$interventions)) {
          all_labels <- unlist(df[[1]]$protocolSection$armsInterventionsModule$interventions$armGroupLabels)
          unique_cohorts <- unique(all_labels)
          return(as.character(length(unique_cohorts)))
        }
        return("NA")
      }, error = function(e) {
        warning("Error in NCOHORT mapping: ", e$message)
        return(NA_character_)
      })
    },
    DCUTDESC = function(df) derive_data_cutoff_description(df),
    DCUTDTC = function(df) if(is.list(df) && length(df) > 0) format_date_iso8601(df[[1]]$protocolSection$statusModule$primaryCompletionDateStruct$date) else NA_character_,
    EGBLIND = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$designModule$masking else NA_character_,
    INTMODEL = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$designInfo$interventionModel)) {
          return(paste(df[[1]]$protocolSection$designModule$designInfo$interventionModel, collapse = "; "))
        }
        return("NA")
      }, error = function(e) {
        warning("Error in INTMODEL mapping: ", e$message)
        return(NA_character_)
      })
    },
    NCOHORT = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$designModule$numberOfArms else NA_integer_,
    ONGOSIND = function(df) if(is.list(df) && length(df) > 0 && df[[1]]$protocolSection$statusModule$overallStatus %in% c("Recruiting", "Active, not recruiting")) "Y" else "N",
    OUTMSPRI = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$outcomesModule$primaryOutcomes)) {
          measures <- df[[1]]$protocolSection$outcomesModule$primaryOutcomes$measure
          return(na.omit(measures))
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in OUTMSPRI mapping: ", e$message)
        return(NA_character_)
      })
    },
    OUTMSSEC = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$outcomesModule$secondaryOutcomes)) {
          measures <- df[[1]]$protocolSection$outcomesModule$secondaryOutcomes$measure
          return(na.omit(measures))
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in OUTMSSEC mapping: ", e$message)
        return(NA_character_)
      })
    },
    OUTMSEXP = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$outcomesModule$otherOutcomes)) {
          measures <- df[[1]]$protocolSection$outcomesModule$otherOutcomes$measure
          return(na.omit(measures))
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in OUTMSEXP mapping: ", e$message)
        return(NA_character_)
      })
    },
    SEXPOP = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$sex)) {
          sex <- df[[1]]$protocolSection$eligibilityModule$sex
          if (toupper(sex) == "ALL") {
            return("BOTH")
          }
          return(sex)
        }
        return("NA")
      }, error = function(e) {
        warning("Error in SEXPOP mapping: ", e$message)
        return(NA_character_)
      })
    },
    SPONSOR = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$organization$fullName)) {
          return(df[[1]]$protocolSection$identificationModule$organization$fullName)
        }
        return("NA")
      }, error = function(e) {
        warning("Error in SPONSOR mapping: ", e$message)
        return(NA_character_)
      })
    },
    FCNTRY = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$contactsLocationsModule$locations) && "country" %in% names(df[[1]]$protocolSection$contactsLocationsModule$locations)) {
          locations <- df[[1]]$protocolSection$contactsLocationsModule$locations
          unique_countries <- unique(locations$country)
          unique_countries <- unique_countries[!is.na(unique_countries) & unique_countries != ""]
          if (length(unique_countries) > 0) {
            return(toupper(unique_countries))
          }
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in FCNTRY mapping: ", e$message)
        return(NA_character_)
      })
    },
    HLTSUBJI = function(df) derive_healthy_subjects(df),
    EXTTIND = function(df) derive_extension_trial(df),
    PDSTIND = function(df) {
      if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$minimumAge)) {
        min_age <- df[[1]]$protocolSection$eligibilityModule$minimumAge
        min_age <- as.numeric(gsub("[^0-9.]", "", min_age))
        if (!is.na(min_age) && min_age < 6) {
          return("Y")
        } else {
          return("N")
        }
      }
      return(NA_character_)
    },
    SPREFID = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0) {
          return(df[[1]]$protocolSection$identificationModule$nctId[1])  # In case there are multiple, take the first one
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in SPREFID mapping: ", e$message)
        return(NA_character_)
      })
    },
    TRT = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$armsInterventionsModule$interventions)) {
          treatments <- df[[1]]$protocolSection$armsInterventionsModule$interventions$name
          return(paste(unique(treatments), collapse = ", "))
        }
        return("NA")
      }, error = function(e) {
        warning("Error in TRT mapping: ", e$message)
        return(NA_character_)
      })
    },
    TBLIND = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$designInfo$maskingInfo$masking)) {
          masking <- df[[1]]$protocolSection$designModule$designInfo$maskingInfo$masking
          if (!is.null(masking) && length(masking) > 0) {
            return(paste(masking, collapse = "; "))
          }
        }
        return("OPEN LABEL")
      }, error = function(e) {
        warning("Error in TBLIND mapping: ", e$message)
        return("OPEN LABEL")
      })
    },
    TCNTRL = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$armsInterventionsModule$armGroups)) {
          arms <- df[[1]]$protocolSection$armsInterventionsModule$armGroups
          if (any(grepl("placebo", tolower(arms$type)))) {
            return("PLACEBO")
          }
          if (any(grepl("active comparator", tolower(arms$type)))) {
            return("ACTIVE")
          }
          if (length(unique(arms$type)) == 1 && unique(arms$type) == "EXPERIMENTAL") {
            return("DOSE COMPARISON")
          }
          return("NONE")
        }
        return("NA")
      }, error = function(e) {
        warning("Error in TCNTRL mapping: ", e$message)
        return(NA_character_)
      })
    }
  )
  
  return(mapping)
}


#' Derive Comparative Treatment
#'
#' @param df The trial data frame.
#' @return A character string of comparative treatments, if any.
#' @keywords internal
derive_comparative_treatment <- function(df) {
  if (!is.null(df[[1]]$protocolSection$armsInterventionsModule$interventions) && !is.na(df[[1]]$protocolSection$armsInterventionsModule$interventions)) {
    arms <- unlist(lapply(df[[1]]$protocolSection$armsInterventionsModule$interventions, function(x) x$description))
    comp_arms <- arms[!grepl("placebo|control", tolower(arms))]
    if (length(comp_arms) > 0) {
      return(paste(comp_arms, collapse = "; "))
    }
  }
  return(NA)
}

#' Derive Therapeutic Area
#'
#' @param condition The condition being studied.
#' @param interventions The interventions being used.
#' @return A character string representing the therapeutic area.
#' @keywords internal
derive_therapeutic_area <- function(df) {
  tryCatch({
    if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$conditionsModule$conditions)) {
      condition <- df[[1]]$protocolSection$conditionsModule$conditions[[1]]
      interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
    } else {
      return(NA_character_)
    }
    
    if (is.null(condition)) return(NA_character_)
    
    intervention_type <- if(!is.null(interventions) && is.list(interventions) && length(interventions) > 0) 
      interventions[[1]]$type 
    else 
      NA_character_
    
    if (grepl("cancer|neoplasm|tumor|carcinoma", condition, ignore.case = TRUE)) {
      return("Oncology")
    } else if (grepl("heart|cardio|stroke", condition, ignore.case = TRUE)) {
      return("Cardiovascular")
    } else if (grepl("diabetes|endocrine", condition, ignore.case = TRUE)) {
      return("Endocrinology")
    } else if (grepl("neuro|brain|alzheimer|parkinson", condition, ignore.case = TRUE)) {
      return("Neurology")
    } else if (grepl("psych|depression|anxiety", condition, ignore.case = TRUE)) {
      return("Psychiatry")
    } else if (!is.na(intervention_type) && grepl("vaccine|immun", intervention_type, ignore.case = TRUE)) {
      return("Immunology")
    } else {
      return("Other")
    }
  }, error = function(e) {
    warning("Error in THERAREA mapping: ", e$message)
    return(NA_character_)
  })
}

#' Derive Trial Type
#'
#' @param df The trial data frame.
#' @return A character string representing the trial type.
#' @keywords internal
derive_trial_type <- function(df) {
  tryCatch({
    if (is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$outcomesModule$primaryOutcomes)) {
      primary_outcomes <- df[[1]]$protocolSection$outcomesModule$primaryOutcomes
      if (length(primary_outcomes) > 0) {
        descriptions <- sapply(primary_outcomes, function(x) tolower(x$description))
        
        types <- character(0)
        if (any(grepl("safety|adverse|toxicity", descriptions))) {
          types <- c(types, "SAFETY")
        }
        if (any(grepl("efficacy|effectiveness|response|outcome", descriptions))) {
          types <- c(types, "EFFICACY")
        }
        if (any(grepl("pharmacokinetics|pk|bioavailability|bioequivalence", descriptions))) {
          types <- c(types, "PHARMACOKINETICS")
        }
        if (any(grepl("pharmacodynamics|pd", descriptions))) {
          types <- c(types, "PHARMACODYNAMICS")
        }
        if (any(grepl("pharmacoeconomics|cost|economic", descriptions))) {
          types <- c(types, "PHARMACOECONOMICS")
        }
        
        if (length(types) == 0) {
          return("OTHER")
        } else {
          return(paste(unique(types), collapse = " "))
        }
      }
    }
    return(NA_character_)
  }, error = function(e) {
    warning("Error in TTYPE mapping: ", e$message)
    return(NA_character_)
  })
}

#' Derive Adaptive Design
#'
#' @param df The trial data frame.
#' @return "Y" if adaptive design, "N" otherwise.
#' @keywords internal
derive_adaptive_design <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$briefTitle) && !is.na(df[[1]]$protocolSection$identificationModule$briefTitle) && grepl("adaptive", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE)) {
    return("Y")
  } else if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$officialTitle) && !is.na(df[[1]]$protocolSection$identificationModule$officialTitle) && grepl("adaptive", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE)) {
    return("Y")
  } else {
    return("N")
  }
}

#' Derive Add-on Treatment
#'
#' @param df The trial data frame.
#' @return "Y" if add-on treatment, "N" otherwise.
#' @keywords internal
derive_addon_treatment <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$briefTitle) && !is.na(df[[1]]$protocolSection$identificationModule$briefTitle) && grepl("add-on|addon|adjunct", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE)) {
    return("Y")
  } else if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$officialTitle) && !is.na(df[[1]]$protocolSection$identificationModule$officialTitle) && grepl("add-on|addon|adjunct", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE)) {
    return("Y")
  } else {
    return("N")
  }
}

#' Derive Data Cutoff Description
#'
#' @param df The trial data frame.
#' @return A character string describing the data cutoff.
#' @keywords internal
derive_data_cutoff_description <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$overallStatus) && !is.na(df[[1]]$protocolSection$statusModule$overallStatus) && df[[1]]$protocolSection$statusModule$overallStatus %in% c("Completed", "Terminated")) {
    return("FINAL")
  } else {
    return("INTERIM")
  }
}

#' Derive Data Cutoff Date
#'
#' @param df The trial data frame.
#' @return A date string for the data cutoff.
#' @keywords internal
derive_data_cutoff_date <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$overallStatus) && !is.na(df[[1]]$protocolSection$statusModule$overallStatus) && df[[1]]$protocolSection$statusModule$overallStatus %in% c("Completed", "Terminated")) {
    return(format_date_iso8601(df[[1]]$protocolSection$statusModule$completionDateStruct$date))
  } else {
    return(NA)
  }
}

#' Convert Phase to Roman Numeral
#'
#' @param phase A character string representing the trial phase.
#' @return A character string with the phase in Roman numeral format.
#' @keywords internal
convert_to_roman <- function(phase) {
  phase <- toupper(phase)
  roman_phases <- c(
            "PHASE0" = "PHASE 0 TRIAL", "PHASE 0" = "PHASE 0 TRIAL",
            "PHASE1" = "PHASE I TRIAL", "PHASE 1" = "PHASE I TRIAL", "PHASE I" = "PHASE I TRIAL",
            "PHASE2" = "PHASE II TRIAL", "PHASE 2" = "PHASE II TRIAL", "PHASE II" = "PHASE II TRIAL",
            "PHASE3" = "PHASE III TRIAL", "PHASE 3" = "PHASE III TRIAL", "PHASE III" = "PHASE III TRIAL",
            "PHASE4" = "PHASE IV TRIAL", "PHASE 4" = "PHASE IV TRIAL", "PHASE IV" = "PHASE IV TRIAL",
            "PHASE5" = "PHASE V TRIAL", "PHASE 5" = "PHASE V TRIAL", "PHASE V" = "PHASE V TRIAL",
            "PHASE1/PHASE2" = "PHASE I/II TRIAL", "PHASE 1/PHASE 2" = "PHASE I/II TRIAL", "PHASE I/II" = "PHASE I/II TRIAL",
            "PHASE1/PHASE2/PHASE3" = "PHASE I/II/III TRIAL", "PHASE 1/PHASE 2/PHASE 3" = "PHASE I/II/III TRIAL", "PHASE I/II/III" = "PHASE I/II/III TRIAL",
            "PHASE1/PHASE3" = "PHASE I/III TRIAL", "PHASE 1/PHASE 3" = "PHASE I/III TRIAL", "PHASE I/III" = "PHASE I/III TRIAL",
            "PHASE2/PHASE3" = "PHASE II/III TRIAL", "PHASE 2/PHASE 3" = "PHASE II/III TRIAL", "PHASE II/III" = "PHASE II/III TRIAL",
            "PHASE1A" = "PHASE IA TRIAL", "PHASE 1A" = "PHASE IA TRIAL", "PHASE IA" = "PHASE IA TRIAL",
            "PHASE1B" = "PHASE IB TRIAL", "PHASE 1B" = "PHASE IB TRIAL", "PHASE IB" = "PHASE IB TRIAL",
            "PHASE2A" = "PHASE IIA TRIAL", "PHASE 2A" = "PHASE IIA TRIAL", "PHASE IIA" = "PHASE IIA TRIAL",
            "PHASE2B" = "PHASE IIB TRIAL", "PHASE 2B" = "PHASE IIB TRIAL", "PHASE IIB" = "PHASE IIB TRIAL",
            "PHASE3A" = "PHASE IIIA TRIAL", "PHASE 3A" = "PHASE IIIA TRIAL", "PHASE IIIA" = "PHASE IIIA TRIAL",
            "PHASE3B" = "PHASE IIIB TRIAL", "PHASE 3B" = "PHASE IIIB TRIAL", "PHASE IIIB" = "PHASE IIIB TRIAL",
            "EARLYPHASE1" = "PHASE Early I TRIAL", "EARLY PHASE 1" = "PHASE Early I TRIAL",
            "NOT APPLICABLE" = "N/A"
  )

  if (phase %in% names(roman_phases)) {
    return(roman_phases[phase])
  } else {
    return(phase)  # Return original if no match found
  }
}

#' Format Date to ISO 8601
#'
#' @param date A date string.
#' @return A character string representing the date in ISO 8601 format.
#' @keywords internal
format_date_iso8601 <- function(date) {
  if (is.na(date) || is.null(date)) return(NA_character_)
  tryCatch({
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      return(date)  # Already in YYYY-MM-DD format
    } else if (grepl("^\\d{4}-\\d{2}$", date)) {
      return(paste0(date, "-01"))  # Convert YYYY-MM to YYYY-MM-01
    } else if (grepl("^\\d{4}$", date)) {
      return(paste0(date, "-01-01"))  # Convert YYYY to YYYY-01-01
    } else {
      formatted_date <- as.Date(date)
      return(format(formatted_date, "%Y-%m-%d"))
    }
  }, error = function(e) {
    warning("Error formatting date: ", e$message)
    return(NA_character_)
  })
}

#' Clean TSVAL
#'
#' @param tsval A character string representing a TSVAL.
#' @return A cleaned character string.
#' @keywords internal
clean_tsval <- function(tsval) {
  if (is.na(tsval)) return(NA)
  tsval <- gsub("Drug: |DRUG: |Allocation: |Intervention Model: |Masking: |Primary Purpose: ", "", tsval)
  tsval <- gsub(" ; ", "; ", tsval)  # Remove spaces before semicolons
  tsval <- gsub("^\\s+|\\s+$", "", tsval)  # Trim leading/trailing whitespace
  tsval <- gsub("[;]+", ";", tsval)  # Remove multiple semicolons
  tsval <- unique(unlist(strsplit(tsval, ";")))  # Remove duplicate values
  tsval <- paste(tsval, collapse = "; ")
  return(tsval)
}

#' Calculate Trial Length
#'
#' @param start_date The start date of the trial.
#' @param end_date The end date of the trial.
#' @return A character string representing the trial length in ISO 8601 duration format.
#' @keywords internal
calculate_trial_length <- function(start_date, end_date) {
  if (!is.na(start_date) && !is.na(end_date)) {
    start <- as.Date(start_date, format="%Y-%m-%d")
    end <- as.Date(end_date, format="%Y-%m-%d")
    length <- as.numeric(difftime(end, start, units = "days"))
    return(paste0("P", length, "D"))  # ISO 8601 duration format
  } else {
    return(NA)
  }
}

#' Derive Extension Trial Indicator
#'
#' @param df The trial data frame.
#' @return "Y" if extension trial, "N" otherwise.
#' @keywords internal
derive_extension_trial <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$briefTitle) && !is.na(df[[1]]$protocolSection$identificationModule$briefTitle) && grepl("extension|follow-up|followup", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE)) {
    return("Y")
  } else if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$identificationModule$officialTitle) && !is.na(df[[1]]$protocolSection$identificationModule$officialTitle) && grepl("extension|follow-up|followup", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE)) {
    return("Y")
  } else {
    return("N")
  }
}

#' Derive Healthy Subject Indicator
#'
#' @param df The trial data frame.
#' @return "Y" if healthy subjects, "N" otherwise.
#' @keywords internal
derive_healthy_subjects <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers) && !is.na(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers)) {
    if (tolower(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers) == "yes") {
      return("Y")
    } else if (tolower(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers) == "no") {
      return("N")
    }
  }
  return(NA)
}

#' Derive Pediatric Study Indicator
#'
#' @param df The trial data frame.
#' @return "Y" if pediatric study, "N" otherwise.
#' @keywords internal
derive_pediatric_study <- function(df) {
  if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule$minimumAge)) {
    min_age <- df[[1]]$protocolSection$eligibilityModule$minimumAge
    min_age <- as.numeric(gsub("[^0-9.]", "", min_age))
    if (!is.na(min_age) && min_age < 18) {
      return("Y")
    } else {
      return("N")
    }
  }
  return(NA)
}