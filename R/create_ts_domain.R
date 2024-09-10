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


#' Create TS Domain
#'
#' This function creates the TS domain based on the provided NCT IDs and study ID.
#'
#' @param nct_ids A vector of NCT IDs
#' @param study_id The study ID
#' @param output_dir The output directory (default: current working directory)
#' @param debug Boolean flag to enable debug mode (default: FALSE)
#'
#' @return A data frame containing the TS domain data
#' @export

create_ts_domain <- function(nct_ids, study_id, output_dir = getwd(), debug = FALSE) {
  tryCatch({
    if(debug) cat("Starting create_ts_domain function\n")

    ts_file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
    if(debug) cat("Looking for Trial_Summary.xlsx at:", ts_file_path, "\n")

    if (ts_file_path == "") {
      stop("Trial_Summary.xlsx not found in package.")
    }

    ts_summary <- openxlsx::read.xlsx(ts_file_path, sheet = "TS")
    if(debug) cat("Successfully read Trial_Summary.xlsx\n")

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

    ts_mapping <- define_ts_mapping()
    if(debug) cat("TS mapping defined\n")

    # Initialize an empty list to store new rows
    new_rows <- list()

    for (i in 1:nrow(ts_summary)) {
      param <- ts_summary$TSPARMCD[i]
      if (param %in% names(ts_mapping)) {
        if(debug) cat("Processing parameter:", param, "\n")
        
        mapped_values <- ts_mapping[[param]](trial_data)
        
        if (length(mapped_values) > 0) {
          for (j in 1:length(mapped_values)) {
            new_row <- ts_summary[i, ]
            new_row$STUDYID <- study_id
            new_row$DOMAIN <- "TS"
            new_row$TSVAL <- as.character(mapped_values[j])
            new_rows <- c(new_rows, list(new_row))
          }
        }
      }
    }

    # Combine all new rows into a single data frame
    ts_summary <- do.call(rbind, new_rows)

    # Update TSSEQ
    ts_summary$TSSEQ <- sequence(rle(as.character(ts_summary$TSPARMCD))$lengths)

  # Process treatments
  all_treatments <- unique(unlist(strsplit(ts_summary$TSVAL[ts_summary$TSPARMCD == "TRT"], ", ")))
  treatments <- all_treatments[all_treatments != ""]  # Remove any empty strings
  treatment_rows <- data.frame(
    STUDYID = study_id,
    DOMAIN = "TS",
    TSSEQ = seq_along(treatments),
    TSPARMCD = "TRT",
    TSPARM = "Planned Treatment",
    TSVAL = toupper(treatments),
    TSVALCD = "TRT",
    stringsAsFactors = FALSE
  )

    # Process countries
    countries <- unique(unlist(strsplit(ts_summary$TSVAL[ts_summary$TSPARMCD == "FCNTRY"], ", ")))
    country_rows <- data.frame(
      STUDYID = study_id,
      DOMAIN = "TS",
      TSSEQ = seq_along(countries),
      TSPARMCD = "FCNTRY",
      TSPARM = "Country",
      TSVAL = toupper(countries),
      TSVALCD = NA,
      stringsAsFactors = FALSE
    )

    # Define the desired columns in the correct order
    desired_columns <- c("STUDYID", "DOMAIN", "TSSEQ", "TSGRPID", "TSPARMCD", "TSPARM", "TSVAL", "TSVALNF", "TSVALCD", "TSVCDREF", "TSVCDVER")

    # Ensure all data frames have the desired columns
    for (df_name in c("ts_summary", "treatment_rows", "country_rows")) {
      df <- get(df_name)
      for (col in desired_columns) {
        if (!(col %in% names(df))) {
          df[[col]] <- NA
        }
      }
      assign(df_name, df[, desired_columns])
    }

    # Combine all rows
    ts_summary <- rbind(ts_summary, treatment_rows, country_rows)

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

    # Reorder TSPARMCD based on the order in Trial_Summary.xlsx
    ts_template <- openxlsx::read.xlsx(ts_file_path, sheet = "TS")
    tsparmcd_order <- ts_template$TSPARMCD

    ts_summary <- ts_summary %>%
      arrange(factor(TSPARMCD, levels = tsparmcd_order))

    # Remove the original horizontal TRT record
    ts_summary <- ts_summary[!(ts_summary$TSPARMCD == "TRT" & grepl(",", ts_summary$TSVAL)), ]

    # Reassign TSSEQ based on TSPARMCD
    ts_summary <- ts_summary %>%
      group_by(TSPARMCD) %>%
      mutate(TSSEQ = row_number()) %>%
    ungroup()


    if(debug) {
      cat("Final TS summary:\n")
      print(ts_summary)
    }

    output_file <- file.path(output_dir, paste0(study_id, "_TS.xlsx"))
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "TS")
    openxlsx::writeData(wb, "TS", ts_summary)
    openxlsx::setColWidths(wb, "TS", cols = 1:ncol(ts_summary), widths = "auto")

    # Apply text wrapping to all cells
    style <- openxlsx::createStyle(wrapText = TRUE)
    openxlsx::addStyle(wb, "TS", style = style, rows = 1:(nrow(ts_summary) + 1), cols = 1:ncol(ts_summary), gridExpand = TRUE)

    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    if(debug) cat("Output written to:", output_file, "\n")

    return(ts_summary)
  }, error = function(e) {
    cat("Error in create_ts_domain:", conditionMessage(e), "\n")
    print(e)
    return(NULL)
  })
}


#' Process Study Information
#'
#' This function processes the study information JSON to extract additional details.
#'
#' @param study_info A list containing study information from ClinicalTrials.gov API.
#' @param debug Logical, if TRUE, print debug information.
#' @return A processed dataframe with extracted information.
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
#' @return A list containing the TS mapping.
#' @keywords internal
define_ts_mapping <- function() {
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
          phase <- df[[1]]$protocolSection$designModule$phases[[1]]
          phase <- toupper(phase)  # Take the first phase if multiple are present
          
          roman_phases <- c(
            "PHASE 1" = "PHASE I", "PHASE 2" = "PHASE II", "PHASE 3" = "PHASE III", "PHASE 4" = "PHASE IV",
            "PHASE1" = "PHASE I", "PHASE2" = "PHASE II", "PHASE3" = "PHASE III", "PHASE4" = "PHASE IV",
            "PHASE 1/PHASE 2" = "PHASE I/II", "PHASE 2/PHASE 3" = "PHASE II/III",
            "PHASE1/PHASE2" = "PHASE I/II", "PHASE2/PHASE3" = "PHASE II/III",
            "EARLY PHASE 1" = "PHASE Early I", "NOT APPLICABLE" = "N/A"
          )

          if (phase %in% names(roman_phases)) {
            return(roman_phases[phase])
          } else {
            # If no exact match, try to extract numeric part and convert
            numeric_part <- gsub("[^0-9]", "", phase)
            if (numeric_part != "") {
              return(paste0("PHASE ", as.roman(as.integer(numeric_part))))
            } else {
              return(paste0("PHASE ", phase))  # Return original with PHASE prefix if no numeric part found
            }
          }
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
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$designModule$designInfo$allocation)) {
          return(paste(df[[1]]$protocolSection$designModule$designInfo$allocation, collapse = "; "))
        }
        return("NA")
      }, error = function(e) {
        warning("Error in INTTYPE mapping: ", e$message)
        return(NA_character_)
      })
    },
    STYPE = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$designModule$studyType else NA_character_,
    TTYPE = function(df) NA_character_,
    ACTSUB = function(df) {
      tryCatch({
        if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$statusModule$enrollmentInfo$count)) {
          return(as.character(df[[1]]$protocolSection$statusModule$enrollmentInfo$count))
        }
        return("NA")
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
    NCOHORT = function(df) {
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
    NARMS = function(df) if(is.list(df) && length(df) > 0) df[[1]]$protocolSection$designModule$numberOfArms else NA_integer_,
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
          return(df[[1]]$protocolSection$eligibilityModule$sex)
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
            return(unique_countries)
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
    "PHASE 1" = "I", "PHASE 2" = "II", "PHASE 3" = "III", "PHASE 4" = "IV",
    "PHASE 1/PHASE 2" = "I/II", "PHASE 2/PHASE 3" = "II/III",
    "EARLY PHASE 1" = "Early I", "NOT APPLICABLE" = "N/A"
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