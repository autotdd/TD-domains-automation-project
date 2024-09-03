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

    ts_rows <- list()
    current_param <- ""
    seq_counter <- 1

    for (i in 1:nrow(ts_summary)) {
      param <- ts_summary$TSPARMCD[i]
      new_rows <- list()
      
      if (param %in% names(ts_mapping)) {
        if(debug) cat("Mapping", param, "\n")
        mapped_values <- ts_mapping[[param]](trial_data)
        if(debug) cat("Mapped values for", param, ":", toString(mapped_values), "\n")
        
        if (length(mapped_values) > 0 && !all(is.na(mapped_values))) {
          if (param == "FCNTRY") {
            for (value in mapped_values) {
              new_row <- ts_summary[i, ]
              new_row$TSVAL <- value
              new_row$TSSEQ <- seq_counter
              new_rows[[length(new_rows) + 1]] <- new_row
              seq_counter <- seq_counter + 1
            }
          } else {
            new_row <- ts_summary[i, ]
            new_row$TSVAL <- mapped_values[1]  # Take the first value if multiple exist
            new_row$TSSEQ <- seq_counter
            new_rows[[length(new_rows) + 1]] <- new_row
            seq_counter <- seq_counter + 1
          }
        } else {
          new_row <- ts_summary[i, ]
          new_row$TSVAL <- NA_character_
          new_row$TSSEQ <- seq_counter
          new_rows[[length(new_rows) + 1]] <- new_row
          seq_counter <- seq_counter + 1
        }
      } else {
        new_row <- ts_summary[i, ]
        new_row$TSVAL <- NA_character_
        new_row$TSSEQ <- seq_counter
        new_rows[[length(new_rows) + 1]] <- new_row
        seq_counter <- seq_counter + 1
      }
      
      ts_rows <- c(ts_rows, new_rows)
    }

    ts_summary_final <- do.call(rbind, ts_rows)
    ts_summary_final$STUDYID <- study_id
    ts_summary_final$DOMAIN <- "TS"

    ts_summary_final <- ts_summary_final[, !names(ts_summary_final) %in% "FDA.Desired"]
    if(debug) cat("FDA.Desired column removed from ts_summary_final\n")

    if(debug) {
      cat("Final TS summary:\n")
      print(ts_summary_final)
    }

    output_file <- file.path(output_dir, paste0(study_id, "_TS.xlsx"))
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "TS")
    openxlsx::writeData(wb, "TS", ts_summary_final)
    openxlsx::setColWidths(wb, "TS", cols = 1:ncol(ts_summary_final), widths = "auto")

    # Apply text wrapping to all cells
    style <- openxlsx::createStyle(wrapText = TRUE)
    openxlsx::addStyle(wb, "TS", style = style, rows = 1:(nrow(ts_summary_final) + 1), cols = 1:ncol(ts_summary_final), gridExpand = TRUE)

    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    if(debug) cat("Output written to:", output_file, "\n")

    return(ts_summary_final)
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
    STUDYID = function(df) df[[1]]$protocolSection$identificationModule$nctId,
    DOMAIN = function(df) "TS",
    TSSEQ = function(df) seq_len(nrow(df)),
    TSPARMCD = function(df) names(df),
    TSPARM = function(df) names(df),
    TSVAL = function(df) as.character(df[, 1]),
    TSVALNF = function(df) NA,
    TSVALCD = function(df) NA,
    TSVCDREF = function(df) NA,
    TSVCDVER = function(df) NA,
    TITLE = function(df) df[[1]]$protocolSection$identificationModule$officialTitle,
    TPHASE = function(df) {
      tryCatch({
        phase <- df[[1]]$protocolSection$designModule$phases
        if (is.null(phase) || length(phase) == 0) {
          return(NA_character_)
        }
        phase <- toupper(phase[[1]])  # Take the first phase if multiple are present
        
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
      }, error = function(e) {
        warning("Error in TPHASE mapping: ", e$message)
        return(NA_character_)
      })
    },
    INDIC = function(df) df[[1]]$protocolSection$conditionsModule$conditions[[1]],
    TDIGRP = function(df) df[[1]]$protocolSection$conditionsModule$conditions[[1]],
    THERAREA = function(df) {
      cat("Debug: Entering THERAREA function\n")
      cat("Debug: Class of df in THERAREA:", class(df), "\n")
      cat("Debug: Length of df in THERAREA:", length(df), "\n")
      tryCatch({
        if(is.atomic(df)) {
          cat("Debug: df is atomic in THERAREA\n")
          return("Other")
        } else if(is.list(df) && length(df) > 0) {
          condition <- df[[1]]$protocolSection$conditionsModule$conditions[[1]]
          cat("Debug: condition =", condition, "\n")
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
          } else {
            return("Other")
          }
        } else {
          return(NA_character_)
        }
      }, error = function(e) {
        cat("Error in THERAREA mapping:", e$message, "\n")
        return(NA_character_)
      })
    },
    REGID = function(df) {
      cat("Debug: Entering REGID function\n")
      tryCatch({
        secondary_ids <- df[[1]]$protocolSection$identificationModule$secondaryIdInfos
        cat("Debug: Structure of secondary_ids:\n")
        print(str(secondary_ids))
        
        if (is.data.frame(secondary_ids) && nrow(secondary_ids) > 0) {
          eudract_row <- secondary_ids[secondary_ids$type == "EUDRACT_NUMBER", ]
          if (nrow(eudract_row) > 0) {
            result <- eudract_row$id
            cat("Debug: REGID result:", result, "\n")
            return(result)
          }
        }
      }, error = function(e) {
        cat("Error in REGID:", conditionMessage(e), "\n")
      })
      cat("Debug: REGID returning NA\n")
      return("NA")
    },
    INTTYPE = function(df) {
      cat("Debug: Entering INTTYPE function\n")
      tryCatch({
        design_info <- df[[1]]$protocolSection$designModule$designInfo
        cat("Debug: Structure of design_info:\n")
        print(str(design_info))
        
        if (!is.null(design_info) && !is.null(design_info$allocation)) {
          result <- paste(design_info$allocation, collapse = "; ")
          cat("Debug: INTTYPE result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in INTTYPE:", conditionMessage(e), "\n")
      })
      cat("Debug: INTTYPE returning NA\n")
      return("NA")
    },
    STYPE = function(df) df[[1]]$protocolSection$designModule$studyType,
    TTYPE = function(df) {
      cat("Debug: Entering TTYPE function\n")
      cat("Debug: Class of df in TTYPE:", class(df), "\n")
      cat("Debug: Length of df in TTYPE:", length(df), "\n")
      tryCatch({
        if(is.list(df) && length(df) > 0) {
          if("protocolSection" %in% names(df[[1]])) {
            cat("Debug: Found protocolSection\n")
            protocol_section <- df[[1]]$protocolSection
            cat("Debug: Names in protocolSection:", paste(names(protocol_section), collapse = ", "), "\n")
            
            if("outcomesModule" %in% names(protocol_section)) {
              cat("Debug: Found outcomesModule\n")
              outcomes_module <- protocol_section$outcomesModule
              cat("Debug: Names in outcomesModule:", paste(names(outcomes_module), collapse = ", "), "\n")
              
              if("primaryOutcomes" %in% names(outcomes_module)) {
                primary_outcomes <- outcomes_module$primaryOutcomes
                cat("Debug: Found primaryOutcomes. Length:", length(primary_outcomes), "\n")
                
                if(length(primary_outcomes) > 0) {
                  descriptions <- sapply(primary_outcomes, function(x) x$description)
                  types <- character(0)
                  if (any(grepl("safety", tolower(descriptions), fixed = TRUE))) {
                    types <- c(types, "SAFETY")
                  }
                  if (any(grepl("efficacy", tolower(descriptions), fixed = TRUE))) {
                    types <- c(types, "EFFICACY")
                  }
                  if (any(grepl("pharmacokinetics|bioavailability|bioequivalence", tolower(descriptions), fixed = TRUE))) {
                    types <- c(types, "PK")
                  }
                  if (length(types) == 0) {
                    types <- "OTHER"
                  }
                  result <- paste(types, collapse = "; ")
                  cat("Debug: TTYPE result:", result, "\n")
                  return(result)
                }
              }
            }
          }
        }
        cat("Debug: Returning OTHER for TTYPE\n")
        return("OTHER")
      }, error = function(e) {
        cat("Error in TTYPE mapping:", e$message, "\n")
        return(NA_character_) 
      })
    },
    ACTSUB = function(df) {
      tryCatch({
        enrollment_info <- df[[1]]$protocolSection$designModule$enrollmentInfo
        if (!is.null(enrollment_info) && !is.null(enrollment_info$count)) {
          return(as.character(enrollment_info$count))
        }
      }, error = function(e) {
        warning("Error in ACTSUB mapping: ", e$message)
      })
      return(NA_character_)
    },
    PLANSUB = function(df) {
      cat("Debug: Entering PLANSUB function\n")
      tryCatch({
        design_info <- df[[1]]$protocolSection$designModule
        cat("Debug: Structure of design_info:\n")
        print(str(design_info))
        
        if (!is.null(design_info) && !is.null(design_info$enrollmentInfo) && !is.null(design_info$enrollmentInfo$count)) {
          result <- as.character(design_info$enrollmentInfo$count)
          cat("Debug: PLANSUB result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in PLANSUB:", conditionMessage(e), "\n")
      })
      cat("Debug: PLANSUB returning NA\n")
      return("NA")
    },
    SENDTC = function(df) {
      tryCatch({
        completion_date <- df[[1]]$protocolSection$statusModule$completionDateStruct$date
        if (!is.null(completion_date) && completion_date != "") {
          return(format_date_iso8601(completion_date))
        }
      }, error = function(e) {
        warning("Error in SENDTC mapping: ", e$message)
      })
      return(NA_character_)
    },
    SSTDTC = function(df) {
      tryCatch({
        start_date <- df[[1]]$protocolSection$statusModule$startDateStruct$date
        if (!is.null(start_date) && start_date != "") {
          return(format_date_iso8601(start_date))
        }
      }, error = function(e) {
        warning("Error in SSTDTC mapping: ", e$message)
      })
      return(NA_character_)
    },
    LENGTH = function(df) {
      tryCatch({
        start_date <- df[[1]]$protocolSection$statusModule$startDateStruct$date
        completion_date <- df[[1]]$protocolSection$statusModule$completionDateStruct$date
        if (!is.null(start_date) && start_date != "" && !is.null(completion_date) && completion_date != "") {
          start <- as.Date(format_date_iso8601(start_date))
          end <- as.Date(format_date_iso8601(completion_date))
          duration <- as.numeric(end - start)
          return(paste0("P", duration, "D"))  # ISO8601 duration format
        }
      }, error = function(e) {
        warning("Error in LENGTH mapping: ", e$message)
      })
      return(NA_character_)
    },
    ADAPT = function(df) derive_adaptive_design(df),
    ADDON = function(df) derive_addon_treatment(df),
    AGEMIN = function(df) {
      tryCatch({
        if(is.atomic(df)) {
          age_min <- df
        } else if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule)) {
          age_min <- df[[1]]$protocolSection$eligibilityModule$minimumAge
        } else {
          return(NA_character_)
        }
        
        if(is.null(age_min) || is.na(age_min) || age_min == "") return(NA_character_)
        
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
      }, error = function(e) {
        warning("Error in AGEMIN mapping: ", e$message)
        return(NA_character_)
      })
    },
    AGEMAX = function(df) {
      tryCatch({
        if(is.atomic(df)) {
          age_max <- df
        } else if(is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$eligibilityModule)) {
          age_max <- df[[1]]$protocolSection$eligibilityModule$maximumAge
        } else {
          return(NA_character_)
        }
        
        if(is.null(age_max) || is.na(age_max) || age_max == "") return(NA_character_)
        
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
      }, error = function(e) {
        warning("Error in AGEMAX mapping: ", e$message)
        return(NA_character_)
      })
    },
    COMPTRT = function(df) {
      cat("Debug: Entering COMPTRT function\n")
      tryCatch({
        interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
        cat("Debug: Structure of interventions:\n")
        print(str(interventions))
        
        if (is.data.frame(interventions) && nrow(interventions) > 0) {
          # Filter for comparative treatments (usually standard of care drugs)
          comp_treatments <- interventions[grepl("carboplatin|cisplatin|pemetrexed", interventions$name, ignore.case = TRUE), ]
          if (nrow(comp_treatments) > 0) {
            result <- paste(comp_treatments$name, collapse = "; ")
            cat("Debug: COMPTRT result:", result, "\n")
            return(result)
          }
        }
      }, error = function(e) {
        cat("Error in COMPTRT:", conditionMessage(e), "\n")
      })
      cat("Debug: COMPTRT returning NA\n")
      return("NA")
    },
    CURTRT = function(df) {
      cat("Debug: Entering CURTRT function\n")
      tryCatch({
        interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
        cat("Debug: Structure of interventions:\n")
        print(str(interventions))
        
        if (is.data.frame(interventions) && nrow(interventions) > 0) {
          # Filter for current treatments (usually experimental drugs)
          cur_treatments <- interventions[grepl("divarasib|pembrolizumab", interventions$name, ignore.case = TRUE), ]
          if (nrow(cur_treatments) > 0) {
            result <- paste(cur_treatments$name, collapse = "; ")
            cat("Debug: CURTRT result:", result, "\n")
            return(result)
          }
        }
      }, error = function(e) {
        cat("Error in CURTRT:", conditionMessage(e), "\n")
      })
      cat("Debug: CURTRT returning NA\n")
      return("NA")
    },
    NCOHORT = function(df) {
      cat("Debug: Entering NCOHORT function\n")
      tryCatch({
        interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
        cat("Debug: Structure of interventions:\n")
        print(str(interventions))
        
        if (is.data.frame(interventions) && nrow(interventions) > 0 && "armGroupLabels" %in% names(interventions)) {
          all_labels <- unlist(interventions$armGroupLabels)
          unique_cohorts <- unique(all_labels)
          result <- length(unique_cohorts)
          cat("Debug: NCOHORT result:", result, "\n")
          return(as.character(result))
        }
      }, error = function(e) {
        cat("Error in NCOHORT:", conditionMessage(e), "\n")
      })
      cat("Debug: NCOHORT returning NA\n")
      return("NA")
    },
    DCUTDESC = function(df) derive_data_cutoff_description(df),
    DCUTDTC = function(df) format_date_iso8601(df[[1]]$protocolSection$statusModule$primaryCompletionDateStruct$date),
    EGBLIND = function(df) df[[1]]$protocolSection$designModule$masking,
    INTMODEL = function(df) {
      cat("Debug: Entering INTMODEL function\n")
      tryCatch({
        design_info <- df[[1]]$protocolSection$designModule$designInfo
        cat("Debug: Structure of design_info:\n")
        print(str(design_info))
        
        if (!is.null(design_info) && !is.null(design_info$interventionModel)) {
          result <- paste(design_info$interventionModel, collapse = "; ")
          cat("Debug: INTMODEL result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in INTMODEL:", conditionMessage(e), "\n")
      })
      cat("Debug: INTMODEL returning NA\n")
      return("NA")
    },
    NARMS = function(df) df[[1]]$protocolSection$designModule$numberOfArms,
    ONGOSIND = function(df) ifelse(df[[1]]$protocolSection$statusModule$overallStatus %in% c("Recruiting", "Active, not recruiting"), "Y", "N"),
    OUTMSPRI = function(df) {
      cat("Debug: Entering OUTMSPRI function\n")
      tryCatch({
        primary_outcomes <- df[[1]]$protocolSection$outcomesModule$primaryOutcomes
        cat("Debug: Structure of primary_outcomes:\n")
        print(str(primary_outcomes))
        
        if (is.data.frame(primary_outcomes) && nrow(primary_outcomes) > 0) {
          measures <- primary_outcomes$measure
          cat("Debug: OUTMSPRI result:\n", paste(measures, collapse = "\n"), "\n")
          return(na.omit(measures))
        }
      }, error = function(e) {
        cat("Error in OUTMSPRI:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSPRI returning NA\n")
      return(NA_character_)
    },

    OUTMSSEC = function(df) {
      cat("Debug: Entering OUTMSSEC function\n")
      tryCatch({
        secondary_outcomes <- df[[1]]$protocolSection$outcomesModule$secondaryOutcomes
        cat("Debug: Structure of secondary_outcomes:\n")
        print(str(secondary_outcomes))
        
        if (is.data.frame(secondary_outcomes) && nrow(secondary_outcomes) > 0) {
          measures <- secondary_outcomes$measure
          cat("Debug: OUTMSSEC result:\n", paste(measures, collapse = "\n"), "\n")
          return(na.omit(measures))
        }
      }, error = function(e) {
        cat("Error in OUTMSSEC:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSSEC returning NA\n")
      return(NA_character_)
    },

    OUTMSEXP = function(df) {
      cat("Debug: Entering OUTMSEXP function\n")
      tryCatch({
        exploratory_outcomes <- df[[1]]$protocolSection$outcomesModule$otherOutcomes
        cat("Debug: Structure of exploratory_outcomes:\n")
        print(str(exploratory_outcomes))
        
        if (is.data.frame(exploratory_outcomes) && nrow(exploratory_outcomes) > 0) {
          measures <- exploratory_outcomes$measure
          cat("Debug: OUTMSEXP result:\n", paste(measures, collapse = "\n"), "\n")
          return(na.omit(measures))
        }
      }, error = function(e) {
        cat("Error in OUTMSEXP:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSEXP returning NA\n")
      return(NA_character_)
    },
    SEXPOP = function(df) {
      cat("Debug: Entering SEXPOP function\n")
      tryCatch({
        eligibility <- df[[1]]$protocolSection$eligibilityModule
        cat("Debug: Structure of eligibility:\n")
        print(str(eligibility))
        
        if (!is.null(eligibility) && !is.null(eligibility$sex)) {
          result <- eligibility$sex
          cat("Debug: SEXPOP result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in SEXPOP:", conditionMessage(e), "\n")
      })
      cat("Debug: SEXPOP returning NA\n")
      return("NA")
    },
    SPONSOR = function(df) {
      cat("Debug: Entering SPONSOR function\n")
      tryCatch({
        sponsor_info <- df[[1]]$protocolSection$identificationModule$organization
        cat("Debug: Structure of sponsor_info:\n")
        print(str(sponsor_info))
        
        if (!is.null(sponsor_info) && !is.null(sponsor_info$fullName)) {
          result <- sponsor_info$fullName
          cat("Debug: SPONSOR result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in SPONSOR:", conditionMessage(e), "\n")
      })
      cat("Debug: SPONSOR returning NA\n")
      return("NA")
    },
    FCNTRY = function(df) {
      tryCatch({
        if (is.list(df) && length(df) > 0 && !is.null(df[[1]]$protocolSection$contactsLocationsModule$locations)) {
          locations <- df[[1]]$protocolSection$contactsLocationsModule$locations
          if (is.data.frame(locations) && "country" %in% names(locations)) {
            unique_countries <- unique(locations$country)
            unique_countries <- unique_countries[!is.na(unique_countries) & unique_countries != ""]
            if (length(unique_countries) > 0) {
              return(unique_countries)
            }
          }
        }
        return(NA_character_)
      }, error = function(e) {
        warning("Error in FCNTRY mapping: ", conditionMessage(e))
        return(NA_character_)
      })
    },
    HLTSUBJI = function(df) derive_healthy_subjects(df),
    EXTTIND = function(df) derive_extension_trial(df),
    PDSTIND = function(df) {
      min_age <- df[[1]]$protocolSection$eligibilityModule$minimumAge
      if (!is.null(min_age)) {
        min_age <- as.numeric(gsub("[^0-9.]", "", min_age))
        if (!is.na(min_age) && min_age < 18) {
          return("Y")
        } else {
          return("N")
        }
      }
      return(NA)
    },
    SPREFID = function(df) {
      tryCatch({
        if(!is.list(df) || length(df) == 0) return(NA_character_)
        
        result <- df[[1]]$protocolSection$identificationModule$nctId[1]  # In case there are multiple, take the first one
        return(result)
      }, error = function(e) {
        warning("Error in SPREFID mapping: ", e$message)
        return(NA_character_)
      })
    },
    TRT = function(df) {
      cat("Debug: Entering TRT function\n")
      tryCatch({
        interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
        cat("Debug: Structure of interventions:\n")
        print(str(interventions))
        
        if (is.data.frame(interventions) && nrow(interventions) > 0) {
          treatments <- interventions$name
          result <- paste(unique(treatments), collapse = ", ")
          cat("Debug: TRT result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in TRT:", conditionMessage(e), "\n")
      })
      cat("Debug: TRT returning NA\n")
      return("NA")
    },
    TBLIND = function(df) {
      cat("Debug: Entering TBLIND function\n")
      tryCatch({
        design_info <- df[[1]]$protocolSection$designModule$designInfo
        cat("Debug: Structure of design_info:\n")
        print(str(design_info))
        
        if (!is.null(design_info) && !is.null(design_info$maskingInfo)) {
          masking <- design_info$maskingInfo$masking
          if (!is.null(masking) && length(masking) > 0) {
            result <- paste(masking, collapse = "; ")
            cat("Debug: TBLIND result:", result, "\n")
            return(result)
          }
        }
        # If no masking info is found, return "OPEN LABEL"
        cat("Debug: TBLIND returning OPEN LABEL\n")
        return("OPEN LABEL")
      }, error = function(e) {
        cat("Error in TBLIND:", conditionMessage(e), "\n")
        return("OPEN LABEL")
      })
    },
    TCNTRL = function(df) {
      cat("Debug: Entering TCNTRL function\n")
      tryCatch({
        arms <- df[[1]]$protocolSection$armsInterventionsModule$armGroups
        cat("Debug: Structure of arms:\n")
        print(str(arms))
        
        if (is.data.frame(arms) && nrow(arms) > 0) {
          # Check for placebo control
          if (any(grepl("placebo", tolower(arms$type)))) {
            return("PLACEBO")
          }
          # Check for active control
          if (any(grepl("active comparator", tolower(arms$type)))) {
            return("ACTIVE")
          }
          # Check for dose comparison
          if (length(unique(arms$type)) == 1 && unique(arms$type) == "EXPERIMENTAL") {
            return("DOSE COMPARISON")
          }
          # If none of the above, assume it's uncontrolled
          return("NONE")
        }
      }, error = function(e) {
        cat("Error in TCNTRL:", conditionMessage(e), "\n")
      })
      cat("Debug: TCNTRL returning NA\n")
      return("NA")
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
derive_therapeutic_area <- function(condition, interventions) {
  tryCatch({
    if(is.atomic(df)) {
      # If df is an atomic vector, it might be the condition itself
      condition <- df
      interventions <- NULL
    } else if(is.list(df) && length(df) > 0) {
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
    if(is.atomic(df)) {
      # If df is an atomic vector, we can't determine the type
      return("OTHER")
    } else if(!is.list(df) || length(df) == 0) {
      return(NA_character_)
    }
    
    types <- character(0)
    primary_outcomes <- df[[1]]$protocolSection$outcomesModule$primaryOutcomes
    if (!is.null(primary_outcomes) && length(primary_outcomes) > 0) {
      descriptions <- if(is.list(primary_outcomes)) {
        sapply(primary_outcomes, function(x) x$description)
      } else if(is.character(primary_outcomes)) {
        primary_outcomes
      } else {
        NA_character_
      }
      
      if (any(grepl("safety", tolower(descriptions), fixed = TRUE))) {
        types <- c(types, "SAFETY")
      }
      if (any(grepl("efficacy", tolower(descriptions), fixed = TRUE))) {
        types <- c(types, "EFFICACY")
      }
      if (any(grepl("pharmacokinetics|bioavailability|bioequivalence", tolower(descriptions), fixed = TRUE))) {
        types <- c(types, "PK")
      }
    }
    if (length(types) == 0) {
      types <- "OTHER"
    }
    return(paste(types, collapse = "; "))
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
  if ((!is.null(df[[1]]$protocolSection$identificationModule$briefTitle) && !is.na(df[[1]]$protocolSection$identificationModule$briefTitle) && grepl("adaptive", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE)) ||
      (!is.null(df[[1]]$protocolSection$identificationModule$officialTitle) && !is.na(df[[1]]$protocolSection$identificationModule$officialTitle) && grepl("adaptive", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE))) {
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
  if ((!is.null(df[[1]]$protocolSection$identificationModule$briefTitle) && !is.na(df[[1]]$protocolSection$identificationModule$briefTitle) && grepl("add-on|addon|adjunct", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE)) ||
      (!is.null(df[[1]]$protocolSection$identificationModule$officialTitle) && !is.na(df[[1]]$protocolSection$identificationModule$officialTitle) && grepl("add-on|addon|adjunct", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE))) {
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
  if (!is.null(df[[1]]$protocolSection$statusModule$overallStatus) && !is.na(df[[1]]$protocolSection$statusModule$overallStatus) && df[[1]]$protocolSection$statusModule$overallStatus %in% c("Completed", "Terminated")) {
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
  if (!is.null(df[[1]]$protocolSection$statusModule$overallStatus) && !is.na(df[[1]]$protocolSection$statusModule$overallStatus) && df[[1]]$protocolSection$statusModule$overallStatus %in% c("Completed", "Terminated")) {
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
  if (grepl("extension|follow-up|followup", tolower(df[[1]]$protocolSection$identificationModule$briefTitle), fixed = TRUE) ||
      grepl("extension|follow-up|followup", tolower(df[[1]]$protocolSection$identificationModule$officialTitle), fixed = TRUE)) {
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
  if (!is.null(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers) && !is.na(df[[1]]$protocolSection$eligibilityModule$healthyVolunteers)) {
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
  min_age <- df[[1]]$protocolSection$eligibilityModule$minimumAge
  if (!is.null(min_age)) {
    min_age <- as.numeric(gsub("[^0-9.]", "", min_age))
    if (!is.na(min_age) && min_age < 18) {
      return("Y")
    } else {
      return("N")
    }
  }
  return(NA)
}