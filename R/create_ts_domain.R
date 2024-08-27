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
  # Ensure debug is logical
  debug <- as.logical(debug)

  if(debug) cat("Starting create_ts_domain function\n")

  # Get the path to the Trial_Summary.xlsx file
  ts_file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")

  if(debug) cat("Looking for Trial_Summary.xlsx at:", ts_file_path, "\n")

  if (ts_file_path == "") {
    stop("Trial_Summary.xlsx not found in package. Please ensure the file is present in inst/extdata directory.")
  } else {
    tryCatch({
      ts_summary <- openxlsx::read.xlsx(ts_file_path, sheet = "TS")
      if(debug) cat("Successfully read Trial_Summary.xlsx\n")
    }, error = function(e) {
      stop(paste("Error reading Trial_Summary.xlsx:", e$message))
    })
  }

  if(debug) cat("TS summary structure created\n")

  # Use the local fetch_study_info function
  local_fetch_study_info <- fetch_study_info_v2

  # Fetch and process trial data for each NCT ID
  trial_data <- lapply(nct_ids, function(nct_id) {
    if(debug) cat("Processing NCT ID:", nct_id, "\n")
    tryCatch({
      study_info <- local_fetch_study_info(nct_ids = nct_id, debug = debug)
      if(debug) cat("Study info fetched for NCT ID:", nct_id, "\n")
      processed_info <- process_study_info(study_info, debug = debug)
      if(debug) {
        cat("Processed study info for NCT ID:", nct_id, "\n")
        print(processed_info)
      }
      return(processed_info)
    }, error = function(e) {
      warning(paste("Error processing NCT ID:", nct_id, "-", e$message))
      if(debug) {
        cat("Error details:\n")
        print(e)
      }
      return(NULL)
    })
  })

  # Remove any NULL entries (failed fetches)
  trial_data <- trial_data[!sapply(trial_data, is.null)]

  if (length(trial_data) == 0) {
    stop("No valid data could be retrieved for the provided NCT IDs.")
  }

  if(debug) cat("Trial data processed\n")

  # Combine all fetched trial data into a single dataframe
  trial_df <- do.call(rbind, trial_data)

  if(debug) {
    cat("Combined trial data structure:\n")
    print(str(trial_df))
  }

  # Define the mapping for TS domain
  ts_mapping <- define_ts_mapping()

  # Initialize an empty list to store TS rows
  ts_rows <- list()

  # Just before applying the mapping:
  cat("Debug: Structure of trial_data:\n")
  print(str(trial_data, max.level = 3))
  cat("Debug: Class of trial_data:", class(trial_data), "\n")
  cat("Debug: Length of trial_data:", length(trial_data), "\n")
  if(is.list(trial_data) && length(trial_data) > 0) {
    cat("Debug: Class of first element of trial_data:", class(trial_data[[1]]), "\n")
  }

  # Loop through each row in ts_summary and apply mapping
  for (i in 1:nrow(ts_summary)) {
    param <- ts_summary$TSPARMCD[i]
    if (param %in% names(ts_mapping)) {
      cat("Debug: Mapping", param, "\n")
      cat("Debug: Class of trial_data passed to mapping function:", class(trial_data), "\n")
      mapped_value <- tryCatch({
        result <- ts_mapping[[param]](trial_data)
        cat("Debug: Mapped value for", param, ":", toString(result), "\n")
        result
      }, error = function(e) {
        warning(paste("Error mapping", param, ":", e$message))
        cat("Error details for", param, ":\n")
        print(e)
        return(NA_character_)
      })
      
      if (param %in% c("OBJPRIM", "OBJSEC", "OUTMSPRI", "OUTMSSEC", "FCNTRY")) {
        # Handle multiple values
        if (length(mapped_value) > 1) {
          for (value in mapped_value) {
            new_row <- ts_summary[i, ]
            new_row$TSVAL <- value
            ts_rows[[length(ts_rows) + 1]] <- new_row
          }
        } else {
          new_row <- ts_summary[i, ]
          new_row$TSVAL <- if (length(mapped_value) == 1) mapped_value else NA_character_
          ts_rows[[length(ts_rows) + 1]] <- new_row
        }
      } else {
        # Handle other parameters
        new_row <- ts_summary[i, ]
        new_row$TSVAL <- if (length(mapped_value) == 1) mapped_value else NA_character_
        ts_rows[[length(ts_rows) + 1]] <- new_row
      }
    } else {
      new_row <- ts_summary[i, ]
      new_row$TSVAL <- NA_character_
      ts_rows[[length(ts_rows) + 1]] <- new_row
    }
  }

  # Combine all rows into a single dataframe
  ts_summary_final <- do.call(rbind, ts_rows)

  # Fill in the STUDYID and DOMAIN columns
  ts_summary_final$STUDYID <- study_id
  ts_summary_final$DOMAIN <- "TS"

  # Clean TSVAL values
  ts_summary_final$TSVAL <- sapply(ts_summary_final$TSVAL, clean_tsval)

  if(debug) {
    cat("Final TS summary:\n")
    print(ts_summary_final)
  }

  # Write the output to an Excel file
  output_file <- file.path(output_dir, paste0(study_id, "_TS.xlsx"))
  openxlsx::write.xlsx(ts_summary_final, output_file)

  if(debug) cat("Output written to:", output_file, "\n")

  return(ts_summary_final)
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
  if (is.na(date) || is.null(date)) return(NA)
  tryCatch({
    formatted_date <- as.Date(date)
    return(format(formatted_date, "%Y-%m-%d"))
  }, error = function(e) {
    return(date)  # Return original if conversion fails
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
    TITLE = function(df) df[[1]]$protocolSection$identificationModule$briefTitle,
    TPHASE = function(df) {
      phase <- df[[1]]$protocolSection$identificationModule$phase
      if (is.null(phase) || is.na(phase) || length(phase) == 0) {
        return(NA)
      }
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
        if(is.atomic(df)) {
          cat("Debug: df is atomic in TTYPE\n")
          return("OTHER")
        } else if(is.list(df) && length(df) > 0) {
          types <- character(0)
          primary_outcomes <- df[[1]]$protocolSection$outcomesModule$primaryOutcomes
          if (!is.null(primary_outcomes) && length(primary_outcomes) > 0) {
            descriptions <- sapply(primary_outcomes, function(x) x$description)
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
        } else {
          return(NA_character_)
        }
      }, error = function(e) {
        cat("Error in TTYPE mapping:", e$message, "\n")
        return(NA_character_)
      })
    },
    ACTSUB = function(df) {
      cat("Debug: Entering ACTSUB function\n")
      tryCatch({
        enrollment_info <- df[[1]]$protocolSection$statusModule$enrollmentInfo
        cat("Debug: Structure of enrollment_info:\n")
        print(str(enrollment_info))
        
        if (!is.null(enrollment_info) && !is.null(enrollment_info$count)) {
          result <- as.character(enrollment_info$count)
          cat("Debug: ACTSUB result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in ACTSUB:", conditionMessage(e), "\n")
      })
      cat("Debug: ACTSUB returning NA\n")
      return("NA")
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
    SENDTC = function(df) format_date_iso8601(df[[1]]$protocolSection$statusModule$completionDateStruct$date),
    SSTDTC = function(df) format_date_iso8601(df[[1]]$protocolSection$statusModule$startDateStruct$date),
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
        
        if (is.data.frame(primary_outcomes) && nrow(primary_outcomes) > 0 && "measure" %in% names(primary_outcomes)) {
          result <- na.omit(primary_outcomes$measure)
          cat("Debug: OUTMSPRI result:", paste(result, collapse = "; "), "\n")
          return(if(length(result) > 0) result else "NA")
        }
      }, error = function(e) {
        cat("Error in OUTMSPRI:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSPRI returning NA\n")
      return("NA")
    },

    OUTMSSEC = function(df) {
      cat("Debug: Entering OUTMSSEC function\n")
      tryCatch({
        secondary_outcomes <- df[[1]]$protocolSection$outcomesModule$secondaryOutcomes
        cat("Debug: Structure of secondary_outcomes:\n")
        print(str(secondary_outcomes))
        
        if (is.data.frame(secondary_outcomes) && nrow(secondary_outcomes) > 0 && "measure" %in% names(secondary_outcomes)) {
          result <- na.omit(secondary_outcomes$measure)
          cat("Debug: OUTMSSEC result:", paste(result, collapse = "; "), "\n")
          return(if(length(result) > 0) result else "NA")
        }
      }, error = function(e) {
        cat("Error in OUTMSSEC:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSSEC returning NA\n")
      return("NA")
    },

    OUTMSEXP = function(df) {
      cat("Debug: Entering OUTMSEXP function\n")
      tryCatch({
        outcomes_module <- df[[1]]$protocolSection$outcomesModule
        cat("Debug: Structure of outcomes_module:\n")
        print(str(outcomes_module))
        
        # Check for otherOutcomes or exploratoryOutcomes
        if (!is.null(outcomes_module$otherOutcomes)) {
          exploratory_outcomes <- outcomes_module$otherOutcomes
        } else if (!is.null(outcomes_module$exploratoryOutcomes)) {
          exploratory_outcomes <- outcomes_module$exploratoryOutcomes
        } else {
          exploratory_outcomes <- NULL
        }
        
        if (!is.null(exploratory_outcomes)) {
          cat("Debug: Structure of exploratory_outcomes:\n")
          print(str(exploratory_outcomes))
          
          if (is.data.frame(exploratory_outcomes)) {
            # If it's a data frame, extract measures
            measures <- exploratory_outcomes$measure
          } else if (is.list(exploratory_outcomes)) {
            # If it's a list, extract measures from each element
            measures <- sapply(exploratory_outcomes, function(x) x$measure)
          } else {
            measures <- exploratory_outcomes
          }
          
          # Remove any NA values and collapse into a single string
          result <- paste(na.omit(measures), collapse = "; ")
          cat("Debug: OUTMSEXP result:", result, "\n")
          return(result)
        }
      }, error = function(e) {
        cat("Error in OUTMSEXP:", conditionMessage(e), "\n")
      })
      cat("Debug: OUTMSEXP returning NA\n")
      return("NA")
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
      cat("Debug: Entering FCNTRY function\n")
      tryCatch({
        locations_module <- df[[1]]$protocolSection$contactsLocationsModule
        cat("Debug: Structure of locations_module:\n")
        print(str(locations_module))
        
        if (!is.null(locations_module$locations)) {
          locations <- locations_module$locations
          if (is.data.frame(locations) && "country" %in% names(locations)) {
            countries <- unique(na.omit(locations$country))
            cat("Debug: FCNTRY results:\n")
            print(countries)
            return(countries)
          }
        }
      }, error = function(e) {
        cat("Error in FCNTRY:", conditionMessage(e), "\n")
      })
      cat("Debug: FCNTRY returning NA\n")
      return("NA")
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
        nct_id <- df[[1]]$protocolSection$identificationModule$nctId
        if (is.null(nct_id) || length(nct_id) == 0) return(NA_character_)
        
        result <- nct_id[1]  # In case there are multiple, take the first one
        return(result)
      }, error = function(e) {
        warning("Error in SPREFID mapping: ", e$message)
        return(NA_character_)
      })
    },
    TRT = function(df) {
      cat("Debug: Entering TRT function\n")
      cat("Debug: Class of df in TRT:", class(df), "\n")
      cat("Debug: Length of df in TRT:", length(df), "\n")
      tryCatch({
        if(is.atomic(df)) {
          cat("Debug: df is atomic in TRT\n")
          return(as.character(df))
        } else if(is.list(df) && length(df) > 0) {
          interventions <- df[[1]]$protocolSection$armsInterventionsModule$interventions
          if (is.null(interventions) || length(interventions) == 0) return(NA_character_)
          treatments <- sapply(interventions, function(x) x$description)
          return(if(all(is.na(treatments))) NA_character_ else paste(treatments, collapse = "; "))
        } else {
          return(NA_character_)
        }
      }, error = function(e) {
        cat("Error in TRT mapping:", e$message, "\n")
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
  if (!is.na(date) && !is.null(date)) {
    return(as.character(as.Date(date, format="%Y-%m-%d")))
  } else {
    return(NA)
  }
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