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


create_ts_domain <- function(nct_ids, study_id, output_dir = getwd(), debug = FALSE) {
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
      warning(paste("Error processing NCT ID:", nct_id, "- ", e$message))
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

  # Map the fetched data to TSVAL based on TSPARM labels
  ts_rows <- list()
  for (i in 1:nrow(ts_summary)) {
    param <- ts_summary$TSPARMCD[i]
    if (param %in% names(ts_mapping)) {
      mapped_value <- tryCatch({
        if(debug) cat("Mapping", param, "\n")
        result <- ts_mapping[[param]](trial_df)
        if(debug) cat("Mapped value for", param, ":", toString(result), "\n")
        result
      }, error = function(e) {
        warning(paste("Error mapping", param, ":", e$message))
        if(debug) {
          cat("Error details for", param, ":\n")
          print(e)
        }
        return(NA)
      })

      if (param %in% c("OUTMSPRI", "OUTMSSEC", "OUTMSEXP")) {
        # Handle multiple outcome measures
        if (length(mapped_value) > 1) {
          for (value in mapped_value) {
            new_row <- ts_summary[i, ]
            new_row$TSVAL <- value
            ts_rows[[length(ts_rows) + 1]] <- new_row
          }
        } else {
          new_row <- ts_summary[i, ]
          new_row$TSVAL <- if (length(mapped_value) == 1) mapped_value else NA
          ts_rows[[length(ts_rows) + 1]] <- new_row
        }
      } else {
        # Handle other parameters
        new_row <- ts_summary[i, ]
        if (!is.null(mapped_value) && !all(is.na(mapped_value))) {
          new_row$TSVAL <- paste(unique(na.omit(mapped_value)), collapse = "; ")
        } else {
          new_row$TSVAL <- NA
        }
        ts_rows[[length(ts_rows) + 1]] <- new_row
      }
    } else {
      new_row <- ts_summary[i, ]
      new_row$TSVAL <- NA
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
    return(data.frame())
  }

  # Try to find the ProtocolSection
  protocol <- study_info[[1]]$protocolSection

  if(is.null(protocol)) {
    if(debug) {
      cat("Error: ProtocolSection not found in study_info\n")
      cat("study_info structure:\n")
      print(str(study_info, max.level = 3))
    }
    return(data.frame())
  }

  safe_extract <- function(data, path, default = NA) {
    if(debug) cat("Extracting:", paste(path, collapse = "$"), "\n")
    result <- tryCatch({
      value <- data
      for(p in path) {
        if(is.null(value[[p]])) {
          if(debug) cat("Path element not found:", p, "\n")
          return(default)
        }
        value <- value[[p]]
      }
      value
    }, error = function(e) {
      if(debug) cat("Error extracting:", paste(path, collapse = "$"), "-", e$message, "\n")
      default
    })
    if (length(result) == 0) default else result
  }

  safe_extract_locations <- function(data) {
    if(debug) cat("Extracting locations\n")
    locations <- safe_extract(data, c("contactsLocationsModule", "locationList"))
    if (is.null(locations) || length(locations) == 0) {
      if(debug) cat("No locations found\n")
      return(NA)
    }

    location_strings <- sapply(locations, function(loc) {
      facility <- safe_extract(loc, "facility")
      city <- safe_extract(loc, "city")
      country <- safe_extract(loc, "country")
      if(debug) cat("Location found:", paste(facility, city, country, sep = ", "), "\n")
      paste(facility, city, country, sep = ", ")
    })

    result <- paste(location_strings, collapse = "; ")
    if(debug) cat("Final locations string:", result, "\n")
    return(result)
  }

  safe_extract_outcomes <- function(data, outcome_type) {
    outcomes <- safe_extract(data, c("outcomesModule", paste0(outcome_type, "OutcomeList"), paste0(outcome_type, "Outcome")))
    if (is.null(outcomes) || length(outcomes) == 0) {
      if(debug) cat("No", outcome_type, "outcomes found\n")
      return(NA)
    }

    outcome_measures <- sapply(outcomes, function(outcome) {
      measure <- safe_extract(outcome, "measure")
      if(debug) cat(outcome_type, "outcome measure found:", measure, "\n")
      return(measure)
    })

    return(outcome_measures)
  }

  convert_age_to_iso8601 <- function(age) {
    if (is.na(age) || is.null(age)) return(NA)

    age_value <- as.numeric(gsub("[^0-9.]", "", age))
    if (grepl("year", tolower(age))) {
      return(paste0("P", age_value, "Y"))
    } else if (grepl("month", tolower(age))) {
      return(paste0("P", age_value, "M"))
    } else if (grepl("week", tolower(age))) {
      return(paste0("P", age_value, "W"))
    } else if (grepl("day", tolower(age))) {
      return(paste0("P", age_value, "D"))
    } else {
      return(age)  # Return original if format is not recognized
    }
  }

  result <- data.frame(
    nctId = safe_extract(protocol, c("identificationModule", "nctId")),
    orgStudyId = safe_extract(protocol, c("identificationModule", "orgStudyIdInfo", "id")),
    briefTitle = safe_extract(protocol, c("identificationModule", "briefTitle")),
    officialTitle = safe_extract(protocol, c("identificationModule", "officialTitle")),
    overallStatus = safe_extract(protocol, c("statusModule", "overallStatus")),
    startDate = safe_extract(protocol, c("statusModule", "startDateStruct", "date")),
    primaryCompletionDate = safe_extract(protocol, c("statusModule", "primaryCompletionDateStruct", "date")),
    completionDate = safe_extract(protocol, c("statusModule", "completionDateStruct", "date")),
    phase = safe_extract(protocol, c("designModule", "phaseList", "phase")),
    enrollment = safe_extract(protocol, c("designModule", "enrollmentInfo", "count")),
    enrollmentType = safe_extract(protocol, c("designModule", "enrollmentInfo", "type")),
    condition = paste(safe_extract(protocol, c("conditionsModule", "conditionList", "condition"), character(0)), collapse = "; "),
    interventionModel = safe_extract(protocol, c("designModule", "designInfo", "interventionModel")),
    interventionType = paste(sapply(safe_extract(protocol, c("armsInterventionsModule", "interventionList", "intervention"), list()), function(x) safe_extract(x, "type")), collapse = "; "),
    numberOfArms = length(safe_extract(protocol, c("armsInterventionsModule", "armGroupList", "armGroup"), list())),
    masking = safe_extract(protocol, c("designModule", "designInfo", "maskingInfo", "masking")),
    allocation = safe_extract(protocol, c("designModule", "designInfo", "allocation")),
    ageMin = convert_age_to_iso8601(safe_extract(protocol, c("eligibilityModule", "minimumAge"))),
    ageMax = convert_age_to_iso8601(safe_extract(protocol, c("eligibilityModule", "maximumAge"))),
    gender = safe_extract(protocol, c("eligibilityModule", "sex")),
    healthyVolunteers = safe_extract(protocol, c("eligibilityModule", "healthyVolunteers")),
    sponsor = safe_extract(protocol, c("sponsorCollaboratorsModule", "leadSponsor", "name")),
    collaborators = paste(sapply(safe_extract(protocol, c("sponsorCollaboratorsModule", "collaboratorList", "collaborator"), list()), function(x) safe_extract(x, "name")), collapse = "; "),
    locations = safe_extract_locations(protocol),
    primaryOutcomeMeasures = I(list(safe_extract_outcomes(protocol, "primary"))),
    secondaryOutcomeMeasures = I(list(safe_extract_outcomes(protocol, "secondary"))),
    otherOutcomeMeasures = I(list(safe_extract_outcomes(protocol, "other"))),
    armGroupLabels = paste(sapply(safe_extract(protocol, c("armsInterventionsModule", "armGroupList", "armGroup"), list()), function(x) safe_extract(x, "label")), collapse = "; "),
    interventions = paste(sapply(safe_extract(protocol, c("armsInterventionsModule", "interventionList", "intervention"), list()), function(x) safe_extract(x, "name")), collapse = "; "),
    eligibilityCriteria = safe_extract(protocol, c("eligibilityModule", "eligibilityCriteria")),
    studyType = safe_extract(protocol, c("designModule", "studyType")),
    stringsAsFactors = FALSE
  )

  if(debug) {
    cat("Processed study info:\n")
    print(result)
  }

  return(result)
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
  list(
    STUDYID = function(df) df$orgStudyId,
    DOMAIN = function(df) "TS",
    TSSEQ = function(df) seq_len(nrow(df)),
    TSPARMCD = function(df) names(df),
    TSPARM = function(df) names(df),
    TSVAL = function(df) as.character(df[, 1]),
    TSVALNF = function(df) NA,
    TSVALCD = function(df) NA,
    TSVCDREF = function(df) NA,
    TSVCDVER = function(df) NA,
    TITLE = function(df) df$officialTitle,
    TPHASE = function(df) convert_to_roman(df$phase),
    INDIC = function(df) df$condition,
    TDIGRP = function(df) df$condition,
    THERAREA = function(df) derive_therapeutic_area(df$condition, df$interventions),
    REGID = function(df) df$nctId,
    INTTYPE = function(df) unique(df$interventionType),
    STYPE = function(df) df$studyType,
    TTYPE = function(df) derive_trial_type(df),
    ACTSUB = function(df) df$enrollment,
    PLANSUB = function(df) df$enrollment,
    SENDTC = function(df) format_date_iso8601(df$completionDate),
    SSTDTC = function(df) format_date_iso8601(df$startDate),
    ADAPT = function(df) derive_adaptive_design(df),
    ADDON = function(df) derive_addon_treatment(df),
    AGEMAX = function(df) df$ageMax,
    AGEMIN = function(df) df$ageMin,
    COMPTRT = function(df) derive_comparative_treatment(df),
    CURTRT = function(df) df$interventions,
    DCUTDESC = function(df) derive_data_cutoff_description(df),
    DCUTDTC = function(df) format_date_iso8601(df$primaryCompletionDate),
    EGBLIND = function(df) df$masking,
    INTMODEL = function(df) df$interventionModel,
    NARMS = function(df) df$numberOfArms,
    ONGOSIND = function(df) ifelse(df$overallStatus %in% c("Recruiting", "Active, not recruiting"), "Y", "N"),
    OUTMSPRI = function(df) {
      measures <- df$primaryOutcomeMeasures[[1]]
      if (is.null(measures) || all(is.na(measures))) {
        return(NA)
      }
      return(measures)
    },

    OUTMSSEC = function(df) {
      measures <- df$secondaryOutcomeMeasures[[1]]
      if (is.null(measures) || all(is.na(measures))) {
        return(NA)
      }
      return(measures)
    },

    OUTMSEXP = function(df) {
      measures <- df$otherOutcomeMeasures[[1]]
      if (is.null(measures) || all(is.na(measures))) {
        return(NA)
      }
      return(measures)
    },
    SEXPOP = function(df) df$gender,
    SPONSOR = function(df) df$sponsor,
    FCNTRY = function(df) {
      if (is.null(df$locations) || all(is.na(df$locations))) {
        warning("Locations data is null or NA")
        return(NA)
      }
      tryCatch({
        locations <- as.character(df$locations)
        cat("Raw locations data:", locations, "\n")  # Debug print
        location_parts <- strsplit(locations, "; ")
        countries <- unique(unlist(lapply(location_parts, function(loc) {
          parts <- strsplit(loc, ", ")[[1]]
          cat("Location parts:", paste(parts, collapse = " | "), "\n")  # Debug print
          if (length(parts) >= 3) {
            return(parts[length(parts)])  # Assume the country is the last part
          } else {
            return(NA)
          }
        })))
        cat("Extracted countries:", paste(countries, collapse = ", "), "\n")  # Debug print
        countries <- countries[!is.na(countries)]
        if (length(countries) > 0) {
          return(paste(countries, collapse = "; "))
        } else {
          warning("No valid countries extracted from locations")
          return(NA)
        }
      }, error = function(e) {
        warning("Error extracting countries from locations: ", e$message)
        return(NA)
      })
    },
    HLTSUBJI = function(df) derive_healthy_subjects(df),
    EXTTIND = function(df) derive_extension_trial(df),
    PDSTIND = function(df) derive_pediatric_study(df),
    LENGTH = function(df) {
      start_date <- as.Date(df$startDate)
      end_date <- as.Date(df$completionDate)
      if (!is.na(start_date) && !is.na(end_date)) {
        days <- as.numeric(end_date - start_date)
        return(paste0("P", days, "D"))
      } else {
        return(NA)
      }
    }
  )
}


#' Derive Comparative Treatment
#'
#' @param df The trial data frame.
#' @return A character string of comparative treatments, if any.
#' @keywords internal
derive_comparative_treatment <- function(df) {
  if (!is.null(df$armGroupLabels) && !is.na(df$armGroupLabels)) {
    arms <- unlist(strsplit(df$armGroupLabels, "; "))
    comp_arms <- arms[!grepl("placebo|control", tolower(arms))]
    if (length(comp_arms) > 0) {
      return(paste(comp_arms, collapse = "; "))
    }
  }
  return(NA)
}

#' Derive FCNTRY
#'
#' @param df The trial data frame.
#' @return A character string of unique countries.
#' @keywords internal
derive_fcntry <- function(df) {
  if (!is.null(df$locations)) {
    locations <- unlist(strsplit(as.character(df$locations), ", "))
    country <- locations[seq(3, length(locations), 3)]
  } else {
    country <- NA
  }
    unique_countries <- unique(countries[!is.na(countries)])
    if (length(unique_countries) > 0) {
      return(paste(unique_countries, collapse = "; "))
    }
  return(NA)
}

#' Derive Healthy Subjects Indicator
#'
#' @param df The trial data frame.
#' @return "Y" if healthy subjects, "N" if not, NA if unknown.
#' @keywords internal
derive_healthy_subjects <- function(df) {
  if (!is.null(df$healthyVolunteers) && !is.na(df$healthyVolunteers)) {
    if (tolower(df$healthyVolunteers) == "yes") {
      return("Y")
    } else if (tolower(df$healthyVolunteers) == "no") {
      return("N")
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
  if (is.na(condition) || is.na(interventions)) {
    return(NA)
  }
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
  } else if (grepl("vaccine|immun", interventions, ignore.case = TRUE)) {
    return("Immunology")
  } else {
    return("Other")
  }
}


#' Derive Trial Type
#'
#' @param df The trial data frame.
#' @return A character string representing the trial type.
#' @keywords internal
derive_trial_type <- function(df) {
  types <- c()
  if (!is.null(df$primaryOutcomeMeasure) && !is.na(df$primaryOutcomeMeasure)) {
    if (grepl("safety", tolower(df$primaryOutcomeMeasure), fixed = TRUE)) {
      types <- c(types, "SAFETY")
    }
    if (grepl("efficacy", tolower(df$primaryOutcomeMeasure), fixed = TRUE)) {
      types <- c(types, "EFFICACY")
    }
    if (grepl("pharmacokinetics|bioavailability|bioequivalence", tolower(df$primaryOutcomeMeasure), fixed = TRUE)) {
      types <- c(types, "PK")
    }
  }
  if (length(types) == 0) {
    types <- c("OTHER")
  }
  return(paste(types, collapse = "; "))
}

#' Derive Adaptive Design
#'
#' @param df The trial data frame.
#' @return "Y" if adaptive design, "N" otherwise.
#' @keywords internal
derive_adaptive_design <- function(df) {
  if ((!is.null(df$officialTitle) && !is.na(df$officialTitle) && grepl("adaptive", tolower(df$officialTitle), fixed = TRUE)) ||
      (!is.null(df$briefTitle) && !is.na(df$briefTitle) && grepl("adaptive", tolower(df$briefTitle), fixed = TRUE))) {
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
  if ((!is.null(df$officialTitle) && !is.na(df$officialTitle) && grepl("add-on|addon|adjunct", tolower(df$officialTitle), fixed = TRUE)) ||
      (!is.null(df$briefTitle) && !is.na(df$briefTitle) && grepl("add-on|addon|adjunct", tolower(df$briefTitle), fixed = TRUE))) {
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
  if (!is.null(df$overallStatus) && !is.na(df$overallStatus) && df$overallStatus %in% c("Completed", "Terminated")) {
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
  if (!is.null(df$overallStatus) && !is.na(df$overallStatus) && df$overallStatus %in% c("Completed", "Terminated")) {
    return(format_date_iso8601(df$completionDate))
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
  if (grepl("extension|follow-up|followup", tolower(df$officialTitle), fixed = TRUE) ||
      grepl("extension|follow-up|followup", tolower(df$briefTitle), fixed = TRUE)) {
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
  if (!is.null(df$healthyVolunteers) && !is.na(df$healthyVolunteers)) {
    if (tolower(df$healthyVolunteers) == "yes") {
      return("Y")
    } else if (tolower(df$healthyVolunteers) == "no") {
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
  min_age <- as.numeric(gsub("[^0-9.]", "", df$ageMin))
  if (!is.na(min_age) && min_age < 18) {
    return("Y")
  } else {
    return("N")
  }
}

# Update the define_ts_mapping function to include new derivations
define_ts_mapping <- function() {
  mapping <- list(
    STUDYID = function(df) df$orgStudyId,
    TITLE = function(df) df$officialTitle,
    TPHASE = function(df) convert_to_roman(df$phase),
    INDIC = function(df) df$condition,
    TDIGRP = function(df) df$condition,
    THERAREA = function(df) derive_therapeutic_area(df$condition, df$interventions),
    REGID = function(df) df$nctId,
    INTTYPE = function(df) unique(df$interventionType),
    STYPE = function(df) df$studyType,
    TTYPE = function(df) derive_trial_type(df),
    ACTSUB = function(df) df$enrollment,
    PLANSUB = function(df) df$enrollment,
    SENDTC = function(df) format_date_iso8601(df$completionDate),
    SSTDTC = function(df) format_date_iso8601(df$startDate),
    ADAPT = function(df) derive_adaptive_design(df),
    ADDON = function(df) derive_addon_treatment(df),
    AGEMAX = function(df) df$ageMax,
    AGEMIN = function(df) df$ageMin,
    COMPTRT = function(df) derive_comparative_treatment(df),
    CURTRT = function(df) df$interventions,
    DCUTDESC = function(df) derive_data_cutoff_description(df),
    DCUTDTC = function(df) derive_data_cutoff_date(df),
    EGBLIND = function(df) df$masking,
    INTMODEL = function(df) df$interventionModel,
    NARMS = function(df) df$numberOfArms,
    ONGOSIND = function(df) ifelse(df$overallStatus %in% c("Recruiting", "Active, not recruiting"), "Y", "N"),
    OUTMSPRI = function(df) df$primaryOutcomeMeasure,
    OUTMSSEC = function(df) df$secondaryOutcomeMeasure,
    SEXPOP = function(df) df$gender,
    SPONSOR = function(df) df$sponsor,
    FCNTRY = function(df) unique(unlist(strsplit(df$locations, ", "))[seq(3, length(unlist(strsplit(df$locations, ", "))), 3)]),
    EXTTIND = function(df) derive_extension_trial(df),
    HLTSUBJI = function(df) derive_healthy_subjects(df),
    PDSTIND = function(df) derive_pediatric_study(df),
    LENGTH = function(df) calculate_trial_length(df$startDate, df$completionDate)
  )

  return(mapping)
}
