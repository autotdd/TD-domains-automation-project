# File: R/create_ta_te_domains_sd.R

#' Generate TA and TE Datasets for Single Group Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the SINGLE GROUP DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `arm`, `epochs`, and `etcd`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @import dplyr openxlsx lubridate
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @importFrom stringr str_to_upper
#' @examples
#' \dontrun{
#' study_id <- "STUDY001"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     arm = "Single Treatment Arm",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#'     etcd = "SCRN,TRT1,TRT2,TRT3,FUP"
#'   )
#' )
#' treatments <- list("A", "B", "C")
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "A", "B", "C", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose of A", "First dose of B",
#'              "First dose of C", "End of treatment"),
#'   TEENRL = c("End of screening", "End of A", "End of B",
#'              "End of C", "End of follow-up period"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D")
#' )
#'
#' result <- create_ta_te_domains_sd(study_id, arms_data, treatments, te_rules)
#' print(result$TA)
#' print(result$TE)
#' }
create_ta_te_domains_sd <- function(study_id, arms_data, treatments, te_rules, output_dir = getwd()) {
  # Initialize TA domain data frame
  ta_df <- data.frame(
    STUDYID = character(),
    DOMAIN = character(),
    ARMCD = character(),
    ARM = character(),
    TAETORD = numeric(),
    ETCD = character(),
    ELEMENT = character(),
    TABRANCH = character(),
    TATRANS = character(),
    EPOCH = character(),
    stringsAsFactors = FALSE
  )
 
  # Populate TA domain data frame based on input rows
  for (i in seq_along(arms_data)) {
    arm_data <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
    etcd <- unlist(strsplit(arm_data$etcd, ","))
    element_descriptions <- generate_elements_sd(epochs, treatments)
    num_elements <- length(element_descriptions)

    armcd <- arm_data$armcd
    arm <- arm_data$arm

    if(length(element_descriptions) != num_elements || length(epochs) != num_elements || length(etcd) != num_elements) {
      stop(paste("Mismatch in the number of elements, epochs, or ETCDs for arm", i))
    }

    treatment_epochs <- sum(grepl("TREATMENT", epochs, ignore.case = TRUE))
    if(treatment_epochs != length(treatments)) {
      stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
    }

    for (j in seq_along(element_descriptions)) {
      ta_df <- rbind(ta_df, data.frame(
        STUDYID = study_id,
        DOMAIN = "TA",
        ARMCD = armcd,
        ARM = arm,
        TAETORD = j,
        ETCD = etcd[j],
        ELEMENT = as.character(element_descriptions[j]),
        TABRANCH = NA,
        TATRANS = NA,
        EPOCH = epochs[j],
        stringsAsFactors = FALSE
      ))
    }
  }

  # Create a complete mapping of ELEMENT to ETCD
  etcd_mapping <- data.frame(
    ELEMENT = character(),
    ETCD = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(arms_data)) {
    arm <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm$epochs, ",")))
    etcds <- unlist(strsplit(arm$etcd, ","))
    element_descriptions <- generate_elements_sd(epochs, treatments)
    
    for (j in seq_along(epochs)) {
      etcd_mapping <- rbind(etcd_mapping, data.frame(
        ELEMENT = as.character(element_descriptions[j]), 
        ETCD = etcds[j], 
        stringsAsFactors = FALSE
      ))
    }
  }
  etcd_mapping <- unique(etcd_mapping)

  # Ensure te_rules ELEMENT column is character
  te_rules$ELEMENT <- as.character(te_rules$ELEMENT)

  # Merge te_rules with etcd_mapping
  te_df <- merge(te_rules, etcd_mapping, by = "ELEMENT", all = TRUE)

  # Process TE rules and sort by days in TEDUR (ISO8601 format)
  te_df$STUDYID <- study_id
  te_df$DOMAIN <- "TE"
  te_df$DurationDays <- as.numeric(gsub("[^0-9]", "", te_df$TEDUR))
  te_df <- te_df[order(te_df$DurationDays), ]
  te_df <- te_df[, c("STUDYID", "DOMAIN", "ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR")]

  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, ta_output_file, overwrite = TRUE)

  # Save TE domain to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}

# Helper function

#' Generate Element Descriptions for Single Group Design
#'
#' This function generates element descriptions from epochs with additional text for treatment
#' specifically for single group design trials.
#'
#' @param epochs A character vector of epochs.
#' @param treatments A list of treatments for the trial.
#' @return A character vector of element descriptions.
#' @keywords internal
generate_elements_sd <- function(epochs, treatments) {
  treatment_counter <- 1
  num_treatments <- length(treatments)
  elements <- character(length(epochs))
  for (i in seq_along(epochs)) {
    if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
      treatment_index <- (treatment_counter - 1) %% num_treatments + 1
      elements[i] <- as.character(treatments[treatment_index])
      treatment_counter <- treatment_counter + 1
    } else {
      elements[i] <- as.character(epochs[i])
    }
  }
  return(elements)
}