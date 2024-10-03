#' Generate TA and TE Datasets for Factorial Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the FACTORIAL DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "FACTORIAL DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `epochs`, `etcd` (optional).
#' @param treatments A list of treatments for each arm. Each element should be a vector of treatments corresponding to the arm.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR (in ISO 8601 format).
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @importFrom dplyr add_row distinct mutate select left_join arrange
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @importFrom lubridate duration
#' @examples
#' \dontrun{
# study_id <- "STUDY004"
# trial_design <- "FACTORIAL DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Follow-Up",
#     etcd = c("SCRN", "TRT1", "TRT2", "FU")
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening,Treatment,Treatment,Follow-Up",
#     etcd = c("SCRN", "TRT3", "TRT4", "FU")
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Treatment,Follow-Up",
#     etcd = c("SCRN", "TRT5", "TRT6", "FU")
#   )
# )
# treatments <- list(
#   c("Treatment A", "Treatment B"), # Treatments for ARM1
#   c("Treatment C", "Treatment D"), # Treatments for ARM2
#   c("Treatment E", "Treatment F")  # Treatments for ARM3
# )
# te_rules <- data.frame(
#   ELEMENT = c(
#     "SCREENING",
#     "TREATMENT A",
#     "TREATMENT B",
#     "TREATMENT C",
#     "TREATMENT D",
#     "TREATMENT E",
#     "TREATMENT F",
#     "FOLLOW-UP"
#   ),
#   TESTRL = c(
#     "Informed consent",
#     "First dose of Treatment A",
#     "First dose of Treatment B",
#     "First dose of Treatment C",
#     "First dose of Treatment D",
#     "First dose of Treatment E",
#     "First dose of Treatment F",
#     "End of follow-up"
#   ),
#   TEENRL = c(
#     "End of screening",
#     "End of Treatment A",
#     "End of Treatment B",
#     "End of Treatment C",
#     "End of Treatment D",
#     "End of Treatment E",
#     "End of Treatment F",
#     "End of study"
#   ),
#   TEDUR = c("P7D", "P14D", "P14D", "P21D", "P14D", "P14D", "P21D", "P30D"),
#   stringsAsFactors = FALSE
# )
#
# result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules)
# print(result$TA)
# print(result$TE)
#' }
create_ta_te_domains_fd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
  # Validate inputs
  if (trial_design != "FACTORIAL DESIGN") {
    stop("This function is customized only for 'FACTORIAL DESIGN'.")
  }

  # Check for mismatched epochs and treatments
  for (i in seq_along(arms_data)) {
    epochs <- unlist(strsplit(arms_data[[i]]$epochs, ","))
    treatment_epochs <- sum(grepl("TREATMENT", epochs, ignore.case = TRUE))
    if (treatment_epochs != length(treatments[[i]])) {
      stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
    }
  }

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
    element_descriptions <- generate_elements_fd(epochs, treatments[[i]])
    etcd <- arm_data$etcd  # Allow users to specify ETCD as free text
    num_elements <- length(element_descriptions)

    armcd <- ifelse(is.null(arm_data$armcd), paste0("ARM", i), arm_data$armcd)
    arm <- ifelse(is.null(arm_data$arm), paste0("Group ", i), arm_data$arm)

    for (j in seq_along(element_descriptions)) {
      row_index <- (i - 1) * num_elements + j
      ta_df <- ta_df %>%
        add_row(
          STUDYID = study_id,
          DOMAIN = "TA",
          ARMCD = armcd,
          ARM = arm,
          TAETORD = j,
          ETCD = if (!is.null(etcd)) etcd[j] else paste0("ET", row_index),
          ELEMENT = element_descriptions[j],  # No "TREATMENT" prefix
          TABRANCH = NA,
          TATRANS = NA,
          EPOCH = epochs[j]
        )
    }
  }

  # Create TE domain
  unique_elements <- ta_df %>%
    distinct(STUDYID, ELEMENT, ETCD, .keep_all = TRUE) %>%
    arrange(factor(EPOCH, levels = c("SCREENING", "TREATMENT", "FOLLOW-UP"))) %>%
    mutate(DOMAIN = "TE",
           ELEMENT_UPPER = toupper(ELEMENT))  # Add uppercase ELEMENT for joining

  # Prepare te_rules
  te_rules <- te_rules %>%
    mutate(
      STUDYID = study_id,
      DOMAIN = "TE",
      ELEMENT_UPPER = toupper(ELEMENT)  # Add uppercase ELEMENT for joining
    )

  # Join TE rules with unique elements from TA domain
  te_df <- unique_elements %>%
    left_join(te_rules, by = c("ELEMENT_UPPER" = "ELEMENT_UPPER")) %>%
    select(STUDYID = STUDYID.x, DOMAIN = DOMAIN.x, ETCD, ELEMENT = ELEMENT.x, TESTRL, TEENRL, TEDUR) %>%
    distinct()

  # Save TA and TE domain to Excel files
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)

  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}

# Helper function for generating elements
generate_elements_fd <- function(epochs, treatments) {
  elements <- character(length(epochs))
  treatment_index <- 1
  for (i in seq_along(epochs)) {
    if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
      elements[i] <- treatments[treatment_index]  # No prefix
      treatment_index <- treatment_index + 1
    } else {
      elements[i] <- epochs[i]
    }
  }
  return(elements)
}

# Function to convert ISO 8601 duration to numeric days
convert_iso8601_to_days <- function(duration_str) {
  duration_obj <- duration(duration_str)
  as.numeric(duration_obj, "days")
}
