# File: R/create_ta_te_domains_fd.R

#' Generate TA and TE Datasets for Factorial Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the FACTORIAL DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "FACTORIAL DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd` and `epochs`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @importFrom dplyr add_row distinct mutate select left_join
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @examples
#' \dontrun{
# study_id <- "STUDY004"
# trial_design <- "FACTORIAL DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM4",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM5",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM6",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   )
# )
# treatments <- list(
#   c("Treatment A", "Treatment B"), # Treatments for ARM1
#   c("Placebo B", "Treatment A"), # Treatments for ARM2
#   c("Placebo A", "Treatment B"), # Treatments for ARM3
#   c("Placebo B", "Treatment B"), # Treatments for ARM4
#   c("Placebo A", "Treatment A"), # Treatments for ARM5
#   c("Placebo A", "Placebo B")  # Treatments for ARM6
# )
# te_rules <- data.frame(
#   ELEMENT = c(
#     "SCREENING",
#     "TREATMENT TREATMENT A TREATMENT B",
#     "TREATMENT PLACEBO B TREATMENT A",
#     "TREATMENT PLACEBO A TREATMENT B",
#     "TREATMENT PLACEBO B TREATMENT B",
#     "TREATMENT PLACEBO A TREATMENT A",
#     "TREATMENT PLACEBO A PLACEBO B",
#     "FOLLOW-UP"
#   ),
#   TESTRL = c(
#     "Informed consent",
#     "First dose of Treatment A and Treatment B",
#     "First dose of Placebo B and Treatment A",
#     "First dose of Placebo A and Treatment B",
#     "First dose of Placebo B and Treatment B",
#     "First dose of Placebo A and Treatment A",
#     "First dose of Placebo A and Placebo B",
#     "End of follow-up"
#   ),
#   TEENRL = c(
#     "End of screening",
#     "End of Treatment A and Treatment B",
#     "End of Placebo B and Treatment A",
#     "End of Placebo A and Treatment B",
#     "End of Placebo B and Treatment B",
#     "End of Placebo A and Treatment A",
#     "End of Placebo A and Placebo B",
#     "End of study"
#   ),
#   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P14D", "P14D", "P21D"),
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
    num_elements <- length(element_descriptions)

    # Use provided ARMCD and ARM values or default to generated ones
    armcd <- ifelse(is.null(arm_data$armcd), paste0("ARM", i), arm_data$armcd)
    arm <- ifelse(is.null(arm_data$arm), paste0("Group ", i), arm_data$arm)

    # Validate the lengths of element descriptions and epochs
    if (length(element_descriptions) != num_elements) {
      stop(paste("Element descriptions do not match the number of elements for arm", i))
    }

    if (length(epochs) != num_elements) {
      stop(paste("Epochs do not match the number of elements for arm", i))
    }

    for (j in seq_along(element_descriptions)) {
      row_index <- (i - 1) * num_elements + j
      ta_df <- ta_df %>%
        add_row(
          STUDYID = study_id,
          DOMAIN = "TA",
          ARMCD = armcd,
          ARM = arm,
          TAETORD = j,
          ETCD = paste0("ET", row_index),
          ELEMENT = element_descriptions[j],
          TABRANCH = NA,
          TATRANS = NA,
          EPOCH = ifelse(grepl("TREATMENT", epochs[j], ignore.case = TRUE), "TREATMENT", epochs[j])
        )
    } 
  }

  # Create TE domain
  unique_elements <- ta_df %>%
    distinct(ELEMENT, .keep_all = TRUE) %>%
    arrange(factor(EPOCH, levels = c("SCREENING", "TREATMENT", "FOLLOW-UP"))) %>%
    mutate(ETCD = paste0("ET", row_number()), DOMAIN = "TE")

  te_df <- te_rules %>%
    mutate(
      STUDYID = study_id,
      DOMAIN = "TE",
      ETCD = paste0("ET", row_number())
    ) %>%
    select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)

  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)

  # Save TE domain to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}

# Helper function

#' Generate Element Descriptions for Factorial Design
#'
#' This function generates element descriptions from epochs with additional text for treatment
#' specifically for factorial design trials.
#'
#' @param epochs A character vector of epochs.
#' @param treatments A character vector of treatments for the current arm.
#' @return A character vector of element descriptions.
#' @keywords internal
generate_elements_fd <- function(epochs, treatments) {
  elements <- character(length(epochs))
  treatment_index <- 1
  for (i in seq_along(epochs)) {
    if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
      elements[i] <- paste("TREATMENT", treatments[treatment_index])
      treatment_index <- treatment_index + 1
    } else {
      elements[i] <- epochs[i]
    }
  }
  return(elements)
}

# study_id <- "STUDY004"
# trial_design <- "FACTORIAL DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM4",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM5",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM6",
#     epochs = "Screening,Treatment,Treatment,Follow-Up"
#   )
# )
# treatments <- list(
#   c("Treatment A", "Treatment B"), # Treatments for ARM1
#   c("Placebo B", "Treatment A"), # Treatments for ARM2
#   c("Placebo A", "Treatment B"), # Treatments for ARM3
#   c("Placebo B", "Treatment B"), # Treatments for ARM4
#   c("Placebo A", "Treatment A"), # Treatments for ARM5
#   c("Placebo A", "Placebo B")  # Treatments for ARM6
# )
# te_rules <- data.frame(
#   ELEMENT = c(
#     "SCREENING",
#     "TREATMENT TREATMENT A TREATMENT B",
#     "TREATMENT PLACEBO B TREATMENT A",
#     "TREATMENT PLACEBO A TREATMENT B",
#     "TREATMENT PLACEBO B TREATMENT B",
#     "TREATMENT PLACEBO A TREATMENT A",
#     "TREATMENT PLACEBO A PLACEBO B",
#     "FOLLOW-UP"
#   ),
#   TESTRL = c(
#     "Informed consent",
#     "First dose of Treatment A and Treatment B",
#     "First dose of Placebo B and Treatment A",
#     "First dose of Placebo A and Treatment B",
#     "First dose of Placebo B and Treatment B",
#     "First dose of Placebo A and Treatment A",
#     "First dose of Placebo A and Placebo B",
#     "End of follow-up"
#   ),
#   TEENRL = c(
#     "End of screening",
#     "End of Treatment A and Treatment B",
#     "End of Placebo B and Treatment A",
#     "End of Placebo A and Treatment B",
#     "End of Placebo B and Treatment B",
#     "End of Placebo A and Treatment A",
#     "End of Placebo A and Placebo B",
#     "End of study"
#   ),
#   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P14D", "P14D", "P21D"),
#   stringsAsFactors = FALSE
# )
#
# result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules)
# print(result$TA)
# print(result$TE)
