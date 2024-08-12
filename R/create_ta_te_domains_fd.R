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
#' study_id <- "STUDY004"
#' trial_design <- "FACTORIAL DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM3",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM4",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up"
#'   )
#' )
#' treatments <- list(
#'   c("A", "B"), # Treatments for ARM1
#'   c("Placebo B", "A"), # Treatments for ARM2
#'   c("Placebo A", "B"), # Treatments for ARM3
#'   c("Placebo A", "Placebo B")  # Treatments for ARM4
#' )
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "FOLLOW-UP", 
#'               "TREATMENT PLACEBO A", "TREATMENT PLACEBO B"),
#'   TESTRL = c("Informed consent", "First dose of study drug A", "First dose of study drug B", 
#'              "End of follow-up", "First dose of placebo A", "First dose of placebo B"),
#'   TEENRL = c("End of screening", "End of treatment A", "End of treatment B", 
#'              "End of study", "End of placebo A treatment", "End of placebo B treatment"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P21D", "P14D", "P14D")
#' )
#' 
#' result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules)
#' print(result$TA)
#' print(result$TE)
#' }
create_ta_te_domains_fd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
  
  # Validate inputs
  if (trial_design != "FACTORIAL DESIGN") {
    stop("This function is customized only for 'FACTORIAL DESIGN'.")
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
          EPOCH = epochs[j]
        )
    }
  }
  
  # Create TE domain
  unique_elements <- ta_df %>%
    distinct(ELEMENT, .keep_all = TRUE) %>%
    arrange(factor(EPOCH, levels = c("SCREENING", "TREATMENT", "FOLLOW-UP"))) %>%
    mutate(ETCD = paste0("ET", row_number()), DOMAIN = .data$TE)
  
  te_df <- unique_elements %>%
    left_join(te_rules, by = "ELEMENT") %>%
    select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)
  
  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, ta_output_file, overwrite = TRUE)
  
  # Save TE domain to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  addWorksheet(wb, "TE")
  writeData(wb, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, te_output_file, overwrite = TRUE)
  
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
  treatment_counter <- 1
  num_treatments <- length(treatments)
  elements <- sapply(seq_along(epochs), function(i) {
    if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
      treatment_index <- (treatment_counter - 1) %% num_treatments + 1
      element <- paste0("TREATMENT ", treatments[treatment_index])
      treatment_counter <<- treatment_counter + 1
      return(element)
    } else {
      return(epochs[i])
    }
  })
  return(elements)
}