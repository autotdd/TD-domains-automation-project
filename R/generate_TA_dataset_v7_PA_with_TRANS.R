library(dplyr)
library(openxlsx)

#' Generate TA and TE Datasets for PA with TRANS
#'
#' This function generates the TA and TE datasets for a given study ID and trial design.
#' It supports both "PARALLEL DESIGN" and "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be either "PARALLEL DESIGN" or "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `epochs`, and optionally `branch` and `trans`.
#' @param treatments_list A list of treatments corresponding to each arm.
#' @param te_rules A data frame containing TE rules with columns: TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing the TA and TE data frames.
#' @details 
#' This function creates the TA (Trial Arms) domain dataset, which is part of the SDTM (Study Data Tabulation Model), 
#' and then creates the TE (Trial Elements) domain dataset using the elements from the TA domain and user-provided rules.
#' It handles both "PARALLEL DESIGN" and "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" trials.
#' The dataset includes columns for study ID, domain, arm codes, epochs, elements with treatments, branches, and transitions.
#' 
#' @note 
#' - The `arms_data` parameter should be a list of arm data, where each arm includes arm code, epochs, branches (optional), and transitions (optional).
#' - The `treatments_list` parameter should be a list of treatments corresponding to each arm.
#' - The function supports "PARALLEL DESIGN" and "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" trials.
#' 
#' @examples
#' \dontrun{
#' # Example usage with PARALLEL DESIGN
#' study_id <- "STUDY002"
#' trial_design <- "PARALLEL DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM3",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#'   )
#' )
#' 
#' # Define treatments dynamically for each arm
#' treatments_list <- list(
#'   c("A", "B", "C"), # Treatments for ARM1
#'   c("D", "E", "F"), # Treatments for ARM2
#'   c("G", "H", "I")  # Treatments for ARM3
#' )
#' 
#' te_rules <- data.frame(
#'   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up"),
#'   TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study"),
#'   TEDUR = c("P7D", "P14D", "P7D", "P21D")
#' )
#' 
#' ta_te_df_parallel <- create_ta_domain_pat(study_id, trial_design, arms_data, treatments_list, te_rules)
#' print(ta_te_df_parallel$TA)
#' print(ta_te_df_parallel$TE)
#' 
#' # Example usage with PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS
#' trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#'     branch = c(NA, "Branch1", NA, NA, NA),
#'     trans = c(NA, "Trans1", "Trans2", NA, NA)
#'   ),
#'   list(
#'     armcd = "ARM2", 
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#'     branch = c(NA, "Branch2", NA, NA, NA),
#'     trans = c(NA, "Trans3", "Trans4", NA, NA)
#'   ),
#'   list(
#'     armcd = "ARM3",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#'     branch = c(NA, "Branch3", NA, NA, NA),
#'     trans = c(NA, "Trans5", "Trans6", NA, NA)
#'   )
#' )
#' 
#' # Define treatments dynamically for each arm
#' treatments_list <- list(
#'   c("A", "B", "C"), # Treatments for ARM1
#'   c("D", "E", "F"), # Treatments for ARM2
#'   c("G", "H", "I")  # Treatments for ARM3
#' )
#' 
#' ta_te_df_parallel_branches <- create_ta_domain_pat(study_id, trial_design, arms_data, treatments_list, te_rules)
#' print(ta_te_df_parallel_branches$TA)
#' print(ta_te_df_parallel_branches$TE)
#' }
#' @export

# Function to create TA and TE domains from dynamic inputs based on selected study design
create_ta_domain_pat <- function(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir = getwd()) {
  
  # Validate inputs
  if (trial_design != "PARALLEL DESIGN" && trial_design != "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS") {
    stop("This function only supports 'PARALLEL DESIGN' and 'PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS'")
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
  
  # Function to generate element descriptions from epochs with additional text for treatment
  generate_elements <- function(epochs, treatments) {
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
  
  # Populate TA domain data frame based on input rows
  for (i in seq_along(arms_data)) {
    arm_data <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
    treatments <- treatments_list[[i]]
    element_descriptions <- generate_elements(epochs, treatments)
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
          TABRANCH = ifelse(trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$branch), arm_data$branch[j], NA),
          TATRANS = ifelse(trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$trans), arm_data$trans[j], NA),
          EPOCH = epochs[j]
        )
    }
  }
  
  # Create TE domain using elements from TA domain and user-provided rules
  unique_elements <- unique(ta_df$ELEMENT)
  te_df <- data.frame(
    STUDYID = rep(study_id, length(unique_elements)),
    DOMAIN = rep("TE", length(unique_elements)),
    ETCD = paste0("ET", seq_along(unique_elements)),
    ELEMENT = unique_elements,
    TESTRL = te_rules$TESTRL[seq_along(unique_elements)],
    TEENRL = te_rules$TEENRL[seq_along(unique_elements)],
    TEDUR = te_rules$TEDUR[seq_along(unique_elements)],
    stringsAsFactors = FALSE
  )
  
  # Save TA to Excel file
  ta_output_file <- paste0(output_dir, "/", study_id, "_TA.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, ta_output_file, overwrite = TRUE)
  
  # Save TE to Excel file
  te_output_file <- paste0(output_dir, "/", study_id, "_TE.xlsx")
  addWorksheet(wb, "TE")
  writeData(wb, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, te_output_file, overwrite = TRUE)
  
  return(list(TA = ta_df, TE = te_df))
}

# # Example usage with PARALLEL DESIGN
# study_id <- "STUDY002"
# trial_design <- "PARALLEL DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#   )
# )
# 
# # Define treatments dynamically for each arm
# treatments_list <- list(
#   c("A", "B", "C"), # Treatments for ARM1
#   c("D", "E", "F"), # Treatments for ARM2
#   c("G", "H", "I")  # Treatments for ARM3
# )
# 
# te_rules <- data.frame(
#   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up"),
#   TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study"),
#   TEDUR = c("P7D", "P14D", "P7D", "P21D")
# )
# 
# ta_te_df_parallel <- create_ta_domain_pat(study_id, trial_design, arms_data, treatments_list, te_rules)
# print(ta_te_df_parallel$TA)
# print(ta_te_df_parallel$TE)
# 
# # Example usage with PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS
# trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#     branch = c(NA, "Branch1", NA, NA, NA),
#     trans = c(NA, "Trans1", "Trans2", NA, NA)
#   ),
#   list(
#     armcd = "ARM2", 
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#     branch = c(NA, "Branch2", NA, NA, NA),
#     trans = c(NA, "Trans3", "Trans4", NA, NA)
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up",
#     branch = c(NA, "Branch3", NA, NA, NA),
#     trans = c(NA, "Trans5", "Trans6", NA, NA)
#   )
# )
# 
# # Define treatments dynamically for each arm
# treatments_list <- list(
#   c("A", "B", "C"), # Treatments for ARM1
#   c("D", "E", "F"), # Treatments for ARM2
#   c("G", "H", "I")  # Treatments for ARM3
# )
# 
# ta_te_df_parallel_branches <- create_ta_domain_pat(study_id, trial_design, arms_data, treatments_list, te_rules)
# print(ta_te_df_parallel_branches$TA)
# print(ta_te_df_parallel_branches$TE)
