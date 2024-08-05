library(dplyr)
library(openxlsx)

#' Generate TA and TE Datasets for SD
#'
#' This function generates the TA and TE datasets for a given study ID based on the SINGLE GROUP DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd` and `epochs`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing the TA and TE data frames.
#' @details 
#' This function creates the TA (Trial Arms) domain dataset, which is part of the SDTM (Study Data Tabulation Model), 
#' and then creates the TE (Trial Elements) domain dataset using the elements from the TA domain and user-provided rules.
#' It handles the "SINGLE GROUP DESIGN" trial design.
#' The dataset includes columns for study ID, domain, arm codes, epochs, elements with treatments.
#' 
#' @note 
#' - The `arms_data` parameter should be a list of arm data, where each arm includes arm code and epochs.
#' - The `treatments` parameter should be a list of treatments.
#' - The function supports only the "SINGLE GROUP DESIGN" trial design.
#' 
#' @examples
#' \dontrun{
#' # Example usage for SINGLE GROUP DESIGN
#' study_id <- "STUDY001"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#'   )
#' )
#' treatments <- list("A", "B", "C") # Define the treatments dynamically as a list
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of treatment", "End of treatment"),
#'   TEENRL = c("End of screening", "End of treatment period", "End of treatment period", "End of treatment period", "End of follow-up period"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D")
#' )
#' 
#' ta_te_df <- create_ta_domain_sd(study_id, arms_data, treatments, te_rules)
#' print(ta_te_df$TA)
#' print(ta_te_df$TE)
#' }
#' @export


# Sid Lokineni July 18th 2024
# Function to create TA and TE domains from dynamic inputs based on SINGLE GROUP DESIGN
create_ta_domain_sd <- function(study_id, arms_data, treatments, te_rules, output_dir = getwd()) {
  
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
    element_descriptions <- generate_elements(epochs, treatments)
    num_elements <- length(element_descriptions)
    
    # Use provided ARMCD and ARM values or default to generated ones
    armcd <- ifelse(is.null(arm_data$armcd), paste0("ARM", i), arm_data$armcd)
    arm <- ifelse(is.null(arm_data$arm), paste0("Group ", i), arm_data$arm)
    
    # Validate the lengths of element descriptions and epochs
    if(length(element_descriptions) != num_elements) {
      stop(paste("Element descriptions do not match the number of elements for arm", i))
    }
    
    if(length(epochs) != num_elements) {
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
  
  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)
  
  # Function to create TE domain from TA domain and TE rules
  create_te_domain <- function(ta_df, te_rules) {
    unique_elements <- ta_df %>%
      distinct(ELEMENT) %>%
      mutate(
        ETCD = paste0("ET", row_number()),
        DOMAIN = "TE"
      )
    
    # Merge the unique elements with the provided TE rules
    te_df <- unique_elements %>%
      left_join(te_rules, by = "ELEMENT") %>%
      mutate(STUDYID = ta_df$STUDYID[1]) %>%
      select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)
    
    return(te_df)
  }
  
  # Create the TE domain using the generated TA domain
  te_df <- create_te_domain(ta_df, te_rules)
  
  # Save the TE domain to an Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)
  
  return(list(TA = ta_df, TE = te_df))
}

# # Example usage for SINGLE GROUP DESIGN
# study_id <- "STUDY001"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#   )
# )
# treatments <- list("A", "B", "C") # Define the treatments dynamically as a list
# te_rules <- data.frame(
#   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"),
#   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of treatment", "End of treatment"),
#   TEENRL = c("End of screening", "End of treatment period", "End of treatment period", "End of treatment period", "End of follow-up period"),
#   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D")
# )
# 
# ta_te_df <- create_ta_domain_sd(study_id, arms_data, treatments, te_rules)
# print(ta_te_df$TA)
# print(ta_te_df$TE)
