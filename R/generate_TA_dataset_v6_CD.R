library(dplyr)
library(openxlsx)
library(tibble)  # Ensure tibble is loaded for add_row function

#' Generate TA and TE Datasets for CROSS-OVER trial design
#'
#' This function generates the TA and TE datasets for a given study ID and trial design.
#' It supports only the CROSS-OVER trial design.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "CROSS-OVER".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd` and `epochs`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing the TA and TE data frames.
#' @details 
#' This function creates the TA (Trial Arms) domain dataset, which is part of the SDTM (Study Data Tabulation Model),
#' and then creates the TE (Trial Elements) domain dataset using the elements from the TA domain and user-provided rules.
#' 
#' @note 
#' - The `arms_data` parameter should be a list of arm data, where each arm includes arm code and epochs.
#' - The `treatments` parameter should be a list of treatments.
#' - The function supports only the "CROSS-OVER DESIGN".
#' 
#' @examples
#' \dontrun{
#' # Example usage with CROSS-OVER
#' study_id <- "STUDY003"
#' trial_design <- "CROSS-OVER DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#'   ),
#'   list(
#'     armcd = "ARM3",
#'     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#'   )
#' )
#' treatments <- list(c("A", "B", "C")) # Define the treatments dynamically
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT"),
#'   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up", "End of washout"),
#'   TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study", "End of washout period"),
#'   TEDUR = c("P7D", "P14D", "P7D", "P21D", "P7D")
#' )
#' 
#' ta_te_df <- create_ta_domain_cd(study_id, trial_design, arms_data, treatments, te_rules)
#' print(ta_te_df$TA)
#' print(ta_te_df$TE)
#' }
#' @export

create_ta_domain_cd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
  
  # Validate inputs
  if (!trial_design %in% c("CROSS-OVER DESIGN")) {
    stop("Invalid trial design. Choose from CROSS-OVER DESIGN")
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
  generate_elements <- function(epochs, arm_index, treatments) {
    num_treatments <- length(treatments[[1]])
    treatment_counter <- (arm_index - 1) %% num_treatments
    elements <- sapply(seq_along(epochs), function(i) {
      if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
        treatment_index <- (treatment_counter %% num_treatments) + 1
        element <- paste0("TREATMENT ", treatments[[1]][treatment_index])
        treatment_counter <<- treatment_counter + 1
        return(element)
      } else {
        return(trimws(epochs[i]))
      }
    })
    return(elements)
  }
  
  # Populate TA domain data frame based on input rows
  row_index <- 1
  for (i in seq_along(arms_data)) {
    arm_data <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
    element_descriptions <- generate_elements(epochs, i, treatments)
    num_elements <- length(element_descriptions)
    
    # Use provided ARMCD and ARM values or default to generated ones
    armcd <- ifelse(is.null(arm_data$armcd), paste0("ARM", i + 1), arm_data$armcd)
    arm <- ifelse(is.null(arm_data$arm), paste0("Group ", i + 1), arm_data$arm)
    
    # Validate the lengths of element descriptions and epochs
    if (length(element_descriptions) != num_elements) {
      stop(paste("Element descriptions do not match the number of elements for arm", i + 1))
    }
    
    if (length(epochs) != num_elements) {
      stop(paste("Epochs do not match the number of elements for arm", i + 1))
    }
    
    # Populate the data frame
    for (j in seq_along(element_descriptions)) {
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
      row_index <- row_index + 1
    }
  }
  
  # Print TA dataframe for debugging
  print("TA DataFrame:")
  print(ta_df)
  print(colnames(ta_df))
  
  # Save TA domain to Excel file
  ta_output_file <- paste0(output_dir, "/", study_id, "_TA.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, ta_output_file, overwrite = TRUE)
  
  # Function to create TE domain from TA domain and TE rules
  create_te_domain <- function(ta_df, te_rules) {
    unique_elements <- ta_df %>%
      distinct(ELEMENT) %>%
      mutate(
        ETCD = paste0("ET", row_number()),
        DOMAIN = "TE",
        STUDYID = ta_df$STUDYID[1]  # Correctly assign STUDYID
      )
    
    # Print Unique Elements for debugging
    print("Unique Elements DataFrame:")
    print(unique_elements)
    print(colnames(unique_elements))
    
    # Merge the unique elements with the provided TE rules
    te_df <- unique_elements %>%
      left_join(te_rules, by = "ELEMENT") %>%
      select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR) %>%
      distinct()  # Ensure there are no duplicate rows
    
    # Print TE DataFrame for debugging
    print("TE DataFrame:")
    print(te_df)
    print(colnames(te_df))
    
    return(te_df)
  }
  
  # Create the TE domain using the generated TA domain
  te_df <- create_te_domain(ta_df, te_rules)
  
  # Save the TE domain to an Excel file
  te_output_file <- paste0(output_dir, "/", study_id, "_TE.xlsx")
  addWorksheet(wb, "TE")
  writeData(wb, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, te_output_file, overwrite = TRUE)
  
  return(list(TA = ta_df, TE = te_df))
}

# # Example usage with CROSS-OVER
# study_id <- "STUDY003"
# trial_design <- "CROSS-OVER DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
#   )
# )
# treatments <- list(c("A", "B", "C")) # Define the treatments dynamically
# te_rules <- data.frame(
#   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT"),
#   TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up", "End of washout"),
#   TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study", "End of washout period"),
#   TEDUR = c("P7D", "P14D", "P7D", "P21D", "P7D")
# )
# 
# ta_te_df <- create_ta_domain_cd(study_id, trial_design, arms_data, treatments, te_rules)
# print(ta_te_df$TA)
# print(ta_te_df$TE)
