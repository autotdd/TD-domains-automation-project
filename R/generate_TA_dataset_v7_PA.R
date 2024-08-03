library(dplyr)
library(openxlsx)

#' Generate TA Dataset (Version 7) for PA
#'
#' This function generates the TA dataset for a given study ID and number of rows,
#' and reads from the 'Trial_Arms.xlsx' file included with the package.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @return A data frame representing the TA dataset.
#' @examples
#' generate_TA_dataset_v7_PA("STUDY123", 5)
#' @export


# Sid Lokineni July 18th 2024
# Function to create TA domain from dynamic inputs based on selected study design
create_ta_domain <- function(study_id, trial_design, arms_data, treatments_list, output_dir = getwd()) {
  
  # Validate inputs
  if (trial_design != "PARALLEL DESIGN") {
    stop("This function only supports 'PARALLEL DESIGN'")
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
          TABRANCH = NA,
          TATRANS = NA,
          EPOCH = epochs[j]
        )
    }
  }
  
  # Save to Excel file
  output_file <- paste0(output_dir, "/", study_id, "_TA.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(ta_df)
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
# ta_df_parallel <- create_ta_domain(study_id, trial_design, arms_data, treatments_list)
# print(ta_df_parallel)
