library(dplyr)
library(openxlsx)

#' Generate TA Dataset (Version 1) for SD
#'
#' This function generates the TA dataset for a given study ID and number of rows,
#' and reads from the 'Trial_Arms_SD.xlsx' file included with the package.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @return A data frame representing the TA dataset.
#' @examples
#' generate_TA_dataset_v1_SD("STUDY123", 5)
#' @export


# Sid Lokineni July 18th 2024
# Function to create TA domain from dynamic inputs based on SINGLE GROUP DESIGN
create_ta_domain <- function(study_id, arms_data, treatments, output_dir = getwd()) {
  
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
  for (i in 1:length(arms_data)) {
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
    
    for (j in 1:num_elements) {
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

# Example usage for SINGLE GROUP DESIGN
study_id <- "STUDY001"
arms_data <- list(
  list(
    armcd = "ARM1",
    epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
  )
)
treatments <- list("A", "B", "C") # Define the treatments dynamically as a list

ta_df_single_group <- create_ta_domain(study_id, arms_data, treatments)
print(ta_df_single_group)
