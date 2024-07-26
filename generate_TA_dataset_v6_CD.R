# Load necessary libraries
library(dplyr)
library(openxlsx)

# Function to create TA domain from dynamic inputs based on selected study design
create_ta_domain <- function(study_id, trial_design, arms_data, treatments, output_dir = getwd()) {
  
  # Validate inputs
  if (!trial_design %in% c("SINGLE GROUP DESIGN", "PARALLEL DESIGN", "CROSS-OVER DESIGN", "FACTORIAL DESIGN", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS")) {
    stop("Invalid trial design. Choose from 'SINGLE GROUP DESIGN', 'PARALLEL DESIGN', 'CROSS-OVER DESIGN', 'FACTORIAL DESIGN', 'PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS'")
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
        return(epochs[i])
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
  
  # Save to Excel file
  output_file <- paste0(output_dir, "/", study_id, "_TA.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(ta_df)
}

# Example usage with CROSS-OVER DESIGN
study_id <- "STUDY003"
trial_design <- "CROSS-OVER DESIGN"
arms_data <- list(
  list(
    armcd = "ARM1",
    epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
  ),
  list(
    armcd = "ARM2",
    epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
  ),
  list(
    armcd = "ARM3",
    epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
  )
)
treatments <- list(c("A", "B", "C")) # Define the treatments dynamically



ta_df_cross_over <- create_ta_domain(study_id, trial_design, arms_data, treatments)
print(ta_df_cross_over)
