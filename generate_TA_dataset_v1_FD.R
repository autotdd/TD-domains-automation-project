# Load necessary libraries
library(dplyr)
library(openxlsx)

# Sid Lokineni July 18th 2024
# Function to create TA domain from dynamic inputs based on FACTORIAL DESIGN
create_ta_domain <- function(study_id, trial_design, arms_data, treatments, output_dir = getwd()) {
  
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
    element_descriptions <- generate_elements(epochs, treatments[[i]])
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

# FACTORIAL DESIGN example
study_id <- "STUDY004"
trial_design <- "FACTORIAL DESIGN"
arms_data <- list(
  list(
    armcd = "ARM1",
    epochs = "Screening,Treatment,Treatment,Follow-Up"
  ),
  list(
    armcd = "ARM2",
    epochs = "Screening,Treatment,Treatment,Follow-Up"
  ),
  list(
    armcd = "ARM3",
    epochs = "Screening,Treatment,Treatment,Follow-Up"
  ),
  list(
    armcd = "ARM4",
    epochs = "Screening,Treatment,Treatment,Follow-Up"
  )
)
treatments <- list(
  c("A", "B"), # Treatments for ARM1
  c("Placebo B", "A"), # Treatments for ARM2
  c("Placebo A", "B"), # Treatments for ARM3
  c("Placebo A", "Placebo B")  # Treatments for ARM4
)

ta_df_factorial <- create_ta_domain(study_id, trial_design, arms_data, treatments)
print(ta_df_factorial)
