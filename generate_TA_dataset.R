# Load necessary libraries
library(dplyr)
library(openxlsx)

# Function to create TA domain from dynamic Excel input
create_ta_domain_from_dynamic_excel <- function(input_file, output_dir = getwd()) {
  
  # Read the Excel file
  inputs <- read.xlsx(input_file, sheet = "Inputs")
  
  # Extract common parameters
  study_id <- inputs$study_id[1]
  trial_design <- inputs$trial_design[1]
  num_arms <- inputs$num_arms[1]
  num_treatments <- inputs$num_treatments[1]
  
  # Validate inputs
  if (!trial_design %in% c("SINGLE GROUP DESIGN", "PARALLEL DESIGN", "CROSS-OVER DESIGN", "FACTORIAL DESIGN")) {
  stop("Invalid trial design. Choose from 'SINGLE GROUP DESIGN', 'PARALLEL DESIGN', 'CROSS-OVER DESIGN', 'FACTORIAL DESIGN'")
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
  
  # Helper function to create ARMCD and ARM names
  create_arm_names <- function(num_arms, design) {
  arm_names <- c()
  for (i in 1:num_arms) {
    arm_code <- paste0("ARM", i)
    if (design == "SINGLE GROUP DESIGN") {
    arm_name <- "Single Group"
    } else if (design == "PARALLEL DESIGN") {
    arm_name <- paste0("Parallel Group ", i)
    } else if (design == "CROSS-OVER DESIGN") {
    arm_name <- paste0("Cross-Over Group ", i)
    } else if (design == "FACTORIAL DESIGN") {
    arm_name <- paste0("Factorial Group ", i)
    }
    arm_names <- c(arm_names, list(c(arm_code, arm_name)))
  }
  return(arm_names)
  }
  
  arm_names <- create_arm_names(num_arms, trial_design)
  
  # Populate TA domain data frame based on input rows
  for (i in 1:nrow(inputs)) {
  element_descriptions <- unlist(strsplit(inputs$element_descriptions[i], ","))
  epochs <- unlist(strsplit(inputs$epochs[i], ","))
  
  if (length(element_descriptions) != num_treatments) {
    stop("Element descriptions do not match the number of treatments for arm ", i)
  }
  
  if (length(epochs) != num_treatments) {
    stop("Epochs do not match the number of treatments for arm ", i)
  }
  
  for (j in 1:num_treatments) {
    row_index <- (i - 1) * num_treatments + j
    ta_df <- ta_df %>% 
    add_row(
      STUDYID = study_id,
      DOMAIN = "TA",
      ARMCD = arm_names[[i]][1],
      ARM = arm_names[[i]][2],
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

# Example usage with input file
input_file <- "TD-domains-automation-project/dynamic_input_file.xlsx"

# Create TA domain from the provided Excel file
ta_df <- create_ta_domain_from_dynamic_excel(input_file)

# Display the TA domain data frame
print(ta_df)