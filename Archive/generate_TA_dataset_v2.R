# Load necessary libraries
library(dplyr)
library(openxlsx)

# Sid Lokineni July 18th 2024
# Function to create TA domain from dynamic inputs based on selected study design
create_ta_domain <- function(study_id, trial_design, arms_data, output_dir = getwd()) {
  
  # Validate inputs
  if(!trial_design %in% c("SINGLE GROUP DESIGN", "PARALLEL DESIGN", "CROSS-OVER DESIGN", "FACTORIAL DESIGN", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS")) {
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
  generate_elements <- function(epochs, arm_index, trial_design) {
    treatment_counter <- 1
    if (trial_design == "CROSS-OVER DESIGN") {
      treatment_order <- ifelse(arm_index %% 2 == 0, c("B", "A"), c("A", "B"))
    } else {
      treatment_order <- LETTERS
    }
    elements <- sapply(seq_along(epochs), function(i) {
      if (grepl("TREATMENT", epochs[i], ignore.case = TRUE)) {
        element <- paste0("TREATMENT ", treatment_order[treatment_counter])
        treatment_counter <<- treatment_counter + 1
        return(element)
      } else {
        return(epochs[i])
      }
    })
    return(elements)
  }
  
  # Function to handle branches and transitions
  handle_branch_transition <- function(arm_data, num_elements) {
    tabranch <- rep(NA, num_elements)
    tatrans <- rep(NA, num_elements)
    if (!is.null(arm_data$branch_points)) {
      for (bp in arm_data$branch_points) {
        tabranch[bp$taetord] <- bp$tabranch
      }
    }
    if (!is.null(arm_data$transitions)) {
      for (tr in arm_data$transitions) {
        tatrans[tr$taetord] <- tr$tatrans
      }
    }
    return(list(tabranch = tabranch, tatrans = tatrans))
  }
  
  # Populate TA domain data frame based on input rows
  for (i in 1:length(arms_data)) {
    arm_data <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
    element_descriptions <- generate_elements(epochs, i, trial_design)
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
    
    # Handle branches and transitions if applicable
    branch_trans <- list(tabranch = rep(NA, num_elements), tatrans = rep(NA, num_elements))
    if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS") {
      branch_trans <- handle_branch_transition(arm_data, num_elements)
    }
    
    tabranch <- branch_trans$tabranch
    tatrans <- branch_trans$tatrans
    
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
          TABRANCH = tabranch[j],
          TATRANS = tatrans[j],
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

# Example usage with different study designs

# # SINGLE GROUP DESIGN example
# study_id <- "STUDY001"
# trial_design <- "SINGLE GROUP DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Follow-Up"
#   )
# )
# 
# ta_df_single_group <- create_ta_domain(study_id, trial_design, arms_data)
# print(ta_df_single_group)

# # # PARALLEL DESIGN example
# # study_id <- "STUDY002"
# # trial_design <- "PARALLEL DESIGN"
# # arms_data <- list(
# #   list(
# #     armcd = "ARM1",
# #     epochs = "Screening,Treatment,Follow-Up"
# #   ),
# #   list(
# #     armcd = "ARM2",
# #     epochs = "Screening,Treatment,Follow-Up"
# #   )
# # )
# # 
# # ta_df_parallel <- create_ta_domain(study_id, trial_design, arms_data)
# # print(ta_df_parallel)
# 
# # CROSS-OVER DESIGN example
# # study_id <- "STUDY003"
# # trial_design <- "CROSS-OVER DESIGN"
# # arms_data <- list(
# #   list(
# #     armcd = "ARM1",
# #     epochs = "Screening,Treatment,Washout,Treatment"
# #   ),
# #   list(
# #     armcd = "ARM2",
# #     epochs = "Screening,Treatment,Washout,Treatment"
# #   )
# # )
# # 
# # ta_df_cross_over <- create_ta_domain(study_id, trial_design, arms_data)
# # print(ta_df_cross_over)
# # 
# # FACTORIAL DESIGN example
# study_id <- "STUDY004"
# trial_design <- "FACTORIAL DESIGN"
# arms_data <- list(
#   list(
#     armcd = "ARM1",
#     epochs = "Screening,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM2",
#     epochs = "Screening,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM3",
#     epochs = "Screening,Treatment,Follow-Up"
#   ),
#   list(
#     armcd = "ARM4",
#     epochs = "Screening,Treatment,Follow-Up"
#   )
# )
# 
# ta_df_factorial <- create_ta_domain(study_id, trial_design, arms_data)
# print(ta_df_factorial)
# 
# #PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS example
# study_id <- "STUDY005"
trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
arms_data <- list(
  list(
    armcd = "ARM1",
    epochs = "Screening,Treatment,Follow-Up",
    branch_points = list(list(taetord = 1, tabranch = "Randomized to Treatment A")),
    transitions = list(list(taetord = 2, tatrans = "If condition X is true, then go to Follow-Up"))
  ),
  list(
    armcd = "ARM2",
    epochs = "Screening,Treatment,Follow-Up",
    branch_points = list(list(taetord = 1, tabranch = "Randomized to Treatment B")),
    transitions = list(list(taetord = 2, tatrans = "If condition Y is true, then go to Follow-Up"))
  )
)

ta_df_branches_transitions <- create_ta_domain(study_id, trial_design, arms_data)
print(ta_df_branches_transitions)
