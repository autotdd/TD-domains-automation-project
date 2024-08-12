# File: R/create_ta_te_domains_pa.R

#' Generate TA and TE Datasets for Parallel Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets 
#' for a given study ID. It supports both simple "PARALLEL DESIGN" and 
#' "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. 
#'        Should be either "PARALLEL DESIGN" or "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#' @param arms_data A list of arm data. Each element in the list should be a list containing 
#'        `armcd`, `epochs`, and optionally `branch` and `trans`.
#' @param treatments_list A list of treatments corresponding to each arm.
#' @param te_rules A data frame containing `TESTRL`, `TEENRL`, and `TEDUR` rules.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @importFrom dplyr add_row distinct arrange mutate select left_join
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @examples
#' \dontrun{
#' # Example for simple PARALLEL DESIGN
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
#'   )
#' )
#' treatments_list <- list(
#'   c("A", "B", "C"),
#'   c("D", "E", "F")
#' )
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", 
#'               "TREATMENT D", "TREATMENT E", "TREATMENT F", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C", 
#'              "First dose D", "First dose E", "First dose F", "End of treatment"),
#'   TEENRL = c("End of screening", "End of A", "End of B", "End of C", 
#'              "End of D", "End of E", "End of F", "End of follow-up"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P14D", "P14D", "P21D")
#' )
#' 
#' result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)
#' print(result$TA)
#' print(result$TE)
#' 
#' # Example for PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS
#' trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up",
#'     branch = c(NA, "Branch1", NA, NA),
#'     trans = c(NA, "Trans1", "Trans2", NA)
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up",
#'     branch = c(NA, "Branch2", NA, NA),
#'     trans = c(NA, "Trans3", "Trans4", NA)
#'   )
#' )
#' 
#' result_with_transitions <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)
#' print(result_with_transitions$TA)
#' print(result_with_transitions$TE)
#' }
create_ta_te_domains_pa <- function(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir = getwd()) {
  
  # Validate inputs
  if (!trial_design %in% c("PARALLEL DESIGN", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS")) {
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
          TABRANCH = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$branch)) arm_data$branch[j] else NA,
          TATRANS = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$trans)) arm_data$trans[j] else NA,
          EPOCH = epochs[j]
        )
    }
  }
  
  # Create TE domain from unique elements in TA domain, sorted by EPOCH
  unique_elements <- ta_df %>%
    distinct(ELEMENT, .keep_all = TRUE) %>%
    arrange(factor(EPOCH, levels = c("SCREENING", "TREATMENT", "FOLLOW-UP"))) %>%
    mutate(ETCD = paste0("ET", row_number())) %>%
    select(STUDYID, ETCD, ELEMENT)
  
  # Ensure te_rules are not repeated
  if (nrow(te_rules) < nrow(unique_elements)) {
    stop("The number of TE rules provided is less than the number of unique elements in the TA domain.")
  }
  
  te_df <- unique_elements %>%
    left_join(te_rules, by = c("ELEMENT")) %>%
    mutate(DOMAIN = "TE") %>%
    select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)
  
  # Save TA to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)
  
  # Save TE to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)
  
  return(list(TA = ta_df, TE = te_df))
}

# Helper function

#' Generate Element Descriptions
#'
#' This function generates element descriptions from epochs with additional text for treatment.
#'
#' @param epochs A character vector of epochs.
#' @param treatments A character vector of treatments.
#' @return A character vector of element descriptions.
#' @keywords internal
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