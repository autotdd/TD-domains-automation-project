#' Create TA and TE Domains for Parallel Arm Studies
#'
#' This function generates Trial Arms (TA) and Trial Elements (TE) domains for parallel arm studies,
#' including those with branches and transitions.
#'
#' @param study_id A string identifying the study.
#' @param trial_design A string specifying the trial design, either "PARALLEL DESIGN" or "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#' @param arms_data A list of arm data, each containing armcd, arm, epochs, and etcd.
#' @param treatments_list A list of treatments for each arm.
#' @param te_rules A data frame specifying rules for trial elements.
#' @param output_dir The directory where output files will be saved (default is current working directory).
#'
#' @return A list containing TA and TE data frames.
#'
#' @examples
#' # Example for "PARALLEL DESIGN"
#' study_id <- "STUDY001"
#' trial_design <- "PARALLEL DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     arm = "Arm 1",
#'     epochs = "Screening,Treatment,Follow-Up",
#'     etcd = c("SCREEN", "TREAT", "FOLLOW")
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     arm = "Arm 2",
#'     epochs = "Screening,Treatment,Follow-Up",
#'     etcd = c("SCREEN", "TREAT", "FOLLOW")
#'   )
#' )
#' treatments_list <- list(c("A"), c("B"))
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose A", "First dose B", "End of treatment"),
#'   TEENRL = c("End of screening", "End of A", "End of B", "End of follow-up"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P21D")
#' )
#' result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)
#'
#' # Example for "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
#' study_id <- "STUDY002"
#' trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     arm = "Arm 1",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up",
#'     etcd = c("SCREEN", "TREAT A", "TREAT B", "FOLLOW"),
#'     branch = c(NA, "BRANCH1", "BRANCH2", NA),
#'     trans = c(NA, NA, "TRANS1", NA)
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     arm = "Arm 2",
#'     epochs = "Screening,Treatment,Treatment,Follow-Up",
#'     etcd = c("SCREEN", "TREAT C", "TREAT D", "FOLLOW"),
#'     branch = c(NA, "BRANCH3", "BRANCH4", NA),
#'     trans = c(NA, NA, "TRANS2", NA)
#'   )
#' )
#' treatments_list <- list(c("A", "B"), c("C", "D"))
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "TREATMENT D", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C", "First dose D", "End of treatment"),
#'   TEENRL = c("End of screening", "End of A", "End of B", "End of C", "End of D", "End of follow-up"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P21D")
#' )
#' result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)
#'
#' @export
create_ta_te_domains_pa <- function(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir = getwd()) {

  for (i in seq_along(arms_data)) {
    epochs <- unlist(strsplit(arms_data[[i]]$epochs, ","))
    treatments <- treatments_list[[i]]
    if (sum(grepl("TREATMENT", epochs, ignore.case = TRUE)) != length(treatments)) {
      stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
    }
  }

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

  # Updated helper function
  generate_elements <- function(epochs, treatments) {
    elements <- character(length(epochs))
    treatment_index <- 1
    
    for (i in seq_along(epochs)) {
      if (toupper(epochs[i]) == "TREATMENT") {
        elements[i] <- paste("TREATMENT", toupper(treatments[treatment_index]))
        treatment_index <- treatment_index + 1
      } else {
        elements[i] <- toupper(epochs[i])
      }
    }
    
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

    for (j in seq_along(element_descriptions)) {
      row_index <- (i - 1) * num_elements + j
      ta_df <- ta_df %>%
        add_row(
          STUDYID = study_id,
          DOMAIN = "TA",
          ARMCD = armcd,
          ARM = arm,
          TAETORD = j,
          ETCD = ifelse(!is.null(arm_data$etcd), arm_data$etcd[j], paste0("ET", row_index)),
          ELEMENT = element_descriptions[j],  # Use the generated element descriptions
          TABRANCH = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$branch)) arm_data$branch[j] else NA,
          TATRANS = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" && !is.null(arm_data$trans)) arm_data$trans[j] else NA,
          EPOCH = epochs[j]
        )
    }
  }

  # Create a complete mapping of ELEMENT to ETCD
  etcd_mapping <- data.frame(
    ELEMENT = character(),
    ETCD = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(arms_data)) {
    arm <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm$epochs, ",")))
    etcds <- arm$etcd
    treatments <- treatments_list[[i]]
    
    treatment_index <- 1
    for (j in seq_along(epochs)) {
      if (epochs[j] == "TREATMENT") {
        element <- paste("TREATMENT", toupper(treatments[treatment_index]))
        treatment_index <- treatment_index + 1
      } else {
        element <- epochs[j]
      }
      
      etcd_mapping <- rbind(etcd_mapping, data.frame(ELEMENT = element, ETCD = etcds[j]))
    }
  }
  etcd_mapping <- distinct(etcd_mapping)

  # Merge te_rules with etcd_mapping
  te_df <- merge(te_rules, etcd_mapping, by = "ELEMENT", all = TRUE)

  # Process TE rules and sort by days in TEDUR (ISO8601 format)
  te_df <- te_df %>%
    mutate(
      STUDYID = study_id,
      DOMAIN = "TE",
      DurationDays = as.numeric(gsub("[^0-9]", "", TEDUR)) # Extract days from TEDUR in ISO8601 format
    ) %>%
    arrange(DurationDays) %>% # Sort by the numeric value of days
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
