# File: R/create_ta_te_domains_sd.R

#' Generate TA and TE Datasets for Single Group Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the SINGLE GROUP DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "SINGLE GROUP DESIGN".
#' @param arms_data A list containing a single arm data. The arm data should be a list containing `armcd`, `arm`, `epochs`, `etcd`, and `elements`.
#' @param treatments A list containing a single vector of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ETCD, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @import dplyr openxlsx lubridate
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @importFrom stringr str_to_upper
#'
#' @examples
#' # Example: Dose Escalation Study
#' study_id <- "SGDE001"
#' trial_design <- "SINGLE GROUP DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "DOSE_ESC",
#'     arm = "Dose Escalation Arm",
#'     epochs = "SCREENING,TREATMENT_1,TREATMENT_2,TREATMENT_3,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,TRT2,TRT3,F/U",
#'     elements = "Screening,Treatment Dose Level 1,Treatment Dose Level 2,Treatment Dose Level 3,Follow-up"
#'   )
#' )
#' treatments <- list(c("Dose Level 1", "Dose Level 2", "Dose Level 3"))
#' te_rules <- data.frame(
#'   ETCD = c("SCRN", "TRT1", "TRT2", "TRT3", "F/U"),
#'   TESTRL = c("Informed consent", 
#'              "First dose of Dose Level 1", 
#'              "First dose of Dose Level 2",
#'              "First dose of Dose Level 3",
#'              "Last dose of study treatment"),
#'   TEENRL = c("First dose of Dose Level 1", 
#'              "Last dose of Dose Level 1 or dose-limiting toxicity", 
#'              "Last dose of Dose Level 2 or dose-limiting toxicity",
#'              "Last dose of Dose Level 3 or dose-limiting toxicity",
#'              "30 days after last dose or resolution of all toxicities"),
#'   TEDUR = c("P28D", "P14D", "P14D", "P14D", "P30D")
#' )
#'
#' result <- create_ta_te_domains_sd(study_id, trial_design, arms_data, treatments, te_rules)
#' SGDE001_TA <- result$TA
#' SGDE001_TE <- result$TE
create_ta_te_domains_sd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
  # Validate inputs
  if (trial_design != "SINGLE GROUP DESIGN") {
    stop("This function is customized only for 'SINGLE GROUP DESIGN'.")
  }

  if (length(arms_data) != 1) {
    stop("Single Group Design should have only one arm.")
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
  arm_data <- arms_data[[1]]
  epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
  etcd <- unlist(strsplit(arm_data$etcd, ","))
  elements <- unlist(strsplit(arm_data$elements, ","))
  num_elements <- length(elements)

  armcd <- arm_data$armcd
  arm <- arm_data$arm

  if(length(elements) != num_elements || length(epochs) != num_elements || length(etcd) != num_elements) {
    stop("Mismatch in the number of elements, epochs, or ETCDs")
  }

  treatment_epochs <- sum(grepl("TREATMENT", epochs, ignore.case = TRUE))
  if(treatment_epochs != length(treatments[[1]])) {
    stop("Mismatch between number of treatments and treatment epochs")
  }

  for (j in seq_along(elements)) {
    ta_df <- rbind(ta_df, data.frame(
      STUDYID = study_id,
      DOMAIN = "TA",
      ARMCD = armcd,
      ARM = arm,
      TAETORD = j,
      ETCD = etcd[j],
      ELEMENT = elements[j],
      TABRANCH = NA,
      TATRANS = NA,
      EPOCH = epochs[j],
      stringsAsFactors = FALSE
    ))
  }

  # Create TE domain
  te_df <- ta_df %>%
    distinct(STUDYID, ETCD, ELEMENT) %>%
    left_join(te_rules, by = "ETCD") %>%
    mutate(
      DOMAIN = "TE",
      TESTRL = coalesce(TESTRL, ""),
      TEENRL = coalesce(TEENRL, ""),
      TEDUR = coalesce(TEDUR, NA_character_)
    ) %>%
    select(STUDYID, DOMAIN, everything())

  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb <- createWorkbook()
  addWorksheet(wb, "TA")
  writeData(wb, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb, ta_output_file, overwrite = TRUE)

  # Save TE domain to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}