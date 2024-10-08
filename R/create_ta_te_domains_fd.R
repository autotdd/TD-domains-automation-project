#' Generate TA and TE Datasets for Factorial Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the FACTORIAL DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "FACTORIAL DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `arm`, `epochs`, `etcd`, and `elements`.
#' @param treatments A list of treatments for each arm. Each element should be a vector of treatments corresponding to the arm.
#' @param te_rules A data frame containing TE rules with columns: ETCD, TESTRL, TEENRL, TEDUR (in ISO 8601 format).
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @import dplyr openxlsx lubridate
#'
#' @examples
#' study_id <- "FDXXX"
#' trial_design <- "FACTORIAL DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "A1B1",
#'     arm = "Factor A Level 1 + Factor B Level 1",
#'     epochs = "SCREENING,TREATMENT,FOLLOW-UP",
#'     etcd = "SCRN,TRT,F/U",
#'     elements = "Screening,Treatment A1B1,Follow-up"
#'   ),
#'   list(
#'     armcd = "A1B2",
#'     arm = "Factor A Level 1 + Factor B Level 2",
#'     epochs = "SCREENING,TREATMENT,FOLLOW-UP",
#'     etcd = "SCRN,TRT,F/U",
#'     elements = "Screening,Treatment A1B2,Follow-up"
#'   ),
#'   list(
#'     armcd = "A2B1",
#'     arm = "Factor A Level 2 + Factor B Level 1",
#'     epochs = "SCREENING,TREATMENT,FOLLOW-UP",
#'     etcd = "SCRN,TRT,F/U",
#'     elements = "Screening,Treatment A2B1,Follow-up"
#'   ),
#'   list(
#'     armcd = "A2B2",
#'     arm = "Factor A Level 2 + Factor B Level 2",
#'     epochs = "SCREENING,TREATMENT,FOLLOW-UP",
#'     etcd = "SCRN,TRT,F/U",
#'     elements = "Screening,Treatment A2B2,Follow-up"
#'   )
#' )
#' treatments <- list(
#'   c("Treatment A1B1"),
#'   c("Treatment A1B2"),
#'   c("Treatment A2B1"),
#'   c("Treatment A2B2")
#' )
#' te_rules <- data.frame(
#'   ETCD = c("SCRN", "TRT", "F/U"),
#'   TESTRL = c("Informed consent", "First dose of study treatment", "Last dose of study treatment"),
#'   TEENRL = c("Randomization", "Last dose of study treatment", "30 days after last dose"),
#'   TEDUR = c("P28D", "P12W", "P30D")
#' )
#' 
#' result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules)
#' FDXXX_TA <- result$TA
#' FDXXX_TE <- result$TE
create_ta_te_domains_fd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
  # Validate inputs
  if (trial_design != "FACTORIAL DESIGN") {
    stop("This function is customized only for 'FACTORIAL DESIGN'.")
  }

  # Check for mismatched epochs and treatments
  for (i in seq_along(arms_data)) {
    epochs <- unlist(strsplit(arms_data[[i]]$epochs, ","))
    treatment_epochs <- sum(grepl("TREATMENT", epochs, ignore.case = TRUE))
    if (treatment_epochs != length(treatments[[i]])) {
      stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
    }
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
    elements <- unlist(strsplit(arm_data$elements, ","))
    etcd <- unlist(strsplit(arm_data$etcd, ","))
    num_elements <- length(elements)

    armcd <- arm_data$armcd
    arm <- arm_data$arm

    for (j in seq_along(elements)) {
      ta_df <- ta_df %>%
        add_row(
          STUDYID = study_id,
          DOMAIN = "TA",
          ARMCD = armcd,
          ARM = arm,
          TAETORD = j,
          ETCD = etcd[j],
          ELEMENT = elements[j],
          TABRANCH = NA,
          TATRANS = NA,
          EPOCH = epochs[j]
        )
    }
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
    select(STUDYID, DOMAIN, everything())  # Reorder columns to put DOMAIN after STUDYID

  # Save TA and TE domain to Excel files
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)

  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}