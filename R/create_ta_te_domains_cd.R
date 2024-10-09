#' Generate TA and TE Datasets for Cross-Over Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID using the CROSS-OVER DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "CROSS-OVER DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `arm`, `epochs`, `etcd`, `elements`, `testrl`, `teenrl`, and `tedur`.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @import dplyr openxlsx lubridate
#'
#' @examples
#' study_id <- "CDXXX"
#' trial_design <- "CROSS-OVER DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "AB",
#'     arm = "A then B",
#'     epochs = "SCREENING,TREATMENT 1,WASHOUT,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,WASH,TRT2,F/U",
#'     elements = "Screening,Treatment A,Washout,Treatment B,Follow-up",
#'     testrl = "Informed consent,First dose of A,Last dose of A,First dose of B,Last dose of B",
#'     teenrl = "Randomization,Last dose of A,First dose of B,Last dose of B,Study completion",
#'     tedur = "P28D,P12W,P4W,P12W,P4W"
#'   ),
#'   list(
#'     armcd = "BA",
#'     arm = "B then A",
#'     epochs = "SCREENING,TREATMENT 1,WASHOUT,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,WASH,TRT2,F/U",
#'     elements = "Screening,Treatment B,Washout,Treatment A,Follow-up",
#'     testrl = "Informed consent,First dose of B,Last dose of B,First dose of A,Last dose of A",
#'     teenrl = "Randomization,Last dose of B,First dose of A,Last dose of A,Study completion",
#'     tedur = "P28D,P12W,P4W,P12W,P4W"
#'   )
#' )
#' 
#' result <- create_ta_te_domains_cd(study_id, trial_design, arms_data)
#' CDXXX_TA <- result$TA
#' CDXXX_TE <- result$TE
create_ta_te_domains_cd <- function(study_id, trial_design, arms_data, output_dir = getwd()) {
  # Validate inputs
  if (trial_design != "CROSS-OVER DESIGN") {
    stop("This function is customized only for 'CROSS-OVER DESIGN'.")
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
    arrange(match(ETCD, unique(ta_df$ETCD))) %>%
    mutate(DOMAIN = "TE",
           TESTRL = NA_character_,
           TEENRL = NA_character_,
           TEDUR = NA_character_)

  # Add TESTRL, TEENRL, and TEDUR to TE domain
  for (i in seq_along(arms_data)) {
    arm_data <- arms_data[[i]]
    etcd <- unlist(strsplit(arm_data$etcd, ","))
    testrl <- unlist(strsplit(arm_data$testrl, ","))
    teenrl <- unlist(strsplit(arm_data$teenrl, ","))
    tedur <- unlist(strsplit(arm_data$tedur, ","))

    for (j in seq_along(etcd)) {
      te_df <- te_df %>%
        mutate(
          TESTRL = ifelse(ETCD == etcd[j] & is.na(TESTRL), testrl[j], TESTRL),
          TEENRL = ifelse(ETCD == etcd[j] & is.na(TEENRL), teenrl[j], TEENRL),
          TEDUR = ifelse(ETCD == etcd[j] & is.na(TEDUR), tedur[j], TEDUR)
        )
    }
  }

  # Ensure correct column order
  te_df <- te_df %>% select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)

  # Save TA domain to Excel file
  ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
  wb_ta <- createWorkbook()
  addWorksheet(wb_ta, "TA")
  writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)

  # Save TE domain to Excel file
  te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
  wb_te <- createWorkbook()
  addWorksheet(wb_te, "TE")
  writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold"))
  saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

  return(list(TA = ta_df, TE = te_df))
}