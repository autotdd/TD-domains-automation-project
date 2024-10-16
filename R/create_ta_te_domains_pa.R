#' Create TA and TE Domains for Parallel Arm Studies
#'
#' This function generates Trial Arms (TA) and Trial Elements (TE) domains for parallel arm studies,
#' including those with branches and transitions.
#'
#' @param study_id A string identifying the study.
#' @param trial_design A string specifying the trial design, either "PARALLEL DESIGN" or "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS".
#' @param arms_data A list of arm data, each containing armcd, arm, epochs, etcd, elements, testrl, teenrl, and tedur.
#' @param output_dir The directory where output files will be saved (default is current working directory).
#'
#' @return A list containing TA and TE data frames.
#'
#' @examples
#' # Example: PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS
#' study_id <- "STUDY002"
#' trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     arm = "Arm 1",
#'     epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,TRT2,F/U",
#'     elements = "Screening,Initial Treatment,Extended Treatment,Follow-up",
#'     testrl = "Informed consent,First dose of initial treatment,First dose of extended treatment,Last dose of study treatment",
#'     teenrl = "Randomization,Last dose of initial treatment,Last dose of extended treatment,30 days after last dose",
#'     tedur = "P28D,P12W,P12W,P30D",
#'     tabranch = "NA,BR1,NA,NA",
#'     tatrans = "NA,TR1,NA,NA"
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     arm = "Arm 2",
#'     epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,TRT2,F/U",
#'     elements = "Screening,Initial Treatment,Extended Treatment,Follow-up",
#'     testrl = "Informed consent,First dose of initial treatment,First dose of extended treatment,Last dose of study treatment",
#'     teenrl = "Randomization,Last dose of initial treatment,Last dose of extended treatment,30 days after last dose",
#'     tedur = "P28D,P12W,P12W,P30D",
#'     tabranch = "NA,BR2,NA,NA",
#'     tatrans = "NA,TR2,NA,NA"
#'   )
#' )
#' result <- create_ta_te_domains_pa(study_id, trial_design, arms_data)
#'
#' @export
create_ta_te_domains_pa <- function(study_id, trial_design, arms_data, output_dir = getwd()) {
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

  # Initialize TE domain data frame
  te_df <- data.frame(
    STUDYID = character(),
    DOMAIN = character(),
    ETCD = character(),
    ELEMENT = character(),
    TESTRL = character(),
    TEENRL = character(),
    TEDUR = character(),
    stringsAsFactors = FALSE
  )

  # Populate TA and TE domain data frames based on input rows
  for (i in seq_along(arms_data)) {
    arm_data <- arms_data[[i]]
    epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
    elements <- unlist(strsplit(arm_data$elements, ","))
    etcd <- unlist(strsplit(arm_data$etcd, ","))
    testrl <- unlist(strsplit(arm_data$testrl, ","))
    teenrl <- unlist(strsplit(arm_data$teenrl, ","))
    tedur <- unlist(strsplit(arm_data$tedur, ","))
    
    if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS") {
      tabranch <- unlist(strsplit(arm_data$tabranch, ","))
      tatrans <- unlist(strsplit(arm_data$tatrans, ","))
    } else {
      tabranch <- rep(NA, length(epochs))
      tatrans <- rep(NA, length(epochs))
    }
    
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
          TABRANCH = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS") tabranch[j] else NA,
          TATRANS = if (trial_design == "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS") tatrans[j] else NA,
          EPOCH = epochs[j]
        )
      
      # Add to TE domain if not already present
      if (!etcd[j] %in% te_df$ETCD) {
        te_df <- te_df %>%
          add_row(
            STUDYID = study_id,
            DOMAIN = "TE",
            ETCD = etcd[j],
            ELEMENT = elements[j],
            TESTRL = testrl[j],
            TEENRL = teenrl[j],
            TEDUR = tedur[j]
          )
      }
    }
  }

  # Sort TE domain based on the order in TA domain
  te_df <- te_df %>%
    left_join(ta_df %>% select(ETCD, TAETORD) %>% distinct(), by = "ETCD") %>%
    arrange(TAETORD) 

  # Remove duplicate records from TE domain
  te_df <- te_df %>%
    group_by(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(TAETORD) %>%
    select(-TAETORD)

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
