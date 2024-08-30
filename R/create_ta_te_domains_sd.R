# File: R/create_ta_te_domains_sd.R

#' Generate TA and TE Datasets for Single Group Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID based on the SINGLE GROUP DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd` and `epochs`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @importFrom dplyr add_row distinct mutate select left_join
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @examples
#' \dontrun{
#' study_id <- "STUDY001"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
#'   )
#' )
#' treatments <- list("A", "B", "C") # Define the treatments dynamically as a list
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose of study drug A", "First dose of study drug B",
#'              "First dose of study drug C", "End of treatment"),
#'   TEENRL = c("End of screening", "End of treatment A", "End of treatment B",
#'              "End of treatment C", "End of follow-up period"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D")
#' )
#'
#' result <- create_ta_te_domains_sd(study_id, arms_data, treatments, te_rules)
#' print(result$TA)
#' print(result$TE)
#' }
create_ta_te_domains_sd <- function(study_id, arms_data, treatments, te_rules, output_dir = getwd()) {

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
    element_descriptions <- generate_elements_sd(epochs, treatments)
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

    # Check if the number of treatments matches the number of Treatment epochs
    treatment_epochs <- sum(grepl("TREATMENT", epochs, ignore.case = TRUE))
    if(treatment_epochs != length(treatments)) {
      stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
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
          TABRANCH = NA,
          TATRANS = NA,
          EPOCH = epochs[j]
        )
    }
  }

  # Create TE domain
  unique_elements <- ta_df %>%
    distinct(ELEMENT) %>%
    mutate(
      ETCD = paste0("ET", row_number()),
      DOMAIN = "TE"
    )

  te_df <- unique_elements %>%
    left_join(te_rules, by = "ELEMENT") %>%
    mutate(STUDYID = study_id) %>%
    select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)

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

# Helper function

#' Generate Element Descriptions for Single Group Design
#'
#' This function generates element descriptions from epochs with additional text for treatment
#' specifically for single group design trials.
#'
#' @param epochs A character vector of epochs.
#' @param treatments A list of treatments for the trial.
#' @return A character vector of element descriptions.
#' @keywords internal
generate_elements_sd <- function(epochs, treatments) {
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
