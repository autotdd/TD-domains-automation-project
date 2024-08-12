# File: R/create_trial_domains.R

#' Create Trial Domains
#'
#' This function creates multiple trial domains (TA, TE, TI, TS) for a given study.
#' It combines the functionality of individual domain creation functions into a single workflow.
#'
#' @param study_id A character string representing the Study ID.
#' @param nct_id A character string representing the NCT ID.
#' @param trial_design A character string representing the trial design. 
#'        Should be one of "PARALLEL DESIGN", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS",
#'        "CROSS-OVER DESIGN", "SINGLE GROUP DESIGN", or "FACTORIAL DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing 
#'        `armcd`, `epochs`, and optionally `branch` and `trans` for designs that support them.
#' @param treatments_list A list of treatments corresponding to each arm.
#' @param te_rules A data frame containing TE rules with columns: ELEMENT, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing data frames for each created domain (TA, TE, TI, TS).
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' study_id <- "STUDY001"
#' nct_id <- "NCT00000419"
#' trial_design <- "PARALLEL DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "ARM1",
#'     epochs = "Screening,Treatment,Follow-Up"
#'   ),
#'   list(
#'     armcd = "ARM2",
#'     epochs = "Screening,Treatment,Follow-Up"
#'   )
#' )
#' treatments_list <- list(
#'   c("A"),
#'   c("B")
#' )
#' te_rules <- data.frame(
#'   ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "FOLLOW-UP"),
#'   TESTRL = c("Informed consent", "First dose A", "First dose B", "End of treatment"),
#'   TEENRL = c("End of screening", "End of A", "End of B", "End of follow-up"),
#'   TEDUR = c("P7D", "P14D", "P14D", "P21D")
#' )
#' 
#' domains <- create_trial_domains(study_id, nct_id, trial_design, arms_data, treatments_list, te_rules)
#' print(names(domains))
#' }
create_trial_domains <- function(study_id, nct_id, trial_design, arms_data, treatments_list, te_rules, output_dir = getwd()) {
  # Validate input
  valid_designs <- c("PARALLEL DESIGN", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS",
                     "CROSS-OVER DESIGN", "SINGLE GROUP DESIGN", "FACTORIAL DESIGN")
  if (!trial_design %in% valid_designs) {
    stop("Invalid trial design. Must be one of: ", paste(valid_designs, collapse = ", "))
  }
  
  # Create TA and TE domains
  ta_te_result <- switch(trial_design,
                         "PARALLEL DESIGN" = ,
                         "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS" = create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir),
                         "CROSS-OVER DESIGN" = create_ta_te_domains_cd(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir),
                         "SINGLE GROUP DESIGN" = create_ta_te_domains_sd(study_id, arms_data, treatments_list, te_rules, output_dir),
                         "FACTORIAL DESIGN" = create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments_list, te_rules, output_dir)
  )
  
  # Create TI domain
  ti_domain <- create_ti_domain(nct_id, study_id, method = "api", output_dir = output_dir)
  
  # Create TS domain
  ts_domain <- create_ts_domain(nct_id, study_id, output_dir)
  
  # Combine all domains into a list
  domains <- list(
    TA = ta_te_result$TA,
    TE = ta_te_result$TE,
    TI = ti_domain,
    TS = ts_domain
  )
  
  # Print summary of created domains
  cat("Created domains:\n")
  for (domain in names(domains)) {
    cat(sprintf("%s: %d rows\n", domain, nrow(domains[[domain]])))
  }
  
  return(domains)
}