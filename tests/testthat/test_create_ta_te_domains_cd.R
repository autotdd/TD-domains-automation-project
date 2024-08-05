# Test Code for create_ta_domain_cd
library(testthat)

test_create_ta_domain_cd <- function() {
  study_id <- "STUDY003"
  trial_design <- "CROSS-OVER DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
    ),
    list(
      armcd = "ARM3",
      epochs = "Screening, Treatment, Washout, Treatment, Washout, Treatment"
    )
  )
  treatments <- list(c("A", "B", "C"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT"),
    TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up", "End of washout"),
    TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study", "End of washout period"),
    TEDUR = c("P7D", "P14D", "P7D", "P21D", "P7D")
  )
  
  result <- create_ta_domain_cd(study_id, trial_design, arms_data, treatments, te_rules)
  
  ta_df <- result$TA
  te_df <- result$TE
  
  print("Generated TA domain:")
  print(ta_df)
  
  print("Generated TE domain:")
  print(te_df)
  
  expected_elements <- c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT")
  
  # Check if all expected elements are present in the TE domain
  missing_elements <- setdiff(expected_elements, te_df$ELEMENT)
  extra_elements <- setdiff(te_df$ELEMENT, expected_elements)
  
  if (length(missing_elements) > 0) {
    print("Missing elements in TE domain:")
    print(missing_elements)
  }
  
  if (length(extra_elements) > 0) {
    print("Extra elements in TE domain:")
    print(extra_elements)
  }
  
  stopifnot(length(missing_elements) == 0)
  stopifnot(length(extra_elements) == 0)
  
  print("All tests passed successfully.")
}

test_create_ta_domain_cd()
