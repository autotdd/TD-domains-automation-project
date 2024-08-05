test_create_ta_domain_fd <- function() {
  study_id <- "STUDY004"
  trial_design <- "FACTORIAL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM3",
      epochs = "Screening,Treatment,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM4",
      epochs = "Screening,Treatment,Treatment,Follow-Up"
    )
  )
  treatments <- list(
    c("A", "B"), # Treatments for ARM1
    c("Placebo B", "A"), # Treatments for ARM2
    c("Placebo A", "B"), # Treatments for ARM3
    c("Placebo A", "Placebo B")  # Treatments for ARM4
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP", "TREATMENT D", "TREATMENT E", 
                "TREATMENT F", "TREATMENT G", "TREATMENT H", "TREATMENT I"),
    TESTRL = c("Informed consent", "First dose of study drug", "End of treatment", "End of follow-up",
               "First dose of study drug", "End of treatment", "End of follow-up", "First dose of study drug",
               "End of treatment", "End of follow-up", "First dose of study drug"),
    TEENRL = c("End of screening", "End of treatment period", "End of follow-up period", "End of study",
               "End of treatment period", "End of follow-up period", "End of study", "End of treatment period",
               "End of follow-up period", "End of study", "End of treatment period"),
    TEDUR = c("P7D", "P14D", "P7D", "P21D", "P14D", "P7D", "P21D", "P14D", "P7D", "P21D", "P14D")
  )
  
  # Run the function to generate TA and TE domains
  result <- create_ta_domain_fd(study_id, trial_design, arms_data, treatments, te_rules)
  
  # Extract the TA and TE data frames
  ta_df <- result$TA
  te_df <- result$TE
  
  # Print the generated TA and TE data frames
  print("Generated TA domain:")
  print(ta_df)
  
  print("Generated TE domain:")
  print(te_df)
  
  # Basic validation of the TA domain
  stopifnot(nrow(ta_df) == 16) # Expecting 16 rows (4 elements per arm * 4 arms)
  stopifnot(ncol(ta_df) == 10) # Expecting 10 columns
  
  # Basic validation of the TE domain
  stopifnot(nrow(te_df) == 6) # Expecting 6 unique elements
  stopifnot(ncol(te_df) == 7)  # Expecting 7 columns
  
  # Check if the STUDYID is correctly populated in TA and TE domains
  stopifnot(all(ta_df$STUDYID == study_id))
  stopifnot(all(te_df$STUDYID == study_id))
  
  # Check if the DOMAIN is correctly populated in TA and TE domains
  stopifnot(all(ta_df$DOMAIN == "TA"))
  stopifnot(all(te_df$DOMAIN == "TE"))
  
  # Check if ETCD values are unique in TA and TE domains
  stopifnot(length(unique(ta_df$ETCD)) == nrow(ta_df))
  stopifnot(length(unique(te_df$ETCD)) == nrow(te_df))
  
  # Ensure TE domain has correctly mapped rules
  expected_elements <- c("SCREENING", "TREATMENT A", "TREATMENT B", "FOLLOW-UP", "TREATMENT Placebo B", "TREATMENT Placebo A")
  stopifnot(all(te_df$ELEMENT %in% expected_elements))
  
  cat("All tests passed successfully.\n")
}

# Run the test function
test_create_ta_domain_fd()
