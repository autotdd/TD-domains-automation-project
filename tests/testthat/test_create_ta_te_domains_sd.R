# Test code for SINGLE GROUP DESIGN

# Define study ID
study_id <- "STUDY001"

# Define arm data with epochs
arms_data <- list(
  list(
    armcd = "ARM1",
    epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
  )
)

# Define treatments dynamically as a list
treatments <- list("A", "B", "C")

# Define TE rules
te_rules <- data.frame(
  ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"),
  TESTRL = c("Informed consent", "First dose of study drug", "First dose of study drug", "First dose of study drug", "Start of follow-up period"),
  TEENRL = c("End of screening", "End of treatment period", "End of treatment period", "End of treatment period", "End of follow-up period"),
  TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D")
)

# Generate the TA and TE datasets using the provided function
ta_te_df <- create_ta_domain_sd(study_id, arms_data, treatments, te_rules)

# Print the generated TA and TE datasets
print("Generated TA domain:")
print(ta_te_df$TA)
print("Generated TE domain:")
print(ta_te_df$TE)

# Perform additional checks to validate the generated datasets

# Check if the TA domain has the correct number of rows
expected_ta_rows <- 5
actual_ta_rows <- nrow(ta_te_df$TA)
if (actual_ta_rows != expected_ta_rows) {
  stop(paste("TA domain row count mismatch: expected", expected_ta_rows, "but got", actual_ta_rows))
}

# Check if the TE domain has the correct number of rows
expected_te_rows <- 5
actual_te_rows <- nrow(ta_te_df$TE)
if (actual_te_rows != expected_te_rows) {
  stop(paste("TE domain row count mismatch: expected", expected_te_rows, "but got", actual_te_rows))
}

# Check if the TA domain contains the expected elements
expected_ta_elements <- c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP")
actual_ta_elements <- ta_te_df$TA$ELEMENT
if (!all(expected_ta_elements %in% actual_ta_elements)) {
  stop("TA domain elements mismatch")
}

# Check if the TE domain contains the expected elements
expected_te_elements <- c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP")
actual_te_elements <- ta_te_df$TE$ELEMENT
if (!all(expected_te_elements %in% actual_te_elements)) {
  stop("TE domain elements mismatch")
}

# Check if the TA domain columns are correctly populated
required_ta_columns <- c("STUDYID", "DOMAIN", "ARMCD", "ARM", "TAETORD", "ETCD", "ELEMENT", "EPOCH")
missing_ta_columns <- setdiff(required_ta_columns, colnames(ta_te_df$TA))
if (length(missing_ta_columns) > 0) {
  stop(paste("TA domain is missing columns:", paste(missing_ta_columns, collapse = ", ")))
}

# Check if the TE domain columns are correctly populated
required_te_columns <- c("STUDYID", "DOMAIN", "ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR")
missing_te_columns <- setdiff(required_te_columns, colnames(ta_te_df$TE))
if (length(missing_te_columns) > 0) {
  stop(paste("TE domain is missing columns:", paste(missing_te_columns, collapse = ", ")))
}

# Check if the STUDYID is correctly populated in both TA and TE domains
if (any(ta_te_df$TA$STUDYID != study_id) || any(ta_te_df$TE$STUDYID != study_id)) {
  stop("STUDYID mismatch in TA or TE domain")
}

print("All checks passed successfully.")
