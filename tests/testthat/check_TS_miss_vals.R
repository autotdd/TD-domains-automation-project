
# Load required libraries
library(autoTDD)
library(dplyr)

# TS Domain Quality Check Function
ts_domain_quality_check <- function(ts_data, print_results = TRUE) {
  # Identify rows with missing TSVAL
  missing_vals <- ts_data[is.na(ts_data$TSVAL) | ts_data$TSVAL == "", ]

  if (nrow(missing_vals) == 0) {
    if (print_results) {
      cat("No missing TSVAL values found.\n")
    }
    return(data.frame())
  } else {
    if (print_results) {
      cat("Found", nrow(missing_vals), "row(s) with missing TSVAL values:\n")
      print(missing_vals[, c("TSPARMCD", "TSPARM", "TSVAL")])
      cat("\nPercentage of missing values:", round(nrow(missing_vals) / nrow(ts_data) * 100, 2), "%\n")
    }
    return(missing_vals)
  }
}

# Run create_ts_domain function
nct_ids <- "NCT05634499"
study_id <- "TEST001"
result <- create_ts_domain(nct_ids, study_id, debug = TRUE)

# Run the quality check
cat("\n--- Running TS Domain Quality Check ---\n")
missing_data <- ts_domain_quality_check(result)

# Save the missing data to a CSV file
if (nrow(missing_data) > 0) {
  write.csv(missing_data, file = "missing_ts_values.csv", row.names = FALSE)
  cat("Missing data has been saved to 'missing_ts_values.csv'\n")
}

# Check for specific important parameters
important_params <- c("TITLE", "TPHASE", "STYPE", "SSTDTC", "SENDTC", "SPONSOR", "TRT", "OBJPRIM", "OBJSEC")
missing_important <- missing_data[missing_data$TSPARMCD %in% important_params, ]

if (nrow(missing_important) > 0) {
  cat("\nWarning: Missing values for important parameters:\n")
  print(missing_important[, c("TSPARMCD", "TSPARM")])
} else {
  cat("\nAll important parameters have values.\n")
}

# Check for parameters with all missing values
all_missing <- names(which(sapply(result, function(x) all(is.na(x) | x == ""))))
if (length(all_missing) > 0) {
  cat("\nParameters with all values missing:\n")
  print(all_missing)
}

# Additional analysis: Summary of non-missing values
cat("\n--- Summary of Non-Missing Values ---\n")
non_missing_summary <- result %>%
  summarise(across(everything(), ~sum(!is.na(.) & . != ""))) %>%
  gather(key = "Parameter", value = "Non_Missing_Count") %>%
  arrange(desc(Non_Missing_Count))

print(non_missing_summary)

# Check for unexpected values in specific fields
cat("\n--- Checking for Unexpected Values ---\n")

check_unexpected <- function(field, expected_values) {
  unexpected <- result[[field]][!result[[field]] %in% expected_values & !is.na(result[[field]]) & result[[field]] != ""]
  if (length(unexpected) > 0) {
    cat("Unexpected values in", field, ":", paste(unexpected, collapse = ", "), "\n")
  } else {
    cat("No unexpected values in", field, "\n")
  }
}

check_unexpected("TPHASE", c("I", "II", "III", "IV", "I/II", "II/III", "Early I", "N/A"))
check_unexpected("STYPE", c("INTERVENTIONAL", "OBSERVATIONAL", "EXPANDED_ACCESS"))
check_unexpected("ONGOSIND", c("Y", "N"))

cat("\n--- Analysis Complete ---\n")
