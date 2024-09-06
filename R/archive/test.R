
# Load necessary libraries
library(pdftools)
library(dplyr)
library(stringr)
library(openxlsx)

# Source both files
source("R/create_ti_domain.R")
source("R/extract_ti_domain.R")

# Set up common parameters
study_id <- "STUDY001"
pdf_path <- "/Users/siddharthlokineni/Desktop/autotdd/TD-domains-automation-project/592_Protocol.pdf"
output_dir <- "/Users/siddharthlokineni/Desktop/autotdd/TD-domains-automation-project/"

# Run create_ti_domain function
create_result <- create_ti_domain(
  study_id = study_id,
  method = "pdf",
  pdf_path = pdf_path,
  incl_range = 51:54,
  excl_range = 54:58,
  incl_section = "4.1.1",
  excl_section = "4.1.2",
  end_section = "4.2",
  output_dir = output_dir
)

# Run extract_ti_domain function
extract_result <- extract_ti_domain(
  study_id = study_id,
  pdf_path = pdf_path,
  incl_range = 51:54,
  excl_range = 54:58,
  incl_section = "4.1.1",
  excl_section = "4.1.2",
  end_section = "4.2"
)

# Print results
cat("Results from create_ti_domain:\n")
print(create_result$ti_domain)
cat("\n\nResults from extract_ti_domain:\n")
print(extract_result)

# Compare the number of criteria extracted
cat("\n\nNumber of criteria extracted:\n")
cat("create_ti_domain: ", nrow(create_result$ti_domain), "\n")
cat("extract_ti_domain: ", nrow(extract_result), "\n")

# Print debug info from create_ti_domain
cat("\n\nDebug info from create_ti_domain:\n")
cat(create_result$debug_info)
