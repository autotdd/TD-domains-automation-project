# Load necessary libraries
library(dplyr)
library(openxlsx)
library(testthat)

# Source the function (assuming it's in the same directory)
source("R/create_ta_te_domains_sd.R")

# Set up the study parameters
study_id <- "SGDE001"
trial_design <- "SINGLE GROUP DESIGN"

# Define the arms_data
arms_data <- list(
  list(
    armcd = "DOSE_ESC",
    arm = "Dose Escalation Arm",
    epochs = "SCREENING,TREATMENT_1,TREATMENT_2,TREATMENT_3,FOLLOW-UP",
    etcd = "SCRN,TRT1,TRT2,TRT3,F/U",
    elements = "Screening,Treatment Dose Level 1,Treatment Dose Level 2,Treatment Dose Level 3,Follow-up",
    testrl = "Informed consent,First dose of Dose Level 1,First dose of Dose Level 2,First dose of Dose Level 3,Last dose of study treatment",
    teenrl = "First dose of Dose Level 1,Last dose of Dose Level 1 or dose-limiting toxicity,Last dose of Dose Level 2 or dose-limiting toxicity,Last dose of Dose Level 3 or dose-limiting toxicity,30 days after last dose or resolution of all toxicities",
    tedur = "P28D,P14D,P14D,P14D,P30D"
  )
)

# Call the function
result <- create_ta_te_domains_sd(study_id, trial_design, arms_data)

# Extract the results
SGDE001_TA <- result$TA
SGDE001_TE <- result$TE

# Print TA Domain
cat("TA Domain:\n")
print(SGDE001_TA)

# Print TE Domain
cat("\nTE Domain:\n")
print(SGDE001_TE)

# Test assertions
test_that("TA domain is correctly generated", {
  expect_equal(nrow(SGDE001_TA), 5)  # 5 elements * 1 arm
  expect_equal(ncol(SGDE001_TA), 10)
  expect_equal(unique(SGDE001_TA$STUDYID), "SGDE001")
  expect_equal(unique(SGDE001_TA$DOMAIN), "TA")
  expect_equal(unique(SGDE001_TA$ARMCD), "DOSE_ESC")
  expect_equal(unique(SGDE001_TA$TAETORD), 1:5)
  expect_true(all(is.na(SGDE001_TA$TABRANCH)))
  expect_true(all(is.na(SGDE001_TA$TATRANS)))
})

test_that("TE domain is correctly generated", {
  expect_equal(nrow(SGDE001_TE), 5)  # 5 unique elements
  expect_equal(ncol(SGDE001_TE), 7)
  expect_equal(unique(SGDE001_TE$STUDYID), "SGDE001")
  expect_equal(unique(SGDE001_TE$DOMAIN), "TE")
  expect_equal(SGDE001_TE$ETCD, c("SCRN", "TRT1", "TRT2", "TRT3", "F/U"))
  expect_false(any(is.na(SGDE001_TE$TESTRL)))
  expect_false(any(is.na(SGDE001_TE$TEENRL)))
  expect_false(any(is.na(SGDE001_TE$TEDUR)))
})

# Check if Excel files are created
test_that("Excel files are created", {
  expect_true(file.exists(paste0(study_id, "_TA.xlsx")))
  expect_true(file.exists(paste0(study_id, "_TE.xlsx")))
})

cat("All tests passed successfully!\n")