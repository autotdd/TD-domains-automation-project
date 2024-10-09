# Load necessary libraries
library(dplyr)
library(openxlsx)
library(testthat)

# Source the function (assuming it's in the same directory)
source("R/create_ta_te_domains_pa.R")

# Set up the study parameters
study_id <- "PABC001"
trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"

# Define the arms_data
arms_data <- list(
  list(
    armcd = "ARM1",
    arm = "Arm 1: Standard Dose",
    epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
    etcd = "SCRN,TRT1,TRT2,F/U",
    elements = "Screening,Initial Treatment,Extended Treatment,Follow-up",
    testrl = "Informed consent,First dose of standard treatment,First dose of extended treatment,Last dose of study treatment",
    teenrl = "Randomization,Last dose of initial treatment or progression,Last dose of extended treatment or progression,30 days after last dose",
    tedur = "P28D,P12W,P12W,P30D",
    tabranch = "NA,BR1,NA,NA",
    tatrans = "NA,TR1,NA,NA"
  ),
  list(
    armcd = "ARM2",
    arm = "Arm 2: Dose Escalation",
    epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
    etcd = "SCRN,TRT1,TRT2,F/U",
    elements = "Screening,Initial Treatment,Escalated Treatment,Follow-up",
    testrl = "Informed consent,First dose of initial treatment,First dose of escalated treatment,Last dose of study treatment",
    teenrl = "Randomization,Last dose of initial treatment,Last dose of escalated treatment or progression,30 days after last dose",
    tedur = "P28D,P12W,P12W,P30D",
    tabranch = "NA,BR2,NA,NA",
    tatrans = "NA,TR2,NA,NA"
  )
)

# Call the function
result <- create_ta_te_domains_pa(study_id, trial_design, arms_data)

# Extract the results
PABC001_TA <- result$TA
PABC001_TE <- result$TE

# Print TA Domain
cat("TA Domain:\n")
print(PABC001_TA)

# Print TE Domain
cat("\nTE Domain:\n")
print(PABC001_TE)

# Test assertions
test_that("TA domain is correctly generated", {
  expect_equal(nrow(PABC001_TA), 8)  # 4 elements * 2 arms
  expect_equal(ncol(PABC001_TA), 10)
  expect_equal(unique(PABC001_TA$STUDYID), "PABC001")
  expect_equal(unique(PABC001_TA$DOMAIN), "TA")
  expect_equal(unique(PABC001_TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(PABC001_TA$TAETORD), 1:4)
  expect_false(all(is.na(PABC001_TA$TABRANCH)))
  expect_false(all(is.na(PABC001_TA$TATRANS)))
})

test_that("TE domain is correctly generated", {
  expect_equal(nrow(PABC001_TE), 5)  # 5 unique elements (SCRN, TRT1, TRT2, F/U, and one extra for the different TRT2 in ARM2)
  expect_equal(ncol(PABC001_TE), 7)
  expect_equal(unique(PABC001_TE$STUDYID), "PABC001")
  expect_equal(unique(PABC001_TE$DOMAIN), "TE")
  expect_equal(PABC001_TE$ETCD, c("SCRN", "TRT1", "TRT2", "TRT2", "F/U"))
  expect_false(any(is.na(PABC001_TE$TESTRL)))
  expect_false(any(is.na(PABC001_TE$TEENRL)))
  expect_false(any(is.na(PABC001_TE$TEDUR)))
})

# Check if Excel files are created
test_that("Excel files are created", {
  expect_true(file.exists(paste0(study_id, "_TA.xlsx")))
  expect_true(file.exists(paste0(study_id, "_TE.xlsx")))
})

cat("All tests passed successfully!\n")