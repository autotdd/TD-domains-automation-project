# Load necessary libraries
library(dplyr)
library(openxlsx)
library(testthat)

# Source the function (assuming it's in the same directory)
source("R/create_ta_te_domains_fd.R")

# Set up the study parameters
study_id <- "FDABC"
trial_design <- "FACTORIAL DESIGN"

# Define the arms_data with treatments A, B, and C shuffled in each arm
arms_data <- list(
  list(
    armcd = "A1B1C1",
    arm = "Treatment A + Treatment B + Treatment C",
    epochs = "SCREENING,TREATMENT A,TREATMENT B,TREATMENT C,FOLLOW-UP",
    etcd = "SCRN,TRTA,TRTB,TRTC,F/U",
    elements = "Screening,Treatment A,Treatment B,Treatment C,Follow-up",
    testrl = "Informed consent,First dose of A,First dose of B,First dose of C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of A,Last dose of B,Last dose of C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A1B1C2",
    arm = "Treatment A + Treatment B + Placebo C",
    epochs = "SCREENING,TREATMENT A,TREATMENT B,PLACEBO C,FOLLOW-UP",
    etcd = "SCRN,TRTA,TRTB,PLBC,F/U",
    elements = "Screening,Treatment A,Treatment B,Placebo C,Follow-up",
    testrl = "Informed consent,First dose of A,First dose of B,First dose of placebo C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of A,Last dose of B,Last dose of placebo C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A1B2C1",
    arm = "Treatment A + Placebo B + Treatment C",
    epochs = "SCREENING,TREATMENT A,PLACEBO B,TREATMENT C,FOLLOW-UP",
    etcd = "SCRN,TRTA,PLBB,TRTC,F/U",
    elements = "Screening,Treatment A,Placebo B,Treatment C,Follow-up",
    testrl = "Informed consent,First dose of A,First dose of placebo B,First dose of C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of A,Last dose of placebo B,Last dose of C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A1B2C2",
    arm = "Treatment A + Placebo B + Placebo C",
    epochs = "SCREENING,TREATMENT A,PLACEBO B,PLACEBO C,FOLLOW-UP",
    etcd = "SCRN,TRTA,PLBB,PLBC,F/U",
    elements = "Screening,Treatment A,Placebo B,Placebo C,Follow-up",
    testrl = "Informed consent,First dose of A,First dose of placebo B,First dose of placebo C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of A,Last dose of placebo B,Last dose of placebo C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A2B1C1",
    arm = "Placebo A + Treatment B + Treatment C",
    epochs = "SCREENING,PLACEBO A,TREATMENT B,TREATMENT C,FOLLOW-UP",
    etcd = "SCRN,PLBA,TRTB,TRTC,F/U",
    elements = "Screening,Placebo A,Treatment B,Treatment C,Follow-up",
    testrl = "Informed consent,First dose of placebo A,First dose of B,First dose of C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of placebo A,Last dose of B,Last dose of C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A2B1C2",
    arm = "Placebo A + Treatment B + Placebo C",
    epochs = "SCREENING,PLACEBO A,TREATMENT B,PLACEBO C,FOLLOW-UP",
    etcd = "SCRN,PLBA,TRTB,PLBC,F/U",
    elements = "Screening,Placebo A,Treatment B,Placebo C,Follow-up",
    testrl = "Informed consent,First dose of placebo A,First dose of B,First dose of placebo C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of placebo A,Last dose of B,Last dose of placebo C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A2B2C1",
    arm = "Placebo A + Placebo B + Treatment C",
    epochs = "SCREENING,PLACEBO A,PLACEBO B,TREATMENT C,FOLLOW-UP",
    etcd = "SCRN,PLBA,PLBB,TRTC,F/U",
    elements = "Screening,Placebo A,Placebo B,Treatment C,Follow-up",
    testrl = "Informed consent,First dose of placebo A,First dose of placebo B,First dose of C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of placebo A,Last dose of placebo B,Last dose of C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  ),
  list(
    armcd = "A2B2C2",
    arm = "Placebo A + Placebo B + Placebo C",
    epochs = "SCREENING,PLACEBO A,PLACEBO B,PLACEBO C,FOLLOW-UP",
    etcd = "SCRN,PLBA,PLBB,PLBC,F/U",
    elements = "Screening,Placebo A,Placebo B,Placebo C,Follow-up",
    testrl = "Informed consent,First dose of placebo A,First dose of placebo B,First dose of placebo C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of placebo A,Last dose of placebo B,Last dose of placebo C,30 days after last dose",
    tedur = "P28D,P4W,P4W,P4W,P30D"
  )
)

# Call the function
result <- create_ta_te_domains_fd(study_id, trial_design, arms_data)

# Extract the results
FDABC_TA <- result$TA
FDABC_TE <- result$TE

# Print TA Domain
cat("TA Domain:\n")
print(FDABC_TA)

# Print TE Domain
cat("\nTE Domain:\n")
print(FDABC_TE)

# Test assertions
test_that("TA domain is correctly generated", {
  expect_equal(nrow(FDABC_TA), 40)  # 5 elements * 8 arms
  expect_equal(ncol(FDABC_TA), 10)
  expect_equal(unique(FDABC_TA$STUDYID), "FDABC")
  expect_equal(unique(FDABC_TA$DOMAIN), "TA")
  expect_equal(length(unique(FDABC_TA$ARMCD)), 8)
  expect_equal(unique(FDABC_TA$TAETORD), 1:5)
  expect_true(all(is.na(FDABC_TA$TABRANCH)))
  expect_true(all(is.na(FDABC_TA$TATRANS)))
})

test_that("TE domain is correctly generated", {
  expect_equal(nrow(FDABC_TE), 9)  # 9 unique elements (SCRN, TRTA, PLBA, TRTB, PLBB, TRTC, PLBC, F/U)
  expect_equal(ncol(FDABC_TE), 7)
  expect_equal(unique(FDABC_TE$STUDYID), "FDABC")
  expect_equal(unique(FDABC_TE$DOMAIN), "TE")
  expect_equal(FDABC_TE$ETCD, c("SCRN", "TRTA", "PLBA", "TRTB", "PLBB", "TRTC", "PLBC", "F/U"))
  expect_false(any(is.na(FDABC_TE$TESTRL)))
  expect_false(any(is.na(FDABC_TE$TEENRL)))
  expect_false(any(is.na(FDABC_TE$TEDUR)))
})

# Check if Excel files are created
test_that("Excel files are created", {
  expect_true(file.exists(paste0(study_id, "_TA.xlsx")))
  expect_true(file.exists(paste0(study_id, "_TE.xlsx")))
})

cat("All tests passed successfully!\n")