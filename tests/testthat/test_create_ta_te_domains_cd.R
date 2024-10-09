# File: tests/testthat/test-create_ta_te_domains_cd.R

# Load necessary libraries
library(dplyr)
library(openxlsx)

# Source the function (assuming it's in the same directory)
source("R/create_ta_te_domains_cd.R")

# Set up the study parameters
study_id <- "CDABC"
trial_design <- "CROSS-OVER DESIGN"

# Define the arms_data for a 3x3 Latin square design
arms_data <- list(
  list(
    armcd = "ABC",
    arm = "A then B then C",
    epochs = "SCREENING,TREATMENT 1,WASHOUT 1,TREATMENT 2,WASHOUT 2,TREATMENT 3,FOLLOW-UP",
    etcd = "SCRN,TRT1,WASH1,TRT2,WASH2,TRT3,F/U",
    elements = "Screening,Treatment A,Washout 1,Treatment B,Washout 2,Treatment C,Follow-up",
    testrl = "Informed consent,First dose of A,Start of washout 1,First dose of B,Start of washout 2,First dose of C,Last dose of study treatment",
    teenrl = "Randomization,Last dose of A,End of washout 1,Last dose of B,End of washout 2,Last dose of C,End of follow-up",
    tedur = "P28D,P4W,P2W,P4W,P2W,P4W,P4W"
  ),
  list(
    armcd = "BCA",
    arm = "B then C then A",
    epochs = "SCREENING,TREATMENT 1,WASHOUT 1,TREATMENT 2,WASHOUT 2,TREATMENT 3,FOLLOW-UP",
    etcd = "SCRN,TRT1,WASH1,TRT2,WASH2,TRT3,F/U",
    elements = "Screening,Treatment B,Washout 1,Treatment C,Washout 2,Treatment A,Follow-up",
    testrl = "Informed consent,First dose of B,Start of washout 1,First dose of C,Start of washout 2,First dose of A,Last dose of study treatment",
    teenrl = "Randomization,Last dose of B,End of washout 1,Last dose of C,End of washout 2,Last dose of A,End of follow-up",
    tedur = "P28D,P4W,P2W,P4W,P2W,P4W,P4W"
  ),
  list(
    armcd = "CAB",
    arm = "C then A then B",
    epochs = "SCREENING,TREATMENT 1,WASHOUT 1,TREATMENT 2,WASHOUT 2,TREATMENT 3,FOLLOW-UP",
    etcd = "SCRN,TRT1,WASH1,TRT2,WASH2,TRT3,F/U",
    elements = "Screening,Treatment C,Washout 1,Treatment A,Washout 2,Treatment B,Follow-up",
    testrl = "Informed consent,First dose of C,Start of washout 1,First dose of A,Start of washout 2,First dose of B,Last dose of study treatment",
    teenrl = "Randomization,Last dose of C,End of washout 1,Last dose of A,End of washout 2,Last dose of B,End of follow-up",
    tedur = "P28D,P4W,P2W,P4W,P2W,P4W,P4W"
  )
)

# Call the function
result <- create_ta_te_domains_cd(study_id, trial_design, arms_data)

# Extract the results
CDABC_TA <- result$TA
CDABC_TE <- result$TE

# Print TA Domain
cat("TA Domain:\n")
print(CDABC_TA)

# Print TE Domain
cat("\nTE Domain:\n")
print(CDABC_TE)

# Test assertions
library(testthat)

test_that("TA domain is correctly generated", {
  expect_equal(nrow(CDABC_TA), 21)  # 7 elements * 3 arms
  expect_equal(ncol(CDABC_TA), 10)
  expect_equal(unique(CDABC_TA$STUDYID), "CDABC")
  expect_equal(unique(CDABC_TA$DOMAIN), "TA")
  expect_equal(unique(CDABC_TA$ARMCD), c("ABC", "BCA", "CAB"))
  expect_equal(unique(CDABC_TA$TAETORD), 1:7)
  expect_true(all(is.na(CDABC_TA$TABRANCH)))
  expect_true(all(is.na(CDABC_TA$TATRANS)))
})

test_that("TE domain is correctly generated", {
  expect_equal(nrow(CDABC_TE), 7)  # 7 unique elements
  expect_equal(ncol(CDABC_TE), 7)
  expect_equal(unique(CDABC_TE$STUDYID), "CDABC")
  expect_equal(unique(CDABC_TE$DOMAIN), "TE")
  expect_equal(CDABC_TE$ETCD, c("SCRN", "TRT1", "WASH1", "TRT2", "WASH2", "TRT3", "F/U"))
  expect_false(any(is.na(CDABC_TE$TESTRL)))
  expect_false(any(is.na(CDABC_TE$TEENRL)))
  expect_false(any(is.na(CDABC_TE$TEDUR)))
})

# Check if Excel files are created
test_that("Excel files are created", {
  expect_true(file.exists(paste0(study_id, "_TA.xlsx")))
  expect_true(file.exists(paste0(study_id, "_TE.xlsx")))
})

cat("All tests passed successfully!\n")