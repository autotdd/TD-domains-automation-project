library(testthat)
library(dplyr)

test_that("create_ta_te_domains_sd produces correct output for a single group design", {
  study_id <- "STUDY001"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
    )
  )
  treatments <- list("A", "B", "C")
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose of study drug A", "First dose of study drug B",
               "First dose of study drug C", "End of treatment"),
    TEENRL = c("End of screening", "End of treatment A", "End of treatment B",
               "End of treatment C", "End of follow-up period"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_sd(study_id, arms_data, treatments, te_rules)

  # Check overall structure
  expect_type(result, "list")
  expect_named(result, c("TA", "TE"))
  expect_s3_class(result$TA, "data.frame")
  expect_s3_class(result$TE, "data.frame")

  # Check TA domain
  expect_equal(nrow(result$TA), 5) # 5 epochs * 1 arm
  expect_equal(ncol(result$TA), 10) # Ensure all required variables are present
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), "ARM1")
  expect_equal(unique(result$TA$ARM), "ARM1")
  expect_equal(unique(result$TA$TAETORD), 1:5)
  expect_equal(result$TA$ETCD, paste0("ET", 1:5))
  expect_equal(result$TA$ELEMENT, c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "FOLLOW-UP"))
  expect_true(all(is.na(result$TA$TABRANCH)))
  expect_true(all(is.na(result$TA$TATRANS)))
  expect_equal(result$TA$EPOCH, c("SCREENING", "TREATMENT", "TREATMENT", "TREATMENT", "FOLLOW-UP"))

  # Check TE domain
  expect_equal(nrow(result$TE), 5) # 5 unique elements
  expect_equal(ncol(result$TE), 7) # Ensure all required variables are present
  expect_equal(unique(result$TE$STUDYID), study_id)
  expect_equal(unique(result$TE$DOMAIN), "TE")
  expect_equal(result$TE$ETCD, paste0("ET", 1:5))
  expect_equal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_sd handles errors correctly", {
  study_id <- "STUDY002"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments <- list("A", "B") # More treatments than Treatment epochs
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose of study drug", "End of treatment"),
    TEENRL = c("End of screening", "End of treatment", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  expect_error(
    create_ta_te_domains_sd(study_id, arms_data, treatments, te_rules),
    "Mismatch between number of treatments and treatment epochs for arm 1"
  )
})

test_that("create_ta_te_domains_sd creates separate Excel files for TA and TE", {
  study_id <- "STUDY003"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments <- list("A")
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose of study drug", "End of treatment"),
    TEENRL = c("End of screening", "End of treatment", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  # Create a temporary directory for output files
  temp_dir <- tempdir()

  result <- create_ta_te_domains_sd(study_id, arms_data, treatments, te_rules, output_dir = temp_dir)

  # Check if both TA and TE Excel files are created
  expect_true(file.exists(file.path(temp_dir, paste0(study_id, "_TA.xlsx"))))
  expect_true(file.exists(file.path(temp_dir, paste0(study_id, "_TE.xlsx"))))

  # Read the Excel files
  ta_excel <- readxl::read_excel(file.path(temp_dir, paste0(study_id, "_TA.xlsx")))
  te_excel <- readxl::read_excel(file.path(temp_dir, paste0(study_id, "_TE.xlsx")))

  # Check that the Excel files contain the correct data
  expect_equal(nrow(ta_excel), nrow(result$TA))
  expect_equal(nrow(te_excel), nrow(result$TE))

  expect_equal(colnames(ta_excel), colnames(result$TA))
  expect_equal(colnames(te_excel), colnames(result$TE))

  # Clean up
  unlink(file.path(temp_dir, paste0(study_id, "_TA.xlsx")))
  unlink(file.path(temp_dir, paste0(study_id, "_TE.xlsx")))
})
