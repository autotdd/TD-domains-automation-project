library(testthat)
library(dplyr)

test_that("create_ta_te_domains_fd produces correct output for a 2x2 factorial design", {
  study_id <- "HYPBP001"
  trial_design <- "FACTORIAL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM3",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM4",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments <- list(
    c("Drug A", "Low-sodium diet"),  # ARM1
    c("Drug A", "Regular diet"),     # ARM2
    c("Placebo", "Low-sodium diet"), # ARM3
    c("Placebo", "Regular diet")     # ARM4
  )
  te_rules <- data.frame(
    ELEMENT = c(
      "SCREENING",
      "TREATMENT DRUG A LOW-SODIUM DIET",
      "TREATMENT DRUG A REGULAR DIET",
      "TREATMENT PLACEBO LOW-SODIUM DIET",
      "TREATMENT PLACEBO REGULAR DIET",
      "FOLLOW-UP"
    ),
    TESTRL = c(
      "Informed consent",
      "Start Drug A and Low-sodium diet",
      "Start Drug A and Regular diet",
      "Start Placebo and Low-sodium diet",
      "Start Placebo and Regular diet",
      "End of treatment"
    ),
    TEENRL = c(
      "End of screening",
      "End of Drug A and Low-sodium diet",
      "End of Drug A and Regular diet",
      "End of Placebo and Low-sodium diet",
      "End of Placebo and Regular diet",
      "End of study"
    ),
    TEDUR = c("P7D", "P84D", "P84D", "P84D", "P84D", "P14D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules)

  # Check overall structure
  expect_type(result, "list")
  expect_named(result, c("TA", "TE"))
  expect_s3_class(result$TA, "data.frame")
  expect_s3_class(result$TE, "data.frame")

  # Check TA domain
  expect_equal(nrow(result$TA), 12) # 3 epochs * 4 arms
  expect_equal(ncol(result$TA), 10) # Ensure all required variables are present
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2", "ARM3", "ARM4"))
  expect_equal(unique(result$TA$TAETORD), 1:3)
  expect_true(all(result$TA$ETCD %in% paste0("ET", 1:12)))
  expect_setequal(unique(result$TA$ELEMENT), c("SCREENING",
                                               "TREATMENT DRUG A LOW-SODIUM DIET",
                                               "TREATMENT DRUG A REGULAR DIET",
                                               "TREATMENT PLACEBO LOW-SODIUM DIET",
                                               "TREATMENT PLACEBO REGULAR DIET",
                                               "FOLLOW-UP"))
  expect_true(all(is.na(result$TA$TABRANCH)))
  expect_true(all(is.na(result$TA$TATRANS)))
  expect_setequal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "FOLLOW-UP"))

  # Check treatment sequence for each arm
  # Update the treatment checks
  arm1_treatments <- result$TA %>% filter(ARMCD == "ARM1", EPOCH == "TREATMENT") %>% pull(ELEMENT)
  expect_equal(arm1_treatments, "TREATMENT DRUG A LOW-SODIUM DIET")

  arm2_treatments <- result$TA %>% filter(ARMCD == "ARM2", EPOCH == "TREATMENT") %>% pull(ELEMENT)
  expect_equal(arm2_treatments, "TREATMENT DRUG A REGULAR DIET")

  arm3_treatments <- result$TA %>% filter(ARMCD == "ARM3", EPOCH == "TREATMENT") %>% pull(ELEMENT)
  expect_equal(arm3_treatments, "TREATMENT PLACEBO LOW-SODIUM DIET")

  arm4_treatments <- result$TA %>% filter(ARMCD == "ARM4", EPOCH == "TREATMENT") %>% pull(ELEMENT)
  expect_equal(arm4_treatments, "TREATMENT PLACEBO REGULAR DIET")

  # Check TE domain
  expect_equal(nrow(result$TE), 6) # 6 unique elements
  expect_equal(ncol(result$TE), 7) # Ensure all required variables are present
  expect_equal(unique(result$TE$STUDYID), study_id)
  expect_equal(unique(result$TE$DOMAIN), "TE")
  expect_true(all(result$TE$ETCD %in% paste0("ET", 1:6)))
  expect_setequal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_fd handles errors correctly", {
  study_id <- "HYPBP001"
  trial_design <- "FACTORIAL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments <- list(c("Drug A", "Low-sodium diet"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT DRUG A LOW-SODIUM DIET", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "Start treatment", "End of treatment"),
    TEENRL = c("End of screening", "End of treatment", "End of study"),
    TEDUR = c("P7D", "P84D", "P14D"),
    stringsAsFactors = FALSE
  )

  # Test invalid trial design
  expect_error(
    create_ta_te_domains_fd(study_id, "PARALLEL DESIGN", arms_data, treatments, te_rules),
    "This function is customized only for 'FACTORIAL DESIGN'."
  )

  # Test mismatched epochs and treatments
  bad_arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Follow-Up"  # Two Treatment epochs
    )
  )
  expect_error(
    create_ta_te_domains_fd(study_id, trial_design, bad_arms_data, treatments, te_rules),
    "Each arm should have exactly one Treatment epoch for arm 1"
  )
})

test_that("create_ta_te_domains_fd creates separate Excel files for TA and TE", {
  study_id <- "HYPBP001"
  trial_design <- "FACTORIAL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments <- list(
    c("Drug A", "Low-sodium diet"),
    c("Placebo", "Regular diet")
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT DRUG A LOW-SODIUM DIET", "TREATMENT PLACEBO REGULAR DIET", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "Start Drug A and Low-sodium diet", "Start Placebo and Regular diet", "End of treatment"),
    TEENRL = c("End of screening", "End of Drug A and Low-sodium diet", "End of Placebo and Regular diet", "End of study"),
    TEDUR = c("P7D", "P84D", "P84D", "P14D"),
    stringsAsFactors = FALSE
  )

  # Create a temporary directory for output files
  temp_dir <- tempdir()

  result <- create_ta_te_domains_fd(study_id, trial_design, arms_data, treatments, te_rules, output_dir = temp_dir)

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
