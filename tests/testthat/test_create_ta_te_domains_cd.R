# File: tests/testthat/test-create_ta_te_domains_cd.R

library(testthat)
library(dplyr)

test_that("create_ta_te_domains_cd produces correct output for a 2x2 cross-over design", {
  study_id <- "STUDY001"
  trial_design <- "CROSS-OVER DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      arm = "Sequence AB",
      epochs = "Screening,Treatment,Washout,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      arm = "Sequence BA",
      epochs = "Screening,Treatment,Washout,Treatment,Follow-Up"
    )
  )
  treatments <- list(c("A", "B"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "WASHOUT","TREATMENT B", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A","Start washout", "First dose B", "End of treatment"),
    TEENRL = c("End of screening", "End of A","End of washout", "End of B", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P7D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_cd(study_id, trial_design, arms_data, treatments, te_rules)

  # Check overall structure
  expect_type(result, "list")
  expect_named(result, c("TA", "TE"))
  expect_s3_class(result$TA, "data.frame")
  expect_s3_class(result$TE, "data.frame")

  # Check TA domain
  expect_equal(nrow(result$TA), 10) # 5 epochs * 2 arms
  expect_equal(ncol(result$TA), 10) # Ensure all required variables are present
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(result$TA$ARM), c("Sequence AB", "Sequence BA"))
  expect_equal(unique(result$TA$TAETORD), 1:5)
  expect_true(all(result$TA$ETCD %in% paste0("ET", 1:10)))
  expect_setequal(unique(result$TA$ELEMENT), c("SCREENING", "TREATMENT A","WASHOUT", "TREATMENT B", "FOLLOW-UP"))
  expect_true(all(is.na(result$TA$TABRANCH)))
  expect_true(all(is.na(result$TA$TATRANS)))
  expect_setequal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "WASHOUT", "FOLLOW-UP"))


  # Check sequence of treatments
  arm1_treatments <- result$TA %>% filter(ARMCD == "ARM1", grepl("TREATMENT", ELEMENT)) %>% pull(ELEMENT)
  arm2_treatments <- result$TA %>% filter(ARMCD == "ARM2", grepl("TREATMENT", ELEMENT)) %>% pull(ELEMENT)
  expect_equal(arm1_treatments, c("TREATMENT A", "TREATMENT B"))
  expect_equal(arm2_treatments, c("TREATMENT B", "TREATMENT A"))


  # Check TE domain
  expect_equal(nrow(result$TE), 5) # 5 unique elements
  expect_equal(ncol(result$TE), 7) # Ensure all required variables are present
  expect_equal(unique(result$TE$STUDYID), study_id)
  expect_equal(unique(result$TE$DOMAIN), "TE")
  expect_true(all(result$TE$ETCD %in% paste0("ET", 1:5)))
  expect_setequal(result$TE$ELEMENT, c("SCREENING", "TREATMENT A", "WASHOUT", "TREATMENT B", "FOLLOW-UP"))
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)

})

test_that("create_ta_te_domains_cd handles 3x3 cross-over design correctly", {
  study_id <- "STUDY002"
  trial_design <- "CROSS-OVER DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      arm = "Sequence ABC",
      epochs = "Screening,Treatment,Washout,Treatment,Washout,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      arm = "Sequence BCA",
      epochs = "Screening,Treatment,Washout,Treatment,Washout,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM3",
      arm = "Sequence CAB",
      epochs = "Screening,Treatment,Washout,Treatment,Washout,Treatment,Follow-Up"
    )
  )
  treatments <- list(c("A", "B", "C"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C", "Start washout", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of C", "End of washout", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P7D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_cd(study_id, trial_design, arms_data, treatments, te_rules)


  # Check overall structure
  expect_type(result, "list")
  expect_named(result, c("TA", "TE"))
  expect_s3_class(result$TA, "data.frame")
  expect_s3_class(result$TE, "data.frame")


  # Check TA domain
  expect_equal(nrow(result$TA), 21) # 7 epochs * 3 arms
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2", "ARM3"))
  expect_equal(unique(result$TA$ARM), c("Sequence ABC", "Sequence BCA", "Sequence CAB"))
  expect_equal(unique(result$TA$TAETORD), 1:7)
  expect_true(all(result$TA$ETCD %in% paste0("ET", 1:21)))

  # Check sequence of treatments for each arm
  for (i in 1:3) {
    arm_treatments <- result$TA %>%
      filter(ARMCD == paste0("ARM", i), grepl("TREATMENT", ELEMENT)) %>%
      pull(ELEMENT)
    expect_equal(length(arm_treatments), 3)
    expect_true(all(arm_treatments %in% c("TREATMENT A", "TREATMENT B", "TREATMENT C")))
    expect_true(!any(duplicated(arm_treatments)))
  }

  # Check TE domain
  expect_equal(nrow(result$TE), 6) # 6 unique elements
  expect_setequal(result$TE$ELEMENT, c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "WASHOUT", "FOLLOW-UP"))
})

test_that("create_ta_te_domains_cd handles errors correctly", {
  study_id <- "STUDY005"
  trial_design <- "CROSS-OVER DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening, Treatment, Washout, Treatment"
    )
  )
  treatments <- list(c("A", "B"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "WASHOUT"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "Start washout"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of washout"),
    TEDUR = c("P7D", "P14D", "P14D", "P7D"),
    stringsAsFactors = FALSE
  )

  # Test invalid trial design
  expect_error(
    create_ta_te_domains_cd(study_id, "PARALLEL DESIGN", arms_data, treatments, te_rules),
    "This function only supports 'CROSS-OVER DESIGN'"
  )

  # Test mismatched epochs and treatments
  bad_treatments <- list(c("A", "B", "C"))
  expect_error(
    create_ta_te_domains_cd(study_id, trial_design, arms_data, bad_treatments, te_rules),
    "Mismatch between number of treatments and treatment epochs for arm 1"
  )
})
