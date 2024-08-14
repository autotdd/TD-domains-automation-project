library(testthat)
library(dplyr)

test_that("create_ta_te_domains_pa works correctly for simple PARALLEL DESIGN", {
  study_id <- "STUDY002"
  trial_design <- "PARALLEL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(
    c("A", "B", "C"),
    c("D", "E", "F")
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C",
                "TREATMENT D", "TREATMENT E", "TREATMENT F", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C",
               "First dose D", "First dose E", "First dose F", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of C",
               "End of D", "End of E", "End of F", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P14D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)

  # Test TA domain
  expect_equal(nrow(result$TA), 10) # 5 epochs * 2 arms
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "FOLLOW-UP"))
  expect_true(all(is.na(result$TA$TABRANCH)))
  expect_true(all(is.na(result$TA$TATRANS)))

  # Check treatment sequence
  arm1_treatments <- result$TA %>%
    filter(ARMCD == "ARM1", EPOCH == "TREATMENT") %>%
    pull(ELEMENT)
  expect_equal(arm1_treatments, c("TREATMENT A", "TREATMENT B", "TREATMENT C"))

  arm2_treatments <- result$TA %>%
    filter(ARMCD == "ARM2", EPOCH == "TREATMENT") %>%
    pull(ELEMENT)
  expect_equal(arm2_treatments, c("TREATMENT D", "TREATMENT E", "TREATMENT F"))

  # Test TE domain
  expect_equal(nrow(result$TE), 8) # Number of unique elements in te_rules
  expect_equal(result$TE$STUDYID, rep(study_id, 8))
  expect_equal(result$TE$DOMAIN, rep("TE", 8))
  expect_equal(result$TE$ETCD, paste0("ET", 1:8))
  expect_equal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_pa works correctly for PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS", {
  study_id <- "STUDY003"
  trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Follow-Up",
      branch = c(NA, "Branch1", NA, NA),
      trans = c(NA, "Trans1", "Trans2", NA)
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Treatment,Follow-Up",
      branch = c(NA, "Branch2", NA, NA),
      trans = c(NA, "Trans3", "Trans4", NA)
    )
  )
  treatments_list <- list(
    c("A", "B"),
    c("C", "D")
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "TREATMENT D", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C", "First dose D", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of C", "End of D", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)

  # Test TA domain
  expect_equal(nrow(result$TA), 8) # 4 epochs * 2 arms
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "FOLLOW-UP"))
  expect_false(all(is.na(result$TA$TABRANCH)))
  expect_false(all(is.na(result$TA$TATRANS)))

  # Check branches and transitions
  arm1_data <- result$TA %>% filter(ARMCD == "ARM1")
  expect_equal(arm1_data$TABRANCH, c(NA, "Branch1", NA, NA))
  expect_equal(arm1_data$TATRANS, c(NA, "Trans1", "Trans2", NA))

  # Test TE domain
  expect_equal(nrow(result$TE), 6) # Number of unique elements in te_rules
  expect_equal(result$TE$STUDYID, rep(study_id, 6))
  expect_equal(result$TE$DOMAIN, rep("TE", 6))
  expect_equal(result$TE$ETCD, paste0("ET", 1:6))
  expect_equal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_pa handles errors correctly", {
  study_id <- "STUDY004"
  trial_design <- "PARALLEL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(c("A"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  # Test invalid trial design
  expect_error(
    create_ta_te_domains_pa(study_id, "INVALID DESIGN", arms_data, treatments_list, te_rules),
    "This function only supports 'PARALLEL DESIGN' and 'PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS'"
  )

  # Test mismatched epochs and treatments
  bad_treatments_list <- list(c("A", "B"))
  expect_error(
    create_ta_te_domains_pa(study_id, trial_design, arms_data, bad_treatments_list, te_rules),
    "Mismatch between number of treatments and treatment epochs for arm 1"
  )
})
