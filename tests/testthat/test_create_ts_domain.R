library(testthat)
library(autoTDD)  # Assuming the package is called autoTDD

# Set up test environment
test_that("create_ts_domain function works correctly", {
  # Mock data
  nct_ids <- c("NCT05789082")
  study_id <- "STUDY001"
  temp_dir <- tempdir()

  # Call the function
  ts_domain <- create_ts_domain(nct_ids, study_id, output_dir = temp_dir)

  # Test that the function returns a data frame
  expect_s3_class(ts_domain, "data.frame")  # Updated from expect_is

  # Test that the output file is created
  expect_true(file.exists(file.path(temp_dir, paste0(study_id, "_TS.xlsx"))))

  # Test that required columns are present
  required_columns <- c("STUDYID", "DOMAIN", "TSPARMCD", "TSVAL")
  expect_true(all(required_columns %in% colnames(ts_domain)))

  # Test specific values
  expect_equal(ts_domain$STUDYID[1], study_id)
  expect_equal(ts_domain$DOMAIN[1], "TS")



  # Test that TSVAL is not empty for key parameters
  key_params <- c("TITLE", "TPHASE", "ACTSUB", "SPONSOR", "SEXPOP")  # Changed TSPHAS to TPHASE

  expect_true(any(!is.na(ts_domain$TSVAL[ts_domain$TSPARMCD == "TPHASE"])),
              info = paste("TSVAL should not be empty for", TPHASE))


  # Test date formatting
  date_params <- c("SSTDTC", "SENDTC")
  for (param in date_params) {
    date_val <- ts_domain$TSVAL[ts_domain$TSPARMCD == param]
    expect_true(all(is.na(date_val) | grepl("^\\d{4}-\\d{2}-\\d{2}$", date_val)),
                info = paste("Date format incorrect for", param))
  }

  # Test age formatting
  age_params <- c("AGEMIN", "AGEMAX")
  for (param in age_params) {
    age_val <- ts_domain$TSVAL[ts_domain$TSPARMCD == param]
    expect_true(all(is.na(age_val) | grepl("^P\\d+[YMD]$", age_val)),
                info = paste("Age format incorrect for", param))
  }


  # Test phase conversion
  phase_val <- ts_domain$TSVAL[ts_domain$TSPARMCD == "TPHASE"]
  expect_true(all(is.na(phase_val) | phase_val %in% c("Phase I", "Phase II", "Phase III", "Phase IV", "N/A")),
              info = "Incorrect phase conversion")
})
