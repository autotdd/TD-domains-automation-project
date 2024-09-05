library(testthat)
library(autoTDD)  # Assuming the package is called autoTDD

# Set up test environment
test_that("create_tv_domain function works correctly", {
  # Mock data
  study_id <- "STUDY01"

  # Call the function
  tv_domain <- create_tv_domain( study_id)

  # Test that the function returns a data frame
  expect_s3_class(tv_domain, "data.frame")  # Updated from expect_is

  # Test that the output file is created
  expect_true(file.exists(file.path( paste0(study_id, "_TV.xlsx"))))

  # Test that required columns are present
  required_columns <- c("STUDYID", "DOMAIN", "VISITNUM", "VISIT" ,"VISITDY", "TVSTRL", "TVENRL")
  expect_true(all(required_columns %in% colnames(tv_domain)))

  # Test specific values for study id and domain name
  expect_equal(tv_domain$STUDYID[1], study_id)
  expect_equal(tv_domain$DOMAIN[1], "TV")

  #Test that Study number  have 7 char length
  expect_true(nchar(tv_domain$STUDYID[1]) == 7 )

  #Test that Visitnum, Visit and visitdy ,TVSTRL, TVENRL   are not missing
  visitnum <- tv_domain[is.na(tv_domain$VISITNUM),]
  expect_true(nrow(visitnum) == 0 )

  visit <- tv_domain[(tv_domain$VISIT == 'NA'),]
  expect_true(nrow(visit) == 0 )

  visitdy <- tv_domain[(tv_domain$VISITDY == 'NA' & tv_domain$VISIT != 'Treatment Discontinuation'),]
  expect_true(nrow(visitdy) == 0 )

  tvstrl <- tv_domain[(tv_domain$TVSTRL == 'NA'),]
  expect_true(nrow(tvstrl) == 0 )

  tvenrl <- tv_domain[(tv_domain$TVENRL == 'NA'),]
  expect_true(nrow(tvenrl) == 0 )


})
