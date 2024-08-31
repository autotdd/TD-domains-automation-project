# test_create_ti_domain.R
library(testthat)
library(autoTDD)

test_that("create_ti_domain works with PDF method", {
  study_id <- "STUDY001"
  method <- "pdf"
  pdf_path <- system.file("extdata", "Temp_Protocol_v1.pdf", package = "autoTDD")
  incl_range <- 6:9
  excl_range <- 9:16
  incl_section <- "4.1.1"
  excl_section <- "4.1.2"
  end_section <- "4.2"
  output_dir <- tempdir()

  expect_error(create_ti_domain(
    study_id = study_id,
    method = method,
    pdf_path = pdf_path,
    incl_range = incl_range,
    excl_range = excl_range,
    incl_section = incl_section,
    excl_section = excl_section,
    end_section = end_section,
    output_dir = output_dir
  ), NA)
})

test_that("create_ti_domain works with API method", {
  study_id <- "STUDY001"
  method <- "api"
  nct_id <- "NCT00000419"
  output_dir <- tempdir()

  ti_domain <- create_ti_domain(
    study_id = study_id,
    method = method,
    nct_id = nct_id,
    output_dir = output_dir
  )

  expect_true(is.data.frame(ti_domain))
  expect_true(nrow(ti_domain) > 0)
  expect_true(file.exists(file.path(output_dir, paste0(study_id, "_TI.xlsx"))))
})
