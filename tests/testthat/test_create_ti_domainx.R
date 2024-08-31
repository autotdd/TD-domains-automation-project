test_create_ti_domain_api <- function() {
  study_id <- "STUDY001"
  nct_id <- "NCT00000419"
  output_dir <- tempdir()
  
  tryCatch({
    ti_domain <- create_ti_domain(study_id, method = "api", nct_id = nct_id, output_dir = output_dir)
    
    if (is.data.frame(ti_domain) && nrow(ti_domain) > 0) {
      cat("Test passed: TI domain created successfully\n")
      cat("Number of rows in TI domain:", nrow(ti_domain), "\n")
      cat("Output file:", file.path(output_dir, paste0(study_id, "_TI.xlsx")), "\n")
    } else {
      cat("Test failed: TI domain is empty or not a data frame\n")
    }
  }, error = function(e) {
    cat("Test failed with error:", conditionMessage(e), "\n")
  })
}