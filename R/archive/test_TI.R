library(autoTDD)
library(pdftools)

# Set the study ID and output directory
study_id <- "STUDY001"
output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Get the correct path to the PDF file
pdf_path <- system.file("extdata", "Temp_Protocol_v1.pdf", package = "autoTDD")

# Read the PDF content
pdf_text <- pdf_text(pdf_path)

# Print content of pages 6-9 for debugging
cat("Content of pages 6-9:\n")
for (i in 6:9) {
  cat("\nPage", i, ":\n")
  cat(pdf_text[i])
}

# Call the create_ti_domain function with PDF method
ti_domain_pdf <- create_ti_domain(
  study_id = study_id,
  method = "pdf",
  pdf_path = pdf_path,
  incl_range = 6:9,  # Adjust this range to cover the inclusion criteria
  excl_range = 9:13,  # Adjust this range to cover the exclusion criteria
  incl_section = "4.1.1",
  excl_section = "4.1.2",
  end_section = "4.2",
  output_dir = output_dir
)

# Print the result
print(head(ti_domain_pdf))

# Save the result to a CSV file for easier viewing
write.csv(ti_domain_pdf, file.path(output_dir, "ti_domain_result.csv"), row.names = FALSE)
cat("Results saved to:", file.path(output_dir, "ti_domain_result.csv"))

