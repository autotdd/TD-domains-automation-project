library(dplyr)
library(openxlsx)

#' Generate TS Dataset (Version 3.4)
#'
#' This function generates the TS dataset for a given study ID and number of rows,
#' and reads from the 'Trial_Summary.xlsx' file included with the package.
#'
#' @param study_id A character string representing the Study ID.
#' @param num_rows An integer representing the number of rows to generate.
#' @return A data frame representing the TS dataset.
#' @examples
#' generate_TS_dataset("STUDY123", 5)
#' @export
generate_TS_dataset <- function(study_id, num_rows) {
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "autoTDD")
  data <- read.xlsx(file_path)
  
}

generate_TS_dataset <- function(study_id, num_rows) {
  # Path to the Trial_Summary.xlsx file within the package
  file_path <- system.file("extdata", "Trial_Summary.xlsx", package = "yourPackageName")
  
  # Read the Excel file
  Trial_Summary <- read.xlsx(file_path)
  
  # Generate the TS dataset 
  dataset <- data.frame(
    STUDYID = rep(study_id, num_rows),
    DOMAIN = rep("TS", num_rows),
    TSSEQ = integer(num_rows),
    TSGRPID = character(num_rows),
    TSPARMCD = character(num_rows),
    TSPARM = character(num_rows),
    TSVAL = character(num_rows),
    TSVALNF = character(num_rows),
    TSVALCD = character(num_rows),
    TSVCDREF = character(num_rows),
    TSVCDVER = character(num_rows),
    stringsAsFactors = FALSE
  )
  
  return(dataset)
}
