library(jsonlite)
library(httr)

# Save JSON data to a file
save_json <- function(data, filename) {
  json_dir <- system.file("json", package = "autoTDD")
  if (json_dir == "") {
    json_dir <- file.path(getwd(), "inst/json")
  }
  if (!dir.exists(json_dir)) {
    dir.create(json_dir, recursive = TRUE)
  }
  json_file <- file.path(json_dir, filename)
  write(toJSON(data, pretty = TRUE), json_file)
}

# Load JSON data from a file
load_json <- function(filename) {
  json_dir <- system.file("json", package = "autoTDD")
  if (json_dir == "") {
    json_dir <- file.path(getwd(), "inst/json")
  }
  json_file <- file.path(json_dir, filename)
  if (!file.exists(json_file)) {
    stop("File does not exist: ", json_file)
  }
  data <- fromJSON(json_file)
  return(data)
}

# Fetch study information from ClinicalTrials.gov
get_study_info <- function(nct_ids) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  url <- paste0(base_url, nct_ids)
  
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Failed to fetch data from ClinicalTrials.gov API")
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content)
  
  # Save JSON data
  filename <- paste0("study_info_", nct_ids, ".json")
  save_json(json_data, filename)
  
  # Load JSON data back into a data frame
  data <- load_json(filename)
  
  return(data)
}
