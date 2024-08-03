# Save JSON data to a file
#' Save JSON Data
#'
#' This function saves data as a JSON file in the package's json directory.
#'
#' @param data The data to save.
#' @param filename The name of the JSON file.
#' @export
save_json <- function(data, filename) {
  json_dir <- system.file("json", package = "autoTDD")
  if (json_dir == "") {
    json_dir <- file.path(getwd(), "inst/json")
  }
  json_file <- file.path(json_dir, filename)
  write(jsonlite::toJSON(data, pretty = TRUE), json_file)
}

# Load JSON data from a file
#' Load JSON Data
#'
#' This function loads data from a JSON file in the package's json directory.
#'
#' @param filename The name of the JSON file.
#' @return The data loaded from the JSON file.
#' @export
load_json <- function(filename) {
  json_dir <- system.file("json", package = "autoTDD")
  json_file <- file.path(json_dir, filename)
  if (!file.exists(json_file)) {
    stop("File does not exist: ", json_file)
  }
  data <- jsonlite::fromJSON(json_file)
  return(data)
}
