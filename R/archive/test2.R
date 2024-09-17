library(httr)
library(jsonlite)
library(dplyr)

api_key <- "418dfd0e31254725a6aa246b2bfb2591"  # Your actual API key
base_url <- "https://library.cdisc.org/api"

# Function to make API requests
make_api_request <- function(endpoint) {
  url <- paste0(base_url, endpoint)
  response <- GET(url, add_headers("api-key" = api_key))
  
  cat("URL accessed:", url, "\n")
  cat("Status code:", status_code(response), "\n")
  
  if (status_code(response) == 200) {
    content <- content(response, "text")
    return(fromJSON(content))
  } else {
    stop("Error: ", status_code(response), " - ", substr(content(response, "text"), 1, 200))
  }
}

# Get available packages
packages <- make_api_request("/mdr/ct/packages")

# Extract the packages data frame
packages_df <- packages$`_links`$packages

# Filter for SDTM packages
sdtm_packages <- packages_df %>%
  filter(grepl("^SDTM", title)) %>%
  arrange(desc(title))

# Get the latest SDTM package
latest_sdtm_package <- sdtm_packages[1, ]

cat("\nLatest SDTM package:", latest_sdtm_package$title, "\n")

# Extract version from the title (assuming format like "SDTM Controlled Terminology Package X Effective YYYY-MM-DD")
version <- sub(".*Effective ([0-9-]+).*", "\\1", latest_sdtm_package$title)

# Get the content of the latest SDTM package
latest_sdtm_content <- make_api_request(latest_sdtm_package$href)

# Save the latest SDTM terminology as a JSON file
json_file_path <- "latest_sdtm_terminology.json"
write_json(latest_sdtm_content, json_file_path, pretty = TRUE)

cat("Latest SDTM terminology saved as:", json_file_path, "\n")

# Print structure of latest_sdtm_content
cat("\nStructure of latest_sdtm_content:\n")
str(latest_sdtm_content, max.level = 2)

# Extract TS-related codelists
ts_codelists <- latest_sdtm_content$codelists %>%
  filter(grepl("^TS", name) | grepl("TRIAL", toupper(name)))

# Save TS-related codelists
ts_codelists_file_path <- "ts_codelists.json"
write_json(ts_codelists, ts_codelists_file_path, pretty = TRUE)

cat("TS-related codelists saved as:", ts_codelists_file_path, "\n")

# Print some information about TS-related codelists
cat("\nNumber of TS-related codelists:", nrow(ts_codelists), "\n")
cat("\nFirst few TS-related codelists:\n")
print(head(ts_codelists$name))

# If you want to see all the codelist names related to TS
cat("\nAll TS-related codelist names:\n")
print(ts_codelists$name)

# To see the terms for a specific codelist (e.g., the first one in the list)
if (nrow(ts_codelists) > 0) {
  first_codelist <- ts_codelists$name[1]
  cat("\nTerms for", first_codelist, ":\n")
  print(ts_codelists$terms[[1]])
}



# Read the JSON file
ts_codelists <- fromJSON("ts_codelists.json")

# Function to flatten the terms list
flatten_terms <- function(terms) {
  if (is.null(terms) || length(terms) == 0) {
    return(data.frame(
      term_conceptId = NA,
      term_definition = NA,
      term_preferredTerm = NA,
      term_submissionValue = NA,
      term_synonyms = NA
    ))
  }
  
  df <- do.call(rbind, lapply(terms, function(term) {
    data.frame(
      term_conceptId = ifelse(is.null(term$conceptId), NA, term$conceptId),
      term_definition = ifelse(is.null(term$definition), NA, term$definition),
      term_preferredTerm = ifelse(is.null(term$preferredTerm), NA, term$preferredTerm),
      term_submissionValue = ifelse(is.null(term$submissionValue), NA, term$submissionValue),
      term_synonyms = ifelse(is.null(term$synonyms) || length(term$synonyms) == 0, NA, 
                             paste(unlist(term$synonyms), collapse = "; "))
    )
  }))
  return(df)
}

# Create the data frame
ts_df <- do.call(rbind, lapply(1:nrow(ts_codelists), function(i) {
  codelist <- ts_codelists[i, ]
  terms_df <- flatten_terms(codelist$terms)
  cbind(
    codelist_conceptId = codelist$conceptId,
    codelist_definition = codelist$definition,
    codelist_extensible = codelist$extensible,
    codelist_name = codelist$name,
    codelist_preferredTerm = codelist$preferredTerm,
    codelist_submissionValue = codelist$submissionValue,
    codelist_synonyms = ifelse(is.null(codelist$synonyms) || length(codelist$synonyms) == 0, NA, 
                               paste(unlist(codelist$synonyms), collapse = "; ")),
    terms_df
  )
}))

# Convert to data frame if it's not already
ts_df <- as.data.frame(ts_df, stringsAsFactors = FALSE)

# Print the first few rows of the data frame
print(head(ts_df))

# Print the dimensions of the data frame
cat("\nDimensions of the data frame:", dim(ts_df)[1], "rows and", dim(ts_df)[2], "columns\n")

# Save the data frame as a CSV file (optional)
write.csv(ts_df, "ts_codelists_dataframe.csv", row.names = FALSE)
cat("\nData frame saved as 'ts_codelists_dataframe.csv'\n")

# Install and load the required packages
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(jsonlite)
library(openxlsx)

# Read the JSON file
ts_codelists <- fromJSON("ts_codelists.json", simplifyVector = FALSE)

# Print the structure of ts_codelists for debugging
cat("Number of codelists:", length(ts_codelists), "\n")

# Function to safely convert a value to string
safe_string <- function(x) {
  if (is.null(x)) return(NA)
  if (is.list(x) && length(x) == 0) return(NA)
  if (is.list(x)) return(paste(unlist(x), collapse = "; "))
  return(as.character(x))
}

# Function to process a single codelist
process_codelist <- function(codelist) {
  terms <- codelist$terms
  if (is.null(terms) || length(terms) == 0) {
    return(data.frame(
      Codelist_ConceptId = safe_string(codelist$conceptId),
      Codelist_Name = safe_string(codelist$name),
      TSPARMCD = safe_string(codelist$submissionValue),
      TSPARM = safe_string(codelist$preferredTerm),
      Codelist_Definition = safe_string(codelist$definition),
      Codelist_Extensible = safe_string(codelist$extensible),
      Codelist_Synonyms = safe_string(codelist$synonyms),
      Term_ConceptId = NA,
      TSVAL = NA,
      Term_PreferredTerm = NA,
      Term_Definition = NA,
      Term_Synonyms = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, lapply(terms, function(term) {
    data.frame(
      Codelist_ConceptId = safe_string(codelist$conceptId),
      Codelist_Name = safe_string(codelist$name),
      TSPARMCD = safe_string(codelist$submissionValue),
      TSPARM = safe_string(codelist$preferredTerm),
      Codelist_Definition = safe_string(codelist$definition),
      Codelist_Extensible = safe_string(codelist$extensible),
      Codelist_Synonyms = safe_string(codelist$synonyms),
      Term_ConceptId = safe_string(term$conceptId),
      TSVAL = safe_string(term$submissionValue),
      Term_PreferredTerm = safe_string(term$preferredTerm),
      Term_Definition = safe_string(term$definition),
      Term_Synonyms = safe_string(term$synonyms),
      stringsAsFactors = FALSE
    )
  }))
}

# Process all codelists
ts_df_reshaped <- do.call(rbind, lapply(ts_codelists, process_codelist))

# Function to update TSPARMCD and TSPARM columns
update_ts_columns <- function(df) {
  tsparmcd_rows <- df$TSPARMCD == "TSPARMCD"
  tsparm_rows <- df$TSPARMCD == "TSPARM"
  
  df$TSPARMCD[tsparmcd_rows] <- df$TSVAL[tsparmcd_rows]
  df$TSPARM[tsparmcd_rows] <- df$Term_PreferredTerm[tsparmcd_rows]
  df$TSVAL[tsparmcd_rows] <- NA
  df$Term_PreferredTerm[tsparmcd_rows] <- NA
  
  df$TSPARMCD[tsparm_rows] <- df$TSVAL[tsparm_rows]
  df$TSPARM[tsparm_rows] <- df$Term_PreferredTerm[tsparm_rows]
  df$TSVAL[tsparm_rows] <- NA
  df$Term_PreferredTerm[tsparm_rows] <- NA
  
  return(df)
}

# Update the data frame
ts_df_reshaped <- update_ts_columns(ts_df_reshaped)

# Print dimensions of the reshaped data frame
cat("\nDimensions of ts_df_reshaped:", dim(ts_df_reshaped), "\n")

# Create a new workbook
wb <- createWorkbook()

# Add the reshaped data to a worksheet
addWorksheet(wb, "TS Codelists")
writeData(wb, "TS Codelists", ts_df_reshaped)

# Apply formatting
style <- createStyle(wrapText = TRUE, textDecoration = "bold")
addStyle(wb, "TS Codelists", style = style, rows = 1, cols = 1:ncol(ts_df_reshaped))

# Adjust column widths
setColWidths(wb, "TS Codelists", cols = 1:ncol(ts_df_reshaped), widths = "auto")

# Adjust row height for Term_Synonyms column
term_synonyms_col <- which(colnames(ts_df_reshaped) == "Term_Synonyms")
for (i in 2:nrow(ts_df_reshaped)) {
  cell_value <- ts_df_reshaped[i, term_synonyms_col]
  if (!is.na(cell_value)) {
    num_lines <- length(strsplit(cell_value, "\n")[[1]])
    setRowHeights(wb, "TS Codelists", rows = i, heights = 15 * max(num_lines, 1))
  }
}

# Save the workbook
excel_file <- "ts_codelists_full_info.xlsx"
saveWorkbook(wb, excel_file, overwrite = TRUE)

cat("\nFull information saved in '", excel_file, "'\n", sep = "")