#' Generate TA and TE Datasets for Cross-Over Design
#'
#' This function generates the TA (Trial Arms) and TE (Trial Elements) datasets
#' for a given study ID using the CROSS-OVER DESIGN.
#'
#' @param study_id A character string representing the Study ID.
#' @param trial_design A character string representing the trial design. Should be "CROSS-OVER DESIGN".
#' @param arms_data A list of arm data. Each element in the list should be a list containing `armcd`, `arm`, `epochs`, `etcd`, and `elements`.
#' @param treatments A list of treatments for the trial.
#' @param te_rules A data frame containing TE rules with columns: ETCD, TESTRL, TEENRL, TEDUR.
#' @param output_dir A character string representing the output directory. Defaults to the current working directory.
#' @return A list containing two data frames: TA dataset and TE dataset.
#' @export
#' @importFrom dplyr add_row distinct mutate select left_join
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle saveWorkbook
#' @importFrom lubridate ymd_hms duration
#'
#' @examples
#' study_id <- "CDXXX"
#' trial_design <- "CROSS-OVER DESIGN"
#' arms_data <- list(
#'   list(
#'     armcd = "AB",
#'     arm = "A then B",
#'     epochs = "SCREENING,TREATMENT 1,WASHOUT,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,WASH,TRT2,F/U",
#'     elements = "Screening,Treatment A,Washout,Treatment B,Follow-up"
#'   ),
#'   list(
#'     armcd = "BA",
#'     arm = "B then A",
#'     epochs = "SCREENING,TREATMENT 1,WASHOUT,TREATMENT 2,FOLLOW-UP",
#'     etcd = "SCRN,TRT1,WASH,TRT2,F/U",
#'     elements = "Screening,Treatment B,Washout,Treatment A,Follow-up"
#'   )
#' )
#' treatments <- list(c("Treatment A", "Treatment B"))
#' te_rules <- data.frame(
#'   ETCD = c("SCRN", "TRT1", "WASH", "TRT2", "F/U"),
#'   TESTRL = c("Informed consent", "First dose", "Last dose of Treatment 1", "First dose", "Last dose of Treatment 2"),
#'   TEENRL = c("Randomization", "Last dose", "First dose of Treatment 2", "Last dose", "Study completion"),
#'   TEDUR = c("P28D", "P12W", "P4W", "P12W", "P4W")
#' )
#' 
#' result <- create_ta_te_domains_cd(study_id, trial_design, arms_data, treatments, te_rules)
#' CDXXX_TA <- result$TA
#' CDXXX_TE <- result$TE
create_ta_te_domains_cd <- function(study_id, trial_design, arms_data, treatments, te_rules, output_dir = getwd()) {
    # Load necessary libraries silently
    suppressPackageStartupMessages({
        library(dplyr)
        library(openxlsx)
        library(lubridate)
    })

    # Validate inputs
    if (trial_design != "CROSS-OVER DESIGN") {
        stop("This function only supports 'CROSS-OVER DESIGN'")
    }

    # Check if te_rules contains all required columns
    required_columns <- c("ETCD", "TESTRL", "TEENRL", "TEDUR")
    if (!all(required_columns %in% colnames(te_rules))) {
        stop("te_rules must contain columns: ", paste(required_columns, collapse = ", "))
    }

    # Check for mismatched epochs and treatments
    for (i in seq_along(arms_data)) {
        epochs <- unlist(strsplit(arms_data[[i]]$epochs, ","))
        treatment_epochs <- sum(grepl("Treatment", epochs, ignore.case = TRUE))
        if (treatment_epochs != length(treatments[[1]])) {
            stop(paste("Mismatch between number of treatments and treatment epochs for arm", i))
        }
    }

    # Initialize TA domain data frame
    ta_df <- data.frame(
        STUDYID = character(),
        DOMAIN = character(),
        ARMCD = character(),
        ARM = character(),
        TAETORD = numeric(),
        ETCD = character(),
        ELEMENT = character(),
        TABRANCH = character(),
        TATRANS = character(),
        EPOCH = character(),
        stringsAsFactors = FALSE
    )

    # Populate TA domain data frame based on input rows
    row_index <- 1
    for (i in seq_along(arms_data)) {
        arm_data <- arms_data[[i]]
        epochs <- toupper(unlist(strsplit(arm_data$epochs, ",")))
        elements <- unlist(strsplit(arm_data$elements, ","))
        etcd <- unlist(strsplit(arm_data$etcd, ","))
        num_elements <- length(elements)

        # Use provided ARMCD and ARM values or default to generated ones
        armcd <- ifelse(is.null(arm_data$armcd), paste0("ARM", i), arm_data$armcd)
        arm <- ifelse(is.null(arm_data$arm), paste0("Group ", i), arm_data$arm)

        # Validate the lengths of element descriptions and epochs
        if (length(elements) != num_elements) {
            stop(paste("Element descriptions do not match the number of elements for arm", i))
        }

        if (length(epochs) != num_elements) {
            stop(paste("Epochs do not match the number of elements for arm", i))
        }

        # Populate the data frame
        for (j in seq_along(elements)) {
            ta_df <- ta_df %>%
                add_row(
                    STUDYID = study_id,
                    DOMAIN = "TA",
                    ARMCD = armcd,
                    ARM = arm,
                    TAETORD = j,
                    ETCD = etcd[j],
                    ELEMENT = elements[j],
                    TABRANCH = NA,
                    TATRANS = NA,
                    EPOCH = epochs[j]
                )
            row_index <- row_index + 1
        }
    }

    # Create TE domain using unique ETCD from TA domain
    te_df <- ta_df %>%
        distinct(STUDYID, ETCD, ELEMENT) %>%
        left_join(te_rules, by = "ETCD") %>%
        mutate(
            DOMAIN = "TE",
            TESTRL = coalesce(TESTRL, ""),
            TEENRL = coalesce(TEENRL, ""),
            TEDUR = coalesce(TEDUR, NA_character_)
    ) %>%
    select(STUDYID, DOMAIN, everything())

    # Save the TA domain to an Excel file
    ta_output_file <- file.path(output_dir, paste0(study_id, "_TA.xlsx"))
    wb_ta <- createWorkbook()
    addWorksheet(wb_ta, "TA")
    
    # Calculate max width for each column, with a minimum width of 10
    col_widths <- sapply(ta_df, function(col) {
        if (length(col) == 0 || all(is.na(col))) {
            return(10)
        }
        max(10, max(nchar(as.character(col)), na.rm = TRUE))
    })
    
    # Write data and set column widths
    writeData(wb_ta, "TA", ta_df, headerStyle = createStyle(textDecoration = "bold", halign = "left"))
    setColWidths(wb_ta, "TA", cols = 1:ncol(ta_df), widths = col_widths)
    
    # Apply left alignment to all cells
    style <- createStyle(halign = "left")
    addStyle(wb_ta, "TA", style = style, rows = 1:(nrow(ta_df) + 1), cols = 1:ncol(ta_df), gridExpand = TRUE)
    
    saveWorkbook(wb_ta, ta_output_file, overwrite = TRUE)

    # Save the TE domain to a separate Excel file
    te_output_file <- file.path(output_dir, paste0(study_id, "_TE.xlsx"))
    wb_te <- createWorkbook()
    addWorksheet(wb_te, "TE")
    
    # Calculate max width for each column, with a minimum width of 10
    col_widths_te <- sapply(te_df, function(col) {
        if (length(col) == 0 || all(is.na(col))) {
            return(10)
        }
        max(10, max(nchar(as.character(col)), na.rm = TRUE))
    })
    
    # Write data and set column widths
    writeData(wb_te, "TE", te_df, headerStyle = createStyle(textDecoration = "bold", halign = "left"))
    setColWidths(wb_te, "TE", cols = 1:ncol(te_df), widths = col_widths_te)
    
    # Apply left alignment to all cells
    addStyle(wb_te, "TE", style = style, rows = 1:(nrow(te_df) + 1), cols = 1:ncol(te_df), gridExpand = TRUE)
    
    saveWorkbook(wb_te, te_output_file, overwrite = TRUE)

    return(list(TA = ta_df, TE = te_df))
}