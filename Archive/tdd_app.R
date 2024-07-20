# Install required packages if not installed
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("readxl")) install.packages("readxl")
if (!require("shinyalert")) install.packages("shinyalert")
if (!require("shinyjs")) install.packages("shinyjs")

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)
library(readxl)
library(shinyalert)
library(shinyjs)

# Load the Trial Summary data for TS domain
file_path <- "Trial_Summary.xlsx"
if (file.exists(file_path)) {
  Trial_Summary <- read_excel(file_path)
} else {
  stop("File 'Trial_Summary.xlsx' not found in the current directory.")
}

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Trial Domains Design (TDD) Master"),
  dashboardSidebar(
    sidebarMenu(
      textInput("study_id", label = "Study ID", placeholder = "Enter Study ID..."),
      menuItem("SDTM I.G Version", tabName = "sdtm_version",
               menuSubItem("Create Dataset 3.2", tabName = "create_dataset_3_2"),
               menuSubItem("Create Dataset 3.4", tabName = "create_dataset_3_4")),
      menuItem("SDTM Controlled Terminology", tabName = "controlled_terminology",
               menuSubItem("2024-03-29", tabName = "controlled_terminology_2024_03_29"),
               menuSubItem("2024-02-15", tabName = "controlled_terminology_2024_02_15")),
      menuItem("Import/Export Data", tabName = "import_export"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    useShinyalert(),  # Include shinyalert in the UI
    useShinyjs(),  # Include shinyjs in the UI
    fluidRow(
      actionButton("edit_variables", "Edit Variables", class = "btn btn-success")
    ),
    tabItems(
      tabItem(
        tabName = "create_dataset_3_2",
        fluidRow(
          textOutput("study_id_display_3_2"),
          selectInput(
            inputId = "domain_selector_3_2",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          actionButton(
            inputId = "generate_dataset_3_2",
            label = "Generate Dataset",
            class = "btn-primary"
          ),
          DTOutput("dataset_table_3_2")
        )
      ),
      tabItem(
        tabName = "create_dataset_3_4",
        fluidRow(
          textOutput("study_id_display_3_4"),
          selectInput(
            inputId = "domain_selector_3_4",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          actionButton(
            inputId = "generate_dataset_3_4",
            label = "Generate Dataset",
            class = "btn-primary"
          ),
          DTOutput("dataset_table_3_4")
        )
      ),
      tabItem(
        tabName = "controlled_terminology_2024_03_29",
        h2("Controlled Terminology Version 2024-03-29 Content")
      ),
      tabItem(
        tabName = "controlled_terminology_2024_02_15",
        h2("Controlled Terminology Version 2024-02-15 Content")
      ),
      tabItem(
        tabName = "import_export",
        fluidRow(
          fileInput(
            inputId = "import_data",
            label = "Import Data (Excel)",
            accept = c(".xlsx")
          ),
          downloadButton(
            outputId = "export_data",
            label = "Export Data (Excel)",
            class = "btn-success"
          )
        ),
        br(),
        DTOutput("imported_data_table")
      ),
      tabItem(
        tabName = "about",
        p("This is an example CDISC Dataset Creator app.")
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store datasets
  generated_data <- reactiveValues(
    dataset_3_2 = NULL,
    dataset_3_4 = NULL,
    imported_data = NULL,
    editable = FALSE  # Track the editable state
  )
  
  # Display study ID
  output$study_id_display_3_2 <- renderText({
    paste("Study ID:", input$study_id)
  })
  output$study_id_display_3_4 <- renderText({
    paste("Study ID:", input$study_id)
  })
  
  # Function to generate TS dataset
  generate_TS_dataset <- function(study_id) {
    dataset <- Trial_Summary
    dataset$STUDYID <- study_id  # Populate STUDYID with the entered study number
    return(dataset)
  }
  
  # Generate dataset when "Generate Dataset" button is clicked
  observeEvent(input$generate_dataset_3_2, {
    if (is.null(input$study_id) || input$study_id == "") {
      shinyalert(
        title = "Warning",
        text = "Please enter a Study ID first.",
        type = "warning",
        size = "s"
      )
      return()
    }
    req(input$study_id, input$domain_selector_3_2)
    domain <- input$domain_selector_3_2
    
    if (domain == "TS") {
      dataset <- generate_TS_dataset(input$study_id)
    } else {
      dataset <- data.frame(Message = paste("No data available for domain", domain))
    }
    
    dataset <- cbind(Delete = ifelse(dataset$`FDA Desired` != 'Y', '<button type="button" class="btn btn-danger btn-sm delete">Delete</button>', ''), dataset)
    generated_data$dataset_3_2 <- dataset
    output$dataset_table_3_2 <- renderDT({
      datatable(dataset, escape = FALSE, editable = TRUE, rownames = FALSE,
                options = list(
                  pageLength = 100,  # Show 100 entries by default
                  columnDefs = list(
                    list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                    list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                    list(targets = '_all', className = 'dt-center')
                  )
                )
      )
    }, server = FALSE)
  })
  
  observeEvent(input$generate_dataset_3_4, {
    if (is.null(input$study_id) || input$study_id == "") {
      shinyalert(
        title = "Warning",
        text = "Please enter a Study ID first.",
        type = "warning",
        size = "s"
      )
      return()
    }
    req(input$study_id, input$domain_selector_3_4)
    domain <- input$domain_selector_3_4
    
    if (domain == "TS") {
      dataset <- generate_TS_dataset(input$study_id)
    } else {
      dataset <- data.frame(Message = paste("No data available for domain", domain))
    }
    
    dataset <- cbind(Delete = ifelse(dataset$`FDA Desired` != 'Y', '<button type="button" class="btn btn-danger btn-sm delete">Delete</button>', ''), dataset)
    generated_data$dataset_3_4 <- dataset
    output$dataset_table_3_4 <- renderDT({
      datatable(dataset, escape = FALSE, editable = TRUE, rownames = FALSE,
                options = list(
                  pageLength = 100,  # Show 100 entries by default
                  columnDefs = list(
                    list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                    list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                    list(targets = '_all', className = 'dt-center')
                  )
                )
      )
    }, server = FALSE)
  })
  
  # Toggle editable state
  observeEvent(input$edit_variables, {
    generated_data$editable <- !generated_data$editable  # Toggle the editable state
    if (generated_data$editable) {
      shinyjs::html("edit_variables", "Finish Editing/Unlock")
    } else {
      shinyjs::html("edit_variables", "Edit Variables")
    }
    
    if (!is.null(generated_data$dataset_3_2)) {
      dataset <- generated_data$dataset_3_2
      dataset$Delete <- ifelse(dataset$`FDA Desired` != 'Y', '<button type="button" class="btn btn-danger btn-sm delete">Delete</button>', '')
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, escape = FALSE, editable = TRUE, rownames = FALSE,
                  options = list(
                    pageLength = 100,  # Show 100 entries by default
                    columnDefs = list(
                      list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                      list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                      list(targets = '_all', className = 'dt-center')
                    )
                  )
        )
      }, server = FALSE)
    }
    if (!is.null(generated_data$dataset_3_4)) {
      dataset <- generated_data$dataset_3_4
      dataset$Delete <- ifelse(dataset$`FDA Desired` != 'Y', '<button type="button" class="btn btn-danger btn-sm delete">Delete</button>', '')
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, escape = FALSE, editable = TRUE, rownames = FALSE,
                  options = list(
                    pageLength = 100,  # Show 100 entries by default
                    columnDefs = list(
                      list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                      list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                      list(targets = '_all', className = 'dt-center')
                    )
                  )
        )
      }, server = FALSE)
    }
  })
  
  # Observe delete button clicks and show confirmation dialog
  observeEvent(input$dataset_table_3_2_cell_clicked, {
    info <- input$dataset_table_3_2_cell_clicked
    print(info)  # Log info for debugging
    
    if (is.null(info) || is.null(info$row) || is.null(info$col) || is.null(info$value)) {
      print("Info is NULL or invalid")
      return()
    }
    
    if (info$col == 0) {  # Change to 0 to reference the first column for delete button
      row_index <- info$row + 1  # Adjust for 1-based indexing
      fda_desired <- generated_data$dataset_3_2[row_index, 'FDA Desired']
      
      print(paste("Deleting row:", info$row))  # Debugging statement
      print(paste("FDA Desired Value:", fda_desired))  # Log fda_desired for debugging
      
      shinyalert(
        title = "Are you sure?",
        text = "Do you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        cancelButtonText = "No",
        callbackR = function(x) {
          if (x) {
            if (!is.na(fda_desired) && fda_desired != 'Y' && fda_desired != "") {
              print("Row will be deleted")  # Debugging statement
              generated_data$dataset_3_2 <- generated_data$dataset_3_2[-row_index, ]
              output$dataset_table_3_2 <- renderDT({
                datatable(generated_data$dataset_3_2, escape = FALSE, editable = TRUE, rownames = FALSE,
                          options = list(
                            pageLength = 100,  # Show 100 entries by default
                            columnDefs = list(
                              list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                              list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                              list(targets = '_all', className = 'dt-center')
                            )
                          )
                )
              }, server = FALSE)
            } else {
              print("Row cannot be deleted due to FDA Desired = 'Y' or empty")  # Debugging statement
            }
          } else {
            print("Row deletion canceled")  # Debugging statement
          }
        }
      )
    }
  })
  
  observeEvent(input$dataset_table_3_4_cell_clicked, {
    info <- input$dataset_table_3_4_cell_clicked
    print(info)  # Log info for debugging
    
    if (is.null(info) || is.null(info$row) || is.null(info$col) || is.null(info$value)) {
      print("Info is NULL or invalid")
      return()
    }
    
    if (info$col == 0) {  # Change to 0 to reference the first column for delete button
      row_index <- info$row + 1  # Adjust for 1-based indexing
      fda_desired <- generated_data$dataset_3_4[row_index, 'FDA Desired']
      
      print(paste("Deleting row:", info$row))  # Debugging statement
      print(paste("FDA Desired Value:", fda_desired))  # Log fda_desired for debugging
      
      shinyalert(
        title = "Are you sure?",
        text = "Do you want to delete this row?",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        cancelButtonText = "No",
        callbackR = function(x) {
          if (x) {
            if (!is.na(fda_desired) && fda_desired != 'Y' && fda_desired != "") {
              print("Row will be deleted")  # Debugging statement
              generated_data$dataset_3_4 <- generated_data$dataset_3_4[-row_index, ]
              output$dataset_table_3_4 <- renderDT({
                datatable(generated_data$dataset_3_4, escape = FALSE, editable = TRUE, rownames = FALSE,
                          options = list(
                            pageLength = 100,  # Show 100 entries by default
                            columnDefs = list(
                              list(targets = 0, orderable = FALSE), # Disable ordering on the delete button column
                              list(targets = c(2, 8), visible = FALSE), # Hide TSPARMCD and TSPARM
                              list(targets = '_all', className = 'dt-center')
                            )
                          )
                )
              }, server = FALSE)
            } else {
              print("Row cannot be deleted due to FDA Desired = 'Y' or empty")  # Debugging statement
            }
          } else {
            print("Row deletion canceled")  # Debugging statement
          }
        }
      )
    }
  })
  
  # Handle imported data
  observeEvent(input$import_data, {
    req(input$import_data)
    infile <- input$import_data$datapath
    ext <- tools::file_ext(infile)
    validate(need(ext == "xlsx", "Please upload an Excel file (.xlsx)"))
    data <- read.xlsx(infile)
    generated_data$imported_data <- data
  })
  
  # Render imported data table
  output$imported_data_table <- renderDT({
    datatable(generated_data$imported_data, editable = TRUE, rownames = FALSE)
  })
  
  # Download imported data
  output$export_data <- downloadHandler(
    filename = function() {
      paste("exported_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(generated_data$dataset_3_2, file, row.names = FALSE)
    }
  )
  
  # About tab content
  output$about <- renderText({
    "This is an example CDISC Dataset Creator app."
  })
  
  # Handle session ending
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)
