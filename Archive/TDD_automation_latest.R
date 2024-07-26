# Install required packages if not installed
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)

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
    tabItems(
      # Create SDTM I.G Version 3.2
      tabItem(
        tabName = "create_dataset_3_2",
        fluidRow(
          textOutput("study_id_display_3_2"), # Display study ID
          selectInput(
            inputId = "domain_selector_3_2",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          numericInput(
            inputId = "num_rows_3_2",
            label = "Number of Rows:",
            value = 1,
            min = 1
          ),
          actionButton(
            inputId = "generate_dataset_3_2",
            label = "Generate Dataset",
            class = "btn-primary"
          )
        ),
        conditionalPanel(
          condition = "input.domain_selector_3_2 == 'TA'",
          textOutput("ta_variable_intro_3_2"),
          DTOutput("ta_dataset_table_3_2")
        ),
        DTOutput("dataset_table_3_2")
      ),
      # Create SDTM I.G Version 3.4
      tabItem(
        tabName = "create_dataset_3_4",
        fluidRow(
          textOutput("study_id_display_3_4"), # Display study ID
          selectInput(
            inputId = "domain_selector_3_4",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          numericInput(
            inputId = "num_rows_3_4",
            label = "Number of Rows:",
            value = 1,
            min = 1
          ),
          actionButton(
            inputId = "generate_dataset_3_4",
            label = "Generate Dataset",
            class = "btn-primary"
          )
        ),
        conditionalPanel(
          condition = "input.domain_selector_3_4 == 'TA'",
          textOutput("ta_variable_intro_3_4"),
          DTOutput("ta_dataset_table_3_4")
        ),
        DTOutput("dataset_table_3_4")
      ),
      # Controlled Terminology Version 2024-03-29
      tabItem(
        tabName = "controlled_terminology_2024_03_29",
        h2("Controlled Terminology Version 2024-03-29 Content")
      ),
      # Controlled Terminology Version 2024-02-15
      tabItem(
        tabName = "controlled_terminology_2024_02_15",
        h2("Controlled Terminology Version 2024-02-15 Content")
      ),
      # Import/Export Data Tab
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
      # About Tab
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
    imported_data = NULL
  )

  # Display study ID
  output$study_id_display_3_2 <- renderText({
    paste("Study ID:", input$study_id)
  })
  output$study_id_display_3_4 <- renderText({
    paste("Study ID:", input$study_id)
  })

  # Display introduction to TA variables for each tab
  output$ta_variable_intro_3_2 <- renderText({
    "Dataset Variables for TA Domain (Version 3.2):"
  })
  output$ta_variable_intro_3_4 <- renderText({
    "Dataset Variables for TA Domain (Version 3.4):"
  })

  # Function to generate dataset for TA domain version 3.4
  generate_TA_dataset_3_4 <- function(num_rows) {
    dataset <- data.frame(
      STUDYID = rep(input$study_id, num_rows),
      DOMAIN = rep("TA", num_rows),
      ARMCD = character(num_rows),
      ARM = character(num_rows),
      TAETORD = integer(num_rows),
      ETCD = character(num_rows),
      ELEMENT = character(num_rows),
      TABRANCH = character(num_rows),
      TATRANS = character(num_rows),
      EPOCH = character(num_rows),
      stringsAsFactors = FALSE
    )
    return(dataset)
  }

  # Function to generate dataset for TE domain version 3.4
  generate_TE_dataset_3_4 <- function(num_rows) {
    dataset <- data.frame(
      STUDYID = rep(input$study_id, num_rows),
      DOMAIN = rep("TE", num_rows),
      ETCD = character(num_rows),
      ELEMENT = character(num_rows),
      TESTRL = character(num_rows),
      TEENRL = character(num_rows),
      TEDUR = character(num_rows),
      stringsAsFactors = FALSE
    )
    return(dataset)
  }

  # Function to generate dataset for TV domain version 3.4
  generate_TV_dataset_3_4 <- function(num_rows) {
    dataset <- data.frame(
      STUDYID = rep(input$study_id, num_rows),
      DOMAIN = rep("TV", num_rows),
      VISITNUM = integer(num_rows),
      VISIT = character(num_rows),
      VISITDY = integer(num_rows),
      ARMCD = character(num_rows),
      ARM = character(num_rows),
      TVSTRL = character(num_rows),
      TVENRL = character(num_rows),
      stringsAsFactors = FALSE
    )
    return(dataset)
  }

  # Function to generate dataset for TI domain version 3.4
  generate_TI_dataset_3_4 <- function(num_rows) {
    dataset <- data.frame(
      STUDYID = rep(input$study_id, num_rows),
      DOMAIN = rep("TI", num_rows),
      USUBJID = character(num_rows),
      IESEQ = integer(num_rows),
      IESPID = character(num_rows),
      IETESTCD = character(num_rows),
      IETEST = character(num_rows),
      IECAT = character(num_rows),
      IESCAT = character(num_rows),
      IEORRES = character(num_rows),
      IESTRESC = character(num_rows),
      VISITNUM = integer(num_rows),
      VISIT = character(num_rows),
      VISITDY = integer(num_rows),
      TAETORD = integer(num_rows),
      EPOCH = character(num_rows),
      IEDTC = character(num_rows),
      IEDY = integer(num_rows),
      stringsAsFactors = FALSE
    )
    return(dataset)
  }

  # Function to generate dataset for TS domain version 3.4
  generate_TS_dataset_3_4 <- function(num_rows) {
    dataset <- data.frame(
      STUDYID = rep(input$study_id, num_rows),
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

  # Generate dataset when "Generate Dataset" button is clicked under "Create Dataset 3.4"
  observeEvent(input$generate_dataset_3_4, {
    req(input$study_id, input$domain_selector_3_4, input$num_rows_3_4)

    # Clear previously displayed datasets
    output$dataset_table_3_4 <- renderDT(NULL)
    output$ta_dataset_table_3_4 <- renderDT(NULL)

    num_rows <- input$num_rows_3_4

    if (input$domain_selector_3_4 == "TA") {
      dataset <- generate_TA_dataset_3_4(num_rows)
      generated_data$dataset_3_4 <- dataset
      output$ta_dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_4 == "TE") {
      dataset <- generate_TE_dataset_3_4(num_rows)
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_4 == "TV") {
      dataset <- generate_TV_dataset_3_4(num_rows)
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_4 == "TI") {
      dataset <- generate_TI_dataset_3_4(num_rows)
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_4 == "TS") {
      dataset <- generate_TS_dataset_3_4(num_rows)
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else {
      dataset <- generate_default_dataset(input$study_id, input$domain_selector_3_4)
      generated_data$dataset_3_4 <- dataset
      output$dataset_table_3_4 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    }
  })

  # Generate dataset when "Generate Dataset" button is clicked under "Create Dataset 3.2"
  observeEvent(input$generate_dataset_3_2, {
    req(input$study_id, input$domain_selector_3_2, input$num_rows_3_2)

    # Clear previously displayed datasets
    output$dataset_table_3_2 <- renderDT(NULL)
    output$ta_dataset_table_3_2 <- renderDT(NULL)

    num_rows <- input$num_rows_3_2

    if (input$domain_selector_3_2 == "TA") {
      dataset <- generate_TA_dataset_3_4(num_rows)  # Use 3.4 as there's no 3.2 function provided
      generated_data$dataset_3_2 <- dataset
      output$ta_dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_2 == "TE") {
      dataset <- generate_TE_dataset_3_4(num_rows)  # Use 3.4 as there's no 3.2 function provided
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_2 == "TV") {
      dataset <- generate_TV_dataset_3_4(num_rows)  # Use 3.4 as there's no 3.2 function provided
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_2 == "TI") {
      dataset <- generate_TI_dataset_3_4(num_rows)  # Use 3.4 as there's no 3.2 function provided
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else if (input$domain_selector_3_2 == "TS") {
      dataset <- generate_TS_dataset_3_4(num_rows)  # Use 3.4 as there's no 3.2 function provided
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    } else {
      dataset <- generate_default_dataset(input$study_id, input$domain_selector_3_2)
      generated_data$dataset_3_2 <- dataset
      output$dataset_table_3_2 <- renderDT({
        datatable(dataset, editable = TRUE, rownames = FALSE)
      })
    }
  })

  # Function to generate default dataset for other domains
  generate_default_dataset <- function(study_id, domain) {
    dataset <- data.frame(
      STUDYID = study_id,
      DOMAIN = domain,
      VARIABLE1 = character(1),
      VARIABLE2 = character(1),
      VARIABLE3 = character(1),
      stringsAsFactors = FALSE
    )
    return(dataset)
  }

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
      write.xlsx(generated_data$imported_data, file, row.names = FALSE)
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
