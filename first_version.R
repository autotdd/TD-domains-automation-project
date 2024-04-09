# Load required packages

library(shiny)
library(shinydashboard)
library(DT)  # for interactive tables
library(dplyr)
library(readxl)  # for importing data
library(openxlsx)  # for exporting data

# Sample data for all 5 domains
sample_data_TA <- data.frame(
  STUDYID = c("Study1", "Study1"),
  DOMAIN = c("TA", "TA"),
  ARMCD = c("Arm1", "Arm2"),
  ARM = c("Arm A", "Arm B"),
  TAETORD = c(1, 2),
  ETCD = c("ET1", "ET2"),
  ELEMENT = c("Elem1", "Elem2"),
  TABRANCH = c("Branch1", "Branch2"),
  TATRANS = c("Trans1", "Trans2"),
  EPOCH = c("Epoch1", "Epoch2")
)

# Sample data for TE domain
sample_data_TE <- data.frame(
  STUDYID = c("Study1", "Study1"),
  DOMAIN = c("TE", "TE"),
  ETCD = c("ET1", "ET2"),
  ELEMENT = c("Elem1", "Elem2"),
  TESTRL = c("Test1", "Test2"),
  TEENRL = c(100, 200),
  TEDUR = c(2, 3)
)

# Sample data for TV domain
sample_data_TV <- data.frame(
  STUDYID = c("Study1", "Study1"),
  DOMAIN = c("TV", "TV"),
  VISITNUM = c(1, 2),
  VISIT = c("Visit1", "Visit2"),
  VISITDY = c(1, 2),
  ARMCD = c("Arm1", "Arm2"),
  ARM = c("Arm A", "Arm B"),
  TVSTRL = c("TVStart1", "TVStart2"),
  TVENRL = c("TVEnd1", "TVEnd2")
)

# Sample data for TI domain
sample_data_TI <- data.frame(
  STUDYID = c("Study1", "Study1"),
  DOMAIN = c("TI", "TI"),
  IETESTCD = c("IE1", "IE2"),
  IETEST = c("IE Test 1", "IE Test 2"),
  IECAT = c("Cat1", "Cat2"),
  IESCAT = c("SubCat1", "SubCat2"),
  TIRL = c("TIRL1", "TIRL2"),
  TIVERS = c("TIVERS1", "TIVERS2")
)

# Sample data for TS domain
sample_data_TS <- data.frame(
  STUDYID = c("Study1", "Study1"),
  DOMAIN = c("TS", "TS"),
  TSSEQ = c(1, 2),
  TSGRPID = c("Group1", "Group2"),
  TSPARMCD = c("Parm1", "Parm2"),
  TSPARM = c("ParmVal1", ""),
  TSVAL = c("Value1", "Value2"),
  TSVALNF = c("ValueNF1", "ValueNF2"),
  TSVALCD = c("ValueCD1", "ValueCD2"),
  TSVCDREF = c("Ref1", "Ref2"),
  TSVCDVER = c("Ver1", "Ver2")
)

# Define CDISC controlled terminology values for TSVALNF and TSVALCD
cdisc_terminology <- list(
  TSVALNF = c("Term1", "Term2", "Term3"),
  TSVALCD = c("CDTerm1", "CDTerm2", "CDTerm3")
)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Trial Domains Design (TDD) Master"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Create Datasets", tabName = "create_datasets"),
      menuItem("Import/Export Data", tabName = "import_export"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      # Create Datasets Tab
      tabItem(
        tabName = "create_datasets",
        fluidRow(
          # Domain Selector
          selectInput(
            inputId = "domain_selector",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TA"
          ),
          # Number of Rows Selector
          numericInput(
            inputId = "num_rows",
            label = "Number of Rows:",
            value = 5,
            min = 1
          ),

          textInput(inputId = "studyid",
                    label = "Please enter your study name",
                    value =""),
          # Button to Generate Dataset
          actionButton(
            inputId = "generate_dataset",
            label = "Generate Dataset",
            class = "btn-primary"
          )
        ),
        # Rendered Dataset Table
        DTOutput("dataset_table")
      ),
      # Import/Export Data Tab
      tabItem(
        tabName = "import_export",
        fluidRow(
          # Import Data
          fileInput(
            inputId = "import_data",
            label = "Import Data (Excel)",
            accept = c(".xlsx")
          ),
          # Export Data
          downloadButton(
            outputId = "export_data",
            label = "Export Data (Excel)",
            class = "btn-success"
          )
        ),
        br(),
        # Rendered Imported Data Table
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
server <- function(input, output) {
  # Reactive function to generate datasets based on selected domain
  generated_data <- reactive({
    domain <- input$domain_selector
    num_rows <- input$num_rows
    switch(
      domain,
      "TA" = sample_data_TA[1:num_rows, ] %>% mutate(DOMAIN = input$domain_selector, STUDYID = input$studyid),
      "TE" = sample_data_TE[1:num_rows, ] %>% mutate(DOMAIN = input$domain_selector,STUDYID = input$studyid),
      "TV" = sample_data_TV[1:num_rows, ] %>% mutate(DOMAIN = input$domain_selector,STUDYID = input$studyid),
      "TI" = sample_data_TI[1:num_rows, ] %>% mutate(DOMAIN = input$domain_selector,STUDYID = input$studyid),
      "TS" = sample_data_TS[1:num_rows, ] %>% mutate(DOMAIN = input$domain_selector,STUDYID = input$studyid),
    )
  }
  )
  # Render generated dataset table
  output$dataset_table <- renderDT({
    datatable(generated_data(), editable = TRUE)
  })
  # Reactive function to import data
  imported_data <- reactive({
    req(input$import_data)
    infile <- input$import_data$datapath
    ext <- tools::file_ext(infile)
    validate(need(ext == "xlsx", "Please upload an Excel file (.xlsx)"))
    read.xlsx(infile)
  })
  # Render imported data table
  output$imported_data_table <- renderDT({
    datatable(imported_data(), editable = TRUE)
  })
  # Download button for exporting data
  output$export_data <- downloadHandler(
    filename = function() {
      paste("exported_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(generated_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)


