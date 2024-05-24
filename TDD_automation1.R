install.packages("shiny")
install.packages("shinydashboard")
install.packages("DT")
install.packages("openxlsx")
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
          textOutput("study_id_display"), # Display study ID
          conditionalPanel(
            condition = "input.domain_selector == 'TA'",
            textOutput("ta_variable_intro"),
            DTOutput("ta_dataset_table")
          ),
          selectInput(
            inputId = "domain_selector",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          actionButton(
            inputId = "generate_dataset_3_2",
            label = "Generate Dataset",
            class = "btn-primary"
          )
        ),
        DTOutput("dataset_table")
      ),
      # Create SDTM I.G Version 3.4
      tabItem(
        tabName = "create_dataset_3_4",
        fluidRow(
          textOutput("study_id_display"), # Display study ID
          conditionalPanel(
            condition = "input.domain_selector == 'TA'",
            textOutput("ta_variable_intro"),
            DTOutput("ta_dataset_table")
          ),
          selectInput(
            inputId = "domain_selector",
            label = "Select Domain:",
            choices = c("TA", "TE", "TV", "TI", "TS"),
            selected = "TS"
          ),
          actionButton(
            inputId = "generate_dataset_3_4",
            label = "Generate Dataset",
            class = "btn-primary"
          )
        ),
        DTOutput("dataset_table")
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
  # Display study ID
  output$study_id_display <- renderText({
    paste("Study ID:", input$study_id)
  })

  # Display introduction to TA variables
  output$ta_variable_intro <- renderText({
    "Dataset Variables for TA Domain:"
  })

  # Function to generate dataset for TA domain
  generate_TA_dataset <- function() {
    # Create dataset with TA variables
    dataset <- data.frame(
      STUDYID = input$study_id,
      DOMAIN = "TA",
      ARMCD = "",
      ARM = "",
      TAETORD = numeric(0),
      ETCD = "",
      ELEMENT = "",
      TABRANCH = "",
      TATRANS = "",
      EPOCH = ""
    )
    return(dataset)
  }

  # Generate dataset when "Generate Dataset" button is clicked under "Create Dataset 3.2"
  observeEvent(input$generate_dataset_3_2, {
    if(input$domain_selector == "TA") {
      dataset <- generate_TA_dataset()
      output$ta_dataset_table <- renderDT({
        datatable(dataset, editable = TRUE)
      })
    } else {
      # Generate default dataset
      dataset <- generate_default_dataset()
      output$dataset_table <- renderDT({
        datatable(dataset, editable = TRUE)
      })
    }
  })

  generated_data <- reactiveVal(NULL)

  observeEvent(input$import_data, {
    req(input$import_data)
    infile <- input$import_data$datapath
    ext <- tools::file_ext(infile)
    validate(need(ext == "xlsx", "Please upload an Excel file (.xlsx)"))
    data <- read.xlsx(infile)
    generated_data(data)
  })

  output$dataset_table <- renderDT({
    datatable(generated_data(), editable = TRUE, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "  var FDA_Column_Index = -1;",
        "  var columnIndex = 0;",
        "  table = settings.oInstance.api();",
        "  table.columns().every(function() {",
        "    var title = this.header().innerHTML.trim();",
        "    if (title === 'FDA.Desired') {",
        "      FDA_Column_Index = columnIndex;",
        "      return false;",
        "    }",
        "    columnIndex++;",
        "  });",
        "  if (FDA_Column_Index !== -1) {",
        "    table.column(FDA_Column_Index).nodes().to$().closest('td').each(function() {",
        "      var cell = this;",
        "      if ($(cell).text().trim() === 'Conditional') {",
        "        $('<button>Add Row</button>').appendTo($(cell).parent());",
        "      }",
        "    });",
        "    $(document).on('click', 'button', function() {",
        "      var rowIndex = $(this).closest('tr').index();",
        "      table.row.add(['', '', '', '', '']).draw();",
        "    });",
        "  }",
        "}"
      )
    ))
  })

  output$imported_data_table <- renderDT({
    datatable(generated_data(), editable = TRUE)
  })

  output$export_data <- downloadHandler(
    filename = function() {
      paste("exported_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(generated_data(), file, row.names = FALSE)
    }
  )

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)

