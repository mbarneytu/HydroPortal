library(shiny)
library(readr)
library(shinyFeedback)

uploaderUI <- function(id) {
  tagList(
    shinyFeedback::useShinyFeedback(),
    
    h3("Upload gage data"),
    
    fileInput(NS(id, "file"), NULL, placeholder = "Select a CSV file", accept = ".csv"),
    
    numericInput(NS(id, "skip_rows"), "Header rows to skip (0 or more):", 
                 value = 0, min = 0, max = 100),
    
    h4("Data preview:"),
    tableOutput(NS(id, "preview")),
    actionButton(NS(id, "btnSave"), "Save to Database", 
                 width = "100%", class = "btn-success")
  )
}

uploaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    csvPreview <- reactive({
      req(input$file)
      
      is_csv <- "csv" == tolower(tools::file_ext(input$file$name))
      shinyFeedback::feedbackWarning("file", !is_csv, "Invalid file; Please select a .csv file")
      req(is_csv)
      
      read_csv(input$file$datapath, 
               col_types = cols(.default = col_character()),
               col_names = c("date", "time", "stage (ft)", "temp (C)", "discharge (cfs)"), 
               n_max = 10,
               skip = input$skip_rows, na = c("#NUM!"))
    })
    
    output$preview <- renderTable({
      head(csvPreview(), 10)
      
    }, rownames = TRUE, bordered = TRUE)
    
    observeEvent(input$btnSave, {
      req(input$file)
      csv <- read_csv(input$file$datapath,
                      col_names = c("date", "time", "stage", "temperature", "discharge"),
                      col_types = list(
                        date = col_date(format = "%m/%d/%Y"), 
                        time = col_time(), 
                        stage = col_double(),
                        temperature = col_double(), 
                        discharge = col_double()),
                      skip = input$skip_rows
      )
      
      saveObservations(csv)
    })
  })
}
    