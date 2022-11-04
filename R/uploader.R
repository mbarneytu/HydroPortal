library(shiny)
library(readr)
library(shinyFeedback)

uploaderUI <- function(id) {
  tagList(
    shinyFeedback::useShinyFeedback(),
    
    h3("Upload gage data"),
    
    fileInput(NS(id, "file"), NULL, placeholder = "Select a CSV file", accept = ".csv"),
    
    numericInput(NS(id, "skip_rows"), "Header rows to skip (0 or more):", 
                 value = 12, min = 0, max = 100),
    
    h4("Data preview:"),
    tableOutput(NS(id, "preview"))
  )
}

uploaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    csv_contents <- reactive({
      req(input$file)
      
      is_csv <- "csv" == tolower(tools::file_ext(input$file$name))
      shinyFeedback::feedbackWarning("file", !is_csv, "Invalid file; Please select a .csv file")
      req(is_csv)
      
      read_csv(input$file$datapath, 
               col_types = cols(.default = col_character()),
               # col_names = FALSE, 
               col_names = c("date", "time", "stage (ft)", "temp (C)", "discharge (cfs)"), 
               n_max = 100,
               skip = input$skip_rows, na = c("#NUM!"))
      
      
      # For this data preview, may want to force everything to be text, so that it
      # displays verbatim what's in the csv. This will be less confusing to the 
      # user when they're determining which column is which.
      # Then when reading the data "for real", we can let it infer datatypes.
      
    })
    
    output$preview <- renderTable({
      head(csv_contents(), 10)
      
    }, rownames = TRUE, bordered = TRUE)
  })
}
    