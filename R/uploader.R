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
    
    h4("Summary:"),
    h5(strong("Records loaded:"), textOutput(NS(id, "numRecords"), inline = TRUE),
       strong(" Dates:"), textOutput(NS(id, "earliestDate"), inline = TRUE),
       strong(" to"), textOutput(NS(id, "latestDate")), inline = TRUE),
    
    actionButton(NS(id, "btnSave"), "Save to Database", 
                 width = "100%", class = "btn-success")
  )
}

uploaderServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    csvPreview <- reactive({
      req(input$file)
      
      is_csv <- "csv" == tolower(tools::file_ext(input$file$name))
      shinyFeedback::feedbackWarning("file", !is_csv, 
                                     "Invalid file; Please select a .csv file")
      req(is_csv)
      
      # read_csv(input$file$datapath, 
      #          col_types = cols(.default = col_character()),
      #          col_names = c("date (mm/dd/yyyy)", "time", "stage (ft)", "temp (C)", 
      #                        "discharge (cfs)"), 
      #          skip = input$skip_rows, na = c("#NUM!"))
      
      read_csv(input$file$datapath,
               col_names = c("date", "time", "stage", "temperature", "discharge"),
               col_types = list(
                 date = col_date(format = "%m/%d/%Y"), 
                 time = col_time(), 
                 stage = col_double(),
                 temperature = col_double(), 
                 discharge = col_double()),
               skip = input$skip_rows
      )
    })
    
    output$preview <- renderTable({
      head(csvPreview()) |> 
        mutate(date = as.character(date), time = as.character(time))
      },
      rownames = TRUE, bordered = TRUE)

    csvStats <- reactive(csvPreview() |>
      summarize(minDT = min(date),
                maxDT = max(date),
                count = n())
    )

    output$numRecords <- renderText(csvStats()$count)
    output$earliestDate <- renderText(as.character(csvStats()$minDT))
    output$latestDate <- renderText(as.character(csvStats()$maxDT))
    
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
      
      saveObservations(csv, selectedSiteId)
    })
  })
}
    