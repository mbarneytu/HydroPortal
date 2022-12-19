library(shiny)
library(readr)
library(shinyFeedback)

uploaderUI <- function(id) {
  tagList(
    textOutput(NS(id,"siteId")),
    
    div(id = NS(id, "thisUI"),
      h4("Upload gage data"),
      
      fluidPage(
        fluidRow(
          column(width = 6,
                 fileInput(NS(id, "file"), label = "CSV file", 
                           accept = ".csv"),
          ),
          column(width = 6,
                 numericInput(NS(id, "skip_rows"), "Skip header rows? (0 or more):", 
                              value = 0, min = 0, max = 100),
          ),
          
        ),
        shinyjs::hidden(
          div(id = NS(id, "previewDiv"),
              h4("Data preview:"),
              tableOutput(NS(id, "preview")),
              
              h5(strong("Records loaded:"), textOutput(NS(id, "numRecords"), inline = TRUE),
                 strong(" Dates:"), textOutput(NS(id, "earliestDate"), inline = TRUE),
                 strong(" to"), textOutput(NS(id, "latestDate"), inline = TRUE)
              ),
              actionButton(NS(id, "btnSave"), "Save to Database", 
                           width = "100%", class = "btn-success")
          )
          
        )
        )
      )
  )
}

resetUploaderUI <- function(output) {
  shinyjs::reset("thisUI")
  # output$preview <- renderTable(NULL) #<< This prevents preview from ever showing.
  output$numRecords <- renderText("")
  output$earliestDate <- renderText("")
  output$latestDate <- renderText("")
}

uploaderServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    
    output$siteId <- renderText(paste0("Uploading data for site ", selectedSiteId()))

    # If selected site has changed, clear inputs & outputs
    observeEvent(selectedSiteId, {
      resetUploaderUI(output) 
    })
    
    csvFile <- reactive({
      req(input$file)
      
      is_csv <- "csv" == tolower(tools::file_ext(input$file$name))
      shinyFeedback::feedbackWarning("file", !is_csv, 
                                     "Invalid file; Please select a .csv file")
      req(is_csv)
      
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
    
    observeEvent(csvFile(), {
      shinyjs::toggle("previewDiv")
    })
    
    output$preview <- renderTable({
      req(csvFile)
      head(csvFile()) |> 
        mutate(date = as.character(date), time = as.character(time))
      },
      rownames = TRUE, bordered = TRUE)

    csvStats <- reactive(csvFile() |>
      summarize(minDT = min(date),
                maxDT = max(date),
                count = n())
    )

    output$numRecords <- renderText(csvStats()$count)
    output$earliestDate <- renderText(as.character(csvStats()$minDT))
    output$latestDate <- renderText(as.character(csvStats()$maxDT))
    
    observeEvent(input$btnSave, {
      req(csvFile)

      saveObservations(csvFile(), selectedSiteId())
    })
  })
}
    