library(shiny)
library(readr)
library(shinyFeedback)

uploaderUI <- function(id) {
  tagList(
    div(id = NS(id, "thisUI"),
      h4("Upload gage data"),
      
      fluidPage(
        fluidRow(
          fileInput(NS(id, "file"), label = "CSV file", accept = ".csv"),

        ),
        shinyjs::hidden(
          div(id = NS(id, "previewDiv"),
              h4("Data preview:"),
              tableOutput(NS(id, "preview")),
              
              h5(strong("# Records loaded:"), textOutput(NS(id, "numRecords"), inline = TRUE),
                 strong(" Date range:"), textOutput(NS(id, "earliestDate"), inline = TRUE),
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
  output$preview <- renderTable(NULL)
  output$numRecords <- renderText("")
  output$earliestDate <- renderText("")
  output$latestDate <- renderText("")
}

uploaderServer <- function(id, selectedSite) {
  moduleServer(id, function(input, output, session) {

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
                 discharge = col_double())
      )
    })
    
    observeEvent(csvFile(), {
      shinyjs::toggle("previewDiv")
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
    })

    fileName <- eventReactive(input$file, input$file$name)
    filePath <- eventReactive(input$file, input$file$datapath)
    
    observeEvent(input$btnSave, {
      req(csvFile)

      result <- saveObservations(csvFile(), selectedSite()$site_id, 
                                 fileName(), filePath())
      if (!result$success) {
        ns <- session$ns
        modal_errormsg <- modalDialog(
          h4("The following error was returned by the database:"), 
          p(result$message),
          title = h3("Data was not saved", style = "color:red"),
          footer = tagList(
            modalButton("Ok")
          )
        )
        showModal(modal_errormsg)
      }

      else {
        resetUploaderUI(output)
        shinyjs::toggle("previewDiv")
      }
    })
  })
}
    