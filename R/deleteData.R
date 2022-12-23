library(DT)
library(lubridate)

deleteDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      actionButton(ns("btnDelete"), label = "Delete selected upload", 
                   class = "btn-danger"),
    ),
    h4("Data upload events:"),
    fluidRow(
      DTOutput(ns("table"))
    ),
  )
}

deleteDataServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    
    myTable <- eventReactive(selectedSiteId, {
      loadUploads(selectedSiteId())
    })
    
    output$table <- renderDT({
      req(myTable())
      datatable(myTable() |> select(!file_upload_id), 
                selection = "single")
    })
    
    observeEvent(input$btnDelete, {
      index = input$table_rows_selected
      selectedRow <- myTable()[index,]
      if (length(index)) {
        
        ns <- session$ns
        modal_confirm <- modalDialog(
          paste0("Are you sure you want to delete ", 
                 selectedRow$row_count, " rows, including data between ",
                 selectedRow$obs_min_datetime |> as_date(), " and ",
                 selectedRow$obs_max_datetime |> as_date(),
                 "?"),
          title = "Deleting datapoints",
          footer = tagList(
            actionButton(ns("btnConfirmDel"), "Delete", class = "btn btn-danger"),
            actionButton(ns("btnCancel"), "Cancel")
          )
        )
        showModal(modal_confirm)
        
        observeEvent(input$btnConfirmDel, {
          deleteUpload(selectedRow$file_upload_id)
          removeModal()
        })

        observeEvent(input$btnCancel, {
          removeModal()
        })
      }
    })
  })
}