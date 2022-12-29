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
    myTable <- reactive(loadUploads(selectedSiteId()))

    output$table <- renderDT({
      datatable(myTable() |> select(!file_upload_id), 
                selection = "single")
    })

    selectedRow <- reactive(tibble())
    
    observeEvent(input$table_rows_selected,{
      selectedRow <<- reactive(myTable()[input$table_rows_selected,])
    })

    observeEvent(input$btnDelete, {
      req(input$table_rows_selected)
      
      ns <- session$ns
      modal_confirm <- modalDialog(
        paste0("Are you sure you want to delete ",
               selectedRow()$row_count, " rows, including data between ",
               selectedRow()$obs_min_datetime |> as_date(), " and ",
               selectedRow()$obs_max_datetime |> as_date(),
               "?"),
        title = "Deleting datapoints",
        footer = tagList(
          actionButton(ns("btnConfirmDel"), "Delete", class = "btn btn-danger"),
          actionButton(ns("btnCancel"), "Cancel")
        )
      )
      showModal(modal_confirm)
    })
    
    observeEvent(input$btnConfirmDel, {
      print(paste0("Deleting id# ", selectedRow()$file_upload_id))
      deleteUpload(selectedRow()$file_upload_id)
      print(paste0("Before re-loading, myTable has rows: ", nrow(myTable())))
      myTable <<- reactive(loadUploads(selectedSiteId()))
      print(paste0("After re-loading, myTable has rows: ", nrow(myTable())))
      removeModal()
    })

    observeEvent(input$btnCancel, {
      removeModal()
    })
  })
}