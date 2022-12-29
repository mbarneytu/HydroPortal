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
      print(paste0("row # selected: ", input$table_rows_selected,
                   ". Upload_id: ", selectedRow()$file_upload_id))
    })
    

    observeEvent(input$btnDelete, {
      req(input$table_rows_selected)
      print(paste0("would have deleted id# ", selectedRow()$file_upload_id))
    })
    
    #   index <- input$table_rows_selected
    #   if (length(index)) {
    #     selectedRow <<- isolate(myTable()[index,])
    #     
    #     # req(selectedRow)
    #     ns <- session$ns
    #     modal_confirm <- modalDialog(
    #       paste0("Are you sure you want to delete ",
    #              selectedRow$row_count, " rows, including data between ",
    #              selectedRow$obs_min_datetime |> as_date(), " and ",
    #              selectedRow$obs_max_datetime |> as_date(),
    #              "?"),
    #       title = "Deleting datapoints",
    #       footer = tagList(
    #         actionButton(ns("btnConfirmDel"), "Delete", class = "btn btn-danger"),
    #         actionButton(ns("btnCancel"), "Cancel")
    #       )
    #     )
    #     showModal(modal_confirm)
    #   }
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$btnConfirmDel, {
    #   print(paste0("would have deleted id# ", isolate(selectedRow$file_upload_id)))
    #   # deleteUpload(selectedRow$file_upload_id)
    #   # myTable <<- reactive(loadUploads(selectedSiteId()))
    #   # selectedRow <<- tibble()
    #   removeModal()
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$btnCancel, {
    #   removeModal()
    # })
  })
}