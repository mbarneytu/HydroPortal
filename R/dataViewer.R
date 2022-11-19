library(DT)
dataViewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("dt"))
  )
}

dataViewerServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    output$dt <- renderDataTable(loadObservations(selectedSiteId))
  })
}