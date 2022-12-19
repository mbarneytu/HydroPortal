library(DT)
dataViewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("siteId")),
    dataTableOutput(ns("dt"))
  )
}

dataViewerServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    output$siteId <- renderText(paste0("Viewing data for site ", selectedSiteId()))
    output$dt <- renderDataTable(loadObservations(selectedSiteId()))
  })
}