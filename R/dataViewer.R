library(shiny)
library(DT)
library(plotly)
library(scales)
library(lubridate)
library(shinyFeedback)

dataViewerUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    fluidRow(
      dateRangeInput(
        ns("dateRange"), 
        "Date Range"
      ),
    ),
    navlistPanel(
      widths = c(2, 10),
      tabPanel(
        "Plot",
        fluidRow(
          plotlyOutput(ns("plot"))
        )
      ),
      tabPanel(
        "Table",
        fluidRow(
          DTOutput(ns("table"))
        ),
      ),
    ),
  )
}

validateDates <- function(start, end) {
  isValidRange <- start < end
  shinyFeedback::feedbackWarning("dateRange",
                                 !isValidRange,
                                 "Start date must be before end date"
  )
  req(isValidRange)
}

dataViewerServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    maxDateRange <- reactive(getSiteDateRange(selectedSiteId()))

    # Set date range to the most recent 90 days' data
    observeEvent(maxDateRange, {
      updateDateRangeInput(
        inputId = "dateRange",
        start = maxDateRange()$max_dt - ddays(90),
        end = maxDateRange()$max_dt,
        min = maxDateRange()$min_dt,
        max = maxDateRange()$max_dt
      )
    })
    
    observeEvent(input$dateRange, {
      observations <- reactive(
        loadObservations(selectedSiteId(),
                         input$dateRange[1], input$dateRange[2])
      )
      
      output$table <- renderDT({
        validateDates(input$dateRange[1], input$dateRange[2])
        observations()
      })
      
      output$plot <- renderPlotly({
        validateDates(input$dateRange[1], input$dateRange[2])
        
        p <- ggplot(observations(), aes(datetime, cfs)) +
          geom_line(color = "blue") +
          scale_y_continuous(labels = label_number()) +
          scale_x_datetime(name = "")
        
        ggplotly(p) 
      })
    }, ignoreInit = TRUE)
  })
}