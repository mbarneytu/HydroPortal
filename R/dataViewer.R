library(shiny)
library(DT)
library(plotly)
library(scales)
library(lubridate)
library(shinyFeedback)
library(vroom)

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
        fluidRow(
          downloadButton(ns("btnExport"), label = "Export to .csv")
        )
      ),
    ),
  )
}

validateDates <- function(start, end) {
  isValidRange <- start <= end
  shinyFeedback::feedbackWarning("dateRange",
                                 !isValidRange,
                                 "Start date must be before end date"
  )
  req(isValidRange)
}

dataViewerServer <- function(id, selectedSiteId) {
  moduleServer(id, function(input, output, session) {
    maxDateRange <- reactive(getSiteDateRange(selectedSiteId()))

    # Limit date range to the most recent 90 days' data
    observeEvent(maxDateRange, {
      maxMinus90 <- maxDateRange()$max_dt - ddays(90)

      start_dt <- maxDateRange()$min_dt

    if ( (!is.na(maxMinus90)) & (maxMinus90 > maxDateRange()$min_dt) ) {
        start_dt <- maxMinus90
    }

    updateDateRangeInput(
      inputId = "dateRange",
      start = start_dt,
      end = maxDateRange()$max_dt,
      min = maxDateRange()$min_dt,
      max = maxDateRange()$max_dt
    )
    })

    observations <- reactive(tibble())
    
    observeEvent(input$dateRange, {
      observations <<- reactive(
        loadObservations(selectedSiteId(),
                         input$dateRange[1], input$dateRange[2])
      )

      output$table <- renderDT({
        validateDates(input$dateRange[1], input$dateRange[2])
        observations()
      }, rownames = FALSE)

      output$plot <- renderPlotly({
        validateDates(input$dateRange[1], input$dateRange[2])

        p <- ggplot(observations()) +
          geom_line(aes(datetime, cfs), color = "blue", linewidth = 0.2) +
          geom_line(aes(datetime, temperature_C), color = "red", linewidth = 0.2) +
          scale_y_continuous(labels = label_number()) +
          scale_x_datetime(name = "")

        ggplotly(p)
      })
    }, ignoreInit = TRUE)
    
    output$btnExport <- downloadHandler(
      filename = function() {
        "data.csv"
      },
      content = function(file) {
        vroom::vroom_write(observations(), file, ",")
      }
    )
  })
}