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
        "Plots",
        fluidRow(
          plotlyOutput(ns("cfsPlot"))
        ),
        fluidRow(
          plotlyOutput(ns("tempPlot"))
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

dataViewerServer <- function(id, selectedSite) {
  moduleServer(id, function(input, output, session) {
    maxDateRange <- reactive(getSiteDateRange(selectedSite()$site_id))

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
        loadObservations(selectedSite()$site_id,
                         input$dateRange[1], input$dateRange[2])
      )

      output$table <- renderDT({
        validateDates(input$dateRange[1], input$dateRange[2])
        observations()
      }, rownames = FALSE)

      output$cfsPlot <- renderPlotly({
        validateDates(input$dateRange[1], input$dateRange[2])

        plot_ly(observations(), 
                type = "scatter", mode = "lines") |> 
          layout(showlegend = FALSE, 
                 xaxis = list(title = list(text = NULL)), 
                 yaxis = list(title = list(text = "CFS", 
                                           font = list(color = "blue")))) |> 
          add_trace(x = ~datetime, y = ~cfs, 
                    line = list(color = "blue"), name = "CFS")
        
      })
      
      output$tempPlot <- renderPlotly({
        validateDates(input$dateRange[1], input$dateRange[2])
        
        plot_ly(observations(), 
                type = "scatter", mode = "lines") |> 
          layout(showlegend = FALSE, 
                 xaxis = list(title = list(text = NULL)), 
                 yaxis = list(title = list(text = "Deg C", 
                                           font = list(color = "red")))) |> 
          add_trace(x = ~datetime, y = ~temperature_C, 
                    line = list(color = "red"), name = "Deg C")
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