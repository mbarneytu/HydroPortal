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
          plotlyOutput(ns("plots"))
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
    
    generate_subplot <- function(var, label, color){
      plot_ly(observations(), x = ~datetime, y = as.formula(paste0("~", var)),
              height = "600") |> 
        add_lines(name = label, line = list(color = color)) |> 
        layout(showlegend = FALSE, 
               hovermode = "x",
               xaxis = list(title = list(text = NULL)), 
               yaxis = list(title = list(text = label,
                                         font = list(color = color)))
        )
    }
    
    observeEvent(input$dateRange, {
      observations <<- reactive(
        loadObservations(selectedSite()$site_id,
                         input$dateRange[1], input$dateRange[2])
      )

      output$table <- renderDT({
        validateDates(input$dateRange[1], input$dateRange[2])
        observations() |> 
          mutate(datetime = as.character.POSIXt(datetime, format = "%m/%d/%Y %H:%M:%S"))
      }, rownames = FALSE)

      output$plots <- renderPlotly({
        validateDates(input$dateRange[1], input$dateRange[2])

        cfs_plot <- generate_subplot("cfs", "CFS", "blue")
        stage_plot <- generate_subplot("stage_ft", "Stage ft", "green")
        temp_plot <- generate_subplot("temperature_C", "Temp C", "red")
        
        myplots <- list(cfs_plot, stage_plot, temp_plot)
        subplot(myplots, nrows = length(myplots), shareX = TRUE, 
                titleX = FALSE, titleY = TRUE)
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