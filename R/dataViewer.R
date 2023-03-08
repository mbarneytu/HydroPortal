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
          "Filter by Date Range",
          format = "mm/dd/yyyy"
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
    
    # Determine how much data exist for the selected site_id
    siteDateRange <- reactive(getSiteDateRange(selectedSite()$site_id))

    observeEvent(siteDateRange, {
      
      if (is.null(siteDateRange)) {
        # No observations exist; set the dateRange control to today's date
        updateDateRangeInput(
          inputId = "dateRange",
          start = today(),
          end = today(),
          min = today(),
          max = today()
        )
      } else { 
        # Set reasonable date range default values
        updateDateRangeInput(
          inputId = "dateRange",
          start = siteDateRange()$viewStartDt,
          end = siteDateRange()$latestDt,
          min = siteDateRange()$earliestDt,
          max = siteDateRange()$latestDt
        )  
      }
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
        vroom::vroom_write(observations() |> transmute(
          date = as.character.POSIXt(datetime, format = "%m/%d/%Y"), 
          time = as.character.POSIXt(datetime, format = "%H:%M:%S"),
          stage_ft, temperature_C, cfs),
          file, delim = ",", 
          bom = TRUE) #Byte Order Mark is recommended for Excel
      }
    )
  })
}