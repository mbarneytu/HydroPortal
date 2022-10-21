library(shiny)
library(shinyFeedback)
library(leaflet)

siteCoordsUI <- function(id) {
  tagList(
    useShinyFeedback(),
    fluidRow(
      column(width = 4,
             h4("Zoom to coordinates:")
      ),
      column(width = 4,
             numericInput(NS(id, "inLat"), label = "Lat", value = "", step = "any")
      ),
      column(width = 4,
             numericInput(NS(id, "inLong"), label = "Long", value = "", step = "any")
      )
    ),
    fluidRow(

      column(width = 12,
             actionButton(NS(id, "zoomBtn"), label = "Zoom", class = "btn-info"))
    ),

    br(),
    fluidRow(
      column(width = 12,
             leafletOutput(NS(id, "map")))
    ),
    
    br(),
    fluidRow(
      column(width = 5, 
             actionButton(NS(id, "pinBtn"), label = "Place Site at Map Center",
                          class = "btn-info")
      ),
      column(width = 1, 
             p(tags$b("*Pin Lat:"))
      ),
      column(width = 2,
             textOutput(NS(id, "pinLat"))
      ),
      column(width = 1,
             p(tags$b("*Pin Long:"))
      ),
      column(width = 2,
             textOutput(NS(id, "pinLong"))
      )
    )
  )
}

validatePoint <- function(input, minLat, maxLat, minLong, maxLong) {
  
  validLat <- (!is.na(input$inLat)
               && input$inLat >= minLat
               && input$inLat <= maxLat
  )
  validLong <- (!is.na(input$inLong)
                && input$inLong >= minLong
                && input$inLong <= maxLong
  )
  
  feedbackWarning("inLat", !validLat, 
                  text = paste0("Lat must be between ", 
                                minLat, " and ", maxLat)
  )
  feedbackWarning("inLong", !validLong,
                  text = paste0("Long must be between ", 
                                minLong, " and ", maxLong)
  )
  req(validLat, validLong)
}

siteCoordsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minLat <- 25
    maxLat <- 49
    minLong <- -125
    maxLong <- -67
    centerLat <- 38
    centerLong <- -96
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        
        fitBounds(minLong, maxLat, maxLong, minLat) 
    })
    
    observeEvent(input$zoomBtn, {

      validatePoint(input, minLat, maxLat, minLong, maxLong)
      leafletProxy("map") %>%
        setView(lng = input$inLong, lat = input$inLat, zoom = 14)
    })

    clickLat <- eventReactive(input$pinBtn, 
                              round(input$map_center$lat, digits = 5))
    clickLong <- eventReactive(input$pinBtn, 
                               round(input$map_center$lng, digits = 6))
    
    observeEvent(input$pinBtn, {
      leafletProxy("map") %>%
        addCircleMarkers(lng = clickLong(),
                         lat = clickLat(),
                         layerId = "myPin")

      output$pinLat <- renderText(clickLat())
      output$pinLong <- renderText(clickLong())
    })
    
    list(
      lat = reactive(clickLat()),
      long = reactive(clickLong())
    )
  })  
}


    