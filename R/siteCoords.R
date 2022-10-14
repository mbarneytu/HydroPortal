siteCoordsUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 3,
             h4("Zoom to coordinates:")
      ),
      column(width = 3,
             numericInput(NS(id, "inLat"), label = "Lat", value = "", step = "any")
      ),
      column(width = 3,
             numericInput(NS(id, "inLong"), label = "Long", value = "", step = "any")
      )
    ),
    fluidRow(
      
      column(width = 3,
             actionButton(NS(id, "zoomBtn"), label = "Zoom", class = "btn-success"))
    ),
    
    br(),
    fluidRow(
      column(width = 12,
             leafletOutput(NS(id, "map"))
      )
    ),
    
    br(),
    fluidRow(
      column(width = 2, offset = 1, 
             actionButton(NS(id, "pinBtn"), label = "Place Pin at Map Center",
                          class = "btn-info")
      ),
      column(width = 1, 
             h5("Pin Lat:")
      ),
      column(width = 1,
             textOutput(NS(id, "pinLat"))
      ),
      column(width = 1, 
             h5("Pin Long:")
      ),
      column(width = 5,
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
    
    observeEvent(input$pinBtn, {
      centerLat <- round(input$map_center$lat, digits = 5)
      centerLong <- round(input$map_center$lng, digits = 6)
      
      leafletProxy("map") %>%
        addCircleMarkers(lng = centerLong, lat = centerLat, layerId = "myPin")
      
      output$pinLat <- renderText(centerLat)
      output$pinLong <- renderText(centerLong)
    })
  })  
}


    