library(shiny)
library(shinyFeedback)
library(leaflet)

# US map bounds
USSouth <- 25
USNorth <- 49
USWest <- -125
USEast <- -67

siteCoordsUI <- function(id) {
  ns <- NS(id)
  useShinyFeedback()
  tagList(
    fluidRow(
      column(
        width = 4,
        h4("Site Location:")
      ),
      column(
        width = 4,
        h4("Latitude: "),
        h4(textOutput(ns("latSelected")))
      ),
      column(
        width = 4,
        h4("Longitude: "),
        h4(textOutput(ns("longSelected")))
      )
    ),
    hr(),
    
    p("Locate site either by entering coordinates or by clicking the map:"),
    fluidRow(
      column(
        width = 4,
        numericInput(ns("latEntered"), "Lat:", value = ""),#, width = "120px"
      ),
      column(
        width = 4,
        numericInput(ns("longEntered"), "Long:", value = "")
      ),
      column(
        width = 4,
        br(),
        actionButton(ns("btnPlacePin"), "Place Pin", class = "btn-info")
      )
    ),
    fluidRow(
      column(
        width = 12,
        leafletOutput(ns("map"))
      )
    ),
  )
}

validateLatLong <- function(lat, long) {
  return(
    !is.null(lat) &&
      !is.null(long) &&
      dplyr::between(lat, USSouth, USNorth) &&
      dplyr::between(long, USWest, USEast)
  )
}

mapCoordinates <- function(mapproxy, lat, long, zoom = 10) {
  if (is.numeric(lat) && is.numeric(long)) {
    # Zoom to the coordinates and add marker to the map
    mapproxy |> 
      addCircleMarkers(lat = lat,
                       lng = long,
                       layerId = "myPin") |> 
      setView(lat = lat,
              lng = long, zoom = zoom)
  }
}

siteCoordsServer <- function(id, inLat = NA, inLong = NA) {
  moduleServer(id, function(input, output, session) {
    stopifnot(!(is.reactive(inLat)))
    stopifnot(!(is.reactive(inLong)))
    
    newCoords <- reactiveValues(
      lat = inLat, 
      long = inLong
    )

    output$latSelected <- renderText(newCoords$lat)
    output$longSelected <- renderText(newCoords$long)
    
    observeEvent(c(newCoords$lat, newCoords$long), {
    # observeEvent(newCoords, {
      # print("mapCoordinates()")
      mapCoordinates(leafletProxy("map"), newCoords$lat, newCoords$long)
    })
    
    output$map <- renderLeaflet({
      leaflet() |>  
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) |> 
        fitBounds(USWest, USNorth, USEast, USSouth) 
    })
    
    observeEvent(input$btnPlacePin, {
      validCoords <- validateLatLong(input$latEntered, input$longEntered)
      shinyFeedback::feedbackWarning(
        "longEntered",
        !validCoords,
        text = paste0("Lat must be between ", USSouth, " and ", USNorth,
                      " and Long must be between ", USWest, " and ", USEast
        )
      )
      req(validCoords)
      newCoords$lat <- round(input$latEntered, digits = 6)
      newCoords$long <- round(input$longEntered, digits = 6)
    })
    
    observeEvent(input$map_click, {
      newCoords$lat <- round(input$map_click$lat, digits = 6)
      newCoords$long <- round(input$map_click$lng, digits = 6)
    })
    newCoords
  })
}


    