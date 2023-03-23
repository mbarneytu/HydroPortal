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
        width = 2,
        h3("Site Location:")
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
    p("Locate site either by entering coordinates or by clicking the map."),
    br(),
    # fluidRow(
    #   column(
    #     width = 2,
    #     radioButtons(
    #       "coordsOrMap",
    #       label = "Specify location by:",
    #       choiceNames = c(
    #         "Entering site coordinates",
    #         "Clicking on the map"
    #       ),
    #       choiceValues = c(
    #         "coords",
    #         "map"
    #       ),
    #       selected = "coords"
    #     )
    #   ),
    #   column(
    #     width = 10,
    fluidRow(
      column(
        offset = 1,
        width = 2,
        numericInput(ns("latEntered"), "Lat:", value = ""),#, width = "120px"
      ),
      column(
        width = 2,
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
    #   ),
    # ),
  )
}

validateLatLong <- function(lat, long) {
  return(
    dplyr::between(lat, USSouth, USNorth) &&
      dplyr::between(long, USWest, USEast)
  )
}

captureCoordinates <- function(lat, long, output, zoom = 10) {
  # Zoom to the coordinates and add marker to the map
  leafletProxy("map") |> 
    addCircleMarkers(lat = lat,
                     lng = long,
                     layerId = "myPin") |> 
    setView(lat = lat,
            lng = long, zoom = zoom)
  
  # Update the selected lat/long, without creating a dependency on the 
  # entered lat/long. 
  output$latSelected <- renderText(round(lat, digits = 5))
  output$longSelected <- renderText(round(long, digits = 6))
}

siteCoordsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Make module later, with inCoords a parameter
    # coords <- siteCoordsServer("siteCoords")
    
    inCoords <- list(lat = NA, long = NA)
    output$latSelected <- renderText(inCoords$lat)
    output$longSelected <- renderText(inCoords$long)
    
    # observeEvent(input$coordsOrMap, {
    #   shinyjs::toggleState("latEntered", input$coordsOrMap == "coords")
    #   shinyjs::toggleState("longEntered", input$coordsOrMap == "coords")
    #   shinyjs::toggleState("btnPlacePin", input$coordsOrMap == "coords")
    # })
    
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
        "latEntered",
        !validCoords,
        text = paste0("Lat must be between ", USSouth, " and ", USNorth,
                      " and Long must be between ", USWest, " and ", USEast
        )
      )
      req(validCoords)
      captureCoordinates(input$latEntered, input$longEntered, output)
    })
    
    observeEvent(input$map_click, {
      # req(input$coordsOrMap == "map")
      # freezeReactiveValue(input, "latEntered")
      # updateTextInput(inputId = "latEntered", value = input$map_click$lat)
      # freezeReactiveValue(input, "longEntered")
      # updateTextInput(inputId = "longEntered", value = input$map_click$lng)
      captureCoordinates(input$map_click$lat, input$map_click$lng, output,
                         zoom = input$map_zoom
      )
    })

    # Return output$latSelected, longSelected as reactive list
    # list(
    #   lat = reactive(clickLat()),
    #   long = reactive(clickLong())
    # )
  })  
}


    