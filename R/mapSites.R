library(shiny)
library(leaflet)

mapSitesUI <- function(id) {
  tagList(
    titlePanel("View Sites"),
    fluidRow(
      column(width = 12,
             leafletOutput(NS(id, "map"))
      )
    )
  )
}

mapSitesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>% 
        
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        
        fitBounds(-125.1, 49, -67.1, 25.2) # zoom to Lower 48 states
    })
  })
}