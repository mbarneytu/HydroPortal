library(shiny)
library(leaflet)
library(DBI)

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

selectSites <- function() {
  query <- "CALL sel_site_locations"
  res <- dbGetQuery(pool, query)
}

mapSitesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <- selectSites()
    
    output$map <- renderLeaflet({
      leaflet(data = df) %>% 
        
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        
        fitBounds(-125.1, 49, -67.1, 25.2) %>% # zoom to Lower 48 states 
        
        addMarkers(~lon, ~lat, popup = ~as.character(site_name), 
                   label = ~as.character(site_name))
    })

    observeEvent(input$map_click, {
      # Handle map clicks
    })
  })
}