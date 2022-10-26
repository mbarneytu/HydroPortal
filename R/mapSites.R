library(shiny)
library(leaflet)
library(DBI)

mapSitesUI <- function(id) {
  tagList(
    h4("Choose a site from the map:"),

    tabsetPanel(
      id = NS(id,"switcher"),
      type = "hidden",
      
      tabPanelBody(
        value = "mapView",
        leafletOutput(NS(id, "map"))
      ),
      
      tabPanelBody(
        value = "dataView",
        textOutput(NS(id, "data")),
        actionButton(NS(id, "btnReturnToMap"), "Return to Map")
      )
    )
  )
}

selectSites <- function() {
  query <- "CALL sel_site_locations" # returns lat, lon, site_name, site_id
  res <- dbGetQuery(pool, query)
}

mapSitesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load all sites into a dataframe
    df <- selectSites()
    
    viewDataLink <- actionLink(
      inputId = "foo", # This Id isn't used in this case.
      label = "View Data",
      # Here we use Shiny.setInputValue() to set up a reactive input.
      # (see: shiny.rstudio.com/articles/communicating-with-js.html)
      # Note that the first argument must be preceded by our module's id.
      onclick = 'Shiny.setInputValue(\"map1-link\", this.id, {priority: "event"})'
    )
    
    output$map <- renderLeaflet({
      leaflet(data = df) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        
        # zoom to Lower 48 states 
        fitBounds(-125.1, 49, -67.1, 25.2) %>% 
        
        addMarkers(
          lng = df$lon, lat = df$lat,
          label = df$site_name,
          layerId = df$site_id,
          popup = paste(
            "<b>", df$site_name, "</b></br>",
            viewDataLink
          )
        )
    })
    observeEvent(input$link, {
      myPoint <- reactiveVal(input$map_marker_click)
      
      print(myPoint()$id)
    })
  })
}