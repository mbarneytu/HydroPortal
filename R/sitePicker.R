library(shiny)
library(leaflet)
library(dplyr)

sitePickerInput <- function(id) {
  tagList(
    h4("Choose a site from the map:"),
    leafletOutput(NS(id, "map"))
  )
}

sitePickerServer <- function(id, gageSites) {
  moduleServer(id, function(input, output, session) {
    
    viewDataLink <- actionLink(
      inputId = "foo", # id isn't used in this case.
      label = "View Data",
      # Here we use Shiny.setInputValue() to set up a reactive input from the 
      # server. (see: shiny.rstudio.com/articles/communicating-with-js.html)
      # Note that the first argument must be preceded by our module's id.
      onclick = 'Shiny.setInputValue(\"sitePicker-link\", this.id, {priority: "event"})'
    )
    
    output$map <- renderLeaflet({
      leaflet(data = gageSites) |> 
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) |> 
        
        # zoom to Lower 48 states 
        fitBounds(-125.1, 49, -67.1, 25.2) |> 
        
        addMarkers(
          lng = gageSites$lon, lat = gageSites$lat,
          label = gageSites$site_name,
          layerId = gageSites$site_id
          ,
          popup = paste(
            "<b>", gageSites$site_name, "</b></br>",
            viewDataLink
          )
        )
    })

    # Return the value gageSites$site_id for the clicked marker
    mySite <- eventReactive(input$link, input$map_marker_click$id)
    
  })
}