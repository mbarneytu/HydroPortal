library(shiny)
library(leaflet)
library(dplyr)

sitePickerInput <- function(id) {
  tagList(
    h4("Choose a site from the map:"),
    leafletOutput(NS(id, "map"))
  )
}

sitePickerServer <- function(id, gageSites, selectedSite) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(gageSites))
    stopifnot(is.reactive(selectedSite))
    
    # Here we use Shiny.setInputValue() to set up a reactive input from the 
    # server. (see: shiny.rstudio.com/articles/communicating-with-js.html)
    viewDataLink <- actionLink(
      inputId = "foo", # id isn't used in this case.
      label = "View Data",
      
      # Note that the name of the input must be preceded by our module's id.
      onclick = 'Shiny.setInputValue(\"sitePicker-link\", this.id, {priority: "event"})'
    )
    
    output$map <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) |> 
        
        # zoom to Lower 48 states 
        fitBounds(-125.1, 49, -67.1, 25.2) |> 
        
        addMarkers(
          lng = gageSites()$lon, lat = gageSites()$lat,
          label = paste0(gageSites()$user_site_id, " - ", gageSites()$site_name),
          layerId = gageSites()$site_id,
          popup = paste0(
            "<b>", 
            gageSites()$user_site_id, " - ", gageSites()$site_name, 
            "</b>",
            "</br>",
            viewDataLink
          )
        )
    })

    # When the link is clicked, update selectedSite value
    observeEvent(input$link, {
      selectedSite(gageSites() |>
                     filter(site_id == input$map_marker_click$id))
    })
  })
}