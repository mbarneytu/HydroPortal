library(shiny)
library(leaflet)
library(DBI)
library(dplyr)

mapSitesUI <- function(id) {
  tagList(

    tabsetPanel(
      id = NS(id, "switcher"),
      type = "hidden",
      
      tabPanelBody(
        value = "mapView",
        h4("Choose a site from the map:"),
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
  # Using raw SQL to work around a weird SP error that happens only on shinyapps.io
  # query <- "CALL sel_site_locations" # returns lat, lon, site_name, site_id
  query <- "SELECT lat, lon, site_name, site_id FROM site"
  res <- as_tibble(dbGetQuery(pool, query))
}

mapSitesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load all sites into a tibble
    sites <- selectSites()
    
    # print(sites)
    
    viewDataLink <- actionLink(
      inputId = "foo", # id isn't used in this case.
      label = "View Data",
      # Here we use Shiny.setInputValue() to set up a reactive input from the 
      # server. (see: shiny.rstudio.com/articles/communicating-with-js.html)
      # Note that the first argument must be preceded by our module's id.
      onclick = 'Shiny.setInputValue(\"map1-link\", this.id, {priority: "event"})'
    )
    
    output$map <- renderLeaflet({
      leaflet(data = sites) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        
        addLayersControl(baseGroups = c("Topo", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        
        # zoom to Lower 48 states 
        fitBounds(-125.1, 49, -67.1, 25.2) %>% 
        
        addMarkers(
          lng = sites$lon, lat = sites$lat,
          label = sites$site_name,
          layerId = sites$site_id,
          popup = paste(
            "<b>", sites$site_name, "</b></br>",
            viewDataLink
          )
        )
    })
    observeEvent(input$link, {
      myPoint <- reactiveVal(input$map_marker_click)
      
      # print(myPoint()$id)
      
      mySite <- sites %>% filter(site_id == myPoint()$id)
      
      updateTabsetPanel(inputId = "switcher", selected = "dataView")
      output$data <- renderText(paste0("Data for ", mySite$site_name))
    })
    
    observeEvent(input$btnReturnToMap, {
      updateTabsetPanel(inputId = "switcher", selected = "mapView")
    })
  })
}