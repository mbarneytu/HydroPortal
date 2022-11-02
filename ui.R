library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    
    tabPanel(
      "Map",
      mapSitesUI("sitesMap")
    ),
    
    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )
    
  )
)
