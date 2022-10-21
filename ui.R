library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    
    tabPanel(
      "Map",
      mapSitesUI("map1")
    ),
    
    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )
  )
)
