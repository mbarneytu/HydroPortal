library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    
    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    ),
    
    tabPanel(
      "Map",
      mapSitesUI("map1")
    ),
    
    viewSite()
    
  )
)
