library(shiny)
library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    selected = "Map",
    tabPanel(
      "Create Site",
      createSite()
    ),
    
    tabPanel(
      "Map",
      mapSitesUI("map1")
    ),
    
    viewSite()
    
  )
)
