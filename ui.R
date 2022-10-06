library(shiny)
library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  tabsetPanel(

    createSite(),
    
    mapSites(),
    
    viewSite()
    
  )
)
