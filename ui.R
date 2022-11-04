library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    
    tabPanel(
      "Map",
      mapSitesUI("sitesMap"),
      h6("Selected site: ", textOutput("site"))
    ),
    
    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )
    
  )
)
