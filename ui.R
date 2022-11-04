library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    
    tabPanel(
      "Map",
      sitePickerInput("sitePicker"),
      h6("Selected site: ", textOutput("site"))
    ),
    
    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )
    
  )
)
