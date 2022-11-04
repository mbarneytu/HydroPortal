library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(

    tabPanel(
      "Map",
      
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        
        tabPanelBody(
          value = "sitePickerPanel",
          sitePickerInput("sitePicker")
          ),
        
        tabPanelBody(
          value = "siteDataView", 
          h4(textOutput("siteName")),
          actionButton("btnReturnMap", "Return to Map")
          )
        
      ),
    ),

    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )

  )
)
