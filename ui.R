library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(

    tabPanel(
      "Map of sites",
      
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        
        tabPanelBody(
          value = "sitePicker",
          sitePickerInput("sitePicker")
          ),
        
        tabPanelBody(
          value = "siteDataView", 
          navlistPanel(
            header = list(
              actionButton("btnReturnMap", "Return to Map of sites"),
              br(),
              h3(textOutput("siteName"), br())
            ),
            widths = c(2, 10),
            tabPanel("View"),
            tabPanel("Upload",
                     uploaderUI("uploader")
                     ),
            tabPanel("Edit Site")
            ),
          ),
      ),
    ),

    tabPanel(
      "Create Site",
      createSiteUI("createSite")
    )

  )
)
