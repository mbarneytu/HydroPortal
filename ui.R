library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(

    tabPanel(
      "Map Sites",
      
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
            header = h4(textOutput("siteName")),
            footer = actionButton("btnReturnMap", "Return to Map"),
            widths = c(2, 10),
            tabPanel("View"),
            tabPanel("Upload",
                     uploadDataUI("uploader")
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
