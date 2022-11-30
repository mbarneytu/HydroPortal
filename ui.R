library(shiny)
library(leaflet)
library(shinyjs)

ui <- fluidPage(
  useShinyFeedback(),
  useShinyjs(),
  
  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(

    tabPanel(
      "Select site",
      
      tabsetPanel(
        id = "tabPickOrView",
        type = "hidden",
        
        tabPanelBody(
          value = "sitePicker",
          sitePickerInput("sitePicker")
          ),
        
        tabPanelBody(
          value = "siteDataView", 
          tabsetPanel(
            id = "tabSiteFunction",
            type = "pills",
            header = list(
              h3(textOutput("siteName"), br())
            ),
            tabPanel("View",
                     dataViewerUI("dataViewer")
                     ),
            tabPanel("Upload",
                     uploaderUI("uploader")
                     ),
            tabPanel("Edit Site"),
            tabPanel("Select a new site", value = "selectSite")
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
