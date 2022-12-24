library(shiny)
library(leaflet)
library(shinyjs)

ui <- fluidPage(
  useShinyFeedback(),
  useShinyjs(),
  
  titlePanel("TU Hydro Portal"),
  
  tabsetPanel(
    id = "mainTabs",

    tabPanel(
      title = "Select site",
      value = "selectSiteTab",
      
      tabsetPanel(
        id = "outerTabs",
        type = "hidden",
        
        tabPanelBody(
          value = "sitePicker",
          sitePickerInput("sitePicker")
          ),
        
        tabPanelBody(
          value = "siteDataView", 
          tabsetPanel(
            id = "innerTabs",
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
            tabPanel("Delete Data",
                     deleteDataUI("deleteData")
            ),
            tabPanel("Edit Site"),
            tabPanel("Select a new site", value = "selectSite")
          ),
        ),
      ),
    ),
    tabPanel(
      "Log in",
      userLoginUI("login")
    )
  )
)
