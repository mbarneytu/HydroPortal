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
              h3(textOutput("siteID")), 
              br()
            ),
            tabPanel(title = "View",
                     value = "viewDataTab",
                     dataViewerUI("dataViewer")
                     ),
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
