library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  credentials <- userLoginServer("login")

  observeEvent(credentials(), {
    if (credentials()$user_auth) {
      enableEdits()
    }
    else {
      disableEdits()
    }
  })
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  createSiteServer("createSite", gageSites)

  selectedSite <- reactiveVal()
  
  observeEvent(gageSites, {
    sitePickerServer("sitePicker", gageSites(), selectedSite)
  })
  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "outerTabs", selected = "siteDataView")
    output$siteName <- renderText(selectedSite()$site_name)
    dataViewerServer("dataViewer", selectedSite)
  })

  uploaderServer("uploader", selectedSite)
  
  deleteDataServer("deleteData", selectedSite)

  observeEvent(input$innerTabs, {
    if (input$innerTabs == "selectSite") {
      updateTabsetPanel(inputId = "innerTabs", selected = "viewDataTab")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}