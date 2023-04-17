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
  }, ignoreInit = TRUE)
  
  # Load sites from the database
  gageSites <- eventReactive(credentials(), {
    loadSites(credentials()$user_auth)
  }, ignoreInit = TRUE)
  
  createSiteServer("createSite", gageSites, credentials()$user_auth)

  selectedSite <- reactiveVal()
  
  sitePickerServer("sitePicker", gageSites, selectedSite)

  output$siteID <- renderText(paste0(selectedSite()$user_site_id, " - ",
                                     selectedSite()$site_name))

  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "outerTabs", selected = "siteDataView")
    dataViewerServer("dataViewer", selectedSite)
  })
  
  editSiteServer("editSite", gageSites, selectedSite, credentials()$user_auth)

  uploaderServer("uploader", selectedSite)
  
  deleteDataServer("deleteData", selectedSite)

  observeEvent(input$innerTabs, {
    if (input$innerTabs == "selectSite") {
      updateTabsetPanel(inputId = "innerTabs", selected = "viewDataTab")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}