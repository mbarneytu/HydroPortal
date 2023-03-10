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
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  createSiteServer("createSite", gageSites)

  selectedSite <- reactiveVal()
  siteObservations <- reactiveVal()
  
  sitePickerServer("sitePicker", gageSites, selectedSite)

  output$siteID <- renderText(paste0(selectedSite()$user_site_id, " - ",
                                     selectedSite()$site_name))

  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "outerTabs", selected = "siteDataView")
    siteObservations(loadObservations(selectedSite()$site_id))
    dataViewerServer("dataViewer", selectedSite, siteObservations)
  })

  uploaderServer("uploader", selectedSite, siteObservations)
  
  observeEvent(siteObservations(), {
    dataViewerServer("dataViewer", selectedSite, siteObservations)
  })
  
  deleteDataServer("deleteData", selectedSite)

  observeEvent(input$innerTabs, {
    if (input$innerTabs == "selectSite") {
      updateTabsetPanel(inputId = "innerTabs", selected = "viewDataTab")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}