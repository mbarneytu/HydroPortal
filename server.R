library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  createSiteServer("createSite", gageSites)

  # Store the user's currently-selected site in a reactive
  selectedSite <- sitePickerServer("sitePicker", gageSites)
  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "outerTabs", selected = "siteDataView")
  })

  output$siteName <- renderText({
    as.character(gageSites() |> 
                   filter(site_id == selectedSite()) |> 
                   select(site_name)
                 )
  })
  
  observeEvent(selectedSite(), {
    dataViewerServer("dataViewer", selectedSite)
  })
  
  uploaderServer("uploader", selectedSite)
  
  deleteDataServer("deleteData", selectedSite)

  observeEvent(input$innerTabs, {
    if (input$innerTabs == "selectSite") {
      updateTabsetPanel(inputId = "innerTabs", selected = "View")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}