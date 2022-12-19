library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  createSiteServer("createSite", gageSites)

  # Store the user's currently-selected site in a reactive
  selectedSite <- sitePickerServer("sitePicker", gageSites)
  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "tabPickOrView", selected = "siteDataView")
  })

  output$siteName <- renderText({
    as.character(gageSites() |> 
                   filter(site_id == selectedSite()) |> 
                   select(site_name)
                 )
  })
  
  dataViewerServer("dataViewer", selectedSite)
  
  # uploaderServer("uploader", selectedSite)

  observeEvent(input$tabSiteFunction, {
    if (input$tabSiteFunction == "selectSite") {
      resetUploaderUI(output)
      updateTabsetPanel(inputId = "tabSiteFunction", selected = "View")
      updateTabsetPanel(inputId = "tabPickOrView", selected = "sitePicker")
    }
    # selectedSite = NULL
  })
}