library(shiny)
library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  observeEvent(gageSites, {
    selectedSite <- sitePickerServer("sitePicker", gageSites())
  })
  
  gageSites(createSiteServer("createSite"))

  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "tabPickOrView", selected = "siteDataView")
    output$siteName <- renderText(
      as.character(gageSites |>
                     filter(site_id == selectedSite()) |>
                     select(site_name)
      ))
    dataViewerServer("dataViewer", selectedSite())

    uploaderServer("uploader", selectedSite())
  })

  observeEvent(selectedSite(), {
    uploaderServer("uploader", selectedSite())
  })

  observeEvent(input$tabSiteFunction, {
    if (input$tabSiteFunction == "selectSite") {
      resetUploaderUI(output)
      updateTabsetPanel(inputId = "tabSiteFunction", selected = "View")
      updateTabsetPanel(inputId = "tabPickOrView", selected = "sitePicker")
    }
  })
}