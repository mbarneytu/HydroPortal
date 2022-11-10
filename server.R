library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  # Load all sites from the database
  gageSites <- loadSites()
  
  createSiteServer("createSite")

  selectedSite <- sitePickerServer("sitePicker", gageSites)
  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "switcher", selected = "siteDataView")
    output$siteName <- renderText(
      as.character(gageSites |> 
                     filter(site_id == selectedSite()) |> 
                     select(site_name)
      ))
  })
  
  observeEvent(input$btnReturnMap, {
    updateTabsetPanel(inputId = "switcher", selected = "sitePicker")
  })
  
  uploaderServer("uploader", selectedSite())
}