library(leaflet)
library(shinyFeedback)
library(DBI)

server <- function(input, output, session) {
  
  # Load all sites from the database
  gageSites <- loadSites()
  
  createSiteServer("createSite")

  selectedSite <- sitePickerServer("sitePicker", gageSites)
  
  output$site <- renderText(
    as.character(gageSites %>% 
      filter(site_id == selectedSite()) %>% 
      select(site_name)
    ))

}