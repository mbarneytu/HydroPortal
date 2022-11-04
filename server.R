library(leaflet)
library(shinyFeedback)
library(DBI)

server <- function(input, output, session) {
  
  # Load all sites into a tibble
  gageSites <- loadSites()
  
  createSiteServer("createSite")

  selectedSite <- mapSitesServer("sitesMap", gageSites)
  
  output$site <- renderText(
    as.character(gageSites %>% 
      filter(site_id == selectedSite()) %>% 
      select(site_name)
    ))

}