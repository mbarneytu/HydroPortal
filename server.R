library(leaflet)
library(shinyFeedback)
library(DBI)

server <- function(input, output, session) {
  
  createSiteServer("createSite")

  mapSitesServer("sitesMap")
  
}