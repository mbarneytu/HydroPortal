library(shiny)
library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  tabsetPanel(
    tabPanel(
      "Create Site",
      createSite()
    )
    # ,
    # tabPanel(
    #   "Map",
    #   mapSites()
    # ),
    # tabPanel(
    #   "View Site Data",
    #   viewSite()
    # )
  )
)
