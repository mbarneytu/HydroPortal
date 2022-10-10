library(leaflet)
library(shinyFeedback)
library(DBI)

validateInputs <- function(input){
  feedbackWarning("site_name", input$site_name == "", "Value is required")
  # Ensure lat and long are in the lower 48 states
  feedbackWarning("lat", 
                  is.na(input$lat) || !(input$lat > 25 & input$lat < 50), 
                  "Enter a valid latitude")
  feedbackWarning("long", is.na(input$long) || 
                    !(input$long > -125.1 & input$long < -67.1), 
                  "Enter a valid longitude")
  feedbackWarning("contact_name", input$contact_name == "",
                  "Value is required")
  feedbackWarning("contact_email", input$contact_email == "",
                  "Value is required")
}

drawMap <- function(){
  renderLeaflet({
    leaflet() %>% 
      
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      addLayersControl(baseGroups = c("Topo", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      
      fitBounds(-125.1, 49, -67.1, 25.2) # zoom to Lower 48 states
  })
}

saveSite <- function(input) {
  query <- paste0("CALL ins_site(?,?,?,?,?,?,?,?,?,?)")
  params <- list(input$site_name, 
                 input$user_site_id, 
                 input$install_date, 
                 input$lat, 
                 input$long, 
                 input$contact_name, 
                 input$contact_email, 
                 ifelse(input$landowner != "", input$landowner, NA), 
                 ifelse(input$equipment != "", input$equipment, NA), 
                 ifelse(input$notes != "", input$notes, NA)
                 )
  dbExecute(pool, query, params)
}

server <- function(input, output, session) {
  output$map <- drawMap()
  observeEvent(input$btnSave, {
    validateInputs(input)
    saveSite(input)
  })
  
  mapSitesServer("map1")
}