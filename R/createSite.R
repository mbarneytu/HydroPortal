library(shiny)
library(shinyFeedback)
library(leaflet)
library(DBI)

createSiteUI <- function(id) {
  tagList(
    h3("Create a New Site"),
    h6("Required fields are marked with *"),
    fluidRow(
      column(
        width = 6,
        textInput(NS(id,"site_name"), "*Site Name", width = "90%"),
        # selectInput(NS(id,"basin"), "*Basin", ),
        # selectInput(NS(id,"subbasin"), "*Subbasin", ),
        textInput(NS(id,"contact_name"), "*TU Staff Contact Name", width = "90%"),
        textInput(NS(id,"contact_email"), "*TU Staff Contact Email", width = "90%"),
        dateInput(NS(id,"install_date"), "*Date of installation", width = "90%"),
        textInput(NS(id,"user_site_id"), "Site ID", width = "90%"), 
        textAreaInput(NS(id,"equipment"), "Equipment", width = "90%"),
        textInput(NS(id,"landowner"), "Landowner", width = "90%"),
        textAreaInput(NS(id,"notes"), "Notes", width = "90%"),
      ),
      
      column(
        width = 6,
        fluidRow(
          column(width = 12,
                 leafletOutput(NS(id,"map"))
          )
        ),
        br(),
        fluidRow(
          column(width = 6,
                 numericInput(NS(id,"lat"), "*Latitude", value = "")),
          column(width = 6,
                 numericInput(NS(id,"long"), "*Longitude", value = "")
          )
        ),
        fluidRow(
          column(width = 6, offset = 3, 
                 actionButton(NS(id,"btnSave"), "Save Site", 
                              width = "100%", class = "btn-success")
          )
        )
      )
    )
  )
}

validateInputs <- function(input){
  feedbackWarning("site_name", input$site_name == "", "Value is required")
  feedbackWarning("install_date", toString(input$install_date) == "", "Value is required")
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
  req(
    input$site_name, 
    input$install_date,
    (input$lat > 25 & input$lat < 50),
    (input$long > -125.1 & input$long < -67.1),
    input$contact_name,
    input$contact_email
  )
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
                 input$landowner,
                 input$equipment,
                 input$notes
  )
  
  dbExecute(pool, query, params)
}

resetInputs <- function() {
  updateTextInput(inputId = "site_name", value = "")
  # updateSelectInput("basin")
  # updateSelectInput("subbasin")
  updateTextInput(inputId = "contact_name", value = "")
  updateTextInput(inputId = "contact_email", value = "")
  updateDateInput(inputId = "install_date", value = NULL)
  updateTextInput(inputId = "user_site_id", value = "")
  updateTextAreaInput(inputId = "equipment", value = "")
  updateTextInput(inputId = "landowner", value = "")
  updateTextAreaInput(inputId = "notes", value = "")
}

createSiteServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- drawMap()
    
    observeEvent(input$btnSave, {
      validateInputs(input)
      
      tryCatch({
        saveSite(input)
        showNotification("Site saved successfully.", type = "message")
        resetInputs()
        },
        
        error = function(cnd) {
          showNotification(paste0("Error saving to database: ", cnd$message), 
                           type = "error")
        }
      )

    })
  })
}