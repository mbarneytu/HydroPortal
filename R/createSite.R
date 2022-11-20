library(shiny)
library(shinyFeedback)
library(leaflet)

createSiteUI <- function(id) {
  tagList(
    h3("Create a New Site"),
    h6("Required fields are marked with *"),
    fluidRow(
      column(
        width = 6,
        textInput(NS(id, "site_name"), "*Site Name", width = "90%"),
        # selectInput(NS(id, "basin"), "*Basin", ),
        # selectInput(NS(id, "subbasin"), "*Subbasin", ),
        textInput(NS(id, "contact_name"), "*TU Staff Contact Name", width = "90%"),
        textInput(NS(id, "contact_email"), "*TU Staff Contact Email", width = "90%"),
        dateInput(NS(id, "install_date"), "*Date of installation", width = "90%"),
        textInput(NS(id, "user_site_id"), "Site ID", width = "90%"), 
        textAreaInput(NS(id, "equipment"), "Equipment", width = "90%"),
        textInput(NS(id, "landowner"), "Landowner", width = "90%"),
        textAreaInput(NS(id, "notes"), "Notes", width = "90%"),
      ),
      column(
        width = 6,
        siteCoordsUI(NS(id, "siteCoords"))
      )
    ),
    fluidRow(
      column(width = 6, offset = 3, 
             actionButton(NS(id, "btnSave"), "Save Site", 
                          width = "100%", class = "btn-success")
      )
    )
  )
}

validateSite <- function(input, lat, long){
  feedbackWarning("site_name", input$site_name == "", "Value is required")
  feedbackWarning("install_date", toString(input$install_date) == "", "Value is required")
  feedbackWarning("contact_name", input$contact_name == "",
                  "Value is required")
  feedbackWarning("contact_email", input$contact_email == "",
                  "Value is required")
  feedbackWarning("siteCoords", is.na(lat) || is.null(lat) || lat == "", 
                  "Click to set site coordinates")
  # message(glue::glue("lat:{lat}"))
  
  req(
    input$site_name,
    input$install_date,
    # (input$lat > 25 & input$lat < 50),
    # (input$long > -125.1 & input$long < -67.1),
    input$contact_name,
    input$contact_email
  )
}

saveSite <- function(input, coords) {
  query <- paste0("CALL ins_site(?,?,?,?,?,?,?,?,?,?)")
  params <- list(input$site_name,
                 input$user_site_id,
                 input$install_date,
                 coords$lat(),
                 coords$long(),
                 input$contact_name,
                 input$contact_email,
                 input$landowner,
                 input$equipment,
                 input$notes
  )
  
  dbExecute(pool, query, params)
}

resetCreateUI <- function() {
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
    
    coords <- siteCoordsServer("siteCoords")
    
    # str(coords$lat)
    # message(glue::glue("coords: {coords}"))
    
    observeEvent(input$btnSave, {
      validateSite(input, coords$lat(), coords$long())
      # message(glue::glue("lat:{coords$lat()}"))
      
      tryCatch({
        saveSite(input, coords)
        resetCreateUI()
        showNotification("Site saved successfully.", type = "message")
        },
        
        error = function(cnd) {
          showNotification(paste0("Error saving to database: ", cnd$message), 
                           type = "error")
        }
      )
    })
  })
}