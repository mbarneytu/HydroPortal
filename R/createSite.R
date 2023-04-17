library(shiny)
library(shinyFeedback)
library(leaflet)
library(shinyBS)

createSiteUI <- function(id) {
  tagList(
    h3("Create a New Site"),
    h6("Required fields are marked with *"),
    fluidRow(
      column(
        width = 5,
        textInput(NS(id, "user_site_id"), "*Site ID"), 
        bsTooltip(NS(id, "user_site_id"), 
                  paste0("Site ID must be unique. ", 
                                                 "For example: MI02")),
        textInput(NS(id, "site_name"), "*Site Name"),
        bsTooltip(NS(id, "site_name"), 
                  paste0("Site Name is intended to be more descriptive and need",
                         " not be unique. For example: Mill Creek at Falls")),
        textInput(NS(id, "contact_name"), "*TU Staff Contact Name"),
        textInput(NS(id, "contact_email"), "*TU Staff Contact Email"),
        dateInput(NS(id, "install_date"), "*Date of installation", format = "m/d/yyyy",),
        textAreaInput(NS(id, "equipment"), "Equipment"),
        textInput(NS(id, "landowner"), "Landowner"),
        textAreaInput(NS(id, "notes"), "Notes"),
        checkboxInput(NS(id, "private_flag"), "Private site (hidden from non-logged-in users)")
      ),
      column(
        width = 7,
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
  feedbackWarning("user_site_id", input$user_site_id == "", "Value is required")
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
      input$user_site_id,
      input$site_name,
      input$install_date,
    # (input$lat > 25 & input$lat < 50),
    # (input$long > -125.1 & input$long < -67.1),
    input$contact_name,
    input$contact_email
  )
}

resetCreateUI <- function() {
  updateTextInput(inputId = "user_site_id", value = "")
  updateTextInput(inputId = "site_name", value = "")
  updateTextInput(inputId = "contact_name", value = "")
  updateTextInput(inputId = "contact_email", value = "")
  updateDateInput(inputId = "install_date", value = NULL)
  updateTextInput(inputId = "user_site_id", value = "")
  updateTextAreaInput(inputId = "equipment", value = "")
  updateTextInput(inputId = "landowner", value = "")
  updateTextAreaInput(inputId = "notes", value = "")
  updateCheckboxInput(inputId = "private_flag", value = FALSE)
}

createSiteServer <- function(id, gageSites, user_auth) {
  moduleServer(id, function(input, output, session) {
    
    coords <- siteCoordsServer("siteCoords")
    
    # str(coords$lat)
    # message(glue::glue("coords: {coords}"))
    
    observeEvent(input$btnSave, {
      validateSite(input, coords$lat(), coords$long())
      # message(glue::glue("lat:{coords$lat()}"))
      
      tryCatch({
        saveSite(input, coords)
        gageSites(loadSites(user_auth))
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