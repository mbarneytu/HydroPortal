editSiteUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Edit Site"),
    h6("Required fields are marked with *"),
    fluidRow(
      column(
        width = 5,
        textInput(NS(id, "user_site_id"), "*Site ID"), 
        shinyBS::bsTooltip(NS(id, "user_site_id"), 
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
  )}

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
  
  req(
    input$user_site_id,
    input$site_name,
    input$install_date,
    input$contact_name,
    input$contact_email
  )
}

populateFields <- function(site) {
  updateTextInput(inputId = "user_site_id", value = site$user_site_id)
  updateTextInput(inputId = "site_name", value = site$site_name)
  updateTextInput(inputId = "contact_name", value = site$contact_name)
  updateTextInput(inputId = "contact_email", value = site$contact_email)
  updateDateInput(inputId = "install_date", value = site$active_datetime)
  updateTextAreaInput(inputId = "equipment", value = site$equipment_desc)
  updateTextInput(inputId = "landowner", value = site$landowner)
  updateTextAreaInput(inputId = "notes", value = site$notes)
}

editSiteServer <- function(id, selectedSite) {
  moduleServer(id, function(input, output, session) {
    populateFields(selectedSite())
    myCoords <- siteCoordsServer("siteCoords", 
                                 selectedSite()$lat, selectedSite()$long)
    
    observeEvent(input$btnSave, {
      validateSite(input, myCoords$lat, myCoords$long)

      tryCatch({
        saveSite(input, myCoords$lat, myCoords$long)
        gageSites(loadSites())
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