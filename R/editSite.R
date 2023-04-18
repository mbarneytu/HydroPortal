library(shinyFeedback)

# US map bounds
USSouth <- 25
USNorth <- 49
USWest <- -125
USEast <- -67

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
        checkboxInput(NS(id, "private_flag"), "Private site (hidden from non-logged-in users)")
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 4,
            numericInput(ns("latEntered"), "*Lat:", value = ""),
          ),
          column(
            width = 4,
            numericInput(ns("longEntered"), "*Long:", value = "")
          ),
        )
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

isLatValid <- function(lat) {
  return(
    !is.na(lat) && dplyr::between(lat, USSouth, USNorth) 
  )
}

isLongValid <- function(long) {
  return(
    !is.na(long) && dplyr::between(long, USWest, USEast)
  )
}

validateSite <- function(input){
  feedbackWarning("user_site_id", input$user_site_id == "", "Value is required")
  feedbackWarning("site_name", input$site_name == "", "Value is required")
  feedbackWarning("install_date", toString(input$install_date) == "", "Value is required")
  feedbackWarning("contact_name", input$contact_name == "",
                  "Value is required")
  feedbackWarning("contact_email", input$contact_email == "",
                  "Value is required")
  feedbackWarning("latEntered", !isLatValid(input$latEntered), 
                  paste0("Lat must be between ", USSouth, " and ", USNorth))
  feedbackWarning("longEntered", !isLongValid(input$longEntered), 
                  paste0("Long must be between ", USWest, " and ", USEast))
  req(
    input$user_site_id,
    input$site_name,
    input$install_date,
    input$contact_name,
    input$contact_email,
    input$latEntered,
    input$longEntered
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
  updateNumericInput(inputId = "latEntered", value = site$lat)
  updateNumericInput(inputId = "longEntered", value = site$long)
  updateCheckboxInput(inputId = "private_flag", value = site$private_flag)
}

resetEditUI <- function() {
  updateTextInput(inputId = "user_site_id", value = "")
  updateTextInput(inputId = "site_name", value = "")
  updateTextInput(inputId = "contact_name", value = "")
  updateTextInput(inputId = "contact_email", value = "")
  updateDateInput(inputId = "install_date", value = "")
  updateTextAreaInput(inputId = "equipment", value = "")
  updateTextInput(inputId = "landowner", value = "")
  updateTextAreaInput(inputId = "notes", value = "")
  updateNumericInput(inputId = "latEntered", value = "")
  updateNumericInput(inputId = "longEntered", value = "")
  updateCheckboxInput(inputId = "private_flag", value = FALSE)
}

editSiteServer <- function(id, gageSites, selectedSite, user_auth) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(gageSites))
    stopifnot(is.reactive(selectedSite))
    
    observeEvent(selectedSite(), {
      populateFields(selectedSite())
    })
    
    observeEvent(input$btnSave, {
      validateSite(input)
      tryCatch({
        updateSite(selectedSite()$site_id, input)
        # gageSites(loadSites(user_auth)) #<<< THIS LINE causes: Warning: Error in gageSites:
                                          # unused argument (loadSites(user_auth))
                                          # when outside tryCatch
        # AND the next time you come into edit the site, it's stale data. 

        gageSites <- reactive(loadSites(user_auth))
        resetEditUI()
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