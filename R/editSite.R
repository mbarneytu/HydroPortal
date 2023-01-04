editSiteUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 5, offset = 2,
             
        textInput(NS(id, "site_name"), "*Site Name"),
        textInput(NS(id, "contact_name"), "*TU Staff Contact Name"),
        textInput(NS(id, "contact_email"), "*TU Staff Contact Email"),
        dateInput(NS(id, "install_date"), "*Date of installation"),
        textInput(NS(id, "user_site_id"), "Site ID"), 
        textAreaInput(NS(id, "equipment"), "Equipment"),
        textInput(NS(id, "landowner"), "Landowner"),
        textAreaInput(NS(id, "notes"), "Notes"),
      )
    ),
  # site_id, site_name, user_site_id, active_datetime, lat, lon, 
  # contact_name, contact_email, landowner, equipment_desc, notes
    
    fluidRow(
      actionButton(NS(id, "btnSave"), "Save Changes", 
                   width = "100%", class = "btn-success")
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

  req(
    input$site_name,
    input$install_date,
    input$contact_name,
    input$contact_email
  )
}
editSiteServer <- function(id, selectedSite) {
  moduleServer(id, function(input, output, session) {
    observe(
      updateTextInput(inputId = "site_name", value = selectedSite()$site_name)
    )
  })
}