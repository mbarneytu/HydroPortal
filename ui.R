library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  titlePanel("Create a New Site"),
  h6("Required fields are marked with *"),
  fluidRow(
    column(width = 6,
           textInput("site_name*", "*Site Name"), 
           textInput("contact_name", "*TU Staff Contact Name"),
           textInput("contact_email", "*TU Staff Contact Email"),
           dateInput("install_date", "*Date of installation"),
           textInput("user_site_id", "Site ID"), 
           textAreaInput("equipment", "Equipment"),
           textInput("landowner", "Landowner"),
           textAreaInput("notes", "Notes")
    ),
    
    column(width = 6,
           fluidRow(
             column(width = 12,
                    leafletOutput("map")
             )
           ),
           br(),
           fluidRow(
             column(width = 6,
                    numericInput("lat", "*Latitude", value = "")),
             column(width = 6,
                   numericInput("long", "*Longitude", value = "")
                   )
           )
    )
  ),
  fluidRow(
    column(width = 2, offset = 5,
           actionButton("btnSave", "Save Site", width = "100%", class = "btn-success")
    )
  )
)