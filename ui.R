library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  titlePanel("Create a New Site"),
  h6("Required fields are marked with *"),
  fluidRow(
    column(width = 6,
           textInput("site_name*", "*Site Name", width = "90%"),
           textInput("contact_name", "*TU Staff Contact Name", width = "90%"),
           textInput("contact_email", "*TU Staff Contact Email", width = "90%"),
           dateInput("install_date", "*Date of installation", width = "90%"),
           textInput("user_site_id", "Site ID", width = "90%"), 
           textAreaInput("equipment", "Equipment", width = "90%"),
           textInput("landowner", "Landowner", width = "90%"),
           textAreaInput("notes", "Notes", width = "90%"),
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
  
  br(),
  
  fluidRow(
    column(width = 2, offset = 5,
           actionButton("btnSave", "Save Site", width = "100%", class = "btn-success")
    )
  )
)