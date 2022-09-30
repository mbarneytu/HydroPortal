library(leaflet)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  titlePanel("Create a New Site"),
  fluidRow(
    column(width = 5,
           textInput("site_name", "Site Name"), 
           dateInput("install_date", "Date of installation"),
           textAreaInput("equipment", "Equipment"),
           textInput("contact_name", "TU Staff Contact Name"),
           textInput("contact_email", "TU Staff Contact Email"),
           textInput("landowner", "Landowner"),
           textAreaInput("notes", "Notes")
    ),
    
    column(width = 7,
           fluidRow(
             column(width = 12,
                    leafletOutput("map")
             )
           ),
           fluidRow(
             column(width = 6,
                    numericInput("lat", "Latitude", value = "")),
             column(width = 6,
                   numericInput("long", "Longitude", value = "")
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