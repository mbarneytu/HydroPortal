# This script is used to run the application defined in app.R (or in two files: 
# server.R and ui.R) in the background
options(shiny.autoreload = TRUE)
shiny::runApp()