library(pool)
library(shiny)
library(config)
library(dplyr)

# get values from config.yml using the config package
config <- config::get("dataconnection")

pool <- dbPool(
  odbc::odbc(), 
  Driver = config$driver,
  Server = config$server,
  Database = config$database,
  UID = config$uid,
  PWD = config$pwd, 
  timeout = 10
)

onStop(function() {
  poolClose(pool)
})

loadSites <- function() {
  # Using raw SQL to work around a weird SP error that happens only on shinyapps.io
  # query <- "CALL sel_site_locations" # returns lat, lon, site_name, site_id
  
  query <- "SELECT lat, lon, site_name, site_id FROM site"
  res <- as_tibble(dbGetQuery(pool, query))
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

saveObservations <- function(tibl, siteId) {
  quotedTibl <- tibl |> 
    transmute(
      site_id = siteId,
      meas_datetime = paste0("'", date, " ", time, "'"), 
      stage_ft = stage, 
      temperature_C = temperature, 
      discharge_cfs = discharge
    )

  tryCatch({
    query <- sqlAppendTable(pool, "observation", quotedTibl)
    dbExecute(pool, query)
    showNotification("Data uploaded successfully.", type = "message")
    },
    
    error = function(cnd) {
      showNotification(paste0("Error saving to database: ", 
                              substr(cnd$message, 1, 200)), 
                     type = "error")
      }
    )
}

getSiteDateRange <- function(siteId) {
  boundsQuery <- paste0("SELECT MIN(meas_datetime) AS min_dt, 
    MAX(meas_datetime) AS max_dt FROM `observation` WHERE site_id = ", siteId)
  
  bounds <- dbGetQuery(pool, boundsQuery)
}

loadObservations <- function(siteId, start, end) {
  obsQuery <- paste0("SELECT site_id, meas_datetime as datetime, 
    discharge_cfs as cfs, temperature_C 
    FROM observation WHERE site_id = ", siteId)
  observations <- as_tibble(dbGetQuery(pool, obsQuery))
  
  obs <- as_tibble(
    observations |> 
      filter(datetime >= start & datetime <= end) |>
      select(datetime, cfs, temperature_C)
  )
}