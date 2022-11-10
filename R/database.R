library(pool)
library(shiny)
library(config)

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