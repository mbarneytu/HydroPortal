library(pool)
library(shiny)
library(config)
library(dplyr)
library(stringr)

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

saveObservations <- function(tibl, siteId, fileName, filePath) {
  # Make the observations' columns match database table structure
  quotedTibl <- tibl |> 
    transmute(
      site_id = siteId,
      meas_datetime = paste0("'", date, " ", time, "'"), 
      stage_ft = stage, 
      temperature_C = temperature, 
      discharge_cfs = discharge
    )

  # First, save a record of this event to the file_upload table so that it
  # the set of uploaded observations can be manipulated as a unit, e.g,
  # if the user decides to delete the upload.
  metadataCols <- quotedTibl |> 
    summarize(minDT = min(meas_datetime),
              maxDT = max(meas_datetime),
              count = n())
  ins_file_qry <- paste0("INSERT INTO file_upload(site_id, csv_filename, 
                         csv_filepath, obs_min_datetime, obs_max_datetime, 
                         row_count) VALUES (?,?,?,?,?,?)")
  ins_file_params <- list(
    siteId, fileName, filePath, 
    # This style SQL query needs different quotation of datetimes 
    # than sqlAppendTable, which we use later
    str_remove_all(metadataCols$minDT,"'"), 
    str_remove_all(metadataCols$maxDT,"'"), 
    metadataCols$count)
  
  tryCatch(
    poolWithTransaction(pool, function(conn) {
    
      # insert the file_upload and retrieve its id, which was 
      # assigned by the database
      dbExecute(pool, ins_file_qry, ins_file_params)
      fileUploadId <- dbGetQuery(pool, "SELECT LAST_INSERT_ID()")
      
      # Add the id of the file upload to all observations
      quotedTibl <- quotedTibl |> mutate(file_upload_id = fileUploadId[1,1])
      
      # Save the observations to the db, using sqlAppendTable for performance.
      query <- sqlAppendTable(pool, "observation", quotedTibl)
      dbExecute(pool, query)
      
      showNotification("Data uploaded successfully.", type = "message")
    }),
    error = function(cnd) {
      showNotification(paste0("Error saving to database: ", 
                              substr(cnd$message, 1, 200)), 
                       type = "error")
    })
}

getSiteDateRange <- function(siteId) {
  boundsQuery <- paste0("SELECT MIN(meas_datetime) AS min_dt, 
    MAX(meas_datetime) AS max_dt FROM `observation` WHERE site_id = ", siteId)
  
  bounds <- dbGetQuery(pool, boundsQuery)
}

loadObservations <- function(siteId, start, end) {
  
  # Set hours and minutes so that start and end include full days' data
  hour(start) <- 0 
  minute(start) <- 0
  hour(end) <- 23
  minute(end) <- 59

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