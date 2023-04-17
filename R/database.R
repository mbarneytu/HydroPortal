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

loadUsers <- function() {
  dbReadTable(pool, "security") |> as_tibble()
}
saveSessionId <- function(user, sessionid) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
  dbWriteTable(pool, "sessionids", ., append = TRUE)
}

loadSessionIds <- function(expiry = 7) {
  dbReadTable(pool, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

loadSites <- function(authorized) {
  query <- "SELECT site_id, site_name, user_site_id, active_datetime, lat, lon as 'long', 
  contact_name, contact_email, landowner, equipment_desc, notes, private_flag FROM site"
  
  # Hide private sites from unauthorized users
  if (!authorized) {
    query <- paste0(query, " WHERE private_flag = 0")
  }
  
  res <- as_tibble(dbGetQuery(pool, query))
}

saveSite <- function(input, coords) {
  query <- paste0("CALL ins_site(?,?,?,?,?,?,?,?,?,?,?)")
  params <- list(input$site_name,
                 input$user_site_id,
                 input$install_date,
                 coords$lat(),
                 coords$long(),
                 input$contact_name,
                 input$contact_email,
                 input$landowner,
                 input$equipment,
                 input$notes,
                 input$private_flag
  )
  
  dbExecute(pool, query, params)
}

updateSite <- function(siteId, input) {
  query <- paste0("UPDATE site SET
    site_name = ?,
    user_site_id = ?,
    active_datetime = ?,
    lat = ?,
    lon = ?,
    contact_name = ?,
    contact_email = ?,
    landowner = ?,
    equipment_desc = ?,
    notes = ?,
    private_flag = ?
    WHERE site_id = ?")
  params <- list(input$site_name,
                 input$user_site_id,
                 input$install_date,
                 input$latEntered,
                 input$longEntered,
                 input$contact_name,
                 input$contact_email,
                 input$landowner,
                 input$equipment,
                 input$notes,
                 input$private_flag,
                 siteId
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
      dbExecute(conn, ins_file_qry, ins_file_params)
      fileUploadId <- dbGetQuery(conn, "SELECT LAST_INSERT_ID()")
      
      # Add the id of the file upload to all observations
      quotedTibl <- quotedTibl |> mutate(file_upload_id = fileUploadId[1,1])
      
      # Save the observations to the db, using sqlAppendTable for performance.
      query <- sqlAppendTable(conn, "observation", quotedTibl, row.names = NA)
      dbExecute(conn, query)
      
      showNotification("Data uploaded successfully.", type = "message")
      return(list(success = TRUE, message = ""))
    }),
    error = function(cnd) {
      errorMsg <- substr(cnd$message, 1, 450)
      return(list(success = FALSE, message = errorMsg))
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

  obsQuery <- paste0("SELECT meas_datetime as datetime, 
    discharge_cfs as cfs, stage_ft, temperature_C 
    FROM observation WHERE site_id = ", siteId)
  observations <- as_tibble(dbGetQuery(pool, obsQuery))
  
  obs <- as_tibble(
    observations |> 
      filter(datetime >= start & datetime <= end)
  )
}

loadUploads <- function(siteId) {
  query <- paste0("SELECT file_upload_id, upload_datetime, row_count, 
                  obs_min_datetime, obs_max_datetime, csv_filename, csv_filepath 
                  FROM file_upload
                  WHERE deleted_datetime IS NULL AND site_id = ", 
                  siteId, " ORDER BY upload_datetime DESC")
  upload <- as_tibble(dbGetQuery(pool, query)) |> 
    mutate(
      upload_datetime = as.character.POSIXt(upload_datetime, format = "%m/%d/%Y %H:%M:%S"),
      obs_min_datetime = as.character.POSIXt(obs_min_datetime, format = "%m/%d/%Y %H:%M:%S"),
      obs_max_datetime = as.character.POSIXt(obs_max_datetime, format = "%m/%d/%Y %H:%M:%S")
    )
}

deleteUpload <- function(file_upload_id) {
  query <- paste0("CALL del_observations(?)")
  params <- list(file_upload_id)
  
  dbExecute(pool, query, params)
}