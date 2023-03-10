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

loadSites <- function() {
  query <- "SELECT site_id, site_name, user_site_id, active_datetime, lat, lon, 
  contact_name, contact_email, landowner, equipment_desc, notes FROM site"
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
      dbExecute(conn, ins_file_qry, ins_file_params)
      fileUploadId <- dbGetQuery(conn, "SELECT LAST_INSERT_ID()")
      
      # Add the id of the file upload to all observations
      quotedTibl <- quotedTibl |> mutate(file_upload_id = fileUploadId[1,1])
      
      # Save the observations to the db, using sqlAppendTable for performance.
      query <- sqlAppendTable(conn, "observation", quotedTibl, row.names = NA)
      dbExecute(conn, query)
      
      return(list(success = TRUE, message = ""))
    }),
    error = function(cnd) {
      errorMsg <- substr(cnd$message, 1, 450)
      return(list(success = FALSE, message = errorMsg))
    })
}

getSiteDateRange <- function(siteId) {
  
  # Get the date range of observations for the given siteId.
  obsDateRange <- dbGetQuery(pool, paste0(
    "SELECT MIN(meas_datetime) AS earliestDt, 
    MAX(meas_datetime) AS latestDt
    FROM `observation` 
    WHERE site_id = ", siteId))
  
  # If there are no observations, return NULL
  if (is.na(obsDateRange$earliestDt)) {
    return(NULL)
  }
  
  # Select a date limit on the number of observations to view, in days.
  dayLimit <- 90
  
  if (obsDateRange$latestDt - obsDateRange$earliestDt > dayLimit) {
    viewStartDt <- (obsDateRange$latestDt - lubridate::ddays(dayLimit))
  } else {
    viewStartDt <- obsDateRange$earliestDt
  }
  
  obsDateRange <- as_tibble(obsDateRange) |> 
    mutate(viewStartDt = viewStartDt) # add column for the computed start datetime
}

loadObservations <- function(siteId, start = NULL, end = NULL) {
  # if start and end are specified, add hh:mm and query with datetimes in the WHERE clause
  # if not specified, use getSiteDateRange for the WHERE clause
  # if no data, ... do we need to handle?
  
  # use the start and end dates, if supplied
  if (!(is.null(start) && is.null(end))) {
    # Set hours and minutes so that start and end include full days' data
    hour(start) <- 0 
    minute(start) <- 0
    hour(end) <- 23
    minute(end) <- 59
    
  } else { # start and end not specified; look up a reasonable date range
    siteDateRange <- getSiteDateRange(siteId)
    if (is.null(siteDateRange)) { # no observations in db
      return(NULL)
    }
    start <- siteDateRange$viewStartDt
    end <- siteDateRange$latestDt
  }

  obsQuery <- paste0("SELECT meas_datetime as datetime, 
    discharge_cfs as cfs, stage_ft, temperature_C 
    FROM observation WHERE site_id = ", siteId,
                     " AND meas_datetime >= '", start,
                     "' AND meas_datetime <= '", end, "'
                     ORDER BY meas_datetime ASC")
  
  observations <- as_tibble(dbGetQuery(pool, obsQuery))
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