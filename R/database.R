library(pool)
library(shiny)
library(config)

# get values from config.yml using the config package
config <- config::get("dataconnection")

pool <- dbPool(
  RMariaDB::MariaDB(),
  dbname = config$database,
  host = config$server,
  user = config$uid,
  password = config$pwd
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
