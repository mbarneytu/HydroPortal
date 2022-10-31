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
