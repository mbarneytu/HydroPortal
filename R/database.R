library(pool)
library(shiny)

pool <- dbPool(
  RMariaDB::MariaDB(), 
  dbname = Sys.getenv("HYDROP_DBNAME"),
  host = Sys.getenv("HYDROP_HOST"),
  user = Sys.getenv("HYDROP_USER"),
  password = Sys.getenv("HYDROP_PWD")
)

onStop(function() {
  poolClose(pool)
})
