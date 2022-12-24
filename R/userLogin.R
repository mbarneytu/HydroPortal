library(dplyr)
library(lubridate)
library(sodium)

# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 7 # Days until session expires

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(user, sessionid, conn = pool) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = pool, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

userTbl <- dbReadTable(pool, "security") |> as_tibble()

userLoginUI <- function(id) {
  ns <- NS(id)
  tagList(
    # add logout button UI
    div(class = "pull-right", shinyauthr::logoutUI(id = ns("logout"))),
    # add login panel UI function
    shinyauthr::loginUI(id = ns("login"), cookie_expiry = cookie_expiry),
  )
}

userLoginServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    # call login module supplying data frame, user and password cols
    # and reactive trigger
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = userTbl,
      user_col = user,
      pwd_col = password,
      sodium_hashed = TRUE,
      cookie_logins = TRUE,
      sessionid_col = sessionid,
      cookie_getter = get_sessionids_from_db,
      cookie_setter = add_sessionid_to_db,
      log_out = reactive(logout_init())
    )
  })
}