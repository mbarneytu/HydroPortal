library(dplyr)
library(lubridate)
library(sodium)

userTbl <- loadUsers()

userLoginUI <- function(id) {
  ns <- NS(id)
  tagList(
    # add logout button UI
    div(class = "pull-right", shinyauthr::logoutUI(id = ns("logout"))),
    # add login panel UI function
    shinyauthr::loginUI(id = ns("login"), cookie_expiry = 7),
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
      cookie_getter = loadSessionIds,
      cookie_setter = saveSessionId,
      log_out = reactive(logout_init())
    )
  })
}