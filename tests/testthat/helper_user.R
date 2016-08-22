# FIRST API login data
# This file is necessary for the automated tests to work. Set username and key
# to the username and authorization key provided by FIRST. Remember -- Don't
# post your authorization key in any publicly accessible place!

#username <- "username"
#key <- "key"

# By default, the tests do not send HTTP requests but instead pull local data
# from the R/sysdata.R file. To test actual HTTP requests to the FIRST API
# server, ensure your FIRST API username and authorization key in character
# vectors named "username" and "key" respectively. Then run the this test file
# with testthat::test_file(), or run the entire test suite with
# testthat::test_dir()

# Create Session objects and check for username, key, and internet connection
sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- tryCatch(
    {
      GetServerStatus(sess_http)
      TRUE
    },
    error = function(cond) {
      return(FALSE)
    },
    warning = function(cond) {
      return(FALSE)
    },
    finally = {}
  )
}
