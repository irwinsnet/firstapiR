# test_20_internal_functions.R
# Version 2.0.1
#


context("firstapiR HTTP and Helper Functions")

sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}

test_that(".AddHTTPArgs constructs valid URL strings.", {
  expect_equal(firstapiR:::.AddHTTPArgs("events", list(teamNumber = 1318)),
               "events?teamNumber=1318")
  expect_equal(firstapiR:::.AddHTTPArgs("events", list(teamNumber = 1318,
                                           eventCode = NULL,
                                           districtCode = "PNW",
                                           excludeDistrict = NULL)),
               "events?teamNumber=1318&districtCode=PNW")
  expect_equal(firstapiR:::.AddHTTPArgs("events", list(teamNumber = 360,
                                            excludeDistrict = TRUE)),
                "events?teamNumber=360&excludeDistrict=true")
})

test_that(".TrimColNames removes column prefixes", {
  col_names = list("Events.date", "Events.code", "Eventcount")
  expect_equal(firstapiR:::.TrimColNames(col_names), c("date", "code",
                                                       "eventcount"))
})


test_that(".GetHTTP() returns a non-empty local data frame", {
  dst <- firstapiR:::.GetHTTP(sess_local, "districts")
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 10)
  expect_equal(length(dst), 3)
})

test_that(".GetHTTP() throws errors for incorrect input", {
  if(!sess_http_valid) skip("No username or authorization key")

  sn_badkey <- GetSession("user", "401BadAuthKeyErrorCheck", staging = T)
  expect_error(firstapiR:::.GetHTTP(sn_badkey, "districts"), NULL)

  # expect_error(firstapiR:::.GetHTTP(sess_http, "501BadApiPatternCheck"), NULL)
  expect_error(firstapiR:::.GetHTTP(sess_http, "events?eventCode=404badEventCodeCheck"), NULL)

  sn_badseason <- GetSession(sess_http$username, sess_http$key, staging = T)
  sn_badseason$season <- 2005
  expect_error(firstapiR:::.GetHTTP(sn_badseason, "districts"), NULL)
})

test_that(".GetHttp() returns valid JSON and XML", {
  if(!sess_http_valid) skip("No username or authorization key")

  sn_json <- GetSession(sess_http$username, sess_http$key, format = "json",
                        staging = FALSE)
  expect_true(jsonlite::validate(firstapiR:::.GetHTTP(sn_json, "districts")))

  sn_xml <- GetSession(sess_http$username, sess_http$key, format = "XML",
                       staging = T)
  expect_equal(class(firstapiR:::.GetHTTP(sn_xml, "districts"))[1], "XMLNode")
})
