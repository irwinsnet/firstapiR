context("FIRST_R HTTP and Helper Functions")


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
                                                       "Eventcount"))
})


test_that(".GetHTTP() returns a non-empty local data frame", {
  dst <- firstapiR:::.GetHTTP(sess_local, "districts")
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
})

test_that(".GetHTTP() throws errors for incorrect input", {
  if(!sess_http_valid) skip("No username or authorization key")

  sn_badkey <- GetSession("user", "401BadAuthKeyErrorCheck", staging = T)
  expect_error(firstapiR:::.GetHTTP(sn_badkey, "districts"),
               "401: Unable To Determine Authorization Token")

  expect_error(firstapiR:::.GetHTTP(sess_http, "501BadApiPatternCheck"),
               "501: Request Did Not Match Any Current API Pattern")
  expect_error(firstapiR:::.GetHTTP(
    sess_http, "events?eventCode=404badEventCodeCheck"),
               paste("404: No event was found using the Season 2016 ",
                     "and Event Code 404badEventCodeCheck", sep = ""))

  sn_badseason <- GetSession(sess_http$username, sess_http$key, staging = T)
  sn_badseason$season <- 2014
  expect_error(firstapiR:::.GetHTTP(sn_badseason, "districts"),
          "400: Season must be between 2015 and the currently active season")
})

test_that(".GetHttp() returns valid JSON and XML", {
  if(!sess_http_valid) skip("No username or authorization key")

  sn_json <- GetSession(sess_http$username, sess_http$key, format = "json",
                        staging = T)
  expect_true(jsonlite::validate(firstapiR:::.GetHTTP(sn_json, "districts")))

  sn_xml <- GetSession(sess_http$username, sess_http$key, format = "XML",
                       staging = T)
  expect_equal(class(firstapiR:::.GetHTTP(sn_xml, "districts"))[1], "XMLNode")
})
