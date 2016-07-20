library("testthat")

# Define username and key variables
source("user.R")

context("FIRST_R HTTP and Helper Functions")

test_that(".AddHTTPArgs constructs valid URL strings.", {
  expect_equal(.AddHTTPArgs("events", list(teamNumber = 1318)),
               "events?teamNumber=1318")
  expect_equal(.AddHTTPArgs("events", list(teamNumber = 1318,
                                           eventCode = NULL,
                                           districtCode = "PNW",
                                           excludeDistrict = NULL)),
               "events?teamNumber=1318&districtCode=PNW")
  expect_equal(.AddHTTPArgs("events", list(teamNumber = 360,
                                            excludeDistrict = TRUE)),
                "events?teamNumber=360&excludeDistrict=true")
})

test_that(".TrimColNames removes column prefixes", {
  col_names = list("Events.date", "Events.code", "Eventcount")
  expect_equal(.TrimColNames(col_names), c("date", "code", "Eventcount"))
})

test_that(".GetHTTP() returns a non-empty data frame", {
  sess <- GetSession(username, key, staging = TRUE)
  dst <- .GetHTTP(sess, 'districts')
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)

  rm(sess)
})

test_that(".GetHTTP() throws errors for incorrect input", {

  sn_badkey <- GetSession(username, "401BadAuthKeyErrorCheck", staging = T)
  expect_error(.GetHTTP(sn_badkey, "districts"),
               "401: Unable To Determine Authorization Token")
  rm(sn_badkey)

  sess <- GetSession(username, key, staging = TRUE)
  expect_error(.GetHTTP(sess, "501BadApiPatternCheck"),
               "501: Request Did Not Match Any Current API Pattern")
  expect_error(.GetHTTP(
    sess, "events?eventCode=404badEventCodeCheck"),
               paste("404: No event was found using the Season 2016 ",
                     "and Event Code 404badEventCodeCheck", sep = ""))
  rm(sess)

  sn_badseason <- GetSession(username, key, staging = T)
  sn_badseason$season <- 2014
  expect_error(.GetHTTP(sn_badseason, "districts"),
            "400: Season must be between 2015 and the currently active season")
  rm(sn_badseason)
})

test_that(".GetHttp() returns valid JSON and XML", {
  sn_json <- GetSession(username, key, format = "json", staging = T)
  expect_true(jsonlite::validate(.GetHTTP(sn_json, "districts")))
  rm(sn_json)

  sn_xml <- GetSession(username, key, format = "XML", staging = T)
  expect_equal(class(.GetHTTP(sn_xml, "districts"))[1], "XMLNode")
  rm(sn_xml)
})

