library("testthat")

context("FIRST_API R Functions")

# FIRST API tests require that username and key be defined and contain
#   FIRST API account authorization data.
test_that("username and key defined", {
  expect_type(username, "character")
  expect_length(username, 1)
  expect_type(key, "character")
  expect_length(key, 1)
})

test_that("GetSession returns correct list", {
  sn <- GetSession(username, key)
  
  expect_type(sn, "list")
  expect_equal(sn$username, username)
  expect_equal(sn$key, key)
  expect_equal(sn$season, 2016)
  expect_false(sn$staging)
  expect_equal(sn$format, "data.frame")
  
  sn$staging <- T
})

test_that("Version and URL variables are correct", {
  expect_equal(.staging_url, "https://frc-staging-api.firstinspires.org")
  expect_equal(.production_url, "https://frc-api.firstinspires.org")
  expect_equal(.version, "v2.0")
})

test_that(".AddHTTPArgs constructs valid URL strings.", {
  expect_equal(.AddHTTPArgs("events", list(teamNumber = 1318)),
               "events?teamNumber=1318")
  expect_equal(.AddHTTPArgs("events", list(teamNumber = 1318,
                                           eventCode = NULL,
                                           districtCode = "PNW",
                                           excludeDistrict = NULL)),
               "events?teamNumber=1318&districtCode=PNW")
})

test_that(".GetHTTP() returns a non-empty data frame", {
  dst <- .GetHTTP(sn, 'districts')
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
})

test_that(".GetHTTP() throws errors for incorrect input", {
  
  sn_badkey <- GetSession(username, "401BadAuthKeyErrorCheck", staging = T)
  expect_error(.GetHTTP(sn_badkey, "districts"),
               "401: Unable To Determine Authorization Token")
  
  expect_error(.GetHTTP(sn, "501BadApiPatternCheck"),
               "501: Request Did Not Match Any Current API Pattern")
  
  expect_error(.GetHTTP(
    sn, "events?eventCode=404badEventCodeCheck"),
               paste("404: No event was found using the Season 2016 ",
                     "and Event Code 404badEventCodeCheck", sep = ""))
  
  sn_badseason <- GetSession(username, key, season = 2014, staging = T)
  expect_error(.GetHTTP(sn_badseason, "districts"),
            "400: Season must be between 2015 and the currently active season")
})

test_that(".GetHttp() returns valid JSON and XML", {
  sn_json <- GetSession(username, key, format = "json", staging = T)
  expect_true(validate(.GetHTTP(sn_json, "districts")))
  
  sn_xml <- GetSession(username, key, format = "XML", staging = T)
  expect_equal(class(.GetHTTP(sn_xml, "districts"))[1], "XMLNode")
})

test_that("GetDistricts() Results", {
  dst <- GetDistricts(sn)
  
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
})

