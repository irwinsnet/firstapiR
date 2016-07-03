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
  sess<- GetSession(username, key)
  
  expect_type(sess, "list")
  expect_equal(sess$username, username)
  expect_equal(sess$key, key)
  expect_equal(sess$season, 2016)
  expect_false(sess$staging)
  expect_equal(sess$format, "data.frame")
  
  rm(sess)
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
  
  sn_badseason <- GetSession(username, key, season = 2014, staging = T)
  expect_error(.GetHTTP(sn_badseason, "districts"),
            "400: Season must be between 2015 and the currently active season")
  rm(sn_badseason)
})

test_that(".GetHttp() returns valid JSON and XML", {
  sn_json <- GetSession(username, key, format = "json", staging = T)
  expect_true(validate(.GetHTTP(sn_json, "districts")))
  rm(sn_json)
  
  sn_xml <- GetSession(username, key, format = "XML", staging = T)
  expect_equal(class(.GetHTTP(sn_xml, "districts"))[1], "XMLNode")
  rm(sn_xml)
})

test_that("GetSeason() returns a data frame", {
  sess <- GetSession(username, key, staging = TRUE)
  season <- GetSeason(sess)
  
  expect_equal(class(season), "data.frame")
  expect_equal(nrow(season), 1)
  expect_equal(length(season), 8)
  expect_equal(names(season), c("eventCount", "gameName", "kickoff", "rookieStart",
                             "teamCount", "FRCChampionships.name",
                             "FRCChampionships.startDate",
                             "FRCChampionships.location"))
  rm(sess, season)
})

test_that("GetDistricts() returns a data frame", {
  sess <- GetSession(username, key, staging = TRUE)
  dst <- GetDistricts(sess)
  
  expect_equal(class(dst), "data.frame")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
  expect_equal(names(dst), c("code", "name", "districtCount"))
  rm(sess, dst)
})

test_that("GetEvents() returns data frame", {
  sess <- GetSession(username, key, staging = TRUE)
  evt <- GetEvents(sess, team = 1318)
  expect_equal(class(evt), "data.frame")
  expect_equal(length(evt), 13)
  expect_true(is.factor(evt$type))
  expect_true(is.factor(evt$districtCode))
  expect_true(is.factor(evt$stateprov))
  expect_true(is.factor(evt$country))
  expect_true(is.factor(evt$timezone))
  
  evt <- GetEvents(sess, event = "WAAMV")
  expect_equal(nrow(evt), 1)
  expect_equal(evt$code, "WAAMV")
  
  evt <- GetEvents(sess, excludeDistrict = TRUE)
  expect_equal(nrow(evt), 67)
  
  sess$staging <- FALSE
  evt <- GetEvents(sess, district = "PNW", team = "1318")
  expect_equal(nrow(evt), 4)

  rm(sess, evt)
  
})

test_that("GetTeams() returns a data frame", {
  sess <- GetSession(username, key, staging = TRUE)
  tms <- GetTeams(sess, team = 1318)
  expect_equal(class(tms), "data.frame")
  expect_equal(length(tms), 14)
  expect_equal(nrow(tms), 1)
  expect_true(is.factor(tms$stateProv))
  expect_true(is.factor(tms$country))
  expect_true(is.factor(tms$districtCode))
  
  tms <- GetTeams(sess, district = "PNW")
  expect_equal(class(tms), "data.frame")
  expect_equal(nrow(tms), 158)
  
  rm(tms, sess)
})

