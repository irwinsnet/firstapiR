library(testthat)
library(firstapiR)

# Define username and key variables
source("user.R")

context("FIRST_R Season Data Functions")

sess <- GetSession(username, key, staging = TRUE)

test_that("GetSeason() returns a data frame", {
  season <- GetSeason(sess)

  expect_is(season, "Season")
  # expect_equal(attr(season, "url"),
  #              "https://frc-staging-api.firstinspires.org/v2.0/2016/")
  expect_equal(nrow(season), 1)
  expect_equal(length(season), 8)
  expect_equal(names(season), c("eventCount", "gameName", "kickoff", "rookieStart",
                             "teamCount", "FRCChampionships.name",
                             "FRCChampionships.startDate",
                             "FRCChampionships.location"))
})

test_that("GetDistricts() returns a data frame", {
  dst <- GetDistricts(sess)

  expect_is(dst, "Districts")
  # expect_equal(attr(dst, "url"),
  #              "https://frc-staging-api.firstinspires.org/v2.0/2016/districts")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
  expect_equal(names(dst), c("code", "name", "districtCount"))
})

test_that("GetEvents() returns data frame", {
  evt <- GetEvents(sess, team = 1318)
  expect_is(evt, "Events")
  # expect_equal(attr(evt, "url"),
  #     paste("https://frc-staging-api.firstinspires.org/v2.0/2016/",
  #           "events?teamNumber=1318", sep = ""))
  expect_equal(length(evt), 13)
  expect_true(is.factor(evt$type))
  expect_true(is.factor(evt$districtCode))
  expect_true(is.factor(evt$stateprov))
  expect_true(is.factor(evt$country))
  expect_true(is.factor(evt$timezone))

  evt <- GetEvents(sess, event = "WAAMV")
  expect_equal(nrow(evt), 1)
  expect_equal(evt$code, "WAAMV")

  evt <- GetEvents(sess, exclude_district = TRUE)
  expect_equal(nrow(evt), 67)

  sess$staging <- FALSE
  evt <- GetEvents(sess, district = "PNW", team = "1318")
  expect_equal(nrow(evt), 4)
})

test_that("GetTeams() returns a data frame", {
  tms <- GetTeams(sess, team = 1318)
  expect_is(tms, "Teams")
  expect_equal(length(tms), 14)
  expect_equal(nrow(tms), 1)
  expect_true(is.factor(tms$stateProv))
  expect_true(is.factor(tms$country))
  expect_true(is.factor(tms$districtCode))

  tms <- GetTeams(sess, district = "PNW")
  expect_equal(nrow(tms), 158)
})

test_that("GetTeams() throws errors or warnings for incorrect arguments", {
  expect_error(GetTeams(sess, team = 1318, event = "WAAMV"),
              "If you specify a team, you cannot specify any other arguments")
  expect_warning(GetTeams(sess, district = "PNW", page=2),
              "Do not specify GetTeams page argument for data frame format")
})

rm(sess)

