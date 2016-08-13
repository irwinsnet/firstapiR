library(testthat)
library(firstapiR)

context("FIRST_R Local Data")

sess <- GetSession("username", "key")

test_that("GetSeason() returns a local data frame", {
  season <- GetSeason(sess)
  expect_is(season, "Season")
  expect_true(attr(season, "local_test_data"))
})

test_that("GetDistricts() returns a local data frame", {
  dst <- GetDistricts(sess)
  expect_is(dst, "Districts")
  expect_true(attr(dst, "local_test_data"))
})

test_that("GetEvents() returns a local data frame", {
  events <- GetEvents(sess, district = "PNW")
  expect_is(events, "Events")
  expect_true(attr(events, "local_test_data"))
})

test_that("GetTeams() returns a local data frame", {
  events <- GetEvents(sess, district = "PNW")
  expect_is(events, "Events")
  expect_true(attr(events, "local_test_data"))
})

test_that("GetSchedule() returns a local data frame", {
  sched <- GetSchedule(sess, event = "PNCMP", level = "playoff")
  expect_is(sched, "Schedule")
  expect_true(attr(sched, "local_test_data"))
})

test_that("GetHybridSchedule() returns a local data frame", {
  hybrid <- GetHybridSchedule(sess, event = "ORPHI")
  expect_is(hybrid, "HybridSchedule")
  expect_true(attr(hybrid, "local_test_data"))
})

test_that("GetMatchResults() returns a local data frame", {
  matches <- GetMatchResults(sess, event = "PNCMP")
  expect_is(matches, "MatchResults")
  expect_true(attr(matches, "local_test_data"))
})

test_that("GetScores() returns a local data frame", {
  scores <- GetScores(sess, event = "ARCHIMEDES")
  expect_is(scores, "Scores")
  expect_true(attr(scores, "local_test_data"))
})

test_that("GetAlliances() returns a local data frame", {
  alliances <- GetAlliances(sess, event = "WAAMV")
  expect_is(alliances, "Alliances")
  expect_true(attr(alliances, "local_test_data"))
})

rm(sess)
