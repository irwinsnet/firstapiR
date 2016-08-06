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

rm(sess)
