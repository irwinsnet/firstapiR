library("testthat")
library("firstapiR")

context("Test local value functionality")

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

rm(sess)
