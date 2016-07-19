library("testthat")

context("FIRST R Sessions and Variables.")

test_username <- "username"
test_key <- "key"

test_that("username and key defined", {
  expect_equal(test_username, "username")
  expect_equal(test_key, "key")
})

test_that("GetSession returns correct list", {
  sess<- GetSession(username, key)
  
  expect_equal(attr(sess, "FIRST_type"), "Session")
  expect_equal(sess$username, username)
  expect_equal(sess$key, key)
  expect_equal(sess$season, 2016)
  expect_false(sess$staging)
  expect_equal(sess$format, "data.frame")
})

test_that("GetSession throws errors for incorrect arguments", {
  expect_error(GetSession(test_username, test_key, season = 2014),
               "season must be an integer between 2015 and next year")
  expect_error(GetSession(test_username, test_key, format = "csv"),
               "format must be 'data.frame', 'xml', or 'json'")
  expect_error(GetSession(test_username, test_key, staging = 2016),
               "staging must be either TRUE or FALSE")
})

test_that("Version and URL variables are correct", {
  expect_equal(.staging_url, "https://frc-staging-api.firstinspires.org")
  expect_equal(.production_url, "https://frc-api.firstinspires.org")
  expect_equal(.version, "v2.0")
})

rm(test_username, test_key)
