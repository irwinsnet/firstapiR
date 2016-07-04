library("testthat")

context("FIRST_R Sessions and Variables.")

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
