# test_10_session.R
# Version 2.0.1


context("firstapiR Sessions and Variables")

test_username <- "username"
test_key <- "key"

test_that("GetSession returns correct list", {
  sess<- GetSession(test_username, test_key)

  expect_is(sess, "Session")
  expect_equal(sess$username, test_username)
  expect_equal(sess$key, test_key)
  expect_equal(sess$season, as.integer(format(Sys.Date(), "%Y")))
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
  expect_equal(firstapiR:::.staging_url,
               "https://frc-staging-api.firstinspires.org")
  expect_equal(firstapiR:::.production_url, "https://frc-api.firstinspires.org")
  expect_equal(firstapiR:::.package_version, "2.0.1")
})
