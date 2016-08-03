library(testthat)
library(firstapiR)

# Define username and key variables
source("user.R")

context("FIRST_R Awards")

sess <- GetSession(username, key)

test_that("GetAwards() returns a data frame", {
  awards <- GetAwards(sess, event = "PNCMP")

  expect_equal(class(awards), "data.frame")
  expect_equal(attr(awards, "FIRST_type"), "Awards")
  expect_equal(attr(awards, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/awards/PNCMP")
  expect_equal(nrow(awards), 56)
  expect_equal(length(awards), 10)
  expect_equal(names(awards), c("awardId", "teamId", "eventId",
                                "eventDivisionId", "eventCode", "name",
                                "series", "teamNumber", "fullTeamName",
                                "person"))
})

test_that("GetAwards team argument filters results", {
  awards <- GetAwards(sess, event = "ARCHIMEDES", team = 180)
  expect_equal(nrow(awards), 1)
})

test_that("GetAwards throws errors for incorrect arguments", {
  expect_error(GetAwards(sess),
               "You must specify either a team number or event code")
})

test_that("GetAwardsList() returns a data frame", {
  alist <- GetAwardsList(sess)

  expect_equal(class(alist), "data.frame")
  expect_equal(attr(alist, "FIRST_type"), "AwardsList")
  expect_equal(attr(alist, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/awards/list")
  expect_equal(nrow(alist), 92)
  expect_equal(length(alist), 4)
  expect_equal(names(alist), c("awardId", "eventType", "description",
                             "forPerson"))
})
rm(sess)
