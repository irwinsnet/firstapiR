library(testthat)
library(firstapiR)

# Define username and key variables
source("user.R")

context("FIRST_R Rankings and Alliances")

sess <- GetSession(username, key)

test_that("GetAlliances() returns a data frame", {
  al <- GetAlliances(sess, event = "WAELL")

  expect_equal(class(al), "data.frame")
  expect_equal(attr(al, "FIRST_type"), "Alliances")
  expect_equal(attr(al, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/alliances/WAELL")
  expect_equal(nrow(al), 8)
  expect_equal(length(al), 9)
})

test_that("GetRankings() returns a data frame", {
  rks <- GetRankings(sess, "PNCMP")

  expect_equal(class(rks), "data.frame")
  expect_equal(attr(rks, "FIRST_type"), "Rankings")
  expect_equal(attr(rks, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/rankings/PNCMP")
  expect_equal(nrow(rks), 64)
  expect_equal(length(rks), 14)
  expect_equal(names(rks), c("rank", "teamNumber", "sortOrder1", "sortOrder2",
                            "sortOrder3", "sortOrder4", "sortOrder5",
                            "sortOrder6", "wins", "losses", "ties",
                            "qualAverage", "dq", "matchesPlayed"))
})

test_that("GetRankings() team and top arguments work", {
  rks <- GetRankings(sess, "PNCMP", team = 5803)
  expect_equal(nrow(rks), 1)
  expect_equal(rks$teamNumber[[1]], 5803)

  rks <- GetRankings(sess, "PNCMP", top = 5)
  expect_equal(nrow(rks), 5)
  expect_equal(rks$rank, 1:5)
})

test_that("GetRankings() throws errors for incorrect arguments", {
  expect_error(GetRankings(sess, event = "WAELL", team = 4911, top = 5),
               "You cannot specify both the team and top argument")
})
rm(sess)
