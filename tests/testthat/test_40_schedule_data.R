library("testthat")

# Define username and key variables
source("user.R")

context("FIRST_R Schedule Functions")

sess <- GetSession(username, key)

test_that("GetSchedule() returns a data frame", {
  sched <- GetSchedule(sess, event = "PNCMP")
  expect_equal(class(sched), "data.frame")
  expect_equal(attr(sched, "FIRST_type"), "Schedule")
  expect_equal(attr(sched, "url"), paste("https://frc-api.firstinspires.org",
      "/v2.0/2016/schedule/PNCMP?tournamentLevel=qual", sep = ""))
  expect_equal(nrow(sched), 768)
  expect_equal(length(sched), 9)
})

test_that("GetSchedule() expand_cols arg returns expanded data frame.", {
  sched <- GetSchedule(sess, event = "PNCMP", expand_cols = TRUE)
  expect_equal(class(sched), "data.frame")
  expect_equal(nrow(sched), 128)
  expect_equal(length(sched), 17)
})

test_that("GetSchedule() start, end, and level arguments work.", {
  sched <- GetSchedule(sess, event = "WAAMV", start = 10, end = 30,
                       expand_cols = TRUE)
  expect_equal(nrow(sched), 21)

  sched <- GetSchedule(sess, event = "WAAMV", level = "playoff",
                       expand_cols = TRUE)
  expect_equal(nrow(sched), 16)
  expect_equal(levels(sched$tournamentLevel[1]), "Playoff")
})

test_that("GetHybridSchedule() returns a data frame.", {
  hyb <- GetHybridSchedule(sess, event = "WAELL")

  expect_equal(class(hyb), "data.frame")
  expect_equal(attr(hyb, "FIRST_type"), "HybridSchedule")
  expect_equal(attr(hyb, "url"),
      "https://frc-api.firstinspires.org/v2.0/2016/schedule/WAELL/qual/hybrid")
  expect_equal(nrow(hyb), 78)
  expect_equal(length(hyb), 29)

  df_names <- c("description", "tournamentLevel", "matchNumber", "startTime",
                "actualStartTime", "scoreRedFinal", "scoreRedFoul",
                "scoreRedAuto", "scoreBlueFinal", "scoreBlueFoul",
                "scoreBlueAuto", "Red1.team", "Red1.surrogate", "Red1.dq",
                "Red2.team", "Red2.surrogate", "Red2.dq", "Red3.team",
                "Red3.surrogate", "Red3.dq", "Blue1.team", "Blue1.surrogate",
                "Blue1.dq", "Blue2.team", "Blue2.surrogate", "Blue2.dq",
                "Blue3.team", "Blue3.surrogate", "Blue3.dq")
  expect_equal(names(hyb), df_names)
})

test_that("GetHybridSchedule() expand_rows arg returns expanded data frame.", {
  hyb <- GetHybridSchedule(sess, event = "ORPHI", expand_rows = TRUE)

  expect_equal(class(hyb), "data.frame")
  expect_equal(nrow(hyb), 360)
  expect_equal(length(hyb), 12)

  df_names <- c("description", "tournamentLevel", "matchNumber", "startTime",
               "actualStartTime", "teamNumber", "station", "surrogate",
               "disqualified", "scoreFinal", "scoreFoul", "scoreAuto"  )
  expect_equal(names(hyb), df_names)
})

rm(sess)
