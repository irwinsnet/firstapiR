library("testthat")

context("FIRST_R Match Results and Score Functions")

sess <- GetSession(username, key)

test_that("GetMatchResults() returns a data frame", {
  mr <- GetMatchResults(sess, event = "WAELL")
  
  expect_equal(class(mr), "data.frame")
  expect_equal(attr(mr, "FIRST_type"), "MatchResults")
  expect_equal(attr(mr, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/matches/WAELL")
  expect_equal(nrow(mr), 94)
  expect_equal(length(mr), 23)
})

test_that("For GetMatchResults, expand_rows returns an expanded data frame", {
  mr <- GetMatchResults(sess, event = "ORPHI", expand_rows = TRUE)
  
  expect_equal(nrow(mr), 462)
  expect_equal(names(mr), c("actualStartTime", "description", "tournamentLevel",
                            "matchNumber", "postResultTime", "teamNumber",
                            "station", "disqualified", "scoreFinal",
                            "scoreFoul", "scoreAuto"))
})

test_that("For GetMatchResults, start, end, and match args are correct", {
  mr <- GetMatchResults(sess, event = "PNCMP", level = "playoff", start = 2,
                        end = 5)
  expect_equal(nrow(mr), 4)
  
  mr <- GetMatchResults(sess, event = "WAAMV", level = "qual", match = 10)
  expect_equal(nrow(mr), 1)
})

test_that("GetScores() returns a data frame", {
  sc <- GetScores(sess, event = "WAAMV")
  
  expect_equal(class(sc), "data.frame")
  expect_equal(attr(sc, "FIRST_type"), "Scores")
  expect_equal(attr(sc, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/scores/WAAMV/qual")
  expect_equal(nrow(sc), 156)
  expect_equal(length(sc), 42)
  expect_equal(names(sc)[5:10], c("robot1Auto", "robot2Auto", "robot3Auto",
                                  "autoBouldersLow", "autoBouldersHigh", 
                                  "teleopBouldersLow"))
  expect_true(is.factor(sc$robot1Auto))
})


test_that("GetScores() throws errors for incorrect arguments", {
  expect_error(GetScores(sess, event = "WAELL", team = 4911, match = 1),
               "You cannot specify both a team and match number")
  expect_error(GetScores(sess, event = "WAELL", match = 2, start = 1),
               "You cannot specify start or end if you specify match")
  
})
rm(sess)
