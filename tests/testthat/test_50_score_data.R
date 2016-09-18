# test_50_score_data.R

context("FIRST_R Match Results and Score Functions")


sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}

# GetMatchResults() ============================================================
test_that("GetMatchResults() returns a local data frame", {
  mr <- GetMatchResults(sess_local, event = "PNCMP")

  expect_is(mr, "MatchResults")
  expect_equal(attr(mr, "url"),
            paste("https://frc-api.firstinspires.org/v2.0/2016/matches/PNCMP",
                  "?tournamentLevel=qual", sep = ""))
  expect_equal(nrow(mr), 768)
  expect_equal(length(mr), 11)
  expect_equal(names(mr), c("actualStartTime", "description", "tournamentLevel",
                            "matchNumber", "postResultTime", "teamNumber",
                            "station", "disqualified", "scoreFinal",
                            "scoreFoul", "scoreAuto"))
})


test_that("For GetMatchResults returns an expanded local data frame", {
  mr <- GetMatchResults(sess_local, event = "PNCMP", expand_cols = TRUE)

  expect_equal(nrow(mr), 128)
  expect_equal(length(mr), 23)
})


test_that("For GetMatchResults start, end, and match args via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  mr <- GetMatchResults(sess_http, event = "PNCMP", level = "playoff",
                        start = 2, end = 5, expand_cols = TRUE)
  expect_equal(nrow(mr), 4)

  mr <- GetMatchResults(sess_http, event = "WAAMV", level = "qual", match = 10,
                        expand_cols = TRUE)
  expect_equal(nrow(mr), 1)
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  results <- GetMatchResults(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(results))
  expect_equal(attr(results, "mod_since"), mod_date)

  results <- GetMatchResults(sess_http, event = "ORPHI", expand_cols = TRUE,
                             only_mod_since = mod_date)
  expect_true(is.na(results))
  expect_equal(attr(results, "only_mod_since"), mod_date)
})


# GetScores() ==================================================================
test_that("GetScores() returns a data frame", {
  sc <- GetScores(sess_local, event = "ARCHIMEDES")

  expect_is(sc, "Scores")
  expect_equal(attr(sc, "url"),
          "https://frc-api.firstinspires.org/v2.0/2016/scores/ARCHIMEDES/qual")
  expect_equal(nrow(sc), 250)
  expect_equal(length(sc), 42)
  expect_equal(names(sc)[5:10], c("robot1Auto", "robot2Auto", "robot3Auto",
                                  "autoBouldersLow", "autoBouldersHigh",
                                  "teleopBouldersLow"))
  expect_true(is.factor(sc$robot1Auto))
})


test_that("GetScores() start and end args via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  sc <- GetScores(sess_http, event <- "PNCMP", start = 5, end = 10)

  expect_is(sc, "Scores")
  expect_equal(nrow(sc), 12)
  expect_equal(length(sc), 42)
})


test_that("GetScores() throws errors for incorrect arguments", {
  expect_error(GetScores(sess_local, event = "WAELL", team = 4911, match = 1),
               "You cannot specify both a team and match number")
  expect_error(GetScores(sess_local, event = "WAELL", match = 2, start = 1),
               "You cannot specify start or end if you specify match")
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  scores <- GetScores(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(scores))
  expect_equal(attr(scores, "mod_since"), mod_date)
})
