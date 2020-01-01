# test_40_schedule_data.R
# Version 2.0.1
#
# By default, these tests run locally, without making HTTP connections to the
# FIRST API server. To run test that firstapiR connects to the FIRST API
# server, create character vectors named "username" and "key" in the console
# and set their values to the username and key assigned to you by FIRST. Then
# run the tests from the console using testthat::test_dir() or
# testthat::test_file().

context("firstapiR Schedule Functions")


sess_http_valid <- FALSE
sess_local <- GetSession("username", "key", season=2016)
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key, season=2016)
  sess_http_valid <- TRUE
}

# GetSchedule ==================================================================
test_that("GetSchedule() returns a local data frame", {
  sched <- GetSchedule(sess_local, event = "ORPHI")
  expect_is(sched, "Schedule")
  expect_equal(attr(sched, "url"), paste("https://frc-api.firstinspires.org",
      "/v2.0/2016/schedule/ORPHI?tournamentLevel=qual", sep = ""))
  expect_equal(nrow(sched), 360)
  expect_equal(length(sched), 9)
})


test_that("GetSchedule() start, end, and level arguments via HTTP.", {
  if(!sess_http_valid) skip("No username or authorization key")

  sched <- GetSchedule(sess_http, event = "WAAMV", start = 10, end = 30)
  expect_equal(nrow(sched), 126)

  sched <- GetSchedule(sess_http, event = "WAAMV", level = "playoff")
  expect_equal(nrow(sched), 96)
  expect_equal(levels(sched$level[1]), "Playoff")
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  sched <- GetSchedule(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "mod_since"), mod_date)

  sched <- GetSchedule(sess_http, event = "ORPHI", only_mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "only_mod_since"), mod_date)
})


# GetHybridSchedule() ==========================================================
test_that("GetHybridSchedule() returns a local data frame.", {
  hyb <- GetHybridSchedule(sess_local, event = "WAELL")

  expect_is(hyb, "HybridSchedule")
  expect_equal(attr(hyb, "url"),
      "https://frc-api.firstinspires.org/v2.0/2016/schedule/WAELL/qual/hybrid")
  expect_equal(nrow(hyb), 468)
  expect_equal(length(hyb), 13)

  df_names <- c("match", "description", "level", "start", "actualStart",
                "team", "alliance", "station", "surrogate", "disqualified",
                "scoreFinal", "scoreAuto", "scoreFoul")
  expect_equal(names(hyb), df_names)
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  sched <- GetHybridSchedule(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "mod_since"), mod_date)

  sched <- GetHybridSchedule(sess_http, event = "ORPHI",
                             only_mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "only_mod_since"), mod_date)
})
