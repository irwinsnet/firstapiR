# test_40_schedule_data.R

context("FIRST_R Schedule Functions")


sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
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


test_that("GetSchedule() expand_cols arg returns expanded local data frame.", {
  sched <- GetSchedule(sess_local, event = "ORPHI", expand_cols = TRUE)
  expect_is(sched, "Schedule")
  expect_equal(nrow(sched), 60)
  expect_equal(length(sched), 17)
})


test_that("GetSchedule() start, end, and level arguments via HTTP.", {
  if(!sess_http_valid) skip("No username or authorization key")

  sched <- GetSchedule(sess_http, event = "WAAMV", start = 10, end = 30,
                       expand_cols = TRUE)
  expect_equal(nrow(sched), 21)

  sched <- GetSchedule(sess_http, event = "WAAMV", level = "playoff",
                       expand_cols = TRUE)
  expect_equal(nrow(sched), 16)
  expect_equal(levels(sched$tournamentLevel[1]), "Playoff")
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  sched <- GetSchedule(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "mod_since"), mod_date)

  sched <- GetSchedule(sess_http, event = "ORPHI", expand_cols = TRUE,
                       only_mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "only_mod_since"), mod_date)
})


# GetHybridSchedule() ==========================================================
test_that("GetHybridSchedule() returns a local data frame.", {
  hyb <- GetHybridSchedule(sess_local, event = "WAELL", expand_cols = TRUE)

  expect_is(hyb, "HybridSchedule")
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


test_that("GetHybridSchedule() expand_cols returns additional rows via HTTP.", {
  if(!sess_http_valid) skip("No username or authorization key")

  hyb <- GetHybridSchedule(sess_http, event = "ORPHI")

  expect_is(hyb, "HybridSchedule")
  expect_equal(nrow(hyb), 360)
  expect_equal(length(hyb), 12)

  df_names <- c("description", "tournamentLevel", "matchNumber", "startTime",
               "actualStartTime", "teamNumber", "station", "surrogate",
               "disqualified", "scoreFinal", "scoreFoul", "scoreAuto"  )
  expect_equal(names(hyb), df_names)
})

test_that("only_mod_since and mod_since return NA when set to current time", {
  if(!sess_http_valid) skip("No username or authorization key")

  mod_date <- httr::http_date(Sys.time())

  sched <- GetHybridSchedule(sess_http, event = "ORPHI", mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "mod_since"), mod_date)

  sched <- GetHybridSchedule(sess_http, event = "ORPHI", expand_cols = TRUE,
                       only_mod_since = mod_date)
  expect_true(is.na(sched))
  expect_equal(attr(sched, "only_mod_since"), mod_date)
})
