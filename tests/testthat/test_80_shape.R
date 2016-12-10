# test_80_shape.R
# Version 2.0.0


context("firstapiR Functions for Reshaping Data")


# Define Helper Functions ======================================================
CheckAttValues <- function(df, cls) {
  expect_is(df, cls)
  expect_true(attr(df, "shape") %in% c("team", "alliance", "match"))
  expect_true(grepl("https://frc-api\\.firstinspires\\.org/v2\\.0/",
                    attr(df, "url")))
  expect_true(attr(df, "local_test_data"))
  expect_true(grepl("https://frc-api\\.firstinspires\\.org/v2\\.0/",
                    attr(df, "local_url")))
  expect_is(attr(df, "time_downloaded"), "POSIXct")
  expect_is(httr::parse_http_date(attr(df, "last_modified")), "POSIXct")
  if(attr(df, "shape") %in% c("alliance", "match"))
    expect_true("idvar" %in% names(attr(df, "reshapeWide")))
  else
    expect_true("idvar" %in% names(attr(df, "reshapeLong")))
}


# Retrieve data frames in team shape. ==========================================
sn <- GetSession("username", "key", season = 2016)
sched.t <- GetSchedule(sn, "ORPHI")
hybrid.t <- GetHybridSchedule(sn, "WAELL")
matches.t <- GetMatchResults(sn, "PNCMP")

test_that("Team shaped data frames are correct", {
  expect_equal(nrow(sched.t), 360)
  expect_named(sched.t, c("match", "description", "level", "field", "start",
                          "team", "alliance", "station", "surrogate"))
  expect_equal(row.names(sched.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                          "1.red1", "1.red2", "1.red3"))

  expect_equal(nrow(hybrid.t), 468)
  expect_named(hybrid.t, c("match", "description", "level", "start",
                           "actualStart", "team", "alliance", "station",
                           "surrogate", "disqualified", "scoreFinal",
                           "scoreAuto", "scoreFoul"))
  expect_equal(row.names(hybrid.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                          "1.red1", "1.red2", "1.red3"))

  expect_equal(nrow(matches.t), 768)
  expect_named(matches.t, c("match", "description", "level", "actualStart",
                           "postResult", "team", "alliance", "station",
                           "disqualified", "scoreFinal", "scoreAuto",
                           "scoreFoul"))
  expect_equal(row.names(matches.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                           "1.red1", "1.red2", "1.red3"))
})


# ToAllianceShape Conversions ==================================================
test_that("ToAllianceShape converts a Schedule data frame", {
  sched.t.a <- ToAllianceShape(sched.t)
  expect_equal(nrow(sched.t.a), 120)
  expect_named(sched.t.a, c("match", "description", "level", "field", "start",
                            "alliance", "team.1", "surrogate.1", "team.2",
                            "surrogate.2", "team.3", "surrogate.3"))
  expect_equal(row.names(sched.t.a)[1:6], c("1.blue", "1.red", "2.blue",
                                            "2.red", "3.blue", "3.red"))
  expect_named(attributes(sched.t.a), c("names", "reshapeWide", "shape", "url",
                                        "local_test_data", "time_downloaded",
                                        "last_modified", "local_url",
                                       "row.names", "class"))

  CheckAttValues(sched.t.a, "Schedule")
})


test_that("ToAllianceShape converts a HybridSchedule data frame", {
  hybrid.t.a <- ToAllianceShape(hybrid.t)
  expect_equal(nrow(hybrid.t.a), 156)
  expect_named(hybrid.t.a, c("match", "description", "level", "start",
                             "actualStart", "alliance", "scoreFinal",
                             "scoreAuto", "scoreFoul", "team.1", "surrogate.1",
                             "disqualified.1", "team.2", "surrogate.2",
                             "disqualified.2", "team.3", "surrogate.3",
                             "disqualified.3"))
  expect_equal(row.names(hybrid.t.a)[1:6], c("1.blue", "1.red", "2.blue",
                                            "2.red", "3.blue", "3.red"))
  expect_named(attributes(hybrid.t.a), c("names", "reshapeWide", "shape", "url",
                                        "local_test_data", "time_downloaded",
                                        "last_modified", "local_url",
                                        "row.names", "class"))

  CheckAttValues(hybrid.t.a, "HybridSchedule")
})


test_that("ToAllianceShape converts a MatchResults data frame", {
  matches.t.a <- ToAllianceShape(matches.t)
  expect_equal(nrow(matches.t.a), 256)
  expect_named(matches.t.a, c("match", "description", "level", "actualStart",
                              "postResult", "alliance", "scoreFinal",
                              "scoreAuto", "scoreFoul", "team.1",
                              "disqualified.1", "team.2", "disqualified.2",
                              "team.3", "disqualified.3"))
  expect_equal(row.names(matches.t.a)[1:6], c("1.blue", "1.red", "2.blue",
                                             "2.red", "3.blue", "3.red"))
  expect_named(attributes(matches.t.a), c("names", "reshapeWide", "shape",
                                          "url", "local_test_data",
                                          "time_downloaded", "last_modified",
                                          "local_url", "row.names", "class"))
  CheckAttValues(matches.t.a, "MatchResults")
})


# ToMatchShape Conversions =====================================================
test_that("ToMatchShape converts a Schedule data frame", {
  sched.t.m <- ToMatchShape(sched.t)
  expect_equal(nrow(sched.t.m), 60)
  expect_named(sched.t.m, c("match", "description", "level", "field", "start",
                            "team.Blue1", "surrogate.Blue1", "team.Blue2",
                            "surrogate.Blue2", "team.Blue3", "surrogate.Blue3",
                            "team.Red1", "surrogate.Red1", "team.Red2",
                            "surrogate.Red2", "team.Red3", "surrogate.Red3"))
  expect_equal(row.names(sched.t.m)[1:6], c("1", "2", "3", "4", "5", "6"))
  expect_named(attributes(sched.t.m), c("names", "reshapeWide", "shape", "url",
                                        "local_test_data", "time_downloaded",
                                        "last_modified", "local_url",
                                        "row.names", "class"))
  CheckAttValues(sched.t.m, "Schedule")
})


test_that("ToMatchShape converts a HybridSchedule data frame", {
  hybrid.t.m <- ToMatchShape(hybrid.t)
  expect_equal(nrow(hybrid.t.m), 78)
  expect_named(hybrid.t.m, c("match", "description", "level", "start",
                             "actualStart", "scoreFinal.Blue", "scoreAuto.Blue",
                             "scoreFoul.Blue", "scoreFinal.Red",
                             "scoreAuto.Red", "scoreFoul.Red", "team.Blue1",
                             "surrogate.Blue1", "disqualified.Blue1",
                             "team.Blue2", "surrogate.Blue2",
                             "disqualified.Blue2", "team.Blue3",
                             "surrogate.Blue3", "disqualified.Blue3",
                             "team.Red1", "surrogate.Red1", "disqualified.Red1",
                             "team.Red2", "surrogate.Red2", "disqualified.Red2",
                             "team.Red3", "surrogate.Red3",
                             "disqualified.Red3"))
  expect_equal(row.names(hybrid.t.m)[1:6], c("1", "2", "3", "4", "5", "6"))
  expect_named(attributes(hybrid.t.m), c("names", "reshapeWide", "shape", "url",
                                         "local_test_data", "time_downloaded",
                                         "last_modified", "local_url",
                                         "row.names", "class"))
  CheckAttValues(hybrid.t.m, "HybridSchedule")
})


test_that("ToMatchShape converts a MatchResults data frame", {
  matches.t.m <- ToMatchShape(matches.t)
  expect_equal(nrow(matches.t.m), 128)
  expect_named(matches.t.m, c("match", "description", "level", "actualStart",
                              "postResult", "scoreFinal.Blue", "scoreAuto.Blue",
                              "scoreFoul.Blue", "scoreFinal.Red",
                              "scoreAuto.Red", "scoreFoul.Red", "team.Blue1",
                              "disqualified.Blue1", "team.Blue2",
                              "disqualified.Blue2", "team.Blue3",
                              "disqualified.Blue3", "team.Red1",
                              "disqualified.Red1", "team.Red2",
                              "disqualified.Red2", "team.Red3",
                              "disqualified.Red3"))
  expect_equal(row.names(matches.t.m)[1:6], c("1", "2", "3", "4", "5", "6"))
  expect_named(attributes(matches.t.m), c("names", "reshapeWide", "shape",
                                          "url", "local_test_data",
                                          "time_downloaded", "last_modified",
                                          "local_url", "row.names", "class"))
  CheckAttValues(matches.t.m, "MatchResults")
})


# ToTeamShape Conversions ======================================================
test_that("ToTeamShape converts Schedule data frame", {
  # Convert from alliance shape
  sched.t.a.t <- ToTeamShape(ToAllianceShape(sched.t))
  expect_equal(nrow(sched.t.a.t), 360)
  expect_named(sched.t.a.t, c("match", "description", "level", "field", "start",
                              "team", "alliance", "station", "surrogate"))
  expect_equal(row.names(sched.t.a.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                          "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(sched.t.a.t), c("names", "reshapeLong", "url",
                                          "local_test_data", "time_downloaded",
                                          "last_modified", "local_url",
                                          "row.names", "class", "shape"))
  CheckAttValues(sched.t.a.t, "Schedule")

  # Convert from match shape.
  sched.t.m.t <- ToTeamShape(ToMatchShape(sched.t))
  expect_equal(nrow(sched.t.m.t), 360)
  expect_named(sched.t.m.t, c("match", "description", "level", "field", "start",
                              "team", "alliance", "station", "surrogate"))
  expect_equal(row.names(sched.t.m.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                              "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(sched.t.m.t), c("names", "reshapeLong", "url",
                                          "local_test_data", "time_downloaded",
                                          "last_modified", "local_url",
                                          "row.names", "class", "shape"))
  CheckAttValues(sched.t.m.t, "Schedule")
})


test_that("ToTeamShape converts a HybridSchedule data frame", {
  # Convert from alliance shape
  hybrid.t.a.t <- ToTeamShape(ToAllianceShape(hybrid.t))
  expect_equal(nrow(hybrid.t.a.t), 468)
  expect_named(hybrid.t.a.t, c("match", "description", "level", "start",
                               "actualStart", "team", "alliance", "station",
                               "surrogate", "disqualified", "scoreFinal",
                               "scoreAuto", "scoreFoul"))
  expect_equal(row.names(hybrid.t.a.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                              "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(hybrid.t.a.t), c("names", "reshapeLong", "url",
                                          "local_test_data", "time_downloaded",
                                          "last_modified", "local_url",
                                          "row.names", "class", "shape"))
  CheckAttValues(hybrid.t.a.t, "HybridSchedule")

  # Convert from match shape
  hybrid.t.m.t <- ToTeamShape(ToMatchShape(hybrid.t))
  expect_equal(nrow(hybrid.t.m.t), 468)
  expect_named(hybrid.t.m.t, c("match", "description", "level", "start",
                               "actualStart", "team", "alliance", "station",
                               "surrogate", "disqualified", "scoreFinal",
                               "scoreAuto", "scoreFoul"))
  expect_equal(row.names(hybrid.t.m.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                               "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(hybrid.t.m.t), c("names", "reshapeLong", "url",
                                           "local_test_data", "time_downloaded",
                                           "last_modified", "local_url",
                                           "row.names", "class", "shape"))
  CheckAttValues(hybrid.t.m.t, "HybridSchedule")
})


test_that("ToTeamShape converts a MatchResults data frame", {
  # Convert from alliance shape
  matches.t.a.t <- ToTeamShape(ToAllianceShape(matches.t))
  expect_equal(nrow(matches.t.a.t), 768)
  expect_named(matches.t.a.t, c("match", "description", "level", "actualStart",
                                "postResult", "team", "alliance", "station",
                                "disqualified", "scoreFinal", "scoreAuto",
                                "scoreFoul"))
  expect_equal(row.names(matches.t.a.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                               "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(matches.t.a.t), c("names", "reshapeLong", "url",
                                           "local_test_data", "time_downloaded",
                                           "last_modified", "local_url",
                                           "row.names", "class", "shape"))
  CheckAttValues(matches.t.a.t, "MatchResults")

  # Convert from match shape
  matches.t.m.t <- ToTeamShape(ToMatchShape(matches.t))
  expect_equal(nrow(matches.t.m.t), 768)
  expect_named(matches.t.a.t, c("match", "description", "level", "actualStart",
                                "postResult", "team", "alliance", "station",
                                "disqualified", "scoreFinal", "scoreAuto",
                                "scoreFoul"))
  expect_equal(row.names(matches.t.m.t)[1:6], c("1.blue1", "1.blue2", "1.blue3",
                                               "1.red1", "1.red2", "1.red3"))
  expect_named(attributes(matches.t.m.t), c("names", "reshapeLong", "url",
                                           "local_test_data", "time_downloaded",
                                           "last_modified", "local_url",
                                           "row.names", "class", "shape"))
  CheckAttValues(matches.t.m.t, "MatchResults")
})

