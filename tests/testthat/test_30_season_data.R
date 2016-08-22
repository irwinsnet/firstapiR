# test_30_season_data.R

context("FIRST_R Season Data Functions")


# GetSeason() ==================================================================
test_that("GetSeason() returns a local data frame", {
  season <- GetSeason(sess_local)

  expect_is(season, "Season")
  expect_equal(attr(season, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/")
  expect_equal(nrow(season), 1)
  expect_equal(length(season), 8)
  expect_equal(names(season), c("eventCount", "gameName", "kickoff",
                                "rookieStart", "teamCount",
                                "FRCChampionships.name",
                                "FRCChampionships.startDate",
                                "FRCChampionships.location"))
})


test_that("GetSeason() returns a data frame via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  season <- GetSeason(sess_http)
  expect_is(season, "Season")
})


# GetDistricts() ===============================================================
test_that("GetDistricts() returns a local data frame", {
  dst <- GetDistricts(sess_local)

  expect_is(dst, "Districts")
  expect_equal(attr(dst, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/districts")
  expect_equal(nrow(dst), 8)
  expect_equal(length(dst), 3)
  expect_equal(names(dst), c("code", "name", "districtCount"))
})


test_that("GetDistricts() returns a data frame via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  dst <- GetDistricts(sess_http)
  expect_is(dst, "Districts")
})


# GetEvents() ==================================================================
test_that("GetEvents() returns local data frame", {
  evt <- GetEvents(sess_local, district = "PNW")
  expect_is(evt, "Events")
  expect_equal(attr(evt, "url"),
      paste("https://frc-api.firstinspires.org/v2.0/2016/",
            "events?districtCode=PNW", sep = ""))
  expect_equal(length(evt), 13)
  expect_equal(nrow(evt), 10)
  expect_true(is.factor(evt$type))
  expect_true(is.factor(evt$districtCode))
  expect_true(is.factor(evt$stateprov))
  expect_true(is.factor(evt$country))
  expect_true(is.factor(evt$timezone))

})

test_that("GetEvents() returns a data frame via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  evt <- GetEvents(sess_http, event = "WAAMV")
  expect_equal(nrow(evt), 1)
  expect_equal(evt$code, "WAAMV")

  evt <- GetEvents(sess_http, exclude_district = TRUE)
  expect_equal(nrow(evt), 70)

  evt <- GetEvents(sess_http, district = "PNW", team = "1318")
  expect_equal(nrow(evt), 4)
})


# GetTeams() ===================================================================
test_that("GetTeams() returns a local data frame", {
  tms <- GetTeams(sess_local, state = "Idaho")
  expect_is(tms, "Teams")
  expect_equal(length(tms), 14)
  expect_equal(nrow(tms), 15)
  expect_true(is.factor(tms$stateProv))
  expect_true(is.factor(tms$country))
  expect_true(is.factor(tms$districtCode))
})



test_that("GetTeams() returns a data frame via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  tms <- GetTeams(sess_http, district = "PNW")
  expect_is(tms, "Teams")
  expect_equal(nrow(tms), 158)
})



test_that("GetTeams() throws errors or warnings for incorrect arguments", {
  expect_error(GetTeams(sess_local, team = 1318, event = "WAAMV"),
              "If you specify a team, you cannot specify any other arguments")
  expect_warning(GetTeams(sess_local, district = "PNW", page=2),
              "Do not specify GetTeams page argument for data frame format")
})

