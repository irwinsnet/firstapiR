# test_90_util.R
# Version 2.0.0


context("firstapiR Utility Functions")

sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}

test_that("GetAll via local data", {
  archimedes <- GetAll(sess_local, "ARCHIMEDES")
  expect_length(archimedes, 16)
  expect_named(archimedes, c("season", "event", "teams", "schedule.qual",
                       "schedule.playoff", "hybrid.qual", "hybrid.playoff",
                       "matches.qual", "matches.playoff", "scores.qual",
                       "scores.playoff", "results.qual",
                       "results.playoff", "rankings", "alliances",
                       "awards"))
  expect_is(archimedes$results.qual, "Results")
  expect_is(archimedes$results.playoff, "Results")
  expect_length(archimedes$results.qual, 49)
  expect_equal(nrow(archimedes$results.qual), 468)
})
