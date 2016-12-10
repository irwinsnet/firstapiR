# test_90_util.R
# Version 2.0.0


context("firstapiR Utility Functions")

sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}

test_that("GetAll via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  archimedes <- GetAll(sn, "ARCHIMEDES")
  expect_length(archimedes, 16)
  expect_named(arch, c("season", "event", "teams", "schedule.qual",
                       "schedule.playoff", "hybrid.qual", "hybrid.playoff",
                       "matches.qual", "matches.playoff", "scores.qual",
                       "scores.playoff", "merged.results.qual",
                       "merged.results.playoff", "rankings", "alliances",
                       "awards"))
})
