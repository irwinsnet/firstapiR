context("FIRST_R Rankings and Alliances")


sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}
# GetAlliances =================================================================
test_that("GetAlliances() returns a local data frame", {
  al <- GetAlliances(sess_local, event = "WAAMV")

  expect_is(al, "Alliances")
  expect_equal(attr(al, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/alliances/WAAMV")
  expect_equal(nrow(al), 8)
  expect_equal(length(al), 9)
})


test_that("GetAlliances returns a data frame via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  al <- GetAlliances(sess_http, event = "WAELL")
  expect_is(al, "Alliances")
  expect_equal(attr(al, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/alliances/WAELL")
  expect_equal(nrow(al), 8)
  expect_equal(length(al), 9)

})


# GetRankings() ================================================================
test_that("GetRankings() returns a data frame", {
  rks <- GetRankings(sess_local, "wAAMV")

  expect_is(rks, "Rankings")
  expect_equal(nrow(rks), 39)
  expect_equal(length(rks), 14)
  expect_equal(names(rks), c("rank", "teamNumber", "sortOrder1", "sortOrder2",
                            "sortOrder3", "sortOrder4", "sortOrder5",
                            "sortOrder6", "wins", "losses", "ties",
                            "qualAverage", "dq", "matchesPlayed"))
})


test_that("GetRankings() team and top arguments via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  rks <- GetRankings(sess_http, "PNCMP", team = 5803)
  expect_equal(nrow(rks), 1)
  expect_equal(rks$teamNumber[[1]], 5803)

  rks <- GetRankings(sess_http, "PNCMP", top = 5)
  expect_equal(nrow(rks), 5)
  expect_equal(rks$rank, 1:5)
})


test_that("GetRankings() throws errors for incorrect arguments", {
  expect_error(GetRankings(sess_local, event = "WAELL", team = 4911, top = 5),
               "You cannot specify both the team and top argument")
})
