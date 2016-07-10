library("testthat")

context("FIRST_R Awards")

sess <- GetSession(username, key)

test_that("GetAwards() returns a data frame", {
  awards <- GetAwards(sess, event = "PNCMP")
  
  expect_equal(class(awards), "data.frame")
  expect_equal(nrow(awards), 56)
  expect_equal(length(awards), 10)
  expect_equal(names(awards), c("awardId", "teamId", "eventId",
                                "eventDivisionId", "eventCode", "name",
                                "series", "teamNumber", "fullTeamName",
                                "person"))
  
})

test_that("GetAwards() team argument filters results", {
  awards <- GetAwards(sess, event = "ORPHI", team = 1983)
  
  expect_equal(nrow(awards), 2)
})

test_that("GetAwards() throws errors for incorrect arguments", {
  expect_error(GetAwards(sess),
               "You must specify either a team number or event code")
})
# 
# test_that("GetRankings() returns a data frame", {
#   rks <- GetRankings(sess, "PNCMP")
# 
#   expect_equal(class(rks), "data.frame")
#   expect_equal(nrow(rks), 64)
#   expect_equal(length(rks), 14)
#   expect_equal(names(rks), c("rank", "teamNumber", "sortOrder1", "sortOrder2",
#                             "sortOrder3", "sortOrder4", "sortOrder5",
#                             "sortOrder6", "wins", "losses", "ties",
#                             "qualAverage", "dq", "matchesPlayed"))
# })
# 
# test_that("GetRankings() team and top arguments work", {
#   rks <- GetRankings(sess, "PNCMP", team = 5803)
#   expect_equal(nrow(rks), 1)
#   expect_equal(rks$teamNumber[[1]], 5803)
#   
#   rks <- GetRankings(sess, "PNCMP", top = 5)
#   expect_equal(nrow(rks), 5)
#   expect_equal(rks$rank, 1:5)
# })
# 
# test_that("GetRankings() throws errors for incorrect arguments", {
#   expect_error(GetRankings(sess, event = "WAELL", team = 4911, top = 5),
#                "You cannot specify both the team and top argument")
# })
rm(sess)
