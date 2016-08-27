
context("FIRST_R Awards")

sess_http_valid <- FALSE
sess_local <- GetSession("username", "key")
if(exists("username") & exists("key")) {
  sess_http <- GetSession(username, key)
  sess_http_valid <- TRUE
}

# GetAwards() ==================================================================
test_that("GetAwards() returns a local data frame", {
  awards <- GetAwards(sess_local, event = "PNCMP")

  expect_is(awards, "Awards")
  expect_equal(attr(awards, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/awards/PNCMP")
  expect_equal(nrow(awards), 56)
  expect_equal(length(awards), 10)
  expect_equal(names(awards), c("awardId", "teamId", "eventId",
                                "eventDivisionId", "eventCode", "name",
                                "series", "teamNumber", "fullTeamName",
                                "person"))
})


test_that("GetAwards team argument via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  awards <- GetAwards(sess_http, event = "ARCHIMEDES", team = 180)
  expect_equal(nrow(awards), 1)
})

test_that("GetAwards throws errors for incorrect arguments", {
  expect_error(GetAwards(sess_local),
               "You must specify either a team number or event code")
})


# GetAwardsList() ==============================================================
test_that("GetAwardsList() returns a data frame", {
  alist <- GetAwardsList(sess_local)

  expect_is(alist, "AwardsList")
  expect_equal(attr(alist, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/awards/list")
  expect_equal(nrow(alist), 92)
  expect_equal(length(alist), 4)
  expect_equal(names(alist), c("awardId", "eventType", "description",
                             "forPerson"))
})


test_that("GetAwardsList via HTTP", {
  if(!sess_http_valid) skip("No username or authorization key")

  alist <- GetAwardsList(sess_http)

  expect_is(alist, "AwardsList")
  expect_equal(attr(alist, "url"),
               "https://frc-api.firstinspires.org/v2.0/2016/awards/list")
  expect_equal(nrow(alist), 92)
  expect_equal(length(alist), 4)
  expect_equal(names(alist), c("awardId", "eventType", "description",
                               "forPerson"))
})
