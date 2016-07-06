library("testthat")

context("FIRST_R Schedule Functions")

test_that("GetSchedule() returns a data frame", {
  sess <- GetSession(username, key)
  sched <- GetSchedule(sess, event = "PNCMP")
  
  expect_equal(class(sched), "data.frame")
  expect_equal(nrow(sched), 128)
  expect_equal(length(sched), 17)
  
  sched <- GetSchedule(sess, event = "PNCMP", expand_rows = TRUE)
  expect_equal(class(sched), "data.frame")
  expect_equal(nrow(sched), 768)
  expect_equal(length(sched), 8)
  
  sched <- GetSchedule(sess, event = "WAAMV", start = 10, end = 30)
  expect_equal(nrow(sched), 21)
  
  sched <- GetSchedule(sess, event = "WAAMV", level = "playoff")
  expect_equal(nrow(sched), 16)
  expect_equal(sched$tournamentLevel[[1]], "Playoff")
  
  rm(sess, sched)
})
