library("testthat")

context("FIRST_R Schedule Functions")

test_that("GetSchedule() returns a data frame", {
  sess <- GetSession(username, key)
  sched <- GetSchedule(sess, event = "PNCMP")
  
  expect_equal(class(sched), "data.frame")
  expect_equal(nrow(sched), 128)
  expect_equal(length(sched), 17)
  rm(sess, sched)
})
