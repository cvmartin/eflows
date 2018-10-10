library(eflows)
context("work of allocate()")

test_that("equal battery doesn't throw error", {
  expect_equal(allocate(50, c(25,50), c(75, 75), c(0.5, 0.5))[[3]],
               0)
})

test_that("share", {
  expect_equal(allocate(60, c(25,50), c(75, 75), c(0.7, 0.3))[[2]],
               c(42, 18))
})

test_that("work with discharge", {
  expect_equal(allocate(-100, c(25,50), c(0, 0), c(0.5, 0.5), eff = c(0.9,0.9))[[3]],
              -32.5)
})

test_that("expect errors", {
  expect_error(allocate(40, c(30, 50, 10), c(0, 70, 50)),
               "cannot be executed in the same call")
})


