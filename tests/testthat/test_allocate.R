library(eflows)
context("work of allocate()")

test_that("efficiency", {
  expect_equal(allocate(100, c(25,50), c(70, 70), eff = c(0.8,0.8))[[3]],
               18.75)
})

test_that("share", {
  expect_equal(allocate(60, c(25,50), c(75, 75), c(0.7, 0.3))[[2]],
               c(42, 18))
})

test_that("levels", {
  expect_equal(allocate(120, c(9, 2, 5, 1), c(30, 30, 50, 30), c(90,40, 50, 80), level = c(3, 2, 1, 3))[[2]],
               c(21,28,42,29))
})

test_that("active", {
  expect_equal(allocate(70, c(50, 50, 50), c(0, 70, 50), active = c(FALSE, TRUE, TRUE))[[3]],
               50)
})

test_that("work with discharge", {
  expect_equal(allocate(-100, c(25,50), c(0, 0), c(0.5, 0.5), eff = c(0.9,0.9))[[3]],
              -32.5)
})

test_that("there should be no flow", {
  expect_equal(allocate(100, c(75,75), c(75, 75),eff = c(0.9,0.9))[[2]],
               c(0,0))
})

test_that("equal battery doesn't throw error", {
  expect_equal(allocate(50, c(25,50), c(75, 75), c(0.5, 0.5))[[3]],
               0)
})

test_that("expect errors", {
  expect_error(allocate(40, c(30, 50, 10), c(0, 70, 50)),
               "cannot be executed in the same call") 
  expect_error(allocate(100, c(25,50), c(0, 0), c(0.5, 0.5), eff = c(0.9,0.9)))
  expect_error(allocate(40, c(30, 50, 10), c(0, 70, 50))) 
  expect_error(allocate(70, c(50, 50, 50), c(0, 70, 50), active = c(TRUE, TRUE, TRUE)))
})


