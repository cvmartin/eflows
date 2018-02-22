library(eflows)
context("work of allocate()")

test_that("basic working", {
  expect_equal(allocate(23, c(0.5, 0.5), c(2,0), c(20, 1)),
               c(18,1,4))
  expect_equal(allocate(130, c(9, 2, 5, 1), c(30, 30, 20, 10), c(90,40, 50, 80)),
               c(60, 10, 30, 30, 0))
})

