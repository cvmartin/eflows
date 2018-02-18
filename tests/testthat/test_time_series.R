library(eflows)
context("usage of time series")

test_that("kw_to_kwh and kwh_to_kw act in reverse", {
  expect_true(exists("qhour"))
  expect_equivalent(qhour, kwh_to_kw(kw_to_kwh(qhour)))
})
