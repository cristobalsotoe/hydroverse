test_that("streamflow_stats works correctly", {
  flow <- c(10, 20, 15, 25)
  result <- streamflow_stats(flow)
  expect_equal(result$mean, 17.5)
  expect_equal(result$max, 25)
  expect_equal(result$min, 10)
})
