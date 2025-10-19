# 1. Elbow Method Tests -----

library(data.table)

test_that(".elbow_method identifies optimal k correctly", {
  # Create toy dataset with clear 3-cluster structure
  set.seed(123)
  cluster1 <- matrix(rnorm(300, mean = 0), ncol = 3)
  cluster2 <- matrix(rnorm(300, mean = 5), ncol = 3)
  cluster3 <- matrix(rnorm(300, mean = -5), ncol = 3)
  features <- rbind(cluster1, cluster2, cluster3)

  # Test elbow method
  result <- longworkR:::.elbow_method(features, k_range = 2:5, seed = 123, verbose = FALSE)

  # Expectations
  expect_type(result, "list")
  expect_named(result, c("optimal_k", "wss_values", "knee_point"))
  expect_equal(result$optimal_k, result$knee_point)
  expect_true(result$optimal_k >= 2 && result$optimal_k <= 5)
  expect_length(result$wss_values, 4)

  # WSS should decrease monotonically
  expect_true(all(diff(result$wss_values) < 0))
})


test_that(".elbow_method handles edge cases", {
  # Single cluster data (all points from same distribution)
  set.seed(456)
  features <- matrix(rnorm(300), ncol = 3)

  result <- longworkR:::.elbow_method(features, k_range = 2:4, seed = 456, verbose = FALSE)

  expect_type(result, "list")
  expect_true(result$optimal_k %in% 2:4)

  # WSS should still decrease monotonically
  expect_true(all(diff(result$wss_values) < 0))
})


test_that(".elbow_method returns correct structure", {
  set.seed(789)
  features <- matrix(rnorm(600, mean = rep(c(0, 3, -3), each = 200)), ncol = 4)

  result <- longworkR:::.elbow_method(features, k_range = 3:6, seed = 789, verbose = FALSE)

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("optimal_k", "wss_values", "knee_point") %in% names(result)))

  # Check wss_values names
  expect_named(result$wss_values, paste0("k", 3:6))

  # Check that WSS values are numeric and positive
  expect_type(result$wss_values, "double")
  expect_true(all(result$wss_values > 0))

  # Check that optimal_k is within range
  expect_true(result$optimal_k %in% 3:6)
})


test_that(".elbow_method with verbose output", {
  set.seed(101)
  features <- matrix(rnorm(400), ncol = 4)

  # Capture output when verbose = TRUE
  expect_output(
    result <- longworkR:::.elbow_method(features, k_range = 2:4, seed = 101, verbose = TRUE),
    "Elbow method"
  )

  expect_type(result, "list")
})


test_that(".elbow_method handles small datasets", {
  set.seed(202)
  # Very small dataset
  features <- matrix(rnorm(60), ncol = 3)  # Only 20 observations

  result <- longworkR:::.elbow_method(features, k_range = 2:4, seed = 202, verbose = FALSE)

  expect_type(result, "list")
  expect_true(result$optimal_k %in% 2:4)
})


test_that(".elbow_method with different k ranges", {
  set.seed(303)
  features <- matrix(rnorm(900, mean = rep(c(0, 4, -4), each = 300)), ncol = 5)

  # Test with k_range = 3:6
  result1 <- longworkR:::.elbow_method(features, k_range = 3:6, seed = 303, verbose = FALSE)
  expect_length(result1$wss_values, 4)
  expect_true(result1$optimal_k %in% 3:6)

  # Test with k_range = 2:5
  result2 <- longworkR:::.elbow_method(features, k_range = 2:5, seed = 303, verbose = FALSE)
  expect_length(result2$wss_values, 4)
  expect_true(result2$optimal_k %in% 2:5)
})


test_that(".elbow_method detects knee point correctly", {
  set.seed(404)
  # Create clear 4-cluster structure
  cluster1 <- matrix(rnorm(200, mean = 0, sd = 0.5), ncol = 2)
  cluster2 <- matrix(rnorm(200, mean = 5, sd = 0.5), ncol = 2)
  cluster3 <- matrix(rnorm(200, mean = c(5, 0)), ncol = 2)
  cluster4 <- matrix(rnorm(200, mean = c(0, 5)), ncol = 2)
  features <- rbind(cluster1, cluster2, cluster3, cluster4)

  result <- longworkR:::.elbow_method(features, k_range = 2:6, seed = 404, verbose = FALSE)

  # With clear 4-cluster structure, optimal k should be 4 or close to it
  expect_true(result$optimal_k %in% 3:5)

  # Check that knee_point equals optimal_k
  expect_equal(result$optimal_k, result$knee_point)
})


test_that(".elbow_method is reproducible with seed", {
  set.seed(505)
  features <- matrix(rnorm(500), ncol = 5)

  result1 <- longworkR:::.elbow_method(features, k_range = 3:6, seed = 999, verbose = FALSE)
  result2 <- longworkR:::.elbow_method(features, k_range = 3:6, seed = 999, verbose = FALSE)

  # Results should be identical
  expect_equal(result1$optimal_k, result2$optimal_k)
  expect_equal(result1$wss_values, result2$wss_values)
  expect_equal(result1$knee_point, result2$knee_point)
})
