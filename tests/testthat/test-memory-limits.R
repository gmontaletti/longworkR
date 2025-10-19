# 1. Memory Limit Calculation Tests -----

library(data.table)

test_that(".calculate_memory_aware_limit applies stricter limits for silhouette", {
  # Test with p=5 features
  limit_clustering <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.2,
    p = 5,
    method = "clustering",
    verbose = FALSE
  )

  limit_silhouette <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.2,
    p = 5,
    method = "silhouette",
    verbose = FALSE
  )

  # Silhouette limit should be much smaller (0.5x safety factor)
  expect_true(limit_silhouette < limit_clustering)
  expect_true(limit_silhouette <= 10000)  # Hard cap

  # Should be approximately 0.5x
  ratio <- limit_silhouette / limit_clustering
  expect_true(ratio >= 0.4 && ratio <= 0.6)  # Allow some tolerance
})


test_that(".calculate_memory_aware_limit respects hard cap for silhouette", {
  # Even with large memory, silhouette should cap at 10K
  limit <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.5,  # Large fraction
    p = 3,
    method = "silhouette",
    verbose = FALSE
  )

  expect_true(limit <= 10000)
})


test_that(".calculate_memory_aware_limit returns reasonable values for clustering", {
  # Test clustering method with moderate memory fraction
  limit <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.33,
    p = 10,
    method = "clustering",
    verbose = FALSE
  )

  # Should be between 1K and 100K (safety bounds)
  expect_true(limit >= 1000)
  expect_true(limit <= 100000)

  # Should be an integer
  expect_true(limit == floor(limit))
})


test_that(".calculate_memory_aware_limit handles different memory fractions", {
  # Smaller memory fraction should give smaller limit
  limit_small <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.1,
    p = 5,
    method = "clustering",
    verbose = FALSE
  )

  limit_large <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.5,
    p = 5,
    method = "clustering",
    verbose = FALSE
  )

  expect_true(limit_small < limit_large)
})


test_that(".calculate_memory_aware_limit handles different numbers of features", {
  # More features should allow fewer observations (same memory)
  limit_few_features <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.33,
    p = 3,
    method = "clustering",
    verbose = FALSE
  )

  limit_many_features <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.33,
    p = 20,
    method = "clustering",
    verbose = FALSE
  )

  # More features means smaller sample limit (approximately)
  # But the effect is small because distance matrix is O(n^2), not O(n*p)
  expect_true(limit_many_features <= limit_few_features)
})


test_that(".calculate_memory_aware_limit verbose mode produces output", {
  # Test verbose output for clustering
  expect_output(
    longworkR:::.calculate_memory_aware_limit(
      memory_fraction = 0.33,
      p = 5,
      method = "clustering",
      verbose = TRUE
    ),
    "System RAM|Memory-aware sample limit"
  )

  # Test verbose output for silhouette
  expect_output(
    longworkR:::.calculate_memory_aware_limit(
      memory_fraction = 0.33,
      p = 5,
      method = "silhouette",
      verbose = TRUE
    ),
    "Silhouette micro-sample limit"
  )
})


test_that(".calculate_memory_aware_limit enforces minimum limit of 1000", {
  # Even with very small memory fraction, should have minimum 1000
  limit <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.01,  # Very small
    p = 50,  # Many features
    method = "clustering",
    verbose = FALSE
  )

  expect_true(limit >= 1000)
})


test_that(".calculate_memory_aware_limit enforces maximum limit of 100000 for clustering", {
  # Even with very large memory fraction, clustering should cap at 100K
  limit <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.9,  # Very large (unrealistic but testing bounds)
    p = 2,  # Few features
    method = "clustering",
    verbose = FALSE
  )

  expect_true(limit <= 100000)
})


test_that(".calculate_memory_aware_limit returns integer values", {
  limit1 <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.25,
    p = 7,
    method = "clustering",
    verbose = FALSE
  )

  limit2 <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.25,
    p = 7,
    method = "silhouette",
    verbose = FALSE
  )

  expect_type(limit1, "integer")
  expect_type(limit2, "integer")
})


test_that(".calculate_memory_aware_limit is deterministic", {
  # Same inputs should give same outputs
  limit1 <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.3,
    p = 8,
    method = "clustering",
    verbose = FALSE
  )

  limit2 <- longworkR:::.calculate_memory_aware_limit(
    memory_fraction = 0.3,
    p = 8,
    method = "clustering",
    verbose = FALSE
  )

  expect_equal(limit1, limit2)
})


test_that(".calculate_memory_aware_limit silhouette is always more conservative", {
  # Test multiple combinations
  test_cases <- expand.grid(
    memory_fraction = c(0.1, 0.2, 0.33, 0.5),
    p = c(3, 5, 10, 20)
  )

  for (i in 1:nrow(test_cases)) {
    mem_frac <- test_cases$memory_fraction[i]
    p_val <- test_cases$p[i]

    limit_clust <- longworkR:::.calculate_memory_aware_limit(
      memory_fraction = mem_frac,
      p = p_val,
      method = "clustering",
      verbose = FALSE
    )

    limit_sil <- longworkR:::.calculate_memory_aware_limit(
      memory_fraction = mem_frac,
      p = p_val,
      method = "silhouette",
      verbose = FALSE
    )

    # Silhouette should always be more conservative (smaller or equal)
    expect_true(limit_sil <= limit_clust,
                info = sprintf("Failed for memory_fraction=%g, p=%d", mem_frac, p_val))

    # Silhouette should be at most 10K
    expect_true(limit_sil <= 10000,
                info = sprintf("Failed for memory_fraction=%g, p=%d", mem_frac, p_val))
  }
})


test_that(".calculate_memory_aware_limit handles default parameters correctly", {
  # Test with default parameters (should not fail)
  limit <- longworkR:::.calculate_memory_aware_limit()

  expect_type(limit, "integer")
  expect_true(limit >= 1000)
  expect_true(limit <= 100000)
})
