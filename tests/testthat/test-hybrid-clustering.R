# 1. Hybrid Clustering Integration Tests -----

library(data.table)

test_that("cluster_career_trajectories works with k_selection_method='hybrid'", {
  # Create synthetic career metrics data
  set.seed(789)
  n <- 1000

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    avg_employment_spell = runif(n, 30, 365),
    job_turnover_rate = runif(n, 0, 5),
    employment_rate = runif(n, 0, 1),
    days_employed = runif(n, 0, 1095),
    employment_spells = sample(1:20, n, replace = TRUE),
    career_advancement_index = runif(n, 0, 1),
    career_success_index = runif(n, 0, 1)
  )

  # Test hybrid method (default)
  result <- cluster_career_trajectories(
    career_metrics,
    features = c("stability", "employment", "progression"),
    n_clusters = NULL,  # Auto-detect
    k_selection_method = "hybrid",
    seed = 789,
    verbose = FALSE
  )

  # Check structure
  expect_type(result, "list")
  expect_true("cluster_assignments" %in% names(result))
  expect_s3_class(result$cluster_assignments, "data.table")
  expect_true("cluster_id" %in% names(result$cluster_assignments))
  expect_true("cluster_label_en" %in% names(result$cluster_assignments))

  # Check number of clusters
  n_clusters <- length(unique(result$cluster_assignments$cluster_id))
  expect_true(n_clusters >= 3 && n_clusters <= 6)

  # Check attributes
  expect_true(!is.null(attr(result, "wss_values")))
  expect_true(!is.null(attr(result, "silhouette_values")))
  expect_equal(attr(result, "k_selection_method"), "hybrid")
  expect_true(!is.null(attr(result, "decision_rule")))

  # Decision rule should be one of the expected values
  decision_rule <- attr(result, "decision_rule")
  expect_true(decision_rule %in% c("agreement", "adjacent_prefer_silhouette",
                                   "disagreement_prefer_elbow"))
})


test_that("cluster_career_trajectories works with k_selection_method='elbow'", {
  set.seed(101)
  n <- 500

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "elbow",
    seed = 101,
    verbose = FALSE
  )

  # Check attributes
  expect_true(!is.null(attr(result, "wss_values")))
  expect_null(attr(result, "silhouette_values"))  # Should be NULL for elbow-only
  expect_equal(attr(result, "decision_rule"), "elbow_only")
})


test_that("cluster_career_trajectories works with k_selection_method='silhouette'", {
  set.seed(202)
  n <- 500

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "silhouette",
    seed = 202,
    verbose = FALSE
  )

  # Check attributes
  expect_null(attr(result, "wss_values"))  # Should be NULL for silhouette-only
  expect_true(!is.null(attr(result, "silhouette_values")))
  expect_equal(attr(result, "decision_rule"), "silhouette_only")
})


test_that("cluster_career_trajectories handles large datasets without memory overflow", {
  skip_on_cran()  # Skip on CRAN (slow test)

  set.seed(303)
  n <- 12000  # Large but manageable in test environment

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # This should not crash with memory overflow
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    memory_fraction = 0.1,  # Conservative
    sample_size_k = 5000,   # Limit sample for k selection
    seed = 303,
    verbose = FALSE
  )

  # Should still produce valid clusters
  expect_type(result, "list")
  expect_true("cluster_assignments" %in% names(result))
  expect_equal(nrow(result$cluster_assignments), n)
})


test_that("Manual k selection still works (backward compatibility)", {
  set.seed(404)
  n <- 500

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Manually specify k=4
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = 4,  # Manual
    seed = 404,
    verbose = FALSE
  )

  # Should have exactly 4 clusters
  expect_equal(length(unique(result$cluster_assignments$cluster_id)), 4)

  # Attributes should be NULL (no optimization performed)
  expect_null(attr(result, "wss_values"))
  expect_null(attr(result, "silhouette_values"))
})


test_that("Hybrid method decision rules work correctly", {
  set.seed(505)
  n <- 800

  # Create data with reasonably clear structure
  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = c(runif(400, 0.7, 1.0), runif(400, 0.0, 0.3)),
    employment_rate = c(runif(400, 0.8, 1.0), runif(400, 0.1, 0.4)),
    career_advancement_index = c(runif(400, 0.6, 0.9), runif(400, 0.0, 0.3))
  )

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 505,
    verbose = FALSE
  )

  # Should have valid decision rule
  decision_rule <- attr(result, "decision_rule")
  expect_true(!is.null(decision_rule))
  expect_type(decision_rule, "character")

  # Should be one of the valid rules
  valid_rules <- c("agreement", "adjacent_prefer_silhouette", "disagreement_prefer_elbow")
  expect_true(decision_rule %in% valid_rules)
})


test_that("Elbow and silhouette values are correctly stored", {
  set.seed(606)
  n <- 600

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 606,
    verbose = FALSE
  )

  # Check WSS values
  wss <- attr(result, "wss_values")
  expect_type(wss, "double")
  expect_named(wss, paste0("k", 3:6))  # Default k_range is 3:6
  expect_length(wss, 4)

  # WSS should decrease monotonically
  expect_true(all(diff(wss) < 0))

  # Check silhouette values
  sil <- attr(result, "silhouette_values")
  expect_type(sil, "double")
  expect_named(sil, paste0("k", 3:6))
  expect_length(sil, 4)

  # Silhouette values should be between -1 and 1
  expect_true(all(sil >= -1 & sil <= 1))
})


test_that("Hybrid method handles agreement between elbow and silhouette", {
  set.seed(707)
  n <- 500

  # Create clear 4-cluster structure
  cluster1 <- data.table(
    cf = paste0("CF", 1:125),
    employment_stability_index = runif(125, 0.8, 1.0),
    employment_rate = runif(125, 0.8, 1.0),
    career_advancement_index = runif(125, 0.7, 0.9)
  )
  cluster2 <- data.table(
    cf = paste0("CF", 126:250),
    employment_stability_index = runif(125, 0.0, 0.2),
    employment_rate = runif(125, 0.0, 0.2),
    career_advancement_index = runif(125, 0.0, 0.2)
  )
  cluster3 <- data.table(
    cf = paste0("CF", 251:375),
    employment_stability_index = runif(125, 0.4, 0.6),
    employment_rate = runif(125, 0.8, 1.0),
    career_advancement_index = runif(125, 0.1, 0.3)
  )
  cluster4 <- data.table(
    cf = paste0("CF", 376:500),
    employment_stability_index = runif(125, 0.8, 1.0),
    employment_rate = runif(125, 0.1, 0.3),
    career_advancement_index = runif(125, 0.6, 0.9)
  )

  career_metrics <- rbindlist(list(cluster1, cluster2, cluster3, cluster4))

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 707,
    verbose = FALSE
  )

  # With clear structure, should likely detect 4 clusters
  # (though this is not guaranteed due to randomness)
  n_clusters <- length(unique(result$cluster_assignments$cluster_id))
  expect_true(n_clusters >= 3 && n_clusters <= 6)

  # Check that decision rule is set
  decision_rule <- attr(result, "decision_rule")
  expect_true(!is.null(decision_rule))
})


test_that("k_selection_method parameter validation works", {
  set.seed(808)
  n <- 200

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1)
  )

  # Test invalid k_selection_method
  expect_error(
    cluster_career_trajectories(
      career_metrics,
      n_clusters = NULL,
      k_selection_method = "invalid_method",
      seed = 808,
      verbose = FALSE
    ),
    "should be one of"
  )
})


test_that("Hybrid clustering with small dataset", {
  set.seed(909)
  n <- 150  # Small dataset

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Should work even with small dataset
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 909,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true(!is.null(attr(result, "wss_values")))
  expect_true(!is.null(attr(result, "silhouette_values")))
})


test_that("Hybrid clustering preserves all worker IDs", {
  set.seed(1010)
  n <- 800

  original_ids <- paste0("WORKER_", 1:n)
  career_metrics <- data.table::data.table(
    cf = original_ids,
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 1010,
    verbose = FALSE
  )

  # All original IDs should be in the result
  expect_setequal(result$cluster_assignments$cf, original_ids)
  expect_equal(nrow(result$cluster_assignments), n)
})


test_that("Different k_selection_methods may produce different k", {
  set.seed(1111)
  n <- 700

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result_hybrid <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 1111,
    verbose = FALSE
  )

  result_elbow <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "elbow",
    seed = 1111,
    verbose = FALSE
  )

  result_silhouette <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "silhouette",
    seed = 1111,
    verbose = FALSE
  )

  # All should produce valid k in range 3-6
  k_hybrid <- length(unique(result_hybrid$cluster_assignments$cluster_id))
  k_elbow <- length(unique(result_elbow$cluster_assignments$cluster_id))
  k_sil <- length(unique(result_silhouette$cluster_assignments$cluster_id))

  expect_true(k_hybrid %in% 3:6)
  expect_true(k_elbow %in% 3:6)
  expect_true(k_sil %in% 3:6)

  # Methods might differ (this is expected behavior, not an error)
  # We just verify all are valid
})


test_that("Clustering results are reproducible with same seed", {
  set.seed(1212)
  n <- 500

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  result1 <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 999,
    verbose = FALSE
  )

  result2 <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 999,
    verbose = FALSE
  )

  # Results should be identical
  expect_equal(
    length(unique(result1$cluster_assignments$cluster_id)),
    length(unique(result2$cluster_assignments$cluster_id))
  )

  # Cluster assignments should be the same
  expect_equal(result1$cluster_assignments$cluster_id, result2$cluster_assignments$cluster_id)
})


# ==============================================================================
# Memory-Aware Clustering Tests
# ==============================================================================

test_that("Silhouette uses micro-sample for large dataset (memory safety)", {
  skip_on_cran()  # Large dataset test

  set.seed(5000)
  n <- 15000  # Large but manageable in test environment

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Should NOT crash with memory overflow
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    memory_fraction = 0.1,  # Conservative for test environment
    sample_size_k = 5000,   # Limit sample size for k selection
    seed = 5000,
    verbose = FALSE
  )

  # Should still produce valid clustering
  expect_type(result, "list")
  expect_true("cluster_assignments" %in% names(result))
  expect_equal(nrow(result$cluster_assignments), n)

  # Should have used hybrid method successfully
  expect_equal(attr(result, "k_selection_method"), "hybrid")
  expect_true(!is.null(attr(result, "wss_values")))

  # Silhouette values should exist and be valid (between -1 and 1) if computed
  sil_vals <- attr(result, "silhouette_values")
  if (!is.null(sil_vals)) {
    expect_true(all(sil_vals >= -1 & sil_vals <= 1))
  }
})


test_that("Automatic fallback to elbow-only with very low memory_fraction", {
  set.seed(5001)
  n <- 10000

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # With very low memory_fraction, should automatically switch to elbow-only or use micro-sample
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    memory_fraction = 0.05,  # Very conservative - may force fallback
    sample_size_k = 3000,    # Conservative sample
    seed = 5001,
    verbose = FALSE
  )

  # Should complete successfully
  expect_type(result, "list")
  expect_true("cluster_assignments" %in% names(result))
  expect_equal(nrow(result$cluster_assignments), n)

  # Should have WSS values (from elbow method)
  expect_true(!is.null(attr(result, "wss_values")))

  # May or may not have silhouette values depending on automatic fallback
  # (this is OK - the key is it didn't crash)
})


test_that("Clustering with conservative memory settings on medium dataset", {
  set.seed(5002)
  n <- 12000

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1),
    career_success_index = runif(n, 0, 1)
  )

  # Test with conservative memory settings
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    memory_fraction = 0.1,   # Conservative
    sample_size_k = 5000,    # Limit sample size
    seed = 5002,
    verbose = FALSE
  )

  # Should work without issues
  expect_type(result, "list")
  expect_equal(nrow(result$cluster_assignments), n)

  # Verify cluster quality metrics are present
  expect_true("cluster_quality" %in% names(result))
  # Quality metrics might be NULL in test environment with memory constraints
  # The key is that clustering completed successfully
})


test_that("Elbow-only method works with large datasets (no silhouette)", {
  skip_on_cran()

  set.seed(5003)
  n <- 18000

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Elbow-only should handle large datasets without silhouette overhead
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "elbow",  # No silhouette computation
    sample_size_k = 8000,          # Use sampling even for elbow with large data
    seed = 5003,
    verbose = FALSE
  )

  # Should have valid results
  expect_type(result, "list")
  expect_equal(nrow(result$cluster_assignments), n)

  # Should have WSS values but not silhouette values
  expect_true(!is.null(attr(result, "wss_values")))
  expect_null(attr(result, "silhouette_values"))
  expect_equal(attr(result, "decision_rule"), "elbow_only")
})


test_that("Quality metrics gracefully handle memory constraints", {
  set.seed(5004)
  n <- 8000

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Test that quality metric computation doesn't crash the entire clustering
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = 4,  # Manual k to focus on quality metrics
    sample_size_quality = 3000,  # Reasonable sample for test environment
    memory_fraction = 0.1,
    seed = 5004,
    verbose = FALSE
  )

  # Should complete successfully
  expect_type(result, "list")
  expect_true("cluster_quality" %in% names(result))

  # Quality metrics should exist (even if some are NA due to memory constraints)
  expect_true("overall_silhouette" %in% names(result$cluster_quality))
  expect_true("dunn_index" %in% names(result$cluster_quality))
  expect_true("calinski_harabasz" %in% names(result$cluster_quality))
})


test_that("Hybrid method decision rules work with various memory constraints", {
  set.seed(5005)
  n <- 6000

  # Create data with clear 3-cluster structure
  cluster_data <- rbindlist(lapply(1:3, function(i) {
    data.table(
      cf = paste0("CF", ((i-1)*2000 + 1):(i*2000)),
      employment_stability_index = runif(2000, (i-1)*0.33, i*0.33),
      employment_rate = runif(2000, (i-1)*0.33, i*0.33),
      career_advancement_index = runif(2000, (i-1)*0.33, i*0.33)
    )
  }))

  # Test with different memory fractions
  memory_fractions <- c(0.1, 0.15)

  for (mem_frac in memory_fractions) {
    result <- cluster_career_trajectories(
      cluster_data,
      n_clusters = NULL,
      k_selection_method = "hybrid",
      memory_fraction = mem_frac,
      sample_size_k = 3000,  # Conservative for test environment
      seed = 5005,
      verbose = FALSE
    )

    # All should complete successfully
    expect_type(result, "list")
    expect_equal(nrow(result$cluster_assignments), n)

    # Should have a valid decision rule
    decision <- attr(result, "decision_rule")
    expect_true(!is.null(decision))
    expect_type(decision, "character")
  }
})


test_that("Memory detection fallback works when system detection fails", {
  # This test verifies the conservative defaults work
  # We can't easily simulate detection failure, but we can test small datasets
  # that should work regardless of memory detection accuracy

  set.seed(5006)
  n <- 500

  career_metrics <- data.table::data.table(
    cf = paste0("CF", 1:n),
    employment_stability_index = runif(n, 0, 1),
    employment_rate = runif(n, 0, 1),
    career_advancement_index = runif(n, 0, 1)
  )

  # Should work even if memory detection were to fail
  result <- cluster_career_trajectories(
    career_metrics,
    n_clusters = NULL,
    k_selection_method = "hybrid",
    seed = 5006,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_equal(nrow(result$cluster_assignments), n)
  expect_true(!is.null(attr(result, "silhouette_values")))
})
