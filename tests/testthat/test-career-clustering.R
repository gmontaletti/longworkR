library(data.table)

test_that("cluster_career_trajectories validates input correctly", {
  # Test with non-data.table input
  expect_error(
    cluster_career_trajectories(list(a = 1, b = 2)),
    "career_metrics must be a data.table"
  )

  # Test with invalid method
  sample_metrics <- data.table(
    cf = 1:100,
    employment_rate = runif(100),
    career_advancement_index = runif(100),
    employment_stability_index = runif(100)
  )

  expect_error(
    cluster_career_trajectories(sample_metrics, method = "invalid"),
    "method must be one of"
  )

  # Test with invalid n_clusters
  expect_error(
    cluster_career_trajectories(sample_metrics, n_clusters = 2),
    "n_clusters must be between 3 and 6"
  )

  expect_error(
    cluster_career_trajectories(sample_metrics, n_clusters = 10),
    "n_clusters must be between 3 and 6"
  )

  # Test with invalid features
  expect_error(
    cluster_career_trajectories(sample_metrics, features = c("invalid_feature")),
    "Invalid features"
  )

  # Test with missing ID column
  expect_error(
    cluster_career_trajectories(sample_metrics, id_column = "missing_id"),
    "not found in career_metrics"
  )
})


test_that("cluster_career_trajectories produces valid clusters with automatic k selection", {
  # Create sample career metrics data
  set.seed(123)
  n_workers <- 200

  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.2, 1.0),
    employment_stability_index = runif(n_workers, 0.1, 1.0),
    career_advancement_index = runif(n_workers, 0, 0.8),
    career_success_index = runif(n_workers, 0.2, 0.9),
    contract_quality_score = runif(n_workers, 0.3, 1.0),
    employment_intensity_score = runif(n_workers, 0.4, 1.0),
    avg_employment_spell = runif(n_workers, 30, 365),
    job_turnover_rate = runif(n_workers, 0, 5),
    career_complexity_index = runif(n_workers, 0, 0.7),
    days_employed = runif(n_workers, 100, 1000),
    employment_spells = sample(1:10, n_workers, replace = TRUE)
  )

  # Run clustering with automatic k selection
  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = NULL,
    method = "kmeans",
    features = c("stability", "employment", "progression"),
    seed = 123,
    verbose = FALSE
  )

  # Check result structure
  expect_type(result, "list")
  expect_true(all(c("cluster_assignments", "cluster_profiles", "cluster_quality",
                    "feature_importance", "cluster_labels") %in% names(result)))

  # Check cluster_assignments
  expect_s3_class(result$cluster_assignments, "data.table")
  expect_true("cf" %in% names(result$cluster_assignments))
  expect_true("cluster_id" %in% names(result$cluster_assignments))
  expect_true("cluster_label_en" %in% names(result$cluster_assignments))
  expect_true("cluster_label_it" %in% names(result$cluster_assignments))
  expect_equal(nrow(result$cluster_assignments), n_workers)

  # Check number of clusters is between 3 and 6
  n_clusters <- length(unique(result$cluster_assignments$cluster_id))
  expect_true(n_clusters >= 3 && n_clusters <= 6)

  # Check cluster profiles
  expect_s3_class(result$cluster_profiles, "data.table")
  expect_equal(nrow(result$cluster_profiles), n_clusters)
  expect_true("cluster_label_en" %in% names(result$cluster_profiles))
  expect_true("cluster_label_it" %in% names(result$cluster_profiles))
  expect_true("n_workers" %in% names(result$cluster_profiles))
  expect_true("pct_workers" %in% names(result$cluster_profiles))

  # Check that percentages sum to ~100
  expect_equal(sum(result$cluster_profiles$pct_workers), 100, tolerance = 0.1)

  # Check cluster quality metrics
  expect_type(result$cluster_quality, "list")
  expect_true("overall_silhouette" %in% names(result$cluster_quality))
  expect_true("dunn_index" %in% names(result$cluster_quality))
  expect_true("calinski_harabasz" %in% names(result$cluster_quality))

  # Silhouette should be between -1 and 1
  expect_true(result$cluster_quality$overall_silhouette >= -1 &&
              result$cluster_quality$overall_silhouette <= 1)

  # Check feature importance
  expect_s3_class(result$feature_importance, "data.table")
  expect_true("feature" %in% names(result$feature_importance))
  expect_true("f_statistic" %in% names(result$feature_importance))
  expect_true("p_value" %in% names(result$feature_importance))
  expect_true("importance_score" %in% names(result$feature_importance))

  # Importance scores should sum to 1
  expect_equal(sum(result$feature_importance$importance_score), 1, tolerance = 0.01)
})


test_that("cluster_career_trajectories works with fixed number of clusters", {
  set.seed(456)
  n_workers <- 150

  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.2, 1.0),
    employment_stability_index = runif(n_workers, 0.1, 1.0),
    career_advancement_index = runif(n_workers, 0, 0.8),
    contract_quality_score = runif(n_workers, 0.3, 1.0)
  )

  # Test with k=4
  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 4,
    method = "kmeans",
    features = c("stability", "progression"),
    seed = 456
  )

  expect_equal(result$n_clusters, 4)
  expect_equal(length(unique(result$cluster_assignments$cluster_id)), 4)
  expect_equal(nrow(result$cluster_profiles), 4)
})


test_that("cluster_career_trajectories works with different clustering methods", {
  set.seed(789)
  n_workers <- 100

  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.3, 1.0),
    employment_stability_index = runif(n_workers, 0.2, 1.0),
    career_advancement_index = runif(n_workers, 0, 0.7)
  )

  # Test kmeans
  result_kmeans <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    method = "kmeans",
    seed = 789
  )
  expect_equal(result_kmeans$method, "kmeans")
  expect_equal(length(unique(result_kmeans$cluster_assignments$cluster_id)), 3)

  # Test PAM
  result_pam <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    method = "pam",
    seed = 789
  )
  expect_equal(result_pam$method, "pam")
  expect_equal(length(unique(result_pam$cluster_assignments$cluster_id)), 3)

  # Test hierarchical
  result_hier <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    method = "hierarchical",
    seed = 789
  )
  expect_equal(result_hier$method, "hierarchical")
  expect_equal(length(unique(result_hier$cluster_assignments$cluster_id)), 3)
})


test_that("cluster labels are bilingual and meaningful", {
  set.seed(111)
  n_workers <- 150

  # Create data with distinct patterns for label testing
  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = c(runif(50, 0.8, 1.0), runif(50, 0.2, 0.4), runif(50, 0.5, 0.7)),
    employment_stability_index = c(runif(50, 0.7, 1.0), runif(50, 0.1, 0.3), runif(50, 0.4, 0.6)),
    career_advancement_index = c(runif(50, 0.6, 0.9), runif(50, 0.1, 0.3), runif(50, 0.3, 0.5)),
    contract_quality_score = c(runif(50, 0.7, 1.0), runif(50, 0.2, 0.4), runif(50, 0.4, 0.6))
  )

  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    method = "kmeans",
    seed = 111
  )

  # Check that all clusters have both English and Italian labels
  expect_true(all(!is.na(result$cluster_labels$cluster_label_en)))
  expect_true(all(!is.na(result$cluster_labels$cluster_label_it)))
  expect_true(all(nchar(result$cluster_labels$cluster_label_en) > 0))
  expect_true(all(nchar(result$cluster_labels$cluster_label_it) > 0))

  # Check that labels are different across clusters
  expect_true(length(unique(result$cluster_labels$cluster_label_en)) >= 2)
  expect_true(length(unique(result$cluster_labels$cluster_label_it)) >= 2)

  # Known label archetypes should be present in some form
  all_labels_en <- paste(result$cluster_labels$cluster_label_en, collapse = " ")
  possible_keywords <- c("Stable", "Precarious", "Upward", "Entry", "Multi", "Declining", "Moderate")
  expect_true(any(sapply(possible_keywords, function(kw) grepl(kw, all_labels_en))))
})


test_that("feature selection works correctly", {
  set.seed(222)
  n_workers <- 120

  # Create comprehensive metrics data
  sample_metrics <- data.table(
    cf = 1:n_workers,
    # Stability features
    employment_stability_index = runif(n_workers, 0.2, 1.0),
    avg_employment_spell = runif(n_workers, 50, 300),
    job_turnover_rate = runif(n_workers, 0, 3),
    # Employment features
    employment_rate = runif(n_workers, 0.3, 1.0),
    days_employed = runif(n_workers, 200, 800),
    # Progression features
    career_advancement_index = runif(n_workers, 0, 0.7),
    career_success_index = runif(n_workers, 0.3, 0.9),
    # Quality features
    contract_quality_score = runif(n_workers, 0.4, 1.0),
    # Complexity features
    career_complexity_index = runif(n_workers, 0, 0.6)
  )

  # Test with specific feature categories
  result_stability <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    features = "stability",
    seed = 222
  )

  # Check that stability features were used
  stability_features <- c("employment_stability_index", "avg_employment_spell", "job_turnover_rate")
  expect_true(any(stability_features %in% result_stability$clustering_features))

  result_multi <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    features = c("stability", "progression", "quality"),
    seed = 222
  )

  # Should have features from multiple categories
  expect_true(length(result_multi$clustering_features) > 3)
})


test_that("handling of missing values works correctly", {
  set.seed(333)
  n_workers <- 100

  # Create data with some missing values
  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.3, 1.0),
    employment_stability_index = runif(n_workers, 0.2, 1.0),
    career_advancement_index = c(runif(80, 0, 0.8), rep(NA, 20))  # 20% missing
  )

  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    method = "kmeans",
    seed = 333
  )

  # Should have fewer rows after removing missing values
  expect_true(nrow(result$cluster_assignments) < n_workers)
  expect_equal(nrow(result$cluster_assignments), 80)  # 20 rows with NA removed

  # All cluster assignments should be valid
  expect_true(all(!is.na(result$cluster_assignments$cluster_id)))
})


test_that("plot_cluster_profiles creates valid plots", {
  skip_if_not_installed("ggplot2")

  set.seed(444)
  n_workers <- 100

  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.3, 1.0),
    employment_stability_index = runif(n_workers, 0.2, 1.0),
    career_advancement_index = runif(n_workers, 0, 0.7),
    contract_quality_score = runif(n_workers, 0.3, 0.9)
  )

  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    seed = 444
  )

  # Test radar plot
  plot_radar <- plot_cluster_profiles(
    result,
    plot_type = "radar",
    language = "en"
  )
  expect_s3_class(plot_radar, "gg")
  expect_s3_class(plot_radar, "ggplot")

  # Test bar plot
  plot_bar <- plot_cluster_profiles(
    result,
    plot_type = "bar",
    language = "it"
  )
  expect_s3_class(plot_bar, "gg")

  # Test heatmap
  plot_heatmap <- plot_cluster_profiles(
    result,
    plot_type = "heatmap"
  )
  expect_s3_class(plot_heatmap, "gg")

  # Test all plots
  all_plots <- plot_cluster_profiles(
    result,
    plot_type = "all"
  )
  expect_type(all_plots, "list")
  expect_true(all(c("radar", "bar", "heatmap") %in% names(all_plots)))
  expect_s3_class(all_plots$radar, "gg")
  expect_s3_class(all_plots$bar, "gg")
  expect_s3_class(all_plots$heatmap, "gg")
})


test_that("plot_cluster_profiles validates input correctly", {
  # Test with invalid input
  expect_error(
    plot_cluster_profiles(list(a = 1)),
    "cluster_result must be output from cluster_career_trajectories"
  )

  # Test with invalid plot_type
  set.seed(555)
  sample_metrics <- data.table(
    cf = 1:50,
    employment_rate = runif(50),
    employment_stability_index = runif(50),
    career_advancement_index = runif(50)
  )

  result <- cluster_career_trajectories(sample_metrics, n_clusters = 3, seed = 555)

  expect_error(
    plot_cluster_profiles(result, plot_type = "invalid"),
    "plot_type must be one of"
  )

  # Test with invalid language
  expect_error(
    plot_cluster_profiles(result, language = "fr"),
    "language must be one of"
  )
})


test_that("cluster quality metrics are reasonable", {
  set.seed(666)
  n_workers <- 200

  # Create data with clear clusters
  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = c(runif(100, 0.8, 1.0), runif(100, 0.1, 0.3)),
    employment_stability_index = c(runif(100, 0.7, 1.0), runif(100, 0.0, 0.3)),
    career_advancement_index = c(runif(100, 0.6, 0.9), runif(100, 0.0, 0.2))
  )

  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,  # Should find clear clusters
    method = "kmeans",
    seed = 666
  )

  # With clear separation, silhouette should be positive
  expect_true(result$cluster_quality$overall_silhouette > 0)

  # Dunn index should be positive
  expect_true(result$cluster_quality$dunn_index > 0)

  # Calinski-Harabasz should be positive and reasonably high for well-separated clusters
  expect_true(result$cluster_quality$calinski_harabasz > 5)
})


test_that("standardization option works correctly", {
  set.seed(777)
  n_workers <- 100

  # Create data with very different scales
  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0, 1),  # 0-1 scale
    days_employed = runif(n_workers, 100, 1000),  # 100-1000 scale
    job_turnover_rate = runif(n_workers, 0, 5)  # 0-5 scale
  )

  # With standardization
  result_std <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    standardize = TRUE,
    seed = 777
  )

  # Without standardization
  result_nostd <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    standardize = FALSE,
    seed = 777
  )

  # Both should produce valid results
  expect_equal(length(unique(result_std$cluster_assignments$cluster_id)), 3)
  expect_equal(length(unique(result_nostd$cluster_assignments$cluster_id)), 3)

  # Results might differ due to scale differences
  # This is expected behavior, not an error
})


test_that("min_cluster_size parameter works correctly", {
  set.seed(888)
  n_workers <- 100

  sample_metrics <- data.table(
    cf = 1:n_workers,
    employment_rate = runif(n_workers, 0.3, 1.0),
    employment_stability_index = runif(n_workers, 0.2, 1.0),
    career_advancement_index = runif(n_workers, 0, 0.7)
  )

  # Test with large min_cluster_size
  result <- cluster_career_trajectories(
    sample_metrics,
    n_clusters = 3,
    min_cluster_size = 20,  # Each cluster must have at least 20 members
    seed = 888
  )

  # Check that all clusters meet minimum size
  cluster_sizes <- result$cluster_assignments[, .N, by = cluster_id]
  expect_true(all(cluster_sizes$N >= 20))
})
