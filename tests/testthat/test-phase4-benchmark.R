# Phase 4 Optimization Real-World Benchmark

library(testthat)
library(data.table)
library(microbenchmark)

test_that("Phase 4: Real-world performance on sample.rds", {
  skip_on_cran()

  # Load real data
  data_file <- "../../data/sample.rds"
  if (!file.exists(data_file)) {
    skip("sample.rds not found")
  }

  data <- readRDS(data_file)

  cat("\n\n=== Phase 4 Real-World Benchmark ===\n")
  cat("Dataset: sample.rds\n")
  cat("Total records:", nrow(data), "\n")

  # Get single-period statistics
  stats <- get_single_period_stats(data)
  cat("Total workers:", stats$total_workers, "\n")
  cat("Single-period workers:", stats$n_single, "(", stats$pct_single, "%)\n")
  cat("Multi-period workers:", stats$n_multi, "(", stats$pct_multi, "%)\n\n")

  # Benchmark full consolidation chain
  cat("Running benchmark (10 iterations)...\n")

  bench_result <- microbenchmark(
    full_chain = {
      data |>
        consolidate_overlapping() |>
        consolidate_adjacent() |>
        consolidate_short_gaps(30)
    },
    times = 10
  )

  median_ms <- summary(bench_result)$median
  median_s <- median_ms / 1000

  cat("\n--- Results ---\n")
  cat("Median time:", round(median_s, 3), "seconds\n")

  # Calculate throughput
  throughput <- nrow(data) / median_s
  cat("Throughput:", format(round(throughput), big.mark = ","), "rec/sec\n\n")

  # Compare with Phase 3 baseline
  phase3_baseline <- 41000
  speedup <- throughput / phase3_baseline

  cat("--- Phase 4 vs Phase 3 ---\n")
  cat("Phase 3 baseline:", format(phase3_baseline, big.mark = ","), "rec/sec\n")
  cat("Phase 4 actual:  ", format(round(throughput), big.mark = ","), "rec/sec\n")
  cat("Speedup:         ", sprintf("%.2fx", speedup), "\n\n")

  # Expected speedup based on single-period percentage
  expected_speedup <- calculate_expected_speedup(stats$pct_single)
  cat("Expected speedup (", stats$pct_single, "% singles): ",
      sprintf("%.2fx\n", expected_speedup), sep = "")

  # Status
  if (speedup >= 1.3) {
    cat("✅ EXCELLENT: ≥1.3x speedup achieved\n")
  } else if (speedup >= 1.1) {
    cat("✅ GOOD: 1.1-1.3x speedup\n")
  } else if (speedup >= 1.0) {
    cat("⚠️ MODEST: 1.0-1.1x speedup\n")
  } else {
    cat("❌ REGRESSION: slower than Phase 3\n")
  }

  cat("=====================================\n\n")

  # Test assertions
  expect_gte(throughput, 30000)  # Should be at least 30K rec/sec
  expect_gte(speedup, 1.0)  # Should be at least as fast as Phase 3
})


test_that("Phase 4: Performance scales with single-period percentage", {
  skip_on_cran()

  cat("\n\n=== Phase 4 Scaling Test ===\n")

  # Test different percentages of single-period workers
  percentages <- c(20, 40, 60)

  results <- lapply(percentages, function(pct) {
    n_single <- 1000 * pct / 100
    n_multi <- 1000 - n_single

    test_data <- generate_mixed_employment_data(
      n_single = n_single,
      n_multi = n_multi,
      periods_per_multi = 3,
      seed = 9999 + pct
    )

    start_time <- Sys.time()
    result <- test_data |>
      consolidate_overlapping() |>
      consolidate_adjacent() |>
      consolidate_short_gaps(30)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    throughput <- nrow(test_data) / elapsed
    expected_speedup <- calculate_expected_speedup(pct)

    cat(sprintf("%d%% singles: %s rec/sec (expected speedup: %.2fx)\n",
                pct,
                format(round(throughput), big.mark = ","),
                expected_speedup))

    list(
      pct = pct,
      throughput = throughput,
      expected_speedup = expected_speedup
    )
  })

  cat("===========================\n\n")

  # Verify throughput increases with single-period percentage
  throughputs <- sapply(results, function(r) r$throughput)

  # Should see general upward trend (allowing some variance)
  expect_true(throughputs[3] > throughputs[1] * 0.9)
})


cat("\n=== Phase 4 Benchmark Tests Complete ===\n\n")
