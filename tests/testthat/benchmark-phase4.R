# Phase 4 Optimization Benchmark
# Test performance on real sample.rds data

library(longworkR)
library(data.table)
library(microbenchmark)

# Source helper functions
source("tests/testthat/helper-phase4-optimization.R")

cat("\n=== Phase 4 Optimization Benchmark ===\n\n")

# Load real data
if (file.exists("data/sample.rds")) {
  data <- readRDS("data/sample.rds")

  cat("Dataset: data/sample.rds\n")
  cat("Total records:", nrow(data), "\n")

  # Get single-period statistics
  stats <- get_single_period_stats(data)
  cat("Total workers:", stats$total_workers, "\n")
  cat("Single-period workers:", stats$n_single, "(", stats$pct_single, "%)\n")
  cat("Multi-period workers:", stats$n_multi, "(", stats$pct_multi, "%)\n\n")

  # Benchmark each function
  cat("--- Benchmarking consolidate_overlapping() ---\n")
  bench_overlap <- microbenchmark(
    consolidate_overlapping(data),
    times = 10
  )
  print(summary(bench_overlap)[, c("expr", "min", "median", "max")])
  throughput_overlap <- nrow(data) / (summary(bench_overlap)$median / 1000)
  cat("Throughput:", format(round(throughput_overlap), big.mark = ","), "rec/sec\n\n")

  cat("--- Benchmarking consolidate_adjacent() ---\n")
  step1 <- consolidate_overlapping(data)
  bench_adjacent <- microbenchmark(
    consolidate_adjacent(step1),
    times = 10
  )
  print(summary(bench_adjacent)[, c("expr", "min", "median", "max")])
  throughput_adjacent <- nrow(step1) / (summary(bench_adjacent)$median / 1000)
  cat("Throughput:", format(round(throughput_adjacent), big.mark = ","), "rec/sec\n\n")

  cat("--- Benchmarking consolidate_short_gaps(30) ---\n")
  step2 <- consolidate_adjacent(step1)
  bench_gaps <- microbenchmark(
    consolidate_short_gaps(step2, 30),
    times = 10
  )
  print(summary(bench_gaps)[, c("expr", "min", "median", "max")])
  throughput_gaps <- nrow(step2) / (summary(bench_gaps)$median / 1000)
  cat("Throughput:", format(round(throughput_gaps), big.mark = ","), "rec/sec\n\n")

  cat("--- Benchmarking Full Chain ---\n")
  bench_chain <- microbenchmark(
    {
      data |>
        consolidate_overlapping() |>
        consolidate_adjacent() |>
        consolidate_short_gaps(30)
    },
    times = 10
  )
  print(summary(bench_chain)[, c("expr", "min", "median", "max")])
  throughput_chain <- nrow(data) / (summary(bench_chain)$median / 1000)
  cat("Full chain throughput:", format(round(throughput_chain), big.mark = ","), "rec/sec\n\n")

  # Compare with Phase 3 baseline (41,000 rec/sec)
  phase3_baseline <- 41000
  speedup <- throughput_chain / phase3_baseline

  cat("=== Phase 4 vs Phase 3 Comparison ===\n")
  cat("Phase 3 baseline:", format(phase3_baseline, big.mark = ","), "rec/sec\n")
  cat("Phase 4 actual:", format(round(throughput_chain), big.mark = ","), "rec/sec\n")
  cat("Speedup:", sprintf("%.2fx", speedup), "\n")

  if (speedup >= 1.5) {
    cat("Status: ✅ EXCELLENT (≥1.5x speedup achieved)\n")
  } else if (speedup >= 1.2) {
    cat("Status: ✅ GOOD (1.2-1.5x speedup achieved)\n")
  } else if (speedup >= 1.0) {
    cat("Status: ⚠️ MODEST (1.0-1.2x speedup achieved)\n")
  } else {
    cat("Status: ❌ REGRESSION (slower than Phase 3)\n")
  }

  cat("\n")

  # Expected speedup based on single-period percentage
  expected_speedup <- calculate_expected_speedup(stats$pct_single)
  cat("Expected speedup (", stats$pct_single, "% singles): ",
      sprintf("%.2fx", expected_speedup), "\n", sep = "")

  if (speedup >= expected_speedup * 0.8) {
    cat("Performance: ✅ Within expected range\n")
  } else {
    cat("Performance: ⚠️ Below expected (investigate)\n")
  }

} else {
  cat("⚠️ sample.rds not found. Skipping benchmark.\n")
  cat("Run this script from the package root directory.\n")
}

cat("\n=== Benchmark Complete ===\n\n")
