#!/usr/bin/env Rscript

#' Demonstration of consolidate_by_employer_fast() Performance
#'
#' This script shows the usage and performance benefits of the new
#' optimized consolidation function.

library(longworkR)
library(data.table)

cat("=====================================\n")
cat("consolidate_by_employer_fast() DEMO\n")
cat("=====================================\n\n")

# Load sample data
if (file.exists("data/sample.rds")) {
  cat("Loading sample data...\n")
  data <- readRDS("data/sample.rds")
} else {
  cat("Generating synthetic data...\n")
  # Generate synthetic test data
  set.seed(123)
  n_people <- 500
  records_per_person <- 15

  data <- data.table(
    cf = rep(paste0("P", seq_len(n_people)), each = records_per_person),
    inizio = rep(seq(as.Date("2019-01-01"), by = "month", length.out = records_per_person), n_people),
    fine = rep(seq(as.Date("2019-01-31"), by = "month", length.out = records_per_person), n_people),
    durata = rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28, 31), n_people),
    datore = rep(c(rep("EMP1", 4), rep("EMP2", 4), rep("EMP1", 3), rep("EMP3", 4)), n_people),
    arco = rep(c(rep(1L, 13), 0L, 1L), n_people),
    stato = rep(c(rep("occupato", 13), "disoccupato", "occupato"), n_people),
    retribuzione = round(runif(n_people * records_per_person, 1500, 4000), 2),
    qualifica = sample(c("operaio", "impiegato", "quadro"),
                      n_people * records_per_person, replace = TRUE,
                      prob = c(0.5, 0.35, 0.15))
  )

  # Add some variability
  data[sample(.N, size = .N * 0.1), datore := paste0(datore, "_VAR")]
}

cat(sprintf("\nDataset info:\n"))
cat(sprintf("  Total records: %d\n", nrow(data)))
cat(sprintf("  Unique people: %d\n", uniqueN(data$cf)))
cat(sprintf("  Unique employers: %d\n", uniqueN(data$datore[!is.na(data$datore)])))
cat(sprintf("  Records with unemployment: %d (%.1f%%)\n",
            sum(data$arco == 0), 100 * mean(data$arco == 0)))

# Identify consolidation candidates
candidates <- data[, .(
  n_records = .N,
  n_employers = uniqueN(datore[!is.na(datore)]),
  has_unemployment = any(arco == 0)
), by = cf]

n_need_consolidation <- candidates[n_records > 1 & (n_employers < n_records | has_unemployment), .N]
cat(sprintf("  People needing consolidation: %d (%.1f%%)\n",
            n_need_consolidation, 100 * n_need_consolidation / uniqueN(data$cf)))

cat("\n=====================================\n")
cat("PERFORMANCE COMPARISON\n")
cat("=====================================\n\n")

# 1. Original function
cat("Running original consolidate_by_employer()...\n")
time_original <- system.time({
  result_original <- consolidate_by_employer(
    data, "datore", min_lag = 8,
    show_progress = FALSE
  )
})

cat(sprintf("  Time: %.3f seconds\n", time_original["elapsed"]))
cat(sprintf("  Result: %d records (%.1f%% reduction)\n",
            nrow(result_original),
            100 * (1 - nrow(result_original)/nrow(data))))

# 2. Fast sequential
cat("\nRunning consolidate_by_employer_fast() [SEQUENTIAL]...\n")
time_fast_seq <- system.time({
  result_fast_seq <- consolidate_by_employer_fast(
    data, "datore", min_lag = 8,
    parallel = FALSE, show_progress = FALSE
  )
})

cat(sprintf("  Time: %.3f seconds (%.1fx faster)\n",
            time_fast_seq["elapsed"],
            time_original["elapsed"] / time_fast_seq["elapsed"]))
cat(sprintf("  Result: %d records\n", nrow(result_fast_seq)))

# 3. Fast parallel
if (parallel::detectCores() >= 2) {
  cat(sprintf("\nRunning consolidate_by_employer_fast() [PARALLEL, %d cores]...\n",
              parallel::detectCores() - 1))
  time_fast_par <- system.time({
    result_fast_par <- consolidate_by_employer_fast(
      data, "datore", min_lag = 8,
      parallel = TRUE, show_progress = FALSE
    )
  })

  cat(sprintf("  Time: %.3f seconds (%.1fx faster)\n",
              time_fast_par["elapsed"],
              time_original["elapsed"] / time_fast_par["elapsed"]))
  cat(sprintf("  Result: %d records\n", nrow(result_fast_par)))
}

cat("\n=====================================\n")
cat("CONSOLIDATION METRICS\n")
cat("=====================================\n\n")

# Analyze consolidation effectiveness
if ("consolidation_coverage" %in% names(result_fast_seq)) {
  consolidated_only <- result_fast_seq[n_periods_consolidated > 1]

  if (nrow(consolidated_only) > 0) {
    cat("For consolidated periods:\n")
    cat(sprintf("  Average coverage: %.1f%%\n",
                100 * mean(consolidated_only$consolidation_coverage, na.rm = TRUE)))
    cat(sprintf("  Total unemployment days removed: %.0f\n",
                sum(consolidated_only$unemployment_days_removed, na.rm = TRUE)))
    cat(sprintf("  Average periods consolidated: %.1f\n",
                mean(consolidated_only$n_periods_consolidated, na.rm = TRUE)))

    # Distribution of consolidation
    coverage_dist <- consolidated_only[, .(
      n = .N,
      avg_periods = mean(n_periods_consolidated)
    ), by = .(coverage_bin = cut(consolidation_coverage,
                                 breaks = c(0, 0.5, 0.8, 0.9, 0.95, 1.0),
                                 include.lowest = TRUE))]

    cat("\nCoverage distribution:\n")
    print(coverage_dist)
  }
}

cat("\n=====================================\n")
cat("KEY OPTIMIZATIONS\n")
cat("=====================================\n\n")

cat("The fast version includes:\n")
cat("1. CANDIDATE PRESELECTION: Only processes people needing consolidation\n")
cat("2. PARALLEL PROCESSING: Splits data by person for parallel execution\n")
cat("3. OPTIMIZED DATA.TABLE: Vectorized operations, minimal copying\n")
cat("4. SIMPLIFIED GROUPING: Integer group IDs, pre-computed shifts\n")
cat("5. BATCH AGGREGATION: Type-based column processing\n")

cat("\nExpected speedup by data size:\n")
cat("  <10K records:   2-3x faster\n")
cat("  10-50K records: 5-10x faster\n")
cat("  >50K records:   10-20x faster (with parallel)\n")

cat("\n=====================================\n")
cat("USAGE EXAMPLES\n")
cat("=====================================\n\n")

cat("# Basic usage (auto-detects parallel)\n")
cat("result <- consolidate_by_employer_fast(data, \"datore\")\n\n")

cat("# Force sequential processing\n")
cat("result <- consolidate_by_employer_fast(data, \"datore\", parallel = FALSE)\n\n")

cat("# Custom parallel configuration\n")
cat("result <- consolidate_by_employer_fast(data, \"datore\", parallel = TRUE, n_cores = 4)\n\n")

cat("# High-performance mode (skip coverage metrics)\n")
cat("result <- consolidate_by_employer_fast(data, \"datore\", calculate_coverage = FALSE)\n\n")

cat("# Backward compatible mode\n")
cat("result <- consolidate_by_employer_fast(\n")
cat("  data, \"datore\",\n")
cat("  remove_intermediate_unemployment = FALSE,\n")
cat("  calculate_coverage = FALSE\n")
cat(")\n")

cat("\n=====================================\n")
cat("DEMO COMPLETE\n")
cat("=====================================\n")