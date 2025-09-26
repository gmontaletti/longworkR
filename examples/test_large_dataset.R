#!/usr/bin/env Rscript

#' Test script for large dataset consolidation
#'
#' This script tests the different consolidation functions on your large dataset

library(longworkR)
library(data.table)

# Load your data
message("Loading data...")
dt <- readRDS("your_data.rds")  # Replace with your actual data file

message(sprintf("Dataset: %d records, %d unique people",
                nrow(dt), uniqueN(dt$cf)))

# Test 1: Ultra-fast version (recommended for >10M records)
message("\n=== Testing ULTRA-FAST version ===")
time_ultra <- system.time({
  result_ultra <- consolidate_by_employer_ultra_fast(
    dt,
    employer_var = "datore",
    min_lag = 8,
    show_progress = TRUE,
    chunk_size = 10000  # Adjust based on memory
  )
})

message(sprintf("Ultra-fast time: %.2f seconds", time_ultra["elapsed"]))
message(sprintf("Result: %d records", nrow(result_ultra)))

# Save result
saveRDS(result_ultra, "consolidated_ultra.rds")

# Test 2: Fast version with sequential (fallback to avoid parallel issues)
message("\n=== Testing FAST version (sequential) ===")
time_fast <- system.time({
  result_fast <- consolidate_by_employer_fast(
    dt,
    employer_var = "datore",
    min_lag = 8,
    parallel = FALSE,  # Force sequential to avoid parallel issues
    show_progress = TRUE
  )
})

message(sprintf("Fast sequential time: %.2f seconds", time_fast["elapsed"]))
message(sprintf("Result: %d records", nrow(result_fast)))

# Save result
saveRDS(result_fast, "consolidated_fast.rds")

# Compare results
message("\n=== Comparison ===")
message(sprintf("Ultra-fast speedup: %.2fx", time_fast["elapsed"] / time_ultra["elapsed"]))

# Check consistency
if (nrow(result_ultra) == nrow(result_fast)) {
  message("✓ Same number of output records")
} else {
  message(sprintf("⚠ Different output sizes: ultra=%d, fast=%d",
                 nrow(result_ultra), nrow(result_fast)))
}

# Memory usage
message(sprintf("\nMemory used: %.0f MB", sum(gc()[, 2])))