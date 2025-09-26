#!/usr/bin/env Rscript

#' Quick script to run consolidation on your large dataset
#'
#' Usage: Rscript run_consolidation.R

library(longworkR)
library(data.table)

# Load your data (adjust path as needed)
message("Loading data...")
dt <- readRDS("your_data.rds")  # CHANGE THIS TO YOUR DATA FILE

message(sprintf("Dataset: %d records, %d unique people",
                nrow(dt), uniqueN(dt$cf)))

# Try different methods in order of robustness

# METHOD 1: Ultra-fast (best for very large datasets)
message("\n=== METHOD 1: ULTRA-FAST ===")
tryCatch({
  time1 <- system.time({
    result <- consolidate_by_employer_ultra_fast(
      dt,
      employer_var = "datore",
      min_lag = 8,
      show_progress = TRUE,
      chunk_size = 5000  # Smaller chunks for safety
    )
  })

  message(sprintf("Success! Time: %.1f seconds", time1["elapsed"]))
  message(sprintf("Result: %d records", nrow(result)))
  saveRDS(result, "consolidated_ultra.rds")
  message("Saved to: consolidated_ultra.rds")

}, error = function(e) {
  message(sprintf("Ultra-fast failed: %s", e$message))
  message("Trying next method...")

  # METHOD 2: Robust (handles type issues)
  message("\n=== METHOD 2: ROBUST ===")
  tryCatch({
    time2 <- system.time({
      result <- consolidate_by_employer_robust(
        dt,
        employer_var = "datore",
        min_lag = 8,
        show_progress = TRUE
      )
    })

    message(sprintf("Success! Time: %.1f seconds", time2["elapsed"]))
    message(sprintf("Result: %d records", nrow(result)))
    saveRDS(result, "consolidated_robust.rds")
    message("Saved to: consolidated_robust.rds")

  }, error = function(e) {
    message(sprintf("Robust also failed: %s", e$message))
    message("Trying fallback method...")

    # METHOD 3: Simple fallback - just remove duplicates
    message("\n=== METHOD 3: SIMPLE FALLBACK ===")

    # Very simple consolidation - just collapse consecutive same employer
    dt_simple <- copy(dt)
    setkey(dt_simple, cf, inizio, fine)

    dt_simple[, `:=`(
      prev_emp = shift(datore, 1L),
      prev_fine = shift(fine, 1L)
    ), by = cf]

    dt_simple[, new_group := is.na(prev_emp) |
                             datore != prev_emp |
                             as.integer(inizio - prev_fine) > 9]

    dt_simple[, group_id := cumsum(new_group), by = cf]

    result <- dt_simple[, .(
      inizio = min(inizio),
      fine = max(fine),
      durata = as.numeric(max(fine) - min(inizio) + 1),
      datore = datore[1L],
      arco = max(arco, na.rm = TRUE),
      n_consolidated = .N
    ), by = .(cf, group_id)]

    result[, group_id := NULL]

    message(sprintf("Simple consolidation done: %d -> %d records",
                   nrow(dt), nrow(result)))
    saveRDS(result, "consolidated_simple.rds")
    message("Saved to: consolidated_simple.rds")
  })
})

message("\n=== COMPLETE ===")
message("Check your working directory for output files")