#!/usr/bin/env Rscript

#' QUICK FIX for your consolidation issue
#'
#' This script loads the safe function and runs it on your data

# Load the function directly
source("R/consolidate_employment_safe.R")

# Load required packages
library(data.table)

# Load your data (CHANGE THIS PATH)
message("Loading data...")
dt <- readRDS("your_data_file.rds")  # <<< CHANGE THIS

message(sprintf("Dataset: %d records, %d people", nrow(dt), uniqueN(dt$cf)))

# Run the safe consolidation
message("Starting safe consolidation...")
result <- consolidate_by_employer_safe(
  dt,
  employer_var = "datore",
  min_lag = 8,
  remove_intermediate_unemployment = TRUE,
  calculate_coverage = TRUE,
  show_progress = TRUE,
  chunk_size = 2000  # Small chunks for safety
)

# Save result
message("Saving result...")
saveRDS(result, "consolidated_safe.rds")

message("DONE! Check consolidated_safe.rds for your result.")