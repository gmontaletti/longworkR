#!/usr/bin/env Rscript
#' Data Management Script for longworkR Package
#' 
#' This script provides utilities for managing both real and synthetic datasets
#' in the longworkR package, ensuring data privacy while maintaining functionality.
#' 
#' @author Giampaolo Montaletti <giampaolo.montaletti@gmail.com>

# Load required libraries
library(data.table)

# Source the synthetic data generator
if (file.exists("R/synthetic_data_generator.R")) {
  source("R/synthetic_data_generator.R")
} else if (file.exists("../../R/synthetic_data_generator.R")) {
  source("../../R/synthetic_data_generator.R")
}

#' Generate and Update Synthetic Dataset
#' 
#' Creates a new synthetic dataset and saves it for public distribution
generate_public_dataset <- function(output_path = "data/synthetic_sample.rds") {
  cat("=== GENERATING PUBLIC SYNTHETIC DATASET ===\n")
  
  # Generate synthetic data
  cat("Creating synthetic employment data...\n")
  synthetic_data <- generate_synthetic_employment_data(
    n_individuals = 4252,
    n_contracts = 476400,
    seed = 12345  # Fixed seed for reproducibility
  )
  
  # Validate the data
  cat("Validating data quality...\n")
  validation <- validate_synthetic_data(synthetic_data)
  
  cat("Validation Results:\n")
  cat("  - Rows:", validation$dimensions$rows, "\n")
  cat("  - Columns:", validation$dimensions$cols, "\n")
  cat("  - Individuals:", validation$dimensions$individuals, "\n")
  cat("  - Quality Score:", round(validation$overall_quality_score, 3), "\n")
  
  if (validation$overall_quality_score < 0.9) {
    warning("Data quality score is below threshold (0.9). Review validation results.")
  }
  
  # Save the dataset
  cat("Saving synthetic dataset to:", output_path, "\n")
  saveRDS(synthetic_data, output_path)
  
  # Report file size
  file_size <- file.info(output_path)$size
  cat("File size:", round(file_size / 1024 / 1024, 2), "MB\n")
  
  cat("=== PUBLIC DATASET GENERATED SUCCESSFULLY ===\n")
  return(synthetic_data)
}

#' Compare Real vs Synthetic Data
#' 
#' Performs comparative analysis between real and synthetic datasets
#' to ensure synthetic data maintains realistic patterns
compare_datasets <- function(real_path = "data/sample.rds", 
                           synthetic_path = "data/synthetic_sample.rds") {
  
  cat("=== DATASET COMPARISON ANALYSIS ===\n")
  
  # Load datasets
  if (file.exists(real_path)) {
    real_data <- readRDS(real_path)
    cat("Real data loaded:", nrow(real_data), "rows\n")
  } else {
    cat("Real data file not found at:", real_path, "\n")
    return(NULL)
  }
  
  if (file.exists(synthetic_path)) {
    synthetic_data <- readRDS(synthetic_path)
    cat("Synthetic data loaded:", nrow(synthetic_data), "rows\n")
  } else {
    cat("Synthetic data file not found at:", synthetic_path, "\n")
    return(NULL)
  }
  
  # Structure comparison
  cat("\n--- STRUCTURE COMPARISON ---\n")
  cat("Columns match:", identical(names(real_data), names(synthetic_data)), "\n")
  cat("Real dimensions:", paste(dim(real_data), collapse = " x "), "\n")
  cat("Synthetic dimensions:", paste(dim(synthetic_data), collapse = " x "), "\n")
  
  # Key statistics comparison
  cat("\n--- KEY STATISTICS COMPARISON ---\n")
  
  # Duration comparison
  cat("Contract Duration:\n")
  cat("  Real - Mean:", round(mean(real_data$durata, na.rm = TRUE), 1), 
      "| Median:", round(median(real_data$durata, na.rm = TRUE), 1), "\n")
  cat("  Synthetic - Mean:", round(mean(synthetic_data$durata, na.rm = TRUE), 1), 
      "| Median:", round(median(synthetic_data$durata, na.rm = TRUE), 1), "\n")
  
  # Age comparison
  if (sum(!is.na(real_data$eta)) > 0 && sum(!is.na(synthetic_data$eta)) > 0) {
    cat("Age Distribution:\n")
    cat("  Real - Mean:", round(mean(real_data$eta, na.rm = TRUE), 1), 
        "| Median:", round(median(real_data$eta, na.rm = TRUE), 1), "\n")
    cat("  Synthetic - Mean:", round(mean(synthetic_data$eta, na.rm = TRUE), 1), 
        "| Median:", round(median(synthetic_data$eta, na.rm = TRUE), 1), "\n")
  }
  
  # Missing data patterns
  cat("Missing Data Rates:\n")
  real_missing <- sapply(real_data, function(x) mean(is.na(x)))
  synthetic_missing <- sapply(synthetic_data, function(x) mean(is.na(x)))
  
  for (col in c("qualifica", "ateco", "ore", "retribuzione", "eta")) {
    if (col %in% names(real_data) && col %in% names(synthetic_data)) {
      cat("  ", col, "- Real:", round(real_missing[col], 3), 
          "| Synthetic:", round(synthetic_missing[col], 3), "\n")
    }
  }
  
  cat("=== COMPARISON COMPLETED ===\n")
}

#' Verify Data Privacy Protection
#' 
#' Checks that private data files are properly protected from version control
verify_privacy_protection <- function() {
  cat("=== PRIVACY PROTECTION VERIFICATION ===\n")
  
  # Check .gitignore
  gitignore_path <- ".gitignore"
  if (file.exists(gitignore_path)) {
    gitignore_content <- readLines(gitignore_path)
    
    privacy_patterns <- c("data/sample.rds", "data/*_private.rds", "data/*_real.rds")
    protected <- sapply(privacy_patterns, function(pattern) {
      any(grepl(gsub("\\*", ".*", pattern), gitignore_content, fixed = FALSE))
    })
    
    cat("Privacy protection in .gitignore:\n")
    for (i in seq_along(privacy_patterns)) {
      cat("  ", privacy_patterns[i], ":", ifelse(protected[i], "✓ PROTECTED", "✗ NOT PROTECTED"), "\n")
    }
    
    if (!all(protected)) {
      warning("Some private data patterns are not protected in .gitignore!")
    }
  } else {
    warning(".gitignore file not found!")
  }
  
  # Check for presence of files
  cat("\nFile presence check:\n")
  real_exists <- file.exists("data/sample.rds")
  synthetic_exists <- file.exists("data/synthetic_sample.rds")
  
  cat("  Real data (sample.rds):", ifelse(real_exists, "Present", "Not found"), "\n")
  cat("  Synthetic data (synthetic_sample.rds):", ifelse(synthetic_exists, "Present", "Not found"), "\n")
  
  if (!synthetic_exists) {
    warning("Synthetic dataset not found! Run generate_public_dataset() first.")
  }
  
  cat("=== PRIVACY VERIFICATION COMPLETED ===\n")
}

#' Run Complete Data Management Workflow
#' 
#' Executes the complete data management and validation workflow
run_complete_workflow <- function() {
  cat("=== LONGWORKR DATA MANAGEMENT WORKFLOW ===\n\n")
  
  # Step 1: Verify privacy protection
  verify_privacy_protection()
  cat("\n")
  
  # Step 2: Generate synthetic dataset
  synthetic_data <- generate_public_dataset()
  cat("\n")
  
  # Step 3: Compare datasets (if real data exists)
  if (file.exists("data/sample.rds")) {
    compare_datasets()
  } else {
    cat("Real data not available for comparison.\n")
  }
  
  cat("\n=== WORKFLOW COMPLETED SUCCESSFULLY ===\n")
  return(invisible(NULL))
}

# Command line execution
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0 || args[1] == "workflow") {
    run_complete_workflow()
  } else if (args[1] == "generate") {
    generate_public_dataset()
  } else if (args[1] == "compare") {
    compare_datasets()
  } else if (args[1] == "verify") {
    verify_privacy_protection()
  } else {
    cat("Usage: Rscript data_management.R [workflow|generate|compare|verify]\n")
    cat("  workflow - Run complete data management workflow (default)\n")
    cat("  generate - Generate synthetic dataset only\n")
    cat("  compare  - Compare real vs synthetic datasets\n")
    cat("  verify   - Verify privacy protection settings\n")
  }
}