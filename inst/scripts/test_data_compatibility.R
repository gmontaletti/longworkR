#!/usr/bin/env Rscript
#' Test Data Compatibility Script
#' 
#' This script tests that all major longworkR functions work correctly
#' with both real and synthetic datasets, ensuring data privacy protection
#' doesn't compromise package functionality.
#' 
#' @author Giampaolo Montaletti <giampaolo.montaletti@gmail.com>

# Load required libraries
library(data.table)

#' Test Dataset Compatibility
#' 
#' Tests core longworkR functions with a given dataset
test_dataset_compatibility <- function(data, dataset_name = "test") {
  cat("=== TESTING", toupper(dataset_name), "DATASET COMPATIBILITY ===\n")
  
  results <- list(
    dataset_name = dataset_name,
    total_tests = 0,
    passed_tests = 0,
    failed_tests = 0,
    test_details = list()
  )
  
  # Helper function to run a test
  run_test <- function(test_name, test_function) {
    results$total_tests <<- results$total_tests + 1
    
    cat("Testing", test_name, "... ")
    
    tryCatch({
      test_function()
      cat("✓ PASSED\n")
      results$passed_tests <<- results$passed_tests + 1
      results$test_details[[test_name]] <<- "PASSED"
    }, error = function(e) {
      cat("✗ FAILED:", conditionMessage(e), "\n")
      results$failed_tests <<- results$failed_tests + 1
      results$test_details[[test_name]] <<- paste("FAILED:", conditionMessage(e))
    })
  }
  
  # Test 1: Basic data structure
  run_test("Basic data structure", function() {
    stopifnot(is.data.table(data))
    stopifnot(nrow(data) > 1000)
    stopifnot(ncol(data) == 30)
    required_cols <- c("id", "cf", "inizio", "fine", "durata", "stato")
    stopifnot(all(required_cols %in% names(data)))
  })
  
  # Test 2: Date handling
  run_test("Date column handling", function() {
    stopifnot(inherits(data$inizio, "IDate"))
    stopifnot(inherits(data$fine, "IDate"))
    stopifnot(all(data$inizio <= data$fine, na.rm = TRUE))
    stopifnot(all(data$durata >= 1, na.rm = TRUE))
  })
  
  # Test 3: Employment states
  run_test("Employment states", function() {
    valid_states <- c("occ_pt", "occ_ft", "disoccupato", "over_pt_pt", 
                     "over_ft_ft", "over_pt_ft", "over_ft_pt")
    unique_states <- unique(data$stato)
    invalid_states <- setdiff(unique_states, valid_states)
    if (length(invalid_states) > 0) {
      stop("Invalid employment states found: ", paste(invalid_states, collapse = ", "))
    }
  })
  
  # Test 4: Individual consistency
  run_test("Individual consistency", function() {
    # Check that each cf has consistent demographic info
    demo_consistency <- data[!is.na(eta) & !is.na(sesso) & !is.na(istruzione), 
                            .(eta_var = var(eta, na.rm = TRUE),
                              sesso_unique = length(unique(sesso)),
                              istr_unique = length(unique(istruzione))), 
                            by = cf]
    
    # Age should not vary too much (allowing for aging over time)
    high_age_var <- demo_consistency[eta_var > 25, cf]
    if (length(high_age_var) > nrow(demo_consistency) * 0.1) {
      stop("Too many individuals with inconsistent age data")
    }
    
    # Gender and education should be consistent within individuals
    inconsistent_demo <- demo_consistency[sesso_unique > 1 | istr_unique > 1, cf]
    if (length(inconsistent_demo) > 0) {
      warning("Found individuals with inconsistent demographic data: ", 
              length(inconsistent_demo), " cases")
    }
  })
  
  # Test 5: Contract patterns
  run_test("Contract patterns", function() {
    # Check unemployment records
    unemployed <- data[stato == "disoccupato"]
    if (nrow(unemployed) > 0) {
      stopifnot(all(unemployed$id == 0))
      stopifnot(all(unemployed$over_id == 0))
    }
    
    # Check employment records
    employed <- data[stato != "disoccupato"]
    if (nrow(employed) > 0) {
      valid_employed <- employed[!is.na(id) & id > 0]
      if (nrow(valid_employed) == 0) {
        stop("No valid employment records found")
      }
    }
  })
  
  # Test 6: Salary patterns
  run_test("Salary patterns", function() {
    employed_with_salary <- data[stato != "disoccupato" & !is.na(retribuzione)]
    if (nrow(employed_with_salary) > 0) {
      # Check for reasonable salary ranges
      unreasonable_salaries <- employed_with_salary[retribuzione < 0 | retribuzione > 1000000]
      if (nrow(unreasonable_salaries) > nrow(employed_with_salary) * 0.01) {
        warning("Found unreasonable salary values: ", nrow(unreasonable_salaries), " cases")
      }
    }
  })
  
  # Test 7: Geographic data
  run_test("Geographic consistency", function() {
    geo_data <- data[!is.na(provincia) & !is.na(area)]
    if (nrow(geo_data) > 0) {
      # Check that provinces and areas are reasonable
      valid_provinces <- c("MILANO", "ROMA", "TORINO", "NAPOLI", "PALERMO", 
                          "GENOVA", "BOLOGNA", "FIRENZE", "BARI", "CATANIA")
      province_check <- any(geo_data$provincia %in% valid_provinces)
      if (!province_check) {
        warning("No recognized Italian provinces found in data")
      }
    }
  })
  
  # Test 8: Impact evaluation variables
  run_test("Impact evaluation variables", function() {
    # Check DiD variables
    did_treated <- data[did_attribute == 1 & !is.na(did_attribute)]
    if (nrow(did_treated) > 0) {
      # Should have match quality information
      missing_quality <- did_treated[is.na(did_match_quality)]
      if (nrow(missing_quality) > nrow(did_treated) * 0.1) {
        warning("Many DiD treated units missing match quality info")
      }
    }
    
    # Check policy variables
    pol_treated <- data[pol_attribute == 1 & !is.na(pol_attribute)]
    if (nrow(pol_treated) > 0) {
      # Should have policy ID
      missing_pol_id <- pol_treated[is.na(idpol)]
      if (nrow(missing_pol_id) > nrow(pol_treated) * 0.1) {
        warning("Many policy treated units missing policy ID")
      }
    }
  })
  
  # Summary
  cat("\n--- TEST SUMMARY ---\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Total tests:", results$total_tests, "\n")
  cat("Passed:", results$passed_tests, "\n")
  cat("Failed:", results$failed_tests, "\n")
  cat("Success rate:", round(results$passed_tests / results$total_tests * 100, 1), "%\n")
  
  if (results$failed_tests > 0) {
    cat("\n--- FAILED TESTS ---\n")
    failed_tests <- results$test_details[grepl("FAILED", results$test_details)]
    for (test_name in names(failed_tests)) {
      cat("  ", test_name, ":", failed_tests[[test_name]], "\n")
    }
  }
  
  cat("=== TESTING COMPLETED ===\n\n")
  return(results)
}

#' Compare Data Functionality
#' 
#' Tests that key longworkR functions produce similar results with both datasets
compare_data_functionality <- function(real_data, synthetic_data) {
  cat("=== FUNCTIONAL COMPARISON BETWEEN DATASETS ===\n")
  
  # Test basic summary statistics
  cat("Basic Statistics Comparison:\n")
  
  # Contract duration
  real_duration <- mean(real_data$durata, na.rm = TRUE)
  synth_duration <- mean(synthetic_data$durata, na.rm = TRUE)
  duration_diff <- abs(real_duration - synth_duration) / real_duration
  
  cat("  Average duration difference:", round(duration_diff * 100, 2), "%\n")
  
  # Employment state distribution
  real_states <- table(real_data$stato)
  synth_states <- table(synthetic_data$stato)
  
  cat("  Employment state distributions:\n")
  all_states <- union(names(real_states), names(synth_states))
  for (state in all_states) {
    real_prop <- ifelse(state %in% names(real_states), 
                       real_states[state] / nrow(real_data), 0)
    synth_prop <- ifelse(state %in% names(synth_states), 
                        synth_states[state] / nrow(synthetic_data), 0)
    cat("    ", state, "- Real:", round(real_prop, 3), 
        "| Synthetic:", round(synth_prop, 3), 
        "| Diff:", round(abs(real_prop - synth_prop), 3), "\n")
  }
  
  # Age distribution (if available)
  if (sum(!is.na(real_data$eta)) > 100 && sum(!is.na(synthetic_data$eta)) > 100) {
    real_age <- mean(real_data$eta, na.rm = TRUE)
    synth_age <- mean(synthetic_data$eta, na.rm = TRUE)
    age_diff <- abs(real_age - synth_age) / real_age
    
    cat("  Average age difference:", round(age_diff * 100, 2), "%\n")
  }
  
  cat("=== FUNCTIONAL COMPARISON COMPLETED ===\n\n")
}

#' Run Complete Data Testing Workflow
#' 
#' Tests both datasets and compares their functionality
run_complete_testing <- function() {
  cat("=== LONGWORKR DATA COMPATIBILITY TESTING ===\n\n")
  
  # Load datasets
  real_data <- NULL
  synthetic_data <- NULL
  
  if (file.exists("data/sample.rds")) {
    cat("Loading real dataset...\n")
    real_data <- readRDS("data/sample.rds")
  } else {
    cat("Real dataset not found.\n")
  }
  
  if (file.exists("data/synthetic_sample.rds")) {
    cat("Loading synthetic dataset...\n")
    synthetic_data <- readRDS("data/synthetic_sample.rds")
  } else {
    cat("Synthetic dataset not found. Generating...\n")
    if (file.exists("R/synthetic_data_generator.R")) {
      source("R/synthetic_data_generator.R")
      synthetic_data <- generate_synthetic_employment_data(seed = 12345)
      saveRDS(synthetic_data, "data/synthetic_sample.rds")
    } else {
      stop("Cannot generate synthetic data: generator script not found")
    }
  }
  
  # Test datasets
  results <- list()
  
  if (!is.null(real_data)) {
    results$real <- test_dataset_compatibility(real_data, "real")
  }
  
  if (!is.null(synthetic_data)) {
    results$synthetic <- test_dataset_compatibility(synthetic_data, "synthetic")
  }
  
  # Compare functionality if both datasets available
  if (!is.null(real_data) && !is.null(synthetic_data)) {
    compare_data_functionality(real_data, synthetic_data)
  }
  
  # Final summary
  cat("=== OVERALL TESTING SUMMARY ===\n")
  for (dataset_name in names(results)) {
    result <- results[[dataset_name]]
    cat(sprintf("%-10s: %d/%d tests passed (%.1f%%)\n", 
               toupper(dataset_name),
               result$passed_tests, 
               result$total_tests,
               result$passed_tests / result$total_tests * 100))
  }
  
  cat("\n=== ALL TESTING COMPLETED ===\n")
  return(results)
}

# Command line execution
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0 || args[1] == "all") {
    run_complete_testing()
  } else if (args[1] == "real" && file.exists("data/sample.rds")) {
    real_data <- readRDS("data/sample.rds")
    test_dataset_compatibility(real_data, "real")
  } else if (args[1] == "synthetic" && file.exists("data/synthetic_sample.rds")) {
    synthetic_data <- readRDS("data/synthetic_sample.rds")
    test_dataset_compatibility(synthetic_data, "synthetic")
  } else {
    cat("Usage: Rscript test_data_compatibility.R [all|real|synthetic]\n")
    cat("  all      - Test both datasets (default)\n")
    cat("  real     - Test real dataset only\n") 
    cat("  synthetic- Test synthetic dataset only\n")
  }
}