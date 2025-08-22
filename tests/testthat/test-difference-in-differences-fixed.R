# Comprehensive Test Suite for Fixed difference_in_differences() Function
# Tests the fixed function with matched data, collinearity resolution, and edge cases

library(testthat)
library(data.table)

# Test data setup - simulating matched data from propensity_score_matching() -----

setup_matched_test_data <- function() {
  set.seed(789)
  
  # Create realistic matched data structure that mimics propensity_score_matching() output
  n_treated <- 30
  n_control <- 60  # 2:1 ratio
  
  # Create treated individuals with pre/post periods
  treated_data <- data.table(
    cf = paste0("T", sprintf("%03d", rep(1:n_treated, 2))),
    is_treated = TRUE,
    event_time = rep(c("pre", "post"), each = n_treated),
    pre_event_period = rep(c(TRUE, FALSE), each = n_treated),
    post_event_period = rep(c(FALSE, TRUE), each = n_treated),
    age = rep(round(rnorm(n_treated, 35, 8)), 2),
    education = rep(sample(c("High School", "Bachelor", "Master"), n_treated, replace = TRUE), 2),
    sector = rep(sample(c("Manufacturing", "Services", "Construction"), n_treated, replace = TRUE), 2)
  )
  
  # Create control individuals (only "control" period - this is key for testing)
  control_data <- data.table(
    cf = paste0("C", sprintf("%03d", 1:n_control)),
    is_treated = FALSE,
    event_time = "control",
    pre_event_period = FALSE,
    post_event_period = FALSE,
    age = round(rnorm(n_control, 35, 8)),
    education = sample(c("High School", "Bachelor", "Master"), n_control, replace = TRUE),
    sector = sample(c("Manufacturing", "Services", "Construction"), n_control, replace = TRUE)
  )
  
  # Combine data
  matched_data <- rbind(treated_data, control_data)
  
  # Add realistic outcome variables with treatment effects
  matched_data[, `:=`(
    # Duration in days (contract length)
    durata = pmax(30, 
      180 +  # Base duration
      20 * (education == "Bachelor") + 40 * (education == "Master") +  # Education effect
      (event_time == "post") * is_treated * 60 +  # Treatment effect for post-treatment
      rnorm(.N, 0, 30)  # Random variation
    ),
    
    # Wages/salary
    retribuzione = pmax(1000,
      2500 +  # Base salary
      300 * (education == "Bachelor") + 600 * (education == "Master") +  # Education effect
      100 * (sector == "Services") + 50 * (sector == "Manufacturing") +  # Sector effect
      (event_time == "post") * is_treated * 400 +  # Treatment effect for post-treatment
      rnorm(.N, 0, 200)  # Random variation
    ),
    
    # Employment stability index (0-1)
    stability_index = pmax(0, pmin(1,
      0.6 +  # Base stability
      0.1 * (education == "Bachelor") + 0.2 * (education == "Master") +
      (event_time == "post") * is_treated * 0.15 +  # Treatment effect
      rnorm(.N, 0, 0.1)
    ))
  )]
  
  # Add some missing values to test edge cases
  missing_indices <- sample(nrow(matched_data), 5)
  matched_data[missing_indices[1:2], durata := NA]
  matched_data[missing_indices[3:4], retribuzione := NA]
  
  return(matched_data)
}

# Test data for single outcome scenario
setup_single_outcome_data <- function() {
  data <- setup_matched_test_data()
  # Keep only one outcome variable
  data[, c("retribuzione", "stability_index") := NULL]
  return(data)
}

# Test data with high collinearity scenario
setup_collinear_data <- function() {
  data <- setup_matched_test_data()
  # Make treatment highly correlated with time by design
  # This simulates the matched data scenario where all treated are "post" 
  # and all control are "control"
  return(data)
}

# Core Tests for Fixed difference_in_differences() Function ------------------

test_that("difference_in_differences correctly handles matched data with event_time structure", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  # Test that the function recognizes and handles matched data structure
  expect_message(
    did_result <- difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata", "retribuzione"),
      treatment_var = "is_treated",
      time_var = "event_time",
      id_var = "cf",
      fixed_effects = "individual",
      verbose = TRUE
    ),
    "Detected matched data with event_time structure"
  )
  
  expect_s3_class(did_result, "did_results")
  expect_true("estimates" %in% names(did_result))
  expect_s3_class(did_result$estimates, "data.table")
})

test_that("difference_in_differences correctly restructures control observations", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  # Check initial data distribution
  initial_distribution <- table(matched_data$event_time, matched_data$is_treated, useNA = "ifany")
  expect_true("control" %in% rownames(initial_distribution))
  expect_true(initial_distribution["control", "FALSE"] > 0)  # Control group in "control" period
  
  # Function should restructure this
  expect_warning(
    did_result <- difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata"),
      treatment_var = "is_treated", 
      time_var = "event_time",
      id_var = "cf",
      fixed_effects = "individual",
      verbose = FALSE
    ),
    "Restructured matched data for DiD: control observations duplicated for pre/post periods"
  )
  
  expect_s3_class(did_result, "did_results")
})

test_that("difference_in_differences handles collinearity by skipping time fixed effects", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_collinear_data()
  
  # This should trigger the collinearity detection and skip time FE
  expect_warning(
    did_result <- difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata", "retribuzione"),
      treatment_var = "is_treated",
      time_var = "event_time", 
      id_var = "cf",
      fixed_effects = "both",  # Request both individual and time FE
      verbose = TRUE
    ),
    "Time fixed effects skipped due to high correlation with treatment"
  )
  
  expect_s3_class(did_result, "did_results")
  expect_true(nrow(did_result$estimates) > 0)
})

test_that("difference_in_differences produces correct results structure", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  did_result <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata", "retribuzione", "stability_index"),
    treatment_var = "is_treated",
    time_var = "event_time",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Check that we get exactly one row per outcome variable (no duplicates)
  expect_equal(nrow(did_result$estimates), 3)
  expect_equal(length(unique(did_result$estimates$outcome)), 3)
  expect_true(all(c("durata", "retribuzione", "stability_index") %in% did_result$estimates$outcome))
  
  # Check all required columns are present and properly populated
  required_cols <- c("outcome", "treatment_effect", "std_error", "t_statistic", 
                    "p_value", "conf_lower", "conf_upper", "significant", "n_obs", "r_squared")
  expect_true(all(required_cols %in% names(did_result$estimates)))
  
  # Check R-squared values are properly extracted (should be numeric and not all NA)
  expect_true(is.numeric(did_result$estimates$r_squared))
  expect_true(sum(!is.na(did_result$estimates$r_squared)) > 0)
  
  # Check that confidence intervals are consistent
  expect_true(all(did_result$estimates$conf_lower <= did_result$estimates$conf_upper, na.rm = TRUE))
  
  # Check treatment effects are numeric and finite
  expect_true(all(is.finite(did_result$estimates$treatment_effect)))
})

test_that("difference_in_differences handles missing values in outcomes correctly", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  # The setup already includes some missing values
  initial_n <- nrow(matched_data)
  
  expect_message(
    did_result <- difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata", "retribuzione"),
      treatment_var = "is_treated",
      time_var = "event_time",
      id_var = "cf",
      fixed_effects = "individual",
      verbose = TRUE
    ),
    "Removed .* rows with missing outcome values"
  )
  
  expect_s3_class(did_result, "did_results")
  # Should still have results for both outcomes
  expect_equal(nrow(did_result$estimates), 2)
})

test_that("difference_in_differences works with single outcome variable", {
  skip_if_not_installed("fixest")
  
  single_outcome_data <- setup_single_outcome_data()
  
  did_result <- difference_in_differences(
    data = single_outcome_data,
    outcome_vars = c("durata"),
    treatment_var = "is_treated",
    time_var = "event_time",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 1)
  expect_equal(did_result$estimates$outcome, "durata")
  
  # All columns should be properly populated for single outcome
  expect_true(is.finite(did_result$estimates$treatment_effect))
  expect_true(is.finite(did_result$estimates$std_error))
  expect_true(!is.na(did_result$estimates$n_obs))
})

test_that("difference_in_differences handles different fixed effects specifications", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  # Test individual FE only
  did_individual <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata"),
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Test time FE only (should be skipped due to collinearity, but shouldn't crash)
  did_time <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata"),
    fixed_effects = "time",
    verbose = FALSE
  )
  
  # Test both FE (time should be skipped)
  expect_warning(
    did_both <- difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata"),
      fixed_effects = "both",
      verbose = FALSE
    ),
    "Time fixed effects skipped"
  )
  
  # All should produce valid results
  expect_s3_class(did_individual, "did_results")
  expect_s3_class(did_time, "did_results")
  expect_s3_class(did_both, "did_results")
  
  # All should have one row for the outcome
  expect_equal(nrow(did_individual$estimates), 1)
  expect_equal(nrow(did_time$estimates), 1)
  expect_equal(nrow(did_both$estimates), 1)
})

test_that("difference_in_differences handles control variables correctly", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  # Test with control variables
  did_with_controls <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata"),
    control_vars = c("age", "education", "sector"),
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_with_controls, "did_results")
  expect_equal(nrow(did_with_controls$estimates), 1)
  
  # Test with non-existent control variables (should be filtered out)
  did_mixed_controls <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata"),
    control_vars = c("age", "nonexistent_var", "education"),
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_mixed_controls, "did_results")
  expect_equal(nrow(did_mixed_controls$estimates), 1)
})

test_that("difference_in_differences error handling works correctly", {
  matched_data <- setup_matched_test_data()
  
  # Test with non-data.table input
  expect_error(
    difference_in_differences(
      data = as.data.frame(matched_data),
      outcome_vars = c("durata")
    ),
    "Input data must be a data.table"
  )
  
  # Test with missing required columns
  expect_error(
    difference_in_differences(
      data = matched_data,
      outcome_vars = c("nonexistent_outcome")
    ),
    "Missing required columns"
  )
  
  # Test with missing treatment variable
  expect_error(
    difference_in_differences(
      data = matched_data,
      outcome_vars = c("durata"),
      treatment_var = "nonexistent_treatment"
    ),
    "Missing required columns"
  )
})

test_that("difference_in_differences handles edge case: no pre/post data for treated", {
  skip_if_not_installed("fixest")
  
  # Create data with only control observations
  control_only_data <- setup_matched_test_data()[is_treated == FALSE]
  control_only_data[, event_time := "control"]
  
  expect_error(
    difference_in_differences(
      data = control_only_data,
      outcome_vars = c("durata"),
      treatment_var = "is_treated",
      time_var = "event_time",
      id_var = "cf"
    ),
    "No pre/post observations found for treated units"
  )
})

test_that("difference_in_differences handles edge case: no control observations", {
  skip_if_not_installed("fixest")
  
  # Create data with only treated observations  
  treated_only_data <- setup_matched_test_data()[is_treated == TRUE]
  
  expect_error(
    difference_in_differences(
      data = treated_only_data,
      outcome_vars = c("durata"),
      treatment_var = "is_treated",
      time_var = "event_time",
      id_var = "cf"
    ),
    "No control observations found"
  )
})

# Tests for Model Results and Advanced Features ------------------------------

test_that("difference_in_differences returns complete model results", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  did_result <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata", "retribuzione"),
    treatment_var = "is_treated",
    time_var = "event_time",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Check that model_results contains fixest objects
  expect_true("model_results" %in% names(did_result))
  expect_true(length(did_result$model_results) > 0)
  
  # Each model should be a fixest object
  for (model_name in names(did_result$model_results)) {
    expect_true(inherits(did_result$model_results[[model_name]], "fixest"))
  }
})

test_that("difference_in_differences detects treatment effects correctly", {
  skip_if_not_installed("fixest")
  
  # Create data with known treatment effect
  set.seed(456)
  n_treated <- 20
  n_control <- 40
  
  # Create treated with clear pre/post difference
  treated_data <- data.table(
    cf = paste0("T", sprintf("%03d", rep(1:n_treated, 2))),
    is_treated = TRUE,
    event_time = rep(c("pre", "post"), each = n_treated),
    outcome_with_effect = c(
      rnorm(n_treated, 100, 10),  # Pre-treatment: mean 100
      rnorm(n_treated, 150, 10)   # Post-treatment: mean 150 (effect = 50)
    )
  )
  
  # Create control group (no time variation, mean 100)
  control_data <- data.table(
    cf = paste0("C", sprintf("%03d", 1:n_control)),
    is_treated = FALSE,
    event_time = "control", 
    outcome_with_effect = rnorm(n_control, 100, 10)
  )
  
  test_data <- rbind(treated_data, control_data)
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = c("outcome_with_effect"),
    treatment_var = "is_treated",
    time_var = "event_time",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Should detect positive treatment effect around 50
  expect_true(did_result$estimates$treatment_effect > 30)
  expect_true(did_result$estimates$treatment_effect < 70)
  expect_true(did_result$estimates$significant)
})

# Performance and Integration Tests -------------------------------------------

test_that("difference_in_differences performs reasonably with larger datasets", {
  skip_if_not_installed("fixest")
  
  # Create larger matched dataset
  set.seed(789)
  n_treated_large <- 100
  n_control_large <- 200
  
  # Generate larger dataset
  treated_large <- data.table(
    cf = paste0("T", sprintf("%04d", rep(1:n_treated_large, 2))),
    is_treated = TRUE,
    event_time = rep(c("pre", "post"), each = n_treated_large),
    outcome1 = rnorm(n_treated_large * 2, 500, 50),
    outcome2 = rnorm(n_treated_large * 2, 1000, 100)
  )
  
  control_large <- data.table(
    cf = paste0("C", sprintf("%04d", 1:n_control_large)),
    is_treated = FALSE,
    event_time = "control",
    outcome1 = rnorm(n_control_large, 500, 50),
    outcome2 = rnorm(n_control_large, 1000, 100)
  )
  
  large_data <- rbind(treated_large, control_large)
  
  # Time the execution
  start_time <- Sys.time()
  
  did_result <- difference_in_differences(
    data = large_data,
    outcome_vars = c("outcome1", "outcome2"),
    treatment_var = "is_treated",
    time_var = "event_time",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(end_time - start_time, units = "secs")
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 2)
  expect_true(duration < 10)  # Should complete within 10 seconds
})

test_that("difference_in_differences print method works", {
  skip_if_not_installed("fixest")
  
  matched_data <- setup_matched_test_data()
  
  did_result <- difference_in_differences(
    data = matched_data,
    outcome_vars = c("durata"),
    verbose = FALSE
  )
  
  # Print method should work without error
  expect_output(print(did_result), "Difference-in-Differences Results")
  expect_output(print(did_result), "Treatment Effects")
})

# Cleanup ---------------------------------------------------------------------

test_that("Test data setup functions work correctly", {
  # Verify our test data setup functions create valid data
  
  matched_data <- setup_matched_test_data()
  expect_s3_class(matched_data, "data.table")
  expect_true(all(c("cf", "is_treated", "event_time", "durata", "retribuzione") %in% names(matched_data)))
  expect_true("pre" %in% matched_data$event_time)
  expect_true("post" %in% matched_data$event_time)
  expect_true("control" %in% matched_data$event_time)
  
  single_data <- setup_single_outcome_data()
  expect_s3_class(single_data, "data.table")
  expect_true("durata" %in% names(single_data))
  expect_false("retribuzione" %in% names(single_data))
  
  collinear_data <- setup_collinear_data()
  expect_s3_class(collinear_data, "data.table")
  expect_true(all(c("cf", "is_treated", "event_time") %in% names(collinear_data)))
})