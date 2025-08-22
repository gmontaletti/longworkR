# Focused Test Suite for difference_in_differences() with Matched Data
# Tests the core functionality with realistic matched data scenarios

library(testthat)
library(data.table)

# Test data setup - simulating realistic matched data scenario ---------------

setup_realistic_matched_data <- function() {
  set.seed(123)
  
  # Create matched data that closely resembles propensity_score_matching() output
  n_treated <- 25
  n_control <- 50
  
  # Treated individuals: both pre and post observations
  treated_pre <- data.table(
    cf = paste0("T", sprintf("%03d", 1:n_treated)),
    is_treated = 1,
    event_period = 0,  # Use event_period instead of event_time for consistency
    age = round(rnorm(n_treated, 35, 8)),
    education = sample(c("High School", "Bachelor", "Master"), n_treated, replace = TRUE),
    durata = rnorm(n_treated, 150, 30),
    retribuzione = rnorm(n_treated, 2500, 400)
  )
  
  treated_post <- data.table(
    cf = paste0("T", sprintf("%03d", 1:n_treated)),
    is_treated = 1,
    event_period = 1,
    age = treated_pre$age,  # Same individuals
    education = treated_pre$education,
    durata = treated_pre$durata + rnorm(n_treated, 50, 20),  # Treatment effect ~50
    retribuzione = treated_pre$retribuzione + rnorm(n_treated, 300, 100)  # Treatment effect ~300
  )
  
  # Control individuals: one observation per person
  control_data <- data.table(
    cf = paste0("C", sprintf("%03d", 1:n_control)),
    is_treated = 0,
    event_period = 0,  # Controls in pre-period
    age = round(rnorm(n_control, 35, 8)),
    education = sample(c("High School", "Bachelor", "Master"), n_control, replace = TRUE),
    durata = rnorm(n_control, 150, 30),
    retribuzione = rnorm(n_control, 2500, 400)
  )
  
  # Combine all data
  all_data <- rbind(treated_pre, treated_post, control_data)
  return(all_data)
}

# Test data that simulates the event_time structure from matching
setup_event_time_matched_data <- function() {
  set.seed(456)
  
  n_treated <- 20
  n_control <- 40
  
  # Treated: pre and post periods
  treated_data <- data.table(
    cf = paste0("T", rep(1:n_treated, 2)),
    is_treated = 1,
    event_time = rep(c("pre", "post"), each = n_treated),
    durata = c(rnorm(n_treated, 150, 20), rnorm(n_treated, 200, 25)),  # Effect = 50
    retribuzione = c(rnorm(n_treated, 2500, 300), rnorm(n_treated, 2800, 350))  # Effect = 300
  )
  
  # Control: only "control" period
  control_data <- data.table(
    cf = paste0("C", 1:n_control),
    is_treated = 0,
    event_time = "control",
    durata = rnorm(n_control, 150, 20),
    retribuzione = rnorm(n_control, 2500, 300)
  )
  
  return(rbind(treated_data, control_data))
}

# Core Functionality Tests ---------------------------------------------------

test_that("difference_in_differences works with standard binary time variable", {
  skip_if_not_installed("fixest")
  
  standard_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = standard_data,
    outcome_vars = c("durata", "retribuzione"),
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_result, "did_results")
  expect_s3_class(did_result$estimates, "data.table")
  expect_equal(nrow(did_result$estimates), 2)
  expect_true(all(c("durata", "retribuzione") %in% did_result$estimates$outcome))
  
  # Check that treatment effects are detected
  expect_true(all(is.finite(did_result$estimates$treatment_effect)))
  expect_true(all(is.finite(did_result$estimates$std_error)))
})

test_that("difference_in_differences handles event_time structure correctly", {
  skip_if_not_installed("fixest")
  
  event_time_data <- setup_event_time_matched_data()
  
  # Should detect and restructure the event_time data
  expect_message(
    did_result <- difference_in_differences(
      data = event_time_data,
      outcome_vars = c("durata"),
      treatment_var = "is_treated",
      time_var = "event_time",
      id_var = "cf",
      fixed_effects = "individual",
      verbose = TRUE
    ),
    "Detected matched data with event_time structure"
  )
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 1)
  expect_equal(did_result$estimates$outcome, "durata")
})

test_that("difference_in_differences produces expected treatment effects", {
  skip_if_not_installed("fixest")
  
  # Use data with known treatment effects
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = c("durata", "retribuzione"),
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Check durata effect (should be around 50)
  durata_effect <- did_result$estimates[outcome == "durata", treatment_effect]
  expect_true(durata_effect > 20)  # Should detect positive effect
  expect_true(durata_effect < 100)
  
  # Check retribuzione effect (should be around 300)
  retrib_effect <- did_result$estimates[outcome == "retribuzione", treatment_effect]
  expect_true(retrib_effect > 100)  # Should detect positive effect
  expect_true(retrib_effect < 600)
  
  # Check that confidence intervals are reasonable
  expect_true(all(did_result$estimates$conf_lower < did_result$estimates$conf_upper))
})

test_that("difference_in_differences handles missing values correctly", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  # Add missing values
  test_data[1:3, durata := NA]
  test_data[4:5, retribuzione := NA]
  
  initial_n <- nrow(test_data)
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = c("durata", "retribuzione"),
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Should still produce results for both outcomes
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 2)
  
  # Check that n_obs reflects missing value removal
  expect_true(all(did_result$estimates$n_obs < initial_n))
})

test_that("difference_in_differences handles single outcome variable", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = "durata",
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 1)
  expect_equal(did_result$estimates$outcome, "durata")
  expect_true(is.finite(did_result$estimates$treatment_effect))
})

test_that("difference_in_differences with control variables", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = "durata",
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    control_vars = c("age", "education"),
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 1)
  expect_true(is.finite(did_result$estimates$treatment_effect))
})

test_that("difference_in_differences error handling", {
  test_data <- setup_realistic_matched_data()
  
  # Test with non-data.table
  expect_error(
    difference_in_differences(
      data = as.data.frame(test_data),
      outcome_vars = "durata"
    ),
    "Input data must be a data.table"
  )
  
  # Test with missing outcome
  expect_error(
    difference_in_differences(
      data = test_data,
      outcome_vars = "nonexistent_var"
    ),
    "Missing required columns"
  )
  
  # Test with missing treatment variable
  expect_error(
    difference_in_differences(
      data = test_data,
      outcome_vars = "durata",
      treatment_var = "nonexistent_treatment"
    ),
    "Missing required columns"
  )
})

# R-squared extraction test
test_that("difference_in_differences extracts R-squared correctly", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = c("durata"),
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # R-squared should be extracted (may be NA if extraction fails, but shouldn't error)
  expect_true("r_squared" %in% names(did_result$estimates))
  expect_true(is.numeric(did_result$estimates$r_squared))
  
  # If R-squared is not NA, it should be between 0 and 1
  r_sq <- did_result$estimates$r_squared
  if (!is.na(r_sq)) {
    expect_true(r_sq >= 0 && r_sq <= 1)
  }
})

# Model results test
test_that("difference_in_differences returns model objects", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = c("durata", "retribuzione"),
    treatment_var = "is_treated", 
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Should return model results
  expect_true("model_results" %in% names(did_result))
  expect_true(is.list(did_result$model_results))
  expect_true(length(did_result$model_results) > 0)
  
  # Models should be fixest objects
  for (model in did_result$model_results) {
    expect_true(inherits(model, "fixest"))
  }
})

# Print method test
test_that("difference_in_differences print method works", {
  skip_if_not_installed("fixest")
  
  test_data <- setup_realistic_matched_data()
  
  did_result <- difference_in_differences(
    data = test_data,
    outcome_vars = "durata",
    treatment_var = "is_treated",
    time_var = "event_period", 
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  # Print should work without error
  expect_output(print(did_result), "Difference-in-Differences Results")
  expect_output(print(did_result), "Treatment Effects")
})

# Performance test
test_that("difference_in_differences performs reasonably", {
  skip_if_not_installed("fixest")
  
  # Create larger dataset
  set.seed(789)
  n_treated_large <- 50
  n_control_large <- 100
  
  large_data <- data.table(
    cf = c(paste0("T", rep(1:n_treated_large, 2)), paste0("C", 1:n_control_large)),
    is_treated = c(rep(1, n_treated_large * 2), rep(0, n_control_large)),
    event_period = c(rep(c(0, 1), each = n_treated_large), rep(0, n_control_large)),
    outcome1 = rnorm(n_treated_large * 2 + n_control_large, 100, 20),
    outcome2 = rnorm(n_treated_large * 2 + n_control_large, 500, 100)
  )
  
  start_time <- Sys.time()
  
  did_result <- difference_in_differences(
    data = large_data,
    outcome_vars = c("outcome1", "outcome2"),
    treatment_var = "is_treated",
    time_var = "event_period",
    id_var = "cf",
    fixed_effects = "individual",
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(end_time - start_time, units = "secs")
  
  expect_s3_class(did_result, "did_results")
  expect_equal(nrow(did_result$estimates), 2)
  expect_true(duration < 5)  # Should complete within 5 seconds
})