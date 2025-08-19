# Test suite for Impact Evaluation Estimation Functions
# Tests difference-in-differences, event studies, and treatment effect aggregation

library(testthat)
library(data.table)

# Test data setup ---------------------------------------------------------

setup_estimation_test_data <- function() {
  set.seed(789)
  
  # Create panel data structure for estimation tests
  n_individuals <- 50
  n_periods <- 8
  
  # Generate individual characteristics
  individuals <- data.table(
    id = 1:n_individuals,
    cf = paste0("CF", sprintf("%03d", 1:n_individuals)),
    is_treated = c(rep(TRUE, n_individuals/2), rep(FALSE, n_individuals/2)),
    treatment_period = 5,  # Treatment starts in period 5
    age = round(rnorm(n_individuals, 35, 8)),
    education = sample(c("High School", "Bachelor", "Master"), n_individuals, replace = TRUE),
    region = sample(c("North", "Center", "South"), n_individuals, replace = TRUE)
  )
  
  # Generate panel data
  panel_data <- CJ(id = 1:n_individuals, period = 1:n_periods)
  panel_data <- merge(panel_data, individuals, by = "id")
  
  # Create treatment indicator (post-treatment for treated units)
  panel_data[, treated_post := is_treated & period >= treatment_period]
  
  # Create event time variable (relative to treatment)
  panel_data[, event_time := period - treatment_period]
  panel_data[!is_treated, event_time := NA]
  
  # Generate outcomes with treatment effects
  panel_data[, `:=`(
    # Employment rate outcome
    employment_rate = pmax(0, pmin(1, 
      0.6 +  # Baseline
      0.1 * (education == "Bachelor") + 0.2 * (education == "Master") +  # Education effect
      0.05 * period +  # Time trend
      0.15 * treated_post +  # Treatment effect
      rnorm(.N, 0, 0.1)  # Random error
    )),
    
    # Wage growth outcome
    wage_growth = pmax(-0.5, pmin(0.5,
      0.02 +  # Baseline growth
      0.01 * (education == "Bachelor") + 0.02 * (education == "Master") +
      0.005 * period +  # Time trend  
      0.08 * treated_post +  # Treatment effect
      rnorm(.N, 0, 0.05)  # Random error
    )),
    
    # Job stability outcome  
    job_stability = pmax(0, pmin(1,
      0.7 +  # Baseline
      0.08 * (education == "Bachelor") + 0.15 * (education == "Master") +
      0.02 * period +  # Time trend
      0.12 * treated_post +  # Treatment effect
      rnorm(.N, 0, 0.08)  # Random error
    ))
  )]
  
  # Add event period indicators for DiD
  panel_data[, `:=`(
    event_period = as.numeric(period >= treatment_period),
    days_to_event = ifelse(is_treated, (period - treatment_period) * 30, NA),  # Convert to days
    in_event_window = TRUE,
    pre_event_period = period < treatment_period,
    post_event_period = period >= treatment_period
  )]
  
  return(panel_data)
}

estimation_test_data <- setup_estimation_test_data()

# Tests for aggregate_treatment_effects() --------------------------------

test_that("aggregate_treatment_effects works with basic input", {
  skip_if_not_installed("data.table")
  
  # Create mock estimation results  
  mock_did_results <- list(
    estimates = data.table(
      outcome = c("employment_rate", "wage_growth"),
      treatment_effect = c(0.15, 0.08),
      std_error = c(0.05, 0.03),
      p_value = c(0.01, 0.02),
      n_obs = c(100, 100)
    )
  )
  class(mock_did_results) <- c("did_results", "list")
  
  # Test aggregation
  aggregated <- aggregate_treatment_effects(
    estimation_results = list(mock_did_results),
    aggregation_method = "weighted_average",
    multiple_testing_correction = "holm"
  )
  
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  expect_true("aggregated_effects" %in% names(aggregated))
  expect_true("individual_effects" %in% names(aggregated))
  
  # Check aggregated effects structure
  expect_s3_class(aggregated$aggregated_effects, "data.table")
  expect_true("pooled_effect" %in% names(aggregated$aggregated_effects))
  expect_true("pooled_se" %in% names(aggregated$aggregated_effects))
  
  # Check individual effects
  expect_s3_class(aggregated$individual_effects, "data.table")
  expect_true("adjusted_p_value" %in% names(aggregated$individual_effects))
})

test_that("aggregate_treatment_effects handles different aggregation methods", {
  # Create mock results
  mock_results <- list(
    estimates = data.table(
      outcome = c("outcome1", "outcome2", "outcome3"),
      treatment_effect = c(0.1, 0.15, 0.2),
      std_error = c(0.05, 0.04, 0.06),
      p_value = c(0.05, 0.01, 0.001),
      n_obs = c(100, 120, 80)
    )
  )
  class(mock_results) <- c("did_results", "list")
  
  # Test different aggregation methods
  for (method in c("simple_average", "weighted_average", "meta_analysis")) {
    aggregated <- aggregate_treatment_effects(
      estimation_results = list(mock_results),
      aggregation_method = method,
      multiple_testing_correction = "none"
    )
    
    expect_s3_class(aggregated, "aggregated_treatment_effects")
    expect_equal(aggregated$aggregation_method, method)
    
    # Meta-analysis should include heterogeneity statistics
    if (method == "meta_analysis") {
      expect_true("tau_squared" %in% names(aggregated$aggregated_effects))
      expect_true("i_squared" %in% names(aggregated$aggregated_effects))
    }
  }
})

test_that("aggregate_treatment_effects handles multiple testing corrections", {
  mock_results <- list(
    estimates = data.table(
      outcome = paste0("outcome", 1:5),
      treatment_effect = rnorm(5, 0.1, 0.05),
      std_error = runif(5, 0.03, 0.07),
      p_value = runif(5, 0.01, 0.1),
      n_obs = rep(100, 5)
    )
  )
  class(mock_results) <- c("did_results", "list")
  
  # Test different correction methods
  for (correction in c("bonferroni", "holm", "fdr")) {
    aggregated <- aggregate_treatment_effects(
      estimation_results = list(mock_results),
      multiple_testing_correction = correction
    )
    
    expect_true("adjusted_p_value" %in% names(aggregated$individual_effects))
    expect_equal(aggregated$multiple_testing_correction, correction)
    
    # Adjusted p-values should be >= original p-values (except for FDR in some cases)
    if (correction %in% c("bonferroni", "holm")) {
      expect_true(all(aggregated$individual_effects$adjusted_p_value >= 
                     aggregated$individual_effects$p_value))
    }
  }
})

test_that("aggregate_treatment_effects handles event study results", {
  # Create mock event study results
  mock_event_study <- list(
    event_estimates = data.table(
      outcome = rep("employment_rate", 6),
      event_time = c(-2, -1, 0, 1, 2, 3),
      coefficient = c(0.02, 0.01, 0.05, 0.12, 0.18, 0.15),
      std_error = rep(0.04, 6),
      p_value = c(0.6, 0.8, 0.2, 0.01, 0.001, 0.002)
    )
  )
  class(mock_event_study) <- c("event_study_results", "list")
  
  # Test aggregation
  aggregated <- aggregate_treatment_effects(
    estimation_results = list(mock_event_study),
    aggregation_method = "simple_average"
  )
  
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  expect_true(nrow(aggregated$individual_effects) > 0)
  
  # Should aggregate post-treatment effects
  expect_true("Event Study" %in% aggregated$individual_effects$method)
})

test_that("aggregate_treatment_effects error handling", {
  # Test with invalid input type
  expect_error(
    aggregate_treatment_effects("not a list"),
    "estimation_results must be a list"
  )
  
  # Test with empty results
  expect_error(
    aggregate_treatment_effects(list()),
    "No treatment effects extracted"
  )
  
  # Test with unrecognized result type
  unrecognized_result <- list(some_data = data.table(x = 1))
  class(unrecognized_result) <- "unknown_class"
  
  aggregated <- aggregate_treatment_effects(list(unrecognized_result))
  expect_equal(nrow(aggregated$individual_effects), 0)
})

# Tests for helper functions ----------------------------------------------

test_that("test_effect_heterogeneity works correctly", {
  # This tests the internal helper function
  effects_data <- data.table(
    treatment_effect = c(0.1, 0.15, 0.2, 0.12, 0.18),
    weight = c(1, 2, 1.5, 1.8, 1.2)
  )
  
  # Call the helper function (assuming it's available)
  if (exists("test_effect_heterogeneity", envir = getNamespace("vecshift"))) {
    het_test <- vecshift:::test_effect_heterogeneity(effects_data)
    
    expect_type(het_test, "list")
    expect_true("q_statistic" %in% names(het_test))
    expect_true("p_value" %in% names(het_test))
    expect_true("i_squared" %in% names(het_test))
    
    # I-squared should be between 0 and 100
    expect_true(het_test$i_squared >= 0 && het_test$i_squared <= 100)
  } else {
    skip("Helper function test_effect_heterogeneity not accessible")
  }
})

# Mock tests for difference_in_differences() -----------------------------
# Note: These tests would normally require fixest package

test_that("difference_in_differences parameter validation", {
  # Test input validation without requiring fixest
  
  # Test with non-data.table input
  expect_error(
    tryCatch({
      difference_in_differences(
        data = as.data.frame(estimation_test_data),
        outcome_vars = c("employment_rate")
      )
    }, error = function(e) {
      if (grepl("Input data must be a data.table", e$message)) {
        stop(e$message)
      } else {
        return("fixest not available")
      }
    }),
    "Input data must be a data.table"
  )
  
  # Test with missing outcome variables
  expect_error(
    tryCatch({
      difference_in_differences(
        data = estimation_test_data,
        outcome_vars = c("nonexistent_outcome")
      )
    }, error = function(e) {
      if (grepl("Missing required columns", e$message)) {
        stop(e$message)
      } else {
        return("fixest not available")
      }
    }),
    "Missing required columns"
  )
})

test_that("difference_in_differences handles fixest dependency", {
  # Test dependency handling
  result <- tryCatch({
    difference_in_differences(
      data = estimation_test_data,
      outcome_vars = c("employment_rate", "wage_growth"),
      treatment_var = "is_treated",
      time_var = "event_period",
      id_var = "cf"
    )
  }, error = function(e) {
    expect_true(grepl("Required packages not installed", e$message) ||
               grepl("fixest", e$message))
    return("expected_error")
  })
  
  expect_true(inherits(result, "did_results") || result == "expected_error")
})

# Mock tests for event_study_design() ------------------------------------

test_that("event_study_design parameter validation", {
  # Test with missing time-to-event variable
  invalid_data <- copy(estimation_test_data)
  invalid_data[, days_to_event := NULL]
  
  expect_error(
    event_study_design(
      data = invalid_data,
      outcome_vars = c("employment_rate"),
      time_to_event_var = "days_to_event"
    ),
    "Missing required columns"
  )
  
  # Test invalid time unit
  expect_error(
    event_study_design(
      data = estimation_test_data,
      outcome_vars = c("employment_rate"),
      time_unit = "invalid_unit"
    ),
    "time_unit must be one of"
  )
  
  # Test invalid event window
  expect_error(
    event_study_design(
      data = estimation_test_data,
      outcome_vars = c("employment_rate"),
      event_window = c(12, -12)  # Wrong order
    ),
    "event_window"
  )
})

test_that("event_study_design handles time unit conversion", {
  # Test different time units
  for (unit in c("days", "weeks", "months")) {
    result <- tryCatch({
      event_study_design(
        data = estimation_test_data,
        outcome_vars = c("employment_rate"),
        time_unit = unit,
        event_window = c(-2, 2)
      )
    }, error = function(e) {
      if (grepl("fixest", e$message) || grepl("Required packages", e$message)) {
        return("fixest not available")
      } else {
        stop(e)
      }
    })
    
    expect_true(inherits(result, "event_study_results") || result == "fixest not available")
  }
})

# Mock tests for synthetic_control_method() ------------------------------

test_that("synthetic_control_method parameter validation", {
  # Create panel data format for synthetic control
  panel_data <- estimation_test_data[, .(
    unit = region,  # Use region as unit
    time = period,
    outcome = employment_rate,
    treatment = as.numeric(treated_post)
  )]
  
  # Test with missing columns
  expect_error(
    tryCatch({
      synthetic_control_method(
        data = panel_data,
        outcome_var = "nonexistent_outcome",
        unit_var = "unit", 
        time_var = "time",
        treatment_var = "treatment",
        treatment_time = 5
      )
    }, error = function(e) {
      if (grepl("Missing required columns", e$message)) {
        stop(e$message)
      } else {
        return("augsynth not available")
      }
    }),
    "Missing required columns"
  )
})

test_that("synthetic_control_method handles augsynth dependency", {
  # Test dependency handling
  panel_data <- estimation_test_data[, .(
    unit = region,
    time = period,
    outcome = employment_rate,
    treatment = as.numeric(treated_post)
  )]
  
  result <- tryCatch({
    synthetic_control_method(
      data = panel_data,
      outcome_var = "outcome",
      unit_var = "unit",
      time_var = "time", 
      treatment_var = "treatment",
      treatment_time = 5
    )
  }, error = function(e) {
    expect_true(grepl("Package 'augsynth' not installed", e$message) ||
               grepl("augsynth", e$message))
    return("expected_error")
  })
  
  expect_true(inherits(result, "synth_results") || result == "expected_error")
})

# Integration tests -------------------------------------------------------

test_that("Estimation workflow integration", {
  # Test that estimation components work together in a pipeline
  
  # Create simplified mock results for integration testing
  mock_did <- list(
    estimates = data.table(
      outcome = c("employment_rate", "wage_growth"),
      treatment_effect = c(0.12, 0.06),
      std_error = c(0.04, 0.03),
      p_value = c(0.01, 0.05),
      n_obs = c(200, 200)
    )
  )
  class(mock_did) <- c("did_results", "list")
  
  mock_event_study <- list(
    event_estimates = data.table(
      outcome = rep("employment_rate", 4),
      event_time = c(-1, 0, 1, 2),
      coefficient = c(0.02, 0.08, 0.15, 0.12),
      std_error = rep(0.05, 4)
    )
  )
  class(mock_event_study) <- c("event_study_results", "list")
  
  # Test aggregation of multiple result types
  aggregated <- aggregate_treatment_effects(
    estimation_results = list(mock_did, mock_event_study),
    aggregation_method = "weighted_average",
    multiple_testing_correction = "holm"
  )
  
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  expect_true(nrow(aggregated$individual_effects) >= 2)  # At least DiD + Event Study
  
  # Should have effects from both methods
  methods <- unique(aggregated$individual_effects$method)
  expect_true(length(methods) >= 2)
})

test_that("Print methods work for results objects", {
  # Test print methods for different result types
  mock_did <- list(
    estimates = data.table(
      outcome = "employment_rate",
      treatment_effect = 0.15,
      std_error = 0.05,
      p_value = 0.01,
      significant = TRUE
    )
  )
  class(mock_did) <- c("did_results", "list")
  
  # Test that print method exists and works
  expect_output(print(mock_did), "Difference-in-Differences")
  
  # Test aggregated results print
  aggregated <- aggregate_treatment_effects(list(mock_did))
  expect_silent(print(aggregated))  # Should not error
})

# Edge cases and robustness tests ----------------------------------------

test_that("Functions handle edge cases in aggregation", {
  # Test with single effect
  single_effect <- list(
    estimates = data.table(
      outcome = "outcome1",
      treatment_effect = 0.1,
      std_error = 0.05,
      p_value = 0.05,
      n_obs = 100
    )
  )
  class(single_effect) <- c("did_results", "list")
  
  aggregated <- aggregate_treatment_effects(list(single_effect))
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  
  # Test with effects with missing standard errors
  missing_se <- list(
    estimates = data.table(
      outcome = "outcome1",
      treatment_effect = 0.1,
      std_error = NA_real_,
      p_value = 0.05,
      n_obs = 100
    )
  )
  class(missing_se) <- c("did_results", "list")
  
  # Should handle gracefully or give informative error
  result <- tryCatch({
    aggregate_treatment_effects(list(missing_se))
  }, error = function(e) {
    return("expected_error_with_missing_se")
  })
  
  expect_true(inherits(result, "aggregated_treatment_effects") || 
              result == "expected_error_with_missing_se")
})

test_that("Functions handle extreme values", {
  # Test with very small/large effect sizes
  extreme_effects <- list(
    estimates = data.table(
      outcome = c("small_effect", "large_effect"),
      treatment_effect = c(0.001, 2.0),  # Very small and very large
      std_error = c(0.0001, 0.5),
      p_value = c(0.01, 0.001),
      n_obs = c(10000, 50)  # Large and small samples
    )
  )
  class(extreme_effects) <- c("did_results", "list")
  
  aggregated <- aggregate_treatment_effects(list(extreme_effects))
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  
  # Aggregated effect should be reasonable
  expect_true(is.finite(aggregated$aggregated_effects$pooled_effect))
  expect_true(aggregated$aggregated_effects$pooled_effect > 0)
})

# Performance tests -------------------------------------------------------

test_that("Aggregation performs well with many effects", {
  # Create many mock effects
  n_effects <- 100
  many_effects <- list(
    estimates = data.table(
      outcome = paste0("outcome_", 1:n_effects),
      treatment_effect = rnorm(n_effects, 0.1, 0.05),
      std_error = runif(n_effects, 0.02, 0.08),
      p_value = runif(n_effects, 0.001, 0.1),
      n_obs = sample(50:500, n_effects, replace = TRUE)
    )
  )
  class(many_effects) <- c("did_results", "list")
  
  # Time the aggregation
  start_time <- Sys.time()
  
  aggregated <- aggregate_treatment_effects(
    list(many_effects),
    aggregation_method = "meta_analysis",
    multiple_testing_correction = "fdr"
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(end_time - start_time, units = "secs")
  
  expect_s3_class(aggregated, "aggregated_treatment_effects")
  expect_true(duration < 2)  # Should complete within 2 seconds
  expect_equal(nrow(aggregated$individual_effects), n_effects)
})

# Cleanup -----------------------------------------------------------------

rm(estimation_test_data)