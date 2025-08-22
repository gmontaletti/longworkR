# Test suite for Impact Evaluation Matching Functions
# Tests propensity score matching, CEM, balance assessment, and match quality functions

library(testthat)
library(data.table)

# Test data setup ---------------------------------------------------------

setup_matching_test_data <- function() {
  set.seed(456)
  
  # Create more realistic data for matching tests
  n <- 200
  # Create age first
  age_values <- round(rnorm(n, 35, 8))
  
  test_data <- data.table(
    id = 1:n,
    is_treated = c(rep(TRUE, n/4), rep(FALSE, 3*n/4)),  # 25% treatment rate
    age = age_values,
    education = sample(c("High School", "Bachelor", "Master"), n, 
                      prob = c(0.5, 0.35, 0.15), replace = TRUE),
    sector = sample(c("Manufacturing", "Services", "Construction", "Trade"), n,
                   prob = c(0.3, 0.4, 0.2, 0.1), replace = TRUE),
    region = sample(c("North", "Center", "South"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    prior_wage = exp(rnorm(n, log(2000), 0.3)),  # Log-normal wage distribution
    experience = pmax(0, age_values - 22 + rnorm(n, 0, 2)),
    baseline_employment_rate = pmin(1, pmax(0, rnorm(n, 0.7, 0.2)))
  )
  
  # Make treatment somewhat dependent on covariates (for realistic matching)
  treatment_prob <- plogis(-1 + 
                          0.02 * test_data$age + 
                          0.3 * (test_data$education == "Bachelor") +
                          0.5 * (test_data$education == "Master") +
                          0.0003 * test_data$prior_wage)
  
  # Reassign treatment status based on realistic probabilities
  test_data[, is_treated := runif(n) < treatment_prob]
  
  return(test_data)
}

matching_test_data <- setup_matching_test_data()

# Tests for assess_balance() ---------------------------------------------

test_that("assess_balance works with numeric variables", {
  skip_if_not_installed("data.table")
  
  balance_result <- assess_balance(
    matched_data = matching_test_data,
    treatment_var = "is_treated",
    balance_variables = c("age", "prior_wage", "experience"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  expect_type(balance_result, "list")
  expect_true("balance_table" %in% names(balance_result))
  expect_true("overall_balance" %in% names(balance_result))
  expect_s3_class(balance_result$balance_table, "data.table")
  
  # Check balance table structure
  expected_cols <- c("variable", "variable_type", "treated_mean", "control_mean", 
                    "std_mean_diff", "balance_concern")
  expect_true(all(expected_cols %in% names(balance_result$balance_table)))
  
  # Check that standardized differences are calculated
  expect_true(all(!is.na(balance_result$balance_table$std_mean_diff)))
  
  # Check overall balance summary
  expect_true("n_variables" %in% names(balance_result$overall_balance))
  expect_true("prop_balanced" %in% names(balance_result$overall_balance))
})

test_that("assess_balance works with categorical variables", {
  balance_result <- assess_balance(
    matched_data = matching_test_data,
    treatment_var = "is_treated",
    balance_variables = c("education", "sector", "gender"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  expect_s3_class(balance_result$balance_table, "data.table")
  
  # Categorical variables should have variable_type = "categorical"
  cat_vars <- balance_result$balance_table[variable %in% c("education", "sector", "gender")]
  expect_true(all(cat_vars$variable_type == "categorical"))
})

test_that("assess_balance handles weighted data", {
  # Add weights column
  weighted_data <- copy(matching_test_data)
  weighted_data[, weights := runif(.N, 0.5, 1.5)]
  
  balance_result <- assess_balance(
    matched_data = weighted_data,
    treatment_var = "is_treated", 
    balance_variables = c("age", "prior_wage"),
    weight_var = "weights"
  )
  
  expect_s3_class(balance_result$balance_table, "data.table")
  expect_true(nrow(balance_result$balance_table) > 0)
})

test_that("assess_balance error handling", {
  # Test with non-data.table input
  expect_error(
    assess_balance(
      matched_data = as.data.frame(matching_test_data),
      balance_variables = c("age")
    ),
    "matched_data must be a data.table"
  )
  
  # Test with missing treatment variable
  expect_error(
    assess_balance(
      matched_data = matching_test_data,
      treatment_var = "nonexistent_var",
      balance_variables = c("age")
    ),
    "Treatment variable nonexistent_var not found"
  )
  
  # Test with missing balance variables
  expect_error(
    assess_balance(
      matched_data = matching_test_data,
      balance_variables = c("nonexistent_var")
    ),
    "Balance variables not found"
  )
})

# Mock tests for propensity_score_matching() ----------------------------
# Note: These tests would normally require MatchIt package

test_that("propensity_score_matching parameter validation", {
  # Test input validation without requiring MatchIt
  
  # Test with non-data.table input
  expect_error(
    tryCatch({
      propensity_score_matching(
        data = as.data.frame(matching_test_data),
        matching_variables = c("age", "education")
      )
    }, error = function(e) {
      if (grepl("Input data must be a data.table", e$message)) {
        stop(e$message)
      } else {
        # MatchIt not installed - that's OK for parameter validation test
        return("MatchIt not available")
      }
    }),
    "Input data must be a data.table"
  )
  
  # Test with missing treatment variable
  expect_error(
    tryCatch({
      propensity_score_matching(
        data = matching_test_data,
        treatment_var = "nonexistent_var",
        matching_variables = c("age", "education")
      )
    }, error = function(e) {
      if (grepl("Treatment variable", e$message)) {
        stop(e$message)
      } else {
        return("MatchIt not available")
      }
    }),
    "Treatment variable nonexistent_var not found"
  )
  
  # Test with missing matching variables
  expect_error(
    tryCatch({
      propensity_score_matching(
        data = matching_test_data,
        matching_variables = c("nonexistent_var")
      )
    }, error = function(e) {
      if (grepl("Matching variables not found", e$message)) {
        stop(e$message)
      } else {
        return("MatchIt not available")
      }
    }),
    "Matching variables not found"
  )
})

test_that("propensity_score_matching handles MatchIt dependency", {
  # Test that function properly handles missing MatchIt package
  result <- tryCatch({
    propensity_score_matching(
      data = matching_test_data,
      matching_variables = c("age", "education", "prior_wage"),
      method = "nearest",
      ratio = 1
    )
  }, error = function(e) {
    expect_true(grepl("Required packages not installed", e$message) ||
               grepl("MatchIt", e$message))
    return("expected_error")
  })
  
  # Either should work (if MatchIt is installed) or give expected error
  expect_true(is.list(result) || result == "expected_error")
})

# Mock tests for coarsened_exact_matching() ------------------------------

test_that("coarsened_exact_matching parameter validation", {
  # Test basic parameter validation
  
  # Test with non-data.table input  
  expect_error(
    tryCatch({
      coarsened_exact_matching(
        data = as.data.frame(matching_test_data),
        matching_variables = c("age", "education")
      )
    }, error = function(e) {
      if (grepl("Input data must be a data.table", e$message)) {
        stop(e$message)
      } else {
        return("cem not available")
      }
    }),
    "Input data must be a data.table"
  )
  
  # Test with missing treatment variable
  expect_error(
    tryCatch({
      coarsened_exact_matching(
        data = matching_test_data,
        treatment_var = "nonexistent_var",
        matching_variables = c("age", "education")
      )
    }, error = function(e) {
      if (grepl("Treatment variable", e$message)) {
        stop(e$message)  
      } else {
        return("cem not available")
      }
    }),
    "Treatment variable nonexistent_var not found"
  )
})

test_that("coarsened_exact_matching handles cem dependency", {
  # Test dependency handling
  result <- tryCatch({
    coarsened_exact_matching(
      data = matching_test_data,
      matching_variables = c("age", "education", "sector")
    )
  }, error = function(e) {
    expect_true(grepl("Package 'cem' not installed", e$message) ||
               grepl("cem", e$message))
    return("expected_error")
  })
  
  expect_true(is.list(result) || result == "expected_error")
})

# Tests for assess_match_quality() ---------------------------------------

test_that("assess_match_quality works with matching result structure", {
  # Create mock matching result structure
  mock_matching_result <- list(
    matched_data = matching_test_data[sample(.N, 100)],  # Sample of matched data
    match_summary = list(
      method = "nearest",
      total_treated = sum(matching_test_data$is_treated),
      total_control = sum(!matching_test_data$is_treated),
      matched_treated = 20,
      matched_control = 40
    ),
    propensity_scores = data.table(
      row_id = 1:100,
      propensity_score = runif(100, 0.1, 0.9),
      is_treated = sample(c(TRUE, FALSE), 100, replace = TRUE)
    ),
    common_support = list(
      treated_range = c(0.1, 0.9),
      control_range = c(0.1, 0.8),
      overlap_range = c(0.1, 0.8),
      treated_in_support = 18,
      control_in_support = 35
    )
  )
  
  quality_result <- assess_match_quality(mock_matching_result)
  
  expect_type(quality_result, "list")
  expect_true("quality_summary" %in% names(quality_result))
  expect_true("recommendations" %in% names(quality_result))
  
  # Check quality summary structure
  expect_true("overall_quality_score" %in% names(quality_result$quality_summary))
  expect_true("quality_rating" %in% names(quality_result$quality_summary))
  
  # Quality score should be between 0 and 1
  expect_true(quality_result$quality_summary$overall_quality_score >= 0)
  expect_true(quality_result$quality_summary$overall_quality_score <= 1)
})

test_that("assess_match_quality generates appropriate recommendations", {
  # Create mock matching result with poor quality
  poor_matching_result <- list(
    matched_data = matching_test_data[1:20],
    match_summary = list(
      method = "nearest",
      total_treated = 50,
      total_control = 150,
      matched_treated = 10,  # Low retention
      matched_control = 140  # High utilization
    )
  )
  
  quality_result <- assess_match_quality(poor_matching_result)
  
  expect_true(length(quality_result$recommendations) > 0)
  expect_type(quality_result$recommendations, "character")
})

test_that("assess_match_quality error handling", {
  # Test with invalid input
  expect_error(
    assess_match_quality("not a list"),
    "matching_result must be a list object"
  )
  
  # Test with missing required components
  incomplete_result <- list(some_component = "value")
  expect_error(
    assess_match_quality(incomplete_result),
    "matched_data|match_summary"  # Should mention missing components
  )
})

# Integration tests for matching workflow --------------------------------

test_that("Balance assessment workflow integration", {
  # Test that balance assessment integrates properly with mock matching results
  
  # Create balanced treatment assignment for testing
  balanced_data <- copy(matching_test_data)
  
  # Simple balance assessment on original data
  original_balance <- assess_balance(
    matched_data = balanced_data,
    balance_variables = c("age", "prior_wage"),
    thresholds = list(mean_diff = 0.25, variance_ratio = 3)  # Lenient thresholds
  )
  
  expect_s3_class(original_balance$balance_table, "data.table")
  expect_true(original_balance$overall_balance$n_variables == 2)
  
  # Test that we can assess balance on a subset (simulating matched data)
  matched_subset <- balanced_data[sample(.N, 80)]  # Random subset
  
  subset_balance <- assess_balance(
    matched_data = matched_subset,
    balance_variables = c("age", "prior_wage"),
    thresholds = list(mean_diff = 0.25, variance_ratio = 3)
  )
  
  expect_s3_class(subset_balance$balance_table, "data.table")
})

test_that("Match quality assessment handles different matching methods", {
  # Test different mock matching method results
  methods <- c("nearest", "optimal", "genetic", "CEM")
  
  for (method in methods) {
    mock_result <- list(
      matched_data = matching_test_data[sample(.N, 60)],
      match_summary = list(
        method = method,
        total_treated = 50,
        total_control = 150,
        matched_treated = 25,
        matched_control = 50
      )
    )
    
    quality_result <- assess_match_quality(mock_result)
    
    expect_equal(quality_result$quality_summary$key_metrics$matching_method, method)
    expect_type(quality_result$recommendations, "character")
  }
})

# Edge cases and robustness tests ----------------------------------------

test_that("Balance assessment handles edge cases", {
  # Test with perfect balance (all same values)
  perfect_balance_data <- copy(matching_test_data[1:20])
  perfect_balance_data[, age := 35]  # All same age
  perfect_balance_data[, prior_wage := 2000]  # All same wage
  
  balance_result <- assess_balance(
    matched_data = perfect_balance_data,
    balance_variables = c("age", "prior_wage"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  # Should handle zero variance gracefully
  expect_s3_class(balance_result$balance_table, "data.table")
  
  # Test with extreme imbalance
  extreme_data <- copy(matching_test_data[1:20])
  extreme_data[is_treated == TRUE, age := 25]
  extreme_data[is_treated == FALSE, age := 65]
  
  balance_result <- assess_balance(
    matched_data = extreme_data,
    balance_variables = c("age"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  # Should detect imbalance
  expect_true(any(balance_result$balance_table$balance_concern))
})

test_that("Functions handle missing values gracefully", {
  # Add missing values to test data
  missing_data <- copy(matching_test_data)
  missing_data[sample(.N, 10), age := NA]
  missing_data[sample(.N, 5), prior_wage := NA]
  
  # Balance assessment should handle missing values
  balance_result <- assess_balance(
    matched_data = missing_data,
    balance_variables = c("age", "prior_wage"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  expect_s3_class(balance_result$balance_table, "data.table")
  # Should complete without error (exact behavior may vary)
})

test_that("Functions handle small sample sizes", {
  # Test with very small samples
  small_data <- matching_test_data[1:6]  # 6 observations
  small_data[1:2, is_treated := TRUE]    # 2 treated, 4 control
  
  # Balance assessment
  balance_result <- assess_balance(
    matched_data = small_data,
    balance_variables = c("age"),
    thresholds = list(mean_diff = 0.5, variance_ratio = 5)  # Very lenient
  )
  
  expect_s3_class(balance_result$balance_table, "data.table")
  
  # Mock matching quality assessment
  small_mock_result <- list(
    matched_data = small_data,
    match_summary = list(
      method = "nearest",
      total_treated = 2,
      total_control = 4,
      matched_treated = 2,
      matched_control = 4
    )
  )
  
  quality_result <- assess_match_quality(small_mock_result)
  expect_type(quality_result, "list")
})

# Performance tests -------------------------------------------------------

test_that("Balance assessment performs reasonably on larger datasets", {
  # Create larger dataset
  large_n <- 1000
  large_data <- data.table(
    is_treated = sample(c(TRUE, FALSE), large_n, replace = TRUE),
    age = rnorm(large_n, 40, 10),
    wage = exp(rnorm(large_n, log(3000), 0.4)),
    education = sample(c("High School", "Bachelor", "Master"), large_n, replace = TRUE)
  )
  
  # Time the balance assessment
  start_time <- Sys.time()
  
  balance_result <- assess_balance(
    matched_data = large_data,
    balance_variables = c("age", "wage", "education"),
    thresholds = list(mean_diff = 0.1, variance_ratio = 2)
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(end_time - start_time, units = "secs")
  
  expect_s3_class(balance_result$balance_table, "data.table")
  expect_true(duration < 5)  # Should complete within 5 seconds
})

# Cleanup -----------------------------------------------------------------

rm(matching_test_data)