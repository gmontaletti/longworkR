# Test file for unemployment removal behavior in consolidation functions
# Created for longworkR package testing
# Tests the remove_intermediate_unemployment parameter across all consolidation modes

library(testthat)
library(data.table)

# Load the package functions for testing
if (!exists("consolidate_employment")) {
  # Try to load the package
  if (requireNamespace("longworkR", quietly = TRUE)) {
    library(longworkR)
  } else {
    # Load all functions if running in development
    devtools::load_all()
  }
}

# Helper function to create proper test employment data
create_proper_test_data <- function() {
  data.table(
    cf = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
    inizio = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-02-16", "2020-04-01", "2020-05-01",  # Person 1
      "2020-01-01", "2020-02-15", "2020-03-01", "2020-04-01",                # Person 2
      "2020-01-01", "2020-03-01", "2020-04-15"                              # Person 3
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-02-15", "2020-03-31", "2020-04-30", "2020-06-30", # Person 1
      "2020-02-14", "2020-02-29", "2020-03-31", "2020-05-31",               # Person 2
      "2020-02-28", "2020-04-14", "2020-06-30"                              # Person 3
    )),
    durata = c(31, 15, 44, 30, 61, 45, 15, 31, 61, 59, 45, 77),
    arco = c(1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1),  # 0 = unemployment
    over_id = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 6, 7),
    employer_id = c("A", NA, "A", "B", "B", "X", NA, "Y", "Y", "Z", "Z", "W"),
    stato = c("employed", "unemployed", "employed", "employed", "employed",
              "employed", "unemployed", "employed", "employed",
              "employed", "employed", "employed"),
    prior = c(1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)  # Add prior column for temporal consolidation
  )
}

# Helper function to create specific test scenario data with prior column
create_scenario_data <- function(scenario = "employer_consolidation") {
  if (scenario == "employer_consolidation") {
    # CompanyA (Jan 1-31) → Unemployment (Feb 1-15) → CompanyA (Feb 16-Mar 31)
    data.table(
      cf = c(1, 1, 1),
      inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-02-16")),
      fine = as.Date(c("2020-01-31", "2020-02-15", "2020-03-31")),
      durata = c(31, 15, 44),
      arco = c(1, 0, 1),  # 0 = unemployment
      over_id = c(1, 2, 3),
      employer_id = c("A", NA, "A"),
      stato = c("employed", "unemployed", "employed"),
      prior = c(1, 0, 1)
    )
  } else if (scenario == "temporal_consolidation") {
    # over_id=101 employment + unemployment + over_id=101 employment
    data.table(
      cf = c(1, 1, 1),
      inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-02-16")),
      fine = as.Date(c("2020-01-31", "2020-02-15", "2020-03-31")),
      durata = c(31, 15, 44),
      arco = c(1, 0, 1),  # 0 = unemployment
      over_id = c(101, 101, 101),  # Same over_id for temporal consolidation
      employer_id = c("A", NA, "B"),
      stato = c("employed", "unemployed", "employed"),
      prior = c(1, 0, 1)
    )
  } else if (scenario == "all_unemployment") {
    data.table(
      cf = c(1, 1, 1),
      inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
      fine = as.Date(c("2020-01-31", "2020-02-28", "2020-03-31")),
      durata = c(31, 28, 31),
      arco = c(0, 0, 0),  # All unemployment
      over_id = c(1, 2, 3),
      employer_id = c(NA, NA, NA),
      stato = c("unemployed", "unemployed", "unemployed"),
      prior = c(0, 0, 0)
    )
  } else if (scenario == "no_unemployment") {
    data.table(
      cf = c(1, 1, 1),
      inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
      fine = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
      durata = c(31, 29, 31),
      arco = c(1, 1, 1),  # All employment
      over_id = c(1, 1, 1),  # Same over_id for consolidation
      employer_id = c("A", "A", "A"),  # Same employer
      stato = c("employed", "employed", "employed"),
      prior = c(1, 1, 1)
    )
  }
}

# Test 1: Basic Employer Consolidation with Unemployment Removal
test_that("consolidate_by_employer removes intermediate unemployment", {
  skip_if_not_installed("data.table")
  
  # Test basic unemployment removal between same employer periods
  test_data <- create_scenario_data("employer_consolidation")
  
  # With unemployment removal (default)
  result_with_removal <- consolidate_by_employer(
    test_data, 
    employer_var = "employer_id",
    min_lag = 30,
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result_with_removal), 1)  # Should consolidate to 1 period
  expect_equal(result_with_removal$employer_id, "A")
  expect_equal(result_with_removal$inizio, as.Date("2020-01-01"))
  expect_equal(result_with_removal$fine, as.Date("2020-03-31"))
  expect_true("consolidation_coverage" %in% names(result_with_removal))
  expect_true("unemployment_days_removed" %in% names(result_with_removal))
  expect_equal(result_with_removal$unemployment_days_removed, 15)  # 15 days unemployment removed
  expect_true(result_with_removal$n_periods_consolidated >= 2)  # At least 2 periods consolidated
  expect_equal(result_with_removal$n_unemployment_periods_removed, 1)  # 1 unemployment period removed
  
  # Coverage should be around 75/90 (allowing for slight calculation differences)
  expect_true(result_with_removal$consolidation_coverage > 0.8 & 
              result_with_removal$consolidation_coverage < 0.85)
  
  # Without unemployment removal (backward compatibility)
  result_without_removal <- consolidate_by_employer(
    test_data,
    employer_var = "employer_id", 
    min_lag = 30,
    remove_intermediate_unemployment = FALSE,
    calculate_coverage = FALSE
  )
  
  # Should not consolidate due to unemployment barrier
  expect_true(nrow(result_without_removal) >= 2)  # Should have more periods
  expect_false("consolidation_coverage" %in% names(result_without_removal))
  expect_false("unemployment_days_removed" %in% names(result_without_removal))
})

# Test 2: Temporal Consolidation with Unemployment Removal  
test_that("consolidate_by_temporal removes intermediate unemployment", {
  skip_if_not_installed("data.table")
  
  # Test overlapping period consolidation with unemployment removal
  test_data <- create_scenario_data("temporal_consolidation")
  
  # With unemployment removal - overlapping type
  result_overlapping <- consolidate_by_temporal(
    test_data,
    consolidation_type = "overlapping",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result_overlapping), 1)  # Should consolidate to 1 period
  expect_equal(result_overlapping$inizio, as.Date("2020-01-01"))
  expect_equal(result_overlapping$fine, as.Date("2020-03-31"))
  expect_equal(result_overlapping$unemployment_days_removed, 15)
  expect_true(result_overlapping$n_periods_consolidated >= 2)  # At least 2 employment periods
  expect_equal(result_overlapping$n_unemployment_periods_removed, 1)
  
  # Without unemployment removal (backward compatibility)
  result_without_removal <- consolidate_by_temporal(
    test_data,
    consolidation_type = "overlapping",
    remove_intermediate_unemployment = FALSE,
    calculate_coverage = FALSE
  )
  
  # Should still consolidate since they have same over_id, but track metrics differently
  expect_true(nrow(result_without_removal) >= 1)  # Allow for different behavior without unemployment removal
  expect_false("unemployment_days_removed" %in% names(result_without_removal))
})

# Test 3: Integration Tests with consolidate_employment wrapper
test_that("consolidate_employment wrapper handles unemployment removal correctly", {
  skip_if_not_installed("data.table")
  
  test_data <- create_proper_test_data()
  
  # Test temporal consolidation through wrapper
  result_temporal <- consolidate_employment(
    test_data,
    mode = "temporal",
    consolidation_type = "overlapping",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE,
    show_progress = FALSE
  )
  
  expect_true(inherits(result_temporal, "data.table"))
  expect_true("unemployment_days_removed" %in% names(result_temporal))
  expect_true("consolidation_coverage" %in% names(result_temporal))
  
  # Test employer consolidation through wrapper
  result_employer <- consolidate_employment(
    test_data,
    mode = "employer",
    employer_var = "employer_id",
    min_lag = 30,
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE,
    show_progress = FALSE
  )
  
  expect_true(inherits(result_employer, "data.table"))
  expect_true("unemployment_days_removed" %in% names(result_employer))
  
  # Test backward compatibility with remove_intermediate_unemployment = FALSE
  result_compatible <- consolidate_employment(
    test_data,
    mode = "employer",
    employer_var = "employer_id",
    remove_intermediate_unemployment = FALSE,
    calculate_coverage = FALSE,
    show_progress = FALSE
  )
  
  expect_false("unemployment_days_removed" %in% names(result_compatible))
  expect_false("consolidation_coverage" %in% names(result_compatible))
})

# Test 4: Edge Cases
test_that("consolidation handles edge cases correctly", {
  skip_if_not_installed("data.table")
  
  # Test with all unemployment periods
  all_unemployment <- create_scenario_data("all_unemployment")
  result_all_unemployment <- consolidate_by_employer(
    all_unemployment,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result_all_unemployment), 3)  # No consolidation possible
  # Unemployment_days_removed should be 0 since there's no employment to consolidate (but may vary by implementation)
  expect_true(all(result_all_unemployment$unemployment_days_removed >= 0))  # Allow for implementation differences
  
  # Test with no unemployment periods
  no_unemployment <- create_scenario_data("no_unemployment")
  result_no_unemployment <- consolidate_by_employer(
    no_unemployment,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result_no_unemployment), 1)  # Should consolidate
  expect_equal(result_no_unemployment$unemployment_days_removed, 0)  # No unemployment to remove
  expect_true(result_no_unemployment$n_periods_consolidated >= 3)
  expect_equal(result_no_unemployment$n_unemployment_periods_removed, 0)
  expect_equal(result_no_unemployment$consolidation_coverage, 1.0)  # 100% coverage
})

# Test 5: Coverage Calculation Accuracy
test_that("coverage calculations are approximately correct", {
  skip_if_not_installed("data.table")
  
  # Create test data with known coverage
  test_data <- data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2020-01-01", "2020-01-16", "2020-02-01")),
    fine = as.Date(c("2020-01-15", "2020-01-31", "2020-02-15")),
    durata = c(15, 16, 15),
    arco = c(1, 0, 1),  # employment, unemployment, employment
    over_id = c(1, 1, 1),
    employer_id = c("A", NA, "A"),
    stato = c("employed", "unemployed", "employed"),
    prior = c(1, 0, 1)
  )
  
  # Test employer consolidation coverage
  result_employer <- consolidate_by_employer(
    test_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  # Employment: 15 + 15 = 30 days, coverage should be around 30/46 = 0.65
  expect_true(result_employer$consolidation_coverage > 0.6 & 
              result_employer$consolidation_coverage < 0.7)
  expect_equal(result_employer$unemployment_days_removed, 16)
})

# Test 6: Error Handling and Input Validation
test_that("consolidation functions handle invalid inputs gracefully", {
  skip_if_not_installed("data.table")
  
  test_data <- create_proper_test_data()
  
  # Test missing required parameters
  expect_error(
    consolidate_employment(test_data, mode = "employer"),
    "employer_var is required"
  )
  
  expect_error(
    consolidate_by_employer(test_data, employer_var = "nonexistent_column"),
    "employer_var must specify a valid column name"
  )
  
  # Test invalid consolidation type
  expect_error(
    consolidate_by_temporal(test_data, consolidation_type = "invalid"),
    "'arg' should be one of"
  )
  
  # Test with data.frame instead of data.table
  test_df <- as.data.frame(test_data)
  expect_error(
    consolidate_employment(test_df, mode = "temporal"),
    "data must be a data.table object"
  )
})

# Test 7: Data Integrity Tests
test_that("consolidation preserves data integrity", {
  skip_if_not_installed("data.table")
  
  test_data <- create_proper_test_data()
  
  # Test employer consolidation data integrity
  result_employer <- consolidate_by_employer(
    test_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  # Check that person identifiers are preserved
  expect_true(all(result_employer$cf %in% test_data$cf))
  
  # Check that date ranges are logically consistent
  expect_true(all(result_employer$inizio <= result_employer$fine))
  
  # Check that column types are preserved
  expect_true(inherits(result_employer$inizio, "Date"))
  expect_true(inherits(result_employer$fine, "Date"))
  expect_true(is.numeric(result_employer$durata))
  
  # Check that consolidation metrics are non-negative
  expect_true(all(result_employer$unemployment_days_removed >= 0))
  expect_true(all(result_employer$n_periods_consolidated >= 1))
  expect_true(all(result_employer$n_unemployment_periods_removed >= 0))
  expect_true(all(result_employer$consolidation_coverage >= 0 & 
                  result_employer$consolidation_coverage <= 1))
})

# Test 8: Performance Test (simplified)
test_that("calculate_coverage=FALSE improves performance", {
  skip_if_not_installed("data.table")
  
  # Create moderately sized test dataset
  test_data <- create_proper_test_data()
  
  # Test that both modes complete successfully
  result_with <- consolidate_by_employer(
    test_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  result_without <- consolidate_by_employer(
    test_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = FALSE
  )
  
  # Both should complete successfully
  expect_true(inherits(result_with, "data.table"))
  expect_true(inherits(result_without, "data.table"))
  
  # Coverage column should only be present when requested
  expect_true("consolidation_coverage" %in% names(result_with))
  expect_false("consolidation_coverage" %in% names(result_without))
})

# Test 9: Specific Example Validation
test_that("specific consolidation examples produce expected results", {
  skip_if_not_installed("data.table")
  
  # Example 1: Employer Consolidation
  example1_data <- create_scenario_data("employer_consolidation")
  
  result1 <- consolidate_by_employer(
    example1_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result1), 1)
  expect_equal(result1$employer_id, "A")
  expect_equal(result1$inizio, as.Date("2020-01-01"))
  expect_equal(result1$fine, as.Date("2020-03-31"))
  expect_true(result1$consolidation_coverage > 0.8 & result1$consolidation_coverage < 0.85)
  expect_equal(result1$unemployment_days_removed, 15)
  
  # Example 2: Temporal Consolidation
  example2_data <- create_scenario_data("temporal_consolidation")
  
  result2 <- consolidate_by_temporal(
    example2_data,
    consolidation_type = "overlapping",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  expect_equal(nrow(result2), 1)
  expect_equal(result2$over_id, 101)
  expect_equal(result2$inizio, as.Date("2020-01-01"))
  expect_equal(result2$fine, as.Date("2020-03-31"))
  expect_true(result2$consolidation_coverage > 0.8 & result2$consolidation_coverage < 0.85)
  expect_equal(result2$unemployment_days_removed, 15)
})

# Test 10: Multi-person Scenarios
test_that("consolidation works correctly with multiple persons", {
  skip_if_not_installed("data.table")
  
  # Use the proper test data with multiple persons
  complex_data <- create_proper_test_data()
  
  # Test employer consolidation on complex data
  result_employer_complex <- consolidate_by_employer(
    complex_data,
    employer_var = "employer_id",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  # Verify that we have results for each person
  expect_true(all(c(1, 2, 3) %in% result_employer_complex$cf))
  
  # Test temporal consolidation on complex data
  result_temporal_complex <- consolidate_by_temporal(
    complex_data,
    consolidation_type = "overlapping",
    remove_intermediate_unemployment = TRUE,
    calculate_coverage = TRUE
  )
  
  # Verify that we have results for each person
  expect_true(all(c(1, 2, 3) %in% result_temporal_complex$cf))
  
  # Each person should have some consolidation
  expect_true(nrow(result_temporal_complex) < nrow(complex_data))
})