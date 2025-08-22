# Tests for prepare_metrics_for_impact_analysis() Fixes
# Validates that the 5862 → 5860 person count discrepancy issue is resolved
# Tests person count preservation, enhanced debugging, and robustness improvements

library(data.table)

test_that("prepare_metrics_for_impact_analysis preserves person count with problematic data", {
  
  # Load sample data and create test scenario
  sample_data <- readRDS(file.path("..", "..", "data", "sample.rds"))
  sample_data <- as.data.table(sample_data)
  
  # Create a manageable test subset
  set.seed(42)
  test_persons <- sample(unique(sample_data$cf), 500)
  test_data <- sample_data[cf %in% test_persons]
  
  # Add event periods (simulate pre/post intervention scenario)
  test_data[, event_date := as.Date("2020-01-01")]
  test_data[, event_period := ifelse(inizio < event_date, "pre", "post")]
  
  # Add some control observations
  control_persons <- sample(test_persons, 50)
  test_data[cf %in% control_persons, event_period := "control"]
  
  # Calculate comprehensive metrics
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = test_data,
    metrics = c("stability", "quality"),
    output_format = "wide",
    period_column = "event_period"
  )
  
  original_person_count <- uniqueN(metrics_result$cf)
  
  # Create problematic treatment assignment (event-level with inconsistencies)
  # This is the type of data that caused the original 5862 → 5860 person loss
  problematic_treatment <- test_data[, .(
    cf,
    inizio,
    over_id,
    is_treated = sample(c(0, 1), .N, replace = TRUE)
  )]
  
  # Add inconsistencies that previously caused person loss
  inconsistent_rows <- sample(nrow(problematic_treatment), floor(nrow(problematic_treatment) * 0.03))
  problematic_treatment[inconsistent_rows, is_treated := 1 - is_treated]
  
  # Add some NA values
  na_rows <- sample(nrow(problematic_treatment), floor(nrow(problematic_treatment) * 0.01))
  problematic_treatment[na_rows, is_treated := NA_real_]
  
  # Test the fixed function
  expect_warning(
    result <- prepare_metrics_for_impact_analysis(
      metrics_output = metrics_result,
      treatment_assignment = problematic_treatment,
      impact_method = "did",
      verbose = FALSE
    ),
    "inconsistent treatment status"
  )
  
  # Main assertion: Person count should be preserved
  final_person_count <- uniqueN(result$cf)
  expect_equal(final_person_count, original_person_count,
               label = "Person count should be preserved (5862 → 5860 issue fixed)")
  
  # Additional quality checks
  expect_true(inherits(result, "data.table"))
  expect_true("cf" %in% names(result))
  expect_true("is_treated" %in% names(result))
  expect_true("post" %in% names(result))
  
  # Check that data structure is valid for DiD
  expect_true(all(result$post %in% c(0, 1, NA)))
  expect_true(all(result$is_treated %in% c(0, 1, NA)))
})

test_that("prepare_metrics_for_impact_analysis handles clean data correctly", {
  
  # Load sample data and create test scenario
  sample_data <- readRDS(file.path("..", "..", "data", "sample.rds"))
  sample_data <- as.data.table(sample_data)
  
  # Create test subset
  set.seed(123)
  test_persons <- sample(unique(sample_data$cf), 200)
  test_data <- sample_data[cf %in% test_persons]
  
  # Add event periods
  test_data[, event_period := sample(c("pre", "post"), nrow(test_data), replace = TRUE)]
  
  # Calculate metrics
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = test_data,
    metrics = "stability",
    output_format = "wide",
    period_column = "event_period"
  )
  
  original_person_count <- uniqueN(metrics_result$cf)
  
  # Create clean treatment assignment (person-level, no inconsistencies)
  clean_treatment <- data.table(
    cf = unique(test_data$cf),
    is_treated = sample(c(0, 1), uniqueN(test_data$cf), replace = TRUE)
  )
  
  # Test with clean data - should work perfectly
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = clean_treatment,
    impact_method = "did",
    verbose = FALSE
  )
  
  # Assertions for clean data
  final_person_count <- uniqueN(result$cf)
  expect_equal(final_person_count, original_person_count)
  expect_equal(sum(is.na(result$is_treated)), 0, 
               label = "Clean data should have no missing treatment assignments")
})

test_that("prepare_metrics_for_impact_analysis integration with DiD works", {
  
  skip_on_cran()  # Skip integration test on CRAN
  
  # Create minimal test data
  sample_data <- readRDS(file.path("..", "..", "data", "sample.rds"))
  sample_data <- as.data.table(sample_data)
  
  # Small test subset for integration
  set.seed(456)
  test_persons <- sample(unique(sample_data$cf), 100)
  test_data <- sample_data[cf %in% test_persons]
  test_data[, event_period := sample(c("pre", "post"), nrow(test_data), replace = TRUE)]
  
  # Calculate metrics
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = test_data,
    metrics = "stability",
    output_format = "wide",
    period_column = "event_period"
  )
  
  # Clean treatment assignment
  treatment_data <- data.table(
    cf = unique(test_data$cf),
    is_treated = sample(c(0, 1), uniqueN(test_data$cf), replace = TRUE)
  )
  
  # Prepare for DiD
  did_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "did",
    verbose = FALSE
  )
  
  # Test integration with difference_in_differences
  expect_no_error(
    did_results <- difference_in_differences(
      data = did_data,
      outcome_vars = "employment_rate",
      treatment_var = "is_treated",
      time_var = "post",
      id_var = "cf"
    )
  )
  
  # Basic checks on DiD results
  expect_true(inherits(did_results, "data.table"))
  expect_true(nrow(did_results) > 0)
})

test_that("prepare_metrics_for_impact_analysis enhanced debugging provides useful information", {
  
  # Create scenario that triggers debugging output
  sample_data <- readRDS(file.path("..", "..", "data", "sample.rds"))
  sample_data <- as.data.table(sample_data)
  
  set.seed(789)
  test_persons <- sample(unique(sample_data$cf), 50)  # Small for testing
  test_data <- sample_data[cf %in% test_persons]
  test_data[, event_period := sample(c("pre", "post", "control"), nrow(test_data), replace = TRUE)]
  
  # Calculate metrics
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = test_data,
    metrics = "stability",
    output_format = "wide",
    period_column = "event_period"
  )
  
  # Create treatment data with event-level structure (triggers debugging)
  treatment_data <- test_data[, .(cf, inizio, is_treated = sample(c(0, 1), .N, replace = TRUE))]
  
  # Capture output to check debugging information is present
  output <- capture.output(
    result <- prepare_metrics_for_impact_analysis(
      metrics_output = metrics_result,
      treatment_assignment = treatment_data,
      impact_method = "did",
      verbose = TRUE
    ),
    type = "output"
  )
  
  # Check that debugging information is provided
  debugging_keywords <- c("DEDUPLICATION", "MERGING DATA", "VALIDATION", 
                         "COLUMN SELECTION", "PERSON COUNT")
  
  has_debugging <- any(sapply(debugging_keywords, function(keyword) {
    any(grepl(keyword, output, ignore.case = TRUE))
  }))
  
  expect_true(has_debugging, 
              label = "Enhanced debugging should provide detailed processing information")
  
  # Should still preserve person count despite complex processing
  expect_equal(uniqueN(result$cf), uniqueN(metrics_result$cf))
})

test_that("prepare_metrics_for_impact_analysis handles edge cases robustly", {
  
  # Test with minimal data
  minimal_data <- data.table::data.table(
    cf = 1:5,
    period = rep(c("pre", "post"), c(2, 3)),
    employment_rate = runif(5)
  )
  
  minimal_treatment <- data.table::data.table(
    cf = 1:5,
    is_treated = c(0, 0, 1, 1, 1)
  )
  
  # Should handle minimal data without errors
  expect_no_error(
    result <- prepare_metrics_for_impact_analysis(
      metrics_output = minimal_data,
      treatment_assignment = minimal_treatment,
      impact_method = "did",
      verbose = FALSE
    )
  )
  
  expect_equal(uniqueN(result$cf), 5)
})