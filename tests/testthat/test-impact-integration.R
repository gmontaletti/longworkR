# Tests for Impact Integration Bridge Functions
# Tests for prepare_metrics_for_impact_analysis() and related functions

library(testthat)
library(data.table)

# Helper function to create test metrics data
create_test_metrics_data <- function() {
  set.seed(123)
  
  # Create individuals with different periods
  individuals <- paste0("ID", sprintf("%03d", 1:30))
  
  # Create some individuals with pre/post and some with control
  data_list <- list()
  
  # Treated individuals with pre/post
  for (i in 1:15) {
    # Pre period
    pre_data <- data.table(
      cf = individuals[i],
      event_period = "pre",
      employment_rate = runif(1, 0.6, 0.8),
      permanent_contract_rate = runif(1, 0.3, 0.5),
      avg_contract_duration = runif(1, 80, 120),
      employment_spells = sample(1:3, 1)
    )
    
    # Post period
    post_data <- data.table(
      cf = individuals[i],
      event_period = "post",
      employment_rate = runif(1, 0.7, 0.9),  # Improvement
      permanent_contract_rate = runif(1, 0.4, 0.6),  # Improvement
      avg_contract_duration = runif(1, 90, 140),  # Improvement
      employment_spells = sample(1:3, 1)
    )
    
    data_list <- c(data_list, list(pre_data, post_data))
  }
  
  # Control individuals with control period
  for (i in 16:30) {
    control_data <- data.table(
      cf = individuals[i],
      event_period = "control",
      employment_rate = runif(1, 0.6, 0.8),
      permanent_contract_rate = runif(1, 0.3, 0.5),
      avg_contract_duration = runif(1, 80, 120),
      employment_spells = sample(1:3, 1)
    )
    
    data_list <- c(data_list, list(control_data))
  }
  
  # Combine all data
  rbindlist(data_list)
}

# Helper function to create treatment assignment
create_test_treatment_assignment <- function() {
  data.table(
    cf = paste0("ID", sprintf("%03d", 1:30)),
    is_treated = c(rep(1, 15), rep(0, 15))  # First 15 are treated
  )
}

# Test basic functionality --------------------------------------------------

test_that("prepare_metrics_for_impact_analysis works with basic inputs", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "did",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true("is_treated" %in% names(result))
  expect_true("post" %in% names(result))
  expect_true("cf" %in% names(result))
  
  # Check that outcome variables were detected
  outcome_vars <- attr(result, "outcome_vars")
  expect_true(length(outcome_vars) > 0)
  expect_true("employment_rate" %in% outcome_vars)
})

test_that("prepare_metrics_for_impact_analysis expands control observations", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "did",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  # Check that control units now have both pre and post observations
  control_periods <- result[is_treated == 0, unique(event_period)]
  expect_true("pre" %in% control_periods)
  expect_true("post" %in% control_periods)
  
  # Check that each control unit has 2 observations
  control_counts <- result[is_treated == 0, .N, by = cf]
  expect_true(all(control_counts$N == 2))
})

test_that("prepare_metrics_for_impact_analysis creates proper panel structure", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "did",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  # Check panel structure
  panel_summary <- result[, .(
    n_periods = .N,
    has_pre = any(post == 0),
    has_post = any(post == 1)
  ), by = .(cf, is_treated)]
  
  # All units should have both periods
  expect_true(all(panel_summary$has_pre))
  expect_true(all(panel_summary$has_post))
  expect_true(all(panel_summary$n_periods == 2))
})

# Test different impact methods ---------------------------------------------

test_that("prepare_metrics_for_impact_analysis works with event_study method", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "event_study",
    period_column = "event_period",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true("event_time" %in% names(result))
  expect_equal(attr(result, "impact_method"), "event_study")
})

test_that("prepare_metrics_for_impact_analysis works with matching method", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "matching",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(attr(result, "impact_method"), "matching")
})

# Test input validation ---------------------------------------------------

test_that("prepare_metrics_for_impact_analysis validates inputs correctly", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  # Test invalid metrics_output type
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = "not_a_datatable",
      treatment_assignment = treatment_data
    ),
    "must be a data.table or list"
  )
  
  # Test invalid treatment_assignment type  
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = "not_a_datatable"
    ),
    "must be a data.table"
  )
  
  # Test missing columns in treatment_assignment
  bad_treatment <- data.table(wrong_id = "test")
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = bad_treatment
    ),
    "Column cf not found in treatment_assignment"
  )
  
  # Test missing is_treated column
  bad_treatment2 <- data.table(cf = "test")
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = bad_treatment2
    ),
    "Column 'is_treated' not found in treatment_assignment"
  )
})

test_that("prepare_metrics_for_impact_analysis handles missing columns", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  # Test missing id column in metrics
  bad_metrics <- copy(metrics_data)
  setnames(bad_metrics, "cf", "wrong_id")
  
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = bad_metrics,
      treatment_assignment = treatment_data
    ),
    "Column cf not found in metrics_output"
  )
  
  # Test missing period column
  bad_metrics2 <- copy(metrics_data)
  setnames(bad_metrics2, "event_period", "wrong_period")
  
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = bad_metrics2,
      treatment_assignment = treatment_data,
      period_column = "event_period"
    ),
    "Column event_period not found in metrics_output"
  )
})

# Test outcome variable detection -----------------------------------------

test_that("prepare_metrics_for_impact_analysis auto-detects outcomes correctly", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    auto_detect_outcomes = TRUE,
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  outcome_vars <- attr(result, "outcome_vars")
  
  # Should detect numeric columns excluding ID and time variables
  expected_outcomes <- c("employment_rate", "permanent_contract_rate", 
                        "avg_contract_duration", "employment_spells")
  expect_true(all(expected_outcomes %in% outcome_vars))
  
  # Should exclude ID and time variables
  expect_false("cf" %in% outcome_vars)
  expect_false("event_period" %in% outcome_vars)
  expect_false("post" %in% outcome_vars)
})

test_that("prepare_metrics_for_impact_analysis respects specified outcomes", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  specified_outcomes <- c("employment_rate", "permanent_contract_rate")
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    outcome_vars = specified_outcomes,
    auto_detect_outcomes = FALSE,
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  outcome_vars <- attr(result, "outcome_vars")
  expect_equal(sort(outcome_vars), sort(specified_outcomes))
})

test_that("prepare_metrics_for_impact_analysis validates outcome variables", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  # Test non-existent outcome variables
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = treatment_data,
      outcome_vars = c("nonexistent_variable"),
      auto_detect_outcomes = FALSE,
      period_column = "event_period"
    ),
    "Outcome variables not found in metrics_output"
  )
})

# Test list input handling ------------------------------------------------

test_that("prepare_metrics_for_impact_analysis handles list input", {
  # Create list format metrics (simulating calculate_comprehensive_impact_metrics list output)
  stability_metrics <- data.table(
    cf = paste0("ID", sprintf("%03d", 1:15)),
    event_period = rep(c("pre", "post"), length.out = 15),
    employment_rate = runif(15, 0.6, 0.9),
    employment_spells = sample(1:3, 15, replace = TRUE)
  )
  
  quality_metrics <- data.table(
    cf = paste0("ID", sprintf("%03d", 1:15)),
    event_period = rep(c("pre", "post"), length.out = 15),
    permanent_contract_rate = runif(15, 0.3, 0.6),
    avg_contract_duration = runif(15, 80, 140)
  )
  
  metrics_list <- list(
    stability = stability_metrics,
    quality = quality_metrics
  )
  
  treatment_data <- data.table(
    cf = paste0("ID", sprintf("%03d", 1:15)),
    is_treated = rep(c(1, 0), length.out = 15)
  )
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_list,
    treatment_assignment = treatment_data,
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  
  # Check that metrics were properly prefixed and merged
  outcome_vars <- attr(result, "outcome_vars")
  expect_true(any(grepl("stability_", outcome_vars)))
  expect_true(any(grepl("quality_", outcome_vars)))
})

# Test integration with difference_in_differences -------------------------

test_that("prepare_metrics_for_impact_analysis output works with difference_in_differences", {
  skip_if_not_installed("fixest")
  
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  prepared_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "did",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  # Test that prepared data can be used with difference_in_differences
  expect_no_error({
    did_result <- difference_in_differences(
      data = prepared_data,
      outcome_vars = c("employment_rate"),
      treatment_var = "is_treated",
      time_var = "post",
      id_var = "cf",
      verbose = FALSE
    )
  })
})

# Test validation function ------------------------------------------------

test_that("validate_integration_setup works correctly", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  prepared_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  # Test successful validation
  expect_true(
    validate_integration_setup(
      data = prepared_data,
      impact_method = "did",
      id_column = "cf",
      verbose = FALSE
    )
  )
  
  # Test validation failure with bad data
  bad_data <- data.table(wrong_structure = 1)
  expect_false(
    validate_integration_setup(
      data = bad_data,
      impact_method = "did",
      id_column = "cf",
      verbose = FALSE
    )
  )
})

# Test edge cases ---------------------------------------------------------

test_that("prepare_metrics_for_impact_analysis handles missing treatment assignments", {
  metrics_data <- create_test_metrics_data()
  
  # Create treatment data with some missing assignments
  incomplete_treatment <- data.table(
    cf = paste0("ID", sprintf("%03d", 1:20)),  # Missing last 10
    is_treated = c(rep(1, 10), rep(0, 10))
  )
  
  expect_warning(
    result <- prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = incomplete_treatment,
      period_column = "event_period",
      verbose = FALSE
    ),
    "Missing treatment assignment"
  )
  
  expect_s3_class(result, "data.table")
})

test_that("prepare_metrics_for_impact_analysis handles empty outcome detection", {
  # Create data with no numeric columns (except IDs)
  non_numeric_data <- data.table(
    cf = paste0("ID", 1:10),
    event_period = rep(c("pre", "post"), 5),
    category = rep(c("A", "B"), 5)
  )
  
  treatment_data <- data.table(
    cf = paste0("ID", 1:5),
    is_treated = c(1, 1, 0, 0, 0)
  )
  
  expect_error(
    prepare_metrics_for_impact_analysis(
      metrics_output = non_numeric_data,
      treatment_assignment = treatment_data,
      auto_detect_outcomes = TRUE,
      period_column = "event_period"
    ),
    "No outcome variables specified or detected"
  )
})

# Test attributes preservation ---------------------------------------------

test_that("prepare_metrics_for_impact_analysis sets proper attributes", {
  metrics_data <- create_test_metrics_data()
  treatment_data <- create_test_treatment_assignment()
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    impact_method = "did",
    period_column = "event_period",  # Test data uses event_period
    verbose = FALSE
  )
  
  # Check attributes
  expect_equal(attr(result, "impact_method"), "did")
  expect_equal(attr(result, "id_column"), "cf")
  expect_equal(attr(result, "period_column"), "event_period")
  expect_true(length(attr(result, "outcome_vars")) > 0)
})

# Test custom column names ------------------------------------------------

test_that("prepare_metrics_for_impact_analysis works with custom column names", {
  # Create data with custom column names
  custom_metrics <- data.table(
    person_id = paste0("P", 1:20),
    time_period = rep(c("before", "after"), 10),
    metric1 = runif(20),
    metric2 = runif(20)
  )
  
  custom_treatment <- data.table(
    person_id = paste0("P", 1:10),
    is_treated = rep(c(1, 0), 5)
  )
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = custom_metrics,
    treatment_assignment = custom_treatment,
    id_column = "person_id",
    period_column = "time_period",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true("person_id" %in% names(result))
  expect_equal(attr(result, "id_column"), "person_id")
  expect_equal(attr(result, "period_column"), "time_period")
})

# Test cartesian join fix with event-level treatment data ----------------

test_that("prepare_metrics_for_impact_analysis prevents cartesian joins with event-level treatment data", {
  
  # Create metrics data (person-period level)
  metrics_data <- data.table(
    cf = rep(paste0("ID", 1:20), each = 2),
    period = rep(c("pre", "post"), 20),
    employment_rate = runif(40, 0.5, 0.9),
    contract_quality = runif(40, 0.3, 0.7)
  )
  
  # Create event-level treatment assignment (the problematic case)
  # This simulates what propensity_score_matching() returns
  event_counts <- rep(3:4, length.out = 20)  # Each person has 3-4 events
  person_ids <- rep(paste0("ID", 1:20), event_counts)
  
  treatment_events <- data.table(
    cf = person_ids,
    event_id = seq_along(person_ids),
    is_treated = sample(c(0, 1), length(person_ids), replace = TRUE),
    # Event-level characteristics (should be excluded)
    propensity_score = runif(length(person_ids), 0.1, 0.9),
    event_period = sample(c("pre", "post"), length(person_ids), replace = TRUE),
    # Person-level characteristics (should be preserved)
    baseline_score = ave(runif(length(person_ids), 0.2, 0.8), person_ids, FUN = function(x) x[1])
  )
  
  # Verify the problematic setup exists
  expect_gt(nrow(treatment_events), uniqueN(treatment_events$cf))
  
  original_metrics_rows <- nrow(metrics_data)
  original_persons <- uniqueN(metrics_data$cf)
  
  # This should work without cartesian join thanks to the fix
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_events,
    impact_method = "did",
    verbose = FALSE
  )
  
  # Validate no cartesian join occurred
  expect_equal(nrow(result), original_metrics_rows, 
               info = "Row count should not increase (no cartesian join)")
  
  expect_equal(uniqueN(result$cf), original_persons,
               info = "Person count should be preserved")
  
  # Check that deduplication worked correctly
  expect_true("is_treated" %in% names(result))
  expect_true("baseline_score" %in% names(result))
  expect_false("propensity_score" %in% names(result))
  expect_false("event_id" %in% names(result))
  
  # Check panel structure
  panel_check <- result[, .N, by = cf]
  expect_true(all(panel_check$N <= 2), 
              info = "Each person should have at most 2 observations (pre/post)")
  
  # Verify attributes are set
  expect_equal(attr(result, "impact_method"), "did")
  expect_true(length(attr(result, "outcome_vars")) > 0)
})

test_that("prepare_metrics_for_impact_analysis preserves person-level covariates while excluding event-level data", {
  
  # Setup with mixed person-level and event-level data
  metrics_data <- data.table(
    cf = rep(paste0("ID", 1:10), each = 2),
    period = rep(c("pre", "post"), 10),
    outcome1 = runif(20),
    outcome2 = runif(20)
  )
  
  event_counts2 <- rep(2:4, length.out = 10)  # Each person has 2-4 events
  person_ids2 <- rep(paste0("ID", 1:10), event_counts2)
  
  treatment_data <- data.table(
    cf = person_ids2,
    is_treated = sample(c(0, 1), length(person_ids2), replace = TRUE),
    # Person-level: should be preserved
    age = ave(sample(25:65, length(person_ids2), replace = TRUE), person_ids2, FUN = function(x) x[1]),
    education = ave(sample(c("HS", "College", "Graduate"), length(person_ids2), replace = TRUE), person_ids2, FUN = function(x) x[1]),
    # Event-level: should be excluded  
    job_spell_id = seq_along(person_ids2),
    contract_start = sample(as.Date("2020-01-01") + 0:730, length(person_ids2), replace = TRUE)
  )
  
  result <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_data,
    treatment_assignment = treatment_data,
    verbose = FALSE
  )
  
  # Check person-level covariates are preserved
  expect_true("age" %in% names(result))
  expect_true("education" %in% names(result))
  
  # Check event-level variables are excluded
  expect_false("job_spell_id" %in% names(result))
  expect_false("contract_start" %in% names(result))
  
  # Verify one observation per person per period
  person_period_counts <- result[, .N, by = .(cf, period)]
  expect_true(all(person_period_counts$N == 1),
              info = "Should have exactly one observation per person-period")
})

test_that("prepare_metrics_for_impact_analysis handles inconsistent treatment status correctly", {
  
  metrics_data <- data.table(
    cf = rep(paste0("ID", 1:5), each = 2),
    period = rep(c("pre", "post"), 5),
    metric1 = runif(10)
  )
  
  # Create treatment data with inconsistent status within persons
  treatment_data <- data.table(
    cf = rep(paste0("ID", 1:5), each = 3),
    is_treated = c(1, 0, 1,  # ID1: inconsistent
                   0, 0, 0,  # ID2: consistent
                   1, 1, 1,  # ID3: consistent  
                   0, 1, 0,  # ID4: inconsistent
                   1, 1, 1), # ID5: consistent
    baseline_var = rep(1:5, each = 3)
  )
  
  # Should work but warn about inconsistencies
  expect_warning(
    result <- prepare_metrics_for_impact_analysis(
      metrics_output = metrics_data,
      treatment_assignment = treatment_data,
      verbose = FALSE
    ),
    "inconsistent treatment status"
  )
  
  # Should still produce valid result
  expect_s3_class(result, "data.table")
  expect_equal(uniqueN(result$cf), 5)
  
  # Treatment status should be resolved (first occurrence used)
  expect_false(any(is.na(result$is_treated)))
})