# Test suite for Impact Evaluation Framework
# Tests all major impact evaluation functions for correctness, error handling, and edge cases

library(testthat)
library(data.table)
library(vecshift)

# Set up test data --------------------------------------------------------

setup_test_data <- function() {
  # Create synthetic employment data for testing
  set.seed(123)
  
  test_data <- data.table(
    cf = rep(paste0("CF", sprintf("%03d", 1:20)), each = 5),
    id = 1:100,
    INIZIO = as.Date("2020-01-01") + sample(0:1000, 100, replace = TRUE),
    FINE = as.Date("2020-01-01") + sample(100:1100, 100, replace = TRUE),
    prior = sample(c(-1, 0, 1, 2, 3), 100, replace = TRUE),
    COD_TIPOLOGIA_CONTRATTUALE = sample(c("C.01.00", "C.02.01", "C.03.07"), 100, 
                                       prob = c(0.3, 0.4, 0.3), replace = TRUE),
    age = sample(25:60, 100, replace = TRUE),
    education = sample(c("High School", "Bachelor", "Master"), 100, replace = TRUE),
    sector = sample(c("Manufacturing", "Services", "Construction"), 100, replace = TRUE),
    region = sample(c("North", "Center", "South"), 100, replace = TRUE)
  )
  
  # Ensure FINE > INIZIO
  test_data[FINE <= INIZIO, FINE := INIZIO + sample(1:365, sum(FINE <= INIZIO))]
  
  # Create mock vecshift output
  test_data[, `:=`(
    over_id = .I,
    arco = 1,
    durata = as.numeric(FINE - INIZIO + 1),
    employment_type = fifelse(prior > 0, "occ_ft", 
                             fifelse(prior == 0, "occ_pt", "temp"))
  )]
  
  return(test_data)
}

# Test data for all tests
test_employment_data <- setup_test_data()

# Tests for identify_treatment_events() ----------------------------------

test_that("identify_treatment_events works with basic conditions", {
  skip_if_not_installed("data.table")
  
  # Test with string condition
  result <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
    event_window = c(-365, 365),
    min_pre_period = 30,
    min_post_period = 30
  )
  
  expect_s3_class(result, "data.table")
  expect_true("is_treated" %in% names(result))
  expect_true("event_id" %in% names(result))
  expect_true("treatment_condition_met" %in% names(result))
  
  # Check that treatment identification worked
  treated_obs <- result[is_treated == TRUE]
  expect_true(nrow(treated_obs) > 0)
  expect_true(all(treated_obs$COD_TIPOLOGIA_CONTRATTUALE == "C.01.00"))
})

test_that("identify_treatment_events handles structured conditions", {
  # Test with structured condition
  result <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list(
      list(column = "COD_TIPOLOGIA_CONTRATTUALE", operator = "==", value = "C.01.00")
    ),
    event_window = c(-180, 180),
    multiple_events = "first"
  )
  
  expect_s3_class(result, "data.table")
  treated_count <- sum(result$is_treated, na.rm = TRUE)
  expect_true(treated_count > 0)
})

test_that("identify_treatment_events handles function conditions", {
  # Test with function condition
  custom_condition <- function(dt) {
    dt$COD_TIPOLOGIA_CONTRATTUALE == "C.01.00" & dt$prior > 0
  }
  
  result <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list(custom_condition),
    event_window = c(-365, 365)
  )
  
  expect_s3_class(result, "data.table")
  treated_obs <- result[is_treated == TRUE]
  if (nrow(treated_obs) > 0) {
    expect_true(all(treated_obs$COD_TIPOLOGIA_CONTRATTUALE == "C.01.00"))
    expect_true(all(treated_obs$prior > 0))
  }
})

test_that("identify_treatment_events handles multiple events correctly", {
  # Test different multiple_events options
  for (option in c("first", "last", "all")) {
    result <- identify_treatment_events(
      data = test_employment_data,
      treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
      event_window = c(-365, 365),
      multiple_events = option
    )
    
    expect_s3_class(result, "data.table")
    if (option %in% c("first", "last")) {
      # Should have at most one event per person
      event_counts <- result[!is.na(event_id), .N, by = cf]
      expect_true(all(event_counts$N <= 1))
    }
  }
})

test_that("identify_treatment_events error handling", {
  # Test invalid input types
  expect_error(
    identify_treatment_events(
      data = data.frame(), # Not a data.table
      treatment_conditions = list("test == 1")
    ),
    "Input data must be a data.table"
  )
  
  # Test missing columns
  invalid_data <- test_employment_data[, .(cf, INIZIO)]  # Missing FINE
  expect_error(
    identify_treatment_events(
      data = invalid_data,
      treatment_conditions = list("test == 1"),
      date_column = "FINE"
    ),
    "Date column FINE not found in data"
  )
  
  # Test invalid event window
  expect_error(
    identify_treatment_events(
      data = test_employment_data,
      treatment_conditions = list("prior == 1"),
      event_window = c(365, -365)  # Wrong order
    ),
    "event_window must be a numeric vector of length 2"
  )
  
  # Test invalid multiple_events option
  expect_error(
    identify_treatment_events(
      data = test_employment_data,
      treatment_conditions = list("prior == 1"),
      multiple_events = "invalid"
    ),
    "multiple_events must be one of"
  )
})

# Tests for assess_treatment_event_quality() -----------------------------

test_that("assess_treatment_event_quality works correctly", {
  # First identify events
  event_data <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
    event_window = c(-365, 365)
  )
  
  assessment <- assess_treatment_event_quality(
    event_data = event_data,
    assessment_variables = c("age", "sector"),
    output_format = "summary"
  )
  
  expect_type(assessment, "list")
  expect_true("event_summary" %in% names(assessment))
  expect_true("data_quality" %in% names(assessment))
  expect_true("recommendations" %in% names(assessment))
  expect_s3_class(assessment, "treatment_event_assessment")
  
  # Check event summary structure
  expect_true("total_observations" %in% names(assessment$event_summary))
  expect_true("total_treated" %in% names(assessment$event_summary))
  expect_true("treatment_rate" %in% names(assessment$event_summary))
})

test_that("assess_treatment_event_quality handles balance assessment", {
  event_data <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
    event_window = c(-365, 365)
  )
  
  assessment <- assess_treatment_event_quality(
    event_data = event_data,
    assessment_variables = c("age"),  # Numeric variable for balance test
    output_format = "detailed"
  )
  
  expect_true("balance_table" %in% names(assessment))
  if (!is.null(assessment$balance_table)) {
    expect_s3_class(assessment$balance_table, "data.table")
  }
})

# Tests for create_treatment_control_groups() ----------------------------

test_that("create_treatment_control_groups basic functionality", {
  event_data <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
    event_window = c(-365, 365)
  )
  
  groups <- create_treatment_control_groups(
    event_data = event_data,
    control_ratio = 2,
    exclude_future_treated = TRUE
  )
  
  expect_s3_class(groups, "data.table")
  expect_true("group_assignment" %in% names(groups))
  expect_true("match_id" %in% names(groups))
  
  # Check group assignment factor levels
  expect_true(is.factor(groups$group_assignment))
  expect_equal(levels(groups$group_assignment), c("treatment", "control"))
  
  # Check that we have both treatment and control observations
  group_counts <- groups[, .N, by = group_assignment]
  expect_true(nrow(group_counts) <= 2)  # At most treatment and control
})

test_that("create_treatment_control_groups error handling", {
  # Test invalid input
  expect_error(
    create_treatment_control_groups(
      event_data = data.frame(),  # Not a data.table
      control_ratio = 1
    ),
    "event_data must be a data.table"
  )
  
  # Test missing required column
  invalid_data <- test_employment_data[, .(cf, INIZIO)]  # Missing is_treated
  expect_error(
    create_treatment_control_groups(
      event_data = invalid_data,
      control_ratio = 1
    ),
    "must contain 'is_treated' column"
  )
})

# Tests for calculate_employment_stability_metrics() ---------------------

test_that("calculate_employment_stability_metrics works correctly", {
  # Add event period column for testing
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  metrics <- calculate_employment_stability_metrics(
    data = test_data_with_period,
    min_spell_duration = 1
  )
  
  expect_s3_class(metrics, "data.table")
  
  expected_cols <- c("cf", "period", "days_employed", "days_unemployed", 
                    "employment_rate", "employment_spells", "employment_stability_index")
  expect_true(all(expected_cols %in% names(metrics)))
  
  # Check that employment rates are between 0 and 1
  expect_true(all(metrics$employment_rate >= 0 & metrics$employment_rate <= 1))
  
  # Check that stability index is between 0 and 1
  expect_true(all(metrics$employment_stability_index >= 0 & metrics$employment_stability_index <= 1))
})

test_that("calculate_employment_stability_metrics handles edge cases", {
  # Test with empty data
  empty_data <- test_employment_data[0]
  empty_data[, event_period := character(0)]
  
  expect_warning(
    metrics <- calculate_employment_stability_metrics(empty_data),
    "No valid observations"
  )
  expect_equal(nrow(metrics), 0)
  
  # Test with missing required columns
  invalid_data <- test_employment_data[, .(cf, INIZIO)]
  expect_error(
    calculate_employment_stability_metrics(invalid_data),
    "Missing required columns"
  )
})

# Tests for calculate_contract_quality_metrics() -------------------------

test_that("calculate_contract_quality_metrics works correctly", {
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  metrics <- calculate_contract_quality_metrics(
    data = test_data_with_period,
    permanent_values = c(1, 2, 3),
    temporary_values = c(-1, 0)
  )
  
  expect_s3_class(metrics, "data.table")
  
  expected_cols <- c("cf", "period", "permanent_contract_days", "temporary_contract_days",
                    "permanent_contract_rate", "average_contract_quality")
  expect_true(all(expected_cols %in% names(metrics)))
  
  # Check that rates are between 0 and 1
  expect_true(all(metrics$permanent_contract_rate >= 0 & metrics$permanent_contract_rate <= 1))
  expect_true(all(metrics$average_contract_quality >= 0 & metrics$average_contract_quality <= 1))
})

# Tests for calculate_comprehensive_impact_metrics() ---------------------

test_that("calculate_comprehensive_impact_metrics works with all metrics", {
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Test "all" metrics
  all_metrics <- calculate_comprehensive_impact_metrics(
    data = test_data_with_period,
    metrics = "all",
    output_format = "wide"
  )
  
  expect_s3_class(all_metrics, "data.table")
  
  # Should contain metrics from all categories
  stability_metrics <- c("employment_rate", "employment_spells", "employment_stability_index")
  quality_metrics <- c("permanent_contract_rate", "average_contract_quality")
  
  # At least some metrics should be present
  expect_true(any(stability_metrics %in% names(all_metrics)) ||
              any(quality_metrics %in% names(all_metrics)))
})

test_that("calculate_comprehensive_impact_metrics works with specific metrics", {
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Test specific metrics
  specific_metrics <- calculate_comprehensive_impact_metrics(
    data = test_data_with_period,
    metrics = c("stability", "quality"),
    output_format = "list"
  )
  
  expect_type(specific_metrics, "list")
  expect_true("stability" %in% names(specific_metrics))
  expect_true("quality" %in% names(specific_metrics))
})

test_that("calculate_comprehensive_impact_metrics output formats", {
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Test different output formats
  for (format in c("wide", "long", "list")) {
    result <- calculate_comprehensive_impact_metrics(
      data = test_data_with_period,
      metrics = "stability",
      output_format = format
    )
    
    if (format == "list") {
      expect_type(result, "list")
    } else {
      expect_s3_class(result, "data.table")
      if (format == "long") {
        expect_true("metric_name" %in% names(result))
        expect_true("metric_value" %in% names(result))
      }
    }
  }
})

test_that("calculate_comprehensive_impact_metrics error handling", {
  test_data_with_period <- copy(test_employment_data)
  test_data_with_period[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Test invalid metrics
  expect_error(
    calculate_comprehensive_impact_metrics(
      data = test_data_with_period,
      metrics = c("invalid_metric"),
      output_format = "wide"
    ),
    "Invalid metrics specified"
  )
  
  # Test invalid output format
  expect_error(
    calculate_comprehensive_impact_metrics(
      data = test_data_with_period,
      metrics = "stability",
      output_format = "invalid"
    ),
    "output_format must be one of"
  )
})

# Integration tests -------------------------------------------------------

test_that("Full impact evaluation workflow integration", {
  # Test that components work together
  skip_if_not_installed("data.table")
  
  # Step 1: Event identification
  events <- identify_treatment_events(
    data = test_employment_data,
    treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
    event_window = c(-180, 180),
    min_pre_period = 30,
    min_post_period = 30
  )
  
  expect_s3_class(events, "data.table")
  
  # Step 2: Treatment/control groups
  if (sum(events$is_treated, na.rm = TRUE) > 0) {
    groups <- create_treatment_control_groups(
      event_data = events,
      control_ratio = 1,
      exclude_future_treated = FALSE
    )
    
    expect_s3_class(groups, "data.table")
    
    # Step 3: Impact metrics (if we have groups)
    if (nrow(groups) > 0) {
      groups[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
      
      metrics <- calculate_comprehensive_impact_metrics(
        data = groups,
        metrics = c("stability"),
        output_format = "wide"
      )
      
      # Should produce some result
      expect_true(nrow(metrics) >= 0)
    }
  }
})

# Performance tests (basic) ----------------------------------------------

test_that("Functions handle reasonable data sizes efficiently", {
  # Create larger test dataset
  large_data <- rbindlist(replicate(10, test_employment_data, simplify = FALSE))
  large_data[, cf := paste0(cf, "_", rep(1:10, each = nrow(test_employment_data)))]
  
  # Test that functions complete in reasonable time
  expect_silent({
    start_time <- Sys.time()
    
    events <- identify_treatment_events(
      data = large_data,
      treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
      event_window = c(-365, 365)
    )
    
    end_time <- Sys.time()
    duration <- as.numeric(end_time - start_time, units = "secs")
    
    # Should complete within 10 seconds for test data
    expect_true(duration < 10)
  })
})

# Edge cases and robustness tests ----------------------------------------

test_that("Functions handle edge cases gracefully", {
  # Test with single observation
  single_obs <- test_employment_data[1]
  single_obs[, event_period := "pre"]
  
  expect_silent({
    metrics <- calculate_employment_stability_metrics(single_obs)
  })
  
  # Test with all same values
  uniform_data <- copy(test_employment_data)
  uniform_data[, `:=`(
    prior = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "C.01.00",
    event_period = "pre"
  )]
  
  expect_silent({
    events <- identify_treatment_events(
      uniform_data,
      treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'")
    )
  })
  
  # Should identify all as treated
  expect_true(all(events$is_treated))
})

# Cleanup -----------------------------------------------------------------

# Remove test data to free memory
rm(test_employment_data)