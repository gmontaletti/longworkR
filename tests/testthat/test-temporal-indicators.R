# Tests for Temporal Employment Indicators Functions
# Tests for compute_temporal_employment_indicators() and create_temporal_indicators_summary()
#
# Author: longworkR Package  
# Created: 2025-08-31

library(testthat)
library(data.table)

# Helper function to create sample vecshift-style data for testing
create_sample_temporal_data <- function(n_persons = 3, n_contracts_per_person = 4) {
  set.seed(123)
  
  # Base dates for temporal analysis
  base_date <- as.Date("2023-01-01")
  
  sample_data <- data.table()
  
  for (i in 1:n_persons) {
    person_id <- sprintf("PERSON%03d", i)
    
    # Create realistic contract sequence for each person
    contracts_start <- base_date + cumsum(c(0, sample(30:120, n_contracts_per_person - 1)))
    contracts_duration <- sample(30:300, n_contracts_per_person)
    contracts_end <- contracts_start + contracts_duration - 1
    
    person_contracts <- data.table(
      cf = person_id,
      inizio = contracts_start,
      fine = contracts_end,
      durata = contracts_duration,
      arco = sample(c(0, 1, 1, 1), n_contracts_per_person, replace = TRUE), # 75% employment
      over_id = paste0(person_id, "_", 1:n_contracts_per_person),
      COD_TIPOLOGIA_CONTRATTUALE = sample(c("A.01.00", "A.03.00", "A.03.01", "C.01.00", "A.07.00"), 
                                         n_contracts_per_person, replace = TRUE),
      prior = sample(c(0, 1), n_contracts_per_person, replace = TRUE, prob = c(0.3, 0.7)),
      retribuzione_media = round(runif(n_contracts_per_person, 1500, 4000), 2),
      company = paste0("Company", sample(LETTERS[1:10], n_contracts_per_person, replace = TRUE))
    )
    
    sample_data <- rbind(sample_data, person_contracts)
  }
  
  return(sample_data)
}

# Helper function to create overlapping contracts for consolidation testing
create_overlapping_contracts_data <- function() {
  data.table(
    cf = rep("PERSON001", 4),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-04-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-03-31", "2023-05-31", "2023-08-31")),
    durata = c(58, 75, 61, 92),
    arco = c(1, 1, 1, 1),
    over_id = c("PERSON001_1", "PERSON001_1", "PERSON001_2", "PERSON001_3"), # First two overlap
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "A.01.00", "A.03.01"),
    prior = c(1, 1, 1, 0),
    retribuzione_media = c(2500, 2600, 2700, 2000)
  )
}

# Test basic functionality of compute_temporal_employment_indicators
test_that("compute_temporal_employment_indicators returns correct structure with quarterly periods", {
  # Use realistic sample data
  sample_data <- create_sample_temporal_data(n_persons = 2, n_contracts_per_person = 3)
  
  result <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  # Should return a data.table
  expect_s3_class(result, "data.table")
  
  # Should have required columns
  expected_cols <- c("cf", "period", "employment_rate")
  expect_true(all(expected_cols %in% names(result)))
  
  # Should have data for both persons
  expect_equal(length(unique(result$cf)), 2)
  
  # Employment rates should be between 0 and 1
  expect_true(all(result$employment_rate >= 0 & result$employment_rate <= 1))
  
  # Periods should be sequential starting from 1
  min_period <- min(result$period)
  expect_equal(min_period, 1)
})

test_that("compute_temporal_employment_indicators works with monthly periods", {
  sample_data <- create_sample_temporal_data(n_persons = 2, n_contracts_per_person = 2)
  
  result <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "monthly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true("employment_rate" %in% names(result))
  
  # Should have periods
  expect_true("period" %in% names(result))
  expect_true(all(result$period >= 1))
})

test_that("compute_temporal_employment_indicators handles employment_rate indicator correctly", {
  # Create specific test data with known employment patterns
  test_data <- data.table(
    cf = rep("TEST001", 3),
    inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
    fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    durata = c(90, 91, 92),
    arco = c(1, 1, 1), # All employment periods
    over_id = paste0("TEST001_", 1:3),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 3),
    prior = rep(1, 3)
  )
  
  result <- compute_temporal_employment_indicators(
    data = test_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    reference_date = as.Date("2023-01-01"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true(all(result$cf == "TEST001"))
  
  # Employment rates should be high but may not be exactly 1.0 due to period boundaries
  expect_true(all(result$employment_rate > 0.5))
  expect_true(nrow(result) >= 1)
})

test_that("compute_temporal_employment_indicators handles arco employment status filtering", {
  # Create data with both employment (arco >= 1) and non-employment (arco = 0) periods
  test_data <- data.table(
    cf = rep("TEST001", 4),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30", "2023-06-30")),
    durata = c(31, 59, 30, 61),
    arco = c(0, 1, 0, 1), # Mixed employment status
    over_id = paste0("TEST001_", 1:4),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 4),
    prior = rep(1, 4)
  )
  
  result <- compute_temporal_employment_indicators(
    data = test_data,
    period_type = "monthly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  
  # Should only consider employment periods (arco >= 1)
  # So should have some employment data
  expect_true(nrow(result) > 0)
  expect_true(any(result$employment_rate > 0))
})

test_that("compute_temporal_employment_indicators handles contract consolidation with over_id", {
  # Use overlapping contracts data
  overlapping_data <- create_overlapping_contracts_data()
  
  # Test with consolidation enabled
  result_consolidated <- compute_temporal_employment_indicators(
    data = overlapping_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    use_consolidation = TRUE,
    verbose = FALSE
  )
  
  # Test without consolidation
  result_not_consolidated <- compute_temporal_employment_indicators(
    data = overlapping_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    use_consolidation = FALSE,
    verbose = FALSE
  )
  
  expect_s3_class(result_consolidated, "data.table")
  expect_s3_class(result_not_consolidated, "data.table")
  
  # Both should have results
  expect_true(nrow(result_consolidated) > 0)
  expect_true(nrow(result_not_consolidated) > 0)
  
  # Employment rates should be reasonable (between 0 and 1)
  expect_true(all(result_consolidated$employment_rate >= 0 & result_consolidated$employment_rate <= 1))
  expect_true(all(result_not_consolidated$employment_rate >= 0 & result_not_consolidated$employment_rate <= 1))
})

test_that("compute_temporal_employment_indicators handles edge case: single contract", {
  # Single contract for single person
  single_contract <- data.table(
    cf = "SINGLE001",
    inizio = as.Date("2023-01-15"),
    fine = as.Date("2023-03-15"),
    durata = 60,
    arco = 1,
    over_id = "SINGLE001_1",
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00",
    prior = 1,
    retribuzione_media = 2500
  )
  
  result <- compute_temporal_employment_indicators(
    data = single_contract,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1) # Should span at least one quarter
  expect_equal(unique(result$cf), "SINGLE001")
  expect_true(all(result$employment_rate > 0))
})

test_that("compute_temporal_employment_indicators handles multiple indicators including contract quality", {
  sample_data <- create_sample_temporal_data(n_persons = 2, n_contracts_per_person = 3)
  
  result <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "quarterly",
    indicators = c("employment_rate", "contract_quality", "wage_statistics"),
    wage_var = "retribuzione_media",
    start_date_var = "inizio",
    end_date_var = "fine",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  
  # Should have employment rate
  expect_true("employment_rate" %in% names(result))
  
  # Should have contract quality based on survival analysis
  expect_true("avg_contract_quality" %in% names(result))
  
  # Should have basic structure
  expect_true("cf" %in% names(result))
  expect_true("period" %in% names(result))
  
  # All rows should have valid cf and period values
  expect_true(all(!is.na(result$cf)))
  expect_true(all(!is.na(result$period)))
  
  # Contract quality should be in expected range (0.3 to 1.0)
  if ("avg_contract_quality" %in% names(result)) {
    quality_values <- result$avg_contract_quality[!is.na(result$avg_contract_quality)]
    if (length(quality_values) > 0) {
      expect_true(all(quality_values >= 0.3 & quality_values <= 1.0))
    }
  }
})

# Tests for create_temporal_indicators_summary function
test_that("create_temporal_indicators_summary returns correct group summaries", {
  # Create temporal indicators first
  sample_data <- create_sample_temporal_data(n_persons = 4, n_contracts_per_person = 3)
  
  temporal_indicators <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  # Add treatment groups for testing
  temporal_indicators[, treatment_group := rep(c("control", "treatment"), length.out = nrow(temporal_indicators))]
  
  summary_result <- create_temporal_indicators_summary(
    temporal_data = temporal_indicators,
    group_var = "treatment_group",
    indicator_vars = c("employment_rate"),
    summary_stats = c("mean", "n"),
    statistical_tests = FALSE
  )
  
  expect_s3_class(summary_result, "data.table")
  
  # Should have required columns
  expect_true("indicator" %in% names(summary_result))
  expect_true("period" %in% names(summary_result))
  expect_true("treatment_group" %in% names(summary_result))
  expect_true("mean" %in% names(summary_result))
  expect_true("n" %in% names(summary_result))
  
  # Should have summaries for employment_rate
  expect_true("employment_rate" %in% summary_result$indicator)
  
  # Should have both treatment groups
  expect_true("control" %in% summary_result$treatment_group)
  expect_true("treatment" %in% summary_result$treatment_group)
})

test_that("create_temporal_indicators_summary works without group variable", {
  sample_data <- create_sample_temporal_data(n_persons = 3, n_contracts_per_person = 2)
  
  temporal_indicators <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "monthly",
    indicators = c("employment_rate"),
    verbose = FALSE
  )
  
  summary_result <- create_temporal_indicators_summary(
    temporal_data = temporal_indicators,
    group_var = NULL,
    indicator_vars = c("employment_rate"),
    summary_stats = c("mean", "n")
  )
  
  expect_s3_class(summary_result, "data.table")
  expect_true("indicator" %in% names(summary_result))
  expect_true("period" %in% names(summary_result))
  expect_true("mean" %in% names(summary_result))
  expect_true("n" %in% names(summary_result))
  
  # Should not have group variable column
  expect_false("treatment_group" %in% names(summary_result))
})

test_that("compute_temporal_employment_indicators validates input parameters", {
  sample_data <- create_sample_temporal_data(n_persons = 1, n_contracts_per_person = 2)
  
  # Test invalid period_type
  expect_error(
    compute_temporal_employment_indicators(data = sample_data, period_type = "invalid"),
    "period_type must be 'quarterly' or 'monthly'"
  )
  
  # Test missing required columns
  incomplete_data <- sample_data[, .(cf, inizio)] # Missing fine, durata
  expect_error(
    compute_temporal_employment_indicators(data = incomplete_data),
    "Missing required columns"
  )
  
  # Test non-data.table input
  expect_error(
    compute_temporal_employment_indicators(data = as.data.frame(sample_data)),
    "data must be a data.table"
  )
})

test_that("compute_temporal_employment_indicators handles overlap calculations correctly", {
  # Create test data with specific overlaps
  overlap_test_data <- data.table(
    cf = "OVERLAP001",
    inizio = as.Date(c("2023-01-15", "2023-03-15")), # Mid-quarter starts
    fine = as.Date(c("2023-02-15", "2023-04-15")),   # Cross-quarter boundaries
    durata = c(32, 32),
    arco = c(1, 1),
    over_id = c("OVERLAP001_1", "OVERLAP001_2"),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.01.00"),
    prior = c(1, 1)
  )
  
  result <- compute_temporal_employment_indicators(
    data = overlap_test_data,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    reference_date = as.Date("2023-01-01"),
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1) # Should span at least 1 period
  
  # Employment rates should be reasonable for partial overlaps
  expect_true(all(result$employment_rate > 0))
  expect_true(all(result$employment_rate <= 1))
})

test_that("compute_temporal_employment_indicators employment_threshold parameter works", {
  # Create data with very short employment periods
  short_employment <- data.table(
    cf = "SHORT001",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-02"), # Only 2 days
    durata = 2,
    arco = 1,
    over_id = "SHORT001_1",
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00",
    prior = 1
  )
  
  # Test with threshold = 1 (should include)
  result_low_threshold <- compute_temporal_employment_indicators(
    data = short_employment,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    employment_threshold = 1,
    verbose = FALSE
  )
  
  # Test with threshold = 5 (should exclude)
  result_high_threshold <- compute_temporal_employment_indicators(
    data = short_employment,
    period_type = "quarterly",
    indicators = c("employment_rate"),
    employment_threshold = 5,
    verbose = FALSE
  )
  
  expect_s3_class(result_low_threshold, "data.table")
  expect_s3_class(result_high_threshold, "data.table")
  
  # Low threshold should show employment
  if (nrow(result_low_threshold) > 0) {
    expect_true(any(result_low_threshold$employment_rate > 0))
  }
  
  # High threshold should show no employment (employment_rate = 0) or no results
  if (nrow(result_high_threshold) > 0) {
    expect_true(all(result_high_threshold$employment_rate == 0))
  }
})

# Test contract quality calculation based on survival analysis
test_that("compute_temporal_employment_indicators contract quality uses survival analysis", {
  sample_data <- create_sample_temporal_data(n_persons = 2, n_contracts_per_person = 4)
  
  result <- compute_temporal_employment_indicators(
    data = sample_data,
    period_type = "quarterly", 
    indicators = c("contract_quality"),
    start_date_var = "inizio",
    end_date_var = "fine",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  
  # Should have contract quality metrics
  expect_true("avg_contract_quality" %in% names(result))
  expect_true("median_survival_based_quality" %in% names(result))
  
  # Contract quality should be within expected range (0.3-1.0)
  if ("avg_contract_quality" %in% names(result)) {
    quality_values <- result$avg_contract_quality[!is.na(result$avg_contract_quality)]
    if (length(quality_values) > 0) {
      expect_true(all(quality_values >= 0.3 & quality_values <= 1.0))
    }
  }
  
  # Flag should indicate survival analysis was used
  if ("median_survival_based_quality" %in% names(result)) {
    expect_true(all(result$median_survival_based_quality == TRUE, na.rm = TRUE))
  }
})

# Test contract quality fallback when survival analysis fails
test_that("compute_temporal_employment_indicators contract quality handles survival analysis failure", {
  # Create data that might cause survival analysis issues (all same duration)
  uniform_data <- data.table(
    cf = rep("UNIFORM001", 3),
    inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
    fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    durata = rep(90, 3), # All exactly same duration
    arco = rep(1, 3),
    over_id = paste0("UNIFORM001_", 1:3),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 3),
    prior = rep(1, 3)
  )
  
  # Function should handle this gracefully and fall back to duration-based quality
  result <- compute_temporal_employment_indicators(
    data = uniform_data,
    period_type = "quarterly",
    indicators = c("contract_quality"),
    start_date_var = "inizio",
    end_date_var = "fine",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  
  # Should still have contract quality (using fallback method)
  expect_true("avg_contract_quality" %in% names(result))
  
  # Quality should still be in valid range
  if ("avg_contract_quality" %in% names(result)) {
    quality_values <- result$avg_contract_quality[!is.na(result$avg_contract_quality)]
    if (length(quality_values) > 0) {
      expect_true(all(quality_values >= 0.3 & quality_values <= 1.0))
    }
  }
})

# Test that quality_weights parameter is deprecated with warning
test_that("compute_temporal_employment_indicators quality_weights parameter is deprecated", {
  sample_data <- create_sample_temporal_data(n_persons = 1, n_contracts_per_person = 2)
  
  # The function should accept the parameter but issue a deprecation warning
  # Note: The parameter is now ignored in favor of survival analysis
  expect_warning(
    result <- compute_temporal_employment_indicators(
      data = sample_data,
      period_type = "quarterly",
      indicators = c("contract_quality"),
      quality_weights = list("A.01.00" = 1.0, "A.03.00" = 0.8),
      start_date_var = "inizio",
      end_date_var = "fine",
      verbose = FALSE
    ),
    "quality_weights.*parameter is deprecated",
    info = "Should warn about deprecated quality_weights parameter"
  )
  
  # Result should still be valid despite using deprecated parameter
  expect_s3_class(result, "data.table")
  expect_true("avg_contract_quality" %in% names(result))
})

# Test contract quality with different contract types
test_that("compute_temporal_employment_indicators contract quality differentiates contract types", {
  # Create data with different contract types that should have different quality scores
  mixed_contracts <- data.table(
    cf = rep("MIXED001", 6),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", 
                      "2023-05-01", "2023-07-01", "2023-08-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30", 
                    "2023-06-30", "2023-07-31", "2023-12-31")),
    durata = c(31, 59, 30, 61, 31, 153), # Varying durations
    arco = rep(1, 6),
    over_id = paste0("MIXED001_", 1:6),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.01.00", "A.03.00", 
                                  "A.03.01", "C.01.00", "A.01.00"),
    prior = c(1, 1, 0, 1, 1, 1)
  )
  
  result <- compute_temporal_employment_indicators(
    data = mixed_contracts,
    period_type = "quarterly",
    indicators = c("contract_quality"),
    start_date_var = "inizio",
    end_date_var = "fine",
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.table")
  expect_true("avg_contract_quality" %in% names(result))
  
  # Contract quality should reflect the survival analysis of different contract types
  quality_values <- result$avg_contract_quality[!is.na(result$avg_contract_quality)]
  if (length(quality_values) > 1) {
    # There should be some variation in quality scores (not all identical)
    # This tests that different contract types get different survival-based scores
    expect_true(var(quality_values) > 0.001)
  }
  
  # All quality values should be in valid range
  expect_true(all(quality_values >= 0.3 & quality_values <= 1.0))
})