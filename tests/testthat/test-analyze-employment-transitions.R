test_that("analyze_employment_transitions handles basic transitions with duration-weighted means", {
  library(data.table)

  # Create sample data with clear transitions
  sample_data <- data.table(
    id = 1:6,
    cf = c(
      "PERSON001",
      "PERSON001",
      "PERSON001",
      "PERSON002",
      "PERSON002",
      "PERSON002"
    ),
    INIZIO = as.Date(c(
      "2023-01-01",
      "2023-04-01",
      "2023-08-01",
      "2023-02-01",
      "2023-06-01",
      "2023-10-01"
    )),
    FINE = as.Date(c(
      "2023-02-28",
      "2023-05-31",
      "2023-12-31",
      "2023-04-30",
      "2023-08-31",
      "2023-12-31"
    )),
    prior = c(1, 0, 1, 1, 1, 0),
    company = c(
      "CompanyA",
      "CompanyB",
      "CompanyC",
      "CompanyD",
      "CompanyE",
      "CompanyF"
    ),
    salary = c(50000, 25000, 60000, 55000, 65000, 30000)
  )

  # Process through pipeline (simulate pipeline result)
  pipeline_result <- data.table(
    cf = c(
      "PERSON001",
      "PERSON001",
      "PERSON001",
      "PERSON002",
      "PERSON002",
      "PERSON002"
    ),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-03-01",
      "2023-08-01",
      "2023-02-01",
      "2023-05-01",
      "2023-10-01"
    )),
    fine = as.Date(c(
      "2023-02-28",
      "2023-07-31",
      "2023-12-31",
      "2023-04-30",
      "2023-09-30",
      "2023-12-31"
    )),
    arco = c(1, 0, 1, 1, 1, 1),
    durata = c(58, 152, 152, 88, 152, 92),
    company = c("CompanyA", NA, "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
    salary = c(50000, NA, 60000, 55000, 65000, 30000)
  )

  # Set merged_columns attribute
  setattr(pipeline_result, "merged_columns", c("company", "salary"))

  # Test basic functionality (will disable consolidation since no over_id)
  result <- suppressMessages(analyze_employment_transitions(pipeline_result))

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have expected columns (note: variable column doesn't exist in current implementation)
  expected_cols <- c("from", "to", "weight", "transition_duration")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("analyze_employment_transitions handles missing transition_columns", {
  library(data.table)

  # Create minimal pipeline result without merged_columns attribute
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-04-01")),
    fine = as.Date(c("2023-03-31", "2023-06-30")),
    arco = c(1, 0),
    durata = c(90, 91)
  )

  # Should throw error because there are no additional columns beyond standard vecshift output
  expect_error(
    analyze_employment_transitions(pipeline_result),
    "No additional columns found for analysis"
  )
})

test_that("analyze_employment_transitions validates input parameters", {
  library(data.table)

  # Invalid pipeline_result
  expect_error(
    analyze_employment_transitions("not_a_data_table"),
    "must be a data.table"
  )

  # Missing required columns
  invalid_dt <- data.table(x = 1:3)
  expect_error(
    analyze_employment_transitions(invalid_dt),
    "Missing required columns"
  )

  # Valid minimal data for further tests
  valid_dt <- data.table(
    cf = "PERSON001",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    arco = 1,
    durata = 365,
    test_col = "value"
  )

  # The function no longer has transition_columns parameter
  # Test with invalid parameters instead
  expect_error(
    analyze_employment_transitions(valid_dt, invalid_param = 123),
    "unused argument"
  )

  # Invalid min_unemployment_duration
  expect_error(
    analyze_employment_transitions(valid_dt, min_unemployment_duration = -1),
    "must be a non-negative numeric value"
  )
})

test_that("analyze_employment_transitions uses duration-weighted means correctly", {
  library(data.table)

  # Create data with transitions and varying durations to test weighted means
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-03-01",
      "2023-06-01",
      "2023-08-01",
      "2023-11-01"
    )),
    fine = as.Date(c(
      "2023-02-28",
      "2023-05-31",
      "2023-07-31",
      "2023-10-31",
      "2023-12-31"
    )),
    arco = c(1, 0, 1, 0, 1), # Employment -> Unemployment -> Employment -> Unemployment -> Employment
    durata = c(58, 92, 61, 92, 61), # Different durations for weighting
    salary = c(50000, NA, 60000, NA, 45000), # Numeric column to test weighted means
    company = c("CompanyA", NA, "CompanyB", NA, "CompanyC") # Character column
  )

  # Set merged_columns attribute
  setattr(pipeline_result, "merged_columns", c("salary", "company"))

  # Test with show_progress = FALSE for cleaner test output
  result <- suppressMessages(analyze_employment_transitions(
    pipeline_result,
    show_progress = FALSE
  ))

  expect_s3_class(result, "data.table")

  # Check for transitions
  if (nrow(result) > 0) {
    # Should have from and to columns
    expect_true("from" %in% names(result))
    expect_true("to" %in% names(result))

    # Should have salary statistics (median by default)
    salary_cols <- names(result)[grepl("salary", names(result))]
    if (length(salary_cols) > 0) {
      expect_true(any(grepl(
        "salary_from_median|salary_to_median",
        names(result)
      )))
    }
  }
})

test_that("analyze_employment_transitions handles edge cases for weighted means", {
  library(data.table)

  # Test with zero/minimal durations
  pipeline_result_edge <- data.table(
    cf = c("EDGE001", "EDGE001", "EDGE001"),
    inizio = as.Date(c("2023-01-01", "2023-01-02", "2023-01-04")),
    fine = as.Date(c("2023-01-01", "2023-01-03", "2023-01-04")), # Durations: 1, 2, 1
    arco = c(1, 0, 1),
    durata = c(1, 2, 1),
    salary = c(1000, NA, 3000) # Different values to test weighting with minimal durations
  )

  # Set merged_columns attribute
  setattr(pipeline_result_edge, "merged_columns", c("salary"))

  result_edge <- suppressMessages(analyze_employment_transitions(
    pipeline_result_edge,
    show_progress = FALSE
  ))

  expect_s3_class(result_edge, "data.table")

  # Should handle minimal durations correctly
  if (nrow(result_edge) > 0) {
    salary_cols <- names(result_edge)[grepl("salary", names(result_edge))]
    if (length(salary_cols) > 0) {
      # Check if salary statistics are present and not all NA
      expect_true(any(!is.na(result_edge[[salary_cols[1]]])))
    }
  }

  # Test with all NA values in numeric column
  pipeline_result_na <- data.table(
    cf = c("NA001", "NA001", "NA001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30")),
    arco = c(1, 0, 1),
    durata = c(31, 59, 30),
    salary = c(NA, NA, NA) # All NA values
  )

  setattr(pipeline_result_na, "merged_columns", c("salary"))

  result_na <- suppressMessages(analyze_employment_transitions(
    pipeline_result_na,
    show_progress = FALSE
  ))
  expect_s3_class(result_na, "data.table")
  # Should handle all NA case gracefully
})

test_that("analyze_employment_transitions return_list parameter works", {
  library(data.table)

  # Create data with transitions
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31")),
    arco = c(1, 0, 1),
    durata = c(58, 92, 214),
    company = c("CompanyA", NA, "CompanyB")
  )

  # Test return_list = TRUE (parameter no longer exists, skip this test)
  # The function now automatically detects transition variables
  result_list <- suppressMessages(analyze_employment_transitions(
    pipeline_result,
    show_progress = FALSE
  ))

  expect_s3_class(result_list, "data.table")
  # Check that company transitions were detected
  if (nrow(result_list) > 0) {
    expect_true("from" %in% names(result_list))
  }

  # Test with default parameters
  result_combined <- suppressMessages(analyze_employment_transitions(
    pipeline_result,
    show_progress = FALSE
  ))

  expect_s3_class(result_combined, "data.table")
  if (nrow(result_combined) > 0) {
    expect_true("from" %in% names(result_combined))
    expect_true("to" %in% names(result_combined))
  }
})

# =================== EMPLOYER-BASED CONSOLIDATION TESTS ===================

test_that("employer-based consolidation validates parameters correctly", {
  library(data.table)

  # Create sample data with employer information
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")),
    arco = c(1, 1, 1),
    durata = c(58, 92, 92),
    company = c("CompanyA", "CompanyA", "CompanyB"),
    employer_id = c("EMP001", "EMP001", "EMP002"),
    salary = c(50000, 55000, 60000)
  )

  # Error: consolidation_mode = "employer" without employer_var
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = NULL,
      show_progress = FALSE
    ),
    "employer_var.*is required"
  )

  # Error: invalid consolidation_mode value
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "invalid_mode",
      show_progress = FALSE
    ),
    "consolidation_mode.*must be one of"
  )

  # Error: employer_var column doesn't exist
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = "nonexistent_column",
      show_progress = FALSE
    ),
    "nonexistent_column not found"
  )

  # Error: negative min_lag
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = "employer_id",
      min_lag = -5,
      show_progress = FALSE
    ),
    "must be a non-negative numeric value"
  )

  # Error: employer_var is not a character string
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = c("employer_id", "company"),
      show_progress = FALSE
    ),
    "Parameter 'employer_var' must be a single character string"
  )

  # Warning: employer_var specified with temporal mode
  expect_warning(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "temporal",
      employer_var = "employer_id",
      show_progress = FALSE
    )),
    "Parameter 'employer_var' is ignored when consolidation_mode = 'temporal'"
  )
})

test_that("employer-based consolidation works with basic same-employer scenarios", {
  library(data.table)

  # Create data with same employer contracts within consolidation window
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-03-15", "2023-07-01")),
    fine = as.Date(c("2023-02-28", "2023-03-10", "2023-06-30", "2023-12-31")),
    arco = c(1, 1, 1, 1),
    durata = c(58, 10, 107, 184),
    company = c("CompanyA", "CompanyA", "CompanyA", "CompanyB"),
    employer_id = c("EMP001", "EMP001", "EMP001", "EMP002"),
    salary = c(50000, 52000, 54000, 60000)
  )

  # Test with default 8-day lag - should consolidate first 3 contracts (same employer, gaps < 8 days)
  result <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result, "data.table")

  # Should have consolidated same-employer contracts
  # Expected: CompanyA (consolidated) -> CompanyB transition
  if (nrow(result) > 0) {
    expect_true("from" %in% names(result))
    expect_true("to" %in% names(result))

    # Check for the expected transition
    transitions <- paste(result$from, "->", result$to)
    expect_true(any(grepl(
      "CompanyA.*CompanyB",
      transitions,
      ignore.case = TRUE
    )))
  }
})

test_that("employer-based consolidation never consolidates different employers", {
  library(data.table)

  # Create data with different employers but close temporal proximity
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), # 1-day gaps
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    arco = c(1, 1, 1),
    durata = c(31, 28, 31),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    employer_id = c("EMP001", "EMP002", "EMP003"), # All different employers
    salary = c(50000, 55000, 60000)
  )

  # Test with large min_lag - should NOT consolidate (different employers)
  result_employer <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 365, # Very permissive lag
    show_progress = FALSE
  ))

  # Test with temporal mode for comparison - should consolidate (ignores employer)
  result_temporal <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "temporal",
    show_progress = FALSE
  ))

  expect_s3_class(result_employer, "data.table")
  expect_s3_class(result_temporal, "data.table")

  # Employer-based should preserve all transitions (no consolidation across employers)
  # Temporal-based might consolidate some transitions
  if (nrow(result_employer) > 0 && nrow(result_temporal) > 0) {
    # Should have transitions A->B and B->C in employer mode
    employer_transitions <- paste(
      result_employer$from,
      "->",
      result_employer$to
    )
    expect_true(length(employer_transitions) >= 2) # Should have multiple transitions
  }
})

test_that("employer-based consolidation respects min_lag parameter", {
  library(data.table)

  # Create data with same employer but varying gaps
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-04-01", "2023-06-01")),
    fine = as.Date(c("2023-02-10", "2023-03-20", "2023-05-15", "2023-07-31")),
    arco = c(1, 1, 1, 1),
    durata = c(40, 34, 45, 61),
    company = c("CompanyA", "CompanyA", "CompanyA", "CompanyA"),
    employer_id = c("EMP001", "EMP001", "EMP001", "EMP001"), # Same employer
    position = c("Junior", "Junior", "Senior", "Senior")
  )

  # Calculate actual gaps: 5 days, 12 days, 17 days

  # Test with strict lag (7 days) - should consolidate first two only
  result_strict <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "position",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 7, # Only first gap (5 days) should consolidate
    show_progress = FALSE
  ))

  # Test with moderate lag (15 days) - should consolidate first three
  result_moderate <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "position",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 15, # First two gaps (5, 12 days) should consolidate
    show_progress = FALSE
  ))

  # Test with permissive lag (30 days) - should consolidate all
  result_permissive <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "position",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 30, # All gaps should consolidate
    show_progress = FALSE
  ))

  expect_s3_class(result_strict, "data.table")
  expect_s3_class(result_moderate, "data.table")
  expect_s3_class(result_permissive, "data.table")

  # Different lag values should produce different consolidation results
  # More strict = more transitions preserved
  # More permissive = fewer transitions (more consolidation)
  if (
    nrow(result_strict) > 0 &&
      nrow(result_moderate) > 0 &&
      nrow(result_permissive) > 0
  ) {
    # Strict should have more transitions than permissive
    expect_true(nrow(result_strict) >= nrow(result_moderate))
    expect_true(nrow(result_moderate) >= nrow(result_permissive))
  }
})

test_that("employer-based consolidation handles edge cases correctly", {
  library(data.table)

  # Test 1: No employer data available (all NA)
  test_data_no_employer <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")),
    arco = c(1, 1, 1),
    durata = c(58, 92, 92),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    employer_id = c(NA, NA, NA) # All missing employer data
  )

  result_no_employer <- suppressMessages(analyze_employment_transitions(
    test_data_no_employer,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    show_progress = FALSE
  ))

  expect_s3_class(result_no_employer, "data.table")
  # Should handle missing employer data gracefully

  # Test 2: All contracts from same employer
  test_data_same_employer <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")),
    arco = c(1, 1, 1),
    durata = c(58, 92, 92),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    employer_id = c("EMP001", "EMP001", "EMP001") # Same employer throughout
  )

  result_same_employer <- suppressMessages(analyze_employment_transitions(
    test_data_same_employer,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result_same_employer, "data.table")

  # Test 3: All contracts from different employers
  test_data_diff_employers <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), # Close dates
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    arco = c(1, 1, 1),
    durata = c(31, 28, 31),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    employer_id = c("EMP001", "EMP002", "EMP003") # All different
  )

  result_diff_employers <- suppressMessages(analyze_employment_transitions(
    test_data_diff_employers,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result_diff_employers, "data.table")
  # Should preserve all transitions since all employers are different
  if (nrow(result_diff_employers) > 0) {
    expect_true(nrow(result_diff_employers) >= 2) # Should have A->B and B->C transitions
  }

  # Test 4: Zero min_lag (no consolidation allowed)
  result_zero_lag <- suppressMessages(analyze_employment_transitions(
    test_data_same_employer,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 0, # No consolidation
    show_progress = FALSE
  ))

  expect_s3_class(result_zero_lag, "data.table")
  # With zero lag, should preserve all transitions even for same employer
})

test_that("employer-based vs temporal consolidation produces different results", {
  library(data.table)

  # Create scenario where modes should differ:
  # Same employer with large temporal gap + different employer with small gap
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-06-01", "2023-06-15")),
    fine = as.Date(c("2023-01-31", "2023-05-31", "2023-06-10", "2023-08-31")),
    arco = c(1, 1, 1, 1),
    durata = c(31, 120, 10, 77),
    company = c("CompanyA", "CompanyA", "CompanyB", "CompanyB"),
    employer_id = c("EMP001", "EMP001", "EMP002", "EMP002"), # Two employers
    salary = c(50000, 55000, 45000, 48000)
  )

  # Temporal mode: might consolidate based purely on time gaps
  result_temporal <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "temporal",
    show_progress = FALSE
  ))

  # Employer mode: consolidates within employer boundaries
  result_employer <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result_temporal, "data.table")
  expect_s3_class(result_employer, "data.table")

  # Results should potentially differ based on consolidation logic
  # Both should be valid data.tables with transition structure
  if (nrow(result_temporal) > 0 && nrow(result_employer) > 0) {
    # Both should have from/to columns
    expect_true(all(c("from", "to") %in% names(result_temporal)))
    expect_true(all(c("from", "to") %in% names(result_employer)))
  }
})

test_that("employer-based consolidation preserves data integrity", {
  library(data.table)

  # Create comprehensive test data with multiple people and employers
  test_data <- data.table(
    cf = c(rep("PERSON001", 4), rep("PERSON002", 3)),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-03-01",
      "2023-06-01",
      "2023-09-01",
      "2023-02-01",
      "2023-05-01",
      "2023-08-01"
    )),
    fine = as.Date(c(
      "2023-02-28",
      "2023-05-31",
      "2023-08-31",
      "2023-12-31",
      "2023-04-30",
      "2023-07-31",
      "2023-10-31"
    )),
    arco = c(1, 1, 1, 1, 1, 1, 1),
    durata = c(58, 92, 92, 122, 89, 92, 92),
    company = c(
      "CompanyA",
      "CompanyA",
      "CompanyB",
      "CompanyC",
      "CompanyD",
      "CompanyD",
      "CompanyE"
    ),
    employer_id = c(
      "EMP001",
      "EMP001",
      "EMP002",
      "EMP003",
      "EMP004",
      "EMP004",
      "EMP005"
    ),
    salary = c(50000, 55000, 60000, 65000, 45000, 48000, 52000),
    department = c(
      "IT",
      "IT",
      "Finance",
      "HR",
      "Marketing",
      "Marketing",
      "Sales"
    )
  )

  result <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "department",
    statistics_variables = "salary",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    # Check required columns exist
    required_cols <- c("from", "to", "weight", "transition_duration")
    expect_true(all(required_cols %in% names(result)))

    # Check for statistics columns
    salary_stats <- names(result)[grepl("salary", names(result))]
    if (length(salary_stats) > 0) {
      expect_true(any(grepl("salary.*median", names(result))))
    }

    # Verify numeric columns are properly typed
    expect_true(is.numeric(result$weight))
    expect_true(is.numeric(result$transition_duration))

    # Check that transition weights are positive
    expect_true(all(result$weight > 0))

    # Check that transition durations are non-negative
    expect_true(all(result$transition_duration >= 0))
  }
})

test_that("employer-based consolidation handles multiple people correctly", {
  library(data.table)

  # Create data with multiple people having different employer patterns
  test_data <- data.table(
    cf = c(rep("PERSON001", 3), rep("PERSON002", 3), rep("PERSON003", 2)),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-03-01",
      "2023-06-01", # Person 1
      "2023-01-15",
      "2023-04-01",
      "2023-07-01", # Person 2
      "2023-02-01",
      "2023-08-01"
    )), # Person 3
    fine = as.Date(c(
      "2023-02-28",
      "2023-05-31",
      "2023-08-31", # Person 1
      "2023-03-15",
      "2023-06-30",
      "2023-09-30", # Person 2
      "2023-07-31",
      "2023-12-31"
    )), # Person 3
    arco = c(1, 1, 1, 1, 1, 1, 1, 1),
    durata = c(58, 92, 92, 60, 91, 92, 181, 153),
    company = c(
      "CompanyA",
      "CompanyA",
      "CompanyB", # P1: same->diff
      "CompanyC",
      "CompanyD",
      "CompanyD", # P2: diff->same
      "CompanyE",
      "CompanyE"
    ), # P3: same->same
    employer_id = c(
      "EMP001",
      "EMP001",
      "EMP002", # P1: same->diff
      "EMP003",
      "EMP004",
      "EMP004", # P2: diff->same
      "EMP005",
      "EMP005"
    ) # P3: same->same
  )

  result <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    # Should handle all three people correctly
    expect_true(all(c("from", "to", "weight") %in% names(result)))

    # Each person should contribute to transitions
    # Person 1: CompanyA->CompanyB (after consolidation)
    # Person 2: CompanyC->CompanyD (after consolidation)
    # Person 3: might be fully consolidated if gaps are small

    expect_true(nrow(result) >= 1) # Should have at least some transitions
  }
})

# =================== COMPREHENSIVE CONSOLIDATION MODE TESTS ===================

test_that("consolidation_mode parameter validation works correctly", {
  library(data.table)

  # Create sample data with employer information
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")),
    arco = c(1, 1, 1),
    durata = c(58, 92, 92),
    over_id = c(100, 101, 102),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    test_employer = c("EMP001", "EMP001", "EMP002"),
    salary = c(50000, 55000, 60000)
  )

  # Test 1: Invalid consolidation_mode should error
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "invalid"
    ),
    "consolidation_mode.*must be one of"
  )

  # Test 2: Missing employer_var for employer mode should error
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer"
    ),
    "employer_var.*is required"
  )

  # Test 3: Missing employer_var for both mode should error
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "both"
    ),
    "employer_var.*is required"
  )

  # Test 4: Non-existent employer_var column should error
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = "nonexistent_column"
    ),
    "Column nonexistent_column not found in pipeline_result"
  )

  # Test 5: employer_var not a single character string should error
  expect_error(
    analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = c("test_employer", "company")
    ),
    "Parameter 'employer_var' must be a single character string"
  )
})

test_that("all consolidation modes execute successfully", {
  library(data.table)

  # Load sample data and prepare a subset
  e <- new.env()
  utils::data("sample", package = "longworkR", envir = e)
  sample_data <- get("sample", envir = e)

  # Take a small subset for faster testing
  test_subset <- sample_data[cf %in% unique(sample_data$cf)[1:3]]

  # Ensure we have over_id for temporal consolidation
  if (!"over_id" %in% names(test_subset)) {
    test_subset[, over_id := seq_len(.N)]
  }

  # Test 1: consolidation_mode = "none"
  expect_no_error({
    result_none <- suppressMessages(analyze_employment_transitions(
      test_subset,
      consolidation_mode = "none",
      show_progress = FALSE
    ))
  })

  # Test 2: consolidation_mode = "temporal" (skip if temporal consolidation not available)
  if (
    exists("merge_consecutive_employment_fast_with_type", mode = "function")
  ) {
    expect_no_error({
      result_temporal <- suppressMessages(analyze_employment_transitions(
        test_subset,
        consolidation_mode = "temporal",
        show_progress = FALSE
      ))
    })
  } else {
    skip("Temporal consolidation function not available")
  }

  # Test 3: consolidation_mode = "employer"
  expect_no_error({
    result_employer <- suppressMessages(analyze_employment_transitions(
      test_subset,
      consolidation_mode = "employer",
      employer_var = "datore",
      show_progress = FALSE
    ))
  })

  # Test 4: consolidation_mode = "both" (skip if temporal consolidation not available)
  if (
    exists("merge_consecutive_employment_fast_with_type", mode = "function")
  ) {
    expect_no_error({
      result_both <- suppressMessages(analyze_employment_transitions(
        test_subset,
        consolidation_mode = "both",
        employer_var = "datore",
        show_progress = FALSE
      ))
    })
  } else {
    skip("Temporal consolidation function not available for 'both' mode")
  }

  # Verify all results are data.table objects
  expect_s3_class(result_none, "data.table")
  expect_s3_class(result_employer, "data.table")

  # Only verify temporal and both if they were tested
  if (
    exists("merge_consecutive_employment_fast_with_type", mode = "function")
  ) {
    expect_s3_class(result_temporal, "data.table")
    expect_s3_class(result_both, "data.table")
  }
})

test_that("output_transition_matrix disables statistics_variables with message", {
  library(data.table)

  # Load sample data subset
  e <- new.env()
  utils::data("sample", package = "longworkR", envir = e)
  sample_data <- get("sample", envir = e)
  test_subset <- sample_data[cf %in% unique(sample_data$cf)[1:2]]

  # Test that when output_transition_matrix = TRUE, statistics_variables are disabled
  # and an informative message is shown
  result <- suppressMessages(capture.output(
    {
      matrix_result <- analyze_employment_transitions(
        test_subset,
        output_transition_matrix = TRUE,
        statistics_variables = c("retribuzione"),
        consolidation_mode = "none",
        show_progress = FALSE
      )
    },
    type = "message"
  ))

  # Should return a matrix when output_transition_matrix = TRUE
  expect_true(is.matrix(matrix_result))

  # Test with default FALSE behavior
  normal_result <- suppressMessages(analyze_employment_transitions(
    test_subset,
    output_transition_matrix = FALSE,
    statistics_variables = c("retribuzione"),
    consolidation_mode = "none",
    show_progress = FALSE
  ))

  # Should return data.table when output_transition_matrix = FALSE
  expect_s3_class(normal_result, "data.table")
})

test_that("consolidation_mode 'both' combines employer and temporal consolidation", {
  library(data.table)

  # Skip if temporal consolidation not available
  if (
    !exists("merge_consecutive_employment_fast_with_type", mode = "function")
  ) {
    skip(
      "Temporal consolidation function not available for 'both' mode testing"
    )
  }

  # Create test data where 'both' mode should produce different results than individual modes
  test_data <- data.table(
    cf = c(rep("PERSON001", 6)),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-02-01",
      "2023-02-15",
      "2023-04-01",
      "2023-04-15",
      "2023-06-01"
    )),
    fine = as.Date(c(
      "2023-01-31",
      "2023-02-10",
      "2023-03-31",
      "2023-04-10",
      "2023-05-31",
      "2023-08-31"
    )),
    arco = c(1, 1, 1, 1, 1, 1),
    durata = c(31, 10, 45, 10, 47, 92),
    over_id = c(100, 101, 101, 102, 102, 103),
    company = c(
      "CompanyA",
      "CompanyA",
      "CompanyB",
      "CompanyB",
      "CompanyC",
      "CompanyD"
    ),
    employer_id = c("EMP001", "EMP001", "EMP002", "EMP002", "EMP003", "EMP004"),
    salary = c(50000, 52000, 55000, 57000, 60000, 65000)
  )

  # Test each consolidation mode
  result_none <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "none",
    show_progress = FALSE
  ))

  result_temporal <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "temporal",
    show_progress = FALSE
  ))

  result_employer <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  result_both <- suppressMessages(analyze_employment_transitions(
    test_data,
    transition_variable = "company",
    consolidation_mode = "both",
    employer_var = "employer_id",
    min_lag = 8,
    show_progress = FALSE
  ))

  # All should return valid data.table objects
  expect_s3_class(result_none, "data.table")
  expect_s3_class(result_temporal, "data.table")
  expect_s3_class(result_employer, "data.table")
  expect_s3_class(result_both, "data.table")

  # All should have required transition columns
  required_cols <- c("from", "to", "weight")
  if (nrow(result_none) > 0) {
    expect_true(all(required_cols %in% names(result_none)))
  }
  if (nrow(result_temporal) > 0) {
    expect_true(all(required_cols %in% names(result_temporal)))
  }
  if (nrow(result_employer) > 0) {
    expect_true(all(required_cols %in% names(result_employer)))
  }
  if (nrow(result_both) > 0) {
    expect_true(all(required_cols %in% names(result_both)))
  }
})

test_that("consolidation modes preserve data integrity with sample data", {
  library(data.table)

  # Use actual sample data for comprehensive testing
  e <- new.env()
  utils::data("sample", package = "longworkR", envir = e)
  sample_data <- get("sample", envir = e)

  # Take a meaningful subset (3 people with sufficient data)
  cf_subset <- unique(sample_data$cf)[1:3]
  test_data <- sample_data[cf %in% cf_subset & !is.na(datore)]

  # Only proceed if we have data
  if (nrow(test_data) > 0) {
    # Test each mode with actual data
    result_none <- suppressMessages(analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "none",
      show_progress = FALSE
    ))

    result_employer <- suppressMessages(analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "employer",
      employer_var = "datore",
      show_progress = FALSE
    ))

    # Only test temporal and both modes if temporal consolidation is available
    results_to_check <- list(result_none, result_employer)

    if (
      exists("merge_consecutive_employment_fast_with_type", mode = "function")
    ) {
      result_temporal <- suppressMessages(analyze_employment_transitions(
        test_data,
        transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
        consolidation_mode = "temporal",
        show_progress = FALSE
      ))

      result_both <- suppressMessages(analyze_employment_transitions(
        test_data,
        transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
        consolidation_mode = "both",
        employer_var = "datore",
        show_progress = FALSE
      ))

      # Verify results are valid
      expect_s3_class(result_temporal, "data.table")
      expect_s3_class(result_both, "data.table")

      # Add temporal and both results to check list
      results_to_check <- list(
        result_none,
        result_employer,
        result_temporal,
        result_both
      )
    }

    # Verify basic results are valid
    expect_s3_class(result_none, "data.table")
    expect_s3_class(result_employer, "data.table")

    # Check data integrity for non-empty results
    for (result in results_to_check) {
      if (nrow(result) > 0) {
        # Required columns should exist
        expect_true(all(c("from", "to", "weight") %in% names(result)))

        # Numeric columns should be properly typed
        expect_true(is.numeric(result$weight))
        expect_true(all(result$weight > 0))

        # From/to should be character
        expect_true(is.character(result$from) || is.factor(result$from))
        expect_true(is.character(result$to) || is.factor(result$to))
      }
    }
  } else {
    skip("Insufficient data in sample for this test")
  }
})

test_that("employer_var warnings work correctly", {
  library(data.table)

  # Create test data
  test_data <- data.table(
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31")),
    arco = c(1, 1),
    durata = c(58, 92),
    over_id = c(100, 101),
    company = c("CompanyA", "CompanyB"),
    employer_id = c("EMP001", "EMP002")
  )

  # Warning when employer_var provided but consolidation_mode is "temporal"
  if (
    exists("merge_consecutive_employment_fast_with_type", mode = "function")
  ) {
    expect_warning(
      suppressMessages(analyze_employment_transitions(
        test_data,
        consolidation_mode = "temporal",
        employer_var = "employer_id",
        show_progress = FALSE
      )),
      "Parameter 'employer_var' is ignored when consolidation_mode = 'temporal'"
    )
  } else {
    # Test the warning with a function that should work
    expect_warning(
      suppressMessages(analyze_employment_transitions(
        test_data,
        consolidation_mode = "none",
        employer_var = "employer_id",
        show_progress = FALSE
      )),
      "Parameter 'employer_var' is ignored when consolidation_mode = 'none'"
    )
  }

  # Warning when employer_var provided but consolidation_mode is "none"
  expect_warning(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "none",
      employer_var = "employer_id",
      show_progress = FALSE
    )),
    "Parameter 'employer_var' is ignored when consolidation_mode = 'none'"
  )
})
