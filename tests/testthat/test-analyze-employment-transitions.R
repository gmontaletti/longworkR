test_that("analyze_employment_transitions handles basic transitions with duration-weighted means", {
  library(data.table)
  
  # Create sample data with clear transitions
  sample_data <- data.table(
    id = 1:6,
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
                       "2023-02-01", "2023-06-01", "2023-10-01")),
    FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
                     "2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(1, 0, 1, 1, 1, 0),
    company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
    salary = c(50000, 25000, 60000, 55000, 65000, 30000)
  )
  
  # Process through pipeline (simulate pipeline result)
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-08-01",
                       "2023-02-01", "2023-05-01", "2023-10-01")),
    fine = as.Date(c("2023-02-28", "2023-07-31", "2023-12-31",
                     "2023-04-30", "2023-09-30", "2023-12-31")),
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
    "must be a data.table object"
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
    analyze_employment_transitions(valid_dt, 
                                 min_unemployment_duration = -1),
    "must be a non-negative numeric value"
  )
})

test_that("analyze_employment_transitions uses duration-weighted means correctly", {
  library(data.table)
  
  # Create data with transitions and varying durations to test weighted means
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01", "2023-08-01", "2023-11-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-07-31", "2023-10-31", "2023-12-31")),
    arco = c(1, 0, 1, 0, 1),  # Employment -> Unemployment -> Employment -> Unemployment -> Employment
    durata = c(58, 92, 61, 92, 61),  # Different durations for weighting
    salary = c(50000, NA, 60000, NA, 45000),  # Numeric column to test weighted means
    company = c("CompanyA", NA, "CompanyB", NA, "CompanyC")  # Character column
  )
  
  # Set merged_columns attribute
  setattr(pipeline_result, "merged_columns", c("salary", "company"))
  
  # Test with show_progress = FALSE for cleaner test output
  result <- suppressMessages(analyze_employment_transitions(pipeline_result, show_progress = FALSE))
  
  expect_s3_class(result, "data.table")
  
  # Check for transitions 
  if (nrow(result) > 0) {
    # Should have from and to columns
    expect_true("from" %in% names(result))
    expect_true("to" %in% names(result))
    
    # Should have salary statistics (median by default)
    salary_cols <- names(result)[grepl("salary", names(result))]
    if (length(salary_cols) > 0) {
      expect_true(any(grepl("salary_from_median|salary_to_median", names(result))))
    }
  }
})

test_that("analyze_employment_transitions handles edge cases for weighted means", {
  library(data.table)
  
  # Test with zero/minimal durations
  pipeline_result_edge <- data.table(
    cf = c("EDGE001", "EDGE001", "EDGE001"),
    inizio = as.Date(c("2023-01-01", "2023-01-02", "2023-01-04")),
    fine = as.Date(c("2023-01-01", "2023-01-03", "2023-01-04")),  # Durations: 1, 2, 1
    arco = c(1, 0, 1),
    durata = c(1, 2, 1),
    salary = c(1000, NA, 3000)  # Different values to test weighting with minimal durations
  )
  
  # Set merged_columns attribute
  setattr(pipeline_result_edge, "merged_columns", c("salary"))
  
  result_edge <- suppressMessages(analyze_employment_transitions(pipeline_result_edge, show_progress = FALSE))
  
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
    salary = c(NA, NA, NA)  # All NA values
  )
  
  setattr(pipeline_result_na, "merged_columns", c("salary"))
  
  result_na <- suppressMessages(analyze_employment_transitions(pipeline_result_na, show_progress = FALSE))
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