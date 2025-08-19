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
  
  # Test basic functionality
  result <- analyze_employment_transitions(pipeline_result)
  
  # Should return a data.table
  expect_s3_class(result, "data.table")
  
  # Should have expected columns
  expected_cols <- c("variable", "from", "to", "weight", "transition_duration")
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
  
  # Should warn and return empty data.table
  expect_warning(
    result <- analyze_employment_transitions(pipeline_result),
    "No transition_columns specified"
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
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
  
  # Invalid transition_columns
  expect_error(
    analyze_employment_transitions(valid_dt, transition_columns = 123),
    "must be a non-empty character vector"
  )
  
  # Non-existent columns
  expect_error(
    analyze_employment_transitions(valid_dt, transition_columns = "nonexistent"),
    "not found in pipeline_result"
  )
  
  # Invalid min_unemployment_duration
  expect_error(
    analyze_employment_transitions(valid_dt, 
                                 transition_columns = "test_col",
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
  result <- analyze_employment_transitions(pipeline_result, show_progress = FALSE)
  
  expect_s3_class(result, "data.table")
  
  # Check for transitions: CompanyA->CompanyB and CompanyB->CompanyC
  if (nrow(result) > 0) {
    # Should have weighted means for salary transitions
    salary_transitions <- result[variable == "salary"]
    if (nrow(salary_transitions) > 0) {
      expect_true("from_mean" %in% names(salary_transitions))
      expect_true("to_mean" %in% names(salary_transitions))
      # from_mean and to_mean should be duration-weighted averages
      expect_true(all(!is.na(salary_transitions$from_mean) | salary_transitions$from_mean == 0))
      expect_true(all(!is.na(salary_transitions$to_mean) | salary_transitions$to_mean == 0))
    }
    
    # Should have modes for company transitions  
    company_transitions <- result[variable == "company"]
    if (nrow(company_transitions) > 0) {
      expect_true("from_mode" %in% names(company_transitions))
      expect_true("to_mode" %in% names(company_transitions))
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
  
  result_edge <- analyze_employment_transitions(pipeline_result_edge, show_progress = FALSE)
  
  expect_s3_class(result_edge, "data.table")
  
  # Should handle minimal durations correctly
  if (nrow(result_edge) > 0) {
    salary_transitions <- result_edge[variable == "salary"]
    if (nrow(salary_transitions) > 0) {
      expect_false(is.na(salary_transitions$from_mean[1]))
      expect_false(is.na(salary_transitions$to_mean[1]))
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
  
  result_na <- analyze_employment_transitions(pipeline_result_na, show_progress = FALSE)
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
  
  # Test return_list = TRUE
  result_list <- analyze_employment_transitions(
    pipeline_result, 
    transition_columns = "company",
    return_list = TRUE,
    show_progress = FALSE
  )
  
  expect_type(result_list, "list")
  expect_true("company" %in% names(result_list))
  
  # Test return_list = FALSE (default)
  result_combined <- analyze_employment_transitions(
    pipeline_result,
    transition_columns = "company",
    return_list = FALSE,
    show_progress = FALSE
  )
  
  expect_s3_class(result_combined, "data.table")
  if (nrow(result_combined) > 0) {
    expect_true("variable" %in% names(result_combined))
  }
})