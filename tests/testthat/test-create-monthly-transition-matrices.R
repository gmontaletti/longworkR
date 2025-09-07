library(testthat)
library(data.table)

# Test create_monthly_transition_matrices function
test_that("create_monthly_transition_matrices basic functionality", {
  
  # Skip if data.table not available
  skip_if_not_installed("data.table")
  
  # Create simple test data
  test_data <- data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    arco = c(1, 0, 1, 1, 0, 1),
    inizio = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-01-05", "2022-01-20", "2022-02-10")),
    fine = as.Date(c("2022-01-14", "2022-01-31", "2022-02-28", "2022-01-19", "2022-02-09", "2022-02-25")),
    durata = c(14, 17, 28, 15, 21, 16),
    over_id = c(1, 0, 2, 3, 0, 4),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.02.00", "A.03.00", "A.01.00", "A.02.00", "A.03.00")
  )
  
  # Test basic monthly matrices creation
  result <- create_monthly_transition_matrices(
    test_data,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    show_progress = FALSE
  )
  
  # Check result structure
  expect_type(result, "list")
  expect_true("matrices" %in% names(result))
  expect_true("metadata" %in% names(result))
  
  # Check matrices are created
  expect_type(result$matrices, "list")
  expect_gt(length(result$matrices), 0)
  
  # Check all matrices have same dimensions
  matrix_dims <- lapply(result$matrices, dim)
  expect_true(all(sapply(matrix_dims[-1], identical, matrix_dims[[1]])))
  
  # Check matrix names format
  matrix_names <- names(result$matrices)
  expect_true(all(grepl("^(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)\\d{4}$", matrix_names)))
})

test_that("create_monthly_transition_matrices parameter validation", {
  
  # Test with invalid input
  expect_error(
    create_monthly_transition_matrices("not_a_data_table"),
    "Parameter 'pipeline_result' must be a data.table object"
  )
  
  # Create minimal test data
  test_data <- data.table(
    cf = 1,
    arco = 1,
    inizio = as.Date("2022-01-01"),
    fine = as.Date("2022-01-14"),
    durata = 14,
    over_id = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )
  
  # Test with missing required columns
  incomplete_data <- test_data[, !"arco"]
  expect_error(
    create_monthly_transition_matrices(incomplete_data),
    "Missing required columns in pipeline_result"
  )
  
  # Test with invalid time_format
  expect_error(
    create_monthly_transition_matrices(test_data, time_format = "invalid"),
    "'arg' should be one of"
  )
  
  # Test with custom format but missing parameters
  expect_error(
    create_monthly_transition_matrices(test_data, time_format = "custom"),
    "custom_period_days must be a positive integer"
  )
  
  expect_error(
    create_monthly_transition_matrices(test_data, name_format = "custom"),
    "custom_names must be provided"
  )
})

test_that("create_monthly_transition_matrices quarterly format", {
  
  # Skip if data.table not available
  skip_if_not_installed("data.table")
  
  # Create test data spanning multiple quarters
  test_data <- data.table(
    cf = c(1, 1, 2, 2),
    arco = c(1, 1, 1, 1),
    inizio = as.Date(c("2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01")),
    fine = as.Date(c("2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31")),
    durata = c(90, 91, 92, 92),
    over_id = c(1, 2, 3, 4),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.02.00", "A.03.00", "A.01.00")
  )
  
  # Test quarterly matrices creation
  result <- create_monthly_transition_matrices(
    test_data,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    time_format = "quarterly",
    show_progress = FALSE
  )
  
  # Check result structure
  expect_type(result, "list")
  expect_true("matrices" %in% names(result))
  expect_true("metadata" %in% names(result))
  
  # Check quarterly naming pattern
  matrix_names <- names(result$matrices)
  expect_true(all(grepl("^\\d{4}q[1-4]$", matrix_names)))
})

test_that("create_monthly_transition_matrices metadata", {
  
  # Skip if data.table not available
  skip_if_not_installed("data.table")
  
  # Create test data
  test_data <- data.table(
    cf = c(1, 1),
    arco = c(1, 1),
    inizio = as.Date(c("2022-01-01", "2022-02-01")),
    fine = as.Date(c("2022-01-31", "2022-02-28")),
    durata = c(31, 28),
    over_id = c(1, 2),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.02.00")
  )
  
  # Test with metadata included
  result <- create_monthly_transition_matrices(
    test_data,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    include_summary = TRUE,
    show_progress = FALSE
  )
  
  # Check metadata structure
  expect_true("metadata" %in% names(result))
  expect_true("global_state_space" %in% names(result$metadata))
  expect_true("period_info" %in% names(result$metadata))
  expect_true("matrix_dimensions" %in% names(result$metadata))
  expect_true("total_periods" %in% names(result$metadata))
  expect_true("periods_with_transitions" %in% names(result$metadata))
  expect_true("analysis_parameters" %in% names(result$metadata))
  
  # Test without metadata
  result_no_meta <- create_monthly_transition_matrices(
    test_data,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    include_summary = FALSE,
    show_progress = FALSE
  )
  
  expect_false("metadata" %in% names(result_no_meta))
})

test_that("create_monthly_transition_matrices probability matrices", {
  
  # Skip if data.table not available
  skip_if_not_installed("data.table")
  
  # Create test data with some transitions
  test_data <- data.table(
    cf = c(1, 1, 1),
    arco = c(1, 0, 1),
    inizio = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01")),
    fine = as.Date(c("2022-01-14", "2022-01-31", "2022-02-28")),
    durata = c(14, 17, 28),
    over_id = c(1, 0, 2),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.02.00", "A.03.00")
  )
  
  # Test probability matrices
  result <- create_monthly_transition_matrices(
    test_data,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    matrix_type = "probability",
    normalize_by = "row",
    show_progress = FALSE
  )
  
  # Check that matrices are normalized (row sums should be <= 1)
  for (matrix_name in names(result$matrices)) {
    matrix_obj <- result$matrices[[matrix_name]]
    row_sums <- rowSums(matrix_obj, na.rm = TRUE)
    # Allow for small floating point differences
    expect_true(all(row_sums <= 1.001))
  }
})

test_that("create_monthly_transition_matrices edge cases", {
  
  # Skip if data.table not available
  skip_if_not_installed("data.table")
  
  # Test with single row
  single_row_data <- data.table(
    cf = 1,
    arco = 1,
    inizio = as.Date("2022-01-01"),
    fine = as.Date("2022-01-31"),
    durata = 31,
    over_id = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )
  
  expect_no_error({
    result <- create_monthly_transition_matrices(
      single_row_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      show_progress = FALSE
    )
  })
  
  # Test with no transitions possible (single employment period per person)
  no_transitions_data <- data.table(
    cf = c(1, 2, 3),
    arco = c(1, 1, 1),
    inizio = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")),
    fine = as.Date(c("2022-01-31", "2022-02-28", "2022-03-31")),
    durata = c(31, 28, 31),
    over_id = c(1, 2, 3),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.02.00", "A.03.00")
  )
  
  expect_no_error({
    result <- create_monthly_transition_matrices(
      no_transitions_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      show_progress = FALSE
    )
  })
  
  # All matrices should have consistent dimensions even with no transitions
  matrix_dims <- lapply(result$matrices, dim)
  expect_true(all(sapply(matrix_dims[-1], identical, matrix_dims[[1]])))
})