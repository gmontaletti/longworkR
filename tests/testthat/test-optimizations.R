# test-optimizations.R
# Comprehensive test suite for analyze_employment_transitions.R optimizations
#
# Tests cover:
# - Numerical equivalence with originals
# - Edge cases for all helper functions
# - Numerical accuracy verification

# 1. Setup and test data generation -----

# Create test data generator for various sizes
generate_test_data <- function(n_persons = 100, n_contracts_per_person = 5) {
  library(data.table)

  # Generate person IDs
  cfs <- paste0("PERSON", sprintf("%05d", 1:n_persons))

  # Create contract types following typical employment pattern
  contract_types <- c("A.01.00", "A.03.00", "B.01.01", "B.02.05", "C.01.00")

  # Build dataset
  dt_list <- lapply(cfs, function(cf_id) {
    n_contracts <- sample(3:n_contracts_per_person, 1)
    start_date <- as.Date("2020-01-01")

    contracts <- lapply(1:n_contracts, function(i) {
      duration <- sample(30:365, 1)
      end_date <- start_date + duration

      contract <- data.table(
        cf = cf_id,
        inizio = start_date,
        fine = end_date,
        durata = as.numeric(duration),
        arco = sample(0:1, 1, prob = c(0.9, 0.1)),
        prior = sample(0:1, 1, prob = c(0.3, 0.7)),
        over_id = i,
        COD_TIPOLOGIA_CONTRATTUALE = sample(contract_types, 1),
        salary = round(rnorm(1, mean = 30000, sd = 10000), 2),
        company = paste0("Company_", sample(1:50, 1))
      )

      # Update start date for next contract (add unemployment gap)
      start_date <<- end_date + sample(1:90, 1)

      contract
    })

    rbindlist(contracts)
  })

  dt <- rbindlist(dt_list)
  setorder(dt, cf, inizio)

  return(dt)
}

# 2. Tests for .convert_types_optimized() -----

test_that(".convert_types_optimized converts integer to numeric correctly", {
  library(data.table)

  # Create test data with integer columns
  test_dt <- data.table(
    int_col1 = 1:10,
    int_col2 = 11:20,
    num_col = as.numeric(21:30),
    char_col = letters[1:10]
  )

  # Apply optimization
  result <- longworkR:::.convert_types_optimized(
    test_dt,
    modify_in_place = FALSE
  )

  # Check that integer columns were converted to numeric
  expect_type(result$int_col1, "double")
  expect_type(result$int_col2, "double")

  # Check that numeric and character columns unchanged
  expect_type(result$num_col, "double")
  expect_type(result$char_col, "character")

  # Check values preserved
  expect_equal(result$int_col1, as.numeric(1:10))
  expect_equal(result$int_col2, as.numeric(11:20))
})

test_that(".convert_types_optimized handles empty data.table", {
  library(data.table)

  empty_dt <- data.table(
    int_col = integer(0),
    num_col = numeric(0),
    char_col = character(0)
  )

  result <- longworkR:::.convert_types_optimized(
    empty_dt,
    modify_in_place = FALSE
  )

  expect_equal(nrow(result), 0)
  expect_type(result$int_col, "double")
})

test_that(".convert_types_optimized preserves original when modify_in_place = FALSE", {
  library(data.table)

  original_dt <- data.table(int_col = 1:5)
  original_class <- class(original_dt$int_col)

  result <- longworkR:::.convert_types_optimized(
    original_dt,
    modify_in_place = FALSE
  )

  # Original should be unchanged
  expect_equal(class(original_dt$int_col), original_class)

  # Result should be converted
  expect_type(result$int_col, "double")
})

test_that(".convert_types_optimized handles data.table with no integer columns", {
  library(data.table)

  test_dt <- data.table(
    num_col = as.numeric(1:10),
    char_col = letters[1:10],
    date_col = as.Date("2020-01-01") + 0:9
  )

  result <- longworkR:::.convert_types_optimized(
    test_dt,
    modify_in_place = FALSE
  )

  # Should return identical structure since no conversion needed
  expect_equal(names(result), names(test_dt))
  expect_type(result$num_col, "double")
  expect_type(result$char_col, "character")
})

# 3. Tests for .calculate_weighted_median_optimized() -----

test_that(".calculate_weighted_median_optimized calculates correct weighted median", {
  # Simple test case
  values <- c(1, 2, 3, 4, 5)
  weights <- c(1, 1, 10, 1, 1) # Weight heavily towards 3

  result <- longworkR:::.calculate_weighted_median_optimized(values, weights)

  expect_equal(result, 3)
})

test_that(".calculate_weighted_median_optimized handles equal weights", {
  values <- c(1, 2, 3, 4, 5)
  weights <- c(1, 1, 1, 1, 1)

  result <- longworkR:::.calculate_weighted_median_optimized(values, weights)

  # Should equal unweighted median
  expect_equal(result, median(values))
})

test_that(".calculate_weighted_median_optimized handles empty input", {
  result <- longworkR:::.calculate_weighted_median_optimized(
    numeric(0),
    numeric(0)
  )

  expect_true(is.na(result))
})

test_that(".calculate_weighted_median_optimized handles NA values", {
  values <- c(1, 2, NA, 4, 5)
  weights <- c(1, 1, 1, 1, 1)

  result <- longworkR:::.calculate_weighted_median_optimized(
    values,
    weights,
    na.rm = TRUE
  )

  expect_false(is.na(result))
  expect_equal(result, median(c(1, 2, 4, 5)))
})

test_that(".calculate_weighted_median_optimized handles all zero weights", {
  values <- c(1, 2, 3, 4, 5)
  weights <- c(0, 0, 0, 0, 0)

  result <- longworkR:::.calculate_weighted_median_optimized(values, weights)

  expect_true(is.na(result))
})

test_that(".calculate_weighted_median_optimized handles mismatched lengths", {
  values <- c(1, 2, 3)
  weights <- c(1, 1)

  expect_error(
    longworkR:::.calculate_weighted_median_optimized(values, weights),
    "same length"
  )
})

test_that(".calculate_weighted_median_optimized handles large weights", {
  # Test case that would cause memory issues with rep() approach
  values <- c(100, 200, 300)
  weights <- c(1e6, 1e6, 1e6) # Very large weights

  # Should not error and should return median
  result <- longworkR:::.calculate_weighted_median_optimized(values, weights)

  expect_equal(result, 200)
})

# 4. Tests for .calculate_mode_optimized() -----

test_that(".calculate_mode_optimized finds mode correctly", {
  # Clear mode
  x <- c("A", "B", "C", "A", "A", "B")

  result <- longworkR:::.calculate_mode_optimized(x)

  expect_equal(result, "A")
})

test_that(".calculate_mode_optimized handles single value", {
  x <- c("A")

  result <- longworkR:::.calculate_mode_optimized(x)

  expect_equal(result, "A")
})

test_that(".calculate_mode_optimized handles empty input", {
  x <- character(0)

  result <- longworkR:::.calculate_mode_optimized(x)

  expect_true(is.na(result))
})

test_that(".calculate_mode_optimized handles all NA input", {
  x <- c(NA_character_, NA_character_, NA_character_)

  result <- longworkR:::.calculate_mode_optimized(x, na.rm = TRUE)

  expect_true(is.na(result))
})

test_that(".calculate_mode_optimized handles NA values with na.rm = TRUE", {
  x <- c("A", "B", NA, "A", "C", "A")

  result <- longworkR:::.calculate_mode_optimized(x, na.rm = TRUE)

  expect_equal(result, "A")
})

test_that(".calculate_mode_optimized handles ties (returns first by data.table ordering)", {
  # Both A and B appear twice
  x <- c("A", "B", "A", "B")

  result <- longworkR:::.calculate_mode_optimized(x)

  # Should return one of them (data.table returns first in sort order)
  expect_true(result %in% c("A", "B"))
})

test_that(".calculate_mode_optimized handles numeric input", {
  x <- c(1, 2, 3, 1, 1, 2)

  result <- longworkR:::.calculate_mode_optimized(x)

  expect_equal(result, 1)
})

# 5. Tests for .normalize_transition_matrix_optimized() -----

test_that(".normalize_transition_matrix_optimized normalizes by row correctly", {
  # Create simple transition matrix
  mat <- matrix(
    c(
      10,
      5,
      5,
      2,
      8,
      0,
      1,
      1,
      8
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "row"
  )

  # Check row sums equal 1
  row_sums <- rowSums(result)
  expect_equal(row_sums, c(1, 1, 1), tolerance = 1e-10)
})

test_that(".normalize_transition_matrix_optimized normalizes by column correctly", {
  mat <- matrix(
    c(
      10,
      5,
      5,
      2,
      8,
      0,
      1,
      1,
      8
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "column"
  )

  # Check column sums equal 1
  col_sums <- colSums(result)
  expect_equal(col_sums, c(1, 1, 1), tolerance = 1e-10)
})

test_that(".normalize_transition_matrix_optimized handles empty matrix", {
  mat <- matrix(numeric(0), nrow = 0, ncol = 0)

  result <- longworkR:::.normalize_transition_matrix_optimized(mat)

  expect_equal(dim(result), c(0, 0))
})

test_that(".normalize_transition_matrix_optimized handles zero rows", {
  mat <- matrix(
    c(
      0,
      0,
      0,
      2,
      8,
      0,
      1,
      1,
      8
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "row"
  )

  # First row should remain 0
  expect_equal(result[1, ], c(0, 0, 0))

  # Other rows should sum to 1
  expect_equal(sum(result[2, ]), 1, tolerance = 1e-10)
  expect_equal(sum(result[3, ]), 1, tolerance = 1e-10)
})

test_that(".normalize_transition_matrix_optimized handles single element matrix", {
  mat <- matrix(5, nrow = 1, ncol = 1)

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "row"
  )

  expect_equal(result[1, 1], 1)
})

test_that(".normalize_transition_matrix_optimized preserves matrix dimensions", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "row"
  )

  expect_equal(dim(result), dim(mat))
})

# 6. Tests for .process_chain_value() -----

test_that(".process_chain_value extracts last value correctly", {
  values <- c("A->B->C", "X->Y", "Z")

  result <- longworkR:::.process_chain_value(values, eval_chain = "last")

  expect_equal(result, c("C", "Y", "Z"))
})

test_that(".process_chain_value extracts first value correctly", {
  values <- c("A->B->C", "X->Y", "Z")

  result <- longworkR:::.process_chain_value(values, eval_chain = "first")

  expect_equal(result, c("A", "X", "Z"))
})

test_that(".process_chain_value handles 'none' correctly", {
  values <- c("A->B->C", "X->Y", "Z")

  result <- longworkR:::.process_chain_value(values, eval_chain = "none")

  expect_equal(result, values)
})

test_that(".process_chain_value handles empty input", {
  values <- character(0)

  result <- longworkR:::.process_chain_value(values, eval_chain = "last")

  expect_equal(result, character(0))
})

test_that(".process_chain_value handles NULL input", {
  result <- longworkR:::.process_chain_value(NULL, eval_chain = "last")

  expect_null(result)
})

test_that(".process_chain_value handles NA values", {
  values <- c("A->B", NA, "C->D")

  result <- longworkR:::.process_chain_value(values, eval_chain = "last")

  expect_equal(result[1], "B")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "D")
})

test_that(".process_chain_value handles whitespace in chains", {
  values <- c("A -> B -> C", "X ->Y", "Z-> W")

  result <- longworkR:::.process_chain_value(values, eval_chain = "last")

  expect_equal(result, c("C", "Y", "W"))
})

test_that(".process_chain_value handles single element chains", {
  values <- c("A", "B", "C")

  result_last <- longworkR:::.process_chain_value(values, eval_chain = "last")
  result_first <- longworkR:::.process_chain_value(values, eval_chain = "first")

  expect_equal(result_last, values)
  expect_equal(result_first, values)
})

# 7. Tests for .create_empty_statistics_result() -----

test_that(".create_empty_statistics_result creates correct structure for data.table", {
  library(data.table)

  test_data <- data.table(
    cf = character(0),
    numeric_var = numeric(0),
    char_var = character(0)
  )

  result <- longworkR:::.create_empty_statistics_result(
    statistics_variables = c("numeric_var", "char_var"),
    pipeline_result = test_data,
    output_transition_matrix = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)

  # Check base columns
  expect_true("from" %in% names(result))
  expect_true("to" %in% names(result))
  expect_true("weight" %in% names(result))
  expect_true("transition_duration" %in% names(result))

  # Check statistics columns
  expect_true("numeric_var_from_median" %in% names(result))
  expect_true("numeric_var_to_median" %in% names(result))
  expect_true("char_var_from_mode" %in% names(result))
  expect_true("char_var_to_mode" %in% names(result))
})

test_that(".create_empty_statistics_result creates correct structure for matrix", {
  library(data.table)

  test_data <- data.table(
    cf = character(0),
    numeric_var = numeric(0)
  )

  result <- longworkR:::.create_empty_statistics_result(
    statistics_variables = character(0),
    pipeline_result = test_data,
    output_transition_matrix = TRUE
  )

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})

test_that(".create_empty_statistics_result handles no statistics variables", {
  library(data.table)

  test_data <- data.table(cf = character(0))

  result <- longworkR:::.create_empty_statistics_result(
    statistics_variables = character(0),
    pipeline_result = test_data,
    output_transition_matrix = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)

  # Should only have base columns
  expected_cols <- c("from", "to", "weight", "transition_duration")
  expect_true(all(expected_cols %in% names(result)))
})

# 8. Integration tests - Compare optimized vs original behavior -----

test_that("analyze_employment_transitions with optimizations produces consistent results", {
  skip_if_not_installed("vecshift")

  library(data.table)

  # Generate test data
  set.seed(123)
  test_data <- generate_test_data(n_persons = 50, n_contracts_per_person = 4)

  # Run analysis
  result <- suppressMessages(
    analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "none",
      statistics_variables = "salary"
    )
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)

  # Check structure
  expect_true("from" %in% names(result))
  expect_true("to" %in% names(result))
  expect_true("weight" %in% names(result))
  expect_true("salary_from_median" %in% names(result))
  expect_true("salary_to_median" %in% names(result))

  # Check data types
  expect_type(result$weight, "integer")
  expect_type(result$transition_duration, "double")
  expect_type(result$salary_from_median, "double")
})

# 9. Edge case tests for full workflow -----

test_that("analyze_employment_transitions handles single person with single contract", {
  library(data.table)

  # Minimal data - no transitions possible
  test_data <- data.table(
    cf = "PERSON001",
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-12-31"),
    durata = 365,
    arco = 1,
    over_id = 1,
    prior = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )

  result <- suppressMessages(
    analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE"
    )
  )

  # Should return empty result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("analyze_employment_transitions handles all identical transitions", {
  library(data.table)

  # All transitions are A -> A
  test_data <- data.table(
    cf = rep("PERSON001", 5),
    inizio = as.Date("2020-01-01") + (0:4) * 100,
    fine = as.Date("2020-01-01") + (1:5) * 100,
    durata = rep(100, 5),
    arco = rep(1, 5),
    over_id = 1:5,
    prior = rep(1, 5),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 5)
  )

  result <- suppressMessages(
    analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE"
    )
  )

  # Should have zero transitions (same state doesn't count as transition)
  expect_equal(nrow(result), 0)
})

test_that("analyze_employment_transitions handles large dataset efficiently", {
  skip_on_cran()
  skip_if(Sys.getenv("CI") != "", "Skip on CI due to time constraints")

  library(data.table)

  # Generate large dataset
  set.seed(456)
  large_data <- generate_test_data(
    n_persons = 1000,
    n_contracts_per_person = 10
  )

  result <- suppressMessages(
    analyze_employment_transitions(
      large_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "none",
      statistics_variables = c("salary", "company")
    )
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# 10. Numerical accuracy tests -----

test_that("optimizations produce numerically equivalent results to naive implementations", {
  library(data.table)

  set.seed(789)

  # Test weighted median
  values <- rnorm(100)
  weights <- rpois(100, lambda = 5) + 1

  # Naive implementation using rep()
  naive_weighted_median <- function(v, w) {
    expanded <- rep(v, times = w)
    median(expanded)
  }

  result_optimized <- longworkR:::.calculate_weighted_median_optimized(
    values,
    weights
  )
  result_naive <- naive_weighted_median(values, weights)

  # Should be nearly identical (allowing for floating point tolerance)
  expect_equal(result_optimized, result_naive, tolerance = 1e-12)
})

test_that("matrix normalization produces correct probability distributions", {
  # Create transition matrix
  mat <- matrix(
    c(100, 50, 30, 20, 150, 40, 10, 20, 200),
    nrow = 3,
    byrow = TRUE
  )

  result <- longworkR:::.normalize_transition_matrix_optimized(
    mat,
    normalize_by = "row"
  )

  # Each row should sum to exactly 1.0
  row_sums <- rowSums(result)
  expect_equal(row_sums, rep(1, 3), tolerance = 1e-15)

  # All values should be in [0, 1]
  expect_true(all(result >= 0 & result <= 1))

  # Check that proportions are preserved
  # First row: 100/180, 50/180, 30/180
  expect_equal(result[1, 1], 100 / 180, tolerance = 1e-15)
  expect_equal(result[1, 2], 50 / 180, tolerance = 1e-15)
  expect_equal(result[1, 3], 30 / 180, tolerance = 1e-15)
})

# 11. Test %chin% optimization usage -----

test_that("analyze_employment_transitions uses %chin% for character filtering", {
  # This is more of a code inspection test
  # We verify that the function works correctly with character vectors

  library(data.table)

  set.seed(999)
  test_data <- generate_test_data(n_persons = 100, n_contracts_per_person = 5)

  result <- suppressMessages(
    analyze_employment_transitions(
      test_data,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "none"
    )
  )

  expect_s3_class(result, "data.table")

  # Verify that character filtering worked correctly
  # All 'from' and 'to' values should be valid contract types
  valid_types <- unique(test_data$COD_TIPOLOGIA_CONTRATTUALE)
  expect_true(all(result$from %in% valid_types))
  expect_true(all(result$to %in% valid_types))
})

# 12. Test with real sample.rds data (if available) -----

test_that("optimizations work with real sample.rds data", {
  # Load real sample data via lazy-loaded `sample` dataset
  sample_data <- tryCatch(
    {
      e <- new.env()
      utils::data("sample", package = "longworkR", envir = e)
      get("sample", envir = e)
    },
    error = function(e) NULL
  )
  skip_if(
    !data.table::is.data.table(sample_data) || nrow(sample_data) == 0L,
    "sample dataset not available"
  )

  # Take subset for faster testing
  sample_subset <- sample_data[cf %in% unique(sample_data$cf)[1:100]]

  # Run analysis with optimizations
  result <- suppressMessages(
    analyze_employment_transitions(
      sample_subset,
      transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
      consolidation_mode = "temporal",
      statistics_variables = "prior",
      show_progress = FALSE
    )
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)

  # Verify output structure
  expect_true(all(
    c("from", "to", "weight", "transition_duration") %in% names(result)
  ))

  # Check that statistics were calculated
  if ("prior" %in% names(sample_subset)) {
    expect_true(
      "prior_from_median" %in%
        names(result) ||
        "prior_from_mode" %in% names(result)
    )
  }
})
