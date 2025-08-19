# Test suite for survival analysis functions

library(testthat)
library(data.table)
library(vecshift)

# Helper function to create test employment data
create_test_employment_data <- function(n_persons = 100, seed = 123) {
  set.seed(seed)
  
  # Create synthetic employment data
  dt <- data.table(
    id = 1:(n_persons * 2),
    cf = rep(paste0("CF", sprintf("%03d", 1:n_persons)), each = 2),
    INIZIO = as.Date("2020-01-01") + sample(0:365, n_persons * 2, replace = TRUE),
    prior = sample(c(0, 1), n_persons * 2, replace = TRUE, prob = c(0.3, 0.7))
  )
  
  # Add end dates with some right-censoring
  dt[, FINE := INIZIO + sample(30:730, .N, replace = TRUE)]
  
  # Add contract types
  contract_types <- c("C.01.00", "C.02.00", "C.03.00", "D.01.00", "D.02.00")
  dt[, COD_TIPOLOGIA_CONTRATTUALE := sample(contract_types, .N, replace = TRUE)]
  
  # Ensure some contracts are censored at max date
  max_date <- max(dt$FINE)
  censored_idx <- sample(1:nrow(dt), size = round(nrow(dt) * 0.2))
  dt[censored_idx, FINE := max_date]
  
  return(dt)
}

test_that("add_contract_survival_metrics adds required columns", {
  # Create test data
  test_data <- create_test_employment_data(50)
  
  # Apply survival metrics
  result <- add_contract_survival_metrics(
    data = test_data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE"
  )
  
  # Check for required columns
  expect_true("survival_time" %in% names(result))
  expect_true("censored" %in% names(result))
  expect_true("contract_type_median" %in% names(result))
  expect_true("median_ci_lower" %in% names(result))
  expect_true("median_ci_upper" %in% names(result))
  expect_true("survival_prob" %in% names(result))
  
  # Check data types
  expect_type(result$survival_time, "double")
  expect_type(result$censored, "integer")
  expect_true(all(result$censored %in% c(0, 1)))
})

test_that("survival_time calculation is correct", {
  # Create simple test case
  dt <- data.table(
    id = 1:3,
    cf = c("CF001", "CF001", "CF002"),
    INIZIO = as.Date(c("2020-01-01", "2020-06-01", "2020-03-01")),
    FINE = as.Date(c("2020-01-31", "2020-12-31", "2020-05-31")),
    prior = c(1, 0, 1),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "C.01.00", "C.02.00")
  )
  
  result <- add_contract_survival_metrics(dt)
  
  # Check survival time calculations
  expect_equal(result$survival_time[1], 31)  # Jan 1 to Jan 31
  expect_equal(result$survival_time[2], 214) # Jun 1 to Dec 31
  expect_equal(result$survival_time[3], 92)  # Mar 1 to May 31
})

test_that("censoring indicator is correctly assigned", {
  # Create data with known censoring
  dt <- data.table(
    id = 1:4,
    cf = paste0("CF", 1:4),
    INIZIO = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    FINE = as.Date(c("2020-06-30", "2020-12-31", "2020-12-31", "2020-08-31")),
    prior = rep(1, 4),
    COD_TIPOLOGIA_CONTRATTUALE = rep("C.01.00", 4)
  )
  
  result <- add_contract_survival_metrics(dt)
  
  # Check censoring (contracts ending at max date should be censored)
  max_date <- max(dt$FINE)
  expect_equal(result$censored[result$FINE == max_date], c(1, 1))
  expect_equal(result$censored[result$FINE != max_date], c(0, 0))
})

test_that("estimate_contract_survival returns correct structure", {
  test_data <- create_test_employment_data(30)
  
  # Add survival columns
  test_data[, `:=`(
    survival_time = as.numeric(FINE - INIZIO + 1),
    censored = as.integer(FINE == max(FINE))
  )]
  
  # Estimate survival curves
  survival_results <- estimate_contract_survival(
    data = test_data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
    duration_var = "survival_time",
    censored_var = "censored"
  )
  
  # Check structure
  expect_type(survival_results, "list")
  expect_true("survival_fits" %in% names(survival_results))
  expect_true("median_survival" %in% names(survival_results))
  expect_true("survival_tables" %in% names(survival_results))
  expect_true("confidence_intervals" %in% names(survival_results))
  
  # Check that we have results for each contract type
  contract_types <- unique(test_data$COD_TIPOLOGIA_CONTRATTUALE)
  expect_true(all(contract_types %in% names(survival_results$survival_fits)))
})

test_that("median survival times are reasonable", {
  test_data <- create_test_employment_data(50)
  
  result <- add_contract_survival_metrics(test_data)
  
  # Check median values are within data range
  min_duration <- min(result$survival_time)
  max_duration <- max(result$survival_time)
  
  medians <- unique(result$contract_type_median)
  medians <- medians[!is.na(medians)]
  
  expect_true(all(medians >= min_duration))
  expect_true(all(medians <= max_duration))
})

test_that("confidence intervals are valid", {
  test_data <- create_test_employment_data(30)
  
  result <- add_contract_survival_metrics(test_data)
  
  # Check confidence interval validity (lower <= median <= upper when not NA)
  valid_ci <- !is.na(result$median_ci_lower) & !is.na(result$median_ci_upper) & !is.na(result$contract_type_median)
  
  if (any(valid_ci)) {
    expect_true(all(result$median_ci_lower[valid_ci] <= result$contract_type_median[valid_ci]))
    expect_true(all(result$contract_type_median[valid_ci] <= result$median_ci_upper[valid_ci]))
    expect_true(all(result$median_ci_lower[valid_ci] >= 0))
  }
})

test_that("survival probabilities decrease monotonically", {
  test_data <- create_test_employment_data(20)
  
  test_data[, `:=`(
    survival_time = as.numeric(FINE - INIZIO + 1),
    censored = as.integer(FINE == max(FINE))
  )]
  
  survival_results <- estimate_contract_survival(
    data = test_data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE"
  )
  
  # Check each survival table
  for (ct in names(survival_results$survival_tables)) {
    surv_table <- survival_results$survival_tables[[ct]]
    
    # Survival probabilities should be non-increasing
    if (nrow(surv_table) > 1) {
      diffs <- diff(surv_table$survival_prob)
      expect_true(all(diffs <= 0))
    }
    
    # All probabilities should be between 0 and 1
    expect_true(all(surv_table$survival_prob >= 0))
    expect_true(all(surv_table$survival_prob <= 1))
  }
})

test_that("compare_contract_survival performs statistical tests", {
  test_data <- create_test_employment_data(50)
  
  test_data[, `:=`(
    survival_time = as.numeric(FINE - INIZIO + 1),
    censored = as.integer(FINE == max(FINE))
  )]
  
  # Compare survival curves
  comparison <- compare_contract_survival(
    data = test_data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
    test_type = "both"
  )
  
  # Check output structure
  expect_type(comparison, "list")
  expect_true("test_results" %in% names(comparison))
  expect_true("pairwise_comparisons" %in% names(comparison))
  
  # Check test results
  expect_true("logrank" %in% names(comparison$test_results))
  expect_true("wilcoxon" %in% names(comparison$test_results))
  
  # Check that tests have chi-square statistics
  expect_true(!is.null(comparison$test_results$logrank$chisq))
  expect_true(!is.null(comparison$test_results$wilcoxon$chisq))
})

test_that("get_survival_at_time helper function works correctly", {
  # Create a simple survival table
  surv_table <- data.table(
    time = c(10, 20, 30, 40, 50),
    survival_prob = c(0.95, 0.85, 0.70, 0.55, 0.40)
  )
  
  # Test various time points
  expect_equal(get_survival_at_time(surv_table, 0), 1.0)
  expect_equal(get_survival_at_time(surv_table, 10), 0.95)
  expect_equal(get_survival_at_time(surv_table, 15), 0.95)  # Step function
  expect_equal(get_survival_at_time(surv_table, 20), 0.85)
  expect_equal(get_survival_at_time(surv_table, 50), 0.40)
  expect_equal(get_survival_at_time(surv_table, 100), 0.40)  # Beyond last time
})

test_that("export_survival_results creates files", {
  skip_if_not(interactive(), "Skipping file creation test in non-interactive mode")
  
  test_data <- create_test_employment_data(20)
  test_data[, `:=`(
    survival_time = as.numeric(FINE - INIZIO + 1),
    censored = as.integer(FINE == max(FINE))
  )]
  
  survival_results <- estimate_contract_survival(
    data = test_data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE"
  )
  
  # Create temporary directory
  temp_dir <- tempdir()
  
  # Export results
  output_files <- export_survival_results(
    survival_results = survival_results,
    output_format = "csv",
    output_dir = temp_dir,
    prefix = "test_survival"
  )
  
  # Check that files were created
  expect_true(length(output_files) > 0)
  expect_true(all(file.exists(output_files)))
  
  # Clean up
  unlink(output_files)
})

test_that("survival analysis handles edge cases", {
  # Test with single contract type
  dt <- data.table(
    id = 1:10,
    cf = paste0("CF", 1:10),
    INIZIO = as.Date("2020-01-01") + 0:9,
    FINE = as.Date("2020-01-01") + 30:39,
    prior = rep(1, 10),
    COD_TIPOLOGIA_CONTRATTUALE = rep("C.01.00", 10)
  )
  
  result <- add_contract_survival_metrics(dt)
  expect_equal(length(unique(result$contract_type_median)), 1)
  
  # Test with no censoring (ensure no contracts end at max date)
  dt2 <- copy(dt)
  # Set deterministic FINE dates that are guaranteed not to be the maximum
  dt2[, FINE := INIZIO + c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]
  # Add one more day to one contract to ensure it's the maximum and won't be censored
  dt2[1, FINE := FINE + 10] 
  
  result2 <- add_contract_survival_metrics(dt2)
  # Only the last contract (with max FINE) should be censored
  expect_true(sum(result2$censored) == 1)
  
  # Test with all censored
  dt3 <- copy(dt)
  dt3[, FINE := max(FINE)]
  
  result3 <- add_contract_survival_metrics(dt3)
  expect_true(all(result3$censored == 1))
})

test_that("survival analysis integrates with vecshift output", {
  # Create employment data
  dt <- data.table(
    id = 1:5,
    cf = c("CF001", "CF001", "CF002", "CF002", "CF003"),
    INIZIO = as.Date(c("2020-01-01", "2020-07-01", "2020-02-01", 
                      "2020-08-01", "2020-03-01")),
    FINE = as.Date(c("2020-06-30", "2020-12-31", "2020-07-31", 
                    "2020-12-31", "2020-09-30")),
    prior = c(1, 0, 1, 1, 0)
  )
  
  # Process with vecshift
  vecshift_result <- vecshift(dt, classify_status = FALSE)
  
  # Add contract types to original data
  dt[, COD_TIPOLOGIA_CONTRATTUALE := c("C.01.00", "C.02.00", "C.01.00", 
                                        "C.03.00", "C.02.00")]
  
  # Add survival metrics
  survival_result <- add_contract_survival_metrics(dt)
  
  # Check compatibility
  expect_true(nrow(survival_result) == nrow(dt))
  expect_true(all(c("survival_time", "censored", "contract_type_median", "median_ci_lower", "median_ci_upper") %in% 
                 names(survival_result)))
})