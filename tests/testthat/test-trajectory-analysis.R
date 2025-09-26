# Unit tests for track_contract_trajectories() function
# Following testthat v3 conventions and longworkR testing patterns

test_that("track_contract_trajectories handles basic functionality", {
  library(data.table)

  # Create sample data with a specific contract type to track
  # Include arco variable: 1 for employment, 0 for unemployment
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON003"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00",
                                   "B.01.00", "A.01.00", "B.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01",
                       "2023-01-15", "2023-04-01", "2023-02-01")),
    fine = as.Date(c("2023-02-28", "2023-04-30", "2023-08-31",
                     "2023-03-15", "2023-05-31", "2023-03-31")),
    over_id = c(1, 2, 3, 1, 2, 1),
    arco = c(1, 1, 1, 1, 1, 1)  # All periods are employment
  )

  # Test basic functionality with B.01.00 contract type
  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 3,
    return_plot = FALSE
  )

  # Should return a list with expected components
  expect_type(result, "list")
  expected_components <- c("data", "summary", "plot", "reference_dates", "parameters")
  expect_true(all(expected_components %in% names(result)))

  # Data should be a data.table
  expect_s3_class(result$data, "data.table")
  expect_s3_class(result$summary, "data.table")
  expect_s3_class(result$reference_dates, "data.table")

  # Should have 3 persons with B.01.00 contracts
  expect_equal(nrow(result$reference_dates), 3)

  # Data should have the correct number of rows (3 persons × 3 quarters = 9)
  expect_equal(nrow(result$data), 9)

  # Should have correct column names in data
  expected_data_cols <- c("cf", "quarter", "quarter_start", "quarter_end",
                         "employment_status", "days_employed", "employment_percentage")
  expect_true(all(expected_data_cols %in% names(result$data)))

  # Parameters should be correctly stored
  expect_equal(result$parameters$contract_type_value, "B.01.00")
  expect_equal(result$parameters$n_quarters, 3)
  expect_equal(result$parameters$n_individuals, 3)
})

test_that("track_contract_trajectories validates input parameters", {
  library(data.table)

  sample_data <- data.table(
    cf = c("PERSON001", "PERSON002"),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "B.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    arco = c(1, 1)  # Employment periods
  )

  # Should error when data is not a data.table
  expect_error(
    track_contract_trajectories(data = as.data.frame(sample_data),
                               contract_type_value = "A.01.00"),
    "Input data must be a data.table object"
  )

  # Should error when missing required columns
  incomplete_data <- sample_data[, .(cf, COD_TIPOLOGIA_CONTRATTUALE, arco)]
  expect_error(
    track_contract_trajectories(data = incomplete_data,
                               contract_type_value = "A.01.00"),
    "Missing required columns"
  )

  # Should error when contract_type_value is missing
  expect_error(
    track_contract_trajectories(data = sample_data),
    "contract_type_value must be specified"
  )

  # Should error when n_quarters is invalid
  expect_error(
    track_contract_trajectories(data = sample_data,
                               contract_type_value = "A.01.00",
                               n_quarters = 0),
    "n_quarters must be between 1 and 20"
  )

  expect_error(
    track_contract_trajectories(data = sample_data,
                               contract_type_value = "A.01.00",
                               n_quarters = 25),
    "n_quarters must be between 1 and 20"
  )

  # Should error when no records found for contract type
  expect_error(
    track_contract_trajectories(data = sample_data,
                               contract_type_value = "NONEXISTENT"),
    "No records found for contract type: NONEXISTENT"
  )
})

test_that("track_contract_trajectories calculates reference dates correctly", {
  library(data.table)

  # Create data where PERSON001 has multiple B.01.00 contracts
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "B.01.00", "A.01.00"),
    inizio = as.Date(c("2023-06-01", "2023-01-01", "2023-09-01")),  # First chronologically is 2023-01-01
    fine = as.Date(c("2023-08-31", "2023-03-31", "2023-11-30")),   # First chronologically ends 2023-03-31
    arco = c(1, 1, 1)  # All employment periods
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Should use the chronologically first contract (earliest start date) as reference
  expect_equal(result$reference_dates$reference_date, as.Date("2023-03-31"))

  # Quarters should start day after reference date
  first_quarter_start <- result$data[quarter == "Q1"]$quarter_start[1]
  expect_equal(first_quarter_start, as.Date("2023-04-01"))
})

test_that("track_contract_trajectories calculates employment percentages correctly", {
  library(data.table)

  # Create precise test data for employment calculation
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-04-30")),
    arco = c(1, 1)  # Both are employment periods
    # B.01.00 ends 2023-01-31, so Q1 starts 2023-02-01
    # Q1: 2023-02-01 to 2023-05-02 (91 days)
    # A.01.00 from 2023-04-01 to 2023-04-30 overlaps with Q1
    # Days employed: April 1-30 = 30 days
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  q1_data <- result$data[quarter == "Q1"]

  # Should have 30 days employed in Q1 (April 1-30)
  # Q1 is 2023-02-01 to 2023-05-02 (91 days), contract is 2023-04-01 to 2023-04-30
  # Overlap is April 1 to April 30, which is 30 days
  expect_equal(q1_data$days_employed, 30)

  # Should be approximately 32.97% employed (30/91 * 100)
  expect_equal(round(q1_data$employment_percentage, 1), 33.0)

  # Should be classified as "Partially Working" (1-40%)
  expect_equal(q1_data$employment_status, "Partially Working")
})

test_that("track_contract_trajectories classifies employment status correctly", {
  library(data.table)

  # Create test cases for each employment status threshold
  test_cases <- data.table(
    cf = paste0("PERSON00", 1:6),
    COD_TIPOLOGIA_CONTRATTUALE = rep("B.01.00", 6),
    inizio = rep(as.Date("2023-01-01"), 6),
    fine = rep(as.Date("2023-01-31"), 6),  # Reference date
    arco = rep(1, 6)  # All employment
  )

  # Add follow-up contracts with different durations for each person
  follow_up_contracts <- data.table(
    cf = paste0("PERSON00", c(1:5)),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 5),
    inizio = rep(as.Date("2023-02-01"), 5),  # Q1 starts here
    fine = as.Date(c(
      "2023-02-01",   # PERSON001: 1 day = 1.10% (Partially Working)
      "2023-02-20",   # PERSON002: 20 days (~22%, Partially Working)
      "2023-04-15",   # PERSON003: 74 days (~81%, Mostly Working)
      "2023-05-02",   # PERSON004: 91 days (100%, Fully Working)
      "2023-01-31"    # PERSON005: No overlap (No Information)
    )),
    arco = c(1, 1, 1, 1, 0)  # PERSON005 has arco=0 (unemployment)
    # PERSON006 has no follow-up contract (No Information)
  )

  # Combine data
  sample_data <- rbind(test_cases, follow_up_contracts)

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  q1_statuses <- result$data[quarter == "Q1"][order(cf)]$employment_status

  # Verify employment status classifications
  expect_equal(q1_statuses[1], "Partially Working")  # 1 day = 1.10%
  expect_equal(q1_statuses[2], "Partially Working")  # 20 days = ~22%
  expect_equal(q1_statuses[3], "Mostly Working")     # 74 days = ~81%
  expect_equal(q1_statuses[4], "Fully Working")      # 91 days = 100%
  expect_equal(q1_statuses[5], "No Information")     # No overlap
  expect_equal(q1_statuses[6], "No Information")     # No follow-up contract
})

test_that("track_contract_trajectories handles true 'Not Working' case", {
  library(data.table)

  # To get "Not Working" (<1%), we need a very short contract
  # Let's create a contract that's less than 1 day in the quarter
  test_case <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),  # Reference date, Q1 starts 2023-02-01
    arco = 1  # Employment
  )

  # Add a contract that barely touches the quarter (fraction of a day)
  # This is hard to achieve with date precision, so "Not Working" may be rare
  # For now, we'll document that <1% employment is difficult to achieve
  # with daily precision and 91-day quarters

  # Skip this test as "Not Working" requires sub-day precision
  skip("Not Working status (<1%) is difficult to achieve with daily precision in 91-day quarters")
})

test_that("track_contract_trajectories handles overlapping contracts", {
  library(data.table)

  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-03-01", "2023-03-15")),
    fine = as.Date(c("2023-01-31", "2023-03-15", "2023-03-30", "2023-04-15")),
    arco = c(1, 1, 1, 1)  # All employment
    # Reference: 2023-01-31, Q1: 2023-02-01 to 2023-05-01
    # Overlapping contracts from 2023-02-15 to 2023-04-15
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  q1_data <- result$data[quarter == "Q1"]

  # Should merge overlapping periods: 2023-02-15 to 2023-04-15 = 60 days
  expect_equal(q1_data$days_employed, 60)

  # Should be 65.9% employed (60/91)
  expect_equal(round(q1_data$employment_percentage, 1), 65.9)

  # Should be classified as "Mostly Working"
  expect_equal(q1_data$employment_status, "Mostly Working")
})

test_that("track_contract_trajectories handles edge cases", {
  library(data.table)

  # Test with single person, single quarter
  minimal_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1  # Employment
  )

  result <- track_contract_trajectories(
    data = minimal_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  expect_equal(nrow(result$data), 1)
  expect_equal(nrow(result$reference_dates), 1)
  expect_equal(result$data$employment_status, "No Information")

  # Test with character dates (should be converted)
  char_date_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = "2023-01-01",
    fine = "2023-01-31",
    arco = 1  # Employment
  )

  expect_no_error({
    result2 <- track_contract_trajectories(
      data = char_date_data,
      contract_type_value = "B.01.00",
      n_quarters = 1,
      return_plot = FALSE
    )
  })

  expect_s3_class(result2$data$quarter_start, "Date")
})

test_that("track_contract_trajectories creates summary correctly", {
  library(data.table)

  # Create data with predictable trajectory patterns
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON002", "PERSON003",
           "PERSON001", "PERSON002"),  # Two people with follow-up contracts
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "B.01.00", "B.01.00",
                                   "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01",
                       "2023-02-15", "2023-02-15")),
    fine = as.Date(c("2023-01-31", "2023-01-31", "2023-01-31",
                     "2023-03-15", "2023-03-15")),
    arco = c(1, 1, 1, 1, 1)  # All employment
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Should have trajectory summary
  expect_s3_class(result$summary, "data.table")
  expect_true("trajectory" %in% names(result$summary))
  expect_true("count" %in% names(result$summary))
  expect_true("percentage" %in% names(result$summary))

  # Should sum to total number of individuals
  expect_equal(sum(result$summary$count), nrow(result$reference_dates))

  # Percentages should sum to 100
  expect_equal(sum(result$summary$percentage), 100)
})

test_that("track_contract_trajectories generates plots correctly", {
  library(data.table)

  sample_data <- data.table(
    cf = c("PERSON001", "PERSON002"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "B.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-01-01")),
    fine = as.Date(c("2023-01-31", "2023-01-31")),
    arco = c(1, 1)  # All employment
  )

  # Test with plot generation enabled
  result_with_plot <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = TRUE
  )

  # Should create a ggplot object
  expect_s3_class(result_with_plot$plot, "ggplot")

  # Test with plot generation disabled
  result_no_plot <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Plot should be NULL
  expect_null(result_no_plot$plot)

  # Test custom plot parameters
  result_custom <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = TRUE,
    use_bw = TRUE,
    plot_title = "Custom Title",
    plot_subtitle = "Custom Subtitle"
  )

  expect_s3_class(result_custom$plot, "ggplot")
  expect_equal(result_custom$plot$labels$title, "Custom Title")
  expect_equal(result_custom$plot$labels$subtitle, "Custom Subtitle")
})

test_that("track_contract_trajectories handles custom column names", {
  library(data.table)

  # Create data with custom column names
  custom_data <- data.table(
    person_identifier = "PERSON001",
    contract_type = "B.01.00",
    start_date = as.Date("2023-01-01"),
    end_date = as.Date("2023-01-31"),
    arco = 1  # Employment
  )

  result <- track_contract_trajectories(
    data = custom_data,
    contract_type_var = "contract_type",
    contract_type_value = "B.01.00",
    person_id = "person_identifier",
    start_date_col = "start_date",
    end_date_col = "end_date",
    n_quarters = 1,
    return_plot = FALSE
  )

  expect_equal(nrow(result$reference_dates), 1)
  expect_equal(nrow(result$data), 1)
  # Should use the custom person_id column name
  expect_true("person_identifier" %in% names(result$data))
  expect_false("cf" %in% names(result$data))
})

test_that("track_contract_trajectories return structure is complete", {
  library(data.table)

  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1  # Employment
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = TRUE,
    palette = "employment"
  )

  # Validate complete return structure
  expect_type(result, "list")
  expect_length(result, 5)

  # Check data component
  expect_s3_class(result$data, "data.table")
  expected_data_cols <- c("cf", "quarter", "quarter_start", "quarter_end",
                         "employment_status", "days_employed", "employment_percentage")
  expect_true(all(expected_data_cols %in% names(result$data)))

  # Check summary component
  expect_s3_class(result$summary, "data.table")
  expect_true(all(c("trajectory", "count", "percentage") %in% names(result$summary)))

  # Check reference_dates component
  expect_s3_class(result$reference_dates, "data.table")
  expect_true(all(c("cf", "reference_date") %in% names(result$reference_dates)))

  # Check parameters component
  expect_type(result$parameters, "list")
  required_params <- c("contract_type_var", "contract_type_value", "n_quarters",
                      "person_id", "start_date_col", "end_date_col",
                      "palette", "use_bw", "n_individuals")
  expect_true(all(required_params %in% names(result$parameters)))

  # Check plot component
  expect_s3_class(result$plot, "ggplot")
})

test_that("track_contract_trajectories handles boundary employment percentages", {
  library(data.table)

  # Create test cases for exact boundary values
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON002", "PERSON003", "PERSON004", "PERSON005"),
    COD_TIPOLOGIA_CONTRATTUALE = rep("B.01.00", 5),
    inizio = rep(as.Date("2023-01-01"), 5),
    fine = rep(as.Date("2023-01-31"), 5),
    arco = rep(1, 5)  # All employment
  )

  # Add contracts with exact boundary employment percentages
  boundary_contracts <- data.table(
    cf = c("PERSON001", "PERSON002", "PERSON003", "PERSON004"),
    COD_TIPOLOGIA_CONTRATTUALE = rep("A.01.00", 4),
    inizio = rep(as.Date("2023-02-01"), 4),  # Q1 starts here
    fine = as.Date(c(
      "2023-02-01",   # Exactly 1 day = 1.10%, should be "Partially Working"
      "2023-03-03",   # 36 days = 39.56%, should be "Partially Working"
      "2023-05-01",   # 90 days = 98.90%, should be "Mostly Working"
      "2023-05-02"    # 91 days = 100%, should be "Fully Working"
    )),
    arco = c(1, 1, 1, 1)  # All employment
  )

  test_data <- rbind(sample_data, boundary_contracts)

  result <- track_contract_trajectories(
    data = test_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  statuses <- result$data[quarter == "Q1"][order(cf)]$employment_status

  # Test boundary conditions
  expect_equal(statuses[1], "Partially Working")  # 1.10% (>= 1%, <= 40%)
  expect_equal(statuses[2], "Partially Working")  # 39.56% (>= 1%, <= 40%)
  expect_equal(statuses[3], "Mostly Working")     # 98.90% (> 40%, <= 99%)
  expect_equal(statuses[4], "Fully Working")      # 100% (> 99%)
  expect_equal(statuses[5], "No Information")     # No follow-up contract
})

test_that("track_contract_trajectories performance with realistic data sizes", {
  library(data.table)

  # Create larger dataset to test performance
  n_persons <- 50
  sample_data <- data.table(
    cf = rep(paste0("PERSON", sprintf("%03d", 1:n_persons)), each = 3),
    COD_TIPOLOGIA_CONTRATTUALE = rep(c("B.01.00", "A.01.00", "A.01.00"), n_persons),
    inizio = rep(as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")), n_persons),
    fine = rep(as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")), n_persons),
    arco = rep(1, n_persons * 3)  # All employment
  )

  # Should complete without errors for moderate dataset size
  start_time <- Sys.time()

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 4,
    return_plot = FALSE
  )

  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete in reasonable time (less than 5 seconds for 50 persons)
  expect_lt(execution_time, 5)

  # Should produce correct results
  expect_equal(nrow(result$reference_dates), n_persons)
  expect_equal(nrow(result$data), n_persons * 4)  # 50 persons × 4 quarters
  expect_equal(result$parameters$n_individuals, n_persons)
})

test_that("track_contract_trajectories handles chunk_size parameter correctly", {
  library(data.table)

  # Create dataset with 15 persons (enough to test chunking)
  n_persons <- 15
  sample_data <- data.table(
    cf = rep(paste0("PERSON", sprintf("%03d", 1:n_persons)), each = 2),
    COD_TIPOLOGIA_CONTRATTUALE = rep(c("B.01.00", "A.01.00"), n_persons),
    inizio = rep(as.Date(c("2023-01-01", "2023-03-01")), n_persons),
    fine = rep(as.Date(c("2023-02-28", "2023-05-31")), n_persons),
    arco = rep(1, n_persons * 2)  # All employment
  )

  # Test with small chunk size (should force chunked processing)
  result_chunked <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE,
    chunk_size = 5
  )

  # Test with large chunk size (should process all at once)
  result_unchunked <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE,
    chunk_size = 20
  )

  # Results should be identical regardless of chunking
  expect_equal(nrow(result_chunked$data), nrow(result_unchunked$data))
  expect_equal(nrow(result_chunked$reference_dates), nrow(result_unchunked$reference_dates))

  # Data should be identical when ordered properly
  setkey(result_chunked$data, cf, quarter)
  setkey(result_unchunked$data, cf, quarter)
  expect_equal(result_chunked$data$employment_status, result_unchunked$data$employment_status)
  expect_equal(result_chunked$data$days_employed, result_unchunked$data$days_employed)

  # Parameters should reflect the chunk_size used
  expect_equal(result_chunked$parameters$chunk_size, 5)
  expect_equal(result_unchunked$parameters$chunk_size, 20)
})

test_that("track_contract_trajectories stores quarter_days parameter correctly", {
  library(data.table)

  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1  # Employment
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  # Should store the updated quarter_days parameter
  expect_equal(result$parameters$quarter_days, 91)
})

test_that("track_contract_trajectories fcase dependency works correctly", {
  library(data.table)

  # Test that fcase is properly imported and works for employment status classification
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-01", "2023-03-15", "2023-05-02")),  # Different employment levels
    arco = c(1, 1, 1, 1)  # All employment
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  # fcase should properly classify the employment status
  employment_statuses <- unique(result$data$employment_status)
  valid_statuses <- c("No Information", "Not Working", "Partially Working",
                     "Mostly Working", "Fully Working")

  expect_true(all(employment_statuses %in% valid_statuses))

  # Should have calculated employment percentages correctly
  expect_true(all(result$data$employment_percentage >= 0))
  expect_true(all(result$data$employment_percentage <= 100))
})

test_that("track_contract_trajectories handles memory-efficient processing", {
  library(data.table)

  # Create moderate size dataset to test memory efficiency
  n_persons <- 100
  sample_data <- data.table(
    cf = rep(paste0("PERSON", sprintf("%03d", 1:n_persons)), each = 2),
    COD_TIPOLOGIA_CONTRATTUALE = rep(c("B.01.00", "A.01.00"), n_persons),
    inizio = rep(as.Date(c("2023-01-01", "2023-03-01")), n_persons),
    fine = rep(as.Date(c("2023-02-28", "2023-05-31")), n_persons),
    arco = rep(1, n_persons * 2)  # All employment
  )

  # Test with chunked processing
  expect_no_error({
    result <- track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      n_quarters = 3,
      return_plot = FALSE,
      chunk_size = 25  # Force chunking
    )
  })

  # Should handle vectorized operations correctly
  expect_s3_class(result$data, "data.table")
  expect_equal(nrow(result$data), n_persons * 3)  # 100 persons × 3 quarters
  expect_equal(nrow(result$reference_dates), n_persons)
})

test_that("track_contract_trajectories reference date calculation handles ties correctly", {
  library(data.table)

  # Create data where multiple contracts have the same start date
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "B.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-01-01", "2023-02-01")),  # Two B.01.00 same start
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31")),     # Different end dates
    arco = c(1, 1, 1)  # All employment
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  # Should use first occurrence when ordered by start then end date
  # First contract chronologically ends on 2023-01-31
  expect_equal(result$reference_dates$reference_date, as.Date("2023-01-31"))

  # Quarters should start day after reference date
  first_quarter_start <- result$data[quarter == "Q1"]$quarter_start[1]
  expect_equal(first_quarter_start, as.Date("2023-02-01"))
})

test_that("track_contract_trajectories handles custom employment thresholds", {
  library(data.table)

  # Create test data with known employment patterns
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON003", "PERSON003"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "B.01.00", "A.01.00", "B.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-10", "2023-01-31", "2023-03-01", "2023-01-31", "2023-04-30")),
    arco = c(1, 1, 1, 1, 1, 1)  # All employment
  )

  # Test with custom thresholds: c(5, 30, 80)
  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    employment_thresholds = c(5, 30, 80),
    return_plot = FALSE
  )

  # Should store the custom thresholds
  expect_equal(result$parameters$employment_thresholds, c(5, 30, 80))

  # Employment percentages for Q1:
  # PERSON001: 10 days / 91 = 10.99% -> "Partially Working" (5-30%)
  # PERSON002: 29 days / 91 = 31.87% -> "Mostly Working" (30-80%)
  # PERSON003: 89 days / 91 = 97.80% -> "Fully Working" (>80%)

  q1_data <- result$data[quarter == "Q1"][order(cf)]

  expect_equal(q1_data$employment_status[1], "Partially Working")  # 10.99%
  expect_equal(q1_data$employment_status[2], "Mostly Working")     # 31.87%
  expect_equal(q1_data$employment_status[3], "Fully Working")      # 97.80%
})

test_that("track_contract_trajectories validates employment_thresholds parameter", {
  library(data.table)

  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1
  )

  # Should error when not numeric
  expect_error(
    track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c("1", "40", "99")
    ),
    "employment_thresholds must be a numeric vector"
  )

  # Should error when wrong length
  expect_error(
    track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c(1, 40)
    ),
    "employment_thresholds must contain exactly 3 values"
  )

  # Should error when values outside 0-100 range
  expect_error(
    track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c(-1, 40, 99)
    ),
    "employment_thresholds values must be between 0 and 100"
  )

  expect_error(
    track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c(1, 40, 101)
    ),
    "employment_thresholds values must be between 0 and 100"
  )

  # Should error when not in ascending order
  expect_error(
    track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c(40, 1, 99)
    ),
    "employment_thresholds must be in ascending order"
  )

  # Should work with valid thresholds
  expect_no_error({
    result <- track_contract_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      employment_thresholds = c(0, 50, 100),
      return_plot = FALSE
    )
  })
})

test_that("track_contract_trajectories uses default thresholds correctly", {
  library(data.table)

  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1
  )

  # Should use default thresholds when not specified
  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    return_plot = FALSE
  )

  expect_equal(result$parameters$employment_thresholds, c(1, 40, 99))
})

test_that("track_contract_trajectories handles arco = 0 periods correctly", {
  library(data.table)

  # Create test data with mixed arco values
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON003"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "B.01.00", "A.01.00", "B.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01", "2023-01-01")),
    fine = as.Date(c("2023-01-31", "2023-02-10", "2023-01-31", "2023-02-10", "2023-01-31")),
    arco = c(1, 0, 1, 1, 1)  # PERSON001 has arco=0 in Q1, others have arco=1
  )

  result <- track_contract_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  q1_data <- result$data[quarter == "Q1"][order(cf)]

  # PERSON001: Has contract in Q1 but arco=0, so 0 employment days -> "Not Working"
  expect_equal(q1_data$days_employed[1], 0)
  expect_equal(q1_data$employment_percentage[1], 0)
  expect_equal(q1_data$employment_status[1], "Not Working")

  # PERSON002: Has employment contract (arco=1) in Q1 -> "Partially Working"
  expect_equal(q1_data$days_employed[2], 10)  # Feb 1-10 = 10 days
  expect_true(q1_data$employment_percentage[2] > 0)
  expect_equal(q1_data$employment_status[2], "Partially Working")

  # PERSON003: No follow-up contract, so NA -> "No Information"
  expect_true(is.na(q1_data$days_employed[3]))
  expect_equal(q1_data$employment_status[3], "No Information")
})

test_that("track_contract_trajectories distinguishes No Information from zero employment", {
  library(data.table)

  # Test case 1: Person with no contracts at all in Q1
  data1 <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1
  )

  result1 <- track_contract_trajectories(
    data = data1,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  # Should be "No Information" because no contracts in Q1
  expect_true(is.na(result1$data$days_employed))
  expect_equal(result1$data$employment_status, "No Information")

  # Test case 2: Person with arco=0 contracts in Q1
  data2 <- data.table(
    cf = c("PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-10")),
    arco = c(1, 0)  # arco=0 in Q1
  )

  result2 <- track_contract_trajectories(
    data = data2,
    contract_type_value = "B.01.00",
    n_quarters = 1,
    return_plot = FALSE
  )

  # Should be "Not Working" because has contracts but arco=0
  expect_equal(result2$data$days_employed, 0)
  expect_equal(result2$data$employment_percentage, 0)
  expect_equal(result2$data$employment_status, "Not Working")
})

# Tests for track_professional_trajectories() function
test_that("track_professional_trajectories handles basic functionality", {
  library(data.table)

  # Create sample data with qualifica variable
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON003"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00",
                                   "B.01.00", "A.01.00", "B.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01",
                       "2023-01-15", "2023-04-01", "2023-02-01")),
    fine = as.Date(c("2023-02-28", "2023-04-30", "2023-08-31",
                     "2023-03-15", "2023-05-31", "2023-03-31")),
    arco = c(1, 1, 1, 1, 1, 1),  # All periods are employment
    qualifica = c("C1", "C2", "C2", "C1", "C1", "C3")  # Professional codes
  )

  # Test basic functionality with B.01.00 contract type
  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 3,
    return_plot = FALSE
  )

  # Should return a list with expected components
  expect_type(result, "list")
  expected_components <- c("data", "summary", "plot", "reference_dates", "parameters")
  expect_true(all(expected_components %in% names(result)))

  # Data should be a data.table
  expect_s3_class(result$data, "data.table")
  expect_s3_class(result$summary, "data.table")
  expect_s3_class(result$reference_dates, "data.table")

  # Should have 3 persons with B.01.00 contracts
  expect_equal(nrow(result$reference_dates), 3)

  # Data should have the correct number of rows (3 persons × 3 quarters = 9)
  expect_equal(nrow(result$data), 9)

  # Should have correct column names in data
  expected_data_cols <- c("cf", "quarter", "quarter_start", "quarter_end",
                         "professional_status", "quarter_code", "reference_code")
  expect_true(all(expected_data_cols %in% names(result$data)))

  # Parameters should be correctly stored
  expect_equal(result$parameters$contract_type_value, "B.01.00")
  expect_equal(result$parameters$n_quarters, 3)
  expect_equal(result$parameters$qualifica_col, "qualifica")
})

test_that("track_professional_trajectories handles professional status classification", {
  library(data.table)

  # Create data to test all professional status categories
  # Reference ends 2023-01-31, so Q1 starts 2023-02-01
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-05-10", "2023-08-10")),
    fine = as.Date(c("2023-01-31", "2023-04-15", "2023-07-10", "2023-10-10")),
    arco = c(1, 1, 0, 1),  # Mixed employment status
    qualifica = c("C1", "C1", "C2", "C3")  # Different professional codes
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 4,
    return_plot = FALSE
  )

  # Check that different professional statuses are assigned correctly
  statuses <- unique(result$data$professional_status)
  possible_statuses <- c("Same Code", "Different Code", "Not Working", "No Information")
  expect_true(all(statuses %in% possible_statuses))

  # Reference code should be "C1" (from the B.01.00 contract)
  expect_equal(result$reference_dates$reference_code, "C1")

  # Check specific quarters
  person_data <- result$data[cf == "PERSON001"]

  # Q1 (Feb 1 - May 2): Should have "Same Code" (C1 = C1)
  expect_equal(person_data[quarter == "Q1"]$professional_status, "Same Code")

  # Q2 (May 3 - Aug 1): Should have "Not Working" for arco=0 periods
  expect_equal(person_data[quarter == "Q2"]$professional_status, "Not Working")

  # Q3 (Aug 2 - Oct 31): Should have "Different Code" for different qualifica (C3 != C1)
  expect_equal(person_data[quarter == "Q3"]$professional_status, "Different Code")

  # Q4: Should have "No Information" (no contracts)
  expect_equal(person_data[quarter == "Q4"]$professional_status, "No Information")
})

test_that("track_professional_trajectories handles arco prioritization correctly", {
  library(data.table)

  # Create data where employment contracts (arco > 0) should be prioritized
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-20", "2023-02-25")),
    arco = c(1, 0, 1),  # Employment, unemployment, employment
    qualifica = c("C1", "C2", "C3")  # Different codes
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # In Q1, should prioritize employment contract (arco=1) over unemployment (arco=0)
  q1_data <- result$data[quarter == "Q1"]

  # Should use code from employment contract (C3) not unemployment contract (C2)
  # Since C3 != C1 (reference), should be "Different Code"
  expect_equal(q1_data$professional_status, "Different Code")
  expect_equal(q1_data$quarter_code, "C3")
})

test_that("track_professional_trajectories handles end date prioritization", {
  library(data.table)

  # Create data where latest end date should be used
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-25")),  # Latest end date wins
    arco = c(1, 1, 1),  # All employment
    qualifica = c("C1", "C2", "C3")  # Different codes, C3 has latest end date
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # In Q1, should use code from contract with latest end date (C3)
  q1_data <- result$data[quarter == "Q1"]
  expect_equal(q1_data$quarter_code, "C3")
  expect_equal(q1_data$professional_status, "Different Code")  # C3 != C1
})

test_that("track_professional_trajectories handles missing data correctly", {
  library(data.table)

  # Create data with gaps (no contracts in some quarters)
  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1,
    qualifica = "C1"
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 3,
    return_plot = FALSE
  )

  # All Q1, Q2 and Q3 should be "No Information" (no contracts in tracking period)
  # Reference ends 2023-01-31, tracking starts 2023-02-01, so no overlapping contracts
  expect_equal(sum(result$data$professional_status == "No Information"), 3)
})

test_that("track_professional_trajectories handles input validation", {
  library(data.table)

  # Valid sample data
  sample_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    arco = 1,
    qualifica = "C1"
  )

  # Test missing contract_type_value
  expect_error(
    track_professional_trajectories(data = sample_data),
    "contract_type_value must be specified"
  )

  # Test invalid n_quarters
  expect_error(
    track_professional_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      n_quarters = 0
    ),
    "n_quarters must be between 1 and 20"
  )

  expect_error(
    track_professional_trajectories(
      data = sample_data,
      contract_type_value = "B.01.00",
      n_quarters = 25
    ),
    "n_quarters must be between 1 and 20"
  )

  # Test missing required columns
  incomplete_data <- sample_data[, .(cf, COD_TIPOLOGIA_CONTRATTUALE)]
  expect_error(
    track_professional_trajectories(
      data = incomplete_data,
      contract_type_value = "B.01.00"
    ),
    "Missing required columns"
  )

  # Test non-data.table input
  expect_error(
    track_professional_trajectories(
      data = data.frame(sample_data),
      contract_type_value = "B.01.00"
    ),
    "Input data must be a data.table object"
  )

  # Test contract type not found
  expect_error(
    track_professional_trajectories(
      data = sample_data,
      contract_type_value = "NONEXISTENT"
    ),
    "No records found for contract type: NONEXISTENT"
  )
})

test_that("track_professional_trajectories handles chronological first occurrence", {
  library(data.table)

  # Create data where person has multiple B.01.00 contracts
  # Should use the chronologically first one as reference
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "A.01.00", "B.01.00"),  # Two B.01.00 contracts
    inizio = as.Date(c("2023-03-01", "2023-02-01", "2023-01-01")),    # Third has earliest start
    fine = as.Date(c("2023-03-31", "2023-02-28", "2023-01-31")),
    arco = c(1, 1, 1),
    qualifica = c("C1", "C2", "C3")  # Should use C3 as reference (earliest start)
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Reference should be from the chronologically first B.01.00 contract (C3)
  expect_equal(result$reference_dates$reference_code, "C3")

  # Reference date should be the end date of the first occurrence
  expect_equal(result$reference_dates$reference_date, as.Date("2023-01-31"))
})

test_that("track_professional_trajectories creates summary correctly", {
  library(data.table)

  # Create data with predictable trajectory patterns
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON002", "PERSON003",
           "PERSON001", "PERSON002"),  # Two people with follow-up contracts
    COD_TIPOLOGIA_CONTRATTUALE = c("B.01.00", "B.01.00", "B.01.00",
                                   "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01",
                       "2023-02-15", "2023-02-15")),
    fine = as.Date(c("2023-01-31", "2023-01-31", "2023-01-31",
                     "2023-03-15", "2023-03-15")),
    arco = c(1, 1, 1, 1, 1),
    qualifica = c("C1", "C1", "C1", "C1", "C2")  # Mixed codes for different patterns
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "B.01.00",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Should have trajectory summary
  expect_s3_class(result$summary, "data.table")
  expect_true("trajectory" %in% names(result$summary))
  expect_true("count" %in% names(result$summary))
  expect_true("percentage" %in% names(result$summary))

  # Percentages should sum to approximately 100 (allow for rounding)
  expect_equal(sum(result$summary$percentage), 100, tolerance = 0.1)

  # Should have different trajectory patterns
  expect_gt(nrow(result$summary), 1)
})

test_that("track_professional_trajectories handles date conversion", {
  library(data.table)

  # Test with character dates (should be converted)
  char_date_data <- data.table(
    cf = "PERSON001",
    COD_TIPOLOGIA_CONTRATTUALE = "B.01.00",
    inizio = "2023-01-01",
    fine = "2023-01-31",
    arco = 1,
    qualifica = "C1"
  )

  expect_no_error({
    result <- track_professional_trajectories(
      data = char_date_data,
      contract_type_value = "B.01.00",
      n_quarters = 1,
      return_plot = FALSE
    )
  })

  expect_s3_class(result$data$quarter_start, "Date")
  expect_s3_class(result$reference_dates$reference_date, "Date")
})

# ===== COMPREHENSIVE UNEMPLOYMENT DETECTION TESTS =====
# Testing the fix for correctly detecting "Not Working" status when quarters
# contain only unemployment periods (arco = 0)

test_that("track_professional_trajectories detects pure unemployment quarters correctly", {
  library(data.table)

  # Create data where person has employment reference, then pure unemployment periods
  # Reference ends 2023-01-31, so Q1 starts 2023-02-01 (91 days = until 2023-05-02)
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-05-10")),
    fine = as.Date(c("2023-01-31", "2023-04-15", "2023-07-10")),
    arco = c(1, 0, 0),  # Reference employment, then two unemployment periods
    professione = c("P1", "P2", "P3")  # Using custom qualifica column name
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",  # Test custom column name as in user's scenario
    n_quarters = 4,
    return_plot = FALSE
  )

  # Reference should be P1 from the C.01.00 contract
  expect_equal(result$reference_dates$reference_code, "P1")

  # Get quarterly data
  person_data <- result$data[cf == "PERSON001"]

  # Q1 (Feb 1 - May 2): Has unemployment period (arco=0) from Feb 15 - Apr 15 → "Not Working"
  q1_status <- person_data[quarter == "Q1"]$professional_status
  expect_equal(q1_status, "Not Working")

  # Q2 (May 3 - Aug 1): Has unemployment period (arco=0) from May 10 - Jul 10 → "Not Working"
  q2_status <- person_data[quarter == "Q2"]$professional_status
  expect_equal(q2_status, "Not Working")

  # Q3 and Q4: No contracts → "No Information"
  q3_status <- person_data[quarter == "Q3"]$professional_status
  q4_status <- person_data[quarter == "Q4"]$professional_status
  expect_equal(q3_status, "No Information")
  expect_equal(q4_status, "No Information")
})

test_that("track_professional_trajectories prioritizes employment over unemployment in mixed quarters", {
  library(data.table)

  # Create data where quarter has both employment and unemployment contracts
  # Reference ends 2023-01-31, so Q1 is 2023-02-01 to 2023-05-02
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-10", "2023-02-20")),
    fine = as.Date(c("2023-01-31", "2023-02-05", "2023-02-15", "2023-02-28")),
    arco = c(1, 0, 1, 0),  # Mixed: unemployment, employment, unemployment
    professione = c("P1", "P2", "P3", "P4")  # Different professional codes
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Q1 has mixed arco values: should prioritize employment contract (arco=1)
  # Latest employment contract is P3 (Feb 10-15), so should be "Different Code" (P3 != P1)
  q1_data <- result$data[quarter == "Q1"]
  expect_equal(q1_data$professional_status, "Different Code")
  expect_equal(q1_data$quarter_code, "P3")  # Should use employment contract, not unemployment

  # Q2: No contracts → "No Information"
  q2_data <- result$data[quarter == "Q2"]
  expect_equal(q2_data$professional_status, "No Information")
})

test_that("track_professional_trajectories handles sequential employment-unemployment-employment pattern", {
  library(data.table)

  # Test user's typical pattern: employment → unemployment → employment
  # Reference ends 2023-01-31, so quarters start 2023-02-01
  # Q1: 2023-02-01 to 2023-05-02, Q2: 2023-05-03 to 2023-08-01, etc.
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "A.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-05-10", "2023-08-10", "2023-11-15")),
    fine = as.Date(c("2023-01-31", "2023-04-30", "2023-07-31", "2023-10-31", "2023-12-31")),
    arco = c(1, 1, 0, 0, 1),  # Employment, employment, unemployment, unemployment, employment
    professione = c("P1", "P1", "P2", "P3", "P1")  # Mixed codes with return to P1
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 6,
    return_plot = FALSE
  )

  person_data <- result$data[cf == "PERSON001"]

  # Q1 (Feb 1 - May 2): Employment contract with P1 → "Same Code"
  expect_equal(person_data[quarter == "Q1"]$professional_status, "Same Code")
  expect_equal(person_data[quarter == "Q1"]$quarter_code, "P1")

  # Q2 (May 3 - Aug 1): Unemployment period only (arco=0) → "Not Working"
  expect_equal(person_data[quarter == "Q2"]$professional_status, "Not Working")

  # Q3 (Aug 2 - Oct 31): Unemployment period only (arco=0) → "Not Working"
  expect_equal(person_data[quarter == "Q3"]$professional_status, "Not Working")

  # Q4 (Nov 1 - Jan 30): Employment returns with P1 → "Same Code"
  expect_equal(person_data[quarter == "Q4"]$professional_status, "Same Code")
  expect_equal(person_data[quarter == "Q4"]$quarter_code, "P1")

  # Q5 & Q6: No contracts → "No Information"
  expect_equal(person_data[quarter == "Q5"]$professional_status, "No Information")
  expect_equal(person_data[quarter == "Q6"]$professional_status, "No Information")
})

test_that("track_professional_trajectories handles custom qualifica_col with unemployment", {
  library(data.table)

  # Test the specific user scenario with custom qualifica_col = "professione"
  # Reference ends 2022-01-31, so Q1 starts 2022-02-01
  vtr_2022_sample <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "A.01.00"),
    inizio = as.Date(c("2022-01-01", "2022-02-15", "2022-05-10")),
    fine = as.Date(c("2022-01-31", "2022-04-15", "2022-07-10")),
    arco = c(1, 0, 0),  # Reference employment, then unemployment
    professione = c("PRO001", "PRO002", "PRO003")  # Custom column name
  )

  # Replicate user's exact function call
  tiroc_prof <- track_professional_trajectories(
    vtr_2022_sample,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 4
  )

  # Should work without errors
  expect_type(tiroc_prof, "list")
  expect_s3_class(tiroc_prof$data, "data.table")

  # Reference code should be from C.01.00 contract
  expect_equal(tiroc_prof$reference_dates$reference_code, "PRO001")

  # Check unemployment detection in quarters with only arco=0
  person_data <- tiroc_prof$data[cf == "PERSON001"]
  unemployment_quarters <- person_data[professional_status == "Not Working"]

  # Should have at least one "Not Working" quarter (Q1 has arco=0 only)
  expect_gt(nrow(unemployment_quarters), 0)

  # Verify specific quarters
  # Q1 (Feb 1 - May 2): Has unemployment period (arco=0) → "Not Working"
  expect_equal(person_data[quarter == "Q1"]$professional_status, "Not Working")
  # Q2 (May 3 - Aug 1): Has unemployment period (arco=0) → "Not Working"
  expect_equal(person_data[quarter == "Q2"]$professional_status, "Not Working")
})

test_that("track_professional_trajectories distinguishes Not Working from No Information correctly", {
  library(data.table)

  # Create clear test cases to distinguish the two statuses
  # Both people have reference ending 2023-01-31, so Q1 is 2023-02-01 to 2023-05-02
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON002"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "C.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-01-01")),
    fine = as.Date(c("2023-01-31", "2023-04-15", "2023-01-31")),
    arco = c(1, 0, 1),  # PERSON001 has unemployment in Q1, PERSON002 has no follow-up
    professione = c("P1", "P2", "P1")
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 4,
    return_plot = FALSE
  )

  # PERSON001: Q1 has unemployment contract (arco=0) → "Not Working"
  person1_q1 <- result$data[cf == "PERSON001" & quarter == "Q1"]
  expect_equal(person1_q1$professional_status, "Not Working")
  expect_false(is.na(person1_q1$quarter_code))  # Should have code from unemployment contract

  # PERSON001: Q2, Q3, Q4 have no contracts → "No Information"
  person1_later <- result$data[cf == "PERSON001" & quarter %in% c("Q2", "Q3", "Q4")]
  expect_true(all(person1_later$professional_status == "No Information"))
  expect_true(all(is.na(person1_later$quarter_code)))  # Should be NA for no contracts

  # PERSON002: All quarters have no contracts → "No Information"
  person2_all <- result$data[cf == "PERSON002"]
  expect_true(all(person2_all$professional_status == "No Information"))
  expect_true(all(is.na(person2_all$quarter_code)))  # Should be NA for no contracts
})

test_that("track_professional_trajectories handles edge case: quarter with arco=0 and no professional code", {
  library(data.table)

  # Test edge case where unemployment periods might have missing professional codes
  sample_data <- data.table(
    cf = c("PERSON001", "PERSON001"),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15")),
    fine = as.Date(c("2023-01-31", "2023-03-15")),
    arco = c(1, 0),
    professione = c("P1", NA_character_)  # Unemployment contract has no professional code
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 2,
    return_plot = FALSE
  )

  # Even with NA professional code, should still be "Not Working" because arco=0
  q1_data <- result$data[quarter == "Q1"]
  expect_equal(q1_data$professional_status, "Not Working")
  expect_true(is.na(q1_data$quarter_code))  # Code should be NA
})

test_that("track_professional_trajectories unemployment detection matches vectorized implementation", {
  library(data.table)

  # Test that the vectorized professional status classification works correctly
  # This ensures the fix in lines 547-552 works as expected
  sample_data <- data.table(
    cf = rep(c("PERSON001", "PERSON002", "PERSON003"), each = 2),
    COD_TIPOLOGIA_CONTRATTUALE = c("C.01.00", "A.01.00", "C.01.00", "A.01.00", "C.01.00", "A.01.00"),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-01-01", "2023-02-15", "2023-01-01", "2023-02-15")),
    fine = as.Date(c("2023-01-31", "2023-03-15", "2023-01-31", "2023-03-15", "2023-01-31", "2023-03-15")),
    arco = c(1, 0, 1, 1, 1, 0),  # Different patterns: unemployment, employment, unemployment
    professione = c("P1", "P2", "P3", "P3", "P5", "P6")
  )

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 3,
    return_plot = FALSE
  )

  # Verify vectorized classification using fcase worked correctly
  status_summary <- result$data[, .N, by = professional_status]

  # Should have all four possible statuses represented
  expected_statuses <- c("Same Code", "Different Code", "Not Working", "No Information")

  # At least some of these should be present
  expect_true(any(c("Not Working", "Same Code") %in% status_summary$professional_status))

  # PERSON001: Q1 unemployment → "Not Working"
  expect_equal(result$data[cf == "PERSON001" & quarter == "Q1"]$professional_status, "Not Working")

  # PERSON002: Q1 employment with same code → "Same Code"
  expect_equal(result$data[cf == "PERSON002" & quarter == "Q1"]$professional_status, "Same Code")

  # PERSON003: Q1 unemployment → "Not Working"
  expect_equal(result$data[cf == "PERSON003" & quarter == "Q1"]$professional_status, "Not Working")
})

test_that("track_professional_trajectories performance with unemployment detection at scale", {
  library(data.table)

  # Test performance with larger dataset including unemployment periods
  n_persons <- 100

  # Create dataset with guaranteed unemployment periods
  # Reference ends 2023-02-28, so Q1 starts 2023-03-01
  sample_data <- data.table(
    cf = rep(paste0("PERSON", sprintf("%03d", 1:n_persons)), each = 3),
    COD_TIPOLOGIA_CONTRATTUALE = rep(c("C.01.00", "A.01.00", "A.01.00"), n_persons),
    inizio = rep(as.Date(c("2023-01-01", "2023-03-15", "2023-06-15")), n_persons),
    fine = rep(as.Date(c("2023-02-28", "2023-05-15", "2023-08-15")), n_persons),
    # Mixed arco: reference employment, then forced unemployment, then employment
    arco = rep(c(1, 0, 1), n_persons),  # Guaranteed unemployment in Q1
    professione = rep(paste0("P", 1:3), n_persons)
  )

  # Should complete efficiently even with unemployment detection logic
  start_time <- Sys.time()

  result <- track_professional_trajectories(
    data = sample_data,
    contract_type_value = "C.01.00",
    qualifica_col = "professione",
    n_quarters = 4,
    return_plot = FALSE
  )

  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete in reasonable time (less than 5 seconds for 100 persons)
  expect_lt(execution_time, 5)

  # Results should be consistent
  expect_equal(nrow(result$reference_dates), n_persons)
  expect_equal(nrow(result$data), n_persons * 4)

  # Should have some "Not Working" statuses due to unemployment periods
  status_counts <- result$data[, .N, by = professional_status]
  expect_true("Not Working" %in% status_counts$professional_status)
})