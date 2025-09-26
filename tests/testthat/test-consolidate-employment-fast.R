test_that("consolidate_by_employer_fast produces identical results to original", {

  # Create test data
  test_data <- data.table::data.table(
    cf = rep(c("A", "B", "C"), each = 5),
    inizio = as.Date(c(
      # Person A: 5 records with same employer
      "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01",
      # Person B: Mixed employers
      "2020-01-01", "2020-02-01", "2020-03-15", "2020-04-01", "2020-05-01",
      # Person C: With unemployment
      "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01"
    )),
    fine = as.Date(c(
      # Person A
      "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31",
      # Person B
      "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31",
      # Person C
      "2020-01-31", "2020-02-15", "2020-03-31", "2020-04-30", "2020-05-31"
    )),
    durata = c(
      31, 29, 31, 30, 31,
      31, 29, 17, 30, 31,
      31, 15, 31, 30, 31
    ),
    datore = c(
      # Person A: Same employer
      "EMP1", "EMP1", "EMP1", "EMP1", "EMP1",
      # Person B: Different employers
      "EMP2", "EMP2", "EMP3", "EMP3", "EMP2",
      # Person C: With unemployment (NA employer)
      "EMP4", NA, "EMP4", "EMP4", "EMP4"
    ),
    arco = as.integer(c(
      # Person A: All employment
      1, 1, 1, 1, 1,
      # Person B: All employment
      1, 1, 1, 1, 1,
      # Person C: With unemployment
      1, 0, 1, 1, 1
    )),
    stato = c(
      rep("occupato", 5),
      rep("occupato", 5),
      "occupato", "disoccupato", rep("occupato", 3)
    )
  )

  # Test 1: Basic consolidation
  result_original <- consolidate_by_employer(
    test_data, "datore", min_lag = 8,
    show_progress = FALSE
  )

  result_fast <- consolidate_by_employer_fast(
    test_data, "datore", min_lag = 8,
    parallel = FALSE, show_progress = FALSE
  )

  # Sort both for comparison
  data.table::setkey(result_original, cf, inizio)
  data.table::setkey(result_fast, cf, inizio)

  # Check key columns are identical
  key_cols <- c("cf", "inizio", "fine", "durata", "datore")
  expect_equal(
    result_original[, ..key_cols],
    result_fast[, ..key_cols],
    tolerance = 1e-10
  )

  # Check consolidation metrics
  if ("consolidation_coverage" %in% names(result_original)) {
    expect_equal(
      result_original$consolidation_coverage,
      result_fast$consolidation_coverage,
      tolerance = 1e-10
    )
  }
})


test_that("consolidate_by_employer_fast handles edge cases correctly", {

  # Test 2: Empty dataset
  empty_data <- data.table::data.table(
    cf = character(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    durata = numeric(0),
    datore = character(0)
  )

  result <- consolidate_by_employer_fast(
    empty_data, "datore", show_progress = FALSE
  )

  expect_equal(nrow(result), 0)


  # Test 3: Single record
  single_data <- data.table::data.table(
    cf = "A",
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-01-31"),
    durata = 31,
    datore = "EMP1",
    arco = 1L
  )

  result <- consolidate_by_employer_fast(
    single_data, "datore", show_progress = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1L)


  # Test 4: All different employers (no consolidation needed)
  no_consolidation_data <- data.table::data.table(
    cf = rep("A", 3),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    fine = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
    durata = c(31, 29, 31),
    datore = c("EMP1", "EMP2", "EMP3"),
    arco = rep(1L, 3)
  )

  result <- consolidate_by_employer_fast(
    no_consolidation_data, "datore", show_progress = FALSE
  )

  expect_equal(nrow(result), 3)
  expect_true(all(result$n_periods_consolidated == 1L))
})


test_that("consolidate_by_employer_fast parallel processing works", {
  skip_if(parallel::detectCores() < 2, "Not enough cores for parallel test")

  # Create larger test dataset
  large_data <- data.table::data.table(
    cf = rep(paste0("P", 1:100), each = 20),
    inizio = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 20), 100),
    fine = rep(seq(as.Date("2020-01-31"), by = "month", length.out = 20), 100),
    durata = rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                   31, 28, 31, 30, 31, 30, 31, 31), 100),
    datore = rep(c(rep("EMP1", 5), rep("EMP2", 5), rep("EMP1", 5), rep("EMP3", 5)), 100),
    arco = rep(1L, 2000)
  )

  # Test parallel gives same result as sequential
  result_seq <- consolidate_by_employer_fast(
    large_data, "datore",
    parallel = FALSE, show_progress = FALSE
  )

  result_par <- consolidate_by_employer_fast(
    large_data, "datore",
    parallel = TRUE, n_cores = 2, show_progress = FALSE
  )

  # Sort for comparison
  data.table::setkey(result_seq, cf, inizio)
  data.table::setkey(result_par, cf, inizio)

  key_cols <- c("cf", "inizio", "fine", "durata", "datore")
  expect_equal(
    result_seq[, ..key_cols],
    result_par[, ..key_cols],
    tolerance = 1e-10
  )
})


test_that("consolidate_by_employer_fast handles unemployment correctly", {

  # Create data with unemployment periods
  unemployment_data <- data.table::data.table(
    cf = rep("A", 7),
    inizio = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-02-20",  # Employment, unemployment, employment
      "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01"
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-02-15", "2020-02-29",
      "2020-03-31", "2020-04-30", "2020-05-31", "2020-06-30"
    )),
    durata = c(31, 15, 10, 31, 30, 31, 30),
    datore = c("EMP1", NA, "EMP1", "EMP1", "EMP1", "EMP1", "EMP1"),
    arco = c(1L, 0L, 1L, 1L, 1L, 1L, 1L),
    stato = c("occupato", "disoccupato", rep("occupato", 5))
  )

  # With unemployment removal
  result_remove <- consolidate_by_employer_fast(
    unemployment_data, "datore",
    remove_intermediate_unemployment = TRUE,
    show_progress = FALSE
  )

  # Without unemployment removal
  result_keep <- consolidate_by_employer_fast(
    unemployment_data, "datore",
    remove_intermediate_unemployment = FALSE,
    show_progress = FALSE
  )

  # With removal, should consolidate more
  expect_true(nrow(result_remove) <= nrow(result_keep))

  # Check unemployment days removed metric
  if ("unemployment_days_removed" %in% names(result_remove)) {
    expect_true(any(result_remove$unemployment_days_removed > 0))
  }
})


test_that("consolidate_by_employer_fast candidate preselection works correctly", {

  # Create data where only some people need consolidation
  mixed_data <- data.table::data.table(
    cf = c(
      rep("NEEDS_CONSOLIDATION", 5),    # Same employer, needs consolidation
      rep("NO_CONSOLIDATION_1", 1),      # Single record
      rep("NO_CONSOLIDATION_2", 3)       # Different employers each time
    ),
    inizio = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01",
      "2020-01-01",
      "2020-01-01", "2020-03-01", "2020-05-01"
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31",
      "2020-06-30",
      "2020-02-29", "2020-04-30", "2020-06-30"
    )),
    durata = c(31, 29, 31, 30, 31, 180, 60, 61, 61),
    datore = c(
      rep("EMP1", 5),
      "EMP2",
      "EMP3", "EMP4", "EMP5"
    ),
    arco = rep(1L, 9)
  )

  result <- consolidate_by_employer_fast(
    mixed_data, "datore", show_progress = FALSE
  )

  # Person with same employer should be consolidated
  needs_consolidation_result <- result[cf == "NEEDS_CONSOLIDATION"]
  expect_true(nrow(needs_consolidation_result) < 5)
  expect_true(any(needs_consolidation_result$n_periods_consolidated > 1))

  # Single record person should remain unchanged
  single_result <- result[cf == "NO_CONSOLIDATION_1"]
  expect_equal(nrow(single_result), 1)
  expect_equal(single_result$n_periods_consolidated, 1L)

  # Different employers person should remain unchanged
  different_result <- result[cf == "NO_CONSOLIDATION_2"]
  expect_equal(nrow(different_result), 3)
  expect_true(all(different_result$n_periods_consolidated == 1L))
})


test_that("benchmark_consolidation function works", {
  skip_if_not_installed("microbenchmark")

  # Create test data
  test_data <- data.table::data.table(
    cf = rep(paste0("P", 1:10), each = 5),
    inizio = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 5), 10),
    fine = rep(seq(as.Date("2020-01-31"), by = "month", length.out = 5), 10),
    durata = rep(c(31, 29, 31, 30, 31), 10),
    datore = rep(c("EMP1", "EMP1", "EMP2", "EMP2", "EMP1"), 10),
    arco = rep(1L, 50)
  )

  # Run benchmark
  bench_results <- benchmark_consolidation(
    test_data, "datore", min_lag = 8
  )

  # Check structure
  expect_type(bench_results, "list")
  expect_named(bench_results, c("timings", "speedup", "results_identical", "results"))

  # Check results are identical
  expect_true(bench_results$results_identical$sequential)
  if (parallel::detectCores() >= 2) {
    expect_true(bench_results$results_identical$parallel)
  }

  # Check timings
  expect_s3_class(bench_results$timings, "data.frame")
  expect_equal(nrow(bench_results$timings), 3)
})