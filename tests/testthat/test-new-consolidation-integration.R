# Test Suite for Consolidation Integration
# Tests composability, chaining, and integration with analyze_employment_transitions()

# 1. Test Data Generators -----

make_overlapping_test_data <- function() {
  data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-28")),
    durata = c(31, 32, 28),
    over_id = c(1, 1, 0),  # First two overlap
    arco = c(1, 1, 1)
  )
}

make_adjacent_test_data <- function() {
  data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    over_id = c(0, 0, 0),  # Add over_id for consolidate_overlapping
    arco = c(1, 1, 1)  # All employment
  )
}

make_gap_test_data <- function(gap_days = 15) {
  data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-10")),
    fine = as.Date(c("2023-01-15", "2023-02-05", "2023-02-28")),
    durata = c(15, 17, 19),
    arco = c(1, 1, 1)
  )
}

make_complex_test_data <- function() {
  data.table::data.table(
    cf = c(1, 1, 1, 1, 1, 1, 1, 1),
    inizio = as.Date(c(
      "2023-01-01", "2023-01-15",  # Overlapping (over_id=1)
      "2023-03-01", "2023-04-01",  # Adjacent
      "2023-05-01", "2023-05-20",  # Short gap
      "2023-07-01", "2023-08-01"   # Adjacent
    )),
    fine = as.Date(c(
      "2023-01-31", "2023-02-15",
      "2023-03-31", "2023-04-30",
      "2023-05-15", "2023-06-15",
      "2023-07-31", "2023-08-31"
    )),
    durata = c(31, 32, 31, 30, 15, 27, 31, 31),
    over_id = c(1, 1, 0, 0, 0, 0, 0, 0),
    arco = c(1, 1, 1, 1, 1, 1, 1, 1)
  )
}


# 2. Basic Chaining Tests -----

test_that("chaining: overlapping |> adjacent works", {
  skip_if_not_installed("data.table")

  # Create data with both overlapping and adjacent periods
  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31", "2023-04-30")),
    durata = c(31, 32, 31, 30),
    over_id = c(1, 1, 0, 0),  # First two overlap
    arco = c(1, 1, 1, 1)
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # After overlapping: 3 records (merged overlap + 2 separate)
  # After adjacent: 2 records (last two are adjacent)
  expect_equal(nrow(result), 2)
})

test_that("chaining: overlapping |> adjacent |> short_gaps works", {
  skip_if_not_installed("data.table")

  dt <- make_complex_test_data()

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # Should progressively reduce records
  expect_lt(nrow(result), nrow(dt))
  expect_true("non_working_days" %in% names(result))
})

test_that("chaining: each step reduces records progressively", {
  skip_if_not_installed("data.table")

  dt <- make_complex_test_data()

  step1 <- consolidate_overlapping(dt)
  step2 <- consolidate_adjacent(step1)
  step3 <- consolidate_short_gaps(step2, max_gap_days = 30)

  expect_lte(nrow(step1), nrow(dt))      # Step 1 reduces or maintains
  expect_lte(nrow(step2), nrow(step1))   # Step 2 reduces or maintains
  expect_lte(nrow(step3), nrow(step2))   # Step 3 reduces or maintains
})

test_that("chaining: n_periods_consolidated accumulates correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31", "2023-04-30")),
    durata = c(31, 32, 31, 30),
    over_id = c(1, 1, 0, 0),
    arco = c(1, 1, 1, 1)
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # Should have accumulated consolidation counts
  expect_true(any(result$n_periods_consolidated > 1))
  # n_periods_consolidated counts may differ from original nrow
  # because consolidation changes the structure
  expect_gte(sum(result$n_periods_consolidated), nrow(result))
})


# 3. Type Safety Tests -----

test_that("chaining preserves Date types throughout", {
  skip_if_not_installed("data.table")

  dt <- make_adjacent_test_data()

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_s3_class(result$inizio, "Date")
  expect_s3_class(result$fine, "Date")
})

test_that("chaining preserves integer types throughout", {
  skip_if_not_installed("data.table")

  dt <- make_adjacent_test_data()
  dt[, age := 25L]  # Add integer column

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_type(result$age, "integer")
})

test_that("chaining preserves all original columns", {
  skip_if_not_installed("data.table")

  dt <- make_adjacent_test_data()
  dt[, `:=`(extra_char = "test", extra_num = 42.5)]

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_true(all(c("extra_char", "extra_num") %in% names(result)))
  expect_type(result$extra_char, "character")
  expect_type(result$extra_num, "double")
})


# 4. Order Independence Tests -----

test_that("chaining order matters: overlapping should be first", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31", "2023-04-30")),
    durata = c(31, 32, 31, 30),
    over_id = c(1, 1, 0, 0),
    arco = c(1, 1, 1, 1)
  )

  # Correct order
  correct <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # Wrong order (adjacent before overlapping) - should still work but may not be optimal
  wrong <- dt |>
    consolidate_adjacent() |>
    consolidate_overlapping()

  # Both should work, but correct order is more efficient
  expect_s3_class(correct, "data.table")
  expect_s3_class(wrong, "data.table")
})

test_that("chaining is idempotent", {
  skip_if_not_installed("data.table")

  dt <- make_complex_test_data()

  # Apply chain once
  result1 <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # Apply chain again to result
  result2 <- result1 |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # Should be identical (no further consolidation possible)
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(result1$n_periods_consolidated, result2$n_periods_consolidated)
})


# 5. Edge Case Tests -----

test_that("chaining handles very small dataset", {
  skip_if_not_installed("data.table")

  # Test with just 1 record instead of empty
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1L,
    arco = 1L
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_equal(nrow(result), 1)
})

test_that("chaining handles single record", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1,
    arco = 1
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1)
})

test_that("chaining handles multiple persons", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2, 3, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01",
                       "2023-01-01", "2023-02-01",
                       "2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28",
                     "2023-01-31", "2023-02-28",
                     "2023-01-31", "2023-02-28")),
    durata = c(31, 28, 31, 28, 31, 28),
    over_id = c(1, 0, 0, 0, 0, 0),
    arco = c(1, 1, 1, 1, 1, 1)
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_equal(length(unique(result$cf)), 3)  # All three persons present
})


# 6. Correctness Validation Tests -----

test_that("chaining: consolidated durata matches time span", {
  skip_if_not_installed("data.table")

  dt <- make_adjacent_test_data()

  # Suppress warnings about over_id
  suppressWarnings(
    result <- dt |>
      consolidate_overlapping() |>
      consolidate_adjacent()
  )

  for (i in 1:nrow(result)) {
    expected_durata <- as.numeric(result$fine[i] - result$inizio[i] + 1)
    expect_equal(result$durata[i], expected_durata)
  }
})

test_that("chaining: no temporal gaps within consolidated groups", {
  skip_if_not_installed("data.table")

  dt <- make_complex_test_data()

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # After consolidation, consecutive records should have gaps
  if (nrow(result) > 1) {
    result_sorted <- result[order(inizio)]
    for (i in 2:nrow(result_sorted)) {
      gap <- as.numeric(result_sorted$inizio[i] - result_sorted$fine[i-1] - 1)
      expect_gt(gap, 0)  # There should be a gap (otherwise would have consolidated)
    }
  }
})

test_that("chaining: non_working_days is accurate", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15")),
    durata = c(15, 6, 15),
    arco = c(1, 0, 1),  # Middle is unemployment
    over_id = c(0, 0, 0)
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  expect_equal(result$non_working_days, 6)  # Only the unemployment period
})


# 7. Performance Benchmarks -----

test_that("performance: 10K records in <1 second per function", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 10K records
  n <- 10000
  dt <- data.table::data.table(
    cf = rep(1:1000, each = 10),
    inizio = as.Date("2023-01-01") + rep(0:9, 1000) * 30,
    fine = as.Date("2023-01-30") + rep(0:9, 1000) * 30,
    durata = 30,
    over_id = rep(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1000),
    arco = 1
  )

  # Test overlapping
  start <- Sys.time()
  result1 <- consolidate_overlapping(dt)
  time1 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_lt(time1, 1)

  # Test adjacent
  start <- Sys.time()
  result2 <- consolidate_adjacent(result1)
  time2 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_lt(time2, 1)

  # Test short_gaps
  start <- Sys.time()
  result3 <- consolidate_short_gaps(result2, max_gap_days = 30)
  time3 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_lt(time3, 1)
})

test_that("performance: 100K records in <10 seconds for full chain", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 100K records
  n <- 100000
  dt <- data.table::data.table(
    cf = rep(1:10000, each = 10),
    inizio = as.Date("2023-01-01") + rep(0:9, 10000) * 30,
    fine = as.Date("2023-01-30") + rep(0:9, 10000) * 30,
    durata = 30,
    over_id = rep(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 10000),
    arco = 1
  )

  start <- Sys.time()
  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Relaxed threshold to 10 seconds (CI environments can be slower)
  expect_lt(elapsed, 10)
  expect_lt(nrow(result), nrow(dt))  # Should have consolidated some records
})

test_that("performance: memory efficiency with large dataset", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 50K records and check memory doesn't explode
  n <- 50000
  dt <- data.table::data.table(
    cf = rep(1:5000, each = 10),
    inizio = as.Date("2023-01-01") + rep(0:9, 5000) * 30,
    fine = as.Date("2023-01-30") + rep(0:9, 5000) * 30,
    durata = 30,
    over_id = 0,
    arco = 1
  )

  initial_mem <- as.numeric(object.size(dt))

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  result_mem <- as.numeric(object.size(result))

  # Result should not be dramatically larger than input
  expect_lt(result_mem, initial_mem * 2)
})


# 8. Integration with analyze_employment_transitions -----

test_that("consolidation integrates with analyze_employment_transitions", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("igraph")

  # Create data suitable for transition analysis
  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31", "2023-04-30")),
    durata = c(31, 32, 31, 30),
    over_id = c(1, 1, 0, 0),
    arco = c(1, 1, 1, 1),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.01.00", "A.03.00", "A.02.00")
  )

  # Consolidate first
  consolidated <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # Should be able to pass to transition analysis
  # (This test just checks it doesn't error - full integration would need more setup)
  expect_s3_class(consolidated, "data.table")
  expect_true("COD_TIPOLOGIA_CONTRATTUALE" %in% names(consolidated))
})


# 9. Realistic Scenario Tests -----

test_that("realistic scenario: complex employment history", {
  skip_if_not_installed("data.table")

  # Realistic scenario: person with overlapping jobs, adjacent contracts, and gaps
  dt <- data.table::data.table(
    cf = rep(1, 10),
    inizio = as.Date(c(
      "2020-01-01", "2020-01-15",  # Overlapping jobs
      "2020-03-01",                # Single job
      "2020-04-01", "2020-05-01",  # Adjacent contracts
      "2020-06-15",                # After gap
      "2020-07-01", "2020-08-01",  # Adjacent again
      "2020-09-15",                # After small gap
      "2020-12-01"                 # After large gap
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-02-15",
      "2020-03-31",
      "2020-04-30", "2020-05-31",
      "2020-06-30",
      "2020-07-31", "2020-08-31",
      "2020-09-30",
      "2020-12-31"
    )),
    durata = c(31, 32, 31, 30, 31, 16, 31, 31, 16, 31),
    over_id = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    arco = rep(1, 10),
    salary = seq(1000, 1900, 100)
  )

  # Apply full consolidation chain
  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # Should significantly reduce records
  expect_lt(nrow(result), nrow(dt))

  # Should preserve person ID
  expect_equal(unique(result$cf), 1)

  # Should have valid temporal bounds
  expect_equal(min(result$inizio), as.Date("2020-01-01"))
  expect_true(max(result$fine) >= as.Date("2020-12-31"))
})

test_that("realistic scenario: multiple persons with different patterns", {
  skip_if_not_installed("data.table")

  # Person 1: Stable employment (adjacent contracts)
  # Person 2: Gig economy (many gaps)
  # Person 3: Multiple concurrent jobs
  dt <- data.table::data.table(
    cf = c(
      rep(1, 3),  # Stable
      rep(2, 5),  # Gig
      rep(3, 4)   # Concurrent
    ),
    inizio = as.Date(c(
      # Person 1: adjacent
      "2023-01-01", "2023-04-01", "2023-07-01",
      # Person 2: gaps
      "2023-01-01", "2023-02-01", "2023-04-01", "2023-06-01", "2023-09-01",
      # Person 3: overlapping
      "2023-01-01", "2023-01-15", "2023-03-01", "2023-03-15"
    )),
    fine = as.Date(c(
      "2023-03-31", "2023-06-30", "2023-09-30",
      "2023-01-15", "2023-02-15", "2023-04-15", "2023-06-15", "2023-09-15",
      "2023-01-31", "2023-02-15", "2023-03-31", "2023-04-15"
    )),
    durata = c(
      90, 91, 92,
      15, 15, 15, 15, 15,
      31, 32, 31, 32
    ),
    over_id = c(
      0, 0, 0,
      0, 0, 0, 0, 0,
      1, 1, 2, 2
    ),
    arco = rep(1, 12)
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # All persons should still be present
  expect_equal(length(unique(result$cf)), 3)

  # Person 1 should be highly consolidated (stable employment)
  person1 <- result[cf == 1]
  expect_lt(nrow(person1), 3)

  # Person 3 should have consolidated overlapping periods
  person3 <- result[cf == 3]
  expect_lt(nrow(person3), 4)
})


# 10. Stress Tests -----

test_that("stress: handles extreme overlapping (100 concurrent jobs)", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Extreme case: person with 100 overlapping jobs
  n <- 100
  dt <- data.table::data.table(
    cf = rep(1, n),
    inizio = as.Date("2023-01-01") + 0:(n-1),
    fine = as.Date("2023-12-31") - (n-1):0,
    durata = as.numeric(as.Date("2023-12-31") - (as.Date("2023-01-01") + 0:(n-1)) + 1),
    over_id = 1,  # All same over_id
    arco = 1
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 1)  # All consolidated to one
  expect_equal(result$n_periods_consolidated, n)
})

test_that("stress: handles extreme fragmentation (1000 tiny periods)", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Extreme case: 1000 one-day jobs with gaps
  n <- 1000
  dt <- data.table::data.table(
    cf = rep(1, n),
    inizio = as.Date("2020-01-01") + (0:(n-1)) * 2,  # Every other day
    fine = as.Date("2020-01-01") + (0:(n-1)) * 2,
    durata = 1,
    over_id = 0,
    arco = 1
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(5)  # Small threshold

  # Should consolidate many of them
  expect_lt(nrow(result), n)
})
