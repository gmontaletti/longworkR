# Test Suite for consolidate_overlapping()
# Tests the function that merges concurrent employment periods

test_that("consolidate_overlapping requires data.table input", {
  skip_if_not_installed("data.table")

  df <- data.frame(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1
  )

  expect_error(
    consolidate_overlapping(df),
    "Input must be a data.table"
  )
})

test_that("consolidate_overlapping requires mandatory columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01")
    # Missing: fine, durata
  )

  expect_error(
    consolidate_overlapping(dt),
    "Missing required columns"
  )
})

test_that("consolidate_overlapping warns when over_id is missing", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31
  )

  expect_warning(
    result <- consolidate_overlapping(dt),
    "over_id.*not found"
  )

  expect_equal(nrow(result), nrow(dt))  # Returns original data
})

test_that("consolidate_overlapping merges periods with same over_id > 0", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-28")),
    durata = c(31, 32, 28),
    over_id = c(1, 1, 2)  # First two overlap
  )

  result <- consolidate_overlapping(dt)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Two groups
  expect_equal(result$n_periods_consolidated, c(2, 1))
})

test_that("consolidate_overlapping does NOT merge over_id = 0", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    over_id = c(0, 0, 0)  # All zero - no consolidation
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 3)  # No consolidation
  expect_equal(result$n_periods_consolidated, c(1, 1, 1))
})

test_that("consolidate_overlapping handles over_id with mixed values", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    over_id = c(0, 5, 0)  # Mixed zero and valid
  )

  result <- consolidate_overlapping(dt)

  # Only the record with over_id=5 would be consolidated
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)  # Each over_id=0 stays separate, over_id=5 is also separate
})

test_that("consolidate_overlapping computes temporal bounds correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-01-10")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-20")),
    durata = c(31, 32, 42),
    over_id = c(5, 5, 5)  # All same over_id
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 1)
  expect_equal(result$inizio, as.Date("2023-01-01"))  # Earliest
  expect_equal(result$fine, as.Date("2023-02-20"))    # Latest
  expect_equal(result$durata, 51)  # 2023-02-20 - 2023-01-01 + 1
})

test_that("consolidate_overlapping handles multiple persons separately", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-01-31", "2023-02-15")),
    durata = c(31, 32, 31, 32),
    over_id = c(1, 1, 2, 2)  # Each person has their own over_id
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 2)  # One per person
  expect_equal(sort(unique(result$cf)), c(1, 2))
})

test_that("consolidate_overlapping creates arco column if missing", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15")),
    durata = c(31, 32),
    over_id = c(1, 1)
    # No arco column
  )

  result <- consolidate_overlapping(dt)

  expect_true("arco" %in% names(result))
  expect_equal(result$arco, 1)  # over_id > 0 means employment
})

test_that("consolidate_overlapping preserves existing arco column", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15")),
    durata = c(31, 32),
    over_id = c(1, 1),
    arco = c(2, 2)  # Existing arco values
  )

  result <- consolidate_overlapping(dt)

  expect_equal(result$arco, 2)  # Max of group
})

test_that("consolidate_overlapping handles single record", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1)
})

test_that("consolidate_overlapping handles very small dataset", {
  skip_if_not_installed("data.table")

  # Test with just 1 record instead of empty
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1L
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 1)
  expect_true("n_periods_consolidated" %in% names(result))
  expect_equal(result$n_periods_consolidated, 1)
})

test_that("consolidate_overlapping preserves column types", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1L,  # integer
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    over_id = 1L,
    age = 25L,  # integer
    salary = 50000.50,  # numeric
    contract = "permanent",  # character
    over_id = 1
  )

  result <- consolidate_overlapping(dt)

  expect_type(result$cf, "integer")
  expect_type(result$age, "integer")
  expect_type(result$salary, "double")
  expect_type(result$contract, "character")
})

test_that("consolidate_overlapping uses weighted aggregation", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    salary = c(1000, 2000),  # Weighted mean: (1000*31 + 2000*28) / 59
    over_id = c(1, 1)
  )

  result <- consolidate_overlapping(dt)

  expected_salary <- (1000*31 + 2000*28) / 59
  expect_equal(result$salary, expected_salary, tolerance = 0.01)
})

test_that("consolidate_overlapping handles mixed over_id values", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31", "2023-04-30")),
    durata = c(31, 32, 31, 30),
    over_id = c(1, 1, 0, 2)  # Group 1 (2 periods), single 0, group 2 (1 period)
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 3)  # Two consolidated, one single zero, one single non-zero
  expect_true(any(result$n_periods_consolidated == 2))
})

test_that("consolidate_overlapping does not modify original data", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15")),
    durata = c(31, 32),
    over_id = c(1, 1)
  )

  original_rows <- nrow(dt)
  result <- consolidate_overlapping(dt)

  expect_equal(nrow(dt), original_rows)  # Original unchanged
})

test_that("consolidate_overlapping returns all original columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15")),
    durata = c(31, 32),
    over_id = c(1, 1),
    extra1 = c("A", "B"),
    extra2 = c(100, 200)
  )

  result <- consolidate_overlapping(dt)

  expect_true(all(c("cf", "inizio", "fine", "durata", "over_id", "extra1", "extra2") %in% names(result)))
  expect_true("n_periods_consolidated" %in% names(result))
})

test_that("consolidate_overlapping handles different over_id per person", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-01-31", "2023-02-15")),
    durata = c(31, 32, 31, 32),
    over_id = c(5, 5, 10, 10)  # Different over_id values per person
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 2)  # One per person
  person1 <- result[cf == 1]
  person2 <- result[cf == 2]
  expect_equal(person1$n_periods_consolidated, 2)
  expect_equal(person2$n_periods_consolidated, 2)
})

test_that("consolidate_overlapping correctly handles over_id selection", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-28")),
    durata = c(31, 32, 28),
    over_id = c(0, 5, 0)  # Mixed: should pick first non-zero (5)
  )

  # This should NOT consolidate all three (over_id logic only groups same values)
  result <- consolidate_overlapping(dt)

  # The first and third have over_id=0 (each unique), second has over_id=5
  expect_equal(nrow(result), 3)
})

test_that("consolidate_overlapping with multiple overlapping groups", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15",  # Group 1
                       "2023-03-01", "2023-03-15",  # Group 2
                       "2023-05-01", "2023-05-15")),  # Group 3
    fine = as.Date(c("2023-01-31", "2023-02-15",
                     "2023-03-31", "2023-04-15",
                     "2023-05-31", "2023-06-15")),
    durata = c(31, 32, 31, 32, 31, 32),
    over_id = c(1, 1, 2, 2, 3, 3)  # Three distinct groups
  )

  result <- consolidate_overlapping(dt)

  expect_equal(nrow(result), 3)  # Three consolidated groups
  expect_equal(result$n_periods_consolidated, c(2, 2, 2))
  expect_equal(result$over_id, c(1, 2, 3))
})

test_that("consolidate_overlapping performance with larger dataset", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 1000 records with overlapping periods
  n <- 1000
  dt <- data.table::data.table(
    cf = rep(1:100, each = 10),
    inizio = as.Date("2023-01-01") + rep(0:9, 100) * 10,
    fine = as.Date("2023-01-31") + rep(0:9, 100) * 10,
    durata = 31,
    over_id = rep(1:500, each = 2)  # Every 2 records share over_id
  )

  start_time <- Sys.time()
  result <- consolidate_overlapping(dt)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 2)  # Should complete in under 2 seconds
  expect_equal(nrow(result), 500)  # Half the records (pairs consolidated)
})
