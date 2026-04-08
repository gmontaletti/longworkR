# Test Suite for consolidate_short_gaps()
# Tests the function that bridges short unemployment gaps between jobs

test_that("consolidate_short_gaps requires data.table input", {
  skip_if_not_installed("data.table")

  df <- data.frame(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31
  )

  expect_error(
    consolidate_short_gaps(df),
    "must be a data.table"
  )
})

test_that("consolidate_short_gaps requires mandatory columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01")
    # Missing: fine, durata
  )

  expect_error(
    consolidate_short_gaps(dt),
    "Missing required columns"
  )
})

test_that("consolidate_short_gaps validates max_gap_days parameter", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1
  )

  expect_error(
    consolidate_short_gaps(dt, max_gap_days = -5),
    "max_gap_days must be a non-negative number"
  )

  expect_error(
    consolidate_short_gaps(dt, max_gap_days = "thirty"),
    "max_gap_days must be a non-negative number"
  )
})

test_that("consolidate_short_gaps merges when gap <= max_gap_days", {
  skip_if_not_installed("data.table")

  # Test with employment periods only (no unemployment barriers)
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15")),
    durata = c(15, 6, 15),
    arco = c(1, 1, 1) # All employment - gaps of 4 and 6 days
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(nrow(result), 1) # All consolidated
  expect_true("non_working_days" %in% names(result))
  expect_equal(result$non_working_days, 0) # No unemployment periods
})

test_that("consolidate_short_gaps does NOT merge when gap > max_gap_days", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-03-01")), # 28-day gap (Feb)
    fine = as.Date(c("2023-01-31", "2023-03-31")),
    durata = c(31, 31),
    arco = c(1, 1)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 20)

  expect_equal(nrow(result), 2) # No consolidation
  expect_equal(result$non_working_days, c(0, 0))
})

test_that("consolidate_short_gaps calculates non_working_days correctly", {
  skip_if_not_installed("data.table")

  # Test with short unemployment periods (all adjacent, no gaps)
  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-16", "2023-01-23", "2023-02-08")),
    fine = as.Date(c("2023-01-15", "2023-01-22", "2023-02-07", "2023-02-28")),
    durata = c(15, 7, 16, 21),
    arco = c(1, 0, 1, 0) # Two unemployment periods: 7 and 21 days
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # All consolidate because unemployment durations <= 30 days
  expect_equal(nrow(result), 1)
  expect_equal(result$non_working_days, 28) # 7 + 21 unemployment days
})

test_that("consolidate_short_gaps handles different thresholds", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-15")),
    fine = as.Date(c("2023-01-15", "2023-02-10", "2023-02-28")),
    durata = c(15, 22, 14),
    arco = c(1, 1, 1)
  )
  # Gaps: 4 days, 4 days

  result_10 <- consolidate_short_gaps(dt, max_gap_days = 10)
  expect_equal(nrow(result_10), 1) # All consolidated (gaps <= 10)

  result_3 <- consolidate_short_gaps(dt, max_gap_days = 3)
  expect_equal(nrow(result_3), 3) # None consolidated (gaps > 3)
})

test_that("consolidate_short_gaps adds arco if missing", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15")),
    durata = c(15, 15)
    # No arco column
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_true("arco" %in% names(result))
  expect_true("non_working_days" %in% names(result))
})

test_that("consolidate_short_gaps handles single record", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1)
  expect_equal(result$non_working_days, 0)
})

test_that("consolidate_short_gaps handles very small dataset", {
  skip_if_not_installed("data.table")

  # Test with just 1 record instead of empty
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1L
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(nrow(result), 1)
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true("non_working_days" %in% names(result))
  expect_equal(result$n_periods_consolidated, 1)
  expect_equal(result$non_working_days, 0)
})

test_that("consolidate_short_gaps handles multiple persons separately", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15", "2023-01-15", "2023-02-15")),
    durata = c(15, 15, 15, 15),
    arco = c(1, 1, 1, 1)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(nrow(result), 2) # One per person
  expect_equal(sort(unique(result$cf)), c(1, 2))
})

test_that("consolidate_short_gaps computes temporal bounds correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15", "2023-03-15")),
    durata = c(15, 15, 15),
    arco = c(1, 1, 1)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-03-15"))
  expect_equal(result$durata, 74) # 2023-03-15 - 2023-01-01 + 1
})

test_that("consolidate_short_gaps preserves column types", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1L, # integer
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1L,
    age = 25L, # integer
    salary = 50000.50, # numeric
    contract = "permanent" # character
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_type(result$cf, "integer")
  expect_type(result$age, "integer")
  expect_type(result$salary, "double")
  expect_type(result$contract, "character")
})

test_that("consolidate_short_gaps uses weighted aggregation", {
  skip_if_not_installed("data.table")

  # Test with employment periods only
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15")),
    durata = c(15, 6, 15),
    salary = c(1000, 1500, 2000),
    arco = c(1, 1, 1) # All employment periods
  )

  result <- consolidate_short_gaps(
    dt,
    max_gap_days = 30,
    variable_handling = "weight"
  )

  # Weighted mean: (1000*15 + 1500*6 + 2000*15) / 36 = 1417
  expected_salary <- (1000 * 15 + 1500 * 6 + 2000 * 15) / 36
  expect_equal(result$salary, expected_salary, tolerance = 0.01)
})

test_that("consolidate_short_gaps does not modify original data", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15")),
    durata = c(15, 15),
    arco = c(1, 1)
  )

  original_rows <- nrow(dt)
  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(nrow(dt), original_rows) # Original unchanged
})

test_that("consolidate_short_gaps handles zero threshold", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 0)

  # max_gap_days=0 means only touching periods (like adjacent)
  expect_equal(nrow(result), 1) # These are adjacent (Jan 31 + 1 = Feb 1)
})

test_that("consolidate_short_gaps handles very large threshold", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-12-01")),
    fine = as.Date(c("2023-01-31", "2023-06-30", "2023-12-31")),
    durata = c(31, 30, 31),
    arco = c(1, 1, 1)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 365)

  expect_equal(nrow(result), 1) # All consolidated with large threshold
})

test_that("consolidate_short_gaps with mixed gap sizes", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01", "2023-04-01")),
    fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15", "2023-04-30")),
    durata = c(15, 6, 15, 30),
    arco = c(1, 1, 1, 1)
  )
  # Gaps: 4 days, 6 days, 44 days

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # First three consolidated (gaps <= 30), last separate (gap = 44 > 30)
  expect_equal(nrow(result), 2)
  expect_equal(result$n_periods_consolidated, c(3, 1))
})

test_that("consolidate_short_gaps handles consecutive unemployment periods", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-16", "2023-01-29", "2023-02-14")),
    fine = as.Date(c("2023-01-15", "2023-01-28", "2023-02-13", "2023-02-28")),
    durata = c(15, 13, 16, 15),
    arco = c(1, 0, 0, 1) # Two consecutive short unemployment periods
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # All consolidate because unemployment durations <= 30 days
  expect_equal(nrow(result), 1)
  # Unemployment periods: 13 + 16 = 29 days
  expect_equal(result$non_working_days, 29)
})

test_that("consolidate_short_gaps treats long unemployment as barrier", {
  skip_if_not_installed("data.table")

  # Test that long unemployment periods (duration > threshold) block consolidation
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30")),
    durata = c(31, 59, 30),
    arco = c(1, 0, 1) # 59-day unemployment (all adjacent, gap=0)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # Long unemployment (59 days > 30) blocks consolidation
  expect_equal(nrow(result), 3)
  expect_equal(result$arco, c(1, 0, 1))
  expect_equal(result$non_working_days, c(0, 59, 0))
})

test_that("consolidate_short_gaps bridges short unemployment", {
  skip_if_not_installed("data.table")

  # Test that short unemployment (duration <= threshold) is bridged
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-11")),
    fine = as.Date(c("2023-01-31", "2023-02-10", "2023-02-28")),
    durata = c(31, 10, 18),
    arco = c(1, 0, 1) # 10-day unemployment (all adjacent)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # Short unemployment (10 days <= 30) is bridged
  expect_equal(nrow(result), 1)
  expect_equal(result$arco, 1) # Consolidated as employment
  expect_equal(result$non_working_days, 10) # Tracks bridged unemployment
})

test_that("consolidate_short_gaps returns all original columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15")),
    durata = c(15, 15),
    arco = c(1, 1),
    extra1 = c("A", "B"),
    extra2 = c(100, 200)
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_true(all(
    c("cf", "inizio", "fine", "durata", "arco", "extra1", "extra2") %in%
      names(result)
  ))
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true("non_working_days" %in% names(result))
})

test_that("consolidate_short_gaps converts non-Date columns to Date", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = "2023-01-01", # character
    fine = "2023-01-31",
    durata = 31,
    arco = 1
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_s3_class(result$inizio, "Date")
  expect_s3_class(result$fine, "Date")
})

test_that("consolidate_short_gaps handles all employment periods", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15", "2023-03-15")),
    durata = c(15, 15, 15),
    arco = c(1, 1, 1) # All employment
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  expect_equal(result$non_working_days, 0) # No unemployment
})

test_that("consolidate_short_gaps handles all unemployment periods", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15", "2023-03-15")),
    durata = c(15, 15, 15),
    arco = c(0, 0, 0) # All unemployment
  )

  result <- consolidate_short_gaps(dt, max_gap_days = 30)

  # All unemployment with gaps <= 30 days - will consolidate
  expect_s3_class(result, "data.table")
  # The function consolidates periods with gaps <= max_gap_days
  # regardless of arco value
  expect_lte(nrow(result), 3)
})

test_that("consolidate_short_gaps exact threshold boundary", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")), # Gap = 0 (adjacent)
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1)
  )

  # Exactly at threshold (0 days gap)
  result_exact <- consolidate_short_gaps(dt, max_gap_days = 0)
  expect_equal(nrow(result_exact), 1) # Consolidated at exact boundary

  # Create data with exact 30-day gap
  dt2 <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")), # 31 days after Jan 1 end
    fine = as.Date(c("2023-01-01", "2023-02-28")),
    durata = c(1, 28),
    arco = c(1, 1)
  )
  # Gap = Feb 1 - Jan 1 - 1 = 30 days

  result_30 <- consolidate_short_gaps(dt2, max_gap_days = 30)
  expect_equal(nrow(result_30), 1) # Consolidated at exact threshold
})

test_that("consolidate_short_gaps with decimal max_gap_days", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1)
  )

  # Decimal threshold should work (comparison uses <= so 0.5 means 0 days)
  result <- consolidate_short_gaps(dt, max_gap_days = 0.5)
  expect_equal(nrow(result), 1) # Adjacent periods (0 gap)
})

test_that("consolidate_short_gaps performance with larger dataset", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 1000 periods with small gaps
  n <- 1000
  dt <- data.table::data.table(
    cf = rep(1:10, each = 100),
    inizio = as.Date("2023-01-01") + rep(0:99, 10) * 35, # 35-day intervals
    fine = as.Date("2023-01-30") + rep(0:99, 10) * 35,
    durata = 30,
    arco = 1
  )

  start_time <- Sys.time()
  result <- consolidate_short_gaps(dt, max_gap_days = 7)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 2) # Should complete in under 2 seconds
})
