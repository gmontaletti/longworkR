# Test Suite for consolidate_adjacent()
# Tests the function that merges contiguous employment periods with no gap

test_that("consolidate_adjacent requires data.table input", {
  skip_if_not_installed("data.table")

  df <- data.frame(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31
  )

  expect_error(
    consolidate_adjacent(df),
    "data must be a data.table"
  )
})

test_that("consolidate_adjacent requires mandatory columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01")
    # Missing: fine, durata
  )

  expect_error(
    consolidate_adjacent(dt),
    "Missing required columns"
  )
})

test_that("consolidate_adjacent merges touching periods (no gap)", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(1, 1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)  # All three merged
  expect_equal(result$n_periods_consolidated, 3)
})

test_that("consolidate_adjacent does NOT merge periods with gap > 0", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05")),  # 4-day gap
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 24),
    arco = c(1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 2)  # No consolidation
  expect_equal(result$n_periods_consolidated, c(1, 1))
})

test_that("consolidate_adjacent identifies adjacency correctly", {
  skip_if_not_installed("data.table")

  # Jan 31 + 1 day = Feb 1 (adjacent)
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 1)  # Adjacent - merged
  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-02-28"))
})

test_that("consolidate_adjacent unemployment acts as barrier", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(1, 0, 1)  # Middle is unemployment
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 3)  # Unemployment blocks consolidation
  expect_equal(result$n_periods_consolidated, c(1, 1, 1))
})

test_that("consolidate_adjacent adds arco if missing", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28)
    # No arco column
  )

  result <- consolidate_adjacent(dt)

  expect_true("arco" %in% names(result))
  expect_equal(result$arco, 1)  # Defaults to employment
})

test_that("consolidate_adjacent handles single record", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1)
})

test_that("consolidate_adjacent handles very small dataset", {
  skip_if_not_installed("data.table")

  # Test with just 1 record instead of empty
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1L
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 1)
  expect_true("n_periods_consolidated" %in% names(result))
  expect_equal(result$n_periods_consolidated, 1)
})

test_that("consolidate_adjacent handles multiple persons separately", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-01-31", "2023-02-28")),
    durata = c(31, 28, 31, 28),
    arco = c(1, 1, 1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 2)  # One per person
  expect_equal(sort(unique(result$cf)), c(1, 2))
  expect_equal(result$n_periods_consolidated, c(2, 2))
})

test_that("consolidate_adjacent computes temporal bounds correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(1, 1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-03-31"))
  expect_equal(result$durata, 90)  # 2023-03-31 - 2023-01-01 + 1
})

test_that("consolidate_adjacent preserves column types", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1L,  # integer
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1L,
    age = 25L,  # integer
    salary = 50000.50,  # numeric
    contract = "permanent"  # character
  )

  result <- consolidate_adjacent(dt)

  expect_type(result$cf, "integer")
  expect_type(result$age, "integer")
  expect_type(result$salary, "double")
  expect_type(result$contract, "character")
})

test_that("consolidate_adjacent uses weighted aggregation", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    salary = c(1000, 2000),
    arco = c(1, 1)
  )

  result <- consolidate_adjacent(dt)

  expected_salary <- (1000*31 + 2000*28) / 59
  expect_equal(result$salary, expected_salary, tolerance = 0.01)
})

test_that("consolidate_adjacent does not modify original data", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1)
  )

  original_rows <- nrow(dt)
  result <- consolidate_adjacent(dt)

  expect_equal(nrow(dt), original_rows)  # Original unchanged
})

test_that("consolidate_adjacent handles partial adjacency", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-05", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-04-30")),
    durata = c(31, 28, 27, 30),
    arco = c(1, 1, 1, 1)
  )

  result <- consolidate_adjacent(dt)

  # First two adjacent, gap before third, but third-fourth adjacent
  expect_equal(nrow(result), 2)
})

test_that("consolidate_adjacent handles unemployment at start", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(0, 1, 1)  # First is unemployment
  )

  result <- consolidate_adjacent(dt)

  # Unemployment separate, last two adjacent employment
  expect_equal(nrow(result), 2)
})

test_that("consolidate_adjacent handles unemployment at end", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(1, 1, 0)  # Last is unemployment
  )

  result <- consolidate_adjacent(dt)

  # First two adjacent employment, last unemployment separate
  expect_equal(nrow(result), 2)
})

test_that("consolidate_adjacent converts non-Date columns to Date", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = "2023-01-01",  # character
    fine = "2023-01-31",
    durata = 31,
    arco = 1
  )

  result <- consolidate_adjacent(dt)

  expect_s3_class(result$inizio, "Date")
  expect_s3_class(result$fine, "Date")
})

test_that("consolidate_adjacent handles all unemployment periods", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(0, 0, 0)  # All unemployment
  )

  result <- consolidate_adjacent(dt)

  # No consolidation - each unemployment is separate
  expect_equal(nrow(result), 3)
  expect_equal(result$n_periods_consolidated, c(1, 1, 1))
})

test_that("consolidate_adjacent handles alternating employment/unemployment", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-04-30", "2023-05-31")),
    durata = c(31, 28, 31, 30, 31),
    arco = c(1, 0, 1, 0, 1)  # Alternating
  )

  result <- consolidate_adjacent(dt)

  # Each is separate due to unemployment barriers
  expect_equal(nrow(result), 5)
})

test_that("consolidate_adjacent returns all original columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1),
    extra1 = c("A", "B"),
    extra2 = c(100, 200)
  )

  result <- consolidate_adjacent(dt)

  expect_true(all(c("cf", "inizio", "fine", "durata", "arco", "extra1", "extra2") %in% names(result)))
  expect_true("n_periods_consolidated" %in% names(result))
})

test_that("consolidate_adjacent handles leap year correctly", {
  skip_if_not_installed("data.table")

  # Feb 29, 2024 (leap year) + 1 day = Mar 1, 2024
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2024-02-01", "2024-03-01")),
    fine = as.Date(c("2024-02-29", "2024-03-31")),
    durata = c(29, 31),
    arco = c(1, 1)
  )

  result <- consolidate_adjacent(dt)

  expect_equal(nrow(result), 1)  # Adjacent
  expect_equal(result$durata, 60)  # Feb 29 + Mar 31 days
})

test_that("consolidate_adjacent with mixed adjacent and non-adjacent", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-04-30", "2023-05-31")),
    durata = c(31, 28, 30, 31),
    arco = c(1, 1, 1, 1)
  )

  result <- consolidate_adjacent(dt)

  # First two adjacent (Jan-Feb), gap, last two adjacent (Apr-May)
  expect_equal(nrow(result), 2)
  expect_equal(result$n_periods_consolidated, c(2, 2))
})

test_that("consolidate_adjacent performance with larger dataset", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Create 1000 adjacent employment periods
  n <- 1000
  dt <- data.table::data.table(
    cf = rep(1:10, each = 100),
    inizio = as.Date("2023-01-01") + rep(0:99, 10) * 30,
    fine = as.Date("2023-01-30") + rep(0:99, 10) * 30,
    durata = 30,
    arco = 1
  )

  start_time <- Sys.time()
  result <- consolidate_adjacent(dt)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 2)  # Should complete in under 2 seconds
  expect_equal(nrow(result), 10)  # All 100 periods per person consolidated
})
