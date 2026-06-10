# Tests for employment summary utilities (employment_summaries.R)

# 1. compare_consolidation_stages -----

test_that("compare_consolidation_stages summarizes records and persons per stage", {
  raw <- data.table::data.table(
    cf = c("A", "A", "B", "C", "C"),
    durata = c(10, 20, 30, 40, 50)
  )
  consolidated <- data.table::data.table(
    cf = c("A", "B", "C"),
    durata = c(30, 30, 90)
  )

  result <- compare_consolidation_stages(
    list(raw = raw, consolidated = consolidated)
  )

  expect_s3_class(result, "data.table")
  expect_named(result, c("stage", "records", "unique_persons"))
  expect_equal(result$stage, c("raw", "consolidated"))
  expect_equal(result$records, c(5L, 3L))
  expect_equal(result$unique_persons, c(3L, 3L))
})

test_that("compare_consolidation_stages handles a single empty stage", {
  empty <- data.table::data.table(cf = character(0))

  result <- compare_consolidation_stages(list(empty = empty))

  expect_equal(result$records, 0L)
  expect_equal(result$unique_persons, 0L)
})

test_that("compare_consolidation_stages errors on invalid input", {
  dt <- data.table::data.table(cf = "A")

  # Not a list
  expect_error(
    compare_consolidation_stages(dt),
    "must be a list"
  )

  # Empty list
  expect_error(
    compare_consolidation_stages(list()),
    "at least one"
  )

  # Unnamed stages
  expect_error(
    compare_consolidation_stages(list(dt)),
    "named"
  )

  # Missing cf column
  expect_error(
    compare_consolidation_stages(
      list(ok = dt, bad = data.table::data.table(x = 1))
    ),
    "bad"
  )
})

# 2. employment_statistics -----

test_that("employment_statistics computes correct totals and averages", {
  dt <- data.table::data.table(
    cf = c("A", "A", "A", "B", "B"),
    durata = c(100, 30, 200, 60, NA_real_),
    arco = c(1, 0, 1, 0, 1)
  )

  stats <- employment_statistics(dt)

  expect_type(stats, "list")
  expect_named(
    stats,
    c(
      "total_records",
      "unique_persons",
      "total_employment_days",
      "total_unemployment_days",
      "avg_employment_duration",
      "avg_unemployment_duration"
    )
  )
  expect_equal(stats$total_records, 5L)
  expect_equal(stats$unique_persons, 2L)
  expect_equal(stats$total_employment_days, 300)
  expect_equal(stats$total_unemployment_days, 90)
  expect_equal(stats$avg_employment_duration, 150)
  expect_equal(stats$avg_unemployment_duration, 45)
})

test_that("employment_statistics handles data with no unemployment spells", {
  dt <- data.table::data.table(
    cf = c("A", "B"),
    durata = c(10, 20),
    arco = c(1, 1)
  )

  stats <- employment_statistics(dt)

  expect_equal(stats$total_unemployment_days, 0)
  expect_true(is.nan(stats$avg_unemployment_duration))
})

test_that("employment_statistics errors on invalid input", {
  # Not a data.frame
  expect_error(
    employment_statistics(list(cf = "A")),
    "data.table or data.frame"
  )

  # Missing required columns
  expect_error(
    employment_statistics(data.table::data.table(cf = "A", durata = 1)),
    "arco"
  )
})
