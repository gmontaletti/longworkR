# Tests for consolidate_employer_gaps()
# Verifies equivalence with sequential pipeline and edge cases

test_that("equivalence with sequential pipeline on sample data (first)", {
  skip_if_not(file.exists("../../data/sample.rds"), "sample.rds not available")

  data <- readRDS("../../data/sample.rds")

  ref <- data |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 30, variable_handling = "first")

  result <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first"
  )

  expect_equal(nrow(result), nrow(ref))
  expect_equal(names(result), names(ref))
  expect_equal(result, ref)
})

test_that("equivalence with sequential pipeline on sample data (weight)", {
  skip_if_not(file.exists("../../data/sample.rds"), "sample.rds not available")

  data <- readRDS("../../data/sample.rds")

  ref <- data |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "weight"
    ) |>
    consolidate_short_gaps(max_gap_days = 30, variable_handling = "weight")

  result <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "weight"
  )

  expect_equal(nrow(result), nrow(ref))
  expect_equal(result, ref)
})

test_that("equivalence with different gap thresholds (8/90)", {
  skip_if_not(file.exists("../../data/sample.rds"), "sample.rds not available")

  data <- readRDS("../../data/sample.rds")

  ref <- data |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 90, variable_handling = "first")

  result <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 90,
    variable_handling = "first"
  )

  expect_equal(result, ref)
})

test_that("equivalence with different gap thresholds (0/8)", {
  skip_if_not(file.exists("../../data/sample.rds"), "sample.rds not available")

  data <- readRDS("../../data/sample.rds")

  ref <- data |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 0,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 8, variable_handling = "first")

  result <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 0,
    gap_max_gap_days = 8,
    variable_handling = "first"
  )

  expect_equal(result, ref)
})

test_that("engine regression: v2 and v1 produce identical output", {
  skip_if_not(file.exists("../../data/sample.rds"), "sample.rds not available")

  data <- readRDS("../../data/sample.rds")

  result_v2 <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first",
    engine = "v2"
  )

  result_v1 <- consolidate_employer_gaps(
    data,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first",
    engine = "v1"
  )

  expect_equal(nrow(result_v2), nrow(result_v1))
  # Structural equivalence: same columns, same dimensions
  expect_equal(sort(names(result_v2)), sort(names(result_v1)))
  # Key columns must match
  expect_equal(result_v2$cf, result_v1$cf)
  expect_equal(result_v2$inizio, result_v1$inizio)
  expect_equal(result_v2$fine, result_v1$fine)
  expect_equal(
    result_v2$n_periods_consolidated,
    result_v1$n_periods_consolidated
  )
  expect_equal(result_v2$non_working_days, result_v1$non_working_days)
})

test_that("synthetic data: multi-person with unemployment barriers", {
  dt <- data.table::data.table(
    cf = c(rep(1L, 5), rep(2L, 3)),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-04-05",
      "2023-05-01",
      "2023-06-01",
      "2023-09-01",
      "2023-01-01",
      "2023-02-01",
      "2023-03-01"
    )),
    fine = as.Date(c(
      "2023-03-31",
      "2023-04-30",
      "2023-05-31",
      "2023-06-30",
      "2023-09-30",
      "2023-01-31",
      "2023-02-28",
      "2023-03-31"
    )),
    durata = c(90L, 26L, 31L, 30L, 30L, 31L, 28L, 31L),
    arco = c(1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L),
    datore = c("A", NA, "A", "A", "B", "X", "X", "X")
  )

  ref <- dt |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 30, variable_handling = "first")

  result <- consolidate_employer_gaps(
    dt,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first"
  )

  expect_equal(result, ref)
})

test_that("synthetic data: employer changes prevent consolidation", {
  dt <- data.table::data.table(
    cf = rep(1L, 3),
    inizio = as.Date(c("2023-01-01", "2023-01-05", "2023-01-10")),
    fine = as.Date(c("2023-01-04", "2023-01-09", "2023-01-15")),
    durata = c(4L, 5L, 6L),
    arco = c(1L, 1L, 1L),
    datore = c("A", "B", "A")
  )

  ref <- dt |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 30, variable_handling = "first")

  result <- consolidate_employer_gaps(
    dt,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first"
  )

  expect_equal(result, ref)
})

test_that("edge case: empty data", {
  dt <- data.table::data.table(
    cf = integer(),
    inizio = as.Date(character()),
    fine = as.Date(character()),
    durata = integer(),
    arco = integer(),
    datore = character()
  )

  result <- consolidate_employer_gaps(dt, "datore")

  expect_true(data.table::is.data.table(result))
  expect_equal(nrow(result), 0L)
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true("non_working_days" %in% names(result))
})

test_that("edge case: single record", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L,
    arco = 1L,
    datore = "A"
  )

  result <- consolidate_employer_gaps(dt, "datore")

  expect_equal(nrow(result), 1L)
  expect_equal(result$n_periods_consolidated, 1L)
  expect_equal(result$non_working_days, 0L)
})

test_that("edge case: all unemployment", {
  dt <- data.table::data.table(
    cf = rep(1L, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31L, 28L, 31L),
    arco = c(0L, 0L, 0L),
    datore = c("A", "A", "A")
  )

  ref <- dt |>
    consolidate_by_employer(
      "datore",
      max_gap_days = 8,
      variable_handling = "first"
    ) |>
    consolidate_short_gaps(max_gap_days = 30, variable_handling = "first")

  result <- consolidate_employer_gaps(
    dt,
    "datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30,
    variable_handling = "first"
  )

  expect_equal(result, ref)
})

test_that("input validation: missing employer_var", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L
  )
  expect_error(consolidate_employer_gaps(dt), "employer_var must be")
})

test_that("input validation: bad employer_var column", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L
  )
  expect_error(
    consolidate_employer_gaps(dt, "nonexistent"),
    "not found in data columns"
  )
})

test_that("input validation: negative employer_max_gap_days", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L,
    datore = "A"
  )
  expect_error(
    consolidate_employer_gaps(dt, "datore", employer_max_gap_days = -1),
    "employer_max_gap_days must be"
  )
})

test_that("input validation: negative gap_max_gap_days", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L,
    datore = "A"
  )
  expect_error(
    consolidate_employer_gaps(dt, "datore", gap_max_gap_days = -1),
    "gap_max_gap_days must be"
  )
})

test_that("input validation: non-data.table input", {
  df <- data.frame(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L,
    datore = "A"
  )
  expect_error(
    consolidate_employer_gaps(df, "datore"),
    "must be a data.table"
  )
})

test_that("input validation: missing required columns", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    datore = "A"
  )
  expect_error(
    consolidate_employer_gaps(dt, "datore"),
    "Missing required columns"
  )
})

test_that("input validation: bad variable_handling", {
  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31L,
    datore = "A"
  )
  expect_error(
    consolidate_employer_gaps(dt, "datore", variable_handling = "bad"),
    "variable_handling must be"
  )
})

test_that("original data is not modified", {
  dt <- data.table::data.table(
    cf = rep(1L, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31L, 28L, 31L),
    arco = c(1L, 1L, 1L),
    datore = c("A", "A", "A")
  )

  original_copy <- data.table::copy(dt)
  consolidate_employer_gaps(dt, "datore")

  expect_equal(dt, original_copy)
})

test_that("arco column added if missing", {
  dt <- data.table::data.table(
    cf = rep(1L, 2),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31L, 28L),
    datore = c("A", "A")
  )

  result <- consolidate_employer_gaps(dt, "datore")

  expect_true("arco" %in% names(result))
  expect_true(nrow(result) > 0)
})
