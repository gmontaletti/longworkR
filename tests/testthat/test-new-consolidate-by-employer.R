# Test Suite for consolidate_by_employer()
# Tests the function that merges consecutive employment periods with the same employer

# 1. Input validation -----

test_that("consolidate_by_employer rejects non-data.table input", {
  skip_if_not_installed("data.table")

  df <- data.frame(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(df, employer_var = "datore"),
    "data must be a data.table"
  )
})

test_that("consolidate_by_employer requires employer_var to be a character string", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(dt, employer_var = 123),
    "employer_var must be a single character string"
  )

  expect_error(
    consolidate_by_employer(dt, employer_var = c("a", "b")),
    "employer_var must be a single character string"
  )

  expect_error(
    consolidate_by_employer(dt),
    "employer_var must be a single character string"
  )
})

test_that("consolidate_by_employer requires employer_var column to exist in data", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(dt, employer_var = "nonexistent"),
    "employer_var 'nonexistent' not found in data columns"
  )
})

test_that("consolidate_by_employer requires non-negative numeric max_gap_days", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(dt, employer_var = "datore", max_gap_days = -1),
    "max_gap_days must be a single non-negative numeric value"
  )

  expect_error(
    consolidate_by_employer(dt, employer_var = "datore", max_gap_days = "five"),
    "max_gap_days must be a single non-negative numeric value"
  )

  expect_error(
    consolidate_by_employer(
      dt,
      employer_var = "datore",
      max_gap_days = c(1, 2)
    ),
    "max_gap_days must be a single non-negative numeric value"
  )
})

test_that("consolidate_by_employer requires valid variable_handling", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(
      dt,
      employer_var = "datore",
      variable_handling = "invalid"
    ),
    "variable_handling must be 'weight' or 'first'"
  )
})

test_that("consolidate_by_employer requires mandatory columns", {
  skip_if_not_installed("data.table")

  dt_no_fine <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(dt_no_fine, employer_var = "datore"),
    "Missing required columns.*fine"
  )

  dt_no_cf <- data.table::data.table(
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    datore = "A"
  )

  expect_error(
    consolidate_by_employer(dt_no_cf, employer_var = "datore"),
    "Missing required columns.*cf"
  )
})

# 2. Core consolidation logic -----

test_that("consolidate_by_employer merges same employer periods within max_gap_days", {
  skip_if_not_installed("data.table")

  # Two periods from employer A with a 5-day gap (within default 8)
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-06")),
    fine = as.Date(c("2023-01-31", "2023-03-31")),
    durata = c(31, 54),
    arco = c(1, 1),
    datore = c("A", "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 2L)
  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-03-31"))
})

test_that("consolidate_by_employer does NOT merge different employers", {
  skip_if_not_installed("data.table")

  # Two adjacent periods from different employers
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 1),
    datore = c("A", "B")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 2)
  expect_true(all(result$n_periods_consolidated == 1L))
})

test_that("consolidate_by_employer handles gap boundary exactly at threshold", {
  skip_if_not_installed("data.table")

  # Period 1 ends 2023-01-31, period 2 starts 2023-02-09

  # Gap = 2023-02-09 - 2023-01-31 - 1 = 8 days (exactly max_gap_days)
  dt_at <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-09")),
    fine = as.Date(c("2023-01-31", "2023-03-31")),
    durata = c(31, 51),
    arco = c(1, 1),
    datore = c("A", "A")
  )

  # Gap == 8 (threshold): should merge
  result_at <- consolidate_by_employer(
    dt_at,
    employer_var = "datore",
    max_gap_days = 8
  )
  expect_equal(
    nrow(result_at),
    1,
    info = "Gap exactly at threshold should be merged"
  )
  expect_equal(result_at$n_periods_consolidated, 2L)

  # Period 1 ends 2023-01-31, period 2 starts 2023-02-10
  # Gap = 2023-02-10 - 2023-01-31 - 1 = 9 days (one above threshold)
  dt_above <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-10")),
    fine = as.Date(c("2023-01-31", "2023-03-31")),
    durata = c(31, 50),
    arco = c(1, 1),
    datore = c("A", "A")
  )

  # Gap == 9 (above threshold): should NOT merge
  result_above <- consolidate_by_employer(
    dt_above,
    employer_var = "datore",
    max_gap_days = 8
  )
  expect_equal(
    nrow(result_above),
    2,
    info = "Gap one above threshold should NOT be merged"
  )
})

test_that("consolidate_by_employer handles multiple employers per person independently", {
  skip_if_not_installed("data.table")

  # Person 1 has two employers: A (two periods) and B (two periods)
  # Each employer's periods are within gap threshold
  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-04-01", "2023-05-05")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30", "2023-06-30")),
    durata = c(31, 55, 30, 57),
    arco = c(1, 1, 1, 1),
    datore = c("A", "A", "B", "B")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 2)
  # Each employer consolidated into one row
  result_A <- result[result$datore == "A"]
  result_B <- result[result$datore == "B"]
  expect_equal(result_A$n_periods_consolidated, 2L)
  expect_equal(result_B$n_periods_consolidated, 2L)
})

test_that("consolidate_by_employer with max_gap_days = 0 only merges truly adjacent periods", {
  skip_if_not_installed("data.table")

  # Period 1 ends 2023-01-31, period 2 starts 2023-02-01 (adjacent, gap = 0)
  # Period 3 starts 2023-02-03 (gap = 1, not adjacent with max_gap_days=0)
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-03")),
    fine = as.Date(c("2023-01-31", "2023-02-02", "2023-03-31")),
    durata = c(31, 2, 57),
    arco = c(1, 1, 1),
    datore = c("A", "A", "A")
  )

  result <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    max_gap_days = 0
  )

  # First two should merge (gap=0), third stays separate (gap=0 between 02-02 and 02-03 is 0 days)
  # gap = 2023-02-01 - 2023-01-31 - 1 = 0 days (merged)
  # gap = 2023-02-03 - 2023-02-02 - 1 = 0 days (merged)
  # All three merge since all gaps are 0
  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 3L)
})

test_that("consolidate_by_employer works with default max_gap_days = 8", {
  skip_if_not_installed("data.table")

  # Verify the default parameter value works correctly
  # Gap of 5 days (within default 8): should merge
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-06")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 23),
    arco = c(1, 1),
    datore = c("A", "A")
  )

  # Call without explicit max_gap_days (use default)
  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 2L)
})

# 3. Edge cases -----

test_that("consolidate_by_employer handles empty data.table", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = integer(),
    inizio = as.Date(character()),
    fine = as.Date(character()),
    durata = numeric(),
    datore = character()
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_true("n_periods_consolidated" %in% names(result))
})

test_that("consolidate_by_employer handles single record", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    arco = 1,
    datore = "A"
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 1L)
  expect_equal(result$datore, "A")
})

test_that("consolidate_by_employer consolidates all periods from single employer", {
  skip_if_not_installed("data.table")

  # All periods from the same employer within gap threshold
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-03-12")),
    fine = as.Date(c("2023-01-31", "2023-03-07", "2023-04-30")),
    durata = c(31, 31, 50),
    arco = c(1, 1, 1),
    datore = c("A", "A", "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 3L)
  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-04-30"))
})

test_that("consolidate_by_employer keeps all records when all employers differ", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    arco = c(1, 1, 1),
    datore = c("A", "B", "C")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 3)
  expect_true(all(result$n_periods_consolidated == 1L))
})

test_that("consolidate_by_employer treats NA employer as always different", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-04-30")),
    durata = c(31, 28, 31, 30),
    arco = c(1, 1, 1, 1),
    datore = c("A", NA, NA, "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  # NA employers should NOT be consolidated with each other or with "A"
  # The two "A" records are separated by NA employer records, so they are not
  # consecutive from the same employer (sorted by cf + employer + inizio,
  # NA values go to separate groups). Each NA starts a new group.
  na_results <- result[is.na(datore)]
  expect_true(all(na_results$n_periods_consolidated == 1L))
})

# 4. Unemployment barrier -----

test_that("consolidate_by_employer prevents merging when record has arco == 0", {
  skip_if_not_installed("data.table")

  # Same employer "A" but middle record is unemployment (arco == 0)
  # After sorting by cf + employer + inizio, all three are grouped together
  # but arco == 0 on the second record triggers a barrier
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-03-05")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 24, 27),
    arco = c(1, 0, 1),
    datore = c("A", "A", "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  # The arco == 0 record creates a barrier:
  # Record 1 (arco=1) is separate (next record is unemployment)
  # Record 2 (arco=0) is separate (it is unemployment itself)
  # Record 3 (arco=1) is separate (previous record was unemployment)
  expect_equal(nrow(result), 3)
  expect_true(all(result$n_periods_consolidated == 1L))
})

test_that("consolidate_by_employer unemployment records not part of consolidation group", {
  skip_if_not_installed("data.table")

  # Mix of employment and unemployment records from different employers
  # The UNEMP record (arco == 0) should remain isolated
  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-15")),
    fine = as.Date(c("2023-01-31", "2023-02-14", "2023-03-31")),
    durata = c(31, 14, 45),
    arco = c(1, 0, 1),
    datore = c("A", "UNEMP", "B")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  # All three records have different employers, so no consolidation
  expect_equal(nrow(result), 3)

  # Unemployment record stands on its own
  unemp_row <- result[arco == 0]
  expect_equal(nrow(unemp_row), 1)
  expect_equal(unemp_row$n_periods_consolidated, 1L)
})

test_that("consolidate_by_employer works without arco column", {
  skip_if_not_installed("data.table")

  # No arco column means all records treated as employment
  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 24),
    datore = c("A", "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 1)
  expect_equal(result$n_periods_consolidated, 2L)
  expect_true("arco" %in% names(result))
})

# 5. Variable handling modes -----

test_that("consolidate_by_employer with variable_handling = 'weight' uses weighted aggregation", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 24),
    arco = c(1, 1),
    datore = c("A", "A"),
    salary = c(1000, 2000)
  )

  result <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "weight"
  )

  # Weighted mean: (1000*31 + 2000*24) / (31+24) = (31000 + 48000) / 55
  expected_salary <- (1000 * 31 + 2000 * 24) / (31 + 24)
  expect_equal(result$salary, expected_salary, tolerance = 0.01)
})

test_that("consolidate_by_employer with variable_handling = 'first' takes first values", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 24),
    arco = c(1, 1),
    datore = c("A", "A"),
    salary = c(1000, 2000),
    contract_type = c("permanent", "temporary")
  )

  result <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "first"
  )

  expect_equal(result$salary, 1000)
  expect_equal(result$contract_type, "permanent")
})

test_that("consolidate_by_employer produces correct n_periods_consolidated in both modes", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-03-10")),
    fine = as.Date(c("2023-01-31", "2023-03-07", "2023-04-30")),
    durata = c(31, 31, 52),
    arco = c(1, 1, 1),
    datore = c("A", "A", "A")
  )

  result_weight <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "weight"
  )
  result_first <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "first"
  )

  expect_equal(result_weight$n_periods_consolidated, 3L)
  expect_equal(result_first$n_periods_consolidated, 3L)
})

# 6. Pipeline composability -----

test_that("consolidate_by_employer works after consolidate_overlapping", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-03-31")),
    durata = c(31, 32, 55),
    over_id = c(1, 1, 0),
    arco = c(1, 1, 1),
    datore = c("A", "A", "A")
  )

  result <- dt |>
    consolidate_overlapping() |>
    consolidate_by_employer("datore")

  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("consolidate_by_employer works before consolidate_adjacent", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30")),
    durata = c(31, 55, 30),
    arco = c(1, 1, 1),
    datore = c("A", "A", "B")
  )

  result <- dt |>
    consolidate_by_employer("datore") |>
    consolidate_adjacent()

  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("full 4-function consolidation pipeline runs without error", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1, 1, 1),
    inizio = as.Date(c(
      "2023-01-01",
      "2023-01-15",
      "2023-02-20",
      "2023-04-01",
      "2023-06-01"
    )),
    fine = as.Date(c(
      "2023-01-31",
      "2023-02-15",
      "2023-03-31",
      "2023-04-30",
      "2023-06-30"
    )),
    durata = c(31, 32, 40, 30, 30),
    over_id = c(1, 1, 0, 0, 0),
    arco = c(1, 1, 1, 1, 1),
    datore = c("A", "A", "A", "B", "B")
  )

  expect_no_error({
    result <- dt |>
      consolidate_overlapping() |>
      consolidate_by_employer("datore") |>
      consolidate_adjacent() |>
      consolidate_short_gaps(30)
  })

  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("n_periods_consolidated column preserved through pipeline", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-04-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30")),
    durata = c(31, 55, 30),
    over_id = c(0, 0, 0),
    arco = c(1, 1, 1),
    datore = c("A", "A", "B")
  )

  result <- dt |>
    consolidate_by_employer("datore") |>
    consolidate_adjacent()

  expect_true("n_periods_consolidated" %in% names(result))
  # n_periods_consolidated should be a positive integer in every row
  expect_true(all(result$n_periods_consolidated >= 1L))
})

# 7. Multi-person independence -----

test_that("consolidate_by_employer processes different persons independently", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-01-01", "2023-02-05")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-01-31", "2023-02-28")),
    durata = c(31, 24, 31, 24),
    arco = c(1, 1, 1, 1),
    datore = c("A", "A", "B", "B")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 2)
  # Each person consolidated independently
  person1 <- result[cf == 1]
  person2 <- result[cf == 2]
  expect_equal(nrow(person1), 1)
  expect_equal(nrow(person2), 1)
  expect_equal(person1$n_periods_consolidated, 2L)
  expect_equal(person2$n_periods_consolidated, 2L)
})

test_that("consolidate_by_employer does NOT merge same employer across different persons", {
  skip_if_not_installed("data.table")

  # Two persons both working for employer A - should remain separate
  dt <- data.table::data.table(
    cf = c(1, 2),
    inizio = as.Date(c("2023-01-01", "2023-01-01")),
    fine = as.Date(c("2023-01-31", "2023-01-31")),
    durata = c(31, 31),
    arco = c(1, 1),
    datore = c("A", "A")
  )

  result <- consolidate_by_employer(dt, employer_var = "datore")

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$cf), c(1, 2))
  # Both are single-period workers, so each gets n_periods_consolidated = 1
  expect_true(all(result$n_periods_consolidated == 1L))
})
