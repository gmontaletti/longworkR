# Regression Tests: v2 (collapse-native) vs v1 (data.table J-expression) Engine
# Ensures that the v2 consolidation engine produces identical results to v1
# on both real sample data and synthetic edge cases.

# 1. Helper: compare v1 vs v2 output -----

compare_engines <- function(v1, v2, label, chained = FALSE) {
  # Same number of rows
  expect_equal(nrow(v1), nrow(v2), info = paste(label, "- row count mismatch"))

  # Same columns (order-independent)
  expect_equal(
    sort(names(v1)),
    sort(names(v2)),
    info = paste(label, "- column names differ")
  )

  # Date columns exact match
  expect_equal(v1$inizio, v2$inizio, info = paste(label, "- inizio mismatch"))
  expect_equal(v1$fine, v2$fine, info = paste(label, "- fine mismatch"))

  # n_periods_consolidated exact match
  expect_equal(
    v1$n_periods_consolidated,
    v2$n_periods_consolidated,
    info = paste(label, "- n_periods_consolidated mismatch")
  )

  # Numeric columns
  num_cols <- names(v1)[vapply(v1, is.numeric, logical(1L))]
  num_cols <- setdiff(num_cols, "n_periods_consolidated")

  if (chained) {
    # Chained comparisons: character column tie-breaking differences at step N
    # cascade into numeric differences at step N+1 (different weighted means
    # when groupings include different character values). Use match rate.
    for (col in num_cols) {
      both_valid <- !is.na(v1[[col]]) & !is.na(v2[[col]])
      if (sum(both_valid) == 0) {
        next
      }
      match_rate <- mean(
        abs(v1[[col]][both_valid] - v2[[col]][both_valid]) < 1e-6,
        na.rm = TRUE
      )
      expect_gt(
        match_rate,
        0.999,
        label = paste(
          label,
          "- numeric column",
          col,
          "match rate:",
          round(match_rate, 5)
        )
      )
    }
  } else {
    for (col in num_cols) {
      expect_equal(
        v1[[col]],
        v2[[col]],
        tolerance = 1e-10,
        info = paste(label, "- numeric column:", col)
      )
    }
  }

  # Character columns: high match rate (>= 99.9%)
  # Weighted mode tie-breaking differs between engines:
  # v1 (tapply/which.max) resolves ties alphabetically,
  # v2 (collapse::fmode ties="first") resolves by data order.
  # This causes a small number of legitimately different values.
  char_cols <- names(v1)[vapply(v1, is.character, logical(1L))]
  for (col in char_cols) {
    match_rate <- mean(v1[[col]] == v2[[col]], na.rm = TRUE)
    expect_gt(
      match_rate,
      0.999,
      label = paste(
        label,
        "- char column",
        col,
        "match rate:",
        round(match_rate, 5)
      )
    )
  }
}

# 2. Load real sample data -----

sample_data <- tryCatch(
  {
    e <- new.env()
    utils::data("sample", package = "longworkR", envir = e)
    get("sample", envir = e)
  },
  error = function(e) NULL
)
has_sample <- data.table::is.data.table(sample_data) && nrow(sample_data) > 0L

# 3. consolidate_adjacent: v2 vs v1 on real data -----

test_that("consolidate_adjacent v2 matches v1 with variable_handling = 'weight'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_adjacent(
    sample_data,
    variable_handling = "weight",
    engine = "v1"
  )
  v2 <- consolidate_adjacent(
    sample_data,
    variable_handling = "weight",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_adjacent(weight)")
})

test_that("consolidate_adjacent v2 matches v1 with variable_handling = 'first'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_adjacent(
    sample_data,
    variable_handling = "first",
    engine = "v1"
  )
  v2 <- consolidate_adjacent(
    sample_data,
    variable_handling = "first",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_adjacent(first)")
})

# 4. consolidate_overlapping: v2 vs v1 on real data -----

test_that("consolidate_overlapping v2 matches v1 with variable_handling = 'weight'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_overlapping(
    sample_data,
    variable_handling = "weight",
    engine = "v1"
  )
  v2 <- consolidate_overlapping(
    sample_data,
    variable_handling = "weight",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_overlapping(weight)")
})

test_that("consolidate_overlapping v2 matches v1 with variable_handling = 'first'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_overlapping(
    sample_data,
    variable_handling = "first",
    engine = "v1"
  )
  v2 <- consolidate_overlapping(
    sample_data,
    variable_handling = "first",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_overlapping(first)")
})

# 5. consolidate_by_employer: v2 vs v1 on real data -----

test_that("consolidate_by_employer v2 matches v1 with variable_handling = 'weight'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")
  skip_if(
    !("datore" %in% names(sample_data)),
    "datore column not in sample data"
  )

  v1 <- consolidate_by_employer(
    sample_data,
    employer_var = "datore",
    variable_handling = "weight",
    engine = "v1"
  )
  v2 <- consolidate_by_employer(
    sample_data,
    employer_var = "datore",
    variable_handling = "weight",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_by_employer(weight)")
})

test_that("consolidate_by_employer v2 matches v1 with variable_handling = 'first'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")
  skip_if(
    !("datore" %in% names(sample_data)),
    "datore column not in sample data"
  )

  v1 <- consolidate_by_employer(
    sample_data,
    employer_var = "datore",
    variable_handling = "first",
    engine = "v1"
  )
  v2 <- consolidate_by_employer(
    sample_data,
    employer_var = "datore",
    variable_handling = "first",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_by_employer(first)")
})

# 6. consolidate_short_gaps: v2 vs v1 on real data -----

test_that("consolidate_short_gaps v2 matches v1 with variable_handling = 'first'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_short_gaps(
    sample_data,
    max_gap_days = 30,
    variable_handling = "first",
    engine = "v1"
  )
  v2 <- consolidate_short_gaps(
    sample_data,
    max_gap_days = 30,
    variable_handling = "first",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_short_gaps(first)")

  # non_working_days exact match
  expect_equal(
    v1$non_working_days,
    v2$non_working_days,
    info = "consolidate_short_gaps(first) - non_working_days mismatch"
  )
})

test_that("consolidate_short_gaps v2 matches v1 with variable_handling = 'weight'", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- consolidate_short_gaps(
    sample_data,
    max_gap_days = 30,
    variable_handling = "weight",
    engine = "v1"
  )
  v2 <- consolidate_short_gaps(
    sample_data,
    max_gap_days = 30,
    variable_handling = "weight",
    engine = "v2"
  )

  compare_engines(v1, v2, "consolidate_short_gaps(weight)")

  # non_working_days exact match
  expect_equal(
    v1$non_working_days,
    v2$non_working_days,
    info = "consolidate_short_gaps(weight) - non_working_days mismatch"
  )
})

test_that("consolidate_short_gaps v2 matches v1 with different max_gap_days", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  for (gap in c(7, 14, 90)) {
    v1 <- consolidate_short_gaps(sample_data, max_gap_days = gap, engine = "v1")
    v2 <- consolidate_short_gaps(sample_data, max_gap_days = gap, engine = "v2")

    compare_engines(v1, v2, paste0("consolidate_short_gaps(gap=", gap, ")"))

    expect_equal(
      v1$non_working_days,
      v2$non_working_days,
      info = paste0("non_working_days mismatch at gap=", gap)
    )
  }
})

# 7. Full chain: v2 vs v1 on real data -----

test_that("full consolidation chain v2 matches v1 (overlapping -> adjacent -> short_gaps)", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- sample_data |>
    consolidate_overlapping(variable_handling = "weight", engine = "v1") |>
    consolidate_adjacent(variable_handling = "weight", engine = "v1") |>
    consolidate_short_gaps(max_gap_days = 30, engine = "v1")

  v2 <- sample_data |>
    consolidate_overlapping(variable_handling = "weight", engine = "v2") |>
    consolidate_adjacent(variable_handling = "weight", engine = "v2") |>
    consolidate_short_gaps(max_gap_days = 30, engine = "v2")

  compare_engines(v1, v2, "full chain (weight)", chained = TRUE)

  expect_equal(
    v1$non_working_days,
    v2$non_working_days,
    info = "full chain - non_working_days mismatch"
  )
})

test_that("full consolidation chain v2 matches v1 with 'first' mode", {
  skip_if_not_installed("data.table")
  skip_if(!has_sample, "Sample data not available")

  v1 <- sample_data |>
    consolidate_overlapping(variable_handling = "first", engine = "v1") |>
    consolidate_adjacent(variable_handling = "first", engine = "v1") |>
    consolidate_short_gaps(
      max_gap_days = 30,
      variable_handling = "first",
      engine = "v1"
    )

  v2 <- sample_data |>
    consolidate_overlapping(variable_handling = "first", engine = "v2") |>
    consolidate_adjacent(variable_handling = "first", engine = "v2") |>
    consolidate_short_gaps(
      max_gap_days = 30,
      variable_handling = "first",
      engine = "v2"
    )

  compare_engines(v1, v2, "full chain (first)", chained = TRUE)

  expect_equal(
    v1$non_working_days,
    v2$non_working_days,
    info = "full chain (first) - non_working_days mismatch"
  )
})

# 8. Edge cases: empty data -----

test_that("v2 engine handles empty data for consolidate_adjacent", {
  skip_if_not_installed("data.table")

  empty_dt <- data.table::data.table(
    cf = integer(),
    inizio = as.IDate(character()),
    fine = as.IDate(character()),
    durata = numeric(),
    arco = numeric()
  )

  result <- consolidate_adjacent(empty_dt, engine = "v2")
  expect_equal(nrow(result), 0L)
  expect_true("n_periods_consolidated" %in% names(result))
})

test_that("v2 engine handles empty data for consolidate_overlapping", {
  skip_if_not_installed("data.table")

  empty_dt <- data.table::data.table(
    cf = integer(),
    inizio = as.IDate(character()),
    fine = as.IDate(character()),
    durata = numeric(),
    over_id = integer()
  )

  result <- consolidate_overlapping(empty_dt, engine = "v2")
  expect_equal(nrow(result), 0L)
  expect_true("n_periods_consolidated" %in% names(result))
})

test_that("v2 engine handles empty data for consolidate_short_gaps", {
  skip_if_not_installed("data.table")

  empty_dt <- data.table::data.table(
    cf = integer(),
    inizio = as.IDate(character()),
    fine = as.IDate(character()),
    durata = numeric(),
    arco = numeric()
  )

  result <- consolidate_short_gaps(empty_dt, engine = "v2")
  expect_equal(nrow(result), 0L)
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true("non_working_days" %in% names(result))
})

# 9. Edge cases: single record -----

test_that("v2 engine handles single record for consolidate_adjacent", {
  skip_if_not_installed("data.table")

  single <- data.table::data.table(
    cf = 1L,
    inizio = as.IDate("2023-01-01"),
    fine = as.IDate("2023-01-31"),
    durata = 31,
    arco = 1
  )

  result <- consolidate_adjacent(single, engine = "v2")
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_periods_consolidated, 1L)
})

test_that("v2 engine handles single record for consolidate_overlapping", {
  skip_if_not_installed("data.table")

  single <- data.table::data.table(
    cf = 1L,
    inizio = as.IDate("2023-01-01"),
    fine = as.IDate("2023-01-31"),
    durata = 31,
    over_id = 1L
  )

  result <- consolidate_overlapping(single, engine = "v2")
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_periods_consolidated, 1L)
})

test_that("v2 engine handles single record for consolidate_short_gaps", {
  skip_if_not_installed("data.table")

  single <- data.table::data.table(
    cf = 1L,
    inizio = as.IDate("2023-01-01"),
    fine = as.IDate("2023-01-31"),
    durata = 31,
    arco = 1
  )

  result <- consolidate_short_gaps(single, engine = "v2")
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_periods_consolidated, 1L)
  expect_equal(result$non_working_days, 0)
})

# 10. Fast path: no consolidation needed -----

test_that("v2 fast path works when no adjacent consolidation is needed", {
  skip_if_not_installed("data.table")

  # Records with large gaps -- no adjacency possible
  dt <- data.table::data.table(
    cf = 1:5,
    inizio = as.IDate(c(
      "2023-01-01",
      "2023-03-01",
      "2023-05-01",
      "2023-07-01",
      "2023-09-01"
    )),
    fine = as.IDate(c(
      "2023-01-15",
      "2023-03-15",
      "2023-05-15",
      "2023-07-15",
      "2023-09-15"
    )),
    durata = rep(15, 5),
    arco = rep(1, 5)
  )

  result <- consolidate_adjacent(dt, engine = "v2")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$n_periods_consolidated == 1L))
})

test_that("v2 fast path works when no overlapping consolidation is needed", {
  skip_if_not_installed("data.table")

  # All over_id == 0 means no overlapping groups
  dt <- data.table::data.table(
    cf = 1:5,
    inizio = as.IDate(c(
      "2023-01-01",
      "2023-03-01",
      "2023-05-01",
      "2023-07-01",
      "2023-09-01"
    )),
    fine = as.IDate(c(
      "2023-01-15",
      "2023-03-15",
      "2023-05-15",
      "2023-07-15",
      "2023-09-15"
    )),
    durata = rep(15, 5),
    arco = rep(1, 5),
    over_id = rep(0L, 5)
  )

  result <- consolidate_overlapping(dt, engine = "v2")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$n_periods_consolidated == 1L))
})

test_that("v2 fast path works when no short gap consolidation is needed", {
  skip_if_not_installed("data.table")

  # Each person has one record, so no gaps to bridge
  dt <- data.table::data.table(
    cf = 1:5,
    inizio = as.IDate(c(
      "2023-01-01",
      "2023-03-01",
      "2023-05-01",
      "2023-07-01",
      "2023-09-01"
    )),
    fine = as.IDate(c(
      "2023-01-15",
      "2023-03-15",
      "2023-05-15",
      "2023-07-15",
      "2023-09-15"
    )),
    durata = rep(15, 5),
    arco = rep(1, 5)
  )

  result <- consolidate_short_gaps(dt, engine = "v2")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$n_periods_consolidated == 1L))
  expect_true(all(result$non_working_days == 0))
})

# 11. Engine parameter validation -----

test_that("invalid engine parameter raises an error", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.IDate("2023-01-01"),
    fine = as.IDate("2023-01-31"),
    durata = 31,
    arco = 1
  )

  expect_error(consolidate_adjacent(dt, engine = "v3"))
  expect_error(consolidate_overlapping(
    data.table::copy(dt)[, over_id := 1L],
    engine = "invalid"
  ))
})

# 12. Synthetic multi-person regression -----

test_that("v2 matches v1 on synthetic multi-person data with mixed scenarios", {
  skip_if_not_installed("data.table")

  set.seed(42)
  n <- 200L
  dt <- data.table::data.table(
    cf = rep(seq_len(20L), each = 10L),
    inizio = as.IDate("2020-01-01") + sample(0:500, n, replace = TRUE),
    durata = sample(15:180, n, replace = TRUE),
    arco = sample(c(0L, 1L, 1L, 1L), n, replace = TRUE),
    over_id = sample(c(0L, 0L, 0L, 1L, 1L, 2L), n, replace = TRUE),
    stato = sample(c("fulltime", "parttime", "inoccupato"), n, replace = TRUE),
    retribuzione = round(runif(n, 800, 3000), 2),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      c("A.01.00", "A.02.00", "A.03.00", "B.01.00"),
      n,
      replace = TRUE
    ),
    datore = sample(1:10, n, replace = TRUE)
  )
  dt[, fine := inizio + durata - 1L]
  data.table::setkey(dt, cf, inizio)

  # consolidate_adjacent
  v1_adj <- consolidate_adjacent(
    dt,
    variable_handling = "weight",
    engine = "v1"
  )
  v2_adj <- consolidate_adjacent(
    dt,
    variable_handling = "weight",
    engine = "v2"
  )
  compare_engines(v1_adj, v2_adj, "synthetic - consolidate_adjacent(weight)")

  # consolidate_overlapping
  v1_ovl <- consolidate_overlapping(
    dt,
    variable_handling = "weight",
    engine = "v1"
  )
  v2_ovl <- consolidate_overlapping(
    dt,
    variable_handling = "weight",
    engine = "v2"
  )
  compare_engines(v1_ovl, v2_ovl, "synthetic - consolidate_overlapping(weight)")

  # consolidate_by_employer
  v1_emp <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "weight",
    engine = "v1"
  )
  v2_emp <- consolidate_by_employer(
    dt,
    employer_var = "datore",
    variable_handling = "weight",
    engine = "v2"
  )
  compare_engines(v1_emp, v2_emp, "synthetic - consolidate_by_employer(weight)")

  # consolidate_short_gaps
  v1_gap <- consolidate_short_gaps(
    dt,
    max_gap_days = 30,
    variable_handling = "first",
    engine = "v1"
  )
  v2_gap <- consolidate_short_gaps(
    dt,
    max_gap_days = 30,
    variable_handling = "first",
    engine = "v2"
  )
  compare_engines(v1_gap, v2_gap, "synthetic - consolidate_short_gaps(first)")
  expect_equal(
    v1_gap$non_working_days,
    v2_gap$non_working_days,
    info = "synthetic - non_working_days mismatch"
  )
})
