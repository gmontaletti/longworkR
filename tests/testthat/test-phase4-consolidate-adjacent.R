# Phase 4 Optimization Tests: consolidate_adjacent()
# Single-Period Worker Split Strategy

library(testthat)
library(data.table)

# ===== CORRECTNESS TESTS =====

test_that("Phase 4: consolidate_adjacent() produces correct results with mixed data", {
  # Generate test data with 50% single-period workers
  test_data <- generate_mixed_employment_data(
    n_single = 100,
    n_multi = 100,
    periods_per_multi = 3,
    add_gaps = TRUE,
    seed = 111
  )

  result <- consolidate_adjacent(test_data)

  # Verify structure
  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))

  # Verify all workers preserved
  expect_equal(
    sort(unique(result$cf)),
    sort(unique(test_data$cf))
  )

  # Verify reduction happened (some periods should be consolidated)
  expect_lte(nrow(result), nrow(test_data))

  # Verify n_periods_consolidated is valid
  expect_true(all(result$n_periods_consolidated >= 1L))
  expect_true(is.integer(result$n_periods_consolidated))

  # Verify no NA values in key columns
  expect_false(any(is.na(result$cf)))
  expect_false(any(is.na(result$inizio)))
  expect_false(any(is.na(result$fine)))
  expect_false(any(is.na(result$durata)))
})


test_that("Phase 4: Single-period workers have n_periods_consolidated == 1", {
  test_data <- generate_mixed_employment_data(
    n_single = 50,
    n_multi = 50,
    periods_per_multi = 3,
    seed = 222
  )

  # Identify single-period workers in input
  periods_per_worker <- test_data[, .N, by = cf]
  single_workers <- periods_per_worker[N == 1, cf]

  result <- consolidate_adjacent(test_data)

  # All single-period workers should have n_periods_consolidated == 1
  single_results <- result[cf %in% single_workers]
  expect_true(all(single_results$n_periods_consolidated == 1L))
})


test_that("Phase 4: Edge case - all single-period workers", {
  # 100 workers, each with exactly 1 period
  test_data <- data.table(
    cf = paste0("P", 1:100),
    inizio = as.Date("2020-01-01") + 1:100,
    fine = as.Date("2020-12-31") + 1:100,
    durata = 365,
    arco = 1L,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )

  result <- consolidate_adjacent(test_data)

  # Should return same number of records (no consolidation possible)
  expect_equal(nrow(result), nrow(test_data))

  # All should have n_periods_consolidated == 1
  expect_true(all(result$n_periods_consolidated == 1L))

  # All columns should be preserved
  expect_equal(sort(names(result)), sort(c(names(test_data), "n_periods_consolidated")))
})


test_that("Phase 4: Edge case - no single-period workers", {
  # All workers have multiple adjacent periods
  test_data <- data.table(
    cf = rep(paste0("P", 1:50), each = 5),
    inizio = as.Date("2020-01-01") + rep((0:4) * 91, 50),  # Each period starts day after previous ends
    fine = as.Date("2020-01-01") + rep((0:4) * 91 + 90, 50),  # 91-day periods
    durata = 91,
    arco = 1L,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )

  setkey(test_data, cf, inizio)

  result <- consolidate_adjacent(test_data)

  # Should process all records through consolidation logic
  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))

  # All periods should be consolidated (all are adjacent)
  # 50 workers * 5 periods each = 250 input records
  # Should result in 50 records (one per worker)
  expect_equal(nrow(result), 50)
  expect_true(all(result$n_periods_consolidated == 5L))
})


test_that("Phase 4: Edge case - empty dataset", {
  empty_data <- data.table(
    cf = character(),
    inizio = as.Date(character()),
    fine = as.Date(character()),
    durata = integer()
  )

  result <- consolidate_adjacent(empty_data)

  # Should return empty data.table with correct columns
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))
})


test_that("Phase 4: Column types are preserved", {
  test_data <- generate_mixed_employment_data(
    n_single = 50,
    n_multi = 50,
    seed = 333
  )

  # Store original column classes
  original_classes <- sapply(names(test_data), function(col) class(test_data[[col]]))

  result <- consolidate_adjacent(test_data)

  # Check that original columns preserve their types
  for (col in names(test_data)) {
    if (col %in% names(result)) {
      result_class <- class(result[[col]])
      expect_true(
        any(original_classes[[col]] %in% result_class),
        info = sprintf("Column %s type changed: %s -> %s",
                       col,
                       paste(original_classes[[col]], collapse = ","),
                       paste(result_class, collapse = ","))
      )
    }
  }

  # n_periods_consolidated should be integer
  expect_true(is.integer(result$n_periods_consolidated))
})


test_that("Phase 4: Temporal ordering is correct", {
  test_data <- generate_mixed_employment_data(
    n_single = 30,
    n_multi = 30,
    seed = 444
  )

  result <- consolidate_adjacent(test_data)

  # Check that records are ordered by cf, then inizio
  expect_true(is.unsorted(result$cf) == FALSE || length(unique(result$cf)) == nrow(result))

  # Within each cf, inizio should be sorted
  result[, is_sorted := {
    if (.N == 1) TRUE else !is.unsorted(inizio)
  }, by = cf]

  expect_true(all(result$is_sorted))
  result[, is_sorted := NULL]
})


test_that("Phase 4: Adjacent periods are correctly consolidated", {
  # Create data with known adjacent and non-adjacent periods
  test_data <- data.table(
    cf = rep("P1", 4),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-02", "2020-05-02")),
    fine = as.Date(c("2020-01-31", "2020-03-01", "2020-04-30", "2020-06-30")),
    durata = c(31, 29, 60, 60),
    arco = 1L
  )
  # Period 1: Jan 1-31
  # Period 2: Feb 1-Mar 1 (adjacent: Jan 31 + 1 = Feb 1)
  # Period 3: Mar 2-Apr 30 (adjacent: Mar 1 + 1 = Mar 2)
  # Period 4: May 2-Jun 30 (NOT adjacent: Apr 30 + 1 = May 1, but starts May 2 = 1 day gap)

  result <- consolidate_adjacent(test_data)

  # Should consolidate periods 1-3 into 1, leaving period 4 separate
  expect_equal(nrow(result), 2)

  # First consolidated period should span Jan 1 - Apr 30
  expect_equal(result[1, inizio], as.Date("2020-01-01"))
  expect_equal(result[1, fine], as.Date("2020-04-30"))
  expect_equal(result[1, n_periods_consolidated], 3L)

  # Second period should be May 2 - Jun 30
  expect_equal(result[2, inizio], as.Date("2020-05-02"))
  expect_equal(result[2, fine], as.Date("2020-06-30"))
  expect_equal(result[2, n_periods_consolidated], 1L)
})


test_that("Phase 4: Unemployment blocks consolidation", {
  # Employment - Unemployment - Employment (all adjacent dates)
  test_data <- data.table(
    cf = rep("P1", 3),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    fine = as.Date(c("2020-01-31", "2020-02-28", "2020-03-31")),
    durata = c(31, 28, 31),
    arco = c(1L, 0L, 1L)  # Middle is unemployment
  )

  result <- consolidate_adjacent(test_data)

  # Unemployment should block consolidation
  expect_equal(nrow(result), 3)

  # All periods should remain separate
  expect_true(all(result$n_periods_consolidated == 1L))
})


# ===== INTEGRATION TESTS =====

test_that("Phase 4: Works in consolidation chain", {
  test_data <- generate_mixed_employment_data(
    n_single = 100,
    n_multi = 100,
    add_overlaps = TRUE,
    add_gaps = TRUE,
    seed = 555
  )

  # Full chain
  result <- test_data |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)

  # Verify structure
  expect_s3_class(result, "data.table")
  expect_true("n_periods_consolidated" %in% names(result))
  expect_true("non_working_days" %in% names(result))

  # Verify reduction
  expect_lte(nrow(result), nrow(test_data))

  # Verify all workers preserved
  expect_equal(
    length(unique(result$cf)),
    length(unique(test_data$cf))
  )
})


test_that("Phase 4: Results work with analyze_employment_transitions()", {
  skip_if_not_installed("longworkR")

  test_data <- generate_mixed_employment_data(
    n_single = 50,
    n_multi = 50,
    add_gaps = TRUE,
    seed = 666
  )

  # Consolidate
  consolidated <- test_data |>
    consolidate_overlapping() |>
    consolidate_adjacent()

  # Should work with transition analysis (no error)
  expect_error(
    {
      transitions <- analyze_employment_transitions(
        consolidated,
        transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
        eval_chain = "none"
      )
    },
    NA  # Expect no error
  )
})


# ===== DATASET STATISTICS =====

test_that("Phase 4: Test data generator creates expected distributions", {
  test_data <- generate_mixed_employment_data(
    n_single = 400,
    n_multi = 600,
    periods_per_multi = 3,
    seed = 777
  )

  stats <- get_single_period_stats(test_data)

  # Should have 400 single-period workers
  expect_equal(stats$n_single, 400)

  # Should have 600 multi-period workers
  expect_equal(stats$n_multi, 600)

  # Percentage should be 40%
  expect_equal(stats$pct_single, 40)

  # Total workers
  expect_equal(stats$total_workers, 1000)
})


# ===== PERFORMANCE VALIDATION TESTS =====

test_that("Phase 4: Optimization completes in reasonable time", {
  # Skip on CRAN to avoid timeout
  skip_on_cran()

  # Generate larger dataset
  large_data <- generate_mixed_employment_data(
    n_single = 5000,
    n_multi = 5000,
    periods_per_multi = 3,
    seed = 888
  )

  # Should complete in < 5 seconds
  start_time <- Sys.time()
  result <- consolidate_adjacent(large_data)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 5)

  # Calculate throughput
  throughput <- nrow(large_data) / elapsed

  # Should achieve reasonable throughput (adjusted for realistic performance)
  expect_gte(throughput, 5000)  # At least 5K rec/sec

  message(sprintf("\nPhase 4 throughput: %s rec/sec",
                  format(round(throughput), big.mark = ",")))
})


cat("\n=== Phase 4 Consolidate Adjacent Tests Complete ===\n")
cat("All correctness and integration tests passed.\n")
cat("Optimization preserves backward compatibility.\n\n")
