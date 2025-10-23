# Test Suite for New Consolidation Helper Functions
# Tests internal helper functions: .weighted_mode, .weighted_mean, .consolidate_groups

test_that(".weighted_mode calculates correct mode with equal weights", {
  skip_if_not_installed("data.table")

  values <- c("A", "A", "A", "B", "B")
  weights <- c(1, 1, 1, 1, 1)

  result <- longworkR:::.weighted_mode(values, weights)

  expect_type(result, "character")
  expect_equal(result, "A")  # A has weight 3, B has weight 2
})

test_that(".weighted_mode calculates correct mode with unequal weights", {
  skip_if_not_installed("data.table")

  values <- c("A", "A", "B", "B", "B")
  weights <- c(10, 10, 5, 5, 5)  # A: 20, B: 15

  result <- longworkR:::.weighted_mode(values, weights)

  expect_equal(result, "A")
})

test_that(".weighted_mode handles reverse weighted preference", {
  skip_if_not_installed("data.table")

  values <- c("A", "A", "B", "B", "B")
  weights <- c(5, 5, 10, 10, 10)  # A: 10, B: 30

  result <- longworkR:::.weighted_mode(values, weights)

  expect_equal(result, "B")
})

test_that(".weighted_mode handles all NA values", {
  skip_if_not_installed("data.table")

  values <- c(NA_character_, NA_character_, NA_character_)
  weights <- c(10, 20, 30)

  result <- longworkR:::.weighted_mode(values, weights)

  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that(".weighted_mode handles single value", {
  skip_if_not_installed("data.table")

  values <- c("A")
  weights <- c(100)

  result <- longworkR:::.weighted_mode(values, weights)

  expect_equal(result, "A")
})

test_that(".weighted_mode handles all identical values", {
  skip_if_not_installed("data.table")

  values <- c("X", "X", "X", "X")
  weights <- c(1, 5, 10, 20)

  result <- longworkR:::.weighted_mode(values, weights)

  expect_equal(result, "X")
})

test_that(".weighted_mode handles mixed NA and valid values", {
  skip_if_not_installed("data.table")

  values <- c("A", NA, "B", "B", NA)
  weights <- c(10, 5, 20, 20, 5)  # A: 10, B: 40 (NAs ignored)

  result <- longworkR:::.weighted_mode(values, weights)

  expect_equal(result, "B")
})

test_that(".weighted_mode handles ties deterministically", {
  skip_if_not_installed("data.table")

  values <- c("A", "A", "B", "B")
  weights <- c(10, 10, 10, 10)  # Perfect tie

  result <- longworkR:::.weighted_mode(values, weights)

  expect_type(result, "character")
  expect_true(result %in% c("A", "B"))  # Either is valid for tie
})


# 2. Tests for .weighted_mean -----

test_that(".weighted_mean calculates correct mean for numeric", {
  skip_if_not_installed("data.table")

  values <- c(10, 20, 30)
  weights <- c(1, 1, 1)

  result <- longworkR:::.weighted_mean(values, weights)

  expect_type(result, "double")
  expect_equal(result, 20)
})

test_that(".weighted_mean handles unequal weights correctly", {
  skip_if_not_installed("data.table")

  values <- c(10, 20, 30)
  weights <- c(1, 2, 1)  # (10*1 + 20*2 + 30*1) / 4 = 80/4 = 20

  result <- longworkR:::.weighted_mean(values, weights)

  expect_equal(result, 20)
})

test_that(".weighted_mean preserves integer type", {
  skip_if_not_installed("data.table")

  values <- as.integer(c(10, 20, 30))
  weights <- c(1, 1, 1)

  result <- longworkR:::.weighted_mean(values, weights)

  expect_type(result, "integer")
  expect_equal(result, 20L)
})

test_that(".weighted_mean rounds integers correctly", {
  skip_if_not_installed("data.table")

  values <- as.integer(c(10, 20, 25))
  weights <- c(1, 1, 1)  # Mean = 18.33... should round to 18

  result <- longworkR:::.weighted_mean(values, weights)

  expect_type(result, "integer")
  expect_equal(result, 18L)
})

test_that(".weighted_mean handles all NA values (numeric)", {
  skip_if_not_installed("data.table")

  values <- c(NA_real_, NA_real_, NA_real_)
  weights <- c(10, 20, 30)

  result <- longworkR:::.weighted_mean(values, weights)

  expect_true(is.na(result))
  expect_type(result, "double")
})

test_that(".weighted_mean handles all NA values (integer)", {
  skip_if_not_installed("data.table")

  values <- c(NA_integer_, NA_integer_, NA_integer_)
  weights <- c(10, 20, 30)

  result <- longworkR:::.weighted_mean(values, weights)

  expect_true(is.na(result))
  expect_type(result, "integer")
})

test_that(".weighted_mean handles mixed NA and valid values", {
  skip_if_not_installed("data.table")

  values <- c(10, NA, 30, NA)
  weights <- c(1, 1, 1, 1)  # (10 + 30) / 2 = 20

  result <- longworkR:::.weighted_mean(values, weights)

  expect_equal(result, 20)
})

test_that(".weighted_mean handles single value", {
  skip_if_not_installed("data.table")

  values <- c(42.5)
  weights <- c(100)

  result <- longworkR:::.weighted_mean(values, weights)

  expect_equal(result, 42.5)
})

test_that(".weighted_mean handles zero weights gracefully", {
  skip_if_not_installed("data.table")

  values <- c(10, 20, 30)
  weights <- c(0, 0, 1)  # Only last value counts

  result <- longworkR:::.weighted_mean(values, weights)

  expect_equal(result, 30)
})


# 3. Tests for .consolidate_groups -----

test_that(".consolidate_groups requires data.table input", {
  skip_if_not_installed("data.table")

  df <- data.frame(cf = 1, inizio = Sys.Date(), fine = Sys.Date(),
                   durata = 1, consolidation_group = 1)

  expect_error(
    longworkR:::.consolidate_groups(df),
    "Input must be a data.table"
  )
})

test_that(".consolidate_groups requires all mandatory columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31")
    # Missing: durata, consolidation_group
  )

  expect_error(
    longworkR:::.consolidate_groups(dt),
    "Missing required columns"
  )
})

test_that(".consolidate_groups consolidates periods correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-28")),
    durata = c(31, 32, 28),
    consolidation_group = c(1, 1, 2)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Groups 1 and 2
  expect_equal(result$n_periods_consolidated, c(2, 1))
})

test_that(".consolidate_groups computes temporal fields correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-01-15")),
    fine = as.Date(c("2023-01-31", "2023-02-15")),
    durata = c(31, 32),
    consolidation_group = c(1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(result$inizio, as.Date("2023-01-01"))  # min
  expect_equal(result$fine, as.Date("2023-02-15"))    # max
  expect_equal(result$durata, 46)  # 2023-02-15 - 2023-01-01 + 1
})

test_that(".consolidate_groups preserves date types", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    consolidation_group = 1
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_s3_class(result$inizio, "Date")
  expect_s3_class(result$fine, "Date")
})

test_that(".consolidate_groups preserves integer types", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1L,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    age = 25L,
    consolidation_group = 1
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_type(result$age, "integer")
})

test_that(".consolidate_groups uses weighted mean for numeric columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    salary = c(1000, 2000),  # (1000*31 + 2000*28) / 59
    consolidation_group = c(1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expected_salary <- (1000*31 + 2000*28) / 59
  expect_equal(result$salary, expected_salary, tolerance = 0.01)
})

test_that(".consolidate_groups uses weighted mode for character columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    contract = c("A", "B", "A"),  # A: 62 days, B: 28 days
    consolidation_group = c(1, 1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(result$contract, "A")  # A has more total duration
})

test_that(".consolidate_groups handles arco column correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    arco = c(1, 2),
    consolidation_group = c(1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(result$arco, 2)  # Maximum
})

test_that(".consolidate_groups handles over_id column correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    over_id = c(0, 5, 0),  # Should take first non-zero (5)
    consolidation_group = c(1, 1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(result$over_id, 5)
})

test_that(".consolidate_groups handles all zero over_id", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    over_id = c(0, 0),  # All zero - should take first
    consolidation_group = c(1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(result$over_id, 0)
})

test_that(".consolidate_groups removes consolidation_group by default", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    consolidation_group = 1
  )

  result <- longworkR:::.consolidate_groups(dt, remove_group_col = TRUE)

  expect_false("consolidation_group" %in% names(result))
})

test_that(".consolidate_groups keeps consolidation_group when requested", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    consolidation_group = 1
  )

  result <- longworkR:::.consolidate_groups(dt, remove_group_col = FALSE)

  expect_true("consolidation_group" %in% names(result))
})

test_that(".consolidate_groups handles single record per group", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    consolidation_group = c(1, 2, 3)  # Each unique
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(nrow(result), 3)
  expect_equal(result$n_periods_consolidated, c(1, 1, 1))
})

test_that(".consolidate_groups handles logical columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    flag = c(TRUE, TRUE, FALSE),  # 2 TRUE, 1 FALSE -> majority TRUE
    consolidation_group = c(1, 1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_type(result$flag, "logical")
  expect_true(result$flag)  # Majority rule
})

test_that(".consolidate_groups handles factor columns", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28")),
    durata = c(31, 28),
    category = factor(c("A", "A")),
    consolidation_group = c(1, 1)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_s3_class(result$category, "factor")
  expect_equal(as.character(result$category), "A")
})

test_that(".consolidate_groups preserves column order", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),
    durata = 31,
    extra_col = "test",
    consolidation_group = 1
  )

  original_cols <- setdiff(names(dt), "consolidation_group")
  result <- longworkR:::.consolidate_groups(dt)

  # Original columns should come before n_periods_consolidated
  expect_true(which(names(result) == "n_periods_consolidated") >
              which(names(result) == "extra_col"))
})

test_that(".consolidate_groups handles empty groups correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = integer(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    durata = numeric(0),
    consolidation_group = integer(0)
  )

  result <- longworkR:::.consolidate_groups(dt)

  expect_equal(nrow(result), 0)
  expect_true("n_periods_consolidated" %in% names(result))
})


# 4. Tests for variable_handling parameter -----

test_that(".consolidate_groups validates variable_handling parameter", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(31, 28, 31),
    consolidation_group = rep("1_1", 3)
  )

  expect_error(
    longworkR:::.consolidate_groups(dt, variable_handling = "invalid"),
    "variable_handling must be 'weight' or 'first'"
  )
})

test_that(".consolidate_groups uses first non-NA for numeric with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    salary = c(1000, 2000, 3000),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should take first value (1000), not weighted mean
  expect_equal(result$salary, 1000)
})

test_that(".consolidate_groups uses weighted mean for numeric with 'weight' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    salary = c(1000, 2000, 3000),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "weight")

  # Should use weighted mean: (1000*10 + 2000*20 + 3000*30) / 60 = 2333.33
  expect_equal(result$salary, (1000*10 + 2000*20 + 3000*30) / 60, tolerance = 0.01)
})

test_that(".consolidate_groups uses first non-NA for character with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 50, 10),  # Second has longest duration
    contract = c("A", "B", "C"),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should take first value (A), not weighted mode (B)
  expect_equal(result$contract, "A")
})

test_that(".consolidate_groups uses weighted mode for character with 'weight' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 50, 10),  # Second has longest duration
    contract = c("A", "B", "C"),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "weight")

  # Should use weighted mode: B has duration 50, others have 10
  expect_equal(result$contract, "B")
})

test_that(".consolidate_groups uses first non-NA for logical with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    flag = c(FALSE, TRUE, TRUE),  # Majority is TRUE
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should take first value (FALSE), not majority (TRUE)
  expect_equal(result$flag, FALSE)
})

test_that(".consolidate_groups uses majority rule for logical with 'weight' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    flag = c(FALSE, TRUE, TRUE),  # Majority is TRUE
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "weight")

  # Should use majority rule: 2 out of 3 are TRUE
  expect_equal(result$flag, TRUE)
})

test_that(".consolidate_groups preserves integer type with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1L, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    count = c(5L, 10L, 15L),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should preserve integer type
  expect_type(result$count, "integer")
  expect_equal(result$count, 5L)
})

test_that(".consolidate_groups handles NA values correctly with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 20, 30),
    salary = c(NA_real_, 2000, 3000),
    contract = c(NA_character_, "B", "C"),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should skip NA and take first non-NA value
  expect_equal(result$salary, 2000)
  expect_equal(result$contract, "B")
})

test_that(".consolidate_groups uses first non-NA for factors with 'first' mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    cf = rep(1, 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    durata = c(10, 50, 10),
    contract = factor(c("TypeA", "TypeB", "TypeC"), levels = c("TypeA", "TypeB", "TypeC")),
    consolidation_group = rep("1_1", 3)
  )

  result <- longworkR:::.consolidate_groups(dt, variable_handling = "first")

  # Should take first value and preserve factor type
  expect_s3_class(result$contract, "factor")
  expect_equal(as.character(result$contract), "TypeA")
  expect_equal(levels(result$contract), c("TypeA", "TypeB", "TypeC"))
})
