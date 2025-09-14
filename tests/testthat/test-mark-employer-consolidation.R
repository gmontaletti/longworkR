test_that("mark_employer_consolidation works correctly", {
  
  # Create sample test data
  dt <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2, 3, 3),
    inizio = as.Date(c("2020-01-01", "2020-02-15", "2020-08-01", 
                       "2020-01-01", "2020-01-20", "2020-03-01",
                       "2020-01-01", "2020-06-01")),
    fine = as.Date(c("2020-02-01", "2020-05-01", "2020-10-01",
                     "2020-01-15", "2020-02-15", "2020-05-01",
                     "2020-03-01", "2020-08-01")),
    employer = c("A", "A", "B", "A", "A", "A", "C", "C")
  )
  
  # Test basic functionality
  mark_employer_consolidation(dt, "employer", min_lag = 30)
  
  # Should add consolidation_group column
  expect_true("consolidation_group" %in% names(dt))
  
  # Check that records are grouped correctly
  # Person 1: employer A (contracts 1,2) should be in same group (gap = 14 days < 30)
  # Person 1: employer B (contract 3) should be separate group
  expect_equal(length(unique(dt[cf == 1 & employer == "A", consolidation_group])), 1)
  expect_equal(length(unique(dt[cf == 1 & employer == "B", consolidation_group])), 1)
  
  # Person 2: all employer A contracts should check gaps
  person2_groups <- dt[cf == 2, consolidation_group]
  # Contract 1 (Jan 1-15) to Contract 2 (Jan 20-Feb 15): gap = 4 days < 30, same group
  # Contract 2 (Jan 20-Feb 15) to Contract 3 (Mar 1-May 1): gap = 14 days < 30, same group
  expect_equal(length(unique(person2_groups)), 1)
  
  # Person 3: both contracts for employer C should be separate (gap > 30 days)
  person3_groups <- dt[cf == 3, consolidation_group]
  expect_equal(length(unique(person3_groups)), 2)  # Should be 2 separate groups
})

test_that("mark_employer_consolidation handles edge cases", {
  
  # Test with single record
  dt_single <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-02-01"),
    employer = "A"
  )
  
  mark_employer_consolidation(dt_single, "employer")
  expect_true("consolidation_group" %in% names(dt_single))
  expect_equal(nrow(dt_single), 1)
  
  # Test with custom column name
  dt_custom <- data.table::data.table(
    cf = c(1, 1),
    inizio = as.Date(c("2020-01-01", "2020-01-10")),
    fine = as.Date(c("2020-01-05", "2020-01-15")),
    employer = c("A", "A")
  )
  
  mark_employer_consolidation(dt_custom, "employer", consolidation_col = "my_groups")
  expect_true("my_groups" %in% names(dt_custom))
  expect_false("consolidation_group" %in% names(dt_custom))
})

test_that("mark_employer_consolidation validates inputs correctly", {
  
  dt <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-02-01"),
    employer = "A"
  )
  
  # Test input validation
  expect_error(mark_employer_consolidation(data.frame(dt), "employer"), 
               "data must be a data.table object")
  
  expect_error(mark_employer_consolidation(dt, "nonexistent_col"),
               "employer_var must specify a valid column name")
  
  expect_error(mark_employer_consolidation(dt, "employer", min_lag = -1),
               "min_lag must be a non-negative numeric value")
  
  expect_error(mark_employer_consolidation(dt, "employer", consolidation_col = c("a", "b")),
               "consolidation_col must be a single character string")
})

test_that("mark_employer_consolidation preserves data integrity", {
  
  # Create test data
  original_dt <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-03-01")),
    fine = as.Date(c("2020-01-31", "2020-02-28", "2020-01-31", "2020-03-31")),
    employer = c("A", "A", "B", "B"),
    salary = c(1000, 1100, 2000, 2200),
    other_col = c("x", "y", "z", "w")
  )
  
  # Keep copy for comparison
  original_copy <- data.table::copy(original_dt)
  
  # Apply marking
  mark_employer_consolidation(original_dt, "employer")
  
  # Check that original columns are unchanged
  for (col in names(original_copy)) {
    expect_equal(original_dt[[col]], original_copy[[col]], info = paste("Column", col, "changed"))
  }
  
  # Check that only one new column was added
  expect_equal(ncol(original_dt), ncol(original_copy) + 1)
  
  # Check new column exists and has correct length
  expect_true("consolidation_group" %in% names(original_dt))
  expect_equal(length(original_dt$consolidation_group), nrow(original_dt))
  expect_true(all(!is.na(original_dt$consolidation_group)))
})