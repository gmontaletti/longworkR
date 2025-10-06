test_that("consolidate_employment main function works correctly", {
  # Create sample data
  sample_data <- data.table::data.table(
    cf = c(rep(1, 5), rep(2, 4)),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-06-01", "2023-07-01",
                      "2023-01-15", "2023-02-20", "2023-04-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-06-30", "2023-07-31",
                    "2023-02-15", "2023-03-20", "2023-04-30", "2023-05-31")),
    durata = c(31, 28, 31, 30, 31, 32, 29, 30, 31),
    arco = c(1, 1, 0, 1, 1, 1, 1, 0, 1),
    prior = c(1, 1, 0, 1, 1, 1, 1, 0, 1),
    over_id = c(1, 1, 0, 2, 2, 3, 3, 0, 4),
    datore = c("A", "A", NA, "B", "B", "C", "C", NA, "D"),
    stato = c("occ_ft", "occ_ft", "disoccupato", "occ_ft", "occ_ft", 
             "occ_pt", "occ_pt", "disoccupato", "occ_ft")
  )
  
  # Test mode = "none"
  result_none <- consolidate_employment(sample_data, mode = "none", show_progress = FALSE)
  expect_equal(nrow(result_none), nrow(sample_data))
  expect_identical(result_none, sample_data)
  
  # Test mode = "employer"
  result_employer <- consolidate_employment(
    sample_data, 
    mode = "employer",
    employer_var = "datore",
    min_lag = 35,
    show_progress = FALSE
  )
  expect_true(nrow(result_employer) <= nrow(sample_data))
  expect_true(all(c("cf", "inizio", "fine", "durata") %in% names(result_employer)))
  
  # Test mode = "temporal" (if over_id exists)
  if ("over_id" %in% names(sample_data)) {
    result_temporal <- consolidate_employment(
      sample_data,
      mode = "temporal", 
      consolidation_type = "overlapping",
      show_progress = FALSE
    )
    expect_true(nrow(result_temporal) <= nrow(sample_data))
  }
  
  # Test mode = "both"
  result_both <- consolidate_employment(
    sample_data,
    mode = "both",
    employer_var = "datore",
    min_lag = 35,
    consolidation_type = "overlapping",
    show_progress = FALSE
  )
  expect_true(nrow(result_both) <= nrow(result_employer))
  expect_true(nrow(result_both) <= nrow(sample_data))
  
  # Test error handling
  expect_error(
    consolidate_employment(sample_data, mode = "employer", show_progress = FALSE),
    "employer_var is required"
  )
  
  expect_error(
    consolidate_employment(sample_data, mode = "employer", employer_var = "nonexistent", show_progress = FALSE),
    "not found in data"
  )
  
  expect_error(
    consolidate_employment(list(a = 1), mode = "none", show_progress = FALSE),
    "must be a data.table"
  )
})

test_that("consolidate_by_employer works correctly", {
  # Create test data with specific employer patterns
  test_data <- data.table::data.table(
    cf = c(rep(1, 4), rep(2, 3)),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-06-01",
                      "2023-01-15", "2023-03-01", "2023-03-20")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-04-30", "2023-06-30",
                    "2023-02-15", "2023-03-15", "2023-04-20")),
    durata = c(31, 28, 30, 30, 32, 15, 32),
    employer = c("CompanyA", "CompanyA", "CompanyB", "CompanyB",
                 "CompanyC", "CompanyC", "CompanyC"),
    salary = c(50000, 52000, 60000, 62000, 45000, 46000, 47000)
  )
  
  # Test with 35-day threshold (should consolidate both CompanyA and CompanyB for person 1)
  # Person 1 gaps: 0 days (CompanyA Jan-Feb), 31 days (CompanyB Apr-Jun)
  # Both gaps <= 35, so all same-employer periods consolidate
  result <- consolidate_by_employer(test_data, "employer", min_lag = 35, show_progress = FALSE)

  expect_true(inherits(result, "data.table"))
  expect_true(nrow(result) < nrow(test_data))

  # Person 1 should have 2 periods after consolidation (CompanyA consolidated, CompanyB consolidated)
  person1_result <- result[cf == 1]
  expect_equal(nrow(person1_result), 2)

  # Check that CompanyA periods were consolidated for person 1
  companyA_person1 <- person1_result[employer == "CompanyA"]
  expect_equal(nrow(companyA_person1), 1)
  expect_equal(companyA_person1$inizio, as.Date("2023-01-01"))
  expect_equal(companyA_person1$fine, as.Date("2023-02-28"))

  # Check that CompanyB periods were also consolidated for person 1 (gap = 31 days <= 35)
  companyB_person1 <- person1_result[employer == "CompanyB"]
  expect_equal(nrow(companyB_person1), 1)
  expect_equal(companyB_person1$inizio, as.Date("2023-04-01"))
  expect_equal(companyB_person1$fine, as.Date("2023-06-30"))

  # Test with 10-day threshold
  # Person 1 gaps: 0 days (consolidates), 31 days (does not consolidate)
  # Person 2 gaps: 13 days (does not consolidate), 4 days (consolidates)
  # Expected: Person 1 = 3 periods (A+A, B, B), Person 2 = 2 periods (C, C+C)
  result_strict <- consolidate_by_employer(test_data, "employer", min_lag = 10, show_progress = FALSE)
  expect_equal(nrow(result_strict), 5)  # 3 + 2 = 5 total periods
  
  # Test error handling
  expect_error(
    consolidate_by_employer(test_data, "nonexistent", show_progress = FALSE),
    "must specify a valid column"
  )
  
  expect_error(
    consolidate_by_employer(test_data, "employer", min_lag = -1, show_progress = FALSE),
    "must be a non-negative"
  )
})

test_that("consolidate_by_temporal handles different consolidation types", {
  # Create test data with overlapping and consecutive periods
  test_data <- data.table::data.table(
    cf = rep(1, 5),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01", "2023-03-01", "2023-04-01")),
    fine = as.Date(c("2023-01-20", "2023-01-31", "2023-02-28", "2023-03-31", "2023-04-30")),
    durata = c(20, 17, 28, 31, 30),
    arco = c(1, 2, 1, 0, 1),
    over_id = c(1, 1, 2, 0, 3)
  )
  
  # Test overlapping consolidation
  result_overlapping <- consolidate_by_temporal(
    test_data,
    consolidation_type = "overlapping",
    show_progress = FALSE
  )
  
  if (!is.null(result_overlapping)) {
    expect_true(inherits(result_overlapping, "data.table"))
    # Periods with same over_id > 0 should be consolidated
    expect_true(nrow(result_overlapping) <= nrow(test_data))
  }
  
  # Test consecutive consolidation
  result_consecutive <- consolidate_by_temporal(
    test_data,
    consolidation_type = "consecutive", 
    show_progress = FALSE
  )
  
  if (!is.null(result_consecutive)) {
    expect_true(inherits(result_consecutive, "data.table"))
  }
  
  # Test both consolidation
  result_both <- consolidate_by_temporal(
    test_data,
    consolidation_type = "both",
    show_progress = FALSE
  )
  
  if (!is.null(result_both)) {
    expect_true(inherits(result_both, "data.table"))
    expect_true(nrow(result_both) <= nrow(result_overlapping))
  }
  
  # Test with missing over_id column
  test_no_over <- copy(test_data)
  test_no_over[, over_id := NULL]
  
  expect_warning(
    result_no_over <- consolidate_by_temporal(test_no_over, show_progress = FALSE),
    "over_id.*not found"
  )
  expect_equal(nrow(result_no_over), nrow(test_no_over))
})

test_that("is_consolidated utility function works", {
  original <- data.table::data.table(
    cf = rep(1, 5),
    inizio = as.Date(paste0("2023-0", 1:5, "-01")),
    fine = as.Date(paste0("2023-0", 1:5, "-28")),
    durata = rep(28, 5)
  )
  
  # Test non-consolidated data
  expect_false(is_consolidated(original))
  
  # Test with consolidation markers
  consolidated <- copy(original)
  consolidated[, collapsed := TRUE]
  expect_true(is_consolidated(consolidated))
  
  consolidated2 <- copy(original)
  consolidated2[, n_periods := 3]
  expect_true(is_consolidated(consolidated2))
  
  consolidated3 <- copy(original)
  consolidated3[, consolidation_id := 1:5]
  expect_true(is_consolidated(consolidated3))
  
  # Test with original data comparison
  smaller_data <- original[1:3]
  expect_true(is_consolidated(smaller_data, original))
  expect_false(is_consolidated(original, smaller_data))
  
  # Test non-data.table input
  expect_false(is_consolidated(list(a = 1)))
  expect_false(is_consolidated(NULL))
})

test_that("consolidation preserves data integrity", {
  # Create comprehensive test data
  test_data <- data.table::data.table(
    cf = c(rep(1, 3), rep(2, 3)),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01",
                      "2023-01-15", "2023-03-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-04-30",
                    "2023-02-15", "2023-03-31", "2023-05-31")),
    durata = c(31, 28, 30, 32, 31, 31),
    employer = c("A", "A", "B", "C", "C", "D"),
    salary = c(50000, 52000, 60000, 45000, 46000, 55000),
    arco = c(1, 1, 1, 1, 1, 1),
    over_id = c(1, 1, 2, 3, 3, 4)
  )
  
  # Test that all persons are preserved
  result <- consolidate_employment(
    test_data,
    mode = "employer",
    employer_var = "employer",
    min_lag = 35,
    show_progress = FALSE
  )
  
  expect_equal(unique(result$cf), unique(test_data$cf))
  expect_equal(length(unique(result$cf)), length(unique(test_data$cf)))
  
  # Test that date ranges are preserved or expanded correctly
  for (person in unique(test_data$cf)) {
    orig_person <- test_data[cf == person]
    result_person <- result[cf == person]
    
    expect_true(min(result_person$inizio) <= min(orig_person$inizio))
    expect_true(max(result_person$fine) >= max(orig_person$fine))
  }
  
  # Test that no data is lost (all employers still present)
  orig_employers <- unique(test_data$employer[!is.na(test_data$employer)])
  result_employers <- unique(result$employer[!is.na(result$employer)])
  expect_true(all(orig_employers %in% result_employers))
})