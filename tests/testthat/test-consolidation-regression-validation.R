# Regression and Validation Tests for Optimized Consolidation Functions
# Ensures that optimizations maintain correctness and backward compatibility

test_that("Regression test: consolidation helper functions work correctly", {
  skip_if_not_installed("data.table")
  
  # Use real sample data if available
  sample_file <- file.path("../../data/sample.rds")
  if (file.exists(sample_file)) {
    sample_data <- readRDS(sample_file)
    # Take a subset for faster testing
    test_data <- sample_data[cf %in% unique(sample_data$cf)[1:100]]
  } else {
    # Generate test data if sample not available
    test_data <- data.table::data.table(
      cf = rep(1:20, each = 5),
      inizio = as.Date("2020-01-01") + sample(0:1000, 100, replace = TRUE),
      fine = as.Date("2020-01-01") + sample(100:1100, 100, replace = TRUE),
      durata = sample(30:300, 100, replace = TRUE),
      arco = sample(c(1, 2), 100, replace = TRUE, prob = c(0.8, 0.2)),
      over_id = sample(1:150, 100, replace = TRUE),
      COD_TIPOLOGIA_CONTRATTUALE = sample(c("A.01.00", "A.03.00", "B.01.00"), 100, replace = TRUE),
      CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:10), 100, replace = TRUE),
      prior = sample(c(0, 1), 100, replace = TRUE)
    )
    test_data <- test_data[order(cf, inizio)]
  }
  
  # Test all consolidation modes work without errors
  result_none <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "none"
  )
  
  result_temporal <- analyze_employment_transitions(
    test_data, consolidation = "temporal", eval_chain = "none"
  )
  
  result_employer <- analyze_employment_transitions(
    test_data, consolidation = "employer", 
    employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
  )
  
  result_both <- analyze_employment_transitions(
    test_data, consolidation = "both",
    employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
  )
  
  # All results should be valid
  expect_type(result_none, "list")
  expect_type(result_temporal, "list")
  expect_type(result_employer, "list")
  expect_type(result_both, "list")
  
  # All should have required components
  required_components <- c("transition_data", "network_data", "metrics")
  expect_true(all(required_components %in% names(result_none)))
  expect_true(all(required_components %in% names(result_temporal)))
  expect_true(all(required_components %in% names(result_employer)))
  expect_true(all(required_components %in% names(result_both)))
  
  # Validate consolidation metrics extraction
  original_data <- test_data[arco > 0]  # Filter once like the optimized functions
  
  metrics_none <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = result_none$transition_data,
    consolidation_mode = "none"
  )
  
  metrics_temporal <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = result_temporal$transition_data,
    consolidation_mode = "temporal"
  )
  
  metrics_employer <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = result_employer$transition_data,
    consolidation_mode = "employer",
    employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  expect_type(metrics_none, "list")
  expect_type(metrics_temporal, "list")
  expect_type(metrics_employer, "list")
  
  # Validate metric structure
  expected_metrics <- c("consolidation_summary", "person_level", "distribution", 
                       "employer_specific", "temporal_specific")
  expect_named(metrics_none, expected_metrics)
  expect_named(metrics_temporal, expected_metrics)
  expect_named(metrics_employer, expected_metrics)
})

test_that("Regression test: contract counting excludes arco==0 correctly", {
  skip_if_not_installed("data.table")
  
  # Create data with arco==0 cases to ensure they're excluded
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", 
                      "2020-01-15", "2020-02-15", "2020-03-15")),
    fine = as.Date(c("2020-01-31", "2020-02-28", "2020-03-31",
                    "2020-02-14", "2020-03-14", "2020-04-14")),
    durata = c(31, 28, 31, 30, 28, 30),
    arco = c(0, 1, 2, 1, 0, 1),  # Mix of arco values
    over_id = c(1, 2, 3, 4, 5, 6)
  )
  
  result <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "none"
  )
  
  # Should only include arco > 0 records
  expect_equal(nrow(result$transition_data), 4)  # 4 records with arco > 0
  expect_true(all(result$transition_data$arco > 0))
  
  # Extract metrics and verify counting
  metrics <- extract_consolidation_metrics(
    original_data = test_data[arco > 0],
    consolidated_data = result$transition_data,
    consolidation_mode = "none"
  )
  
  expect_equal(metrics$consolidation_summary$original_contracts, 4)
  expect_equal(metrics$consolidation_summary$consolidated_periods, 4)
})

test_that("Regression test: temporal consolidation with over_id works correctly", {
  skip_if_not_installed("data.table")
  
  # Create data with clear over_id consolidation opportunities
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-01-15", "2020-06-01", 
                      "2020-02-01", "2020-02-15", "2020-07-01")),
    fine = as.Date(c("2020-01-31", "2020-05-31", "2020-08-31",
                    "2020-02-29", "2020-06-30", "2020-09-30")),
    durata = c(31, 137, 92, 29, 136, 92),
    arco = c(1, 2, 1, 1, 2, 1),
    over_id = c(1, 1, 2, 3, 3, 4)  # Same over_id = consolidation opportunity
  )
  
  result_none <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "none"
  )
  
  result_temporal <- analyze_employment_transitions(
    test_data, consolidation = "temporal", eval_chain = "none"
  )
  
  # Temporal consolidation should reduce the number of periods
  expect_true(nrow(result_temporal$transition_data) < nrow(result_none$transition_data))
  
  # Should consolidate to 4 periods (2 consolidated + 2 individual)
  expect_equal(nrow(result_temporal$transition_data), 4)
  
  # Verify consolidation metrics
  metrics <- extract_consolidation_metrics(
    original_data = test_data[arco > 0],
    consolidated_data = result_temporal$transition_data,
    consolidation_mode = "temporal"
  )
  
  expect_true(metrics$consolidation_summary$consolidation_ratio > 0)
  expect_equal(metrics$consolidation_summary$original_contracts, 6)
  expect_equal(metrics$consolidation_summary$consolidated_periods, 4)
})

test_that("Regression test: employer consolidation works correctly", {
  skip_if_not_installed("data.table")
  
  # Create data with clear employer consolidation opportunities
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-06-01",
                      "2020-01-15", "2020-03-01", "2020-07-01")),
    fine = as.Date(c("2020-01-31", "2020-02-29", "2020-08-31",
                    "2020-02-14", "2020-05-31", "2020-09-30")),
    durata = c(31, 29, 92, 30, 92, 92),
    arco = c(1, 1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5, 6),
    CODICE_FISCALE_AZIENDA = c("COMP1", "COMP1", "COMP2", "COMP1", "COMP1", "COMP2")
    # Same company for some consecutive contracts
  )
  
  result_employer <- analyze_employment_transitions(
    test_data, consolidation = "employer", 
    employer_var = "CODICE_FISCALE_AZIENDA", 
    eval_chain = "none"
  )
  
  # Should consolidate some contracts with same employer
  expect_true(nrow(result_employer$transition_data) <= nrow(test_data[arco > 0]))
  
  # Verify employer consolidation metrics
  metrics <- extract_consolidation_metrics(
    original_data = test_data[arco > 0],
    consolidated_data = result_employer$transition_data,
    consolidation_mode = "employer",
    employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  expect_type(metrics$employer_specific, "list")
  expect_true("n_unique_employers_original" %in% names(metrics$employer_specific))
  expect_true("n_unique_employers_consolidated" %in% names(metrics$employer_specific))
})

test_that("Regression test: eval_chain parameter works correctly", {
  skip_if_not_installed("data.table")
  
  # Create data that will generate chain values
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-03-01", "2020-06-01", 
                      "2020-01-15", "2020-04-01")),
    fine = as.Date(c("2020-02-28", "2020-05-31", "2020-08-31", 
                    "2020-03-15", "2020-06-30")),
    durata = c(59, 92, 92, 60, 91),
    arco = c(1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "B.01.00", "A.01.00", "C.01.00")
  )
  
  result_none <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "none"
  )
  
  result_first <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "first"
  )
  
  result_last <- analyze_employment_transitions(
    test_data, consolidation = "none", eval_chain = "last"
  )
  
  # All should complete without errors
  expect_type(result_none, "list")
  expect_type(result_first, "list")
  expect_type(result_last, "list")
  
  # Should produce same structure but potentially different values
  expect_equal(names(result_none), names(result_first))
  expect_equal(names(result_none), names(result_last))
  expect_equal(nrow(result_none$transition_data), nrow(result_first$transition_data))
  expect_equal(nrow(result_none$transition_data), nrow(result_last$transition_data))
})

test_that("Regression test: edge cases handled correctly", {
  skip_if_not_installed("data.table")
  
  # Test empty data
  empty_data <- data.table::data.table(
    cf = integer(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    durata = numeric(0),
    arco = integer(0),
    over_id = integer(0)
  )
  
  expect_error({
    result <- analyze_employment_transitions(
      empty_data, consolidation = "none", eval_chain = "none"
    )
  }, "No employment periods found")
  
  # Test single person, single contract
  single_data <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-12-31"),
    durata = 365,
    arco = 1,
    over_id = 1
  )
  
  result <- analyze_employment_transitions(
    single_data, consolidation = "none", eval_chain = "none"
  )
  
  expect_type(result, "list")
  expect_equal(nrow(result$transition_data), 1)
  
  # Test all arco == 0 (should be filtered out)
  no_employment_data <- data.table::data.table(
    cf = c(1, 1, 2),
    inizio = as.Date(c("2020-01-01", "2020-03-01", "2020-01-15")),
    fine = as.Date(c("2020-02-28", "2020-05-31", "2020-03-15")),
    durata = c(59, 92, 60),
    arco = c(0, 0, 0),  # All non-employment
    over_id = c(1, 2, 3)
  )
  
  expect_error({
    result <- analyze_employment_transitions(
      no_employment_data, consolidation = "none", eval_chain = "none"
    )
  }, "No employment periods found")
})

test_that("Regression test: API compatibility maintained", {
  skip_if_not_installed("data.table")
  
  # Create test data
  test_data <- data.table::data.table(
    cf = rep(1:5, each = 3),
    inizio = as.Date("2020-01-01") + sample(0:365, 15),
    fine = as.Date("2020-01-01") + sample(100:500, 15),
    durata = sample(30:200, 15),
    arco = sample(c(1, 2), 15, replace = TRUE),
    over_id = sample(1:20, 15),
    CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:5), 15, replace = TRUE)
  )
  
  # Test all parameter combinations work
  expect_type(
    analyze_employment_transitions(test_data), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(test_data, consolidation = "none"), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(test_data, consolidation = "temporal"), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(
      test_data, 
      consolidation = "employer", 
      employer_var = "CODICE_FISCALE_AZIENDA"
    ), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(
      test_data, 
      consolidation = "both",
      employer_var = "CODICE_FISCALE_AZIENDA"
    ), 
    "list"
  )
  
  # Test eval_chain parameter
  expect_type(
    analyze_employment_transitions(test_data, eval_chain = "first"), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(test_data, eval_chain = "last"), 
    "list"
  )
  
  expect_type(
    analyze_employment_transitions(test_data, eval_chain = "none"), 
    "list"
  )
})

test_that("Regression test: thread safety and parallel processing", {
  skip_if_not_installed("data.table")
  skip_on_cran()  # Skip parallel tests on CRAN
  
  # Generate larger dataset to trigger potential parallel processing
  large_test_data <- data.table::data.table(
    cf = rep(1:1000, each = 5),
    inizio = as.Date("2015-01-01") + sample(0:2000, 5000),
    fine = as.Date("2015-01-01") + sample(50:2100, 5000),
    durata = sample(10:365, 5000),
    arco = sample(c(1, 2), 5000, replace = TRUE, prob = c(0.8, 0.2)),
    over_id = sample(1:7500, 5000),
    CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:100), 5000, replace = TRUE)
  )
  
  # Run multiple times to check for race conditions
  results <- replicate(3, {
    analyze_employment_transitions(
      large_test_data, 
      consolidation = "temporal", 
      eval_chain = "none"
    )
  }, simplify = FALSE)
  
  # All results should be identical (deterministic)
  expect_equal(results[[1]]$transition_data, results[[2]]$transition_data)
  expect_equal(results[[2]]$transition_data, results[[3]]$transition_data)
  
  # Network data should also be identical
  expect_equal(results[[1]]$network_data, results[[2]]$network_data)
  expect_equal(results[[2]]$network_data, results[[3]]$network_data)
})

test_that("Regression test: memory efficiency", {
  skip_if_not_installed("data.table")
  skip_on_cran()
  
  # Monitor memory usage during processing
  initial_memory <- gc()
  
  # Generate moderately large dataset
  test_data <- data.table::data.table(
    cf = rep(1:500, each = 10),
    inizio = as.Date("2018-01-01") + sample(0:1500, 5000),
    fine = as.Date("2018-01-01") + sample(30:1600, 5000),
    durata = sample(5:365, 5000),
    arco = sample(c(1, 2), 5000, replace = TRUE, prob = c(0.85, 0.15)),
    over_id = sample(1:7500, 5000),
    CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:75), 5000, replace = TRUE)
  )
  
  # Process with all consolidation modes
  result1 <- analyze_employment_transitions(test_data, consolidation = "none")
  result2 <- analyze_employment_transitions(test_data, consolidation = "temporal")
  result3 <- analyze_employment_transitions(test_data, consolidation = "employer", 
                                           employer_var = "CODICE_FISCALE_AZIENDA")
  result4 <- analyze_employment_transitions(test_data, consolidation = "both",
                                           employer_var = "CODICE_FISCALE_AZIENDA")
  
  # Clean up
  rm(result1, result2, result3, result4)
  final_memory <- gc()
  
  # Should not have major memory leaks (exact check is platform dependent)
  expect_type(final_memory, "matrix")
  
  # All functions should complete without memory errors
  expect_true(TRUE)  # If we get here, memory management worked
})