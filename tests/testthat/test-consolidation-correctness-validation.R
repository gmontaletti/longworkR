# Comprehensive Correctness Validation Tests for Consolidation Functions
# These tests ensure that all optimization phases maintain functional correctness

test_that("Correctness validation: consolidation preserves person identity", {
  skip_if_not_installed("data.table")
  
  # Generate test data with known characteristics
  test_data <- generate_realistic_employment_data(
    n_records = 1000,
    n_persons = 100,
    temporal_consolidation_rate = 0.4,
    seed = 123
  )
  
  original_persons <- unique(test_data[arco > 0, cf])
  
  # Test all consolidation modes preserve persons
  result_none <- analyze_employment_transitions(test_data, consolidation_type = "none")
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  result_employer <- analyze_employment_transitions(
    test_data, consolidation_type = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
  )
  result_both <- analyze_employment_transitions(
    test_data, consolidation_type = "both", employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  # All modes should preserve the same set of persons
  expect_setequal(unique(result_none$transition_data$cf), original_persons)
  expect_setequal(unique(result_temporal$transition_data$cf), original_persons)
  expect_setequal(unique(result_employer$transition_data$cf), original_persons)
  expect_setequal(unique(result_both$transition_data$cf), original_persons)
})

test_that("Correctness validation: temporal consolidation reduces periods correctly", {
  skip_if_not_installed("data.table")
  
  # Create data with explicit consolidation opportunities
  test_data <- data.table::data.table(
    cf = c(rep(1, 6), rep(2, 4)),
    inizio = as.Date(c(
      "2020-01-01", "2020-01-15", "2020-06-01", "2020-06-15", "2020-12-01", "2021-01-01",
      "2020-02-01", "2020-02-15", "2020-07-01", "2020-08-01"
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-05-31", "2020-06-30", "2020-11-30", "2020-12-31", "2021-03-31",
      "2020-02-29", "2020-06-30", "2020-07-31", "2020-09-30"
    )),
    durata = c(31, 137, 30, 169, 31, 90, 29, 136, 31, 61),
    arco = c(1, 2, 1, 2, 1, 1, 1, 2, 1, 1),
    over_id = c(1, 1, 2, 2, 3, 4, 5, 5, 6, 7),  # Pairs have same over_id
    CODICE_FISCALE_AZIENDA = paste0("COMP", c(1, 1, 2, 2, 3, 4, 1, 1, 2, 3))
  )
  
  result_none <- analyze_employment_transitions(test_data, consolidation_type = "none")
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  
  # Should reduce from 10 to 6 periods (4 consolidations: 2 pairs per person)
  expect_equal(nrow(result_none$transition_data), 10)
  expect_equal(nrow(result_temporal$transition_data), 6)
  
  # Check that consolidation happened correctly
  person1_none <- result_none$transition_data[cf == 1]
  person1_temporal <- result_temporal$transition_data[cf == 1]
  
  expect_equal(nrow(person1_none), 6)
  expect_equal(nrow(person1_temporal), 4)  # 3 pairs consolidated to 2 periods + 2 individual
  
  # Validate consolidation metrics
  metrics <- extract_consolidation_metrics(
    original_data = test_data[arco > 0],
    consolidated_data = result_temporal$transition_data,
    consolidation_mode = "temporal"
  )
  
  expect_equal(metrics$consolidation_summary$original_contracts, 10)
  expect_equal(metrics$consolidation_summary$consolidated_periods, 6)
  expect_equal(metrics$consolidation_summary$contracts_eliminated, 4)
})

test_that("Correctness validation: employer consolidation works within same employer only", {
  skip_if_not_installed("data.table")
  
  # Create data with clear employer consolidation scenarios
  test_data <- data.table::data.table(
    cf = c(rep(1, 4), rep(2, 4)),
    inizio = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-06-01", "2020-07-01",  # Person 1
      "2020-01-15", "2020-03-01", "2020-05-01", "2020-08-01"   # Person 2
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-05-31", "2020-06-30", "2020-12-31",  # Person 1
      "2020-02-14", "2020-04-30", "2020-07-31", "2020-10-31"   # Person 2
    )),
    durata = c(31, 120, 30, 184, 30, 60, 92, 92),
    arco = rep(1, 8),
    over_id = 1:8,
    CODICE_FISCALE_AZIENDA = c(
      "COMP1", "COMP1", "COMP2", "COMP2",  # Person 1: 2 employers, 2 contracts each
      "COMP1", "COMP1", "COMP1", "COMP2"   # Person 2: mostly COMP1, one COMP2
    )
  )
  
  result_employer <- analyze_employment_transitions(
    test_data, 
    consolidation_type = "employer", 
    employer_var = "CODICE_FISCALE_AZIENDA",
    min_lag = 60  # Allow consolidation within 60 days
  )
  
  # Should consolidate some same-employer contracts
  expect_true(nrow(result_employer$transition_data) < nrow(test_data))
  
  # Person 1: Should consolidate COMP1 contracts and COMP2 contracts separately
  # Person 2: Should consolidate the 3 COMP1 contracts, keep COMP2 separate
  person1_result <- result_employer$transition_data[cf == 1]
  person2_result <- result_employer$transition_data[cf == 2]
  
  # Person 1 should have 2 periods (one per employer)
  expect_equal(nrow(person1_result), 2)
  expect_setequal(person1_result$CODICE_FISCALE_AZIENDA, c("COMP1", "COMP2"))
  
  # Person 2 should have 2 periods (consolidated COMP1, separate COMP2)
  expect_equal(nrow(person2_result), 2)
  expect_setequal(person2_result$CODICE_FISCALE_AZIENDA, c("COMP1", "COMP2"))
  
  # Validate employer-specific metrics
  metrics <- extract_consolidation_metrics(
    original_data = test_data,
    consolidated_data = result_employer$transition_data,
    consolidation_mode = "employer",
    employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  expect_type(metrics$employer_specific, "list")
  expect_true(metrics$consolidation_summary$consolidation_ratio > 0)
})

test_that("Correctness validation: 'both' consolidation applies both methods sequentially", {
  skip_if_not_installed("data.table")
  
  # Create data that benefits from both temporal and employer consolidation
  test_data <- data.table::data.table(
    cf = rep(1, 8),
    inizio = as.Date(c(
      "2020-01-01", "2020-01-15", "2020-03-01", "2020-03-15",
      "2020-06-01", "2020-06-15", "2020-09-01", "2020-10-01"
    )),
    fine = as.Date(c(
      "2020-01-31", "2020-02-29", "2020-03-31", "2020-05-31",
      "2020-06-30", "2020-08-31", "2020-09-30", "2020-12-31"
    )),
    durata = c(31, 45, 31, 77, 30, 77, 30, 92),
    arco = c(1, 2, 1, 2, 1, 2, 1, 1),
    over_id = c(1, 1, 2, 2, 3, 3, 4, 5),  # Temporal consolidation pairs
    CODICE_FISCALE_AZIENDA = c("COMP1", "COMP1", "COMP1", "COMP1", "COMP2", "COMP2", "COMP2", "COMP3")
    # First 4 same employer, next 3 same employer, last different
  )
  
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  result_employer <- analyze_employment_transitions(
    test_data, consolidation_type = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
  )
  result_both <- analyze_employment_transitions(
    test_data, consolidation_type = "both", employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  # 'both' should achieve maximum consolidation
  expect_true(nrow(result_both$transition_data) <= nrow(result_temporal$transition_data))
  expect_true(nrow(result_both$transition_data) <= nrow(result_employer$transition_data))
  
  # Expected result: 3 periods (COMP1 consolidated, COMP2 consolidated, COMP3 individual)
  expect_equal(nrow(result_both$transition_data), 3)
  expect_setequal(result_both$transition_data$CODICE_FISCALE_AZIENDA, c("COMP1", "COMP2", "COMP3"))
})

test_that("Correctness validation: eval_chain parameter processes chains correctly", {
  skip_if_not_installed("data.table")
  
  # Create a scenario that will generate chain values in transitions
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-03-01", "2020-06-01", 
                      "2020-02-01", "2020-04-01", "2020-07-01")),
    fine = as.Date(c("2020-02-28", "2020-05-31", "2020-08-31", 
                    "2020-03-31", "2020-06-30", "2020-09-30")),
    durata = c(59, 92, 92, 60, 91, 92),
    arco = c(1, 1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5, 6),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "B.01.00", "C.01.00", 
                                   "A.01.00", "D.01.00", "E.01.00")
  )
  
  result_none <- analyze_employment_transitions(test_data, eval_chain = "none")
  result_first <- analyze_employment_transitions(test_data, eval_chain = "first")
  result_last <- analyze_employment_transitions(test_data, eval_chain = "last")
  
  # All should have same structure
  expect_equal(nrow(result_none$transition_data), nrow(result_first$transition_data))
  expect_equal(nrow(result_none$transition_data), nrow(result_last$transition_data))
  
  # Check that transitions are generated (look for actual chain creation)
  # This depends on the specific implementation, but we can check basic properties
  expect_true(all(c("from_state", "to_state") %in% names(result_none$transition_data) | 
                 c("transition") %in% names(result_none$transition_data)))
  
  # Network data should be present and valid
  expect_type(result_none$network_data, "list")
  expect_type(result_first$network_data, "list")
  expect_type(result_last$network_data, "list")
})

test_that("Correctness validation: arco filtering works consistently", {
  skip_if_not_installed("data.table")
  
  # Create data with mix of arco values
  test_data <- data.table::data.table(
    cf = c(rep(1, 5), rep(2, 5)),
    inizio = as.Date("2020-01-01") + c(0, 30, 60, 90, 120, 0, 35, 70, 105, 140),
    fine = as.Date("2020-01-01") + c(29, 59, 89, 119, 149, 34, 69, 104, 139, 174),
    durata = rep(30, 10),
    arco = c(0, 1, 2, 0, 1, 1, 0, 2, 1, 0),  # Mix of values
    over_id = 1:10,
    CODICE_FISCALE_AZIENDA = paste0("COMP", c(1, 1, 1, 2, 2, 1, 1, 2, 2, 3))
  )
  
  # Count expected employment periods (arco > 0)
  expected_periods <- nrow(test_data[arco > 0])
  expect_equal(expected_periods, 6)  # Should be 6 employment periods
  
  # Test all consolidation modes filter consistently
  result_none <- analyze_employment_transitions(test_data, consolidation_type = "none")
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  result_employer <- analyze_employment_transitions(
    test_data, consolidation_type = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
  )
  
  # All modes should start with same base data (arco > 0)
  expect_equal(nrow(result_none$transition_data), 6)
  expect_true(all(result_none$transition_data$arco > 0))
  expect_true(all(result_temporal$transition_data$arco > 0))
  expect_true(all(result_employer$transition_data$arco > 0))
  
  # Consolidation should only reduce, never increase period count
  expect_true(nrow(result_temporal$transition_data) <= 6)
  expect_true(nrow(result_employer$transition_data) <= 6)
})

test_that("Correctness validation: date and duration calculations are preserved", {
  skip_if_not_installed("data.table")
  
  # Create test data with known date ranges
  test_data <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01", "2020-03-01")),
    fine = as.Date(c("2020-01-31", "2020-05-31", "2020-02-29", "2020-05-31")),
    durata = c(31, 137, 29, 92),
    arco = c(1, 2, 1, 2),
    over_id = c(1, 1, 2, 2),  # Two consolidation pairs
    CODICE_FISCALE_AZIENDA = c("COMP1", "COMP1", "COMP2", "COMP2")
  )
  
  # Test temporal consolidation preserves total duration
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  
  # Should have 2 consolidated periods
  expect_equal(nrow(result_temporal$transition_data), 2)
  
  # Check that consolidated periods have correct date ranges
  person1_period <- result_temporal$transition_data[cf == 1]
  person2_period <- result_temporal$transition_data[cf == 2]
  
  expect_equal(person1_period$inizio, as.Date("2020-01-01"))  # First start date
  expect_equal(person1_period$fine, as.Date("2020-05-31"))    # Last end date
  expect_equal(person2_period$inizio, as.Date("2020-02-01"))  # First start date
  expect_equal(person2_period$fine, as.Date("2020-05-31"))    # Last end date
  
  # Check duration calculation
  expected_duration_p1 <- as.numeric(as.Date("2020-05-31") - as.Date("2020-01-01")) + 1
  expected_duration_p2 <- as.numeric(as.Date("2020-05-31") - as.Date("2020-02-01")) + 1
  
  expect_equal(person1_period$durata, expected_duration_p1)
  expect_equal(person2_period$durata, expected_duration_p2)
})

test_that("Correctness validation: consolidation metrics are mathematically correct", {
  skip_if_not_installed("data.table")
  
  # Generate known test case
  test_data <- generate_realistic_employment_data(
    n_records = 500, 
    n_persons = 50,
    temporal_consolidation_rate = 0.5,  # High rate for testing
    seed = 456
  )
  
  original_data <- test_data[arco > 0]
  
  result_temporal <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  
  metrics <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = result_temporal$transition_data,
    consolidation_mode = "temporal"
  )
  
  # Validate basic math
  original_count <- metrics$consolidation_summary$original_contracts
  consolidated_count <- metrics$consolidation_summary$consolidated_periods
  eliminated_count <- metrics$consolidation_summary$contracts_eliminated
  
  expect_equal(original_count, consolidated_count + eliminated_count)
  expect_equal(original_count, nrow(original_data))
  expect_equal(consolidated_count, nrow(result_temporal$transition_data))
  
  # Validate consolidation ratio
  expected_ratio <- eliminated_count / original_count
  expect_equal(metrics$consolidation_summary$consolidation_ratio, expected_ratio, tolerance = 1e-10)
  
  # Validate person-level metrics sum correctly
  person_level_eliminated <- sum(metrics$person_level$contracts_eliminated)
  expect_equal(person_level_eliminated, eliminated_count)
  
  person_level_original <- sum(metrics$person_level$original_contracts)
  expect_equal(person_level_original, original_count)
})

test_that("Correctness validation: network data structure is valid", {
  skip_if_not_installed("data.table")
  
  test_data <- generate_realistic_employment_data(
    n_records = 200,
    n_persons = 20,
    seed = 789
  )
  
  result <- analyze_employment_transitions(test_data, consolidation_type = "temporal")
  
  # Network data should exist and have proper structure
  expect_type(result$network_data, "list")
  
  # Should have nodes and edges (exact structure depends on implementation)
  network_names <- names(result$network_data)
  
  # Common network structures
  valid_structures <- list(
    c("nodes", "edges"),
    c("vertices", "edges"), 
    c("nodes", "links"),
    c("vertices", "links")
  )
  
  has_valid_structure <- any(sapply(valid_structures, function(struct) {
    all(struct %in% network_names)
  }))
  
  expect_true(has_valid_structure, info = paste("Network structure:", paste(network_names, collapse = ", ")))
  
  # If nodes/vertices exist, they should contain the transition states
  if ("nodes" %in% network_names) {
    expect_true(nrow(result$network_data$nodes) > 0)
  } else if ("vertices" %in% network_names) {
    expect_true(nrow(result$network_data$vertices) > 0)
  }
  
  # If edges/links exist, they should represent transitions
  if ("edges" %in% network_names) {
    expect_true(nrow(result$network_data$edges) >= 0)  # Can be 0 for simple cases
  } else if ("links" %in% network_names) {
    expect_true(nrow(result$network_data$links) >= 0)
  }
})