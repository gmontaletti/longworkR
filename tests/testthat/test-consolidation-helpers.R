test_that("extract_consolidation_metrics works with no consolidation", {
  skip_if_not_installed("data.table")

  # Create test data
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-03-01",
      "2020-06-01",
      "2020-01-15",
      "2020-04-01"
    )),
    fine = as.Date(c(
      "2020-02-28",
      "2020-05-31",
      "2020-08-31",
      "2020-03-15",
      "2020-06-30"
    )),
    durata = as.numeric(c(59, 92, 92, 60, 91)),
    arco = c(1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5)
  )

  # Test no consolidation mode
  metrics <- extract_consolidation_metrics(
    original_data = test_data,
    consolidated_data = test_data, # Same data for no consolidation
    consolidation_mode = "none"
  )

  expect_type(metrics, "list")
  expect_named(
    metrics,
    c(
      "consolidation_summary",
      "person_level",
      "distribution",
      "employer_specific",
      "temporal_specific"
    )
  )
  expect_equal(metrics$consolidation_summary$consolidation_ratio, 0.0)
  expect_equal(metrics$consolidation_summary$original_contracts, 5)
  expect_equal(metrics$consolidation_summary$consolidated_periods, 5)
  expect_equal(metrics$person_level[, sum(contracts_eliminated)], 0)
})

test_that("extract_consolidation_metrics works with temporal consolidation", {
  skip_if_not_installed("data.table")

  # Create test data with over_id for consolidation
  original_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-01-15",
      "2020-06-01",
      "2020-01-01",
      "2020-01-20",
      "2020-04-01"
    )),
    fine = as.Date(c(
      "2020-02-28",
      "2020-05-31",
      "2020-08-31",
      "2020-03-15",
      "2020-03-31",
      "2020-06-30"
    )),
    durata = as.numeric(c(59, 137, 92, 75, 71, 91)),
    arco = c(1, 2, 1, 1, 2, 1),
    over_id = c(1, 1, 2, 3, 3, 4) # Same over_id means consolidation opportunity
  )

  # Create consolidated data (simulate consolidation effect)
  consolidated_data <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2020-01-01", "2020-04-01")),
    fine = as.Date(c("2020-05-31", "2020-08-31", "2020-03-31", "2020-06-30")),
    durata = as.numeric(c(196, 92, 146, 91)),
    arco = c(2, 1, 2, 1),
    over_id = c(1, 2, 3, 4)
  )

  metrics <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = consolidated_data,
    consolidation_mode = "temporal",
    consolidation_type = "both"
  )

  expect_type(metrics, "list")
  expect_true(metrics$consolidation_summary$consolidation_ratio > 0)
  expect_equal(metrics$consolidation_summary$original_contracts, 6)
  expect_equal(metrics$consolidation_summary$consolidated_periods, 4)
  expect_equal(metrics$consolidation_summary$contracts_eliminated, 2)
  expect_equal(metrics$consolidation_summary$consolidation_mode, "temporal")
  expect_true("temporal_specific" %in% names(metrics))
  expect_false(is.null(metrics$temporal_specific))
})

test_that("extract_consolidation_metrics works with employer consolidation", {
  skip_if_not_installed("data.table")

  # Create test data with employer information
  original_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-03-01",
      "2020-06-01",
      "2020-01-15",
      "2020-04-01"
    )),
    fine = as.Date(c(
      "2020-02-28",
      "2020-05-31",
      "2020-08-31",
      "2020-03-15",
      "2020-06-30"
    )),
    durata = as.numeric(c(59, 92, 92, 60, 91)),
    arco = c(1, 1, 1, 1, 1),
    employer_id = c("A", "A", "B", "A", "A") # Same employer for some contracts
  )

  # Create consolidated data (contracts from same employer consolidated)
  consolidated_data <- data.table::data.table(
    cf = c(1, 1, 2),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2020-01-15")),
    fine = as.Date(c("2020-05-31", "2020-08-31", "2020-06-30")),
    durata = as.numeric(c(151, 92, 151)),
    arco = c(1, 1, 1),
    employer_id = c("A", "B", "A")
  )

  metrics <- extract_consolidation_metrics(
    original_data = original_data,
    consolidated_data = consolidated_data,
    consolidation_mode = "employer",
    employer_var = "employer_id",
    min_lag = 8
  )

  expect_type(metrics, "list")
  expect_equal(metrics$consolidation_summary$consolidation_mode, "employer")
  expect_equal(metrics$consolidation_summary$employer_variable, "employer_id")
  expect_equal(metrics$consolidation_summary$employer_lag_threshold, 8)
  expect_true("employer_specific" %in% names(metrics))
  expect_false(is.null(metrics$employer_specific))
})

test_that("extract_consolidation_metrics handles edge cases", {
  skip_if_not_installed("data.table")

  # Single person, single contract
  single_data <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-12-31"),
    durata = 366,
    arco = 1
  )

  metrics <- extract_consolidation_metrics(
    original_data = single_data,
    consolidated_data = single_data,
    consolidation_mode = "none"
  )

  expect_type(metrics, "list")
  expect_equal(metrics$consolidation_summary$consolidation_ratio, 0.0)
  expect_equal(nrow(metrics$person_level), 1)

  # Empty data
  empty_data <- data.table::data.table(
    cf = integer(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    durata = numeric(0),
    arco = integer(0)
  )

  expect_error(
    extract_consolidation_metrics(
      original_data = empty_data,
      consolidated_data = empty_data,
      consolidation_mode = "none"
    ),
    NA # Should not error
  )
})

test_that("analyze_employment_transitions_with_metrics wrapper works", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("vecshift")

  # Create minimal test data that works with analyze_employment_transitions
  test_data <- data.table::data.table(
    cf = c(1, 1, 2, 2),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2020-01-15", "2020-04-01")),
    fine = as.Date(c("2020-05-31", "2020-12-31", "2020-03-15", "2020-06-30")),
    durata = as.numeric(c(152, 214, 60, 91)),
    arco = c(1, 1, 1, 1),
    CONTRACT_TYPE = c("A", "B", "A", "B")
  )

  # Test with no consolidation (simplest case)
  result <- analyze_employment_transitions_with_metrics(
    pipeline_result = test_data,
    transition_variable = "CONTRACT_TYPE",
    consolidation_mode = "none",
    show_progress = FALSE
  )

  expect_type(result, "list")
  expect_s3_class(result, "longworkR_transitions_with_metrics")
  expect_named(
    result,
    c("transition_results", "consolidation_metrics", "consolidation_summary")
  )

  # Check that transition results exist
  expect_true(!is.null(result$transition_results))

  # Check consolidation metrics
  expect_equal(
    result$consolidation_metrics$consolidation_summary$consolidation_mode,
    "none"
  )
  expect_equal(
    result$consolidation_metrics$consolidation_summary$consolidation_ratio,
    0.0
  )

  # Check consolidation summary
  expect_type(result$consolidation_summary, "list")
  expect_s3_class(
    result$consolidation_summary,
    "longworkR_consolidation_summary"
  )
})

test_that("summarize_consolidation creates proper summaries", {
  skip_if_not_installed("data.table")

  # Create test consolidation metrics
  test_metrics <- list(
    consolidation_summary = list(
      consolidation_mode = "temporal",
      original_contracts = 10,
      consolidated_periods = 6,
      contracts_eliminated = 4,
      consolidation_ratio = 0.4,
      consolidation_percentage = 40.0,
      total_persons = 2,
      consolidation_type = "both",
      employer_variable = NA_character_,
      employer_lag_threshold = NA_real_
    ),
    person_level = data.table::data.table(
      cf = c(1, 2),
      original_contracts = c(6, 4),
      consolidated_periods = c(3, 3),
      contracts_eliminated = c(3, 1),
      person_consolidation_ratio = c(0.5, 0.25),
      avg_consolidation_per_person = c(0.5, 0.25),
      total_duration = c(300, 200),
      consolidated_duration = c(300, 200),
      duration_efficiency = c(1.0, 1.0)
    ),
    distribution = list(
      consolidation_distribution = data.table::data.table(
        contracts_eliminated = c(1, 3),
        persons_count = c(1, 1),
        consolidation_pattern = c(
          "1 contracts eliminated",
          "3 contracts eliminated"
        )
      ),
      distribution_summary = list(
        persons_with_no_consolidation = 0,
        persons_with_consolidation = 2,
        max_consolidation_per_person = 3,
        avg_consolidation_per_person = 2.0,
        median_consolidation_per_person = 2.0
      )
    ),
    employer_specific = NULL,
    temporal_specific = NULL
  )

  summary_result <- summarize_consolidation(test_metrics)

  expect_type(summary_result, "list")
  expect_s3_class(summary_result, "longworkR_consolidation_summary")

  # Check required components
  expected_names <- c(
    "overview",
    "effectiveness",
    "person_summary",
    "distribution_summary",
    "recommendations"
  )
  expect_true(all(expected_names %in% names(summary_result)))

  # Check overview content
  expect_equal(summary_result$overview$consolidation_mode, "temporal")
  expect_equal(summary_result$overview$consolidation_ratio, 0.4)
  expect_equal(summary_result$overview$contracts_eliminated, 4)

  # Check effectiveness assessment
  expect_type(summary_result$effectiveness$effectiveness_level, "character")
  expect_type(summary_result$effectiveness$interpretation, "character")

  # Check recommendations
  expect_true(length(summary_result$recommendations) > 0)
})

test_that("summarize_consolidation handles different effectiveness levels", {
  skip_if_not_installed("data.table")

  # Test high consolidation
  high_consolidation_metrics <- list(
    consolidation_summary = list(
      consolidation_mode = "both",
      original_contracts = 100,
      consolidated_periods = 30,
      contracts_eliminated = 70,
      consolidation_ratio = 0.7,
      consolidation_percentage = 70.0,
      total_persons = 10
    ),
    person_level = data.table::data.table(
      cf = 1:10,
      person_consolidation_ratio = rep(0.7, 10),
      contracts_eliminated = rep(7, 10)
    ),
    distribution = list(
      consolidation_distribution = data.table::data.table(
        contracts_eliminated = 7L,
        persons_count = 10L,
        consolidation_pattern = "7 contracts eliminated"
      ),
      distribution_summary = list()
    )
  )

  summary_high <- summarize_consolidation(high_consolidation_metrics)
  expect_equal(
    summary_high$effectiveness$effectiveness_level,
    "High consolidation"
  )
  expect_true(grepl(
    "Major data reduction",
    summary_high$effectiveness$interpretation
  ))

  # Test low consolidation
  low_consolidation_metrics <- list(
    consolidation_summary = list(
      consolidation_mode = "temporal",
      original_contracts = 100,
      consolidated_periods = 95,
      contracts_eliminated = 5,
      consolidation_ratio = 0.05,
      consolidation_percentage = 5.0,
      total_persons = 10
    ),
    person_level = data.table::data.table(
      cf = 1:10,
      person_consolidation_ratio = rep(0.05, 10),
      contracts_eliminated = rep(0.5, 10)
    ),
    distribution = list(
      consolidation_distribution = data.table::data.table(
        contracts_eliminated = 0L,
        persons_count = 10L,
        consolidation_pattern = "0 contracts eliminated"
      ),
      distribution_summary = list()
    )
  )

  summary_low <- summarize_consolidation(low_consolidation_metrics)
  expect_equal(
    summary_low$effectiveness$effectiveness_level,
    "Low consolidation"
  )
  expect_true(grepl("Minor", summary_low$effectiveness$interpretation))
})

test_that("input validation works correctly", {
  skip_if_not_installed("data.table")

  # Test invalid consolidation_mode
  test_data <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-12-31"),
    durata = 366,
    arco = 1
  )

  expect_error(
    extract_consolidation_metrics(
      original_data = test_data,
      consolidated_data = test_data,
      consolidation_mode = "invalid_mode"
    ),
    "consolidation_mode must be one of"
  )

  # Test non-data.table input
  expect_error(
    extract_consolidation_metrics(
      original_data = "not_a_data_table",
      consolidated_data = test_data,
      consolidation_mode = "none"
    ),
    "original_data must be a data.table object"
  )

  expect_error(
    extract_consolidation_metrics(
      original_data = test_data,
      consolidated_data = "not_a_data_table",
      consolidation_mode = "none"
    ),
    "consolidated_data must be a data.table object"
  )

  # Test invalid summarize_consolidation input
  expect_error(
    summarize_consolidation("not_a_list"),
    "consolidation_metrics must be a list"
  )

  expect_error(
    summarize_consolidation(list()),
    "consolidation_metrics must be a list from extract_consolidation_metrics"
  )
})

test_that("print methods work correctly", {
  skip_if_not_installed("data.table")

  # Create test consolidation summary
  test_summary <- list(
    overview = list(
      consolidation_mode = "temporal",
      original_contracts = 10,
      consolidated_periods = 6,
      consolidation_ratio = 0.4,
      consolidation_percentage = 40.0,
      contracts_eliminated = 4,
      total_persons = 2,
      avg_contracts_per_person_original = 5.0,
      avg_periods_per_person_consolidated = 3.0
    ),
    effectiveness = list(
      effectiveness_level = "Substantial consolidation",
      data_reduction_factor = 1.67,
      interpretation = "Significant simplification while maintaining essential patterns"
    ),
    person_summary = list(
      persons_analyzed = 2,
      avg_consolidation_ratio = 0.375,
      persons_with_consolidation = 2,
      max_contracts_eliminated = 3
    ),
    recommendations = c(
      "Consolidation appears effective",
      "Validate transition patterns"
    )
  )
  class(test_summary) <- c("longworkR_consolidation_summary", "list")

  # Test that print method doesn't error
  expect_output(print(test_summary), "longworkR Consolidation Summary")
  expect_output(print(test_summary), "Substantial consolidation")
  expect_output(print(test_summary), "40.0%")

  # Test transitions with metrics print method
  test_transitions_with_metrics <- list(
    transition_results = data.table::data.table(
      from = c("A", "B"),
      to = c("B", "A"),
      weight = c(5, 3)
    ),
    consolidation_summary = test_summary
  )
  class(test_transitions_with_metrics) <- c(
    "longworkR_transitions_with_metrics",
    "list"
  )

  expect_output(
    print(test_transitions_with_metrics),
    "Employment Transitions with Consolidation Metrics"
  )
  expect_output(print(test_transitions_with_metrics), "Unique transitions: 2")
})
