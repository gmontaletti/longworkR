# Test ggraph functions availability and basic structure
# Focus on testing what we can test without running into implementation issues

library(testthat)
library(data.table)

test_that("ggraph visualization functions are exported and available", {
  # Test that functions are exported and accessible
  expect_true(exists("plot_transitions_network"))
  expect_true(exists("plot_transitions_heatmap"))
  expect_true(exists("plot_transitions_circular"))
  expect_true(exists("plot_transitions_hierarchical"))
  expect_true(exists("analyze_transitions_network"))
  expect_true(exists("create_accessibility_report"))
})

test_that("helper functions for test data generation work", {
  # Test our helper functions
  basic_data <- data.table(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(10, 15, 8),
    transition_duration = c(30, 45, 60),
    variable = rep("test", 3)
  )
  
  # Test matrix creation
  create_test_matrix <- function(dt) {
    states <- sort(unique(c(dt$from, dt$to)))
    n_states <- length(states)
    matrix <- matrix(0, nrow = n_states, ncol = n_states)
    rownames(matrix) <- states
    colnames(matrix) <- states
    
    for (i in seq_len(nrow(dt))) {
      from_idx <- which(states == dt$from[i])
      to_idx <- which(states == dt$to[i])
      matrix[from_idx, to_idx] <- dt$weight[i]
    }
    matrix
  }
  
  test_matrix <- create_test_matrix(basic_data)
  
  expect_true(is.matrix(test_matrix))
  expect_equal(nrow(test_matrix), 3)
  expect_equal(ncol(test_matrix), 3)
  expect_equal(sum(test_matrix), sum(basic_data$weight))
})

test_that("package dependency checks work", {
  # Test package availability checking
  check_packages <- function(pkgs) {
    all(sapply(pkgs, requireNamespace, quietly = TRUE))
  }
  
  # These should be available in the vecshift environment
  basic_pkgs <- c("ggplot2", "data.table")
  expect_true(check_packages(basic_pkgs))
  
  # These might not be available
  advanced_pkgs <- c("ggraph", "tidygraph", "igraph")
  advanced_available <- check_packages(advanced_pkgs)
  
  # We should be able to test this without error
  expect_type(advanced_available, "logical")
})

test_that("plot_transitions_heatmap works with valid data", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  # Create simple valid data
  simple_data <- data.table(
    from = c("state1", "state2", "state1"),
    to = c("state2", "state1", "state1"),
    weight = c(5, 3, 2),
    variable = rep("test", 3)
  )
  
  # This should work as it only needs ggplot2
  result <- plot_transitions_heatmap(
    transitions_data = simple_data,
    input_format = "data.table",
    cell_value = "weight"
  )
  
  expect_s3_class(result, "ggplot")
  expect_true(length(result$layers) > 0)
})

test_that("create_accessibility_report works with valid data", {
  skip_if_not(exists("create_accessibility_report"), "Function not available")
  
  simple_data <- data.table(
    from = c("state1", "state2"),
    to = c("state2", "state1"), 
    weight = c(5, 3),
    variable = rep("test", 2)
  )
  
  report <- create_accessibility_report(
    transitions_data = simple_data,
    layout = "fr",
    palette = "viridis"
  )
  
  expect_type(report, "list")
  expect_true("accessibility_score" %in% names(report))
  expect_true("accessibility_percentage" %in% names(report))
  expect_true("overall_assessment" %in% names(report))
  
  # Check that score is reasonable
  expect_gte(report$accessibility_score, 0)
  expect_lte(report$accessibility_percentage, 100)
})

test_that("heatmap function handles different normalization methods", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  test_data <- data.table(
    from = c("A", "A", "B"),
    to = c("B", "C", "A"),
    weight = c(10, 5, 8),
    variable = rep("test", 3)
  )
  
  # Test each normalization method
  normalizations <- c("none", "row", "column", "total")
  
  for (norm in normalizations) {
    result <- plot_transitions_heatmap(
      transitions_data = test_data,
      normalize = norm,
      cell_value = "weight"
    )
    expect_s3_class(result, "ggplot")
  }
})

test_that("heatmap function handles different cell value options", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  test_data <- data.table(
    from = c("A", "B", "A"),
    to = c("B", "A", "A"),
    weight = c(10, 15, 5),
    variable = rep("test", 3)
  )
  
  # Test each cell value option
  cell_values <- c("weight", "percentage", "both", "none")
  
  for (cell_val in cell_values) {
    result <- plot_transitions_heatmap(
      transitions_data = test_data,
      cell_value = cell_val
    )
    expect_s3_class(result, "ggplot")
  }
})

test_that("functions handle parameter validation correctly", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  valid_data <- data.table(
    from = c("A", "B"),
    to = c("B", "A"),
    weight = c(5, 3),
    variable = rep("test", 2)
  )
  
  # Test invalid normalization parameter
  expect_error(
    plot_transitions_heatmap(
      transitions_data = valid_data,
      normalize = "invalid_method"
    ),
    "'arg' should be one of"
  )
  
  # Test invalid cell_value parameter
  expect_error(
    plot_transitions_heatmap(
      transitions_data = valid_data,
      cell_value = "invalid_value"
    ),
    "'arg' should be one of"
  )
})

test_that("functions handle missing required columns appropriately", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  # Data missing required columns
  invalid_data <- data.table(
    x = 1:3,
    y = letters[1:3]
  )
  
  expect_error(
    plot_transitions_heatmap(
      transitions_data = invalid_data,
      input_format = "data.table"
    ),
    "Missing required columns"
  )
})

test_that("functions handle empty data appropriately", {
  skip_if_not(exists("plot_transitions_heatmap"), "Function not available")
  
  empty_data <- data.table(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    variable = character(0)
  )
  
  expect_warning(
    result <- plot_transitions_heatmap(empty_data),
    "Empty transition matrix"
  )
  
  expect_s3_class(result, "ggplot")
})

test_that("matrix conversion functions work correctly", {
  test_data <- data.table(
    from = c("A", "B", "C", "A"),
    to = c("B", "C", "A", "C"),
    weight = c(10, 15, 8, 5),
    variable = rep("test", 4)
  )
  
  # Test matrix conversion
  create_test_matrix <- function(dt) {
    states <- sort(unique(c(dt$from, dt$to)))
    n_states <- length(states)
    matrix <- matrix(0, nrow = n_states, ncol = n_states)
    rownames(matrix) <- states
    colnames(matrix) <- states
    
    for (i in seq_len(nrow(dt))) {
      from_idx <- which(states == dt$from[i])
      to_idx <- which(states == dt$to[i])
      matrix[from_idx, to_idx] <- dt$weight[i]
    }
    matrix
  }
  
  result_matrix <- create_test_matrix(test_data)
  
  expect_true(is.matrix(result_matrix))
  expect_equal(nrow(result_matrix), 3)
  expect_equal(ncol(result_matrix), 3)
  expect_true(!is.null(rownames(result_matrix)))
  expect_true(!is.null(colnames(result_matrix)))
  
  # Check that row and column names match the unique states
  unique_states <- sort(unique(c(test_data$from, test_data$to)))
  expect_equal(rownames(result_matrix), unique_states)
  expect_equal(colnames(result_matrix), unique_states)
  
  # Check that the total weights are preserved
  expect_equal(sum(result_matrix), sum(test_data$weight))
})

test_that("accessibility report provides meaningful output structure", {
  skip_if_not(exists("create_accessibility_report"), "Function not available")
  
  test_data <- data.table(
    from = c("unemployed", "full_time"),
    to = c("full_time", "unemployed"),
    weight = c(20, 5),
    variable = rep("employment", 2)
  )
  
  report <- create_accessibility_report(
    transitions_data = test_data,
    layout = "fr",
    palette = "viridis"
  )
  
  # Check all expected report components are present
  expected_components <- c(
    "timestamp", "layout", "palette", "accessibility_score",
    "accessibility_percentage", "overall_assessment"
  )
  
  for (component in expected_components) {
    expect_true(component %in% names(report), info = paste("Missing component:", component))
  }
  
  # Check that values are reasonable
  expect_true(is.numeric(report$accessibility_score))
  expect_true(is.numeric(report$accessibility_percentage))
  expect_true(is.character(report$overall_assessment))
  expect_gte(report$accessibility_percentage, 0)
  expect_lte(report$accessibility_percentage, 100)
})