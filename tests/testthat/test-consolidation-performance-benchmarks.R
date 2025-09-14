# Performance Benchmark Suite for longworkR Consolidation Functions
# This file contains comprehensive performance tests for the optimized consolidation functions

test_that("Setup benchmark infrastructure", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()  # Skip performance tests on CRAN
  
  # Verify benchmark packages
  expect_true(requireNamespace("microbenchmark", quietly = TRUE))
  expect_true(requireNamespace("data.table", quietly = TRUE))
})

# Helper function to generate synthetic employment data of specific sizes
generate_test_data <- function(n_records, n_persons = NULL, seed = 42) {
  set.seed(seed)
  
  if (is.null(n_persons)) {
    n_persons <- max(10, n_records %/% 10)  # Average 10 contracts per person
  }
  
  # Generate person IDs with realistic distribution
  cf_weights <- rexp(n_persons, rate = 0.1)  # Some people have many more contracts
  cf_probs <- cf_weights / sum(cf_weights)
  
  data.table::data.table(
    cf = sample(1:n_persons, n_records, replace = TRUE, prob = cf_probs),
    inizio = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 
                   n_records, replace = TRUE),
    fine = sample(seq(as.Date("2010-01-01"), as.Date("2024-12-31"), by = "day"), 
                 n_records, replace = TRUE),
    durata = sample(1:365, n_records, replace = TRUE),
    arco = sample(c(0, 1, 2), n_records, replace = TRUE, prob = c(0.1, 0.7, 0.2)),
    over_id = sample(1:(n_records * 2), n_records, replace = TRUE),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      c("A.01.00", "A.03.00", "B.01.00", "C.01.00", "D.01.00"), 
      n_records, replace = TRUE
    ),
    CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:100), n_records, replace = TRUE),
    prior = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.3, 0.7))
  )[order(cf, inizio)]  # Sort for realistic data structure
}

test_that("Benchmark data generation functions work correctly", {
  skip_if_not_installed("data.table")
  
  # Test small dataset
  small_data <- generate_test_data(1000)
  expect_equal(nrow(small_data), 1000)
  expect_true(all(c("cf", "inizio", "fine", "durata", "arco", "over_id") %in% names(small_data)))
  expect_true(all(small_data$arco %in% c(0, 1, 2)))
  
  # Test medium dataset
  medium_data <- generate_test_data(50000)
  expect_equal(nrow(medium_data), 50000)
  expect_true(length(unique(medium_data$cf)) >= 10)  # Should have multiple persons
})

# Performance benchmark tests for different dataset sizes
test_that("Performance benchmarks - Small dataset (1K records)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  small_data <- generate_test_data(1000)
  
  # Benchmark different consolidation modes
  benchmark_results <- microbenchmark::microbenchmark(
    no_consolidation = analyze_employment_transitions(
      small_data, consolidation = "none", eval_chain = "none"
    ),
    temporal_consolidation = analyze_employment_transitions(
      small_data, consolidation = "temporal", eval_chain = "none"
    ),
    employer_consolidation = analyze_employment_transitions(
      small_data, consolidation = "employer", 
      employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
    ),
    both_consolidation = analyze_employment_transitions(
      small_data, consolidation = "both",
      employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
    ),
    times = 10,
    unit = "ms"
  )
  
  expect_s3_class(benchmark_results, "microbenchmark")
  
  # Performance expectations for small datasets (should be very fast)
  median_times <- summary(benchmark_results)$median
  expect_true(all(median_times < 1000))  # All should complete under 1 second
  
  # Store results for later analysis
  small_benchmark <<- benchmark_results
})

test_that("Performance benchmarks - Medium dataset (50K records)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  medium_data <- generate_test_data(50000)
  
  benchmark_results <- microbenchmark::microbenchmark(
    no_consolidation = analyze_employment_transitions(
      medium_data, consolidation = "none", eval_chain = "none"
    ),
    temporal_consolidation = analyze_employment_transitions(
      medium_data, consolidation = "temporal", eval_chain = "none"
    ),
    employer_consolidation = analyze_employment_transitions(
      medium_data, consolidation = "employer", 
      employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
    ),
    both_consolidation = analyze_employment_transitions(
      medium_data, consolidation = "both",
      employer_var = "CODICE_FISCALE_AZIENDA", eval_chain = "none"
    ),
    times = 5,  # Fewer iterations for larger data
    unit = "ms"
  )
  
  expect_s3_class(benchmark_results, "microbenchmark")
  
  # Performance expectations - should process at target rate
  median_times <- summary(benchmark_results)$median
  processing_rates <- 50000 / (median_times / 1000)  # records per second
  
  # Target: 300K+ records/second based on optimization notes
  expect_true(any(processing_rates > 100000))  # At least one mode should be fast
  
  medium_benchmark <<- benchmark_results
})

test_that("Performance benchmarks - Large dataset (500K records)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_ci()    # Skip on CI due to memory/time constraints
  skip_on_cran()
  
  large_data <- generate_test_data(500000)
  
  # Test with fewer repetitions due to size
  benchmark_results <- microbenchmark::microbenchmark(
    no_consolidation = analyze_employment_transitions(
      large_data, consolidation = "none", eval_chain = "none"
    ),
    temporal_consolidation = analyze_employment_transitions(
      large_data, consolidation = "temporal", eval_chain = "none"
    ),
    times = 3,  # Even fewer iterations for large data
    unit = "s"
  )
  
  expect_s3_class(benchmark_results, "microbenchmark")
  
  # Performance expectations for large datasets
  median_times <- summary(benchmark_results)$median
  processing_rates <- 500000 / median_times  # records per second
  
  # Should maintain high processing rate even for large datasets
  expect_true(any(processing_rates > 200000))  # Target rate maintained
  
  large_benchmark <<- benchmark_results
})

# Memory usage benchmarks
test_that("Memory usage benchmarks", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("bench")
  skip_on_cran()
  
  # Test memory efficiency with bench package if available
  if (requireNamespace("bench", quietly = TRUE)) {
    medium_data <- generate_test_data(50000)
    
    memory_results <- bench::mark(
      no_consolidation = analyze_employment_transitions(
        medium_data, consolidation = "none", eval_chain = "none"
      ),
      temporal_consolidation = analyze_employment_transitions(
        medium_data, consolidation = "temporal", eval_chain = "none"
      ),
      check = FALSE,  # Don't check results are identical (they're not supposed to be)
      min_iterations = 3,
      max_iterations = 10
    )
    
    expect_s3_class(memory_results, "bench_mark")
    
    # Memory usage should be reasonable (not more than 10x data size)
    data_size_mb <- object.size(medium_data) / (1024^2)
    max_memory_mb <- max(as.numeric(memory_results$mem_alloc)) / (1024^2)
    
    expect_true(max_memory_mb < (data_size_mb * 10))  # Memory efficiency check
    
    memory_benchmark <<- memory_results
  } else {
    skip("bench package not available for memory testing")
  }
})

# Scalability tests
test_that("Scalability analysis", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  # Test how performance scales with data size
  sizes <- c(1000, 5000, 10000, 25000)
  times_by_size <- list()
  
  for (size in sizes) {
    test_data <- generate_test_data(size)
    
    benchmark_result <- microbenchmark::microbenchmark(
      temporal_consolidation = analyze_employment_transitions(
        test_data, consolidation = "temporal", eval_chain = "none"
      ),
      times = 5,
      unit = "ms"
    )
    
    times_by_size[[as.character(size)]] <- summary(benchmark_result)$median
  }
  
  # Check that scaling is reasonable (should be roughly linear)
  times_vector <- unlist(times_by_size)
  sizes_vector <- as.numeric(names(times_by_size))
  
  # Calculate processing rates
  processing_rates <- sizes_vector / (times_vector / 1000)  # records/second
  
  # Processing rate should be relatively stable across sizes
  rate_cv <- sd(processing_rates) / mean(processing_rates)  # Coefficient of variation
  expect_true(rate_cv < 0.5)  # Should not vary too much
  
  scalability_results <<- data.frame(
    size = sizes_vector,
    time_ms = times_vector,
    processing_rate = processing_rates
  )
})

# Correctness validation under performance pressure
test_that("Correctness validation with performance data", {
  skip_if_not_installed("data.table")
  
  # Generate larger test dataset
  test_data <- generate_test_data(10000, seed = 123)
  
  # Run different consolidation modes
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
  
  # Validate structure
  expect_true(all(c("transition_data", "network_data", "metrics") %in% names(result_none)))
  expect_true(all(c("transition_data", "network_data", "metrics") %in% names(result_temporal)))
  expect_true(all(c("transition_data", "network_data", "metrics") %in% names(result_employer)))
  
  # Validate consolidation effects
  original_contracts <- nrow(test_data[arco > 0])
  consolidated_contracts_temporal <- nrow(result_temporal$transition_data)
  consolidated_contracts_employer <- nrow(result_employer$transition_data)
  
  # Temporal consolidation should reduce contracts
  expect_true(consolidated_contracts_temporal <= original_contracts)
  
  # Employer consolidation should also potentially reduce contracts
  expect_true(consolidated_contracts_employer <= original_contracts)
  
  # Validate data quality
  expect_false(any(is.na(result_temporal$transition_data$cf)))
  expect_false(any(is.na(result_employer$transition_data$cf)))
  
  # Validate that person IDs are preserved
  original_persons <- unique(test_data[arco > 0, cf])
  temporal_persons <- unique(result_temporal$transition_data$cf)
  employer_persons <- unique(result_employer$transition_data$cf)
  
  expect_setequal(original_persons, temporal_persons)
  expect_setequal(original_persons, employer_persons)
})