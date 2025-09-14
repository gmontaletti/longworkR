# Advanced Performance Testing Suite for longworkR Consolidation Functions
# Tests against the documented 360K-420K records/second optimization targets

test_that("Advanced performance: Real-world dataset performance", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  # Try to use actual sample data if available
  sample_file <- file.path("../../data/sample.rds")
  if (file.exists(sample_file)) {
    full_sample <- readRDS(sample_file)
    # Take a substantial subset for performance testing
    test_persons <- sample(unique(full_sample$cf), min(1000, length(unique(full_sample$cf))))
    real_data <- full_sample[cf %in% test_persons]
    
    # Ensure minimum size for meaningful performance test
    if (nrow(real_data) < 10000) {
      # Generate additional realistic data
      extra_data <- generate_realistic_employment_data(
        n_records = 10000 - nrow(real_data),
        seed = 789
      )
      real_data <- rbind(real_data, extra_data, fill = TRUE)
    }
  } else {
    # Generate realistic test data
    real_data <- generate_realistic_employment_data(
      n_records = 50000,
      n_persons = 5000,
      temporal_consolidation_rate = 0.3,
      seed = 789
    )
  }
  
  dataset_size <- nrow(real_data[arco > 0])
  message(sprintf("Testing with %d employment records", dataset_size))
  
  # Benchmark all consolidation modes
  performance_results <- microbenchmark::microbenchmark(
    none = analyze_employment_transitions(real_data, consolidation = "none"),
    temporal = analyze_employment_transitions(real_data, consolidation = "temporal"),
    employer = analyze_employment_transitions(
      real_data, consolidation = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
    ),
    both = analyze_employment_transitions(
      real_data, consolidation = "both", employer_var = "CODICE_FISCALE_AZIENDA"
    ),
    times = 3,
    unit = "ms"
  )
  
  # Validate performance against targets
  targets <- get_performance_targets()
  validation <- validate_performance(performance_results, dataset_size, targets)
  
  # Performance assertions
  expect_true(validation$meets_minimum_rate, 
              info = sprintf("Processing rates: %s", 
                           paste(format_processing_rate(validation$processing_rates), collapse = ", ")))
  
  expect_true(validation$acceptable_variance,
              info = sprintf("Rate variance: %.2f (limit: %.2f)", 
                           validation$rate_variance, targets$max_rate_variance))
  
  # Store results for reporting
  real_world_benchmark <<- performance_results
  real_world_validation <<- validation
})

test_that("Advanced performance: Memory efficiency with large datasets", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("bench")
  skip_on_cran()
  
  if (!requireNamespace("bench", quietly = TRUE)) {
    skip("bench package required for memory testing")
  }
  
  # Generate memory test dataset
  memory_test_data <- generate_performance_test_data("medium", "complex", seed = 999)
  dataset_size <- nrow(memory_test_data[arco > 0])
  
  # Measure memory usage
  memory_benchmark <- bench::mark(
    temporal_consolidation = analyze_employment_transitions(
      memory_test_data, consolidation = "temporal"
    ),
    employer_consolidation = analyze_employment_transitions(
      memory_test_data, consolidation = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
    ),
    check = FALSE,  # Results won't be identical
    min_iterations = 2,
    max_iterations = 5
  )
  
  # Memory efficiency validation
  data_size_bytes <- object.size(memory_test_data)
  max_memory_used <- max(memory_benchmark$mem_alloc)
  memory_multiplier <- as.numeric(max_memory_used / data_size_bytes)
  
  targets <- get_performance_targets()
  expect_true(memory_multiplier <= targets$max_memory_multiplier,
              info = sprintf("Memory multiplier: %.2fx (limit: %.2fx)", 
                           memory_multiplier, targets$max_memory_multiplier))
  
  # Processing rate validation
  median_time_s <- min(memory_benchmark$median)
  processing_rate <- dataset_size / as.numeric(median_time_s)
  
  expect_true(processing_rate >= targets$minimum_rate,
              info = sprintf("Processing rate: %s (target: %s)",
                           format_processing_rate(processing_rate),
                           format_processing_rate(targets$minimum_rate)))
  
  memory_efficiency_results <<- list(
    benchmark = memory_benchmark,
    memory_multiplier = memory_multiplier,
    processing_rate = processing_rate
  )
})

test_that("Advanced performance: Scalability analysis across dataset sizes", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  # Test scalability across different sizes
  sizes <- c(1000, 5000, 10000, 25000, 50000)
  scalability_results <- list()
  
  for (size in sizes) {
    test_data <- generate_performance_test_data("small", "simple", seed = size)
    # Adjust actual size to target
    if (nrow(test_data) != size) {
      subset_persons <- sample(unique(test_data$cf), 
                              ceiling(size / (nrow(test_data) / length(unique(test_data$cf)))))
      test_data <- test_data[cf %in% subset_persons][1:min(size, .N)]
    }
    
    actual_size <- nrow(test_data[arco > 0])
    if (actual_size < size * 0.8) next  # Skip if too small
    
    # Benchmark temporal consolidation (fastest mode)
    benchmark <- microbenchmark::microbenchmark(
      temporal = analyze_employment_transitions(test_data, consolidation = "temporal"),
      times = 3,
      unit = "ms"
    )
    
    scalability_results[[as.character(actual_size)]] <- list(
      size = actual_size,
      median_time_ms = summary(benchmark)$median,
      processing_rate = actual_size / (summary(benchmark)$median / 1000)
    )
  }
  
  # Analyze scalability
  sizes_tested <- sapply(scalability_results, function(x) x$size)
  processing_rates <- sapply(scalability_results, function(x) x$processing_rate)
  
  # Check that processing rate doesn't degrade significantly with size
  rate_cv <- sd(processing_rates) / mean(processing_rates)
  targets <- get_performance_targets()
  
  expect_true(rate_cv <= targets$max_rate_variance,
              info = sprintf("Scalability variance: %.2f (limit: %.2f)", 
                           rate_cv, targets$max_rate_variance))
  
  # All sizes should meet minimum performance
  min_rate <- min(processing_rates)
  expect_true(min_rate >= targets$minimum_rate * 0.8,  # Allow 20% degradation
              info = sprintf("Minimum rate across sizes: %s", format_processing_rate(min_rate)))
  
  scalability_analysis <<- data.frame(
    size = sizes_tested,
    processing_rate = processing_rates,
    rate_formatted = sapply(processing_rates, format_processing_rate),
    stringsAsFactors = FALSE
  )
})

test_that("Advanced performance: Stress testing with pathological datasets", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  # Create pathological dataset (many consolidation opportunities)
  pathological_data <- generate_performance_test_data("small", "pathological", seed = 666)
  stress_size <- nrow(pathological_data[arco > 0])
  
  # Should still complete in reasonable time even with complex data
  stress_benchmark <- microbenchmark::microbenchmark(
    temporal_stress = analyze_employment_transitions(
      pathological_data, consolidation = "temporal"
    ),
    employer_stress = analyze_employment_transitions(
      pathological_data, consolidation = "employer", employer_var = "CODICE_FISCALE_AZIENDA"
    ),
    both_stress = analyze_employment_transitions(
      pathological_data, consolidation = "both", employer_var = "CODICE_FISCALE_AZIENDA"
    ),
    times = 3,
    unit = "ms"
  )
  
  # Even pathological cases should meet minimum performance
  processing_rates <- stress_size / (summary(stress_benchmark)$median / 1000)
  min_pathological_rate <- min(processing_rates)
  
  targets <- get_performance_targets()
  # Allow more degradation for pathological cases (50% of minimum)
  expect_true(min_pathological_rate >= targets$minimum_rate * 0.5,
              info = sprintf("Pathological case rate: %s", 
                           format_processing_rate(min_pathological_rate)))
  
  stress_test_results <<- list(
    benchmark = stress_benchmark,
    processing_rates = processing_rates,
    min_rate = min_pathological_rate
  )
})

test_that("Advanced performance: Parallel processing efficiency", {
  skip_if_not_installed("data.table")
  skip_on_cran()
  
  # Test with dataset large enough to potentially trigger parallel processing
  parallel_test_data <- generate_realistic_employment_data(
    n_records = 100000,
    n_persons = 10000,
    seed = 777
  )
  
  # Test different thread scenarios (if applicable)
  original_threads <- data.table::getDTthreads()
  
  results_single <- NULL
  results_multi <- NULL
  
  tryCatch({
    # Single thread
    data.table::setDTthreads(1)
    start_time <- Sys.time()
    result_single <- analyze_employment_transitions(parallel_test_data, consolidation = "temporal")
    single_thread_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Multiple threads (if available)
    data.table::setDTthreads(0)  # Use all available
    start_time <- Sys.time()
    result_multi <- analyze_employment_transitions(parallel_test_data, consolidation = "temporal")
    multi_thread_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Results should be identical regardless of threading
    expect_equal(nrow(result_single$transition_data), nrow(result_multi$transition_data))
    
    # Multi-threaded should be at least as fast (or not significantly slower)
    speedup <- single_thread_time / multi_thread_time
    expect_true(speedup >= 0.8,  # Allow for threading overhead
                info = sprintf("Threading speedup: %.2fx", speedup))
    
    parallel_efficiency <<- list(
      single_thread_time = single_thread_time,
      multi_thread_time = multi_thread_time,
      speedup = speedup,
      available_threads = data.table::getDTthreads()
    )
    
  }, finally = {
    # Restore original thread setting
    data.table::setDTthreads(original_threads)
  })
})

test_that("Advanced performance: Edge case performance", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("microbenchmark")
  skip_on_cran()
  
  # Test performance with various edge cases
  
  # 1. Many persons, few contracts each
  sparse_data <- generate_realistic_employment_data(
    n_records = 10000,
    n_persons = 8000,  # Average ~1.25 contracts per person
    seed = 888
  )
  
  # 2. Few persons, many contracts each
  dense_data <- generate_realistic_employment_data(
    n_records = 10000,
    n_persons = 100,   # Average ~100 contracts per person
    seed = 999
  )
  
  # 3. Many employers (high diversity)
  diverse_data <- generate_realistic_employment_data(
    n_records = 10000,
    n_employers = 5000,  # Average ~2 contracts per employer
    seed = 111
  )
  
  edge_case_results <- list()
  
  test_cases <- list(
    sparse = sparse_data,
    dense = dense_data,
    diverse = diverse_data
  )
  
  for (case_name in names(test_cases)) {
    case_data <- test_cases[[case_name]]
    case_size <- nrow(case_data[arco > 0])
    
    benchmark <- microbenchmark::microbenchmark(
      temporal = analyze_employment_transitions(case_data, consolidation = "temporal"),
      times = 3,
      unit = "ms"
    )
    
    processing_rate <- case_size / (summary(benchmark)$median / 1000)
    
    edge_case_results[[case_name]] <- list(
      size = case_size,
      processing_rate = processing_rate,
      median_time_ms = summary(benchmark)$median
    )
  }
  
  # All edge cases should meet reasonable performance
  targets <- get_performance_targets()
  min_edge_rate <- min(sapply(edge_case_results, function(x) x$processing_rate))
  
  expect_true(min_edge_rate >= targets$minimum_rate * 0.7,  # Allow some degradation
              info = sprintf("Minimum edge case rate: %s", format_processing_rate(min_edge_rate)))
  
  edge_case_performance <<- edge_case_results
})

# Performance summary and reporting
test_that("Advanced performance: Generate performance summary report", {
  skip_on_cran()
  
  # Collect all performance results
  all_results <- list()
  
  if (exists("real_world_benchmark")) {
    all_results$real_world <- list(
      benchmark = real_world_benchmark,
      validation = real_world_validation
    )
  }
  
  if (exists("memory_efficiency_results")) {
    all_results$memory <- memory_efficiency_results
  }
  
  if (exists("scalability_analysis")) {
    all_results$scalability <- scalability_analysis
  }
  
  if (exists("stress_test_results")) {
    all_results$stress <- stress_test_results
  }
  
  if (exists("parallel_efficiency")) {
    all_results$parallel <- parallel_efficiency
  }
  
  if (exists("edge_case_performance")) {
    all_results$edge_cases <- edge_case_performance
  }
  
  # Generate summary
  targets <- get_performance_targets()
  
  summary_report <- list(
    timestamp = Sys.time(),
    r_version = R.version.string,
    system_info = Sys.info(),
    targets = targets,
    results = all_results
  )
  
  # Basic validation that we have some results
  expect_true(length(all_results) > 0, 
              info = "At least some performance tests should have run")
  
  # Store final results
  performance_summary <<- summary_report
  
  # Print summary for manual review
  cat("\n=== longworkR Consolidation Performance Summary ===\n")
  cat(sprintf("Generated: %s\n", summary_report$timestamp))
  cat(sprintf("R Version: %s\n", summary_report$r_version))
  cat(sprintf("System: %s\n", summary_report$system_info[["sysname"]]))
  
  if (!is.null(all_results$real_world)) {
    cat(sprintf("\nReal-world Performance:\n"))
    rates <- all_results$real_world$validation$processing_rates
    cat(sprintf("  Processing rates: %s\n", 
                paste(format_processing_rate(rates), collapse = ", ")))
    cat(sprintf("  Meets targets: %s\n", all_results$real_world$validation$meets_minimum_rate))
  }
  
  if (!is.null(all_results$scalability)) {
    cat("\nScalability Analysis:\n")
    rates <- all_results$scalability$processing_rate
    cat(sprintf("  Rate range: %s - %s\n", 
                format_processing_rate(min(rates)), 
                format_processing_rate(max(rates))))
    cat(sprintf("  Rate variance: %.1f%%\n", (sd(rates) / mean(rates)) * 100))
  }
  
  if (!is.null(all_results$memory)) {
    cat(sprintf("\nMemory Efficiency:\n"))
    cat(sprintf("  Memory multiplier: %.2fx\n", all_results$memory$memory_multiplier))
    cat(sprintf("  Processing rate: %s\n", format_processing_rate(all_results$memory$processing_rate)))
  }
  
  cat("\n=== End Performance Summary ===\n")
})