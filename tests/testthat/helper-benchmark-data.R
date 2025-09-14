# Helper functions for benchmark data generation and performance testing

# Generate realistic employment data with configurable characteristics
generate_realistic_employment_data <- function(
  n_records, 
  n_persons = NULL, 
  n_employers = NULL,
  temporal_consolidation_rate = 0.3,
  employer_consolidation_rate = 0.2,
  contract_types = c("A.01.00", "A.03.00", "B.01.00", "C.01.00", "D.01.00"),
  date_range = c(as.Date("2010-01-01"), as.Date("2023-12-31")),
  seed = 42
) {
  
  set.seed(seed)
  
  if (is.null(n_persons)) {
    n_persons <- max(10, n_records %/% 8)  # Average 8 contracts per person
  }
  
  if (is.null(n_employers)) {
    n_employers <- max(5, n_records %/% 50)  # Average 50 contracts per employer
  }
  
  # Generate person IDs with realistic distribution (power law)
  cf_weights <- rexp(n_persons, rate = 0.1)
  cf_probs <- cf_weights / sum(cf_weights)
  person_ids <- sample(1:n_persons, n_records, replace = TRUE, prob = cf_probs)
  
  # Generate employer distribution
  employer_weights <- rexp(n_employers, rate = 0.05)
  employer_probs <- employer_weights / sum(employer_weights)
  employer_ids <- sample(paste0("COMP", sprintf("%04d", 1:n_employers)), 
                        n_records, replace = TRUE, prob = employer_probs)
  
  # Generate dates with realistic patterns
  start_dates <- sample(seq(date_range[1], date_range[2], by = "day"), 
                       n_records, replace = TRUE)
  
  # Generate contract durations with realistic distribution
  durations <- pmax(1, round(rexp(n_records, rate = 1/180)))  # Average ~180 days, min 1
  end_dates <- start_dates + durations
  
  # Create base data
  data <- data.table::data.table(
    cf = person_ids,
    inizio = start_dates,
    fine = end_dates,
    durata = as.numeric(durations),
    arco = sample(c(0, 1, 2), n_records, replace = TRUE, prob = c(0.05, 0.75, 0.2)),
    CODICE_FISCALE_AZIENDA = employer_ids,
    COD_TIPOLOGIA_CONTRATTUALE = sample(contract_types, n_records, replace = TRUE),
    prior = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.3, 0.7))
  )
  
  # Sort by person and date for realistic ordering
  data <- data[order(cf, inizio)]
  
  # Generate over_id with consolidation opportunities
  data[, over_id := seq_len(.N), by = cf]
  
  # Introduce temporal consolidation opportunities
  if (temporal_consolidation_rate > 0) {
    consolidation_candidates <- data[, .I[runif(.N) < temporal_consolidation_rate], by = cf]$V1
    if (length(consolidation_candidates) > 0) {
      # Create pairs of contracts with same over_id
      pairs <- matrix(sample(consolidation_candidates, 
                           2 * (length(consolidation_candidates) %/% 2)), 
                     ncol = 2)
      for (i in seq_len(nrow(pairs))) {
        same_over_id <- max(data$over_id) + i
        data[pairs[i, ], over_id := same_over_id]
      }
    }
  }
  
  # Add some realistic columns for contract analysis
  data[, retribuzioni_lorde_somma := round(runif(.N, 1000, 5000) * durata / 30, 2)]
  data[, ore_lavorate_somma := round(runif(.N, 20, 40) * durata / 7, 1)]
  
  return(data)
}

# Generate data specifically designed to test performance edge cases
generate_performance_test_data <- function(
  size_category = c("small", "medium", "large", "xlarge"),
  complexity = c("simple", "complex", "pathological"),
  seed = 42
) {
  
  size_category <- match.arg(size_category)
  complexity <- match.arg(complexity)
  
  # Define size parameters
  size_params <- switch(size_category,
    "small" = list(n_records = 1000, n_persons = 100, n_employers = 20),
    "medium" = list(n_records = 50000, n_persons = 5000, n_employers = 500),
    "large" = list(n_records = 500000, n_persons = 50000, n_employers = 2000),
    "xlarge" = list(n_records = 1000000, n_persons = 100000, n_employers = 5000)
  )
  
  # Define complexity parameters
  complexity_params <- switch(complexity,
    "simple" = list(
      temporal_rate = 0.1,
      employer_rate = 0.1,
      contract_variety = 3
    ),
    "complex" = list(
      temporal_rate = 0.3,
      employer_rate = 0.2,
      contract_variety = 8
    ),
    "pathological" = list(
      temporal_rate = 0.7,  # Many consolidation opportunities
      employer_rate = 0.5,
      contract_variety = 15
    )
  )
  
  contract_types <- c("A.01.00", "A.03.00", "B.01.00", "C.01.00", "D.01.00", 
                     "E.01.00", "F.01.00", "G.01.00", "H.01.00", "I.01.00",
                     "J.01.00", "K.01.00", "L.01.00", "M.01.00", "N.01.00")[1:complexity_params$contract_variety]
  
  data <- generate_realistic_employment_data(
    n_records = size_params$n_records,
    n_persons = size_params$n_persons,
    n_employers = size_params$n_employers,
    temporal_consolidation_rate = complexity_params$temporal_rate,
    employer_consolidation_rate = complexity_params$employer_rate,
    contract_types = contract_types,
    seed = seed
  )
  
  # Add complexity-specific features
  if (complexity == "pathological") {
    # Add many overlapping contracts for the same person
    overlap_data <- data[sample(.N, .N * 0.1)][, inizio := inizio - sample(1:30, .N, replace = TRUE)]
    data <- rbind(data, overlap_data, fill = TRUE)
    data <- data[order(cf, inizio)]
    
    # Recalculate over_id to create more consolidation opportunities
    data[, over_id := cumsum(c(TRUE, diff(cf) != 0 | 
                              c(FALSE, diff(as.numeric(inizio))[1:(.N-1)] > 10)))]
  }
  
  attr(data, "size_category") <- size_category
  attr(data, "complexity") <- complexity
  attr(data, "generation_params") <- list(
    size = size_params,
    complexity = complexity_params
  )
  
  return(data)
}

# Create test datasets for different scenarios
create_benchmark_dataset_suite <- function(seed = 42) {
  
  datasets <- list()
  
  # Standard size progression
  datasets$tiny <- generate_performance_test_data("small", "simple", seed)
  datasets$small <- generate_performance_test_data("small", "complex", seed)
  datasets$medium <- generate_performance_test_data("medium", "simple", seed)
  datasets$medium_complex <- generate_performance_test_data("medium", "complex", seed)
  datasets$large <- generate_performance_test_data("large", "simple", seed)
  
  # Special test cases
  datasets$pathological_small <- generate_performance_test_data("small", "pathological", seed)
  datasets$pathological_medium <- generate_performance_test_data("medium", "pathological", seed)
  
  # Memory test dataset (if system allows)
  if (Sys.info()["sysname"] != "Darwin" || as.numeric(Sys.info()["release"]) < 20) {
    # Skip xlarge on older or non-Mac systems
    message("Skipping xlarge dataset - system limitations")
  } else {
    datasets$xlarge <- generate_performance_test_data("xlarge", "simple", seed)
  }
  
  return(datasets)
}

# Utility functions for benchmarking
calculate_processing_rate <- function(n_records, time_seconds) {
  n_records / time_seconds
}

format_processing_rate <- function(rate) {
  sapply(rate, function(r) {
    if (is.na(r)) {
      "NA records/sec"
    } else if (r > 1e6) {
      sprintf("%.2fM records/sec", r / 1e6)
    } else if (r > 1e3) {
      sprintf("%.1fK records/sec", r / 1e3)
    } else {
      sprintf("%.0f records/sec", r)
    }
  })
}

format_memory_usage <- function(bytes) {
  if (bytes > 1024^3) {
    sprintf("%.2f GB", bytes / (1024^3))
  } else if (bytes > 1024^2) {
    sprintf("%.1f MB", bytes / (1024^2))
  } else if (bytes > 1024) {
    sprintf("%.1f KB", bytes / 1024)
  } else {
    sprintf("%.0f B", bytes)
  }
}

# Performance expectations based on optimization claims
get_performance_targets <- function() {
  list(
    # Target processing rates from optimization documentation
    minimum_rate = 300000,  # 300K records/second minimum
    target_rate = 360000,   # 360K records/second target
    optimal_rate = 420000,  # 420K records/second optimal for medium datasets
    
    # Memory usage targets (reasonable bounds)
    max_memory_multiplier = 5,  # Should not use more than 5x input data size
    
    # Scalability expectations
    max_rate_variance = 0.3,  # Processing rate should not vary more than 30%
    
    # Time thresholds for different dataset sizes
    small_max_time_ms = 100,    # Small datasets: under 100ms
    medium_max_time_ms = 5000,  # Medium datasets: under 5 seconds
    large_max_time_s = 10       # Large datasets: under 10 seconds
  )
}

# Validate performance against targets
validate_performance <- function(benchmark_results, dataset_size, targets = get_performance_targets()) {
  
  results <- summary(benchmark_results)
  median_time_ms <- results$median
  processing_rates <- dataset_size / (median_time_ms / 1000)
  
  validation <- list(
    meets_minimum_rate = any(processing_rates >= targets$minimum_rate),
    meets_target_rate = any(processing_rates >= targets$target_rate),
    processing_rates = processing_rates,
    median_time_ms = median_time_ms,
    rate_variance = sd(processing_rates) / mean(processing_rates),
    acceptable_variance = sd(processing_rates) / mean(processing_rates) <= targets$max_rate_variance
  )
  
  # Size-specific validation
  if (dataset_size <= 10000) {
    validation$meets_time_threshold <- all(median_time_ms <= targets$small_max_time_ms)
  } else if (dataset_size <= 100000) {
    validation$meets_time_threshold <- all(median_time_ms <= targets$medium_max_time_ms)
  } else {
    validation$meets_time_threshold <- all(median_time_ms <= targets$large_max_time_s * 1000)
  }
  
  return(validation)
}

# Create summary report of benchmark results
create_benchmark_report <- function(benchmark_results_list) {
  
  report <- data.frame(
    dataset = character(),
    records = numeric(),
    median_time_ms = numeric(),
    processing_rate = numeric(),
    rate_formatted = character(),
    meets_targets = logical(),
    stringsAsFactors = FALSE
  )
  
  targets <- get_performance_targets()
  
  for (name in names(benchmark_results_list)) {
    result <- benchmark_results_list[[name]]
    
    # Extract dataset info
    if ("dataset_size" %in% names(attributes(result))) {
      size <- attr(result, "dataset_size")
    } else {
      # Try to infer from name
      size <- switch(name,
        "tiny" = 1000,
        "small" = 1000,
        "medium" = 50000,
        "large" = 500000,
        1000  # default
      )
    }
    
    validation <- validate_performance(result, size, targets)
    
    report <- rbind(report, data.frame(
      dataset = name,
      records = size,
      median_time_ms = validation$median_time_ms[1],  # Take first result
      processing_rate = validation$processing_rates[1],
      rate_formatted = format_processing_rate(validation$processing_rates[1]),
      meets_targets = validation$meets_minimum_rate,
      stringsAsFactors = FALSE
    ))
  }
  
  return(report)
}