#!/usr/bin/env Rscript
#
# benchmark_optimizations.R
# Self-contained benchmark script consolidating all performance measurement
# code from the longworkR test suite.
#
# Usage:
#   Rscript tests/benchmark_optimizations.R
#
# Sections:
#   1. Setup
#   2. Helper functions
#   3. Micro-benchmarks
#   4. Memory efficiency
#   5. Large dataset stress test
#   6. Real-world benchmark (sample.rds)
#   7. Scaling test (single-period ratio)
#   8. Dataset suite benchmarks
#   9. Summary

# 1. Setup -----

tryCatch(
  library(longworkR),
  error = function(e) {
    cat("library(longworkR) failed, falling back to devtools::load_all()\n")
    devtools::load_all(".")
  }
)

library(data.table)
library(microbenchmark)

has_profmem <- requireNamespace("profmem", quietly = TRUE)

cat("\n")
cat("========================================\n")
cat("  longworkR Optimization Benchmarks\n")
cat("========================================\n\n")
cat(sprintf("Timestamp: %s\n", Sys.time()))
cat(sprintf("R version: %s\n", R.version.string))
cat(sprintf("Platform:  %s\n", R.version$platform))
cat(sprintf("OS:        %s\n", Sys.info()["sysname"]))
cat(sprintf("Machine:   %s\n", Sys.info()["machine"]))
cat(sprintf(
  "profmem:   %s\n",
  if (has_profmem) "available" else "not available"
))
cat("\n")


# 2. Helper functions -----

# -- From helper-phase4-optimization.R --

calculate_expected_speedup <- function(
  pct_single,
  overhead_ms = 2,
  records_per_sec = 41000
) {
  fraction_skip <- pct_single / 100
  us_per_record <- 1000000 / records_per_sec
  overhead_us <- overhead_ms * 1000
  speedup <- 1 / (1 - fraction_skip + 0.05)
  return(speedup)
}

print_performance_comparison <- function(
  baseline_time,
  optimized_time,
  n_records
) {
  speedup <- baseline_time / optimized_time
  baseline_rate <- n_records / baseline_time
  optimized_rate <- n_records / optimized_time
  cat("\n=== Performance Comparison ===\n")
  cat(sprintf(
    "Baseline:  %.3f sec (%s rec/sec)\n",
    baseline_time,
    format(round(baseline_rate), big.mark = ",")
  ))
  cat(sprintf(
    "Optimized: %.3f sec (%s rec/sec)\n",
    optimized_time,
    format(round(optimized_rate), big.mark = ",")
  ))
  cat(sprintf("Speedup:   %.2fx\n", speedup))
  cat(sprintf(
    "Time saved: %.3f sec (%.1f%%)\n",
    baseline_time - optimized_time,
    (1 - optimized_time / baseline_time) * 100
  ))
  cat("==============================\n\n")
  invisible(list(
    speedup = speedup,
    baseline_rate = baseline_rate,
    optimized_rate = optimized_rate
  ))
}

generate_mixed_employment_data <- function(
  n_single = 500,
  n_multi = 500,
  periods_per_multi = 3,
  add_overlaps = TRUE,
  add_gaps = TRUE,
  seed = 12345
) {
  set.seed(seed)

  # Single-period workers
  single_workers <- data.table(
    cf = paste0("S", 1:n_single),
    inizio = as.Date("2020-01-01") + sample(0:730, n_single, replace = TRUE),
    durata = sample(90:365, n_single, replace = TRUE)
  )
  single_workers[, fine := inizio + durata - 1]
  single_workers[, arco := 1L]
  single_workers[, over_id := 0L]
  single_workers[,
    COD_TIPOLOGIA_CONTRATTUALE := sample(
      c("A.01.00", "A.03.00", "A.04.00"),
      nrow(single_workers),
      replace = TRUE
    )
  ]
  single_workers[,
    prior := sample(
      c(0, 1),
      nrow(single_workers),
      replace = TRUE,
      prob = c(0.3, 0.7)
    )
  ]

  # Multi-period workers
  multi_records <- lapply(1:n_multi, function(i) {
    n_periods <- rpois(1, lambda = periods_per_multi - 1) + 2
    n_periods <- min(n_periods, 10)
    cf_id <- paste0("M", i)
    start_date <- as.Date("2020-01-01") + sample(0:365, 1)

    periods <- data.table(
      cf = cf_id,
      period_num = 1:n_periods
    )
    periods[, inizio := start_date]
    periods[, durata := sample(60:180, .N, replace = TRUE)]

    for (j in 2:n_periods) {
      prev_end <- periods[j - 1, inizio + durata - 1]
      if (add_gaps && runif(1) < 0.5) {
        gap_days <- sample(1:60, 1)
        periods[j, inizio := prev_end + gap_days + 1]
      } else {
        periods[j, inizio := prev_end + 1]
      }
    }

    periods[, fine := inizio + durata - 1]
    periods[, arco := 1L]
    periods[, over_id := 0L]

    if (add_overlaps && runif(1) < 0.3) {
      overlap_indices <- sample(1:n_periods, min(2, n_periods))
      periods[
        overlap_indices,
        over_id := sample(1:5, length(overlap_indices), replace = TRUE)
      ]
    }

    periods[,
      COD_TIPOLOGIA_CONTRATTUALE := sample(
        c("A.01.00", "A.03.00", "A.04.00"),
        .N,
        replace = TRUE
      )
    ]
    periods[, prior := sample(c(0, 1), .N, replace = TRUE, prob = c(0.3, 0.7))]
    periods[, period_num := NULL]
    return(periods)
  })

  multi_workers <- rbindlist(multi_records)

  all_data <- rbindlist(
    list(single_workers, multi_workers),
    use.names = TRUE,
    fill = TRUE
  )
  setkey(all_data, cf, inizio, fine)
  all_data[, retribuzione := round(runif(.N, 1000, 3000), 2)]
  all_data[,
    ore := sample(c(20, 30, 40), .N, replace = TRUE, prob = c(0.2, 0.3, 0.5))
  ]
  return(all_data)
}

get_single_period_stats <- function(data) {
  periods_per_worker <- data[, .N, by = cf]
  n_single <- sum(periods_per_worker$N == 1)
  n_multi <- sum(periods_per_worker$N > 1)
  total_workers <- nrow(periods_per_worker)
  pct_single <- round(n_single / total_workers * 100, 1)
  pct_multi <- round(n_multi / total_workers * 100, 1)
  avg_periods_multi <- if (n_multi > 0) {
    round(mean(periods_per_worker[N > 1, N]), 2)
  } else {
    NA_real_
  }
  list(
    total_workers = total_workers,
    n_single = n_single,
    n_multi = n_multi,
    pct_single = pct_single,
    pct_multi = pct_multi,
    avg_periods_multi = avg_periods_multi,
    total_records = nrow(data),
    records_single = sum(periods_per_worker[N == 1, N]),
    records_multi = sum(periods_per_worker[N > 1, N])
  )
}

# -- From test-optimizations.R --

generate_test_data <- function(n_persons = 100, n_contracts_per_person = 5) {
  cfs <- paste0("PERSON", sprintf("%05d", 1:n_persons))
  contract_types <- c("A.01.00", "A.03.00", "B.01.01", "B.02.05", "C.01.00")

  dt_list <- lapply(cfs, function(cf_id) {
    n_contracts <- sample(3:n_contracts_per_person, 1)
    start_date <- as.Date("2020-01-01")

    contracts <- lapply(1:n_contracts, function(i) {
      duration <- sample(30:365, 1)
      end_date <- start_date + duration
      contract <- data.table(
        cf = cf_id,
        inizio = start_date,
        fine = end_date,
        durata = as.numeric(duration),
        arco = sample(0:1, 1, prob = c(0.9, 0.1)),
        prior = sample(0:1, 1, prob = c(0.3, 0.7)),
        over_id = i,
        COD_TIPOLOGIA_CONTRATTUALE = sample(contract_types, 1),
        salary = round(rnorm(1, mean = 30000, sd = 10000), 2),
        company = paste0("Company_", sample(1:50, 1))
      )
      start_date <<- end_date + sample(1:90, 1)
      contract
    })
    rbindlist(contracts)
  })

  dt <- rbindlist(dt_list)
  setorder(dt, cf, inizio)
  return(dt)
}

# -- From helper-benchmark-data.R --

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
    n_persons <- max(10, n_records %/% 8)
  }
  if (is.null(n_employers)) {
    n_employers <- max(5, n_records %/% 50)
  }

  cf_weights <- rexp(n_persons, rate = 0.1)
  cf_probs <- cf_weights / sum(cf_weights)
  person_ids <- sample(1:n_persons, n_records, replace = TRUE, prob = cf_probs)

  employer_weights <- rexp(n_employers, rate = 0.05)
  employer_probs <- employer_weights / sum(employer_weights)
  employer_ids <- sample(
    paste0("COMP", sprintf("%04d", 1:n_employers)),
    n_records,
    replace = TRUE,
    prob = employer_probs
  )

  start_dates <- sample(
    seq(date_range[1], date_range[2], by = "day"),
    n_records,
    replace = TRUE
  )
  durations <- pmax(1, round(rexp(n_records, rate = 1 / 180)))
  end_dates <- start_dates + durations

  data <- data.table(
    cf = person_ids,
    inizio = start_dates,
    fine = end_dates,
    durata = as.numeric(durations),
    arco = sample(
      c(0, 1, 2),
      n_records,
      replace = TRUE,
      prob = c(0.05, 0.75, 0.2)
    ),
    CODICE_FISCALE_AZIENDA = employer_ids,
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      contract_types,
      n_records,
      replace = TRUE
    ),
    prior = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.3, 0.7))
  )

  data <- data[order(cf, inizio)]
  data[, over_id := seq_len(.N), by = cf]

  if (temporal_consolidation_rate > 0) {
    consolidation_candidates <- data[,
      .I[runif(.N) < temporal_consolidation_rate],
      by = cf
    ]$V1
    if (length(consolidation_candidates) > 0) {
      pairs <- matrix(
        sample(
          consolidation_candidates,
          2 * (length(consolidation_candidates) %/% 2)
        ),
        ncol = 2
      )
      for (i in seq_len(nrow(pairs))) {
        same_over_id <- max(data$over_id) + i
        data[pairs[i, ], over_id := same_over_id]
      }
    }
  }

  data[,
    retribuzioni_lorde_somma := round(runif(.N, 1000, 5000) * durata / 30, 2)
  ]
  data[, ore_lavorate_somma := round(runif(.N, 20, 40) * durata / 7, 1)]

  return(data)
}

generate_performance_test_data <- function(
  size_category = c("small", "medium", "large", "xlarge"),
  complexity = c("simple", "complex", "pathological"),
  seed = 42
) {
  size_category <- match.arg(size_category)
  complexity <- match.arg(complexity)

  size_params <- switch(
    size_category,
    "small" = list(n_records = 1000, n_persons = 100, n_employers = 20),
    "medium" = list(n_records = 50000, n_persons = 5000, n_employers = 500),
    "large" = list(n_records = 500000, n_persons = 50000, n_employers = 2000),
    "xlarge" = list(n_records = 1000000, n_persons = 100000, n_employers = 5000)
  )

  complexity_params <- switch(
    complexity,
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
      temporal_rate = 0.7,
      employer_rate = 0.5,
      contract_variety = 15
    )
  )

  contract_types <- c(
    "A.01.00",
    "A.03.00",
    "B.01.00",
    "C.01.00",
    "D.01.00",
    "E.01.00",
    "F.01.00",
    "G.01.00",
    "H.01.00",
    "I.01.00",
    "J.01.00",
    "K.01.00",
    "L.01.00",
    "M.01.00",
    "N.01.00"
  )[1:complexity_params$contract_variety]

  data <- generate_realistic_employment_data(
    n_records = size_params$n_records,
    n_persons = size_params$n_persons,
    n_employers = size_params$n_employers,
    temporal_consolidation_rate = complexity_params$temporal_rate,
    employer_consolidation_rate = complexity_params$employer_rate,
    contract_types = contract_types,
    seed = seed
  )

  if (complexity == "pathological") {
    overlap_data <- data[sample(.N, .N * 0.1)][,
      inizio := inizio - sample(1:30, .N, replace = TRUE)
    ]
    data <- rbind(data, overlap_data, fill = TRUE)
    data <- data[order(cf, inizio)]
    data[,
      over_id := cumsum(c(
        TRUE,
        diff(cf) != 0 |
          c(FALSE, diff(as.numeric(inizio))[1:(.N - 1)] > 10)
      ))
    ]
  }

  attr(data, "size_category") <- size_category
  attr(data, "complexity") <- complexity
  attr(data, "generation_params") <- list(
    size = size_params,
    complexity = complexity_params
  )

  return(data)
}

create_benchmark_dataset_suite <- function(seed = 42) {
  datasets <- list()

  datasets$tiny <- generate_performance_test_data("small", "simple", seed)
  datasets$small <- generate_performance_test_data("small", "complex", seed)
  datasets$medium <- generate_performance_test_data("medium", "simple", seed)
  datasets$medium_complex <- generate_performance_test_data(
    "medium",
    "complex",
    seed
  )
  datasets$large <- generate_performance_test_data("large", "simple", seed)

  datasets$pathological_small <- generate_performance_test_data(
    "small",
    "pathological",
    seed
  )
  datasets$pathological_medium <- generate_performance_test_data(
    "medium",
    "pathological",
    seed
  )

  if (
    Sys.info()["sysname"] != "Darwin" || as.numeric(Sys.info()["release"]) < 20
  ) {
    cat("Skipping xlarge dataset - system limitations\n")
  } else {
    datasets$xlarge <- generate_performance_test_data("xlarge", "simple", seed)
  }

  return(datasets)
}

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

get_performance_targets <- function() {
  list(
    minimum_rate = 300000,
    target_rate = 360000,
    optimal_rate = 420000,
    max_memory_multiplier = 5,
    max_rate_variance = 0.3,
    small_max_time_ms = 100,
    medium_max_time_ms = 5000,
    large_max_time_s = 10
  )
}

validate_performance <- function(
  benchmark_results,
  dataset_size,
  targets = get_performance_targets()
) {
  results <- summary(benchmark_results)
  median_time_ms <- results$median
  processing_rates <- dataset_size / (median_time_ms / 1000)

  validation <- list(
    meets_minimum_rate = any(processing_rates >= targets$minimum_rate),
    meets_target_rate = any(processing_rates >= targets$target_rate),
    processing_rates = processing_rates,
    median_time_ms = median_time_ms,
    rate_variance = sd(processing_rates) / mean(processing_rates),
    acceptable_variance = sd(processing_rates) / mean(processing_rates) <=
      targets$max_rate_variance
  )

  if (dataset_size <= 10000) {
    validation$meets_time_threshold <- all(
      median_time_ms <= targets$small_max_time_ms
    )
  } else if (dataset_size <= 100000) {
    validation$meets_time_threshold <- all(
      median_time_ms <= targets$medium_max_time_ms
    )
  } else {
    validation$meets_time_threshold <- all(
      median_time_ms <= targets$large_max_time_s * 1000
    )
  }

  return(validation)
}

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

    if ("dataset_size" %in% names(attributes(result))) {
      size <- attr(result, "dataset_size")
    } else {
      size <- switch(
        name,
        "tiny" = 1000,
        "small" = 1000,
        "medium" = 50000,
        "large" = 500000,
        1000
      )
    }

    validation <- validate_performance(result, size, targets)

    report <- rbind(
      report,
      data.frame(
        dataset = name,
        records = size,
        median_time_ms = validation$median_time_ms[1],
        processing_rate = validation$processing_rates[1],
        rate_formatted = format_processing_rate(validation$processing_rates[1]),
        meets_targets = validation$meets_minimum_rate,
        stringsAsFactors = FALSE
      )
    )
  }

  return(report)
}


# 3. Micro-benchmarks -----

cat("\n=== 3. Micro-benchmarks ===\n\n")

tryCatch(
  {
    # .calculate_mode_optimized vs base R
    test_vec <- sample(letters[1:10], 10000, replace = TRUE)

    mode_base <- function(x) {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }

    bench_mode <- microbenchmark(
      optimized = longworkR:::.calculate_mode_optimized(test_vec),
      base = mode_base(test_vec),
      times = 100
    )

    cat("Mode calculation benchmark (100 iterations):\n")
    print(summary(bench_mode)[, c("expr", "min", "median", "mean", "max")])
    median_times <- summary(bench_mode)$median
    cat(sprintf("Speedup: %.2fx\n\n", median_times[2] / median_times[1]))

    # .process_chain_value vs naive
    test_chains <- replicate(10000, {
      n_parts <- sample(1:5, 1)
      paste(sample(LETTERS, n_parts, replace = TRUE), collapse = "->")
    })

    process_chain_naive <- function(values, eval_chain) {
      if (eval_chain == "last") {
        sapply(strsplit(values, "->"), function(x) trimws(x[length(x)]))
      } else {
        sapply(strsplit(values, "->"), function(x) trimws(x[1]))
      }
    }

    bench_chain <- microbenchmark(
      optimized = longworkR:::.process_chain_value(test_chains, "last"),
      naive = process_chain_naive(test_chains, "last"),
      times = 50
    )

    cat("Chain processing benchmark (50 iterations):\n")
    print(summary(bench_chain)[, c("expr", "min", "median", "mean", "max")])
    median_times <- summary(bench_chain)$median
    cat(sprintf("Speedup: %.2fx\n\n", median_times[2] / median_times[1]))
  },
  error = function(e) {
    cat(sprintf("Micro-benchmark error: %s\n\n", conditionMessage(e)))
  }
)


# 4. Memory efficiency -----

cat("\n=== 4. Memory efficiency ===\n\n")

tryCatch(
  {
    # Weighted median profmem
    if (has_profmem) {
      library(profmem)
      values <- runif(100)
      weights <- rep(10000, 100)
      mem_usage <- profmem({
        result <- longworkR:::.calculate_weighted_median_optimized(
          values,
          weights
        )
      })
      total_bytes <- sum(mem_usage$bytes, na.rm = TRUE)
      cat(sprintf(
        "Weighted median memory usage: %.2f KB\n",
        total_bytes / 1024
      ))
    } else {
      cat("profmem not available, skipping memory profiling\n")
    }

    # Type conversion throughput
    n_rows <- 10000
    n_cols <- 50
    test_data <- as.data.table(
      lapply(1:n_cols, function(i) sample(1:100, n_rows, replace = TRUE))
    )
    setnames(test_data, paste0("col_", 1:n_cols))

    start_time <- Sys.time()
    result <- longworkR:::.convert_types_optimized(
      test_data,
      modify_in_place = FALSE
    )
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf(
      "Type conversion for %d rows x %d columns: %.2f ms\n",
      n_rows,
      n_cols,
      elapsed * 1000
    ))
    cat(sprintf("All columns numeric: %s\n\n", all(sapply(result, is.numeric))))
  },
  error = function(e) {
    cat(sprintf("Memory efficiency error: %s\n\n", conditionMessage(e)))
  }
)


# 5. Large dataset stress test -----

cat("\n=== 5. Large dataset stress test ===\n\n")

tryCatch(
  {
    set.seed(456)
    large_data <- generate_test_data(
      n_persons = 1000,
      n_contracts_per_person = 10
    )
    cat(sprintf(
      "Dataset: %d records (%d persons x ~10 contracts)\n",
      nrow(large_data),
      1000
    ))

    start_time <- Sys.time()
    result <- suppressMessages(
      analyze_employment_transitions(
        large_data,
        transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
        consolidation_mode = "none",
        statistics_variables = c("salary", "company")
      )
    )
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("Processed in %.2f seconds\n", elapsed))
    cat(sprintf("Result: %d transitions\n\n", nrow(result)))
  },
  error = function(e) {
    cat(sprintf("Large dataset stress test error: %s\n\n", conditionMessage(e)))
  }
)


# 6. Real-world benchmark (sample.rds) -----

cat("\n=== 6. Real-world benchmark (sample.rds) ===\n\n")

tryCatch(
  {
    data_file <- file.path(getwd(), "data", "sample.rds")
    if (!file.exists(data_file)) {
      data_file <- file.path(getwd(), "..", "data", "sample.rds")
    }

    if (file.exists(data_file)) {
      data <- readRDS(data_file)
      cat(sprintf("Dataset: sample.rds (%d records)\n", nrow(data)))

      stats <- get_single_period_stats(data)
      cat(sprintf(
        "Workers: %d total, %d single-period (%.1f%%), %d multi-period (%.1f%%)\n\n",
        stats$total_workers,
        stats$n_single,
        stats$pct_single,
        stats$n_multi,
        stats$pct_multi
      ))

      # Per-step benchmarks
      cat("--- Benchmarking consolidate_overlapping() ---\n")
      bench_overlap <- microbenchmark(consolidate_overlapping(data), times = 10)
      print(summary(bench_overlap)[, c("expr", "min", "median", "max")])
      throughput_overlap <- nrow(data) / (summary(bench_overlap)$median / 1000)
      cat(sprintf(
        "Throughput: %s rec/sec\n\n",
        format(round(throughput_overlap), big.mark = ",")
      ))

      cat("--- Benchmarking consolidate_adjacent() ---\n")
      step1 <- consolidate_overlapping(data)
      bench_adjacent <- microbenchmark(consolidate_adjacent(step1), times = 10)
      print(summary(bench_adjacent)[, c("expr", "min", "median", "max")])
      throughput_adjacent <- nrow(step1) /
        (summary(bench_adjacent)$median / 1000)
      cat(sprintf(
        "Throughput: %s rec/sec\n\n",
        format(round(throughput_adjacent), big.mark = ",")
      ))

      cat("--- Benchmarking consolidate_short_gaps(30) ---\n")
      step2 <- consolidate_adjacent(step1)
      bench_gaps <- microbenchmark(
        consolidate_short_gaps(step2, 30),
        times = 10
      )
      print(summary(bench_gaps)[, c("expr", "min", "median", "max")])
      throughput_gaps <- nrow(step2) / (summary(bench_gaps)$median / 1000)
      cat(sprintf(
        "Throughput: %s rec/sec\n\n",
        format(round(throughput_gaps), big.mark = ",")
      ))

      cat("--- Benchmarking Full Chain ---\n")
      bench_chain <- microbenchmark(
        {
          data |>
            consolidate_overlapping() |>
            consolidate_adjacent() |>
            consolidate_short_gaps(30)
        },
        times = 10
      )
      print(summary(bench_chain)[, c("expr", "min", "median", "max")])
      throughput_chain <- nrow(data) / (summary(bench_chain)$median / 1000)
      cat(sprintf(
        "Full chain throughput: %s rec/sec\n\n",
        format(round(throughput_chain), big.mark = ",")
      ))

      # Phase comparison
      phase3_baseline <- 41000
      speedup <- throughput_chain / phase3_baseline
      cat("--- Phase 4 vs Phase 3 ---\n")
      cat(sprintf(
        "Phase 3 baseline: %s rec/sec\n",
        format(phase3_baseline, big.mark = ",")
      ))
      cat(sprintf(
        "Phase 4 actual:   %s rec/sec\n",
        format(round(throughput_chain), big.mark = ",")
      ))
      cat(sprintf("Speedup:          %.2fx\n", speedup))
      expected_speedup <- calculate_expected_speedup(stats$pct_single)
      cat(sprintf(
        "Expected speedup (%.1f%% singles): %.2fx\n\n",
        stats$pct_single,
        expected_speedup
      ))
    } else {
      cat("sample.rds not found. Skipping real-world benchmark.\n\n")
    }
  },
  error = function(e) {
    cat(sprintf("Real-world benchmark error: %s\n\n", conditionMessage(e)))
  }
)


# 7. Scaling test (single-period ratio) -----

cat("\n=== 7. Scaling test (single-period ratio) ===\n\n")

tryCatch(
  {
    percentages <- c(20, 40, 60)
    scaling_results <- lapply(percentages, function(pct) {
      n_single <- 1000 * pct / 100
      n_multi <- 1000 - n_single
      test_data <- generate_mixed_employment_data(
        n_single = n_single,
        n_multi = n_multi,
        periods_per_multi = 3,
        seed = 9999 + pct
      )
      start_time <- Sys.time()
      result <- test_data |>
        consolidate_overlapping() |>
        consolidate_adjacent() |>
        consolidate_short_gaps(30)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      throughput <- nrow(test_data) / elapsed
      expected_speedup <- calculate_expected_speedup(pct)
      cat(sprintf(
        "%d%% singles: %s rec/sec (expected speedup: %.2fx)\n",
        pct,
        format(round(throughput), big.mark = ","),
        expected_speedup
      ))
      list(
        pct = pct,
        throughput = throughput,
        expected_speedup = expected_speedup
      )
    })

    throughputs <- sapply(scaling_results, function(r) r$throughput)
    cat(sprintf(
      "\nScaling trend: %.0f -> %.0f -> %.0f rec/sec\n\n",
      throughputs[1],
      throughputs[2],
      throughputs[3]
    ))
  },
  error = function(e) {
    cat(sprintf("Scaling test error: %s\n\n", conditionMessage(e)))
  }
)


# 8. Dataset suite benchmarks -----

cat("\n=== 8. Dataset suite benchmarks ===\n\n")

suite_results <- list()

tryCatch(
  {
    suite <- create_benchmark_dataset_suite(seed = 42)

    for (name in names(suite)) {
      ds <- suite[[name]]
      cat(sprintf("%-25s %7d records ... ", name, nrow(ds)))
      start_time <- Sys.time()
      result <- suppressMessages(
        analyze_employment_transitions(
          ds,
          transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
          consolidation_mode = "none"
        )
      )
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- nrow(ds) / elapsed
      cat(sprintf(
        "%.2f sec  %s rec/sec\n",
        elapsed,
        format(round(rate), big.mark = ",")
      ))
      suite_results[[name]] <- list(
        records = nrow(ds),
        elapsed = elapsed,
        rate = rate
      )
    }
    cat("\n")
  },
  error = function(e) {
    cat(sprintf("\nDataset suite benchmark error: %s\n\n", conditionMessage(e)))
  }
)


# 9. Summary -----

cat("\n=== 9. Summary ===\n\n")
cat(sprintf("Timestamp: %s\n", Sys.time()))
cat(sprintf("R version: %s\n", R.version.string))
cat(sprintf("Platform:  %s\n", R.version$platform))
cat(sprintf("OS:        %s\n\n", Sys.info()["sysname"]))

if (length(suite_results) > 0) {
  cat("Dataset Suite Results:\n")
  cat(sprintf(
    "%-25s %10s %10s %15s\n",
    "Dataset",
    "Records",
    "Time (s)",
    "Rate (rec/sec)"
  ))
  cat(paste(rep("-", 65), collapse = ""), "\n")
  for (name in names(suite_results)) {
    r <- suite_results[[name]]
    cat(sprintf(
      "%-25s %10d %10.2f %15s\n",
      name,
      r$records,
      r$elapsed,
      format(round(r$rate), big.mark = ",")
    ))
  }
}

cat("\n=== Benchmark complete ===\n")
