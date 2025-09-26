#!/usr/bin/env Rscript

#' Benchmark Script for consolidate_by_employer_fast()
#'
#' This script generates test data of various sizes and benchmarks
#' the performance improvements of the fast consolidation function.

library(data.table)
library(longworkR)

# Function to generate test data
generate_test_data <- function(n_people, records_per_person = 20) {

  message(sprintf("Generating test data: %d people, %d records each...",
                  n_people, records_per_person))

  # Generate base structure
  cf <- rep(paste0("P", seq_len(n_people)), each = records_per_person)

  # Generate dates
  start_dates <- rep(seq(as.Date("2018-01-01"), by = "month",
                         length.out = records_per_person), n_people)

  # Variable contract lengths (1-3 months)
  durations <- sample(c(30, 60, 90), n_people * records_per_person,
                      replace = TRUE, prob = c(0.5, 0.3, 0.2))

  end_dates <- start_dates + durations - 1

  # Generate employers with consolidation opportunities
  # Each person has 2-4 different employers with returns
  employer_patterns <- list(
    c(rep("E1", 5), rep("E2", 5), rep("E1", 5), rep("E3", 5)),  # Return to E1
    c(rep("E1", 8), rep("E2", 6), rep("E1", 6)),                # Long E1 periods
    c("E1", "E2", "E1", "E2", "E1", "E3", "E3", "E1", rep("E2", 12)),  # Frequent changes
    c(rep("E1", 4), NA, rep("E1", 4), rep("E2", 5), NA, rep("E2", 6))  # With unemployment
  )

  employers <- rep(
    unlist(lapply(seq_len(n_people), function(i) {
      pattern <- employer_patterns[[(i - 1) %% length(employer_patterns) + 1]]
      if (length(pattern) < records_per_person) {
        c(pattern, rep(pattern[length(pattern)], records_per_person - length(pattern)))
      } else {
        pattern[1:records_per_person]
      }
    })),
    length.out = n_people * records_per_person
  )

  # Add employer IDs
  employers <- paste0(employers, "_", ((seq_along(employers) - 1) %/% records_per_person) %% 100)

  # Generate employment status
  arco <- ifelse(is.na(employers), 0L, 1L)
  stato <- ifelse(arco == 1, "occupato", "disoccupato")

  # Create data.table
  dt <- data.table(
    cf = cf,
    inizio = start_dates,
    fine = end_dates,
    durata = as.numeric(end_dates - start_dates + 1),
    datore = employers,
    arco = arco,
    stato = stato,
    # Add some additional columns for realism
    retribuzione = round(runif(length(cf), 1000, 5000), 2),
    qualifica = sample(c("operaio", "impiegato", "quadro"),
                      length(cf), replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    settore = sample(c("industria", "servizi", "commercio"),
                    length(cf), replace = TRUE)
  )

  # Fix overlaps within person
  setkey(dt, cf, inizio)
  dt[, `:=`(
    inizio = pmax(inizio, shift(fine + 1, type = "lag", fill = inizio[1])),
    overlap_flag = FALSE
  ), by = cf]

  dt[, fine := inizio + durata - 1]

  return(dt)
}

# Run benchmarks
run_benchmarks <- function() {

  cat("\n========================================\n")
  cat("consolidate_by_employer_fast() BENCHMARK\n")
  cat("========================================\n\n")

  # Test different dataset sizes
  sizes <- c(
    list(n = 100, records = 20),    # Small: 2,000 records
    list(n = 500, records = 20),    # Medium: 10,000 records
    list(n = 2500, records = 20),   # Large: 50,000 records
    list(n = 5000, records = 20)    # Very Large: 100,000 records
  )

  results <- list()

  for (size in sizes) {

    cat(sprintf("\n--- Dataset: %d people, ~%d total records ---\n",
                size$n, size$n * size$records))

    # Generate test data
    data <- generate_test_data(size$n, size$records)

    cat(sprintf("Actual records: %d\n", nrow(data)))
    cat(sprintf("People with multiple employers: %d\n",
                data[, .(n_employers = uniqueN(datore[!is.na(datore)])), by = cf][n_employers > 1, .N]))

    # Run benchmark
    bench <- benchmark_consolidation(data, "datore", min_lag = 8)

    # Store results
    results[[paste0(size$n, "_people")]] <- bench

    # Display summary
    cat("\nTiming Summary:\n")
    print(bench$timings)

    cat(sprintf("\nSpeedup vs Original:\n"))
    cat(sprintf("  Sequential: %.2fx faster\n", bench$speedup$fast_seq_vs_original))
    cat(sprintf("  Parallel:   %.2fx faster\n", bench$speedup$fast_par_vs_original))

    cat(sprintf("\nResults identical: Sequential=%s, Parallel=%s\n",
                bench$results_identical$sequential,
                bench$results_identical$parallel))

    # Show consolidation stats
    orig <- bench$results$original
    cat(sprintf("\nConsolidation reduction: %d -> %d records (%.1f%% reduction)\n",
                nrow(data), nrow(orig),
                100 * (1 - nrow(orig)/nrow(data))))
  }

  # Create summary plot if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    # Extract timing data for plotting
    timing_data <- do.call(rbind, lapply(names(results), function(name) {
      n_people <- as.numeric(sub("_people", "", name))
      timings <- results[[name]]$timings
      data.frame(
        n_people = n_people,
        method = timings$method,
        elapsed = timings$elapsed
      )
    }))

    p <- ggplot2::ggplot(timing_data, ggplot2::aes(x = n_people, y = elapsed,
                                                    color = method, group = method)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_x_log10(labels = scales::comma) +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        title = "consolidate_by_employer Performance Comparison",
        subtitle = "Log-scale comparison of execution time",
        x = "Number of People (log scale)",
        y = "Time (seconds, log scale)",
        color = "Method"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11)
      )

    cat("\nSaving performance plot to 'benchmark_results.png'...\n")
    ggplot2::ggsave("benchmark_results.png", p, width = 10, height = 6, dpi = 150)
  }

  cat("\n========================================\n")
  cat("BENCHMARK COMPLETE\n")
  cat("========================================\n\n")

  # Return all results
  invisible(results)
}

# Check if script is being run directly
if (!interactive()) {
  # Load package (assuming it's installed or we're in development mode)
  if (!requireNamespace("longworkR", quietly = TRUE)) {
    devtools::load_all()
  }

  # Run benchmarks
  results <- run_benchmarks()

  # Print final summary
  cat("\nFINAL SUMMARY\n")
  cat("=============\n\n")

  # Calculate average speedups across all sizes
  speedups_seq <- sapply(results, function(r) r$speedup$fast_seq_vs_original)
  speedups_par <- sapply(results, function(r) r$speedup$fast_par_vs_original)

  cat(sprintf("Average speedup (Sequential): %.2fx\n", mean(speedups_seq)))
  cat(sprintf("Average speedup (Parallel):   %.2fx\n", mean(speedups_par)))
  cat(sprintf("Range (Sequential): %.2fx - %.2fx\n", min(speedups_seq), max(speedups_seq)))
  cat(sprintf("Range (Parallel):   %.2fx - %.2fx\n", min(speedups_par), max(speedups_par)))

} else {
  cat("Benchmark script loaded. Run 'run_benchmarks()' to start.\n")
}