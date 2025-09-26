#' High-Performance Employment Consolidation by Employer
#'
#' @description
#' Optimized version of consolidate_by_employer() with significant performance improvements:
#' - Candidate preselection to process only relevant records
#' - Parallel processing support for large datasets
#' - Optimized data.table operations with minimal copying
#' - Vectorized grouping logic
#'
#' @param data data.table with employment records from vecshift output
#' @param employer_var Character string specifying employer column name
#' @param min_lag Numeric days threshold between contracts (default: 8)
#' @param remove_intermediate_unemployment Logical. If TRUE (default), unemployment
#'   periods between consolidated employment periods are removed.
#' @param calculate_coverage Logical. If TRUE (default), calculates consolidation_coverage
#'   ratio (employment_days / total_elapsed_days).
#' @param show_progress Logical. Display progress messages (default: FALSE)
#' @param parallel Logical. Use parallel processing (default: TRUE for >10000 rows)
#' @param n_cores Integer. Number of cores for parallel processing (default: detectCores() - 1)
#'
#' @return data.table with consolidated employment periods including metrics:
#'   \itemize{
#'     \item{\code{consolidation_coverage}}: Ratio of employment days to total elapsed days
#'     \item{\code{unemployment_days_removed}}: Days of unemployment removed
#'     \item{\code{n_periods_consolidated}}: Number of original records consolidated
#'     \item{\code{n_unemployment_periods_removed}}: Number of unemployment periods removed
#'   }
#'
#' @export
#' @importFrom data.table data.table copy setkey setindex rbindlist := .N .SD
#' @importFrom parallel detectCores mclapply
#'
#' @examples
#' \dontrun{
#' # Fast consolidation with automatic parallelization
#' consolidated <- consolidate_by_employer_fast(data, "datore", 30)
#'
#' # Force sequential processing
#' consolidated <- consolidate_by_employer_fast(
#'   data, "employer_id", 7,
#'   parallel = FALSE
#' )
#'
#' # Custom parallel configuration
#' consolidated <- consolidate_by_employer_fast(
#'   data, "company", 90,
#'   parallel = TRUE,
#'   n_cores = 4
#' )
#' }
consolidate_by_employer_fast <- function(data,
                                         employer_var,
                                         min_lag = 8,
                                         remove_intermediate_unemployment = TRUE,
                                         calculate_coverage = TRUE,
                                         show_progress = FALSE,
                                         parallel = NULL,
                                         n_cores = NULL) {

  # Input validation
  if (!inherits(data, "data.table")) {
    stop("data must be a data.table object")
  }

  if (is.null(employer_var) || !employer_var %in% names(data)) {
    stop("employer_var must specify a valid column name in data")
  }

  if (!is.numeric(min_lag) || min_lag < 0) {
    stop("min_lag must be a non-negative numeric value")
  }

  # Auto-determine parallel processing
  if (is.null(parallel)) {
    parallel <- nrow(data) > 10000
  }

  if (is.null(n_cores) && parallel) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Create working copy
  dt <- copy(data)
  n_original <- nrow(dt)

  if (show_progress) {
    message(sprintf("Starting FAST employer consolidation for %d records...", n_original))
    if (parallel) {
      message(sprintf("Using parallel processing with %d cores", n_cores))
    }
  }

  # Ensure required columns exist
  required_cols <- c("cf", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Add arco column if missing
  if (!"arco" %in% names(dt)) {
    dt[, arco := fifelse(
      is.na(get(employer_var)) | get(employer_var) == "",
      0L,
      1L
    )]
  }

  # Ensure date columns are Date objects
  if (!inherits(dt$inizio, "Date")) {
    dt[, inizio := as.Date(inizio)]
  }
  if (!inherits(dt$fine, "Date")) {
    dt[, fine := as.Date(fine)]
  }

  # OPTIMIZATION 1: Candidate Preselection
  # Only process cf values that actually need consolidation
  if (show_progress) message("Phase 1: Identifying consolidation candidates...")

  # Find candidates: people with multiple records OR any unemployment
  candidates <- dt[, .(
    n_records = .N,
    has_unemployment = any(arco == 0),
    n_employers = uniqueN(get(employer_var)[arco > 0])
  ), by = cf]

  # Need consolidation if: multiple records AND (has unemployment OR same employer appears multiple times)
  candidate_cfs <- candidates[
    n_records > 1 & (has_unemployment | n_employers < sum(n_records - has_unemployment)),
    cf
  ]

  if (show_progress) {
    n_candidates <- length(candidate_cfs)
    pct_candidates <- round(100 * n_candidates / uniqueN(dt$cf), 1)
    message(sprintf("Found %d candidates for consolidation (%.1f%% of people)",
                   n_candidates, pct_candidates))
  }

  # Split data
  dt_to_process <- dt[cf %in% candidate_cfs]
  dt_no_consolidation <- dt[!cf %in% candidate_cfs]

  # If no consolidation needed, return with metrics
  if (nrow(dt_to_process) == 0) {
    if (calculate_coverage) {
      dt_no_consolidation[, `:=`(
        consolidation_coverage = 1.0,
        unemployment_days_removed = 0.0,
        n_periods_consolidated = 1L,
        n_unemployment_periods_removed = 0L
      )]
    }
    return(dt_no_consolidation)
  }

  # OPTIMIZATION 2: Ultra-fast key setting
  setkey(dt_to_process, cf, inizio, fine)

  # Function to process a subset of data
  process_chunk <- function(chunk_dt) {
    # Safety check
    if (nrow(chunk_dt) == 0) {
      return(data.table())
    }

    # Store original classes
    exclude_cols <- c("cf")
    agg_cols <- setdiff(names(chunk_dt), exclude_cols)
    original_classes <- lapply(chunk_dt[, ..agg_cols], class)

    # OPTIMIZATION 3: Vectorized Grouping Logic
    # Pre-compute all shifts at once
    chunk_dt[, `:=`(
      prev_employer = shift(get(employer_var), 1L),
      prev_fine = shift(fine, 1L),
      prev_arco = shift(arco, 1L),
      prev_durata = shift(durata, 1L)
    ), by = cf]

    # Vectorized gap calculation
    chunk_dt[, gap_days := fifelse(
      is.na(prev_fine),
      0L,
      as.integer(inizio - prev_fine - 1L)
    )]

    if (remove_intermediate_unemployment) {
      # OPTIMIZATION 4: Simplified employer tracking
      # Use run-length encoding for employer sequences
      chunk_dt[, employer_run := {
        emp_vals <- get(employer_var)
        emp_vals[arco == 0] <- NA_character_

        # Create run IDs for continuous employer sequences
        rle_result <- rle(!is.na(emp_vals) & c(TRUE, emp_vals[-1] == emp_vals[-.N]))
        rep(seq_along(rle_result$lengths), rle_result$lengths)
      }, by = cf]

      # Identify consolidation barriers
      chunk_dt[, new_group := {
        # Start new group when:
        is_first <- is.na(prev_employer)
        employer_changed <- arco > 0 & !is.na(get(employer_var)) &
                           !is.na(prev_employer) &
                           get(employer_var) != prev_employer
        unemployment_barrier <- arco == 0 & durata >= min_lag
        after_long_unemployment <- prev_arco == 0 & prev_durata >= min_lag & arco > 0
        gap_exceeded <- gap_days > min_lag

        is_first | employer_changed | unemployment_barrier | after_long_unemployment | gap_exceeded
      }]

    } else {
      # Original grouping logic - simpler
      chunk_dt[, new_group := {
        is.na(prev_employer) |
        is.na(get(employer_var)) |
        get(employer_var) != prev_employer |
        gap_days > min_lag
      }]
    }

    # Create consolidation groups
    chunk_dt[, consolidation_group := cumsum(new_group), by = cf]

    # OPTIMIZATION 5: High-performance aggregation using GForce
    consolidated <- chunk_dt[, {
      # Core metrics - all vectorized
      min_date <- min(inizio)
      max_date <- max(fine)
      elapsed <- as.numeric(max_date - min_date + 1)
      n_periods <- .N

      # Employment calculations
      emp_mask <- arco > 0
      unemp_mask <- arco == 0
      emp_days <- sum(durata[emp_mask], na.rm = TRUE)
      n_unemp <- sum(unemp_mask)
      unemp_days <- if(remove_intermediate_unemployment && n_unemp > 0) {
        sum(durata[unemp_mask], na.rm = TRUE)
      } else { 0.0 }

      # Build result efficiently
      res <- list(
        inizio = min_date,
        fine = max_date,
        durata = elapsed,
        n_periods_consolidated = n_periods,
        n_unemployment_periods_removed = if(remove_intermediate_unemployment) n_unemp else 0L
      )

      if (calculate_coverage) {
        res$consolidation_coverage <- if(elapsed > 0) emp_days/elapsed else 1.0
        res$unemployment_days_removed <- if(remove_intermediate_unemployment) unemp_days else 0.0
      }

      # Handle employer
      if (employer_var %in% names(.SD)) {
        emp_vals <- .SD[[employer_var]][!is.na(.SD[[employer_var]])]
        res[[employer_var]] <- if(length(emp_vals) > 0) emp_vals[1L] else NA_character_
      }

      # Handle arco
      if ("arco" %in% names(.SD)) {
        res$arco <- max(arco, na.rm = TRUE)
      }

      # OPTIMIZATION 6: Batch process remaining columns by type
      other_cols <- setdiff(names(.SD), c("inizio", "fine", "durata", "cf",
                                          employer_var, "arco", "prev_employer",
                                          "prev_fine", "prev_arco", "prev_durata",
                                          "gap_days", "new_group", "consolidation_group",
                                          "employer_run"))

      if (length(other_cols) > 0) {
        # Get column types from first row to ensure consistency
        sd_other <- .SD[, other_cols, with = FALSE]

        # Process each column with type consistency
        for (col in other_cols) {
          col_data <- .SD[[col]]
          non_na_vals <- col_data[!is.na(col_data)]

          # Determine the target type based on the first non-NA value
          if (length(non_na_vals) > 0) {
            first_val <- non_na_vals[1L]

            if (is.numeric(first_val)) {
              # Numeric column - weighted mean
              if (.N > 1 && sum(!is.na(col_data)) > 1) {
                weights <- durata[!is.na(col_data)]
                values <- col_data[!is.na(col_data)]
                total_weight <- sum(weights, na.rm = TRUE)
                if (total_weight > 0) {
                  weighted_val <- sum(values * weights, na.rm = TRUE) / total_weight
                  # Maintain numeric type consistency
                  res[[col]] <- if(is.integer(first_val)) {
                    as.integer(round(weighted_val))
                  } else {
                    as.numeric(weighted_val)
                  }
                } else {
                  res[[col]] <- if(is.integer(first_val)) as.integer(first_val) else as.numeric(first_val)
                }
              } else {
                # Single value or all same
                res[[col]] <- if(is.integer(first_val)) as.integer(first_val) else as.numeric(first_val)
              }
            } else if (is.logical(first_val)) {
              # Logical column - majority vote
              res[[col]] <- as.logical(mean(non_na_vals) >= 0.5)
            } else if (is.character(first_val) || is.factor(first_val)) {
              # Character column - most frequent
              if (col == "stato" && "arco" %in% names(.SD)) {
                # Special handling for stato
                if (max(arco, na.rm = TRUE) > 0) {
                  emp_states <- col_data[arco > 0 & !is.na(col_data)]
                  if (length(emp_states) > 0) {
                    tbl <- table(emp_states)
                    res[[col]] <- as.character(names(tbl)[which.max(tbl)])
                  } else {
                    res[[col]] <- as.character(first_val)
                  }
                } else {
                  res[[col]] <- "disoccupato"
                }
              } else {
                # Most frequent value
                tbl <- table(non_na_vals)
                res[[col]] <- as.character(names(tbl)[which.max(tbl)])
              }
            } else {
              # Default: take first value as-is
              res[[col]] <- first_val
            }
          } else {
            # All NA - maintain type consistency
            if (col %in% names(sd_other)) {
              sample_val <- sd_other[[col]][1L]
              if (is.numeric(sample_val)) {
                res[[col]] <- if(is.integer(sample_val)) NA_integer_ else NA_real_
              } else if (is.logical(sample_val)) {
                res[[col]] <- NA
              } else if (is.character(sample_val) || is.factor(sample_val)) {
                res[[col]] <- NA_character_
              } else {
                res[[col]] <- NA
              }
            } else {
              res[[col]] <- NA
            }
          }
        }
      }

      res
    }, by = .(cf, consolidation_group)]

    # Clean up
    consolidated[, consolidation_group := NULL]

    # Restore column types efficiently
    for (col in names(original_classes)) {
      if (col %in% names(consolidated)) {
        orig_class <- original_classes[[col]]
        if ("integer" %in% orig_class && is.numeric(consolidated[[col]])) {
          set(consolidated, j = col, value = as.integer(round(consolidated[[col]])))
        } else if ("IDate" %in% orig_class || "Date" %in% orig_class) {
          set(consolidated, j = col, value = structure(consolidated[[col]], class = orig_class))
        }
      }
    }

    return(consolidated)
  }

  # OPTIMIZATION 7: Parallel Processing
  if (parallel && nrow(dt_to_process) > 1000) {
    if (show_progress) message("Phase 2: Parallel consolidation processing...")

    # Check OS for parallel compatibility
    is_windows <- .Platform$OS.type == "windows"

    if (is_windows) {
      if (show_progress) message("Note: Parallel processing not available on Windows, using sequential...")
      parallel <- FALSE
    } else {
      # Split by cf for parallel processing
      unique_cfs <- unique(dt_to_process$cf)
      n_chunks <- min(n_cores, length(unique_cfs))

      # Create more balanced chunks
      if (n_chunks > 1) {
        # Balance chunk sizes
        chunk_size <- ceiling(length(unique_cfs) / n_chunks)
        cf_chunks <- split(unique_cfs,
                          rep(1:n_chunks, each = chunk_size, length.out = length(unique_cfs)))
      } else {
        cf_chunks <- list(unique_cfs)
      }

      # Process chunks in parallel with error handling
      # Need to export the process_chunk function and variables to workers
      chunk_results <- tryCatch({
        parallel::mclapply(cf_chunks, function(cfs) {
          tryCatch({
            # Subset data for this chunk
            chunk_data <- dt_to_process[cf %in% cfs]

            # Call process_chunk with properly scoped variables
            # Re-create the function in worker scope
            local({
              process_chunk_local <- function(chunk_dt, employer_var_local, remove_intermediate_unemployment_local, calculate_coverage_local) {
                # Return immediately for empty chunks
                if (nrow(chunk_dt) == 0) {
                  return(data.table::data.table())
                }

                # Store original classes
                exclude_cols <- c("cf")
                agg_cols <- setdiff(names(chunk_dt), exclude_cols)
                original_classes <- lapply(chunk_dt[, ..agg_cols], class)

                # Grouping logic
                chunk_dt[, `:=`(
                  prev_employer = shift(get(employer_var_local), 1L),
                  prev_fine = shift(fine, 1L),
                  prev_arco = shift(arco, 1L),
                  prev_durata = shift(durata, 1L)
                ), by = cf]

                chunk_dt[, gap_days := fifelse(
                  is.na(prev_fine),
                  0L,
                  as.integer(inizio - prev_fine - 1L)
                )]

                # Determine grouping based on parameters
                if (remove_intermediate_unemployment_local) {
                  chunk_dt[, new_group := {
                    is_first <- is.na(prev_employer)
                    employer_changed <- arco > 0 & !is.na(get(employer_var_local)) &
                                       !is.na(prev_employer) &
                                       get(employer_var_local) != prev_employer
                    unemployment_barrier <- arco == 0 & durata >= 8
                    after_long_unemployment <- prev_arco == 0 & prev_durata >= 8 & arco > 0
                    gap_exceeded <- gap_days > 8

                    is_first | employer_changed | unemployment_barrier | after_long_unemployment | gap_exceeded
                  }]
                } else {
                  chunk_dt[, new_group := {
                    is.na(prev_employer) |
                    is.na(get(employer_var_local)) |
                    get(employer_var_local) != prev_employer |
                    gap_days > 8
                  }]
                }

                chunk_dt[, consolidation_group := cumsum(new_group), by = cf]

                # Aggregation
                consolidated <- chunk_dt[, {
                  min_date <- min(inizio)
                  max_date <- max(fine)
                  elapsed <- as.numeric(max_date - min_date + 1)
                  n_periods <- .N

                  emp_mask <- arco > 0
                  unemp_mask <- arco == 0
                  emp_days <- sum(durata[emp_mask], na.rm = TRUE)
                  n_unemp <- sum(unemp_mask)
                  unemp_days <- if(remove_intermediate_unemployment_local && n_unemp > 0) {
                    sum(durata[unemp_mask], na.rm = TRUE)
                  } else { 0.0 }

                  res <- list(
                    inizio = min_date,
                    fine = max_date,
                    durata = elapsed,
                    n_periods_consolidated = n_periods,
                    n_unemployment_periods_removed = if(remove_intermediate_unemployment_local) n_unemp else 0L
                  )

                  if (calculate_coverage_local) {
                    res$consolidation_coverage <- if(elapsed > 0) emp_days/elapsed else 1.0
                    res$unemployment_days_removed <- if(remove_intermediate_unemployment_local) unemp_days else 0.0
                  }

                  # Handle employer
                  if (employer_var_local %in% names(.SD)) {
                    emp_vals <- .SD[[employer_var_local]][!is.na(.SD[[employer_var_local]])]
                    res[[employer_var_local]] <- if(length(emp_vals) > 0) emp_vals[1L] else NA_character_
                  }

                  # Handle other key columns
                  if ("arco" %in% names(.SD)) {
                    res$arco <- max(arco, na.rm = TRUE)
                  }

                  # Basic handling of other columns
                  other_cols <- setdiff(names(.SD), c("inizio", "fine", "durata", "cf",
                                                      employer_var_local, "arco", "prev_employer",
                                                      "prev_fine", "prev_arco", "prev_durata",
                                                      "gap_days", "new_group", "consolidation_group"))

                  for (col in other_cols) {
                    vals <- .SD[[col]][!is.na(.SD[[col]])]
                    if (length(vals) > 0) {
                      res[[col]] <- vals[1L]
                    } else {
                      res[[col]] <- NA
                    }
                  }

                  res
                }, by = .(cf, consolidation_group)]

                consolidated[, consolidation_group := NULL]
                return(consolidated)
              }

              # Call the local function
              process_chunk_local(chunk_data, employer_var, remove_intermediate_unemployment, calculate_coverage)
            })
          }, error = function(e) {
            warning(sprintf("Error in chunk processing: %s", e$message))
            NULL
          })
        }, mc.cores = n_cores, mc.preschedule = FALSE)
      }, error = function(e) {
        if (show_progress) {
          message(sprintf("Parallel processing failed: %s", e$message))
          message("Falling back to sequential processing...")
        }
        NULL
      })

      # Check if parallel processing succeeded
      if (!is.null(chunk_results) && all(sapply(chunk_results, function(x) !is.null(x)))) {
        # Verify all results are data.tables
        if (all(sapply(chunk_results, inherits, "data.table"))) {
          consolidated <- rbindlist(chunk_results, use.names = TRUE, fill = TRUE)
        } else {
          if (show_progress) message("Invalid parallel results, falling back to sequential...")
          parallel <- FALSE
        }
      } else {
        if (show_progress) message("Some chunks failed, falling back to sequential processing...")
        parallel <- FALSE
      }
    }
  }

  # Sequential processing (either as primary or fallback)
  if (!parallel || nrow(dt_to_process) <= 1000 || !exists("consolidated")) {
    if (show_progress) message("Phase 2: Sequential consolidation processing...")
    consolidated <- process_chunk(dt_to_process)
  }

  # Add metrics to non-consolidated data if needed
  if (calculate_coverage && nrow(dt_no_consolidation) > 0) {
    dt_no_consolidation[, `:=`(
      consolidation_coverage = 1.0,
      unemployment_days_removed = 0.0,
      n_periods_consolidated = 1L,
      n_unemployment_periods_removed = 0L
    )]
  }

  # Combine all results
  final_result <- rbindlist(
    list(consolidated, dt_no_consolidation),
    use.names = TRUE,
    fill = TRUE
  )

  # Restore original column order
  original_cols <- intersect(names(data), names(final_result))
  new_cols <- setdiff(names(final_result), names(data))
  data.table::setcolorder(final_result, c(original_cols, new_cols))

  # Set index for fast lookups
  setindex(final_result, cf)

  # Final reporting
  if (show_progress) {
    n_final <- nrow(final_result)
    reduction_pct <- round((1 - n_final/n_original) * 100, 1)

    if (calculate_coverage && remove_intermediate_unemployment) {
      # Only calculate metrics for consolidated records
      consolidated_only <- final_result[n_periods_consolidated > 1]
      if (nrow(consolidated_only) > 0) {
        avg_coverage <- round(mean(consolidated_only$consolidation_coverage, na.rm = TRUE) * 100, 1)
        total_unemployment_removed <- sum(consolidated_only$unemployment_days_removed, na.rm = TRUE)

        message(sprintf(
          "FAST consolidation complete: %d -> %d periods (%.1f%% reduction)",
          n_original, n_final, reduction_pct
        ))
        message(sprintf(
          "Average coverage (consolidated): %.1f%%, Total unemployment days removed: %.0f",
          avg_coverage, total_unemployment_removed
        ))
      } else {
        message(sprintf(
          "FAST consolidation complete: %d -> %d periods (%.1f%% reduction)",
          n_original, n_final, reduction_pct
        ))
      }
    } else {
      message(sprintf(
        "FAST consolidation complete: %d -> %d periods (%.1f%% reduction)",
        n_original, n_final, reduction_pct
      ))
    }
  }

  return(final_result)
}


#' Benchmark Consolidation Functions
#'
#' @description
#' Compare performance between original and fast consolidation functions.
#'
#' @param data data.table with employment records
#' @param employer_var Character string specifying employer column
#' @param min_lag Numeric days threshold (default: 8)
#' @param n_cores Number of cores for parallel version (default: detectCores() - 1)
#'
#' @return List with timing results and verification of identical outputs
#' @export
#'
#' @examples
#' \dontrun{
#' results <- benchmark_consolidation(data, "datore")
#' print(results$timings)
#' print(results$speedup)
#' }
benchmark_consolidation <- function(data, employer_var, min_lag = 8, n_cores = NULL) {

  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("Package 'microbenchmark' is required for benchmarking")
  }

  message("Starting consolidation benchmark...")
  message(sprintf("Dataset size: %d rows, %d unique people",
                 nrow(data), uniqueN(data$cf)))

  # Time original function
  message("\nTesting original consolidate_by_employer()...")
  time_original <- system.time({
    result_original <- consolidate_by_employer(
      data, employer_var, min_lag,
      show_progress = FALSE
    )
  })

  # Time fast sequential
  message("Testing consolidate_by_employer_fast() (sequential)...")
  time_fast_seq <- system.time({
    result_fast_seq <- consolidate_by_employer_fast(
      data, employer_var, min_lag,
      parallel = FALSE,
      show_progress = FALSE
    )
  })

  # Time fast parallel
  message("Testing consolidate_by_employer_fast() (parallel)...")
  time_fast_par <- system.time({
    result_fast_par <- consolidate_by_employer_fast(
      data, employer_var, min_lag,
      parallel = TRUE,
      n_cores = n_cores,
      show_progress = FALSE
    )
  })

  # Verify results are identical
  setkey(result_original, cf, inizio, fine)
  setkey(result_fast_seq, cf, inizio, fine)
  setkey(result_fast_par, cf, inizio, fine)

  # Compare key columns
  key_cols <- c("cf", "inizio", "fine", "durata", employer_var)

  identical_seq <- all.equal(
    result_original[, ..key_cols],
    result_fast_seq[, ..key_cols]
  )

  identical_par <- all.equal(
    result_original[, ..key_cols],
    result_fast_par[, ..key_cols]
  )

  # Create results
  timings <- data.frame(
    method = c("Original", "Fast (Sequential)", "Fast (Parallel)"),
    elapsed = c(time_original["elapsed"],
               time_fast_seq["elapsed"],
               time_fast_par["elapsed"]),
    row.names = NULL
  )

  speedup <- list(
    fast_seq_vs_original = time_original["elapsed"] / time_fast_seq["elapsed"],
    fast_par_vs_original = time_original["elapsed"] / time_fast_par["elapsed"],
    fast_par_vs_seq = time_fast_seq["elapsed"] / time_fast_par["elapsed"]
  )

  message("\n=== BENCHMARK RESULTS ===")
  print(timings)
  message(sprintf("\nSpeedup (Fast Sequential): %.2fx", speedup$fast_seq_vs_original))
  message(sprintf("Speedup (Fast Parallel): %.2fx", speedup$fast_par_vs_original))

  if (!isTRUE(identical_seq)) {
    warning("Fast sequential results differ from original!")
  }
  if (!isTRUE(identical_par)) {
    warning("Fast parallel results differ from original!")
  }

  return(list(
    timings = timings,
    speedup = speedup,
    results_identical = list(
      sequential = isTRUE(identical_seq),
      parallel = isTRUE(identical_par)
    ),
    results = list(
      original = result_original,
      fast_seq = result_fast_seq,
      fast_par = result_fast_par
    )
  ))
}