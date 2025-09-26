#' Ultra-Fast Employment Consolidation for Very Large Datasets
#'
#' @description
#' Simplified high-performance version optimized for very large datasets (>10M records).
#' Uses sequential processing with aggressive memory optimization and minimal overhead.
#'
#' @param data data.table with employment records
#' @param employer_var Character string specifying employer column name
#' @param min_lag Numeric days threshold (default: 8)
#' @param remove_intermediate_unemployment Logical (default: TRUE)
#' @param calculate_coverage Logical (default: TRUE)
#' @param show_progress Logical (default: TRUE)
#' @param chunk_size Number of people to process at once (default: 10000)
#'
#' @return Consolidated data.table
#' @export
#'
consolidate_by_employer_ultra_fast <- function(data,
                                               employer_var,
                                               min_lag = 8,
                                               remove_intermediate_unemployment = TRUE,
                                               calculate_coverage = TRUE,
                                               show_progress = TRUE,
                                               chunk_size = 10000) {

  # Input validation
  if (!inherits(data, "data.table")) {
    stop("data must be a data.table object")
  }

  if (is.null(employer_var) || !employer_var %in% names(data)) {
    stop("employer_var must specify a valid column name in data")
  }

  # Create working copy
  dt <- copy(data)
  n_original <- nrow(dt)

  if (show_progress) {
    message(sprintf("Starting ULTRA-FAST consolidation for %d records...", n_original))
  }

  # Ensure required columns
  required_cols <- c("cf", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Add arco if missing
  if (!"arco" %in% names(dt)) {
    dt[, arco := fifelse(is.na(get(employer_var)) | get(employer_var) == "", 0L, 1L)]
  }

  # Convert dates if needed
  if (!inherits(dt$inizio, "Date")) dt[, inizio := as.Date(inizio)]
  if (!inherits(dt$fine, "Date")) dt[, fine := as.Date(fine)]

  # Set key for optimal performance
  setkey(dt, cf, inizio, fine)

  # PHASE 1: Quick candidate identification
  if (show_progress) message("Phase 1: Identifying consolidation candidates...")

  # Use simplified logic for speed
  candidates_summary <- dt[, .(
    n_records = .N,
    n_unique_employers = uniqueN(get(employer_var)[!is.na(get(employer_var))])
  ), keyby = cf]

  # Only process people with multiple records
  candidate_cfs <- candidates_summary[n_records > 1]$cf

  if (show_progress) {
    message(sprintf("Found %d candidates (%.1f%% of people)",
                   length(candidate_cfs),
                   100 * length(candidate_cfs) / uniqueN(dt$cf)))
  }

  # Split into those needing and not needing consolidation
  dt_to_process <- dt[cf %in% candidate_cfs]
  dt_no_consolidation <- dt[!cf %in% candidate_cfs]

  if (nrow(dt_to_process) == 0) {
    if (show_progress) message("No consolidation needed")
    return(dt)
  }

  # PHASE 2: Chunked sequential processing
  if (show_progress) {
    message(sprintf("Phase 2: Processing in chunks of %d people...", chunk_size))
  }

  # Process in chunks to manage memory
  unique_cfs <- unique(dt_to_process$cf)
  n_chunks <- ceiling(length(unique_cfs) / chunk_size)

  results <- list()

  for (i in seq_len(n_chunks)) {
    if (show_progress && n_chunks > 1) {
      message(sprintf("  Processing chunk %d/%d (%.0f%% complete)...",
                     i, n_chunks, 100 * (i-1) / n_chunks))
    }

    # Get chunk of CFs
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, length(unique_cfs))
    chunk_cfs <- unique_cfs[start_idx:end_idx]

    # Process chunk
    chunk_data <- dt_to_process[cf %in% chunk_cfs]

    # Simple consolidation logic
    chunk_data[, `:=`(
      prev_employer = shift(get(employer_var), 1L),
      prev_fine = shift(fine, 1L),
      prev_arco = shift(arco, 1L),
      gap_days = as.integer(inizio - shift(fine, 1L) - 1L)
    ), by = cf]

    # Set NA gaps to 0
    chunk_data[is.na(gap_days), gap_days := 0L]

    # Grouping logic
    if (remove_intermediate_unemployment) {
      chunk_data[, new_group := {
        is_first <- is.na(prev_employer)
        employer_changed <- !is.na(get(employer_var)) & !is.na(prev_employer) &
                           get(employer_var) != prev_employer & arco > 0
        long_unemployment <- arco == 0 & durata >= min_lag
        gap_exceeded <- gap_days > min_lag

        is_first | employer_changed | long_unemployment | gap_exceeded
      }]
    } else {
      chunk_data[, new_group := {
        is.na(prev_employer) |
        is.na(get(employer_var)) |
        get(employer_var) != prev_employer |
        gap_days > min_lag
      }]
    }

    chunk_data[, group_id := cumsum(new_group), by = cf]

    # Aggregate by group - simplified for speed
    chunk_consolidated <- chunk_data[, {
      # Core dates and duration
      min_date <- min(inizio)
      max_date <- max(fine)
      total_days <- as.numeric(max_date - min_date + 1)

      # Employment metrics
      emp_mask <- arco > 0
      emp_days <- sum(durata[emp_mask], na.rm = TRUE)
      n_unemp <- sum(arco == 0)

      # Build result
      result <- list(
        inizio = min_date,
        fine = max_date,
        durata = total_days,
        n_periods_consolidated = .N
      )

      # Optional metrics
      if (calculate_coverage) {
        result$consolidation_coverage <- if(total_days > 0) emp_days/total_days else 1.0
        result$unemployment_days_removed <- if(remove_intermediate_unemployment & n_unemp > 0) {
          sum(durata[arco == 0], na.rm = TRUE)
        } else { 0.0 }
        result$n_unemployment_periods_removed <- if(remove_intermediate_unemployment) n_unemp else 0L
      }

      # Employer (take first non-NA)
      if (employer_var %in% names(.SD)) {
        emp_vals <- get(employer_var)[!is.na(get(employer_var))]
        result[[employer_var]] <- if(length(emp_vals) > 0) emp_vals[1L] else NA_character_
      }

      # Arco
      result$arco <- max(arco, na.rm = TRUE)

      # Other important columns - take most common or first
      for (col in setdiff(names(.SD), c("inizio", "fine", "durata", "cf", employer_var,
                                        "arco", "prev_employer", "prev_fine", "prev_arco",
                                        "gap_days", "new_group", "group_id"))) {
        vals <- get(col)[!is.na(get(col))]
        if (length(vals) > 0) {
          # For character columns, take most common
          if (is.character(vals)) {
            tbl <- table(vals)
            result[[col]] <- names(tbl)[which.max(tbl)]
          } else {
            # For numeric, take weighted mean or first
            result[[col]] <- if(.N > 1 && is.numeric(vals)) {
              weighted.mean(vals[1:min(length(vals), .N)],
                          durata[1:min(length(vals), .N)],
                          na.rm = TRUE)
            } else {
              vals[1L]
            }
          }
        } else {
          result[[col]] <- NA
        }
      }

      result
    }, by = .(cf, group_id)]

    # Remove group_id column
    chunk_consolidated[, group_id := NULL]

    # Store result
    results[[i]] <- chunk_consolidated

    # Clear memory periodically
    if (i %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }

  # Combine all chunks
  if (show_progress) message("Phase 3: Combining results...")
  consolidated <- rbindlist(results, use.names = TRUE, fill = TRUE)

  # Add metrics to non-consolidated if needed
  if (calculate_coverage && nrow(dt_no_consolidation) > 0) {
    dt_no_consolidation[, `:=`(
      consolidation_coverage = 1.0,
      n_periods_consolidated = 1L
    )]
    if (remove_intermediate_unemployment) {
      dt_no_consolidation[, `:=`(
        unemployment_days_removed = 0.0,
        n_unemployment_periods_removed = 0L
      )]
    }
  }

  # Combine all data
  final_result <- rbindlist(
    list(consolidated, dt_no_consolidation),
    use.names = TRUE,
    fill = TRUE
  )

  # Restore column order
  original_cols <- intersect(names(data), names(final_result))
  new_cols <- setdiff(names(final_result), names(data))
  setcolorder(final_result, c(original_cols, new_cols))

  # Set index
  setindex(final_result, cf)

  # Final reporting
  if (show_progress) {
    n_final <- nrow(final_result)
    reduction_pct <- round((1 - n_final/n_original) * 100, 1)

    message(sprintf("ULTRA-FAST consolidation complete: %d -> %d records (%.1f%% reduction)",
                   n_original, n_final, reduction_pct))

    if (calculate_coverage) {
      consolidated_records <- final_result[n_periods_consolidated > 1]
      if (nrow(consolidated_records) > 0) {
        avg_coverage <- mean(consolidated_records$consolidation_coverage, na.rm = TRUE)
        message(sprintf("Average coverage for consolidated periods: %.1f%%",
                       100 * avg_coverage))
      }
    }
  }

  return(final_result)
}