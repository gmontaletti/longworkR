#' Safe Employment Consolidation - Bulletproof for Large Datasets
#'
#' @description
#' Completely safe version that avoids all data.table aggregation issues
#' by using explicit loops and pre-allocated structures.
#'
#' @param data data.table with employment records
#' @param employer_var Character string specifying employer column name
#' @param min_lag Numeric days threshold (default: 8)
#' @param remove_intermediate_unemployment Logical (default: TRUE)
#' @param calculate_coverage Logical (default: TRUE)
#' @param show_progress Logical (default: TRUE)
#' @param chunk_size Number of people to process at once (default: 5000)
#'
#' @return Consolidated data.table
#' @export
#'
consolidate_by_employer_safe <- function(data,
                                        employer_var,
                                        min_lag = 8,
                                        remove_intermediate_unemployment = TRUE,
                                        calculate_coverage = TRUE,
                                        show_progress = TRUE,
                                        chunk_size = 5000) {

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
    message(sprintf("Starting SAFE consolidation for %d records...", n_original))
  }

  # Basic setup
  required_cols <- c("cf", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Add arco if missing
  if (!"arco" %in% names(dt)) {
    dt[, arco := fifelse(is.na(get(employer_var)) | get(employer_var) == "", 0L, 1L)]
  }

  # Convert dates
  if (!inherits(dt$inizio, "Date")) dt[, inizio := as.Date(inizio)]
  if (!inherits(dt$fine, "Date")) dt[, fine := as.Date(fine)]

  # Set key
  setkey(dt, cf, inizio, fine)

  # Get list of all unique people
  all_people <- unique(dt$cf)
  n_people <- length(all_people)

  if (show_progress) {
    message(sprintf("Processing %d people in chunks of %d...", n_people, chunk_size))
  }

  # Process in chunks
  n_chunks <- ceiling(n_people / chunk_size)
  results <- list()

  for (chunk_i in seq_len(n_chunks)) {
    if (show_progress) {
      progress_pct <- round(100 * (chunk_i - 1) / n_chunks, 1)
      message(sprintf("  Chunk %d/%d (%.1f%% complete)", chunk_i, n_chunks, progress_pct))
    }

    # Get people for this chunk
    start_idx <- (chunk_i - 1) * chunk_size + 1
    end_idx <- min(chunk_i * chunk_size, n_people)
    chunk_people <- all_people[start_idx:end_idx]

    # Get data for these people
    chunk_data <- dt[cf %in% chunk_people]

    # Process each person individually to avoid aggregation issues
    person_results <- list()

    for (person in chunk_people) {
      person_data <- chunk_data[cf == person]

      if (nrow(person_data) == 1) {
        # Single record - no consolidation needed
        result_row <- person_data[1]
        result_row[, n_periods_consolidated := 1L]
        if (calculate_coverage) {
          result_row[, `:=`(
            consolidation_coverage = 1.0,
            unemployment_days_removed = 0.0,
            n_unemployment_periods_removed = 0L
          )]
        }
        person_results[[person]] <- result_row
      } else {
        # Multiple records - consolidate
        person_data <- person_data[order(inizio)]

        # Add grouping variables
        person_data[, `:=`(
          prev_emp = shift(get(employer_var), 1L),
          prev_fine = shift(fine, 1L),
          gap_days = as.integer(inizio - shift(fine, 1L) - 1L)
        )]

        person_data[is.na(gap_days), gap_days := 0L]

        # Create groups
        person_data[, new_group := {
          is.na(prev_emp) |
          (!is.na(get(employer_var)) & !is.na(prev_emp) & get(employer_var) != prev_emp) |
          gap_days > min_lag |
          (remove_intermediate_unemployment & arco == 0 & durata >= min_lag)
        }]

        person_data[, group_id := cumsum(new_group)]

        # Consolidate each group separately
        groups <- unique(person_data$group_id)
        group_results <- list()

        for (g in groups) {
          group_data <- person_data[group_id == g]

          # Manual aggregation - no data.table grouping
          min_date <- min(group_data$inizio)
          max_date <- max(group_data$fine)
          total_duration <- as.numeric(max_date - min_date + 1)

          # Create result row manually
          result_row <- group_data[1]  # Start with first row

          # Override with consolidated values
          result_row[, `:=`(
            inizio = min_date,
            fine = max_date,
            durata = total_duration,
            n_periods_consolidated = nrow(group_data)
          )]

          # Handle employer
          emp_vals <- group_data[[employer_var]][!is.na(group_data[[employer_var]])]
          if (length(emp_vals) > 0) {
            result_row[[employer_var]] <- emp_vals[1]
          }

          # Handle arco
          result_row$arco <- max(group_data$arco, na.rm = TRUE)

          # Optional metrics
          if (calculate_coverage) {
            emp_days <- sum(group_data$durata[group_data$arco > 0], na.rm = TRUE)
            result_row$consolidation_coverage <- if(total_duration > 0) emp_days/total_duration else 1.0

            if (remove_intermediate_unemployment) {
              unemp_days <- sum(group_data$durata[group_data$arco == 0], na.rm = TRUE)
              result_row$unemployment_days_removed <- unemp_days
              result_row$n_unemployment_periods_removed <- sum(group_data$arco == 0)
            } else {
              result_row$unemployment_days_removed <- 0.0
              result_row$n_unemployment_periods_removed <- 0L
            }
          }

          # Handle other columns - take most common or first
          other_cols <- setdiff(names(group_data), c("cf", "inizio", "fine", "durata",
                                                     employer_var, "arco", "prev_emp",
                                                     "prev_fine", "gap_days", "new_group",
                                                     "group_id"))

          for (col in other_cols) {
            vals <- group_data[[col]][!is.na(group_data[[col]])]
            if (length(vals) > 0) {
              if (is.character(vals) || is.factor(vals)) {
                # Most common for character
                tbl <- table(vals)
                result_row[[col]] <- names(tbl)[which.max(tbl)]
              } else if (is.numeric(vals)) {
                # Weighted mean for numeric
                weights <- group_data$durata[!is.na(group_data[[col]])]
                result_row[[col]] <- weighted.mean(vals, weights, na.rm = TRUE)
              } else {
                # First for others
                result_row[[col]] <- vals[1]
              }
            }
          }

          # Clean up temporary columns
          temp_cols <- c("prev_emp", "prev_fine", "gap_days", "new_group", "group_id")
          for (temp_col in temp_cols) {
            if (temp_col %in% names(result_row)) {
              result_row[[temp_col]] <- NULL
            }
          }

          group_results[[as.character(g)]] <- result_row
        }

        # Combine groups for this person
        person_results[[person]] <- rbindlist(group_results, use.names = TRUE, fill = TRUE)
      }
    }

    # Combine all people in this chunk
    chunk_result <- rbindlist(person_results, use.names = TRUE, fill = TRUE)
    results[[chunk_i]] <- chunk_result

    # Garbage collection every few chunks
    if (chunk_i %% 5 == 0) {
      gc(verbose = FALSE)
    }
  }

  # Combine all chunks
  if (show_progress) message("Combining all results...")
  final_result <- rbindlist(results, use.names = TRUE, fill = TRUE)

  # Restore original column order
  original_cols <- intersect(names(data), names(final_result))
  new_cols <- setdiff(names(final_result), names(data))

  # Only reorder if columns exist
  if (length(original_cols) > 0) {
    setcolorder(final_result, c(original_cols, new_cols))
  }

  # Set index
  if ("cf" %in% names(final_result)) {
    setindex(final_result, cf)
  }

  if (show_progress) {
    n_final <- nrow(final_result)
    reduction_pct <- round((1 - n_final/n_original) * 100, 1)
    message(sprintf("SAFE consolidation complete: %d -> %d records (%.1f%% reduction)",
                   n_original, n_final, reduction_pct))
  }

  return(final_result)
}