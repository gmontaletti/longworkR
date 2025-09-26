#' Robust Employment Consolidation for Large Datasets with Type Safety
#'
#' @description
#' Bulletproof version with explicit type handling to avoid aggregation errors.
#' Optimized for large datasets with mixed data types.
#'
#' @param data data.table with employment records
#' @param employer_var Character string specifying employer column name
#' @param min_lag Numeric days threshold (default: 8)
#' @param remove_intermediate_unemployment Logical (default: TRUE)
#' @param calculate_coverage Logical (default: TRUE)
#' @param show_progress Logical (default: TRUE)
#'
#' @return Consolidated data.table with consistent types
#' @export
#'
consolidate_by_employer_robust <- function(data,
                                          employer_var,
                                          min_lag = 8,
                                          remove_intermediate_unemployment = TRUE,
                                          calculate_coverage = TRUE,
                                          show_progress = TRUE) {

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
    message(sprintf("Starting ROBUST consolidation for %d records...", n_original))
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

  # CRITICAL: Store original column types for consistency
  all_cols <- names(dt)
  col_types <- sapply(dt, function(x) {
    if (is.integer(x)) return("integer")
    if (is.numeric(x)) return("numeric")
    if (is.logical(x)) return("logical")
    if (is.character(x)) return("character")
    if (is.factor(x)) return("factor")
    if (inherits(x, "Date")) return("Date")
    return("other")
  })

  # Set key
  setkey(dt, cf, inizio, fine)

  # PHASE 1: Identify candidates
  if (show_progress) message("Phase 1: Identifying consolidation candidates...")

  candidates <- dt[, .(n = .N), by = cf][n > 1]$cf

  if (show_progress) {
    message(sprintf("Found %d candidates (%.1f%% of people)",
                   length(candidates), 100 * length(candidates) / uniqueN(dt$cf)))
  }

  dt_to_process <- dt[cf %in% candidates]
  dt_no_consolidation <- dt[!cf %in% candidates]

  if (nrow(dt_to_process) == 0) {
    if (show_progress) message("No consolidation needed")
    return(dt)
  }

  # PHASE 2: Grouping
  if (show_progress) message("Phase 2: Creating consolidation groups...")

  dt_to_process[, `:=`(
    prev_emp = shift(get(employer_var), 1L),
    prev_fine = shift(fine, 1L),
    prev_arco = shift(arco, 1L),
    prev_durata = shift(durata, 1L)
  ), by = cf]

  dt_to_process[, gap := fifelse(is.na(prev_fine), 0L,
                                 as.integer(inizio - prev_fine - 1L))]

  # Simple grouping
  dt_to_process[, new_group := {
    is.na(prev_emp) |
    (!is.na(get(employer_var)) & !is.na(prev_emp) & get(employer_var) != prev_emp) |
    gap > min_lag |
    (remove_intermediate_unemployment & arco == 0 & durata >= min_lag)
  }]

  dt_to_process[, group_id := cumsum(new_group), by = cf]

  # PHASE 3: Aggregation with type safety
  if (show_progress) message("Phase 3: Consolidating with type safety...")

  # Define aggregation functions for each column type
  agg_functions <- list()

  # Always aggregate these core columns
  agg_functions[["inizio"]] <- function(x) min(x, na.rm = TRUE)
  agg_functions[["fine"]] <- function(x) max(x, na.rm = TRUE)
  agg_functions[["durata"]] <- function(x, start, end) as.numeric(end - start + 1)
  agg_functions[["arco"]] <- function(x) as.integer(max(x, na.rm = TRUE))

  # Build aggregation expression with explicit type casting
  agg_expr <- quote({
    # Core dates
    .inizio <- min(inizio, na.rm = TRUE)
    .fine <- max(fine, na.rm = TRUE)
    .durata <- as.numeric(.fine - .inizio + 1)

    # Build result list
    result <- list(
      inizio = .inizio,
      fine = .fine,
      durata = .durata,
      n_periods_consolidated = as.integer(.N)
    )

    # Employment metrics
    if (calculate_coverage) {
      emp_days <- sum(durata[arco > 0], na.rm = TRUE)
      result$consolidation_coverage <- as.numeric(if(.durata > 0) emp_days/.durata else 1.0)
      if (remove_intermediate_unemployment) {
        result$unemployment_days_removed <- as.numeric(sum(durata[arco == 0], na.rm = TRUE))
        result$n_unemployment_periods_removed <- as.integer(sum(arco == 0))
      }
    }

    # Handle employer
    emp_vals <- get(employer_var)[!is.na(get(employer_var))]
    result[[employer_var]] <- if(length(emp_vals) > 0) {
      as.character(emp_vals[1L])
    } else {
      NA_character_
    }

    # Handle arco
    result$arco <- as.integer(max(arco, na.rm = TRUE))

    # Other columns - maintain type consistency
    other_cols <- setdiff(names(.SD), c("inizio", "fine", "durata", "cf",
                                        employer_var, "arco", "prev_emp",
                                        "prev_fine", "prev_arco", "prev_durata",
                                        "gap", "new_group", "group_id"))

    for (col in other_cols) {
      vals <- get(col)
      non_na <- vals[!is.na(vals)]

      if (length(non_na) > 0) {
        # Use the predefined type
        col_type <- col_types[col]

        if (col_type == "integer") {
          # For integer: take first or most common
          result[[col]] <- as.integer(non_na[1L])
        } else if (col_type == "numeric") {
          # For numeric: weighted mean
          if (.N > 1) {
            w <- durata[!is.na(vals)]
            v <- vals[!is.na(vals)]
            result[[col]] <- as.numeric(weighted.mean(v, w, na.rm = TRUE))
          } else {
            result[[col]] <- as.numeric(non_na[1L])
          }
        } else if (col_type == "logical") {
          # For logical: majority
          result[[col]] <- as.logical(mean(non_na) >= 0.5)
        } else if (col_type %in% c("character", "factor")) {
          # For character/factor: most common
          tbl <- table(non_na)
          result[[col]] <- as.character(names(tbl)[which.max(tbl)])
        } else if (col_type == "Date") {
          # For dates: first
          result[[col]] <- non_na[1L]
        } else {
          # Other: first value
          result[[col]] <- non_na[1L]
        }
      } else {
        # All NA - use appropriate NA type
        col_type <- col_types[col]

        if (col_type == "integer") {
          result[[col]] <- NA_integer_
        } else if (col_type == "numeric") {
          result[[col]] <- NA_real_
        } else if (col_type == "logical") {
          result[[col]] <- NA
        } else if (col_type %in% c("character", "factor")) {
          result[[col]] <- NA_character_
        } else {
          result[[col]] <- NA
        }
      }
    }

    result
  })

  # Execute aggregation
  consolidated <- dt_to_process[, eval(agg_expr), by = .(cf, group_id)]

  # Remove group_id
  consolidated[, group_id := NULL]

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

  # Combine results
  if (show_progress) message("Phase 4: Combining results...")
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

  if (show_progress) {
    n_final <- nrow(final_result)
    reduction_pct <- round((1 - n_final/n_original) * 100, 1)
    message(sprintf("ROBUST consolidation complete: %d -> %d records (%.1f%% reduction)",
                   n_original, n_final, reduction_pct))
  }

  return(final_result)
}