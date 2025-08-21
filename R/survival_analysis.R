#' Survival Analysis for Contract Types
#'
#' @description
#' This module provides survival analysis functionality for employment contracts,
#' computing survival curves, median durations, and survival probabilities by
#' contract type. Handles right-censoring at observation end (max FINE date).
#'
#' @importFrom data.table data.table setDT copy := .SD .N
#' @importFrom survival Surv survfit
#' @importFrom stats quantile
#' @name survival_analysis
NULL

#' Add Contract Survival Metrics to Employment Data
#'
#' @description
#' Enhances employment data with survival analysis metrics including survival time,
#' censoring indicators, median survival times, and confidence intervals for each contract type.
#'
#' @param data A data.table from vecshift() containing employment segments
#' @param contract_type_var Character. Name of the contract type variable 
#'   (default: "COD_TIPOLOGIA_CONTRATTUALE")
#' @param id_var Character. Person identifier variable (default: "cf")
#' @param start_var Character. Contract start date variable (default: "INIZIO")
#' @param end_var Character. Contract end date variable (default: "FINE")
#' @param confidence_level Numeric. Confidence level for survival estimates (default: 0.95)
#'
#' @return Enhanced data.table with additional columns:
#'   \itemize{
#'     \item{\code{survival_time}}: Duration or time to censoring
#'     \item{\code{censored}}: 1 if censored at max(FINE), 0 otherwise
#'     \item{\code{contract_type_median}}: Median survival time for contract type (accounting for censoring)
#'     \item{\code{median_ci_lower}}: Lower bound of 95% confidence interval for median survival
#'     \item{\code{median_ci_upper}}: Upper bound of 95% confidence interval for median survival
#'     \item{\code{survival_prob}}: Current survival probability at observed duration
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Process employment data with vecshift
#' processed_data <- vecshift(employment_data)
#' 
#' # Add contract type information
#' processed_data[, COD_TIPOLOGIA_CONTRATTUALE := contract_codes]
#' 
#' # Add survival metrics
#' survival_data <- add_contract_survival_metrics(
#'   data = processed_data,
#'   contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE"
#' )
#' }
add_contract_survival_metrics <- function(
    data,
    contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
    id_var = "cf",
    start_var = "INIZIO",
    end_var = "FINE",
    confidence_level = 0.95
) {
  
  # Input validation
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }
  
  # Check required columns
  required_cols <- c(id_var, start_var, end_var)
  if (!contract_type_var %in% names(data)) {
    stop(sprintf("Contract type variable '%s' not found in data", contract_type_var))
  }
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Create a copy to avoid modifying original data
  result <- copy(data)
  
  # Ensure date columns are Date type
  if (!inherits(result[[start_var]], "Date")) {
    result[, (start_var) := as.Date(get(start_var))]
  }
  if (!inherits(result[[end_var]], "Date")) {
    result[, (end_var) := as.Date(get(end_var))]
  }
  
  # Calculate max observation date for censoring
  max_date <- result[, max(get(end_var), na.rm = TRUE)]
  
  # Calculate survival time and censoring indicator
  result[, `:=`(
    duration = as.numeric(get(end_var) - get(start_var) + 1),
    censored = as.integer(get(end_var) == max_date),
    survival_time = as.numeric(get(end_var) - get(start_var) + 1)
  )]
  
  # Calculate survival curves by contract type (optimized)
  survival_curves <- estimate_contract_survival_optimized(
    data = result,
    contract_type_var = contract_type_var,
    duration_var = "survival_time",
    censored_var = "censored",
    confidence_level = confidence_level
  )
  
  # Add all survival metrics using optimized vectorized operations
  result <- add_all_survival_metrics_optimized(
    data = result,
    survival_curves = survival_curves,
    contract_type_var = contract_type_var,
    duration_var = "survival_time"
  )
  
  return(result)
}

#' Estimate Contract Survival Curves
#'
#' @description
#' Calculates Kaplan-Meier survival curves for each contract type,
#' handling right-censoring appropriately.
#'
#' @param data A data.table with contract information
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#' @param censored_var Character. Censoring indicator variable name
#' @param confidence_level Numeric. Confidence level for estimates
#'
#' @return List containing:
#'   \itemize{
#'     \item{\code{survival_fits}}: survfit objects by contract type
#'     \item{\code{median_survival}}: Named vector of median survival times
#'     \item{\code{survival_tables}}: Survival probability tables
#'     \item{\code{confidence_intervals}}: CI for survival estimates
#'   }
#'
#' @export
estimate_contract_survival <- function(
    data,
    contract_type_var,
    duration_var = "survival_time",
    censored_var = "censored",
    confidence_level = 0.95
) {
  
  # Validate inputs
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for this function. Please install it.")
  }
  
  # Input validation
  if (nrow(data) == 0) {
    stop("Input data is empty")
  }
  
  # Check required columns exist
  required_cols <- c(contract_type_var, duration_var, censored_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Get unique contract types
  contract_types <- unique(data[[contract_type_var]])
  contract_types <- contract_types[!is.na(contract_types)]
  
  if (length(contract_types) == 0) {
    stop("No valid contract types found in data")
  }
  
  # Initialize results storage
  survival_fits <- list()
  median_survival <- numeric()
  survival_tables <- list()
  confidence_intervals <- list()
  
  # Calculate survival curves for each contract type
  for (ct in contract_types) {
    # Subset data for this contract type
    ct_data <- data[get(contract_type_var) == ct]
    
    # Handle edge cases
    if (nrow(ct_data) == 0) {
      warning(sprintf("No data available for contract type '%s'. Skipping.", ct))
      next
    }
    
    # Check for invalid duration values
    invalid_duration <- is.na(ct_data[[duration_var]]) | ct_data[[duration_var]] <= 0
    if (any(invalid_duration)) {
      warning(sprintf("Contract type '%s' has %d invalid duration values. Removing them.", 
                     ct, sum(invalid_duration)))
      ct_data <- ct_data[!invalid_duration]
      
      if (nrow(ct_data) == 0) {
        warning(sprintf("No valid data remaining for contract type '%s' after removing invalid durations. Skipping.", ct))
        next
      }
    }
    
    # Create survival object
    surv_obj <- survival::Surv(
      time = ct_data[[duration_var]],
      event = 1 - ct_data[[censored_var]]  # 1 for event, 0 for censored
    )
    
    # Fit Kaplan-Meier curve with error handling
    km_fit <- tryCatch({
      survival::survfit(
        surv_obj ~ 1,
        conf.int = confidence_level,
        conf.type = "log-log"
      )
    }, error = function(e) {
      warning(sprintf("Failed to fit survival curve for contract type '%s': %s", ct, e$message))
      return(NULL)
    })
    
    # Skip if fitting failed
    if (is.null(km_fit)) {
      next
    }
    
    # Store results
    survival_fits[[ct]] <- km_fit
    
    # Extract median survival time using proper survival analysis methods
    median_survival[ct] <- tryCatch({
      # Use quantile function to extract median from survival curve
      median_result <- quantile(km_fit, probs = 0.5)$quantile
      
      # Handle case where median cannot be estimated (>50% censored)
      if (is.na(median_result) || is.infinite(median_result)) {
        # Check if survival curve drops below 0.5
        min_survival <- min(km_fit$surv, na.rm = TRUE)
        censoring_rate <- mean(ct_data[[censored_var]])
        
        if (min_survival > 0.5) {
          # More than 50% censored - median cannot be estimated
          message(sprintf("Contract type '%s': Median survival time cannot be estimated (min survival probability: %.3f > 0.5, censoring rate: %.1f%%)", 
                         ct, min_survival, censoring_rate * 100))
          NA_real_
        } else {
          # Manually find median using survival table
          surv_below_half <- which(km_fit$surv <= 0.5)
          if (length(surv_below_half) > 0) {
            median_manual <- km_fit$time[surv_below_half[1]]
            message(sprintf("Contract type '%s': Median estimated using manual calculation (%.2f days)", ct, median_manual))
            median_manual
          } else {
            NA_real_
          }
        }
      } else {
        # Valid median result from quantile function
        message(sprintf("Contract type '%s': Median survival time estimated at %.2f days", ct, median_result))
        median_result
      }
    }, error = function(e) {
      # If quantile function fails, try manual calculation
      tryCatch({
        # Find first time where survival drops to 0.5 or below
        surv_below_half <- which(km_fit$surv <= 0.5)
        if (length(surv_below_half) > 0) {
          median_manual <- km_fit$time[surv_below_half[1]]
          warning(sprintf("Contract type '%s': Quantile function failed, using manual calculation (%.2f days): %s", ct, median_manual, e$message))
          median_manual
        } else {
          # More than 50% censored
          censoring_rate <- mean(ct_data[[censored_var]])
          warning(sprintf("Contract type '%s': Cannot estimate median (censoring rate: %.1f%%, quantile error: %s)", ct, censoring_rate * 100, e$message))
          NA_real_
        }
      }, error = function(e2) {
        # Complete fallback
        warning(sprintf("Unable to estimate median for contract type '%s': %s", ct, e2$message))
        NA_real_
      })
    })
    
    # Create survival probability table
    survival_tables[[ct]] <- data.table(
      time = km_fit$time,
      survival_prob = km_fit$surv,
      std_error = km_fit$std.err,
      lower_ci = km_fit$lower,
      upper_ci = km_fit$upper,
      n_risk = km_fit$n.risk,
      n_event = km_fit$n.event
    )
    
    # Store confidence intervals for median with proper extraction
    confidence_intervals[[ct]] <- tryCatch({
      # Use quantile function to get median confidence intervals
      median_ci <- quantile(km_fit, probs = 0.5)
      list(
        median = if (is.na(median_ci$quantile)) NA_real_ else median_ci$quantile,
        lower = if (is.na(median_ci$lower)) NA_real_ else median_ci$lower,
        upper = if (is.na(median_ci$upper)) NA_real_ else median_ci$upper
      )
    }, error = function(e) {
      # Fallback to using stored median value
      list(
        median = median_survival[ct],
        lower = NA_real_,
        upper = NA_real_
      )
    })
  }
  
  # Final validation
  if (length(survival_fits) == 0) {
    warning("No survival curves could be computed for any contract type")
    # Return empty but valid structure
    return(list(
      survival_fits = list(),
      median_survival = numeric(0),
      survival_tables = list(),
      confidence_intervals = list()
    ))
  }
  
  return(list(
    survival_fits = survival_fits,
    median_survival = median_survival,
    survival_tables = survival_tables,
    confidence_intervals = confidence_intervals
  ))
}

#' Optimized Contract Survival Estimation 
#'
#' @description
#' High-performance version of estimate_contract_survival with data.table optimizations
#' and caching for improved performance on large datasets.
#'
#' @param data A data.table with contract information
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#' @param censored_var Character. Censoring indicator variable name
#' @param confidence_level Numeric. Confidence level for estimates
#'
#' @return List containing survival analysis results with pre-computed lookup tables
#'
#' @export
estimate_contract_survival_optimized <- function(
    data,
    contract_type_var,
    duration_var = "survival_time",
    censored_var = "censored",
    confidence_level = 0.95
) {
  
  # Validate inputs
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for this function. Please install it.")
  }
  
  if (nrow(data) == 0) {
    stop("Input data is empty")
  }
  
  # Check required columns exist
  required_cols <- c(contract_type_var, duration_var, censored_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Get unique contract types efficiently
  contract_types <- data[!is.na(get(contract_type_var)), unique(get(contract_type_var))]
  
  if (length(contract_types) == 0) {
    stop("No valid contract types found in data")
  }
  
  # Initialize results storage
  survival_fits <- list()
  median_survival <- numeric()
  survival_tables <- list()
  confidence_intervals <- list()
  lookup_tables <- list()  # New: pre-computed lookup tables
  
  # Process each contract type
  for (ct in contract_types) {
    # Efficient subsetting using data.table
    ct_data <- data[get(contract_type_var) == ct & 
                   !is.na(get(duration_var)) & 
                   get(duration_var) > 0]
    
    if (nrow(ct_data) == 0) {
      warning(sprintf("No valid data for contract type '%s'. Skipping.", ct))
      next
    }
    
    # Create survival object
    surv_obj <- survival::Surv(
      time = ct_data[[duration_var]],
      event = 1 - ct_data[[censored_var]]
    )
    
    # Fit Kaplan-Meier curve with error handling
    km_fit <- tryCatch({
      survival::survfit(
        surv_obj ~ 1,
        conf.int = confidence_level,
        conf.type = "log-log"
      )
    }, error = function(e) {
      warning(sprintf("Failed to fit survival curve for contract type '%s': %s", ct, e$message))
      return(NULL)
    })
    
    if (is.null(km_fit)) {
      next
    }
    
    # Store survival fit
    survival_fits[[ct]] <- km_fit
    
    # Extract median survival time
    median_survival[ct] <- tryCatch({
      median_result <- quantile(km_fit, probs = 0.5)$quantile
      
      if (is.na(median_result) || is.infinite(median_result)) {
        min_survival <- min(km_fit$surv, na.rm = TRUE)
        if (min_survival > 0.5) {
          NA_real_
        } else {
          surv_below_half <- which(km_fit$surv <= 0.5)
          if (length(surv_below_half) > 0) {
            km_fit$time[surv_below_half[1]]
          } else {
            NA_real_
          }
        }
      } else {
        median_result
      }
    }, error = function(e) {
      NA_real_
    })
    
    # Create survival probability table
    surv_table <- data.table(
      time = km_fit$time,
      survival_prob = km_fit$surv,
      std_error = km_fit$std.err,
      lower_ci = km_fit$lower,
      upper_ci = km_fit$upper,
      n_risk = km_fit$n.risk,
      n_event = km_fit$n.event
    )
    survival_tables[[ct]] <- surv_table
    
    # Create optimized lookup table for fast probability retrieval
    # This pre-computes survival probabilities for all unique durations
    unique_times <- data[get(contract_type_var) == ct, unique(get(duration_var))]
    unique_times <- unique_times[!is.na(unique_times) & unique_times > 0]
    
    if (length(unique_times) > 0) {
      lookup_dt <- data.table(
        time = unique_times,
        survival_prob = sapply(unique_times, function(t) {
          get_survival_at_time(surv_table, t)  # Use original function for consistency
        })
      )
      setkey(lookup_dt, time)  # Set key for fast lookups
      lookup_tables[[ct]] <- lookup_dt
    }
    
    # Store confidence intervals
    confidence_intervals[[ct]] <- tryCatch({
      median_ci <- quantile(km_fit, probs = 0.5)
      list(
        median = if (is.na(median_ci$quantile)) NA_real_ else median_ci$quantile,
        lower = if (is.na(median_ci$lower)) NA_real_ else median_ci$lower,
        upper = if (is.na(median_ci$upper)) NA_real_ else median_ci$upper
      )
    }, error = function(e) {
      list(
        median = median_survival[ct],
        lower = NA_real_,
        upper = NA_real_
      )
    })
  }
  
  # Final validation
  if (length(survival_fits) == 0) {
    warning("No survival curves could be computed for any contract type")
    return(list(
      survival_fits = list(),
      median_survival = numeric(0),
      survival_tables = list(),
      confidence_intervals = list(),
      lookup_tables = list()
    ))
  }
  
  return(list(
    survival_fits = survival_fits,
    median_survival = median_survival,
    survival_tables = survival_tables,
    confidence_intervals = confidence_intervals,
    lookup_tables = lookup_tables  # New: pre-computed lookup tables
  ))
}

#' Optimized Helper: Get Survival Probability at Specific Time
#'
#' @description
#' Optimized version using data.table's fast binary search for probability lookup.
#'
#' @param surv_table A data.table with time and survival_prob columns
#' @param time Numeric. Time point to evaluate
#'
#' @return Numeric. Survival probability at specified time
#'
#' @keywords internal
get_survival_at_time_optimized <- function(surv_table, query_time) {
  if (nrow(surv_table) == 0) return(NA_real_)
  if (query_time <= 0) return(1.0)
  
  # Find the latest time point not exceeding the requested time
  # This should match the original get_survival_at_time logic
  valid_rows <- surv_table[time <= query_time]
  
  if (nrow(valid_rows) == 0) {
    return(1.0)  # Before first event
  } else {
    return(valid_rows[.N, survival_prob])  # Return survival prob from last valid row
  }
}

#' Add All Survival Metrics Using Optimized Vectorized Operations
#'
#' @description
#' High-performance implementation that adds all survival metrics using vectorized
#' data.table operations and pre-computed lookup tables instead of loops.
#' Adds median survival times with confidence intervals and current survival probabilities.
#'
#' @param data A data.table with contract information
#' @param survival_curves List output from estimate_contract_survival_optimized
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#' @param calculate_median_prob Logical. DEPRECATED - parameter ignored (for backward compatibility)
#'
#' @return Enhanced data.table with survival metrics:
#'   \itemize{
#'     \item{\code{contract_type_median}}: Median survival time for contract type
#'     \item{\code{median_ci_lower}}: Lower bound of 95% confidence interval
#'     \item{\code{median_ci_upper}}: Upper bound of 95% confidence interval
#'     \item{\code{survival_prob}}: Current survival probability at observed duration
#'   }
#'
#' @export
add_all_survival_metrics_optimized <- function(
    data,
    survival_curves,
    contract_type_var,
    duration_var = "survival_time",
    calculate_median_prob = FALSE  # Remove broken median prob calculation
) {
  
  # Create a copy to avoid modifying original
  result <- copy(data)
  
  # Initialize all columns including confidence intervals
  result[, `:=`(
    contract_type_median = NA_real_,
    median_ci_lower = NA_real_,
    median_ci_upper = NA_real_,
    survival_prob = NA_real_
  )]
  
  # Extract median survival times and confidence intervals efficiently
  median_times <- survival_curves$median_survival
  confidence_intervals <- survival_curves$confidence_intervals
  
  # Add median survival times and confidence intervals using vectorized lookup
  if (length(median_times) > 0) {
    # Create comprehensive lookup table with median times and CIs
    median_dt <- data.table(
      contract_type_temp = names(median_times),
      median_time = as.numeric(median_times)
    )
    
    # Add confidence intervals if available
    if (!is.null(confidence_intervals) && length(confidence_intervals) > 0) {
      ci_lower <- sapply(names(median_times), function(ct) {
        ci <- confidence_intervals[[ct]]
        if (!is.null(ci) && !is.null(ci$lower)) ci$lower else NA_real_
      })
      ci_upper <- sapply(names(median_times), function(ct) {
        ci <- confidence_intervals[[ct]]
        if (!is.null(ci) && !is.null(ci$upper)) ci$upper else NA_real_
      })
      
      median_dt[, `:=`(
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper)
      )]
    } else {
      median_dt[, `:=`(
        ci_lower = NA_real_,
        ci_upper = NA_real_
      )]
    }
    
    # Set proper column name for join
    setnames(median_dt, "contract_type_temp", contract_type_var)
    
    # Use simpler approach: loop through contract types to avoid join issues
    for (ct in names(median_times)) {
      ct_mask <- result[[contract_type_var]] == ct
      if (any(ct_mask)) {
        result[ct_mask, contract_type_median := median_times[ct]]
        
        # Add confidence intervals if available
        if (ct %in% names(confidence_intervals)) {
          ci <- confidence_intervals[[ct]]
          if (!is.null(ci)) {
            result[ct_mask, median_ci_lower := if (!is.null(ci$lower)) ci$lower else NA_real_]
            result[ct_mask, median_ci_upper := if (!is.null(ci$upper)) ci$upper else NA_real_]
          }
        }
      }
    }
  }
  
  # Process survival probabilities for each contract type using vectorized operations
  for (ct in names(survival_curves$lookup_tables)) {
    lookup_table <- survival_curves$lookup_tables[[ct]]
    
    if (is.null(lookup_table) || nrow(lookup_table) == 0) {
      next
    }
    
    # Get indices for this contract type
    ct_mask <- result[[contract_type_var]] == ct
    
    if (!any(ct_mask)) {
      next
    }
    
    # Extract durations for this contract type
    ct_durations <- result[ct_mask, get(duration_var)]
    
    # Vectorized lookup using data.table rolling join
    # This is much faster than individual get_survival_at_time calls
    lookup_expanded <- data.table(time = ct_durations)
    
    # Rolling join to find survival probabilities
    survival_probs <- lookup_table[lookup_expanded, roll = TRUE, on = "time"]$survival_prob
    
    # Handle edge cases for times before first event (set to 1.0)
    survival_probs[is.na(survival_probs)] <- 1.0
    
    # Assign survival probabilities
    result[ct_mask, survival_prob := survival_probs]
  }
  
  return(result)
}

#' Calculate Median Survival Probability (DEPRECATED)
#'
#' @description
#' DEPRECATED: This function has been removed because the median survival probability
#' calculation was fundamentally flawed. Use add_contract_survival_metrics() instead
#' to get proper median survival times with confidence intervals.
#'
#' @param data A data.table with contract information
#' @param survival_curves List output from estimate_contract_survival
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#'
#' @return Enhanced data.table with survival metrics (no median_survival_prob column)
#'
#' @export
# DEPRECATED: The median survival probability calculation was broken
# Use add_contract_survival_metrics() for proper survival analysis
calculate_median_survival_probability <- function(
    data,
    survival_curves,
    contract_type_var,
    duration_var = "survival_time"
) {
  
  warning("calculate_median_survival_probability() is deprecated. The median survival probability calculation was flawed. Use add_contract_survival_metrics() instead for proper median survival times with confidence intervals.")
  
  # Create a copy to avoid modifying original
  result <- copy(data)
  
  # Use optimized vectorized implementation (without broken median prob)
  result <- add_all_survival_metrics_optimized(
    data = result,
    survival_curves = survival_curves,
    contract_type_var = contract_type_var,
    duration_var = duration_var
  )
  
  return(result)
}

#' Add Survival Probabilities to Data
#'
#' @description
#' Adds the current survival probability for each contract based on
#' its duration and contract type.
#'
#' @param data A data.table with contract information
#' @param survival_curves List output from estimate_contract_survival
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#'
#' @return Enhanced data.table with survival_prob column
#'
#' @export
# Optimized function: replaced by add_all_survival_metrics_optimized
# Kept for backward compatibility
add_survival_probabilities <- function(
    data,
    survival_curves,
    contract_type_var,
    duration_var = "survival_time"
) {
  
  result <- copy(data)
  
  # Use optimized vectorized implementation
  result <- add_all_survival_metrics_optimized(
    data = result,
    survival_curves = survival_curves,
    contract_type_var = contract_type_var,
    duration_var = duration_var
  )
  
  return(result)
}

#' Helper: Get Survival Probability at Specific Time
#'
#' @description
#' Internal helper function to extract survival probability at a given time
#' from a survival table using step function logic.
#'
#' @param surv_table A data.table with time and survival_prob columns
#' @param time Numeric. Time point to evaluate
#'
#' @return Numeric. Survival probability at specified time
#'
#' @keywords internal
get_survival_at_time <- function(surv_table, query_time) {
  if (nrow(surv_table) == 0) return(NA_real_)
  if (query_time <= 0) return(1.0)
  
  # Find the latest time point not exceeding the requested time
  valid_times <- surv_table[time <= query_time]
  
  if (nrow(valid_times) == 0) {
    return(1.0)  # Before first event
  } else {
    return(valid_times[.N, survival_prob])
  }
}

#' Compare Survival Curves Across Contract Types
#'
#' @description
#' Performs statistical tests to compare survival curves between different
#' contract types (log-rank test, Wilcoxon test).
#'
#' @param data A data.table with contract information
#' @param contract_type_var Character. Contract type variable name
#' @param duration_var Character. Duration variable name
#' @param censored_var Character. Censoring indicator variable name
#' @param test_type Character. Type of test: "logrank", "wilcoxon", or "both"
#'
#' @return List containing:
#'   \itemize{
#'     \item{\code{test_results}}: Statistical test results
#'     \item{\code{pairwise_comparisons}}: Pairwise comparison p-values
#'     \item{\code{hazard_ratios}}: Estimated hazard ratios between groups
#'   }
#'
#' @export
compare_contract_survival <- function(
    data,
    contract_type_var,
    duration_var = "survival_time",
    censored_var = "censored",
    test_type = "both"
) {
  
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for this function.")
  }
  
  # Input validation
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }
  
  # Check required columns exist
  required_cols <- c(contract_type_var, duration_var, censored_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter out rows with missing values in key variables
  complete_data <- data[
    !is.na(get(contract_type_var)) & 
    !is.na(get(duration_var)) & 
    !is.na(get(censored_var)) &
    get(duration_var) > 0
  ]
  
  if (nrow(complete_data) == 0) {
    stop("No complete data available after filtering missing values")
  }
  
  # Check number of unique contract types
  contract_types <- complete_data[, unique(get(contract_type_var))]
  n_groups <- length(contract_types)
  
  if (n_groups < 2) {
    stop(sprintf("Need at least 2 groups for comparison. Found %d groups: %s", 
                 n_groups, paste(contract_types, collapse = ", ")))
  }
  
  cat(sprintf("Comparing survival across %d contract types with %d observations:\n", 
              n_groups, nrow(complete_data)))
  cat(sprintf("Contract types: %s\n", paste(contract_types, collapse = ", ")))
  
  # Create survival object using filtered data
  surv_obj <- survival::Surv(
    time = complete_data[[duration_var]],
    event = 1 - complete_data[[censored_var]]
  )
  
  # Overall comparison
  formula <- as.formula(paste("surv_obj ~", contract_type_var))
  
  test_results <- list()
  
  # Log-rank test
  if (test_type %in% c("logrank", "both")) {
    test_results$logrank <- survival::survdiff(
      formula,
      data = complete_data,
      rho = 0  # Log-rank test
    )
  }
  
  # Wilcoxon test (emphasizes early differences)
  if (test_type %in% c("wilcoxon", "both")) {
    test_results$wilcoxon <- survival::survdiff(
      formula,
      data = complete_data,
      rho = 1  # Wilcoxon test
    )
  }
  
  # Pairwise comparisons
  n_types <- length(contract_types)
  
  if (n_types > 2) {
    pairwise_p <- matrix(NA, n_types, n_types,
                        dimnames = list(contract_types, contract_types))
    
    for (i in 1:(n_types-1)) {
      for (j in (i+1):n_types) {
        pair_data <- complete_data[get(contract_type_var) %in% c(contract_types[i], contract_types[j])]
        
        if (nrow(pair_data) == 0) {
          warning(sprintf("No data available for pairwise comparison between %s and %s", 
                         contract_types[i], contract_types[j]))
          next
        }
        
        pair_surv <- survival::Surv(
          time = pair_data[[duration_var]],
          event = 1 - pair_data[[censored_var]]
        )
        
        pair_test <- tryCatch({
          survival::survdiff(
            pair_surv ~ get(contract_type_var),
            data = pair_data
          )
        }, error = function(e) {
          warning(sprintf("Failed pairwise comparison between %s and %s: %s", 
                         contract_types[i], contract_types[j], e$message))
          return(NULL)
        })
        
        if (!is.null(pair_test)) {
          pairwise_p[i, j] <- pairwise_p[j, i] <- 1 - pchisq(pair_test$chisq, 1)
        }
      }
    }
  } else {
    pairwise_p <- NULL
  }
  
  return(list(
    test_results = test_results,
    pairwise_comparisons = pairwise_p
  ))
}

#' Export Survival Analysis Results
#'
#' @description
#' Exports survival analysis results in various formats for reporting
#' and further analysis.
#'
#' @param survival_results List output from survival analysis functions
#' @param output_format Character. Format: "csv", "excel", "rdata"
#' @param output_dir Character. Directory for output files
#' @param prefix Character. Prefix for output filenames
#'
#' @return Character vector of created file paths
#'
#' @export
export_survival_results <- function(
    survival_results,
    output_format = "csv",
    output_dir = ".",
    prefix = "survival_analysis"
) {
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_files <- character()
  
  if (output_format == "csv") {
    # Export median survival times
    if (!is.null(survival_results$median_survival)) {
      median_file <- file.path(output_dir, paste0(prefix, "_median_times.csv"))
      write.csv(
        data.frame(
          contract_type = names(survival_results$median_survival),
          median_survival = survival_results$median_survival
        ),
        median_file,
        row.names = FALSE
      )
      output_files <- c(output_files, median_file)
    }
    
    # Export survival tables
    if (!is.null(survival_results$survival_tables)) {
      for (ct in names(survival_results$survival_tables)) {
        table_file <- file.path(output_dir, 
                               paste0(prefix, "_survival_", ct, ".csv"))
        write.csv(
          survival_results$survival_tables[[ct]],
          table_file,
          row.names = FALSE
        )
        output_files <- c(output_files, table_file)
      }
    }
  }
  
  if (output_format == "rdata") {
    rdata_file <- file.path(output_dir, paste0(prefix, ".RData"))
    save(survival_results, file = rdata_file)
    output_files <- c(output_files, rdata_file)
  }
  
  message(sprintf("Exported %d files to %s", length(output_files), output_dir))
  return(invisible(output_files))
}