#' Impact Evaluation: Control Group Identification and Matching
#'
#' This module provides comprehensive matching functionality for impact evaluation
#' studies including propensity score matching, coarsened exact matching, and
#' balance assessment with diagnostic tools.
#'
#' @name impact_matching
#' @author vecshift package
#' @importFrom dplyr first last
#' @importFrom data.table as.data.table
NULL

#' Aggregate Event-Level Data to Person-Level for Matching
#'
#' Helper function to aggregate event-level employment data to person-level
#' characteristics for propensity score matching. Handles different aggregation
#' methods for various variable types.
#'
#' @param data data.table with event-level data
#' @param person_id_var Character. Name of person identifier column
#' @param variables Character vector. Variables to aggregate
#' @param aggregation_method Character. Aggregation method: "first", "last", "mode", "mean"
#' @param verbose Logical. Print aggregation diagnostics?
#'
#' @return List containing aggregated data.table and aggregation report
aggregate_to_person_level <- function(data, person_id_var, variables, 
                                    aggregation_method = "first", verbose = TRUE) {
  
  # Store original event-level data for later
  original_data <- copy(data)
  
  # Initialize aggregation report
  aggregation_report <- list(
    original_events = nrow(data),
    unique_persons = uniqueN(data[[person_id_var]]),
    aggregation_method = aggregation_method,
    variables_aggregated = variables,
    aggregation_details = list()
  )
  
  if (verbose) {
    cat("\n=== PERSON-LEVEL AGGREGATION ===\n")
    cat("Original events:", nrow(data), "\n")
    cat("Unique persons:", aggregation_report$unique_persons, "\n")
    cat("Aggregation method:", aggregation_method, "\n")
  }
  
  # Create simple aggregation using .SD approach
  # This is more reliable than building complex expressions
  person_data <- data[, lapply(.SD, function(x) {
    var_class <- class(x)[1]
    
    # Record aggregation details
    var_name <- deparse(substitute(x))
    if (is.null(aggregation_report$aggregation_details[[var_name]])) {
      aggregation_report$aggregation_details[[var_name]] <<- list(
        type = var_class,
        method_used = aggregation_method,
        original_unique_values = uniqueN(x, na.rm = TRUE),
        missing_count = sum(is.na(x))
      )
    }
    
    # Apply aggregation based on variable type and method
    if (is.numeric(x)) {
      if (aggregation_method == "mean") return(mean(x, na.rm = TRUE))
      if (aggregation_method == "mode") return(median(x, na.rm = TRUE))  # Mode not appropriate for numeric
      if (aggregation_method == "last") return(last(x[!is.na(x)]))
      return(first(x[!is.na(x)]))  # Default to first
    } else if (is.character(x) || is.factor(x)) {
      if (aggregation_method == "mode" || aggregation_method == "mean") {
        tbl <- table(x, useNA = "no")
        if (length(tbl) > 0) return(names(tbl)[which.max(tbl)])
        return(NA_character_)
      }
      if (aggregation_method == "last") return(last(x[!is.na(x)]))
      return(first(x[!is.na(x)]))  # Default to first
    } else {
      # For other types, use first non-NA value
      if (aggregation_method == "last") return(last(x[!is.na(x)]))
      return(first(x[!is.na(x)]))
    }
  }), .SDcols = variables, by = c(person_id_var)]
  
  # Print aggregation details after processing
  if (verbose) {
    for (var in variables) {
      if (var %in% names(data)) {
        values <- data[[var]]
        var_class <- class(values)[1]
        method_used <- if (is.numeric(values) && aggregation_method == "mode") "median" else aggregation_method
        cat(sprintf("  %s (%s): using %s\n", var, var_class, method_used))
      }
    }
  }
  
  # Add aggregation metadata
  aggregation_report$final_persons <- nrow(person_data)
  aggregation_report$variables_created <- names(person_data)[!names(person_data) %in% person_id_var]
  
  if (verbose) {
    cat("Final person-level observations:", nrow(person_data), "\n")
    cat("Variables aggregated:", length(variables), "\n")
  }
  
  return(list(
    person_data = person_data,
    original_data = original_data,
    aggregation_report = aggregation_report
  ))
}

#' Propensity Score Matching for Impact Evaluation (Person-Level Matching)
#'
#' Performs propensity score matching to identify comparable control units for
#' treatment effect estimation at the person level. This function first aggregates
#' event-level data to person-level characteristics, performs matching on people,
#' then returns all events for matched individuals. This is crucial for employment
#' data where we want to match people (not individual employment spells) based on
#' their stable characteristics.
#'
#' @param data A data.table containing treatment and control observations (can be event-level)
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param person_id_var Character. Name of person identifier variable (e.g., "cf"). Default: "cf"
#' @param matching_variables Character vector. Variables to include in propensity score model
#' @param exact_match_vars Character vector. Variables requiring exact matches. Default: NULL
#' @param person_aggregation Character. How to aggregate person characteristics: "first", "last", "mode", "mean". Default: "first"
#' @param method Character. Matching method: "nearest", "optimal", "full", "genetic". Default: "nearest"
#' @param ratio Numeric. Ratio of control to treatment units. Default: 1
#' @param caliper Numeric. Maximum allowable distance for matches. Default: NULL (no caliper)
#' @param replace Logical. Allow replacement in matching? Default: FALSE
#' @param estimand Character. Target estimand: "ATT", "ATE", "ATC". Default: "ATT"
#' @param link Character. Link function for propensity model: "logit", "probit". Default: "logit"
#' @param distance_metric Character. Distance metric: "glm", "gam", "gbm", "randomforest". Default: "glm"
#' @param missing_data_action Character. How to handle missing data: "complete_cases" (default), "impute", "exclude_vars"
#' @param imputation_method Character. For numeric variables: "median" (default), "mean". For categorical: "mode"
#' @param min_complete_cases Numeric. Minimum proportion of complete cases required to proceed (0-1). Default: 0.5
#' @param max_missing_proportion Numeric. Maximum proportion of missing values allowed per variable (0-1). Default: 0.3
#' @param factor_level_threshold Numeric. Minimum observations per factor level. Default: 5
#' @param verbose Logical. Print detailed missing data diagnostics? Default: TRUE
#'
#' @return A list containing:
#'   \item{matched_data}{Data.table with ALL events for matched individuals (both treated and control). Includes event_time column with values: "pre" (pre-treatment), "post" (post-treatment), "control" (control group), or NA}
#'   \item{matched_persons}{Data.table with person-level characteristics used for matching}
#'   \item{match_matrix}{Matrix showing which persons were matched}
#'   \item{propensity_scores}{Propensity scores for all persons}
#'   \item{balance_before}{Balance statistics before matching (person-level)}
#'   \item{balance_after}{Balance statistics after matching (person-level)}
#'   \item{match_summary}{Summary of matching procedure}
#'   \item{common_support}{Information about common support region}
#'   \item{data_quality_report}{Report on missing data handling and data quality issues}
#'   \item{aggregation_report}{Report on person-level aggregation process}
#'
#' @examples
#' \dontrun{
#' # Process employment data first
#' employment_data <- vecshift(raw_employment_data)
#' 
#' # Add treatment indicator (e.g., policy intervention)
#' employment_data[, is_treated := some_treatment_condition]
#' 
#' # Basic person-level propensity score matching
#' ps_match <- propensity_score_matching(
#'   data = employment_data,
#'   person_id_var = "cf",
#'   matching_variables = c("age", "education", "sector", "region"),
#'   exact_match_vars = c("gender"),
#'   person_aggregation = "first",
#'   method = "nearest",
#'   ratio = 2,
#'   missing_data_action = "complete_cases",
#'   min_complete_cases = 0.7
#' )
#' 
#' # Result contains all events for matched persons
#' matched_employment_data <- ps_match$matched_data
#' person_characteristics <- ps_match$matched_persons
#' 
#' # Advanced matching with imputation for missing values
#' ps_match_imputed <- propensity_score_matching(
#'   data = employment_data,
#'   person_id_var = "cf",
#'   matching_variables = c("age", "education", "prior_employment", "wage"),
#'   person_aggregation = "mode",  # Use mode for categorical variables
#'   method = "optimal",
#'   caliper = 0.1,
#'   distance_metric = "gbm",
#'   missing_data_action = "impute",
#'   imputation_method = "median"
#' )
#' }
#'
#' @export
propensity_score_matching <- function(data,
                                    treatment_var = "is_treated",
                                    person_id_var = "cf",
                                    matching_variables,
                                    exact_match_vars = NULL,
                                    person_aggregation = "first",
                                    method = "nearest",
                                    ratio = 1,
                                    caliper = NULL,
                                    replace = FALSE,
                                    estimand = "ATT",
                                    link = "logit",
                                    distance_metric = "glm",
                                    missing_data_action = "complete_cases",
                                    imputation_method = "median",
                                    min_complete_cases = 0.5,
                                    max_missing_proportion = 0.3,
                                    factor_level_threshold = 5,
                                    verbose = TRUE) {
  
  # Validate parameters
  valid_missing_actions <- c("complete_cases", "impute", "exclude_vars")
  if (!missing_data_action %in% valid_missing_actions) {
    stop(paste("missing_data_action must be one of:", paste(valid_missing_actions, collapse = ", ")))
  }
  
  valid_imputation_methods <- c("median", "mean", "mode")
  if (!imputation_method %in% valid_imputation_methods) {
    stop(paste("imputation_method must be one of:", paste(valid_imputation_methods, collapse = ", ")))
  }
  
  valid_aggregation_methods <- c("first", "last", "mode", "mean")
  if (!person_aggregation %in% valid_aggregation_methods) {
    stop(paste("person_aggregation must be one of:", paste(valid_aggregation_methods, collapse = ", ")))
  }
  
  if (min_complete_cases < 0 || min_complete_cases > 1) {
    stop("min_complete_cases must be between 0 and 1")
  }
  
  if (max_missing_proportion < 0 || max_missing_proportion > 1) {
    stop("max_missing_proportion must be between 0 and 1")
  }
  
  # Check for required packages
  required_packages <- c("MatchIt", "cobalt")
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!treatment_var %in% names(data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  if (!person_id_var %in% names(data)) {
    stop(paste("Person ID variable", person_id_var, "not found in data"))
  }
  
  missing_vars <- setdiff(matching_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Matching variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  if (!is.null(exact_match_vars)) {
    missing_exact <- setdiff(exact_match_vars, names(data))
    if (length(missing_exact) > 0) {
      stop(paste("Exact match variables not found:", paste(missing_exact, collapse = ", ")))
    }
  }
  
  # Step 1: Person-Level Aggregation
  if (verbose) cat("\n=== PROPENSITY SCORE MATCHING: PERSON-LEVEL AGGREGATION ===\n")
  
  # Aggregate event-level data to person-level characteristics
  all_vars_for_aggregation <- c(treatment_var, matching_variables, exact_match_vars)
  aggregation_result <- aggregate_to_person_level(
    data = data,
    person_id_var = person_id_var,
    variables = all_vars_for_aggregation,
    aggregation_method = person_aggregation,
    verbose = verbose
  )
  
  # Use person-level data for matching but keep original data for final results
  working_data <- aggregation_result$person_data
  original_data <- aggregation_result$original_data
  aggregation_report <- aggregation_result$aggregation_report
  
  # Update variable list for person-level data
  all_vars <- c(treatment_var, matching_variables, exact_match_vars)
  
  # Step 2: Comprehensive Data Quality Assessment (on person-level data)
  if (verbose) cat("\n=== PROPENSITY SCORE MATCHING: DATA QUALITY ASSESSMENT ===\n")
  
  # Initialize data quality report
  data_quality_report <- list(
    original_n = nrow(working_data),
    original_vars = length(all_vars),
    missing_pattern = list(),
    variable_issues = list(),
    preprocessing_actions = character(),
    final_n = NA,
    final_vars = NA
  )
  
  # Analyze missing patterns by variable
  missing_analysis <- lapply(all_vars, function(var) {
    values <- working_data[[var]]
    
    # Count different types of missingness/issues
    na_count <- sum(is.na(values))
    inf_count <- if (is.numeric(values)) sum(is.infinite(values), na.rm = TRUE) else 0
    nan_count <- if (is.numeric(values)) sum(is.nan(values), na.rm = TRUE) else 0
    empty_str_count <- if (is.character(values)) sum(values == "" | values == " ", na.rm = TRUE) else 0
    
    total_issues <- na_count + inf_count + nan_count + empty_str_count
    prop_issues <- total_issues / length(values)
    
    # For factors, check level distribution
    factor_issues <- character()
    if (is.factor(values) || is.character(values)) {
      value_counts <- table(values, useNA = "no")
      small_levels <- sum(value_counts < factor_level_threshold)
      if (small_levels > 0) {
        factor_issues <- paste("Factor levels with <", factor_level_threshold, "observations:", small_levels)
      }
    }
    
    list(
      variable = var,
      type = class(values)[1],
      n_observations = length(values),
      na_count = na_count,
      inf_count = inf_count,
      nan_count = nan_count,
      empty_str_count = empty_str_count,
      total_issues = total_issues,
      proportion_issues = prop_issues,
      factor_issues = factor_issues
    )
  })
  names(missing_analysis) <- all_vars
  data_quality_report$variable_issues <- missing_analysis
  
  if (verbose) {
    cat("Original data: n =", nrow(working_data), "variables =", length(all_vars), "\n")
    cat("\nVariable Quality Summary:\n")
    for (var in names(missing_analysis)) {
      info <- missing_analysis[[var]]
      cat(sprintf("  %s (%s): %.1f%% issues (%d NA, %d Inf/NaN, %d empty)\n", 
                  info$variable, info$type, info$proportion_issues * 100,
                  info$na_count, info$inf_count + info$nan_count, info$empty_str_count))
      if (length(info$factor_issues) > 0) {
        cat(sprintf("    Warning: %s\n", info$factor_issues))
      }
    }
  }
  
  # Step 2: Handle Missing Data According to Strategy
  if (missing_data_action == "exclude_vars") {
    # Remove variables with too many missing values
    vars_to_remove <- character()
    for (var in all_vars) {
      if (missing_analysis[[var]]$proportion_issues > max_missing_proportion) {
        vars_to_remove <- c(vars_to_remove, var)
      }
    }
    
    if (length(vars_to_remove) > 0) {
      if (treatment_var %in% vars_to_remove) {
        stop(paste("Treatment variable", treatment_var, "has too many missing values (>", 
                   max_missing_proportion * 100, "%). Cannot proceed."))
      }
      
      matching_variables <- setdiff(matching_variables, vars_to_remove)
      exact_match_vars <- setdiff(exact_match_vars, vars_to_remove)
      
      data_quality_report$preprocessing_actions <- c(
        data_quality_report$preprocessing_actions,
        paste("Excluded variables:", paste(vars_to_remove, collapse = ", "))
      )
      
      if (verbose) {
        cat(sprintf("\nExcluded %d variables with >%.1f%% missing: %s\n", 
                    length(vars_to_remove), max_missing_proportion * 100,
                    paste(vars_to_remove, collapse = ", ")))
      }
      
      # Update all_vars after exclusions
      all_vars <- c(treatment_var, matching_variables, exact_match_vars)
      if (length(matching_variables) == 0) {
        stop("No matching variables remain after excluding variables with high missing rates")
      }
    }
  }
  
  # Clean non-finite values first
  for (var in all_vars) {
    if (is.numeric(working_data[[var]])) {
      # Replace Inf, -Inf, NaN with NA
      inf_indices <- is.infinite(working_data[[var]]) | is.nan(working_data[[var]])
      if (sum(inf_indices, na.rm = TRUE) > 0) {
        working_data[inf_indices, (var) := NA]
        data_quality_report$preprocessing_actions <- c(
          data_quality_report$preprocessing_actions,
          paste("Converted", sum(inf_indices, na.rm = TRUE), "non-finite values to NA in", var)
        )
      }
    }
    
    # Clean empty strings
    if (is.character(working_data[[var]])) {
      empty_indices <- working_data[[var]] == "" | working_data[[var]] == " " | is.na(working_data[[var]])
      if (sum(empty_indices, na.rm = TRUE) > 0) {
        working_data[empty_indices, (var) := NA_character_]
      }
    }
  }
  
  # Apply missing data strategy
  if (missing_data_action == "complete_cases") {
    # Use only complete cases
    complete_cases <- complete.cases(working_data[, all_vars, with = FALSE])
    n_complete <- sum(complete_cases)
    prop_complete <- n_complete / nrow(working_data)
    
    if (prop_complete < min_complete_cases) {
      stop(sprintf("Only %.1f%% complete cases available (minimum required: %.1f%%). Consider using imputation or excluding variables.",
                   prop_complete * 100, min_complete_cases * 100))
    }
    
    working_data <- working_data[complete_cases]
    data_quality_report$preprocessing_actions <- c(
      data_quality_report$preprocessing_actions,
      paste("Kept only complete cases:", n_complete, "of", nrow(data), "observations")
    )
    
    if (verbose) {
      cat(sprintf("\nUsing complete cases: %d observations (%.1f%% of original)\n", 
                  n_complete, prop_complete * 100))
    }
    
  } else if (missing_data_action == "impute") {
    # Impute missing values
    imputed_vars <- character()
    
    for (var in all_vars) {
      if (var == treatment_var) {
        # Never impute treatment variable
        if (any(is.na(working_data[[var]]))) {
          stop(paste("Treatment variable", var, "contains missing values. Cannot impute treatment status."))
        }
        next
      }
      
      values <- working_data[[var]]
      missing_mask <- is.na(values)
      
      if (any(missing_mask)) {
        if (is.numeric(values)) {
          # Numeric imputation
          if (imputation_method == "median") {
            impute_value <- median(values, na.rm = TRUE)
          } else if (imputation_method == "mean") {
            impute_value <- mean(values, na.rm = TRUE)
          }
          
          if (is.finite(impute_value)) {
            working_data[missing_mask, (var) := impute_value]
            imputed_vars <- c(imputed_vars, var)
            
            data_quality_report$preprocessing_actions <- c(
              data_quality_report$preprocessing_actions,
              sprintf("Imputed %d missing values in %s with %s = %.3f", 
                      sum(missing_mask), var, imputation_method, impute_value)
            )
          } else {
            stop(paste("Cannot impute variable", var, ": all non-missing values are non-finite"))
          }
          
        } else if (is.character(values) || is.factor(values)) {
          # Categorical imputation (mode)
          value_counts <- table(values, useNA = "no")
          if (length(value_counts) > 0) {
            mode_value <- names(value_counts)[which.max(value_counts)]
            working_data[missing_mask, (var) := mode_value]
            imputed_vars <- c(imputed_vars, var)
            
            data_quality_report$preprocessing_actions <- c(
              data_quality_report$preprocessing_actions,
              sprintf("Imputed %d missing values in %s with mode = '%s'", 
                      sum(missing_mask), var, mode_value)
            )
          } else {
            stop(paste("Cannot impute variable", var, ": no valid values available"))
          }
        }
      }
    }
    
    if (verbose && length(imputed_vars) > 0) {
      cat(sprintf("\nImputed missing values in %d variables: %s\n", 
                  length(imputed_vars), paste(imputed_vars, collapse = ", ")))
    }
  }
  
  # Step 3: Final data validation
  # Check for factor level consistency between treatment groups
  for (var in all_vars) {
    if (is.factor(working_data[[var]]) || is.character(working_data[[var]])) {
      treated_levels <- unique(working_data[get(treatment_var) == 1, get(var)])
      control_levels <- unique(working_data[get(treatment_var) == 0, get(var)])
      
      missing_in_treated <- setdiff(control_levels, treated_levels)
      missing_in_control <- setdiff(treated_levels, control_levels)
      
      if (length(missing_in_treated) > 0 || length(missing_in_control) > 0) {
        warning(sprintf("Variable %s: Factor level mismatch between treatment groups. Missing in treated: %s. Missing in control: %s",
                        var, paste(missing_in_treated, collapse = ", "), paste(missing_in_control, collapse = ", ")))
      }
    }
  }
  
  # Convert to data.frame for MatchIt
  match_data <- as.data.frame(working_data)
  
  # Update quality report
  data_quality_report$final_n <- nrow(working_data)
  data_quality_report$final_vars <- length(all_vars)
  
  if (verbose) {
    cat(sprintf("\nFinal data for matching: %d observations, %d variables\n", 
                nrow(working_data), length(all_vars)))
    cat("Treatment distribution:", table(working_data[[treatment_var]]), "\n")
  }
  
  # Step 4: Create propensity score formula
  if (!is.null(exact_match_vars) && length(exact_match_vars) > 0) {
    formula_str <- paste(treatment_var, "~", 
                        paste(c(matching_variables, exact_match_vars), collapse = " + "))
    exact_str <- paste(exact_match_vars, collapse = " + ")
  } else {
    formula_str <- paste(treatment_var, "~", 
                        paste(matching_variables, collapse = " + "))
    exact_str <- NULL
  }
  
  match_formula <- as.formula(formula_str)
  
  if (verbose) {
    cat("\nMatching formula:", formula_str, "\n")
    if (!is.null(exact_str)) {
      cat("Exact matching on:", exact_str, "\n")
    }
  }
  
  # Step 5: Perform matching using MatchIt
  if (verbose) cat("\n=== PERFORMING PROPENSITY SCORE MATCHING ===\n")
  
  tryCatch({
    match_result <- MatchIt::matchit(
      formula = match_formula,
      data = match_data,
      method = method,
      ratio = ratio,
      caliper = caliper,
      replace = replace,
      estimand = estimand,
      link = link,
      distance = distance_metric,
      exact = if (!is.null(exact_str)) as.formula(paste("~", exact_str)) else NULL
    )
  }, error = function(e) {
    stop(paste("Matching failed after data preprocessing. Error:", e$message,
               "\nThis suggests the data quality issues could not be resolved with the current strategy.",
               "\nConsider: (1) Different missing data strategy, (2) Different matching variables, (3) Manual data cleaning"))
  })
  
  # Extract matched data
  matched_data <- MatchIt::match.data(match_result)
  matched_dt <- as.data.table(matched_data)
  
  if (verbose) {
    cat("Matching completed successfully!\n")
    cat("Matched observations: treated =", sum(matched_data[[treatment_var]]), 
        ", control =", sum(1 - matched_data[[treatment_var]]), "\n")
  }
  
  # Step 6: Calculate balance before and after matching
  balance_before <- tryCatch({
    cobalt::bal.tab(match_result, un = TRUE, thresholds = c(m = 0.1))
  }, error = function(e) {
    warning(paste("Balance calculation failed:", e$message))
    NULL
  })
  
  balance_after <- tryCatch({
    cobalt::bal.tab(match_result, thresholds = c(m = 0.1))
  }, error = function(e) {
    warning(paste("Balance calculation failed:", e$message))
    NULL
  })
  
  # Extract propensity scores
  propensity_scores <- data.table(
    row_id = 1:nrow(match_data),
    propensity_score = match_result$distance %||% NA_real_,
    is_treated = match_data[[treatment_var]]
  )
  
  # Common support analysis
  if (!is.null(match_result$distance)) {
    ps_treated <- match_result$distance[match_data[[treatment_var]] == 1]
    ps_control <- match_result$distance[match_data[[treatment_var]] == 0]
    
    common_support <- list(
      treated_range = range(ps_treated, na.rm = TRUE),
      control_range = range(ps_control, na.rm = TRUE),
      overlap_range = c(max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)),
                       min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE))),
      treated_in_support = sum(ps_treated >= max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)) &
                              ps_treated <= min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE))),
      control_in_support = sum(ps_control >= max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)) &
                              ps_control <= min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE)))
    )
  } else {
    common_support <- NULL
  }
  
  # Enhanced match summary
  match_summary <- list(
    method = method,
    total_treated = sum(match_data[[treatment_var]]),
    total_control = sum(!match_data[[treatment_var]]),
    matched_treated = nrow(matched_dt[get(treatment_var) == 1]),
    matched_control = nrow(matched_dt[get(treatment_var) == 0]),
    matching_ratio = ratio,
    caliper_used = !is.null(caliper),
    caliper_value = caliper,
    replacement_used = replace,
    exact_variables = exact_match_vars,
    missing_data_strategy = missing_data_action,
    data_reduction_pct = (1 - nrow(working_data) / nrow(original_data)) * 100,
    variables_used = length(all_vars)
  )
  
  if (verbose) {
    cat("\n=== MATCHING SUMMARY ===\n")
    cat("Data reduction:", sprintf("%.1f%%", match_summary$data_reduction_pct), 
        "(", nrow(original_data), "->", nrow(working_data), "observations )\n")
    cat("Missing data strategy:", missing_data_action, "\n")
    cat("Variables used:", length(all_vars), "\n")
    cat("Final matches: treated =", match_summary$matched_treated, 
        ", control =", match_summary$matched_control, "\n")
  }
  
  # Step 6: Extract all events for matched persons
  if (verbose) cat("\n=== EXTRACTING ALL EVENTS FOR MATCHED PERSONS ===\n")
  
  # Get list of matched person IDs
  matched_person_ids <- matched_dt[[person_id_var]]
  
  # Extract all original events for these matched persons
  all_events_for_matched_persons <- original_data[get(person_id_var) %in% matched_person_ids]
  
  # Add event_time column for compatibility with calculate_comprehensive_impact_metrics()
  if ("pre_event_period" %in% names(all_events_for_matched_persons) && 
      "post_event_period" %in% names(all_events_for_matched_persons) &&
      "is_treated" %in% names(all_events_for_matched_persons)) {
    
    all_events_for_matched_persons[, event_time := ifelse(
      is_treated == FALSE, "control",
      ifelse(is_treated == TRUE & pre_event_period == TRUE, "pre",
        ifelse(is_treated == TRUE & post_event_period == TRUE, "post", 
          NA_character_))
    )]
    
    if (verbose) {
      cat("Added event_time column with distribution:\n")
      print(table(all_events_for_matched_persons$event_time, useNA = "ifany"))
    }
  } else {
    if (verbose) {
      cat("Warning: Cannot create event_time column - missing required columns (pre_event_period, post_event_period, is_treated)\n")
    }
  }
  
  if (verbose) {
    cat("Matched persons:", length(matched_person_ids), "\n")
    cat("Total events for matched persons:", nrow(all_events_for_matched_persons), "\n")
    cat("Events per matched person (avg):", round(nrow(all_events_for_matched_persons) / length(matched_person_ids), 1), "\n")
  }
  
  return(list(
    matched_data = all_events_for_matched_persons,
    matched_persons = matched_dt,
    match_matrix = match_result$match.matrix,
    propensity_scores = propensity_scores,
    balance_before = balance_before,
    balance_after = balance_after,
    match_summary = match_summary,
    common_support = common_support,
    matchit_object = match_result,
    data_quality_report = data_quality_report,
    aggregation_report = aggregation_report
  ))
}

#' Coarsened Exact Matching (CEM) with Person-Level Matching
#'
#' Performs coarsened exact matching to create balanced treatment and control groups
#' by automatically binning continuous variables and exactly matching on specified factors.
#' This function first aggregates event-level data to person-level characteristics, 
#' performs matching on people, then returns all events for matched individuals.
#' This implementation provides both native CEM (when available) and a fallback
#' implementation that doesn't require X11/tcltk dependencies.
#'
#' @param data A data.table containing treatment and control observations (can be event-level)
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param person_id_var Character. Name of person identifier variable (e.g., "cf"). Default: "cf"
#' @param matching_variables Character vector. Variables to include in matching
#' @param person_aggregation Character. How to aggregate person characteristics: "first", "last", "mode", "mean". Default: "first"
#' @param automatic_binning Logical. Automatically bin continuous variables? Default: TRUE
#' @param cutpoints Named list. Custom cutpoints for continuous variables. Default: NULL
#' @param k2k Logical. Perform k-to-k matching (1:1 ratio)? Default: FALSE
#' @param control_ratio Numeric. Ratio of control to treatment units (e.g., 2 for 2:1). Default: 1
#' @param keep_all Logical. Keep all matched observations? Default: TRUE
#' @param use_native_cem Logical. Try to use native CEM package? Default: TRUE
#' @param n_bins Integer. Number of bins for automatic binning. Default: 4
#' @param verbose Logical. Print detailed information? Default: TRUE
#'
#' @return A list containing:
#'   \item{matched_data}{Data.table with ALL events for matched individuals (both treated and control). Includes event_time column with values: "pre" (pre-treatment), "post" (post-treatment), "control" (control group), or NA}
#'   \item{matched_persons}{Data.table with person-level characteristics used for matching}
#'   \item{match_summary}{Summary of CEM procedure}
#'   \item{imbalance_measures}{L1 and other imbalance statistics}
#'   \item{strata_info}{Information about matching strata}
#'   \item{aggregation_report}{Report on person-level aggregation process}
#'   \item{implementation_used}{Which implementation was used: "native_cem" or "fallback"}
#'
#' @examples
#' \dontrun{
#' # Process employment data first
#' employment_data <- vecshift(raw_employment_data)
#' 
#' # Add treatment indicator (e.g., policy intervention)
#' employment_data[, is_treated := some_treatment_condition]
#' 
#' # Basic CEM with person-level matching
#' cem_match <- coarsened_exact_matching(
#'   data = employment_data,
#'   person_id_var = "cf",
#'   matching_variables = c("age", "education", "sector"),
#'   person_aggregation = "first",
#'   automatic_binning = TRUE
#' )
#' 
#' # Result contains all events for matched persons
#' matched_employment_data <- cem_match$matched_data
#' person_characteristics <- cem_match$matched_persons
#' 
#' # CEM with 2:1 control-to-treatment ratio
#' cem_match_2to1 <- coarsened_exact_matching(
#'   data = employment_data,
#'   person_id_var = "cf",
#'   matching_variables = c("age", "wage", "experience"),
#'   control_ratio = 2,  # 2 controls per treated unit
#'   cutpoints = list(
#'     age = c(25, 35, 45, 55),
#'     wage = c(1000, 2000, 3000, 4000)
#'   )
#' )
#' 
#' # Force use of fallback implementation with custom ratio
#' cem_match_fallback <- coarsened_exact_matching(
#'   data = employment_data,
#'   person_id_var = "cf",
#'   matching_variables = c("age", "education", "sector"),
#'   control_ratio = 2,
#'   use_native_cem = FALSE
#' )
#' }
#'
#' @export
coarsened_exact_matching <- function(data,
                                   treatment_var = "is_treated",
                                   person_id_var = "cf",
                                   matching_variables,
                                   person_aggregation = "first",
                                   automatic_binning = TRUE,
                                   cutpoints = NULL,
                                   k2k = FALSE,
                                   control_ratio = 1,
                                   keep_all = TRUE,
                                   use_native_cem = TRUE,
                                   n_bins = 4,
                                   verbose = TRUE) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!treatment_var %in% names(data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  if (!person_id_var %in% names(data)) {
    stop(paste("Person ID variable", person_id_var, "not found in data"))
  }
  
  missing_vars <- setdiff(matching_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Matching variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  # Validate parameters
  valid_aggregation_methods <- c("first", "last", "mode", "mean")
  if (!person_aggregation %in% valid_aggregation_methods) {
    stop(paste("person_aggregation must be one of:", paste(valid_aggregation_methods, collapse = ", ")))
  }
  
  if (control_ratio < 1) {
    stop("control_ratio must be >= 1")
  }
  
  # Step 1: Person-Level Aggregation
  if (verbose) cat("\n=== COARSENED EXACT MATCHING: PERSON-LEVEL AGGREGATION ===\n")
  
  # Store original event-level data for later
  original_data <- copy(data)
  
  # Aggregate event-level data to person-level characteristics
  all_vars_for_aggregation <- c(treatment_var, matching_variables)
  aggregation_result <- aggregate_to_person_level(
    data = data,
    person_id_var = person_id_var,
    variables = all_vars_for_aggregation,
    aggregation_method = person_aggregation,
    verbose = verbose
  )
  
  # Use person-level data for matching but keep original data for final results
  working_data <- aggregation_result$person_data
  aggregation_report <- aggregation_result$aggregation_report
  
  if (verbose) {
    cat("\nPerson-level data prepared for matching:\n")
    cat("  Original events:", nrow(original_data), "\n")
    cat("  Unique persons:", nrow(working_data), "\n")
    cat("  Treatment distribution (persons):", table(working_data[[treatment_var]]), "\n")
  }
  
  # Try native CEM first if requested
  native_cem_available <- FALSE
  if (use_native_cem) {
    tryCatch({
      if ("cem" %in% rownames(installed.packages())) {
        # Test if CEM package loads without errors
        test_result <- tryCatch({
          requireNamespace("cem", quietly = TRUE)
          # Additional test: try to call cem with minimal data to check for tcltk issues
          test_data <- data.frame(x = 1:10, y = rep(c(0,1), 5))
          cem::cem(treatment = test_data$y, data = test_data[, "x", drop = FALSE])
          TRUE
        }, error = function(e) {
          if (verbose) {
            cat("Native CEM package failed to load:", e$message, "\n")
            cat("Falling back to native implementation...\n")
          }
          FALSE
        })
        native_cem_available <- test_result
      } else {
        if (verbose) {
          cat("CEM package not installed. Using native implementation...\n")
        }
      }
    }, error = function(e) {
      if (verbose) {
        cat("Error checking CEM package:", e$message, "\n")
        cat("Using native implementation...\n")
      }
    })
  }
  
  if (native_cem_available) {
    # Try native implementation, with fallback if it fails
    tryCatch({
      result <- cem_native_implementation(working_data, treatment_var, person_id_var, matching_variables, 
                                     automatic_binning, cutpoints, k2k, control_ratio, keep_all, verbose,
                                     original_data, aggregation_report)
      return(result)
    }, error = function(e) {
      if (verbose) {
        cat("Native CEM implementation failed:", e$message, "\n")
        cat("Falling back to native implementation...\n")
      }
      result <- cem_fallback_implementation(working_data, treatment_var, person_id_var, matching_variables,
                                       automatic_binning, cutpoints, k2k, control_ratio, keep_all, 
                                       n_bins, verbose, original_data, aggregation_report)
      return(result)
    })
  } else {
    result <- cem_fallback_implementation(working_data, treatment_var, person_id_var, matching_variables,
                                     automatic_binning, cutpoints, k2k, control_ratio, keep_all, 
                                     n_bins, verbose, original_data, aggregation_report)
    return(result)
  }
}

#' Native CEM Implementation (using CEM package) with Person-Level Matching
#' @keywords internal
cem_native_implementation <- function(data, treatment_var, person_id_var, matching_variables,
                                    automatic_binning, cutpoints, k2k, control_ratio, keep_all, 
                                    verbose, original_data, aggregation_report) {
  
  # Convert to data.frame for CEM
  match_data <- as.data.frame(data)
  
  # Prepare matching variables
  X <- match_data[, matching_variables, drop = FALSE]
  treatment <- match_data[[treatment_var]]
  
  # Apply custom cutpoints if provided
  if (!is.null(cutpoints)) {
    for (var in names(cutpoints)) {
      if (var %in% matching_variables && is.numeric(match_data[[var]])) {
        X[[var]] <- cut(X[[var]], breaks = cutpoints[[var]], include.lowest = TRUE)
      }
    }
  }
  
  # Perform CEM
  tryCatch({
    cem_result <- cem::cem(
      treatment = treatment,
      data = X,
      k2k = k2k,
      keep.all = keep_all
    )
  }, error = function(e) {
    stop(paste("Native CEM matching failed:", e$message))
  })
  
  # Create matched person dataset with weights
  matched_indices <- which(cem_result$w > 0)
  matched_persons <- as.data.table(match_data[matched_indices, ])
  matched_persons[, cem_weight := cem_result$w[matched_indices]]
  matched_persons[, cem_strata := cem_result$strata[matched_indices]]
  
  # Handle control_ratio if > 1 (CEM package doesn't directly support this)
  if (control_ratio > 1 && !k2k) {
    if (verbose) cat("\nAdjusting for control_ratio =", control_ratio, "\n")
    # For each stratum, sample additional controls to achieve desired ratio
    strata_adjustment <- matched_persons[, {
      n_treated <- sum(get(treatment_var) == 1)
      n_control <- sum(get(treatment_var) == 0)
      
      if (n_treated > 0 && n_control > 0) {
        # Calculate how many controls we need
        desired_control <- n_treated * control_ratio
        
        if (n_control < desired_control) {
          # Not enough controls, use all available
          .SD
        } else {
          # Sample controls to achieve ratio
          treated_rows <- .SD[get(treatment_var) == 1]
          control_rows <- .SD[get(treatment_var) == 0]
          sampled_controls <- control_rows[sample(.N, min(.N, as.integer(desired_control)))]
          rbind(treated_rows, sampled_controls)
        }
      } else {
        .SD
      }
    }, by = cem_strata]
    matched_persons <- strata_adjustment
  }
  
  # Extract all events for matched persons
  matched_person_ids <- matched_persons[[person_id_var]]
  matched_data <- original_data[get(person_id_var) %in% matched_person_ids]
  
  # Add event_time column for compatibility with calculate_comprehensive_impact_metrics()
  if ("pre_event_period" %in% names(matched_data) && 
      "post_event_period" %in% names(matched_data) &&
      "is_treated" %in% names(matched_data)) {
    
    matched_data[, event_time := ifelse(
      is_treated == FALSE, "control",
      ifelse(is_treated == TRUE & pre_event_period == TRUE, "pre",
        ifelse(is_treated == TRUE & post_event_period == TRUE, "post", 
          NA_character_))
    )]
    
    if (verbose) {
      cat("Added event_time column with distribution:\n")
      print(table(matched_data$event_time, useNA = "ifany"))
    }
  } else {
    if (verbose) {
      cat("Warning: Cannot create event_time column - missing required columns (pre_event_period, post_event_period, is_treated)\n")
    }
  }
  
  # Add weights and strata info to matched data
  person_info <- matched_persons[, c(person_id_var, "cem_weight", "cem_strata"), with = FALSE]
  matched_data <- merge(matched_data, person_info, by = person_id_var)
  
  # Calculate imbalance measures
  imbalance_measures <- list(
    l1_before = cem_result$L1$L1,
    l1_after = cem_result$L1$L1.after,
    l1_reduction = (cem_result$L1$L1 - cem_result$L1$L1.after) / cem_result$L1$L1,
    breaks_used = cem_result$breaks
  )
  
  # Strata information
  strata_info <- data.table(
    strata = names(table(cem_result$strata[cem_result$strata > 0])),
    n_treated = as.vector(tapply(treatment[cem_result$w > 0], 
                                cem_result$strata[cem_result$w > 0], 
                                function(x) sum(x == 1))),
    n_control = as.vector(tapply(treatment[cem_result$w > 0], 
                                cem_result$strata[cem_result$w > 0], 
                                function(x) sum(x == 0)))
  )
  strata_info[, total_n := n_treated + n_control]
  
  # Match summary
  match_summary <- list(
    method = "CEM",
    implementation = "native_cem",
    total_treated = sum(treatment),
    total_control = sum(!treatment),
    matched_treated = sum(treatment[matched_indices]),
    matched_control = sum(!treatment[matched_indices]),
    n_strata = length(unique(cem_result$strata[cem_result$strata > 0])),
    automatic_binning = automatic_binning,
    k2k_matching = k2k,
    cutpoints_used = cutpoints
  )
  
  if (verbose) {
    cat("\nMatching completed!\n")
    cat("Matched persons:", nrow(matched_persons), "\n")
    cat("Total events for matched persons:", nrow(matched_data), "\n")
    cat("Events per matched person (avg):", round(nrow(matched_data) / nrow(matched_persons), 1), "\n")
  }
  
  return(list(
    matched_data = matched_data,
    matched_persons = matched_persons,
    match_summary = match_summary,
    imbalance_measures = imbalance_measures,
    strata_info = strata_info,
    aggregation_report = aggregation_report,
    cem_object = cem_result,
    implementation_used = "native_cem"
  ))
}

#' Fallback CEM Implementation (native R, no dependencies) with Person-Level Matching
#' @keywords internal
cem_fallback_implementation <- function(data, treatment_var, person_id_var, matching_variables,
                                      automatic_binning, cutpoints, k2k, control_ratio, keep_all, 
                                      n_bins, verbose, original_data, aggregation_report) {
  
  if (verbose) {
    cat("\n=== COARSENED EXACT MATCHING (Fallback Implementation) ===\n")
    cat("Original observations:", nrow(data), "\n")
    cat("Treatment distribution:", table(data[[treatment_var]]), "\n")
  }
  
  # Work with a copy
  working_data <- copy(data)
  
  # Create binned/coarsened versions of matching variables
  coarsened_vars <- character()
  breaks_used <- list()
  
  for (var in matching_variables) {
    values <- working_data[[var]]
    coarsened_var_name <- paste0(var, "_coarsened")
    
    if (is.numeric(values)) {
      # Handle numeric variables with binning
      if (!is.null(cutpoints) && var %in% names(cutpoints)) {
        # Use custom cutpoints
        breaks <- cutpoints[[var]]
        working_data[, (coarsened_var_name) := cut(get(var), breaks = breaks, include.lowest = TRUE)]
        breaks_used[[var]] <- breaks
        if (verbose) {
          cat("Variable", var, ": using custom cutpoints", paste(breaks, collapse = ", "), "\n")
        }
      } else if (automatic_binning) {
        # Automatic binning using quantiles
        if (uniqueN(values, na.rm = TRUE) > n_bins) {
          quantile_probs <- seq(0, 1, length.out = n_bins + 1)
          breaks <- unique(quantile(values, probs = quantile_probs, na.rm = TRUE))
          working_data[, (coarsened_var_name) := cut(get(var), breaks = breaks, include.lowest = TRUE)]
          breaks_used[[var]] <- breaks
          if (verbose) {
            cat("Variable", var, ": automatic binning with", length(breaks) - 1, "bins\n")
          }
        } else {
          # Few unique values, keep as is
          working_data[, (coarsened_var_name) := as.factor(get(var))]
          breaks_used[[var]] <- "no_binning_needed"
          if (verbose) {
            cat("Variable", var, ": keeping original values (few unique values)\n")
          }
        }
      } else {
        # No binning, convert to factor
        working_data[, (coarsened_var_name) := as.factor(get(var))]
        breaks_used[[var]] <- "no_binning"
        if (verbose) {
          cat("Variable", var, ": no binning applied\n")
        }
      }
    } else {
      # Categorical variables - use as is
      working_data[, (coarsened_var_name) := as.factor(get(var))]
      breaks_used[[var]] <- "categorical"
      if (verbose) {
        cat("Variable", var, ": categorical, using original levels\n")
      }
    }
    
    coarsened_vars <- c(coarsened_vars, coarsened_var_name)
  }
  
  # Create strata based on unique combinations of coarsened variables
  if (verbose) cat("\nCreating matching strata...\n")
  
  # Create a unique strata identifier
  strata_cols <- coarsened_vars
  working_data[, strata_id := do.call(paste, c(.SD, sep = "_|_")), .SDcols = strata_cols]
  
  # Identify strata with both treated and control units
  strata_summary <- working_data[, .(
    n_treated = sum(get(treatment_var)),
    n_control = sum(1 - get(treatment_var)),
    total_n = .N
  ), by = strata_id]
  
  # Matched strata are those with both treated and control units
  matched_strata <- strata_summary[n_treated > 0 & n_control > 0, strata_id]
  
  if (verbose) {
    cat("Total strata created:", nrow(strata_summary), "\n")
    cat("Strata with both treated and control:", length(matched_strata), "\n")
  }
  
  # Apply proper matching within strata (always do this - don't fall back to broken keep_all)
  if (k2k || control_ratio >= 1) {
    if (k2k) {
      if (verbose) cat("Applying k-to-k (1:1) matching within strata...\n")
      actual_ratio <- 1
    } else {
      if (verbose) cat("Applying 1:", control_ratio, "matching within strata...\n")
      actual_ratio <- control_ratio
    }
    
    matched_persons_list <- list()
    
    for (stratum in matched_strata) {
      stratum_data <- working_data[strata_id == stratum]
      treated_in_stratum <- stratum_data[get(treatment_var) == 1]
      control_in_stratum <- stratum_data[get(treatment_var) == 0]
      
      n_treated <- nrow(treated_in_stratum)
      n_control <- nrow(control_in_stratum)
      
      if (n_treated > 0 && n_control > 0) {
        # Calculate how many units to match based on ratio
        if (n_control >= n_treated * actual_ratio) {
          # Enough controls for desired ratio
          n_matched_treated <- n_treated
          n_matched_control <- min(n_control, as.integer(n_treated * actual_ratio))
        } else {
          # Not enough controls, adjust treated to maintain ratio
          n_matched_control <- n_control
          n_matched_treated <- min(n_treated, as.integer(n_control / actual_ratio))
        }
        
        # Sample if necessary
        if (nrow(treated_in_stratum) > n_matched_treated) {
          treated_in_stratum <- treated_in_stratum[sample(.N, n_matched_treated)]
        }
        if (nrow(control_in_stratum) > n_matched_control) {
          control_in_stratum <- control_in_stratum[sample(.N, n_matched_control)]
        }
        
        stratum_matched <- rbind(treated_in_stratum, control_in_stratum)
        stratum_matched[, cem_weight := 1.0]
        matched_persons_list[[stratum]] <- stratum_matched
      }
    }
    
    matched_persons <- rbindlist(matched_persons_list, fill = TRUE)
    
  } else {
    # Keep all matched observations
    if (keep_all) {
      matched_persons <- working_data[strata_id %in% matched_strata]
      
      # Calculate weights for each stratum to balance treated and control
      weight_calc <- matched_persons[, .(
        n_treated = sum(get(treatment_var)),
        n_control = sum(1 - get(treatment_var))
      ), by = strata_id]
      
      # Simple weighting: weight to achieve 1:1 balance within each stratum
      weight_calc[, weight_treated := pmin(n_treated, n_control) / n_treated]
      weight_calc[, weight_control := pmin(n_treated, n_control) / n_control]
      
      # Merge weights back
      matched_persons <- merge(matched_persons, 
                           weight_calc[, .(strata_id, weight_treated, weight_control)], 
                           by = "strata_id")
      
      matched_persons[, cem_weight := ifelse(get(treatment_var) == 1, weight_treated, weight_control)]
      matched_persons[, c("weight_treated", "weight_control") := NULL]
      
    } else {
      # This would be a more complex implementation for keep_all = FALSE
      matched_persons <- working_data[strata_id %in% matched_strata]
      matched_persons[, cem_weight := 1.0]
    }
  }
  
  # Add strata numbering
  strata_mapping <- data.table(
    strata_id = matched_strata,
    cem_strata = seq_along(matched_strata)
  )
  matched_persons <- merge(matched_persons, strata_mapping, by = "strata_id")
  
  # Extract all events for matched persons
  matched_person_ids <- matched_persons[[person_id_var]]
  matched_data <- original_data[get(person_id_var) %in% matched_person_ids]
  
  # Add event_time column for compatibility with calculate_comprehensive_impact_metrics()
  if ("pre_event_period" %in% names(matched_data) && 
      "post_event_period" %in% names(matched_data) &&
      "is_treated" %in% names(matched_data)) {
    
    matched_data[, event_time := ifelse(
      is_treated == FALSE, "control",
      ifelse(is_treated == TRUE & pre_event_period == TRUE, "pre",
        ifelse(is_treated == TRUE & post_event_period == TRUE, "post", 
          NA_character_))
    )]
    
    if (verbose) {
      cat("Added event_time column with distribution:\n")
      print(table(matched_data$event_time, useNA = "ifany"))
    }
  } else {
    if (verbose) {
      cat("Warning: Cannot create event_time column - missing required columns (pre_event_period, post_event_period, is_treated)\n")
    }
  }
  
  # Add weights and strata info to matched data
  person_info <- matched_persons[, c(person_id_var, "cem_weight", "cem_strata"), with = FALSE]
  matched_data <- merge(matched_data, person_info, by = person_id_var)
  
  # Calculate L1 imbalance measures (simplified) on person-level data
  l1_before <- calculate_l1_imbalance(working_data, treatment_var, matching_variables)
  l1_after <- calculate_l1_imbalance(matched_persons, treatment_var, matching_variables)
  l1_reduction <- (l1_before - l1_after) / l1_before
  
  # Create final strata info
  strata_info <- matched_persons[, .(
    n_treated = sum(get(treatment_var)),
    n_control = sum(1 - get(treatment_var))
  ), by = cem_strata]
  strata_info[, total_n := n_treated + n_control]
  setnames(strata_info, "cem_strata", "strata")
  
  # Match summary
  match_summary <- list(
    method = "CEM",
    implementation = "fallback",
    total_treated = sum(working_data[[treatment_var]]),
    total_control = sum(1 - working_data[[treatment_var]]),
    matched_treated = sum(matched_persons[[treatment_var]]),
    matched_control = sum(1 - matched_persons[[treatment_var]]),
    n_strata = length(matched_strata),
    automatic_binning = automatic_binning,
    k2k_matching = k2k,
    control_ratio = control_ratio,
    cutpoints_used = cutpoints,
    n_bins_used = n_bins
  )
  
  # Imbalance measures
  imbalance_measures <- list(
    l1_before = l1_before,
    l1_after = l1_after,
    l1_reduction = l1_reduction,
    breaks_used = breaks_used
  )
  
  # Remove helper columns from matched_persons dataset
  matched_persons[, c(coarsened_vars, "strata_id") := NULL]
  
  if (verbose) {
    cat("\nMatching completed!\n")
    cat("Matched persons:", nrow(matched_persons), "\n")
    cat("Total events for matched persons:", nrow(matched_data), "\n")
    cat("Events per matched person (avg):", round(nrow(matched_data) / nrow(matched_persons), 1), "\n")
    cat("Matched treated:", sum(matched_persons[[treatment_var]]), "\n")
    cat("Matched control:", sum(1 - matched_persons[[treatment_var]]), "\n")
    cat("L1 imbalance reduction:", sprintf("%.1f%%", l1_reduction * 100), "\n")
  }
  
  return(list(
    matched_data = matched_data,
    matched_persons = matched_persons,
    match_summary = match_summary,
    imbalance_measures = imbalance_measures,
    strata_info = strata_info,
    aggregation_report = aggregation_report,
    cem_object = NULL,  # No native CEM object in fallback
    implementation_used = "fallback"
  ))
}

#' Calculate L1 Imbalance Measure
#' @keywords internal
calculate_l1_imbalance <- function(data, treatment_var, matching_variables) {
  # Simplified L1 calculation
  total_imbalance <- 0
  
  for (var in matching_variables) {
    if (is.numeric(data[[var]])) {
      # For numeric variables, use standardized mean difference
      treated_mean <- mean(data[get(treatment_var) == 1, get(var)], na.rm = TRUE)
      control_mean <- mean(data[get(treatment_var) == 0, get(var)], na.rm = TRUE)
      pooled_sd <- sqrt((var(data[get(treatment_var) == 1, get(var)], na.rm = TRUE) + 
                        var(data[get(treatment_var) == 0, get(var)], na.rm = TRUE)) / 2)
      if (pooled_sd > 0) {
        total_imbalance <- total_imbalance + abs(treated_mean - control_mean) / pooled_sd
      }
    } else {
      # For categorical variables, use proportion differences
      treated_props <- data[get(treatment_var) == 1, table(get(var))] / data[get(treatment_var) == 1, .N]
      control_props <- data[get(treatment_var) == 0, table(get(var))] / data[get(treatment_var) == 0, .N]
      
      # Align factor levels
      all_levels <- union(names(treated_props), names(control_props))
      treated_aligned <- sapply(all_levels, function(level) 
        ifelse(level %in% names(treated_props), treated_props[level], 0))
      control_aligned <- sapply(all_levels, function(level) 
        ifelse(level %in% names(control_props), control_props[level], 0))
      
      total_imbalance <- total_imbalance + sum(abs(treated_aligned - control_aligned))
    }
  }
  
  return(total_imbalance / length(matching_variables))
}

#' Balance Assessment for Matched Data
#'
#' Comprehensive balance assessment for matched treatment and control groups including
#' standardized mean differences, variance ratios, and distributional balance tests.
#'
#' @param matched_data Data.table with matched observations
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param balance_variables Character vector. Variables to assess balance for
#' @param weight_var Character. Name of weight variable (for weighted balance). Default: NULL
#' @param thresholds Named list. Balance thresholds: list(mean_diff = 0.1, variance_ratio = 2)
#'
#' @return A list containing:
#'   \item{balance_table}{Data.table with balance statistics for each variable}
#'   \item{overall_balance}{Overall balance summary}
#'   \item{distributional_tests}{Results of distributional balance tests}
#'   \item{balance_plots}{Data for creating balance plots}
#'
#' @examples
#' \dontrun{
#' balance_results <- assess_balance(
#'   matched_data = ps_match$matched_data,
#'   balance_variables = c("age", "education", "wage", "sector"),
#'   thresholds = list(mean_diff = 0.1, variance_ratio = 2)
#' )
#' }
#'
#' @export
assess_balance <- function(matched_data,
                          treatment_var = "is_treated",
                          balance_variables,
                          weight_var = NULL,
                          thresholds = list(mean_diff = 0.1, variance_ratio = 2)) {
  
  if (!inherits(matched_data, "data.table")) {
    stop("matched_data must be a data.table")
  }
  
  if (!treatment_var %in% names(matched_data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  missing_vars <- setdiff(balance_variables, names(matched_data))
  if (length(missing_vars) > 0) {
    stop(paste("Balance variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  # Check if weights should be used
  use_weights <- !is.null(weight_var) && weight_var %in% names(matched_data)
  
  # Calculate balance statistics
  balance_results <- list()
  
  for (var in balance_variables) {
    if (is.numeric(matched_data[[var]])) {
      # Numeric variable balance
      if (use_weights) {
        treated_stats <- matched_data[get(treatment_var) == 1, .(
          mean = weighted.mean(get(var), get(weight_var), na.rm = TRUE),
          var = sum(get(weight_var) * (get(var) - weighted.mean(get(var), get(weight_var), na.rm = TRUE))^2, na.rm = TRUE) / sum(get(weight_var), na.rm = TRUE),
          n = sum(get(weight_var), na.rm = TRUE)
        )]
        
        control_stats <- matched_data[get(treatment_var) == 0, .(
          mean = weighted.mean(get(var), get(weight_var), na.rm = TRUE),
          var = sum(get(weight_var) * (get(var) - weighted.mean(get(var), get(weight_var), na.rm = TRUE))^2, na.rm = TRUE) / sum(get(weight_var), na.rm = TRUE),
          n = sum(get(weight_var), na.rm = TRUE)
        )]
      } else {
        treated_stats <- matched_data[get(treatment_var) == 1, .(
          mean = mean(get(var), na.rm = TRUE),
          var = var(get(var), na.rm = TRUE),
          n = .N
        )]
        
        control_stats <- matched_data[get(treatment_var) == 0, .(
          mean = mean(get(var), na.rm = TRUE),
          var = var(get(var), na.rm = TRUE),
          n = .N
        )]
      }
      
      # Standardized mean difference
      pooled_sd <- sqrt((treated_stats$var + control_stats$var) / 2)
      std_mean_diff <- (treated_stats$mean - control_stats$mean) / pooled_sd
      
      # Variance ratio
      var_ratio <- treated_stats$var / control_stats$var
      
      balance_results[[var]] <- data.table(
        variable = var,
        variable_type = "numeric",
        treated_mean = treated_stats$mean,
        control_mean = control_stats$mean,
        mean_diff = treated_stats$mean - control_stats$mean,
        std_mean_diff = std_mean_diff,
        treated_var = treated_stats$var,
        control_var = control_stats$var,
        var_ratio = var_ratio,
        treated_n = treated_stats$n,
        control_n = control_stats$n,
        balance_concern = abs(std_mean_diff) > thresholds$mean_diff || 
                         var_ratio > thresholds$variance_ratio || 
                         var_ratio < (1/thresholds$variance_ratio)
      )
    } else {
      # Categorical variable balance
      if (use_weights) {
        treated_props <- matched_data[get(treatment_var) == 1, .(
          prop = sum(get(weight_var), na.rm = TRUE) / sum(matched_data[get(treatment_var) == 1, get(weight_var)], na.rm = TRUE)
        ), by = c(var)]
        
        control_props <- matched_data[get(treatment_var) == 0, .(
          prop = sum(get(weight_var), na.rm = TRUE) / sum(matched_data[get(treatment_var) == 0, get(weight_var)], na.rm = TRUE)
        ), by = c(var)]
      } else {
        treated_props <- matched_data[get(treatment_var) == 1, .(
          prop = .N / matched_data[get(treatment_var) == 1, .N]
        ), by = c(var)]
        
        control_props <- matched_data[get(treatment_var) == 0, .(
          prop = .N / matched_data[get(treatment_var) == 0, .N]
        ), by = c(var)]
      }
      
      # Merge proportions
      cat_balance <- merge(treated_props, control_props, by = var, suffixes = c("_treated", "_control"), all = TRUE)
      cat_balance[is.na(prop_treated), prop_treated := 0]
      cat_balance[is.na(prop_control), prop_control := 0]
      
      # Calculate maximum difference in proportions
      max_prop_diff <- max(abs(cat_balance$prop_treated - cat_balance$prop_control), na.rm = TRUE)
      
      balance_results[[var]] <- data.table(
        variable = var,
        variable_type = "categorical",
        treated_mean = NA_real_,
        control_mean = NA_real_,
        mean_diff = NA_real_,
        std_mean_diff = max_prop_diff, # Use this field for max prop diff
        treated_var = NA_real_,
        control_var = NA_real_,
        var_ratio = NA_real_,
        treated_n = matched_data[get(treatment_var) == 1, if(use_weights) sum(get(weight_var)) else .N],
        control_n = matched_data[get(treatment_var) == 0, if(use_weights) sum(get(weight_var)) else .N],
        balance_concern = max_prop_diff > thresholds$mean_diff
      )
    }
  }
  
  # Combine balance results
  balance_table <- rbindlist(balance_results)
  
  # Overall balance summary
  overall_balance <- list(
    n_variables = nrow(balance_table),
    n_balanced = sum(!balance_table$balance_concern),
    n_imbalanced = sum(balance_table$balance_concern),
    prop_balanced = mean(!balance_table$balance_concern),
    max_std_diff = max(abs(balance_table$std_mean_diff), na.rm = TRUE),
    mean_abs_std_diff = mean(abs(balance_table$std_mean_diff), na.rm = TRUE)
  )
  
  # Distributional tests (if applicable)
  distributional_tests <- list()
  for (var in balance_variables[balance_variables %in% names(matched_data)]) {
    if (is.numeric(matched_data[[var]])) {
      tryCatch({
        ks_test <- ks.test(matched_data[get(treatment_var) == 1, get(var)],
                          matched_data[get(treatment_var) == 0, get(var)])
        distributional_tests[[var]] <- list(
          test = "Kolmogorov-Smirnov",
          statistic = ks_test$statistic,
          p_value = ks_test$p.value
        )
      }, error = function(e) {
        distributional_tests[[var]] <- list(
          test = "Kolmogorov-Smirnov",
          statistic = NA,
          p_value = NA,
          error = e$message
        )
      })
    }
  }
  
  return(list(
    balance_table = balance_table,
    overall_balance = overall_balance,
    distributional_tests = distributional_tests,
    thresholds_used = thresholds
  ))
}

#' Match Quality Diagnostics
#'
#' Provides comprehensive diagnostics for matching quality including common support,
#' match distances, and recommendations for improving matches.
#'
#' @param matching_result List object from propensity_score_matching() or coarsened_exact_matching()
#' @param diagnostic_plots Logical. Generate data for diagnostic plots? Default: TRUE
#'
#' @return A list containing:
#'   \item{quality_summary}{Overall quality assessment}
#'   \item{match_distances}{Distribution of matching distances}
#'   \item{common_support_analysis}{Common support region analysis}
#'   \item{recommendations}{Specific recommendations for improvement}
#'   \item{diagnostic_data}{Data for creating diagnostic plots}
#'
#' @examples
#' \dontrun{
#' diagnostics <- assess_match_quality(
#'   matching_result = ps_match,
#'   diagnostic_plots = TRUE
#' )
#' print(diagnostics$quality_summary)
#' }
#'
#' @export
assess_match_quality <- function(matching_result,
                                diagnostic_plots = TRUE) {
  
  if (!is.list(matching_result)) {
    stop("matching_result must be a list object from matching functions")
  }
  
  # Check required components exist
  required_components <- c("matched_data", "match_summary")
  missing_components <- setdiff(required_components, names(matching_result))
  if (length(missing_components) > 0) {
    stop(paste("Required components missing from matching_result:", paste(missing_components, collapse = ", ")))
  }
  
  # Extract components
  matched_data <- matching_result$matched_data
  match_summary <- matching_result$match_summary
  
  # Validate match_summary has required fields
  required_summary_fields <- c("method", "total_treated", "total_control", "matched_treated", "matched_control")
  missing_summary_fields <- setdiff(required_summary_fields, names(match_summary))
  if (length(missing_summary_fields) > 0) {
    stop(paste("Required fields missing from match_summary:", paste(missing_summary_fields, collapse = ", ")))
  }
  
  # Quality metrics
  quality_metrics <- list(
    matching_method = match_summary$method,
    treatment_retention_rate = match_summary$matched_treated / match_summary$total_treated,
    control_utilization_rate = match_summary$matched_control / match_summary$total_control,
    effective_sample_size = match_summary$matched_treated + match_summary$matched_control,
    balance_achieved = if ("balance_after" %in% names(matching_result)) {
      !is.null(matching_result$balance_after)
    } else {
      NA
    }
  )
  
  # Match distance analysis (for PSM)
  match_distances <- NULL
  if ("propensity_scores" %in% names(matching_result)) {
    ps_data <- matching_result$propensity_scores
    if (!is.null(ps_data) && "propensity_score" %in% names(ps_data)) {
      match_distances <- list(
        min_distance = min(ps_data$propensity_score, na.rm = TRUE),
        max_distance = max(ps_data$propensity_score, na.rm = TRUE),
        mean_distance = mean(ps_data$propensity_score, na.rm = TRUE),
        median_distance = median(ps_data$propensity_score, na.rm = TRUE),
        distance_range = range(ps_data$propensity_score, na.rm = TRUE)
      )
    }
  }
  
  # Common support analysis
  common_support_analysis <- NULL
  if ("common_support" %in% names(matching_result)) {
    cs_info <- matching_result$common_support
    if (!is.null(cs_info)) {
      common_support_analysis <- list(
        overlap_exists = cs_info$overlap_range[2] > cs_info$overlap_range[1],
        treated_in_support_pct = cs_info$treated_in_support / match_summary$total_treated * 100,
        control_in_support_pct = cs_info$control_in_support / match_summary$total_control * 100,
        support_quality = "good" # Could be enhanced with more sophisticated assessment
      )
    }
  }
  
  # Generate recommendations
  recommendations <- character()
  
  if (quality_metrics$treatment_retention_rate < 0.8) {
    recommendations <- c(recommendations, 
                        "Low treatment retention rate - consider relaxing matching criteria")
  }
  
  if (quality_metrics$control_utilization_rate > 0.9) {
    recommendations <- c(recommendations, 
                        "Very high control utilization - may indicate limited control pool")
  }
  
  if (!is.null(common_support_analysis)) {
    if (common_support_analysis$treated_in_support_pct < 80) {
      recommendations <- c(recommendations, 
                          "Limited common support for treated units - consider trimming or different matching approach")
    }
    
    if (common_support_analysis$control_in_support_pct < 50) {
      recommendations <- c(recommendations, 
                          "Limited common support for control units - consider expanding control pool")
    }
  }
  
  if (match_summary$method == "nearest" && is.null(match_summary$caliper_value)) {
    recommendations <- c(recommendations, 
                        "Consider using caliper matching to improve match quality")
  }
  
  # Diagnostic plot data
  diagnostic_data <- list()
  if (diagnostic_plots) {
    if ("propensity_scores" %in% names(matching_result)) {
      diagnostic_data$propensity_histogram <- matching_result$propensity_scores
    }
    
    if ("matched_data" %in% names(matching_result)) {
      diagnostic_data$balance_data <- matched_data
    }
  }
  
  # Overall quality assessment
  quality_score <- (
    0.3 * pmin(1, quality_metrics$treatment_retention_rate) +
    0.2 * pmin(1, quality_metrics$control_utilization_rate) +
    0.3 * ifelse(!is.null(common_support_analysis), 
                 (common_support_analysis$treated_in_support_pct + common_support_analysis$control_in_support_pct) / 200, 
                 0.5) +
    0.2 * ifelse(length(recommendations) == 0, 1, pmax(0, 1 - length(recommendations) * 0.2))
  )
  
  quality_summary <- list(
    overall_quality_score = quality_score,
    quality_rating = cut(quality_score, 
                        breaks = c(0, 0.5, 0.7, 0.85, 1), 
                        labels = c("Poor", "Fair", "Good", "Excellent"), 
                        include.lowest = TRUE),
    key_metrics = quality_metrics,
    major_concerns = length(recommendations)
  )
  
  return(list(
    quality_summary = quality_summary,
    match_distances = match_distances,
    common_support_analysis = common_support_analysis,
    recommendations = recommendations,
    diagnostic_data = diagnostic_data
  ))
}

#' Print method for balance assessment
#' @param x A balance assessment object
#' @param ... Additional arguments passed to print
#' @method print balance_assessment
#' @export
print.balance_assessment <- function(x, ...) {
  cat("Balance Assessment Results\n")
  cat("==========================\n\n")
  
  cat("Overall Balance:\n")
  cat("  Variables assessed:", x$overall_balance$n_variables, "\n")
  cat("  Balanced variables:", x$overall_balance$n_balanced, "\n")
  cat("  Imbalanced variables:", x$overall_balance$n_imbalanced, "\n")
  cat("  Balance rate:", sprintf("%.1f%%", x$overall_balance$prop_balanced * 100), "\n")
  cat("  Max standardized difference:", sprintf("%.3f", x$overall_balance$max_std_diff), "\n")
  
  if (x$overall_balance$n_imbalanced > 0) {
    cat("\nImbalanced Variables:\n")
    imbalanced <- x$balance_table[balance_concern == TRUE]
    print(imbalanced[, .(variable, std_mean_diff, var_ratio)])
  }
  
  invisible(x)
}

#' Data Quality Diagnostics for Propensity Score Matching
#'
#' Provides comprehensive diagnostics for data quality issues that can affect
#' propensity score matching, including missing value patterns, non-finite values,
#' and categorical variable issues.
#'
#' @param data A data.table containing the data to be assessed
#' @param treatment_var Character. Name of treatment indicator variable
#' @param matching_variables Character vector. Variables to include in assessment
#' @param exact_match_vars Character vector. Variables requiring exact matches. Default: NULL
#' @param factor_level_threshold Numeric. Minimum observations per factor level. Default: 5
#'
#' @return A list containing:
#'   \item{overall_summary}{Overall data quality summary}
#'   \item{variable_analysis}{Detailed analysis by variable}
#'   \item{missing_patterns}{Missing data patterns across observations}
#'   \item{recommendations}{Specific recommendations for data cleaning}
#'
#' @examples
#' \dontrun{
#' # Assess data quality before matching
#' quality_report <- diagnose_matching_data(
#'   data = my_data,
#'   treatment_var = "is_treated",
#'   matching_variables = c("age", "education", "sector"),
#'   exact_match_vars = c("gender")
#' )
#' print(quality_report$overall_summary)
#' }
#'
#' @export
diagnose_matching_data <- function(data,
                                 treatment_var = "is_treated",
                                 matching_variables,
                                 exact_match_vars = NULL,
                                 factor_level_threshold = 5) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!treatment_var %in% names(data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  missing_vars <- setdiff(matching_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Matching variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  all_vars <- c(treatment_var, matching_variables, exact_match_vars)
  
  # Overall summary
  overall_summary <- list(
    n_observations = nrow(data),
    n_variables = length(all_vars),
    n_treated = sum(data[[treatment_var]], na.rm = TRUE),
    n_control = sum(1 - data[[treatment_var]], na.rm = TRUE),
    treatment_missing = sum(is.na(data[[treatment_var]])),
    complete_cases = sum(complete.cases(data[, all_vars, with = FALSE])),
    complete_case_rate = sum(complete.cases(data[, all_vars, with = FALSE])) / nrow(data)
  )
  
  # Variable-level analysis
  variable_analysis <- list()
  
  for (var in all_vars) {
    values <- data[[var]]
    
    # Basic statistics
    var_info <- list(
      variable = var,
      type = class(values)[1],
      n_observations = length(values),
      role = if (var == treatment_var) "treatment" 
             else if (var %in% exact_match_vars) "exact_match" 
             else "matching"
    )
    
    # Missing value analysis
    na_count <- sum(is.na(values))
    if (is.numeric(values)) {
      inf_count <- sum(is.infinite(values), na.rm = TRUE)
      nan_count <- sum(is.nan(values), na.rm = TRUE)
      var_info$na_count <- na_count
      var_info$inf_count <- inf_count
      var_info$nan_count <- nan_count
      var_info$total_issues <- na_count + inf_count + nan_count
      var_info$proportion_issues <- var_info$total_issues / length(values)
      
      if (var_info$total_issues == 0) {
        var_info$numeric_summary <- list(
          min = min(values),
          max = max(values),
          mean = mean(values),
          median = median(values),
          sd = sd(values)
        )
      }
    } else {
      empty_count <- if (is.character(values)) sum(values == "" | values == " ", na.rm = TRUE) else 0
      var_info$na_count <- na_count
      var_info$empty_count <- empty_count
      var_info$total_issues <- na_count + empty_count
      var_info$proportion_issues <- var_info$total_issues / length(values)
    }
    
    # Factor level analysis
    if (is.factor(values) || is.character(values)) {
      value_counts <- table(values, useNA = "ifany")
      var_info$n_levels <- length(value_counts)
      var_info$small_levels <- sum(value_counts < factor_level_threshold)
      var_info$level_distribution <- as.list(value_counts)
      
      # Check balance across treatment groups
      if (var != treatment_var) {
        treated_levels <- unique(data[get(treatment_var) == 1, get(var)])
        control_levels <- unique(data[get(treatment_var) == 0, get(var)])
        var_info$level_balance <- list(
          in_treated_only = setdiff(treated_levels, control_levels),
          in_control_only = setdiff(control_levels, treated_levels),
          common_levels = intersect(treated_levels, control_levels)
        )
      }
    }
    
    variable_analysis[[var]] <- var_info
  }
  
  # Missing patterns analysis
  missing_pattern <- data[, all_vars, with = FALSE]
  missing_pattern <- missing_pattern[, lapply(.SD, is.na)]
  
  # Create missing pattern summary
  pattern_counts <- missing_pattern[, .N, by = names(missing_pattern)]
  setorderv(pattern_counts, "N", -1)
  
  missing_patterns <- list(
    n_patterns = nrow(pattern_counts),
    most_common_patterns = head(pattern_counts, 10),
    variables_with_missing = names(which(sapply(missing_pattern, any))),
    pairwise_missing_correlations = if (nrow(missing_pattern) > 0) {
      tryCatch({
        cor(as.matrix(missing_pattern * 1), use = "complete.obs")
      }, error = function(e) NULL)
    } else NULL
  )
  
  # Generate recommendations
  recommendations <- character()
  
  # Check overall data quality
  if (overall_summary$complete_case_rate < 0.7) {
    recommendations <- c(recommendations, 
                        "Low complete case rate (<70%) - consider imputation strategy")
  }
  
  if (overall_summary$treatment_missing > 0) {
    recommendations <- c(recommendations,
                        "Treatment variable has missing values - this must be resolved before matching")
  }
  
  # Variable-specific recommendations
  for (var in names(variable_analysis)) {
    info <- variable_analysis[[var]]
    
    if (info$proportion_issues > 0.3) {
      recommendations <- c(recommendations,
                          paste("Variable", var, "has >30% issues - consider excluding or imputing"))
    }
    
    if ("small_levels" %in% names(info) && info$small_levels > 0) {
      recommendations <- c(recommendations,
                          paste("Variable", var, "has factor levels with <", 
                                factor_level_threshold, "observations - may cause matching issues"))
    }
    
    if ("level_balance" %in% names(info)) {
      imbalanced_levels <- length(info$level_balance$in_treated_only) + 
                          length(info$level_balance$in_control_only)
      if (imbalanced_levels > 0) {
        recommendations <- c(recommendations,
                            paste("Variable", var, "has factor levels not present in both treatment groups"))
      }
    }
  }
  
  # Treatment group balance
  if (abs(overall_summary$n_treated - overall_summary$n_control) / 
      (overall_summary$n_treated + overall_summary$n_control) > 0.8) {
    recommendations <- c(recommendations,
                        "Severe treatment group imbalance - matching may be difficult")
  }
  
  return(list(
    overall_summary = overall_summary,
    variable_analysis = variable_analysis,
    missing_patterns = missing_patterns,
    recommendations = recommendations
  ))
}

#' Print method for data quality diagnostics
#' @param x A data quality diagnostics object
#' @param ... Additional arguments passed to print
#' @method print matching_diagnostics
#' @export
print.matching_diagnostics <- function(x, ...) {
  cat("Data Quality Diagnostics for Propensity Score Matching\n")
  cat("======================================================\n\n")
  
  cat("Overall Summary:\n")
  cat("  Observations:", x$overall_summary$n_observations, "\n")
  cat("  Variables:", x$overall_summary$n_variables, "\n")
  cat("  Treated units:", x$overall_summary$n_treated, "\n")
  cat("  Control units:", x$overall_summary$n_control, "\n")
  cat("  Complete cases:", x$overall_summary$complete_cases, 
      sprintf("(%.1f%%)", x$overall_summary$complete_case_rate * 100), "\n")
  
  if (x$overall_summary$treatment_missing > 0) {
    cat("  WARNING: Treatment variable has", x$overall_summary$treatment_missing, "missing values\n")
  }
  
  cat("\nVariable Quality Issues:\n")
  for (var_name in names(x$variable_analysis)) {
    info <- x$variable_analysis[[var_name]]
    if (info$proportion_issues > 0) {
      cat(sprintf("  %s (%s): %.1f%% issues", var_name, info$type, info$proportion_issues * 100))
      if ("na_count" %in% names(info) && info$na_count > 0) {
        cat(sprintf(" (%d missing)", info$na_count))
      }
      if ("inf_count" %in% names(info) && info$inf_count > 0) {
        cat(sprintf(" (%d infinite)", info$inf_count))
      }
      if ("small_levels" %in% names(info) && info$small_levels > 0) {
        cat(sprintf(" (%d small factor levels)", info$small_levels))
      }
      cat("\n")
    }
  }
  
  if (length(x$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for (i in seq_along(x$recommendations)) {
      cat(sprintf("  %d. %s\n", i, x$recommendations[i]))
    }
  } else {
    cat("\nNo major data quality issues detected!\n")
  }
  
  cat("\nMissing Patterns:\n")
  cat("  Number of patterns:", x$missing_patterns$n_patterns, "\n")
  cat("  Variables with missing:", paste(x$missing_patterns$variables_with_missing, collapse = ", "), "\n")
  
  invisible(x)
}