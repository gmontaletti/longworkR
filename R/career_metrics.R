#' Career Evaluation: Generalized Career Trajectory Analysis
#'
#' This module provides comprehensive career trajectory analysis functionality that extends
#' beyond pre/post intervention evaluation. It analyzes employment quality, career transitions,
#' and risk indicators across any time period, integrating full-time employment status,
#' contract durations from survival analysis, salary progression, and termination risk.
#'
#' @name career_metrics
#' @author vecshift package
#' @importFrom collapse fmean fmedian fmax fvar
#' @importFrom data.table fcase
NULL

# Required packages imported via NAMESPACE

#' Calculate Career Quality Metrics Using Survival Analysis
#'
#' Calculates comprehensive career quality metrics based exclusively on survival analysis
#' results combined with employment intensity (full-time vs part-time). This data-driven
#' approach provides objective career quality assessment without relying on predefined
#' contract type hierarchies.
#'
#' @details 
#' The function computes career quality using empirical survival analysis results.
#' Contract quality scores are derived exclusively from survival data:
#' 
#' **Survival-Based Scoring:**
#' \itemize{
#'   \item Contract quality scores are based on median survival times from survival analysis
#'   \item Longer median survival times indicate higher contract quality
#'   \item Scores are normalized to a 0-1 scale with a minimum quality floor of 0.1
#'   \item Contract types not present in survival data receive a default moderate score (0.5)
#'   \item High-quality contracts are defined as those with quality scores > 0.7
#' }
#' 
#' The composite quality index combines contract quality (60% weight) with employment
#' intensity (40% weight), where full-time employment (prior >= 1) contributes more
#' to career quality than part-time employment.
#'
#' @param data A data.table containing employment records with required columns:
#'   \code{durata} (contract duration), \code{over_id} (employment period ID), 
#'   and columns specified by other parameters
#' @param survival_data Required. Pre-computed survival analysis results from 
#'   \code{estimate_contract_survival_optimized()} containing a named vector 
#'   \code{median_survival} with median survival times by contract type.
#'   Expected structure: \code{list(median_survival = c("A.01.00" = 365, "A.03.00" = 180, ...))}
#'   This parameter is mandatory as the function relies exclusively on survival data.
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods. 
#'   If NULL, analyzes entire career trajectory. Default: NULL
#' @param contract_code_column Character. Column containing contract type codes. Default: "COD_TIPOLOGIA_CONTRATTUALE"
#' @param employment_intensity_column Character. Column indicating employment intensity (prior). 
#'   Values >= 1 indicate full-time employment, 0 indicates part-time. Default: "prior"
#' @param min_spell_duration Numeric. Minimum duration (days) to include in analysis. Default: 7
#'
#' @return A data.table with comprehensive quality metrics:
#'   \item{cf}{Person identifier}
#'   \item{time_period}{Time period (if time_period_column provided)}
#'   \item{total_employment_days}{Total days in employment}
#'   \item{fulltime_employment_days}{Days in full-time employment (prior >= 1)}
#'   \item{parttime_employment_days}{Days in part-time employment (prior == 0)}
#'   \item{fulltime_employment_rate}{Proportion of employment in full-time (0-1 scale)}
#'   \item{high_quality_contract_days}{Days in high-quality contracts (survival score > 0.7)}
#'   \item{high_quality_fulltime_days}{Days in high-quality AND full-time contracts}
#'   \item{high_quality_fulltime_rate}{Proportion of employment in high-quality full-time contracts}
#'   \item{contract_quality_score}{Duration-weighted average contract quality (0-1 scale)}
#'   \item{employment_intensity_score}{Duration-weighted average employment intensity (0-1 scale)}
#'   \item{composite_quality_index}{Weighted combination: 60% contract quality + 40% intensity (0-1 scale)}
#' #'
#' @examples
#' \dontrun{
#' # Load sample employment data
#' employment_data <- readRDS("data/sample.rds")
#' 
#' # Example 1: Career quality analysis with survival data (required)
#' survival_results <- estimate_contract_survival_optimized(
#'   data = employment_data,
#'   contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
#'   duration_var = "durata",
#'   censored_var = "censored"
#' )
#' 
#' career_quality <- calculate_career_quality_metrics(
#'   data = employment_data,
#'   survival_data = survival_results
#' )
#' 
#' # View results
#' print(career_quality[, .(cf, composite_quality_index, 
#'                          fulltime_employment_rate, contract_quality_score)])
#' 
#' # Example 2: Time-period analysis (e.g., by year)
#' employment_data[, year := year(inizio)]
#' yearly_quality <- calculate_career_quality_metrics(
#'   data = employment_data,
#'   survival_data = survival_results,
#'   time_period_column = "year"
#' )
#' 
#' # Example 3: Custom parameters and minimum spell duration
#' custom_quality <- calculate_career_quality_metrics(
#'   data = employment_data,
#'   survival_data = survival_results,
#'   contract_code_column = "contract_type",
#'   employment_intensity_column = "intensity",
#'   min_spell_duration = 30  # Only include spells of 30+ days
#' )
#' }
#'
#' @seealso 
#' \code{\link{estimate_contract_survival_optimized}} for survival analysis computation,
#' \code{\link{calculate_career_transition_metrics}} for analyzing career transitions,
#' \code{\link{calculate_comprehensive_career_metrics}} for integrated career analysis
#'
#' @export
calculate_career_quality_metrics <- function(data,
                                           survival_data = NULL,
                                           id_column = "cf",
                                           time_period_column = NULL,
                                           contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
                                           employment_intensity_column = "prior",
                                           min_spell_duration = 7) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  # Validate survival_data is provided and has required structure
  if (is.null(survival_data) || !"median_survival" %in% names(survival_data)) {
    stop("survival_data is required and must contain 'median_survival' component. ",
         "Use estimate_contract_survival_optimized() to generate survival data.")
  }
  
  required_cols <- c(id_column, contract_code_column, employment_intensity_column, "durata", "over_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy with standardized names
  dt <- copy(data)
  setnames(dt, c(id_column, contract_code_column, employment_intensity_column), 
           c("cf", "contract_code", "employment_intensity"))
  
  # Filter for employment periods and minimum duration
  dt <- dt[over_id > 0 & durata >= min_spell_duration]
  
  if (nrow(dt) == 0) {
    warning("No valid employment observations found")
    return(data.table())
  }
  
  # Add time period column if specified
  if (!is.null(time_period_column) && time_period_column %in% names(data)) {
    dt[, time_period := data[[time_period_column]]]
    group_cols <- c("cf", "time_period")
  } else {
    dt[, time_period := "overall"]
    group_cols <- c("cf", "time_period")
  }
  
  # Classify employment intensity only (contract types handled via survival data)
  dt[, `:=`(
    is_fulltime = employment_intensity >= 1,
    is_parttime = employment_intensity == 0
  )]
  
  # Contract quality scores based exclusively on survival data
  # Use survival-based scoring: normalize median survival times to 0-1 scale
  median_survivals <- survival_data$median_survival
  
  # Remove NA values and ensure we have valid data
  valid_medians <- median_survivals[!is.na(median_survivals) & median_survivals > 0]
  
  if (length(valid_medians) == 0) {
    stop("No valid survival data found. All median survival times are NA or <= 0. ",
         "Please check your survival analysis results.")
  }
  
  # Normalize survival times to 0-1 scale where higher survival = higher quality
  max_survival <- max(valid_medians)
  min_survival <- min(valid_medians)
  
  # Create survival-based quality scores
  survival_scores <- if (max_survival > min_survival) {
    # Scale to 0-1 range with some minimum quality floor (0.1)
    0.1 + 0.9 * (valid_medians - min_survival) / (max_survival - min_survival)
  } else {
    # If all survival times are equal, assign moderate quality
    rep(0.5, length(valid_medians))
  }
  
  names(survival_scores) <- names(valid_medians)
  
  # Add survival-based quality scores to data
  dt[, contract_quality_score := {
    score <- as.numeric(survival_scores[contract_code])
    # Handle contract types not present in survival data
    if (any(is.na(score))) {
      missing_contracts <- unique(contract_code[is.na(score)])
      warning("Contract types not found in survival data: ", 
              paste(missing_contracts, collapse = ", "), 
              ". These will be assigned median quality score.")
      score[is.na(score)] <- 0.5
    }
    score
  }]
  
  # Employment intensity scores (normalize prior values)
  dt[, intensity_score := pmax(0, pmin(1, employment_intensity / 3))]
  
  # Calculate comprehensive quality metrics
  quality_metrics <- dt[, {
    total_days <- sum(durata, na.rm = TRUE)
    fulltime_days <- sum(durata[is_fulltime == TRUE], na.rm = TRUE)
    parttime_days <- sum(durata[is_parttime == TRUE], na.rm = TRUE)
    
    # Calculate high-quality contract days based on survival scores
    # Consider contracts with quality score > 0.7 as "high quality" (similar to permanent)
    high_quality_days <- sum(durata[contract_quality_score > 0.7], na.rm = TRUE)
    high_quality_fulltime_days <- sum(durata[contract_quality_score > 0.7 & is_fulltime == TRUE], na.rm = TRUE)
    
    # Weighted averages
    avg_contract_quality <- fmean(contract_quality_score, w = durata, na.rm = TRUE)
    avg_intensity_score <- fmean(intensity_score, w = durata, na.rm = TRUE)
    
    # Composite quality index (60% contract quality, 40% employment intensity)
    composite_quality <- 0.6 * avg_contract_quality + 0.4 * avg_intensity_score
    
    
    list(
      total_employment_days = as.double(total_days),
      fulltime_employment_days = as.double(fulltime_days),
      parttime_employment_days = as.double(parttime_days),
      fulltime_employment_rate = as.double(fulltime_days / pmax(1, total_days)),
      high_quality_contract_days = as.double(high_quality_days),
      high_quality_fulltime_days = as.double(high_quality_fulltime_days),
      high_quality_fulltime_rate = as.double(high_quality_fulltime_days / pmax(1, total_days)),
      contract_quality_score = as.double(avg_contract_quality),
      employment_intensity_score = as.double(avg_intensity_score),
      composite_quality_index = as.double(composite_quality)
    )
  }, by = group_cols]
  
  # Remove overall column if not using time periods
  if (is.null(time_period_column)) {
    quality_metrics[, time_period := NULL]
  }
  
  return(quality_metrics[])
}

#' Calculate Career Transition Metrics with Duration and Salary Analysis
#'
#' Analyzes career transitions incorporating contract duration expectations from survival
#' analysis and salary progression. Identifies moves to longer-lasting contracts and
#' salary improvements as positive career transitions.
#'
#' @param data A data.table containing employment records
#' @param survival_data Optional. Pre-computed survival analysis results with median durations
#'   by contract type. If NULL, will compute basic duration statistics.
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods. Default: NULL
#' @param contract_code_column Character. Column containing contract type codes. Default: "COD_TIPOLOGIA_CONTRATTUALE"
#' @param salary_column Character. Column containing salary information. Default: NULL
#' @param date_column Character. Name of date column. Default: "inizio"
#' @param min_transition_gap Numeric. Minimum gap (days) between jobs to count as transition. Default: 1
#'
#' @return A data.table with transition metrics:
#'   \item{cf}{Person identifier}
#'   \item{time_period}{Time period (if specified)}
#'   \item{total_transitions}{Total number of employment transitions}
#'   \item{duration_improvements}{Transitions to longer-lasting contract types}
#'   \item{duration_deteriorations}{Transitions to shorter-lasting contract types}
#'   \item{salary_improvements}{Transitions with salary increases (if salary data available)}
#'   \item{salary_deteriorations}{Transitions with salary decreases (if salary data available)}
#'   \item{fulltime_improvements}{Transitions to full-time from part-time}
#'   \item{fulltime_deteriorations}{Transitions from full-time to part-time}
#'   \item{composite_improvement_rate}{Overall rate of positive career transitions}
#'   \item{career_progression_index}{Comprehensive career progression score}
#'
#' @examples
#' \dontrun{
#' # Analyze transitions with survival data
#' survival_results <- estimate_contract_survival(employment_data)
#' transitions <- calculate_career_transition_metrics(
#'   data = employment_data,
#'   survival_data = survival_results,
#'   salary_column = "monthly_wage"
#' )
#' }
#'
#' @export
calculate_career_transition_metrics <- function(data,
                                              survival_data = NULL,
                                              id_column = "cf",
                                              time_period_column = NULL,
                                              contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
                                              salary_column = NULL,
                                              date_column = "inizio",
                                              min_transition_gap = 1) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, contract_code_column, date_column, "durata", "over_id", "prior")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, contract_code_column, date_column), 
           c("cf", "contract_code", "start_date"))
  
  # Filter for employment periods
  dt <- dt[over_id > 0]
  dt[, start_date := as.Date(start_date)]
  
  if (nrow(dt) == 0) {
    warning("No valid employment observations found")
    return(data.table())
  }
  
  # Add time period if specified
  if (!is.null(time_period_column) && time_period_column %in% names(dt)) {
    dt[, time_period := dt[[time_period_column]]]
    group_cols <- c("cf", "time_period")
  } else {
    dt[, time_period := "overall"]
    group_cols <- c("cf", "time_period")
  }
  
  # Get contract duration expectations (from survival data or compute basic stats)
  if (!is.null(survival_data) && "median_survival" %in% names(survival_data)) {
    contract_durations <- survival_data$median_survival
  } else {
    # Fallback: compute median durations from data
    contract_durations <- dt[, .(median_duration = fmedian(durata, na.rm = TRUE)), 
                            by = contract_code][, setNames(median_duration, contract_code)]
  }
  
  # Add expected durations to data
  dt[, expected_duration := as.numeric(contract_durations[contract_code])]
  dt[is.na(expected_duration), expected_duration := fmedian(durata, na.rm = TRUE)]
  
  # Include salary column if provided
  has_salary <- !is.null(salary_column) && salary_column %in% names(dt)
  if (has_salary) {
    dt[, salary := dt[[salary_column]]]
  }
  
  # Calculate transition metrics
  transition_metrics <- dt[order(cf, time_period, start_date), {
    if (.N <= 1) {
      # No transitions possible
      base_result <- list(
        total_transitions = 0L,
        duration_improvements = 0L,
        duration_deteriorations = 0L,
        fulltime_improvements = 0L,
        fulltime_deteriorations = 0L,
        composite_improvement_rate = 0.0,
        career_progression_index = 0.0
      )
      
      if (has_salary) {
        base_result$salary_improvements <- 0L
        base_result$salary_deteriorations <- 0L
      }
      
      base_result
    } else {
      # Identify transitions (consecutive employment periods)
      transitions_dt <- data.table(
        from_contract = contract_code[-.N],
        to_contract = contract_code[-1],
        from_duration_exp = expected_duration[-.N],
        to_duration_exp = expected_duration[-1],
        from_fulltime = prior[-.N] >= 1,
        to_fulltime = prior[-1] >= 1
      )
      
      if (has_salary && any(!is.na(salary))) {
        transitions_dt[, `:=`(
          from_salary = salary[-.N],
          to_salary = salary[-1]
        )]
      }
      
      n_transitions <- nrow(transitions_dt)
      
      # Duration improvements/deteriorations
      duration_improvements <- sum(transitions_dt$to_duration_exp > transitions_dt$from_duration_exp * 1.1, na.rm = TRUE)
      duration_deteriorations <- sum(transitions_dt$to_duration_exp < transitions_dt$from_duration_exp * 0.9, na.rm = TRUE)
      
      # Full-time improvements/deteriorations
      fulltime_improvements <- sum(!transitions_dt$from_fulltime & transitions_dt$to_fulltime, na.rm = TRUE)
      fulltime_deteriorations <- sum(transitions_dt$from_fulltime & !transitions_dt$to_fulltime, na.rm = TRUE)
      
      # Salary improvements/deteriorations (if available)
      if (has_salary) {
        valid_salary_transitions <- !is.na(transitions_dt$from_salary) & !is.na(transitions_dt$to_salary)
        salary_improvements <- sum(transitions_dt$to_salary > transitions_dt$from_salary * 1.05 & valid_salary_transitions, na.rm = TRUE)
        salary_deteriorations <- sum(transitions_dt$to_salary < transitions_dt$from_salary * 0.95 & valid_salary_transitions, na.rm = TRUE)
      }
      
      # Composite improvement rate
      total_improvements <- duration_improvements + fulltime_improvements
      if (has_salary) total_improvements <- total_improvements + salary_improvements
      
      improvement_rate <- total_improvements / pmax(1, n_transitions)
      
      # Career progression index (0-1 scale)
      progression_components <- c(
        duration_improvements / pmax(1, n_transitions),
        fulltime_improvements / pmax(1, n_transitions)
      )
      
      if (has_salary) {
        progression_components <- c(progression_components, salary_improvements / pmax(1, n_transitions))
      }
      
      progression_index <- fmean(progression_components)
      
      result <- list(
        total_transitions = as.integer(n_transitions),
        duration_improvements = as.integer(duration_improvements),
        duration_deteriorations = as.integer(duration_deteriorations),
        fulltime_improvements = as.integer(fulltime_improvements),
        fulltime_deteriorations = as.integer(fulltime_deteriorations),
        composite_improvement_rate = as.double(improvement_rate),
        career_progression_index = as.double(progression_index)
      )
      
      if (has_salary) {
        result$salary_improvements <- as.integer(salary_improvements)
        result$salary_deteriorations <- as.integer(salary_deteriorations)
      }
      
      result
    }
  }, by = group_cols]
  
  # Remove overall column if not using time periods
  if (is.null(time_period_column)) {
    transition_metrics[, time_period := NULL]
  }
  
  return(transition_metrics[])
}

#' Calculate Career Risk Metrics Using Survival Analysis
#'
#' Computes career risk indicators using survival analysis, incorporating termination
#' hazards and contract stability risks. Higher termination risks are associated with
#' longer contract durations due to higher opportunity costs.
#'
#' @param data A data.table containing employment records
#' @param survival_data Optional. Pre-computed survival analysis results. If NULL, will
#'   compute basic risk measures.
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods. Default: NULL
#' @param contract_code_column Character. Column containing contract type codes. Default: "COD_TIPOLOGIA_CONTRATTUALE"
#'
#' @return A data.table with risk metrics:
#'   \item{cf}{Person identifier}
#'   \item{time_period}{Time period (if specified)}
#'   \item{average_termination_risk}{Average termination risk across contracts}
#'   \item{risk_adjusted_stability}{Stability score adjusted for termination risk}
#'   \item{high_risk_exposure_days}{Days in high-risk contract types}
#'   \item{risk_diversification_index}{Measure of risk diversification across contract types}
#'   \item{career_risk_score}{Overall career risk assessment (0-1, higher = riskier)}
#'
#' @examples
#' \dontrun{
#' # Calculate risk metrics with survival analysis
#' survival_results <- estimate_contract_survival(employment_data)
#' risk_metrics <- calculate_career_risk_metrics(
#'   data = employment_data,
#'   survival_data = survival_results
#' )
#' }
#'
#' @export
calculate_career_risk_metrics <- function(data,
                                        survival_data = NULL,
                                        id_column = "cf",
                                        time_period_column = NULL,
                                        contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE") {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, contract_code_column, "durata", "over_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, contract_code_column), c("cf", "contract_code"))
  
  # Filter for employment periods
  dt <- dt[over_id > 0]
  
  if (nrow(dt) == 0) {
    warning("No valid employment observations found")
    return(data.table())
  }
  
  # Add time period if specified
  if (!is.null(time_period_column) && time_period_column %in% names(dt)) {
    dt[, time_period := dt[[time_period_column]]]
    group_cols <- c("cf", "time_period")
  } else {
    dt[, time_period := "overall"]
    group_cols <- c("cf", "time_period")
  }
  
  # Calculate or assign risk scores
  if (!is.null(survival_data) && "survival_fits" %in% names(survival_data)) {
    # Extract hazard information from survival fits
    # Simplified approach: use median survival as inverse risk proxy
    if ("median_survival" %in% names(survival_data)) {
      median_survivals <- survival_data$median_survival
      # Higher median survival = lower risk (inverse relationship)
      # Normalize to 0-1 scale where 1 = highest risk
      max_median <- max(median_survivals, na.rm = TRUE)
      # Ensure risk scores are bounded between 0 and 1
      risk_scores <- pmin(1, pmax(0, 1 - (median_survivals / max_median)))
      names(risk_scores) <- names(median_survivals)
    } else {
      # Fallback: basic risk assignment
      risk_scores <- rep(0.5, length(unique(dt$contract_code)))
      names(risk_scores) <- unique(dt$contract_code)
    }
  } else {
    # Basic risk scoring based on contract duration variability
    contract_stats <- dt[, .(
      median_dur = fmedian(durata, na.rm = TRUE),
      var_dur = fvar(durata, na.rm = TRUE)
    ), by = contract_code]
    
    # Higher variability = higher risk
    max_var <- max(contract_stats$var_dur, na.rm = TRUE)
    contract_stats[, risk_score := pmin(1, pmax(0, var_dur / max_var))]
    risk_scores <- setNames(contract_stats$risk_score, contract_stats$contract_code)
  }
  
  # Add risk scores to data
  dt[, contract_risk := as.numeric(risk_scores[contract_code])]
  dt[is.na(contract_risk), contract_risk := 0.5]  # Default moderate risk
  
  # Calculate risk metrics
  risk_metrics <- dt[, {
    total_days <- sum(durata, na.rm = TRUE)
    
    # Weighted average risk
    avg_risk <- fmean(contract_risk, w = durata, na.rm = TRUE)
    
    # Risk-adjusted stability (lower risk = higher stability)
    risk_adjusted_stability <- 1 - avg_risk
    
    # High-risk exposure (contracts with risk > 0.7)
    high_risk_days <- sum(durata[contract_risk > 0.7], na.rm = TRUE)
    
    # Risk diversification (Shannon entropy of contract types)
    unique_contracts <- length(unique(contract_code))
    if (unique_contracts > 1) {
      # Aggregate durations by contract type for proper entropy calculation
      contract_type_days <- tapply(durata, contract_code, sum, na.rm = TRUE)
      contract_proportions <- contract_type_days / total_days
      contract_entropy <- -sum(contract_proportions * log(contract_proportions + 1e-10))
      # Normalize by maximum possible entropy for this number of contract types
      max_entropy <- log(unique_contracts)
      risk_diversification <- if (max_entropy > 0) contract_entropy / max_entropy else 0
      # Ensure diversification is bounded to [0, 1]
      risk_diversification <- pmin(1, pmax(0, risk_diversification))
    } else {
      risk_diversification <- 0
    }
    
    # Overall career risk score (0-1, higher = riskier)
    high_risk_rate <- high_risk_days / total_days
    lack_of_diversification <- 1 - risk_diversification
    
    career_risk_score <- (
      0.4 * avg_risk +                           # Average contract risk
      0.3 * high_risk_rate +                     # High-risk exposure
      0.3 * lack_of_diversification              # Lack of diversification
    )
    
    # Ensure final risk score is bounded to [0, 1]
    career_risk_score <- pmin(1, pmax(0, career_risk_score))
    
    list(
      average_termination_risk = as.double(avg_risk),
      risk_adjusted_stability = as.double(risk_adjusted_stability),
      high_risk_exposure_days = as.double(high_risk_days),
      high_risk_exposure_rate = as.double(high_risk_days / total_days),
      risk_diversification_index = as.double(risk_diversification),
      career_risk_score = as.double(career_risk_score)
    )
  }, by = group_cols]
  
  # Remove overall column if not using time periods
  if (is.null(time_period_column)) {
    risk_metrics[, time_period := NULL]
  }
  
  return(risk_metrics[])
}

#' Calculate Career Stability Metrics for General Career Analysis
#'
#' Calculates employment stability metrics for generalized career trajectory analysis,
#' extending beyond pre/post event evaluation. Analyzes employment rates, spell durations,
#' and job turnover measures across any time period or overall career.
#'
#' @param data A data.table containing employment records
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods.
#'   If NULL, analyzes entire career trajectory. Default: NULL
#' @param date_column Character. Name of date column. Default: "inizio"
#' @param employment_indicator Character. Column indicating employment status. Default: "over_id"
#' @param min_spell_duration Numeric. Minimum duration (days) to count as employment spell. Default: 7
#'
#' @return A data.table with employment stability metrics:
#'   \item{cf}{Person identifier}
#'   \item{time_period}{Time period (if time_period_column provided)}
#'   \item{days_employed}{Total days employed in period}
#'   \item{days_unemployed}{Total days unemployed in period}
#'   \item{employment_rate}{Proportion of period employed}
#'   \item{employment_spells}{Number of distinct employment periods}
#'   \item{unemployment_spells}{Number of distinct unemployment periods}
#'   \item{avg_employment_spell}{Average duration of employment spells}
#'   \item{avg_unemployment_spell}{Average duration of unemployment spells}
#'   \item{max_employment_spell}{Longest employment spell in period}
#'   \item{job_turnover_rate}{Employment spells per year in period}
#'   \item{employment_stability_index}{Composite stability measure (0-1)}
#'
#' @examples
#' \dontrun{
#' # Analyze overall career stability
#' career_stability <- calculate_career_stability_metrics(
#'   data = employment_data,
#'   min_spell_duration = 14
#' )
#' 
#' # Analyze stability by year
#' yearly_stability <- calculate_career_stability_metrics(
#'   data = employment_data,
#'   time_period_column = "year"
#' )
#' }
#'
#' @export
#' @importFrom collapse fmean fmax
#' @importFrom data.table rleid fifelse
calculate_career_stability_metrics <- function(data,
                                             id_column = "cf",
                                             time_period_column = NULL,
                                             date_column = "inizio",
                                             employment_indicator = "over_id",
                                             min_spell_duration = 7) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, date_column, employment_indicator, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy with standardized column names
  dt <- copy(data)
  setnames(dt, c(id_column, date_column, employment_indicator), 
           c("cf", "start_date", "employment_status"))
  
  # Filter for minimum duration
  dt <- dt[durata >= min_spell_duration]
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Add time period column if specified
  if (!is.null(time_period_column) && time_period_column %in% names(dt)) {
    dt[, time_period := dt[[time_period_column]]]
    group_cols <- c("cf", "time_period")
  } else {
    dt[, time_period := "overall"]
    group_cols <- c("cf", "time_period")
  }
  
  # Create employment indicator using efficient fifelse
  dt[, employed := fifelse(employment_status > 0, 1.0, 0.0)]
  
  # Single-pass comprehensive calculation using optimized data.table operations
  result <- dt[order(cf, time_period, start_date), {
    
    # Vectorized basic metrics
    emp_mask <- employed == 1.0
    unemp_mask <- employed == 0.0
    
    days_employed <- sum(durata[emp_mask], na.rm = TRUE)
    days_unemployed <- sum(durata[unemp_mask], na.rm = TRUE)
    total_days <- days_employed + days_unemployed
    
    # Efficient employment rate calculation
    employment_rate <- fifelse(total_days > 0, days_employed / total_days, 0)
    
    # Optimized spell calculation using rleid for run-length encoding
    if (.N <= 1) {
      # Handle single observation case efficiently
      list(
        days_employed = as.double(days_employed),
        days_unemployed = as.double(days_unemployed),
        total_days = as.double(total_days),
        total_observations = as.double(.N),
        employment_rate = as.double(employment_rate),
        employment_spells = as.double(fifelse(days_employed > 0, 1.0, 0.0)),
        unemployment_spells = as.double(fifelse(days_unemployed > 0, 1.0, 0.0)),
        avg_employment_spell = as.double(days_employed),
        avg_unemployment_spell = as.double(days_unemployed),
        max_employment_spell = as.double(days_employed),
        max_unemployment_spell = as.double(days_unemployed),
        job_turnover_rate = as.double(0.0),
        employment_stability_index = as.double(employment_rate)
      )
    } else {
      # Vectorized spell identification using efficient rleid
      spell_groups <- rleid(employed)
      
      # Pre-aggregate spell statistics using data.table for safety
      spell_dt <- data.table(spell_id = spell_groups, employed = employed, durata = durata)
      spell_stats <- spell_dt[, .(spell_duration = sum(durata)), by = .(spell_id, employed)]
      
      # Extract employment and unemployment spell durations efficiently
      emp_spell_durations <- spell_stats[employed == 1.0, spell_duration]
      unemp_spell_durations <- spell_stats[employed == 0.0, spell_duration]
      
      # Use length() for counting - consistent scalar behavior
      n_emp_spells <- length(emp_spell_durations)
      n_unemp_spells <- length(unemp_spell_durations)
      
      # Robust statistics handling empty vectors
      avg_emp_spell <- if (n_emp_spells > 0) as.double(fmean(emp_spell_durations)) else 0.0
      avg_unemp_spell <- if (n_unemp_spells > 0) as.double(fmean(unemp_spell_durations)) else 0.0
      max_emp_spell <- if (n_emp_spells > 0) as.double(fmax(emp_spell_durations)) else 0.0
      max_unemp_spell <- if (n_unemp_spells > 0) as.double(fmax(unemp_spell_durations)) else 0.0
      
      # Efficient turnover and stability calculations
      turnover_rate <- n_emp_spells / pmax(total_days / 365.25, 1/365.25)
      
      stability_index <- pmin(1, (
        0.4 * employment_rate +
        0.3 * pmin(1, max_emp_spell / 365) +
        0.2 * pmax(0, 1 - pmin(1, n_emp_spells / 4)) +
        0.1 * pmin(1, avg_emp_spell / 90)
      ))
      
      list(
        days_employed = as.double(days_employed),
        days_unemployed = as.double(days_unemployed),
        total_days = as.double(total_days),
        total_observations = as.double(.N),
        employment_rate = as.double(employment_rate),
        employment_spells = as.double(n_emp_spells),
        unemployment_spells = as.double(n_unemp_spells),
        avg_employment_spell = as.double(avg_emp_spell),
        avg_unemployment_spell = as.double(avg_unemp_spell),
        max_employment_spell = as.double(max_emp_spell),
        max_unemployment_spell = as.double(max_unemp_spell),
        job_turnover_rate = as.double(turnover_rate),
        employment_stability_index = as.double(stability_index)
      )
    }
  }, by = group_cols]
  
  # Clean up temporary column
  dt[, employed := NULL]
  
  # Remove overall column if not using time periods
  if (is.null(time_period_column)) {
    result[, time_period := NULL]
  }
  
  return(result[])
}

#' Calculate Career Complexity Metrics for General Career Analysis
#'
#' Calculates career complexity metrics for generalized career trajectory analysis,
#' extending beyond pre/post event evaluation. Analyzes concurrent employment patterns,
#' employment diversity measures, and complexity indices across any time period.
#'
#' @param data A data.table containing employment records
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods.
#'   If NULL, analyzes entire career trajectory. Default: NULL
#' @param complexity_variables Character vector. Variables to use for complexity calculation.
#'   Default: c("over_id", "arco", "prior")
#'
#' @return A data.table with career complexity metrics:
#'   \item{cf}{Person identifier}
#'   \item{time_period}{Time period (if time_period_column provided)}
#'   \item{max_concurrent_jobs}{Maximum number of concurrent jobs}
#'   \item{avg_concurrent_jobs}{Average number of concurrent jobs}
#'   \item{concurrent_employment_days}{Days with multiple concurrent jobs}
#'   \item{concurrent_employment_rate}{Proportion of employment with multiple jobs}
#'   \item{employment_diversity_index}{Shannon diversity index of employment types}
#'   \item{job_complexity_score}{Overall job complexity score}
#'   \item{career_fragmentation_index}{Measure of career fragmentation}
#'
#' @examples
#' \dontrun{
#' # Analyze overall career complexity
#' career_complexity <- calculate_career_complexity_metrics(
#'   data = employment_data,
#'   complexity_variables = c("over_id", "arco", "sector", "contract_type")
#' )
#' 
#' # Analyze complexity by year
#' yearly_complexity <- calculate_career_complexity_metrics(
#'   data = employment_data,
#'   time_period_column = "year"
#' )
#' }
#'
#' @export
#' @importFrom collapse fmean fmax
calculate_career_complexity_metrics <- function(data,
                                              id_column = "cf",
                                              time_period_column = NULL,
                                              complexity_variables = c("over_id", "arco", "prior")) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for complexity variables
  available_vars <- intersect(complexity_variables, names(data))
  if (length(available_vars) == 0) {
    stop("None of the specified complexity variables found in data")
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, id_column, "cf")
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Add time period column if specified
  if (!is.null(time_period_column) && time_period_column %in% names(dt)) {
    dt[, time_period := dt[[time_period_column]]]
    group_cols <- c("cf", "time_period")
  } else {
    dt[, time_period := "overall"]
    group_cols <- c("cf", "time_period")
  }
  
  # Calculate concurrent job metrics (if arco available)
  concurrent_metrics <- if ("arco" %in% available_vars) {
    dt[, {
      emp_data <- .SD[over_id > 0]
      max_conc <- if (nrow(emp_data) > 0) as.numeric(fmax(emp_data$arco, na.rm = TRUE)) else 0.0
      avg_conc <- if (nrow(emp_data) > 0) as.numeric(fmean(emp_data$arco, na.rm = TRUE)) else 0.0
      conc_days <- if (nrow(emp_data) > 0) as.double(sum(emp_data$durata[emp_data$arco > 1], na.rm = TRUE)) else 0.0
      total_days <- if (nrow(emp_data) > 0) as.double(sum(emp_data$durata, na.rm = TRUE)) else 0.0
      
      list(
        max_concurrent_jobs = as.double(max_conc),
        avg_concurrent_jobs = as.double(avg_conc),
        concurrent_employment_days = as.double(conc_days),
        total_employment_days = as.double(total_days)
      )
    }, by = group_cols]
  } else {
    # Default values if arco not available
    dt[, .(
      max_concurrent_jobs = 1.0,
      avg_concurrent_jobs = 1.0,
      concurrent_employment_days = 0.0,
      total_employment_days = as.double(sum(durata[over_id > 0], na.rm = TRUE))
    ), by = group_cols]
  }
  
  concurrent_metrics[, concurrent_employment_rate := 
    as.double(concurrent_employment_days) / pmax(1.0, as.double(total_employment_days))]
  
  # Calculate diversity metrics using specified complexity variables
  diversity_metrics <- dt[, {
    diversity_components <- list()
    
    # Calculate employment diversity based on complexity variables
    if (any(over_id > 0)) {
      emp_data <- .SD[over_id > 0]
      
      # Use available complexity variables for diversity calculation
      diversity_vars <- intersect(available_vars, c("prior", "COD_TIPOLOGIA_CONTRATTUALE", "qualifica", "ateco"))
      
      if (length(diversity_vars) > 0) {
        # If prior is available, use it for employment intensity diversity
        if ("prior" %in% diversity_vars) {
          emp_types <- emp_data$prior
          if (length(emp_types) > 0) {
            if (length(unique(emp_types)) == 1) {
              employment_diversity <- 0.0
            } else {
              type_props <- table(emp_types) / length(emp_types)
              employment_diversity <- as.double(-sum(type_props * log(type_props)))
            }
            diversity_components$employment_diversity_index <- employment_diversity
          } else {
            diversity_components$employment_diversity_index <- 0.0
          }
        } else {
          # Fallback: use the first available complexity variable
          first_var <- diversity_vars[1]
          if (first_var %in% names(emp_data)) {
            emp_types <- emp_data[[first_var]]
            if (length(emp_types) > 0 && !all(is.na(emp_types))) {
              unique_types <- unique(emp_types[!is.na(emp_types)])
              if (length(unique_types) == 1) {
                employment_diversity <- 0.0
              } else {
                type_props <- table(emp_types[!is.na(emp_types)]) / length(emp_types[!is.na(emp_types)])
                employment_diversity <- as.double(-sum(type_props * log(type_props)))
              }
              diversity_components$employment_diversity_index <- employment_diversity
            } else {
              diversity_components$employment_diversity_index <- 0.0
            }
          } else {
            diversity_components$employment_diversity_index <- 0.0
          }
        }
      } else {
        diversity_components$employment_diversity_index <- 0.0
      }
    } else {
      diversity_components$employment_diversity_index <- 0.0
    }
    
    diversity_components
  }, by = group_cols]
  
  # Calculate fragmentation index
  fragmentation_metrics <- dt[order(cf, time_period, inizio), {
    if (.N <= 1) {
      list(career_fragmentation_index = 0.0)
    } else {
      # Count employment/unemployment transitions
      employment_status <- over_id > 0
      transitions <- as.double(sum(diff(as.integer(employment_status)) != 0, na.rm = TRUE))
      
      # Normalize by period length (in years)
      period_length_years <- as.double(sum(durata, na.rm = TRUE)) / 365.25
      fragmentation_rate <- transitions / pmax(1.0, period_length_years)
      
      list(career_fragmentation_index = as.double(pmin(1.0, fragmentation_rate / 4.0))) # Scale to 0-1
    }
  }, by = group_cols]
  
  # Merge all metrics
  complexity_metrics <- Reduce(function(x, y) merge(x, y, by = group_cols, all = TRUE),
                               list(concurrent_metrics, diversity_metrics, fragmentation_metrics))
  
  # Calculate overall complexity score
  complexity_metrics[, job_complexity_score := as.double(pmin(1.0, (
    0.3 * pmin(1.0, max_concurrent_jobs / 3.0) +
    0.3 * pmin(1.0, concurrent_employment_rate) +
    0.2 * pmin(1.0, employment_diversity_index / 2.0) +
    0.2 * career_fragmentation_index
  )))]
  
  # Remove overall column if not using time periods
  if (is.null(time_period_column)) {
    complexity_metrics[, time_period := NULL]
  }
  
  return(complexity_metrics[])
}

#' Comprehensive Career Metrics Analysis
#'
#' Calculates all career evaluation metrics (quality, transitions, risk) in a single
#' function call with consistent formatting and validation. Provides unified analysis
#' of career trajectories with optional survival analysis integration.
#'
#' @param data A data.table containing employment records
#' @param survival_data Optional. Pre-computed survival analysis results for enhanced
#'   transition and risk analysis.
#' @param metrics Character vector. Metrics to calculate. Options:
#'   c("quality", "transitions", "risk", "stability", "complexity", "all"). Default: "all"
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param time_period_column Character. Optional column for grouping by time periods. Default: NULL
#' @param output_format Character. Output format: "wide", "long", or "list". Default: "wide"
#' @param contract_code_column Character. Column containing contract type codes. Default: "COD_TIPOLOGIA_CONTRATTUALE"
#' @param salary_column Character. Column containing salary information. Default: NULL
#'
#' @return Based on output_format:
#'   \item{wide}{Single data.table with all metrics as columns}
#'   \item{long}{Long-format data.table with metric_name and metric_value columns}
#'   \item{list}{Named list with separate data.tables for each metric type}
#'
#' @examples
#' \dontrun{
#' # Comprehensive career analysis
#' career_metrics <- calculate_comprehensive_career_metrics(
#'   data = employment_data,
#'   metrics = "all",
#'   salary_column = "monthly_wage"
#' )
#' 
#' # Quality, stability and risk analysis only
#' quality_stability_risk <- calculate_comprehensive_career_metrics(
#'   data = employment_data,
#'   metrics = c("quality", "stability", "risk"),
#'   output_format = "list"
#' )
#' 
#' # Time-period analysis
#' yearly_metrics <- calculate_comprehensive_career_metrics(
#'   data = employment_data,
#'   time_period_column = "year",
#'   output_format = "wide"
#' )
#' }
#'
#' @export
calculate_comprehensive_career_metrics <- function(data,
                                                 survival_data = NULL,
                                                 metrics = "all",
                                                 id_column = "cf",
                                                 time_period_column = NULL,
                                                 output_format = "wide",
                                                 contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
                                                 salary_column = NULL) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!"all" %in% metrics) {
    valid_metrics <- c("quality", "transitions", "risk", "stability", "complexity")
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
      stop(paste("Invalid metrics specified:", paste(invalid_metrics, collapse = ", ")))
    }
  } else {
    metrics <- c("quality", "transitions", "risk", "stability", "complexity")
  }
  
  if (!output_format %in% c("wide", "long", "list")) {
    stop("output_format must be one of: 'wide', 'long', 'list'")
  }
  
  # Calculate requested metrics
  metric_results <- list()
  
  if ("quality" %in% metrics) {
    metric_results$quality <- calculate_career_quality_metrics(
      data = data,
      survival_data = survival_data,
      id_column = id_column,
      time_period_column = time_period_column,
      contract_code_column = contract_code_column
    )
  }
  
  if ("transitions" %in% metrics) {
    metric_results$transitions <- calculate_career_transition_metrics(
      data = data,
      survival_data = survival_data,
      id_column = id_column,
      time_period_column = time_period_column,
      contract_code_column = contract_code_column,
      salary_column = salary_column
    )
  }
  
  if ("risk" %in% metrics) {
    metric_results$risk <- calculate_career_risk_metrics(
      data = data,
      survival_data = survival_data,
      id_column = id_column,
      time_period_column = time_period_column,
      contract_code_column = contract_code_column
    )
  }
  
  if ("stability" %in% metrics) {
    metric_results$stability <- calculate_career_stability_metrics(
      data = data,
      id_column = id_column,
      time_period_column = time_period_column
    )
  }
  
  if ("complexity" %in% metrics) {
    metric_results$complexity <- calculate_career_complexity_metrics(
      data = data,
      id_column = id_column,
      time_period_column = time_period_column
    )
  }
  
  # Return based on output format
  if (output_format == "list") {
    return(metric_results)
  }
  
  # Merge all metrics for wide or long format
  if (length(metric_results) == 0) {
    warning("No metrics calculated")
    return(data.table())
  }
  
  # Determine merge columns
  merge_cols <- if (is.null(time_period_column)) "cf" else c("cf", "time_period")
  
  # Merge all metric tables
  merged_metrics <- Reduce(function(x, y) {
    merge(x, y, by = merge_cols, all = TRUE)
  }, metric_results)
  
  if (output_format == "wide") {
    return(merged_metrics)
  }
  
  # Convert to long format
  id_vars <- merge_cols
  measure_vars <- setdiff(names(merged_metrics), id_vars)
  
  long_metrics <- melt(merged_metrics, id.vars = id_vars, measure.vars = measure_vars,
                       variable.name = "metric_name", value.name = "metric_value")
  
  # Add metric category
  long_metrics[, metric_category := fcase(
    grepl("quality|fulltime|permanent|intensity|composite", metric_name), "quality",
    grepl("transition|improvement|deterioration|progression", metric_name), "transitions",
    grepl("risk|exposure|diversification", metric_name), "risk",
    grepl("employment_rate|employment_spells|turnover|employment_stability", metric_name), "stability",
    grepl("concurrent|diversity|complexity|fragmentation", metric_name), "complexity",
    default = "other"
  )]
  
  return(long_metrics[])
}