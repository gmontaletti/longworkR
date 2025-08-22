#' Impact Evaluation: Pre/Post Event Metrics
#'
#' This module provides comprehensive metric calculation functionality for impact evaluation
#' studies. It calculates employment stability, contract quality, career complexity, and
#' transition pattern metrics for both pre- and post-event periods.
#'
#' @name impact_metrics
#' @author vecshift package
NULL

# Load collapse library for high-performance statistical functions
# Provides 3-7x speed improvements for aggregation operations
library(collapse)

#' Calculate Employment Stability Metrics
#'
#' Calculates employment stability metrics for pre- and post-event periods including
#' employment rates, spell durations, and job turnover measures.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param date_column Character. Name of date column. Default: "inizio"
#' @param employment_indicator Character. Column indicating employment status. Default: "over_id"
#' @param min_spell_duration Numeric. Minimum duration (days) to count as employment spell. Default: 7
#'
#' @return A data.table with employment stability metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
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
#' stability_metrics <- calculate_employment_stability_metrics(
#'   data = event_data,
#'   min_spell_duration = 14
#' )
#' }
#'
#' @export
calculate_employment_stability_metrics <- function(data,
                                                 id_column = "cf",
                                                 period_column = "event_period",
                                                 date_column = "inizio",
                                                 employment_indicator = "over_id",
                                                 min_spell_duration = 7) {
  
  # Refactored to use career stability metrics as basis engine
  # This eliminates code duplication and ensures consistency
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, date_column, employment_indicator, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Use career stability metrics as basis engine with period grouping
  stability_metrics <- calculate_career_stability_metrics(
    data = data,
    id_column = id_column,
    time_period_column = period_column,
    date_column = date_column,
    employment_indicator = employment_indicator,
    min_spell_duration = min_spell_duration
  )
  
  # Rename period column to match expected output
  if (period_column != "period" && period_column %in% names(stability_metrics)) {
    setnames(stability_metrics, period_column, "period")
  }
  
  return(stability_metrics[])
}

#' Calculate Contract Quality Metrics
#'
#' Calculates contract quality metrics including temporary to permanent transitions,
#' contract type distributions, and quality improvements over time.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param contract_code_column Character. Column containing actual contract type codes. Default: "COD_TIPOLOGIA_CONTRATTUALE"
#' @param permanent_codes Character vector. Contract codes indicating permanent contracts. Default: c("C.01.00")
#' @param temporary_codes Character vector. Contract codes indicating temporary contracts. Default: c("A.03.00", "A.03.01", "A.09.00")
#' @param internship_codes Character vector. Contract codes indicating internship/apprenticeship contracts. Default: c("A.07.00", "A.07.01")
#'
#' @return A data.table with contract quality metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{permanent_contract_days}{Days in permanent contracts}
#'   \item{temporary_contract_days}{Days in temporary contracts}
#'   \item{permanent_contract_rate}{Proportion of employment in permanent contracts}
#'   \item{internship_contract_rate}{Proportion of employment in internship contracts (if internship_codes provided)}
#'   \item{internship_contract_days}{Days in internship contracts (if internship_codes provided)}
#'   \item{temp_to_perm_transitions}{Number of temporary to permanent transitions}
#'   \item{temp_to_internship_transitions}{Number of temporary to internship transitions (if internship_codes provided)}
#'   \item{internship_to_perm_transitions}{Number of internship to permanent transitions (if internship_codes provided)}
#'   \item{perm_to_temp_transitions}{Number of permanent to temporary transitions}
#'   \item{contract_stability_trend}{Trend in contract stability over time}
#'   \item{average_contract_quality}{Average contract quality score}
#'   \item{contract_improvement_rate}{Rate of contract quality improvement}
#'
#' @examples
#' \dontrun{
#' # Example with Italian employment contract codes
#' quality_metrics <- calculate_contract_quality_metrics(
#'   data = event_data,
#'   contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
#'   permanent_codes = c("C.01.00"),  # Permanent contract codes
#'   temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),  # Fixed-term contract codes
#'   internship_codes = c("A.07.00", "A.07.01")  # Apprenticeship/internship codes
#' )
#' 
#' # Example with custom contract column and codes
#' quality_metrics <- calculate_contract_quality_metrics(
#'   data = event_data,
#'   contract_code_column = "contract_type",
#'   permanent_codes = c("PERMANENT", "INDETERMINATE"),
#'   temporary_codes = c("FIXED_TERM", "TEMPORARY"),
#'   internship_codes = NULL  # Two-tier classification only
#' )
#' }
#'
#' @export
calculate_contract_quality_metrics <- function(data,
                                             id_column = "cf",
                                             period_column = "event_period",
                                             contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
                                             permanent_codes = c("C.01.00"),
                                             temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),
                                             internship_codes = c("A.07.00", "A.07.01")) {
  
  # IMPORTANT: This function uses contract_code_column for actual contract types,
  # NOT the 'prior' variable which represents employment intensity (full-time vs part-time).
  # prior > 0: Full-time employment, prior <= 0: Part-time employment
  # Contract quality analysis requires actual contract type codes (permanent, temporary, etc.)
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, contract_code_column, "durata", "over_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, period_column, contract_code_column), 
           c("cf", "period", "contract_code"))
  
  # Filter for employment periods only
  dt <- dt[over_id > 0 & !is.na(period)]
  
  if (nrow(dt) == 0) {
    warning("No employment observations found")
    return(data.table())
  }
  
  # Classify contract types (handle two-tier or three-tier classification)
  if (!is.null(internship_codes) && length(internship_codes) > 0) {
    # Three-tier classification: temporary, internship, permanent
    dt[, `:=`(
      is_permanent = contract_code %in% permanent_codes,
      is_temporary = contract_code %in% temporary_codes,
      is_internship = contract_code %in% internship_codes,
      contract_quality_score = fcase(
        contract_code %in% permanent_codes, 1.0,
        contract_code %in% internship_codes, 0.5,
        contract_code %in% temporary_codes, 0.0,
        default = 0.25
      )
    )]
  } else {
    # Two-tier classification: temporary, permanent
    dt[, `:=`(
      is_permanent = contract_code %in% permanent_codes,
      is_temporary = contract_code %in% temporary_codes,
      is_internship = FALSE,
      contract_quality_score = fcase(
        contract_code %in% permanent_codes, 1.0,
        contract_code %in% temporary_codes, 0.0,
        default = 0.5
      )
    )]
  }
  
  # Calculate basic quality metrics
  if (!is.null(internship_codes) && length(internship_codes) > 0) {
    # Include internship metrics
    quality_metrics <- dt[, .(
      permanent_contract_days = as.numeric(sum(durata[is_permanent == TRUE], na.rm = TRUE)),
      temporary_contract_days = as.numeric(sum(durata[is_temporary == TRUE], na.rm = TRUE)),
      internship_contract_days = as.numeric(sum(durata[is_internship == TRUE], na.rm = TRUE)),
      total_employment_days = as.numeric(sum(durata, na.rm = TRUE)),
      average_contract_quality = as.numeric(fmean(contract_quality_score, na.rm = TRUE)),
      contract_observations = as.numeric(.N)
    ), by = .(cf, period)]
  } else {
    # Two-tier metrics only
    quality_metrics <- dt[, .(
      permanent_contract_days = as.numeric(sum(durata[is_permanent == TRUE], na.rm = TRUE)),
      temporary_contract_days = as.numeric(sum(durata[is_temporary == TRUE], na.rm = TRUE)),
      total_employment_days = as.numeric(sum(durata, na.rm = TRUE)),
      average_contract_quality = as.numeric(fmean(contract_quality_score, na.rm = TRUE)),
      contract_observations = as.numeric(.N)
    ), by = .(cf, period)]
  }
  
  # Calculate rates
  if (!is.null(internship_codes) && length(internship_codes) > 0) {
    # Three-tier rates
    quality_metrics[, `:=`(
      permanent_contract_rate = permanent_contract_days / total_employment_days,
      internship_contract_rate = internship_contract_days / total_employment_days,
      temporary_contract_rate = temporary_contract_days / total_employment_days
    )]
    quality_metrics[is.nan(permanent_contract_rate), permanent_contract_rate := 0]
    quality_metrics[is.nan(internship_contract_rate), internship_contract_rate := 0]
    quality_metrics[is.nan(temporary_contract_rate), temporary_contract_rate := 0]
  } else {
    # Two-tier rates
    quality_metrics[, permanent_contract_rate := permanent_contract_days / 
                     (permanent_contract_days + temporary_contract_days)]
    quality_metrics[is.nan(permanent_contract_rate), permanent_contract_rate := 0]
  }
  
  # Calculate transition metrics - unified approach to avoid column type mismatch
  transition_metrics <- dt[order(cf, period, inizio), {
    if (.N <= 1) {
      # Single observation: no transitions - ensure consistent types
      list(
        temp_to_perm_transitions = as.double(0),
        perm_to_temp_transitions = as.double(0),
        temp_to_internship_transitions = as.double(0),
        internship_to_perm_transitions = as.double(0),
        contract_stability_trend = as.double(0)
      )
    } else {
      if (!is.null(internship_codes) && length(internship_codes) > 0) {
        # Three-tier transitions
        contract_changes <- data.table(
          from = contract_code[-.N],
          to = contract_code[-1]
        )
        temp_to_perm <- as.numeric(sum(contract_changes$from %in% temporary_codes & 
                           contract_changes$to %in% permanent_codes, na.rm = TRUE))
        perm_to_temp <- as.numeric(sum(contract_changes$from %in% permanent_codes & 
                           contract_changes$to %in% temporary_codes, na.rm = TRUE))
        temp_to_internship <- as.numeric(sum(contract_changes$from %in% temporary_codes & 
                                 contract_changes$to %in% internship_codes, na.rm = TRUE))
        internship_to_perm <- as.numeric(sum(contract_changes$from %in% internship_codes & 
                                 contract_changes$to %in% permanent_codes, na.rm = TRUE))
      } else {
        # Two-tier transitions - use quality score differences
        transitions <- diff(contract_quality_score)
        temp_to_perm <- as.numeric(sum(transitions > 0.3, na.rm = TRUE))  # Significant improvement
        perm_to_temp <- as.numeric(sum(transitions < -0.3, na.rm = TRUE)) # Significant deterioration
        temp_to_internship <- 0.0  # Not applicable in two-tier
        internship_to_perm <- 0.0  # Not applicable in two-tier
      }
      
      # Calculate trend using linear regression
      if (.N >= 3) {
        time_seq <- seq_len(.N)
        trend_coef <- coef(lm(contract_quality_score ~ time_seq))[2]
        trend <- ifelse(is.na(trend_coef), 0.0, as.numeric(trend_coef))
      } else {
        trend <- 0.0
      }
      
      # Ensure consistent types across all execution paths - explicit casting
      list(
        temp_to_perm_transitions = as.double(temp_to_perm),
        perm_to_temp_transitions = as.double(perm_to_temp),
        temp_to_internship_transitions = as.double(temp_to_internship),
        internship_to_perm_transitions = as.double(internship_to_perm),
        contract_stability_trend = as.double(trend)
      )
    }
  }, by = .(cf, period)]
  
  # Merge metrics
  contract_metrics <- merge(quality_metrics, transition_metrics, by = c("cf", "period"))
  
  # Calculate improvement rate
  contract_metrics[, contract_improvement_rate := 
    pmax(0, temp_to_perm_transitions) / pmax(1, contract_observations / 4)]
  
  return(contract_metrics[])
}

#' Calculate Career Complexity Metrics
#'
#' Calculates career complexity metrics including concurrent employment patterns,
#' employment diversity measures, and complexity indices.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param complexity_variables Character vector. Variables to use for complexity calculation.
#'   Default: c("over_id", "arco", "prior")
#'
#' @return A data.table with career complexity metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
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
#' complexity_metrics <- calculate_career_complexity_metrics(
#'   data = event_data,
#'   complexity_variables = c("over_id", "arco", "sector", "contract_type")
#' )
#' }
#'
#' @export
calculate_impact_career_complexity_metrics <- function(data,
                                              id_column = "cf",
                                              period_column = "event_period",
                                              complexity_variables = c("over_id", "arco", "prior")) {
  
  # Refactored to use career complexity metrics as basis engine
  # This eliminates code duplication and ensures use of IMPROVED complexity formula
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Use career complexity metrics as basis engine with period grouping
  complexity_metrics <- calculate_career_complexity_metrics(
    data = data,
    id_column = id_column,
    time_period_column = period_column,
    complexity_variables = complexity_variables
  )
  
  # Rename period column to match expected output
  if (period_column != "period" && period_column %in% names(complexity_metrics)) {
    setnames(complexity_metrics, period_column, "period")
  }
  
  return(complexity_metrics[])
}

#' Calculate Transition Pattern Metrics
#'
#' Calculates transition pattern metrics including time to next job, contract type changes,
#' and transition frequency measures.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param date_column Character. Name of date column. Default: "inizio"
#'
#' @return A data.table with transition pattern metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{avg_time_to_next_job}{Average days from job end to next job start}
#'   \item{median_time_to_next_job}{Median days from job end to next job start}
#'   \item{job_search_success_rate}{Proportion of unemployment spells ending in employment}
#'   \item{contract_type_changes}{Number of contract type changes}
#'   \item{upward_transitions}{Number of transitions to better contract types}
#'   \item{downward_transitions}{Number of transitions to worse contract types}
#'   \item{transition_frequency}{Number of job transitions per year}
#'   \item{employment_continuity_index}{Measure of employment continuity}
#'
#' @examples
#' \dontrun{
#' transition_metrics <- calculate_transition_pattern_metrics(
#'   data = event_data
#' )
#' }
#'
#' @export
calculate_transition_pattern_metrics <- function(data,
                                               id_column = "cf",
                                               period_column = "event_period",
                                               date_column = "inizio") {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, date_column, "durata", "over_id", "fine")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, period_column, date_column), c("cf", "period", "start_date"))
  
  # Ensure dates are proper format
  dt[, `:=`(
    start_date = as.Date(start_date),
    end_date = as.Date(fine)
  )]
  
  # Filter for valid periods
  dt <- dt[!is.na(period)]
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Calculate transition metrics by person and period
  transition_metrics <- dt[order(cf, period, start_date), {
    if (.N <= 1) {
      list(
        avg_time_to_next_job = NA_real_,
        median_time_to_next_job = NA_real_,
        job_search_success_rate = NA_real_,
        contract_type_changes = 0.0,
        upward_transitions = 0.0,
        downward_transitions = 0.0,
        transition_frequency = 0.0,
        employment_continuity_index = 1.0
      )
    } else {
      # Employment status
      employed <- over_id > 0
      
      # Calculate gaps between employment periods
      emp_periods <- .SD[employed == TRUE]
      if (nrow(emp_periods) >= 2) {
        # Time between job endings and next job starts
        job_gaps <- emp_periods[2:.N, start_date] - emp_periods[1:(.N-1), end_date]
        job_gaps <- as.numeric(job_gaps[job_gaps > 0]) # Only positive gaps
        
        avg_gap <- if (length(job_gaps) > 0) as.double(fmean(job_gaps, na.rm = TRUE)) else NA_real_
        median_gap <- if (length(job_gaps) > 0) as.double(fmedian(job_gaps, na.rm = TRUE)) else NA_real_
      } else {
        avg_gap <- NA_real_
        median_gap <- NA_real_
      }
      
      # Job search success rate
      unemp_spells <- as.double(sum(!employed))
      successful_searches <- if (unemp_spells > 0) {
        # Count unemployment periods followed by employment
        unemp_to_emp <- as.double(sum(diff(as.integer(employed)) > 0, na.rm = TRUE))
        unemp_to_emp / unemp_spells
      } else {
        NA_real_
      }
      
      # Contract type changes (if prior column exists)
      contract_changes <- if ("prior" %in% names(.SD)) {
        as.double(sum(diff(prior) != 0, na.rm = TRUE))
      } else {
        0.0
      }
      
      # Transition quality (simplified - could be enhanced with actual contract rankings)
      upward <- downward <- 0.0
      if ("prior" %in% names(.SD) && length(unique(prior)) > 1) {
        quality_changes <- diff(prior)
        upward <- as.double(sum(quality_changes > 0, na.rm = TRUE))
        downward <- as.double(sum(quality_changes < 0, na.rm = TRUE))
      }
      
      # Overall transition frequency
      employment_transitions <- as.double(sum(diff(as.integer(employed)) != 0, na.rm = TRUE))
      period_years <- as.double(sum(durata, na.rm = TRUE)) / 365.25
      transition_freq <- employment_transitions / pmax(1.0, period_years)
      
      # Employment continuity index
      total_employed_days <- as.double(sum(durata[employed], na.rm = TRUE))
      total_days <- as.double(sum(durata, na.rm = TRUE))
      continuity <- total_employed_days / pmax(1.0, total_days)
      
      list(
        avg_time_to_next_job = as.double(avg_gap),
        median_time_to_next_job = as.double(median_gap),
        job_search_success_rate = as.double(successful_searches),
        contract_type_changes = as.double(contract_changes),
        upward_transitions = as.double(upward),
        downward_transitions = as.double(downward),
        transition_frequency = as.double(transition_freq),
        employment_continuity_index = as.double(continuity)
      )
    }
  }, by = .(cf, period)]
  
  return(transition_metrics[])
}

#' Calculate Career Success Index for Impact Analysis
#'
#' Internal helper function that calculates the unified career success index
#' from component metrics calculated in the impact system.
#'
#' @param metric_results List containing calculated metric results
#' @param id_column Character. Name of person identifier column
#' @param period_column Character. Column indicating pre/post event period
#'
#' @return A data.table with career_success_index values
#'
#' @keywords internal
calculate_career_success_index_for_impact <- function(metric_results, id_column, period_column) {
  
  # Initialize component metrics with defaults
  contract_quality_component <- NULL
  intensity_component <- NULL
  stability_component <- NULL
  growth_component <- NULL
  
  # Extract components from available metrics
  if ("quality" %in% names(metric_results)) {
    contract_quality_component <- metric_results$quality[, .(cf, period, 
      contract_quality = fifelse(is.na(average_contract_quality), 0.5, average_contract_quality))]
  }
  
  if ("stability" %in% names(metric_results)) {
    stability_component <- metric_results$stability[, .(cf, period,
      stability_score = fifelse(is.na(employment_stability_index), 0.5, employment_stability_index))]
  }
  
  # Employment intensity (derived from employment rate as proxy)
  if ("stability" %in% names(metric_results)) {
    intensity_component <- metric_results$stability[, .(cf, period,
      intensity_score = fifelse(is.na(employment_rate), 0.5, employment_rate))]
  }
  
  # Growth opportunity (derived from contract improvement rate)
  if ("quality" %in% names(metric_results)) {
    growth_component <- metric_results$quality[, .(cf, period,
      growth_score = fifelse(is.na(contract_improvement_rate), 0.3, 
                           pmin(1.0, contract_improvement_rate)))]
  }
  
  # Merge all available components
  career_success_data <- NULL
  
  # Start with the first available component
  if (!is.null(contract_quality_component)) {
    career_success_data <- contract_quality_component
  } else if (!is.null(stability_component)) {
    career_success_data <- stability_component[, .(cf, period)]
    career_success_data[, contract_quality := 0.5]
  } else {
    # If no components available, return empty
    return(data.table())
  }
  
  # Merge other components
  if (!is.null(intensity_component)) {
    career_success_data <- merge(career_success_data, intensity_component, 
                                by = c("cf", "period"), all.x = TRUE)
  } else {
    career_success_data[, intensity_score := 0.5]
  }
  
  if (!is.null(stability_component) && is.null(career_success_data[["stability_score"]])) {
    career_success_data <- merge(career_success_data, stability_component, 
                                by = c("cf", "period"), all.x = TRUE)
  } else if (is.null(career_success_data[["stability_score"]])) {
    career_success_data[, stability_score := 0.5]
  }
  
  if (!is.null(growth_component)) {
    career_success_data <- merge(career_success_data, growth_component, 
                                by = c("cf", "period"), all.x = TRUE)
  } else {
    career_success_data[, growth_score := 0.3]
  }
  
  # Fill any remaining NAs with defaults
  career_success_data[is.na(contract_quality), contract_quality := 0.5]
  career_success_data[is.na(intensity_score), intensity_score := 0.5]
  career_success_data[is.na(stability_score), stability_score := 0.5]
  career_success_data[is.na(growth_score), growth_score := 0.3]
  
  # Calculate career success index using the same formula as the career system
  # Weights: Contract quality (35%) + Employment intensity (25%) + Career stability (25%) + Growth opportunity (15%)
  career_success_data[, career_success_index := pmax(0, pmin(1,
    0.35 * contract_quality + 0.25 * intensity_score + 
    0.25 * stability_score + 0.15 * growth_score
  ))]
  
  # Return only the required columns
  return(career_success_data[, .(cf, period, career_success_index)])
}

#' Comprehensive Impact Metrics Calculation
#'
#' Calculates all impact evaluation metrics (stability, quality, complexity, transitions)
#' in a single function call with consistent formatting and validation. This function
#' serves as the first step in the integrated workflow for causal inference analysis.
#' Optionally integrates comprehensive career metrics from the career analysis system.
#'
#' @details
#' This function is designed to work seamlessly with the impact evaluation pipeline:
#' 
#' \strong{Integration Workflow:}
#' \enumerate{
#'   \item \strong{Metrics Calculation}: Use this function to compute comprehensive 
#'     employment metrics across pre/post periods
#'   \item \strong{Data Preparation}: Use \code{\link{prepare_metrics_for_impact_analysis}} 
#'     to bridge metrics output to causal inference format
#'   \item \strong{Impact Analysis}: Apply methods like \code{\link{difference_in_differences}}
#'     using the prepared data
#' }
#' 
#' The computed metrics serve as outcome variables in causal inference studies, allowing
#' researchers to measure treatment effects on various aspects of employment quality 
#' and career trajectories.
#'
#' \strong{Career Metrics Integration:}
#' When \code{career_metrics = TRUE}, this function integrates all validated career metrics
#' from the career analysis system, including 35+ career metrics such as:
#' \itemize{
#'   \item \code{career_success_index}: Unified career success measure (0-1)
#'   \item \code{career_advancement_index}: Career progression and transition success (0-1)
#'   \item \code{contract_quality_score}: Duration-weighted contract quality (0-1)
#'   \item \code{employment_intensity_score}: Full-time vs part-time patterns (0-1)
#'   \item \code{career_stability_score}: Contract duration consistency (0-1)
#'   \item \code{growth_opportunity_score}: Access to diverse contract types (0-1)
#'   \item Career transition, complexity, and stability metrics
#' }
#' These metrics maintain their validated calculations and naming from the career system.
#'
#' @param data A data.table containing employment records with event identification
#' @param metrics Character vector. Metrics to calculate. Options: 
#'   c("stability", "quality", "complexity", "transitions", "all"). When career_metrics = TRUE,
#'   "all" includes both impact and career metrics. Default: "all"
#' @param career_metrics Logical. Whether to include comprehensive career metrics from the 
#'   career analysis system. Default: FALSE (maintains backward compatibility)
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param output_format Character. Output format: "wide", "long", or "list". Default: "wide"
#' @param contract_code_column Character. Column containing actual contract type codes (for quality metrics). Default: "COD_TIPOLOGIA_CONTRATTUALE"
#' @param permanent_codes Character vector. Contract codes indicating permanent contracts (for quality metrics). Default: c("C.01.00")
#' @param temporary_codes Character vector. Contract codes indicating temporary contracts (for quality metrics). Default: c("A.03.00", "A.03.01", "A.09.00")
#' @param internship_codes Character vector. Contract codes indicating internship/apprenticeship contracts (for quality metrics). Default: c("A.07.00", "A.07.01")
#'
#' @return Based on output_format:
#'   \item{wide}{Single data.table with all metrics as columns, including career_success_index when multiple metrics are calculated. When career_metrics = TRUE, includes 35+ career metrics}
#'   \item{long}{Long-format data.table with metric_name and metric_value columns}
#'   \item{list}{Named list with separate data.tables for each metric type, with career_success_index integrated when available. When career_metrics = TRUE, includes career metrics in separate list element}
#'
#' @examples
#' \dontrun{
#' # ===== BASIC METRICS CALCULATION =====
#' # Calculate all metrics for impact analysis
#' all_metrics <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = "all",
#'   output_format = "wide"
#' )
#' 
#' # Calculate specific metrics for focused analysis
#' stability_quality <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = c("stability", "quality"),
#'   output_format = "list"
#' )
#' 
#' # ===== CAREER METRICS INTEGRATION =====
#' # Include comprehensive career metrics from the career analysis system
#' all_metrics_with_career <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = "all",
#'   career_metrics = TRUE,
#'   output_format = "wide"
#' )
#' # Result includes 35+ career metrics: career_success_index, career_advancement_index,
#' # contract_quality_score, employment_intensity_score, etc.
#' 
#' # Career metrics with specific impact metrics
#' career_focused <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = c("stability", "quality"),
#'   career_metrics = TRUE,
#'   output_format = "list"
#' )
#' # Includes both impact and career metrics in separate list elements
#' 
#' # ===== INTEGRATION WITH IMPACT EVALUATION =====
#' # Complete workflow for causal inference
#' 
#' # Step 1: Calculate comprehensive metrics (including career metrics)
#' metrics_result <- calculate_comprehensive_impact_metrics(
#'   data = employment_data,
#'   metrics = c("stability", "quality", "complexity"),
#'   career_metrics = TRUE,  # Include 35+ career metrics
#'   output_format = "wide"
#' )
#' 
#' # Step 2: Define treatment assignment
#' treatment_data <- data.table(
#'   cf = unique(employment_data$cf),
#'   is_treated = sample(c(0, 1), length(unique(employment_data$cf)), replace = TRUE)
#' )
#' 
#' # Step 3: Prepare for impact analysis
#' did_data <- prepare_metrics_for_impact_analysis(
#'   metrics_output = metrics_result,
#'   treatment_assignment = treatment_data,
#'   impact_method = "did"
#' )
#' 
#' # Step 4: Run difference-in-differences analysis
#' did_results <- difference_in_differences(
#'   data = did_data,
#'   outcome_vars = c("employment_rate", "career_success_index", "career_advancement_index"),
#'   treatment_var = "is_treated",
#'   time_var = "post",
#'   id_var = "cf"
#' )
#' 
#' # ===== OUTPUT FORMAT OPTIONS =====
#' # Wide format: All metrics as columns (best for analysis)
#' metrics_wide <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   output_format = "wide"
#' )
#' 
#' # Long format: Metric names as variables (best for plotting)
#' metrics_long <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   output_format = "long"
#' )
#' 
#' # List format: Separate tables per metric category (best for detailed examination)
#' metrics_list <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   output_format = "list"
#' )
#' }
#'
#' @seealso 
#' \code{\link{calculate_comprehensive_career_metrics}} for comprehensive career metrics calculation,
#' \code{\link{prepare_metrics_for_impact_analysis}} for bridging metrics to impact analysis,
#' \code{\link{difference_in_differences}} for causal inference using the metrics,
#' \code{vignette("impact-metrics-integration", package = "longworkR")} for complete workflow examples
#'
#' @export
calculate_comprehensive_impact_metrics <- function(data,
                                                 metrics = "all",
                                                 career_metrics = FALSE,
                                                 id_column = "cf",
                                                 period_column = "event_period",
                                                 output_format = "wide",
                                                 contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
                                                 permanent_codes = c("C.01.00"),
                                                 temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),
                                                 internship_codes = c("A.07.00", "A.07.01")) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!"all" %in% metrics) {
    # Define valid metrics based on whether career metrics are enabled
    if (career_metrics) {
      valid_metrics <- c("stability", "quality", "complexity", "transitions", 
                        "career_success_index", "career_advancement_index", "career_quality",
                        "career_stability", "career_complexity", "career_transitions")
    } else {
      valid_metrics <- c("stability", "quality", "complexity", "transitions")
    }
    
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
      stop(paste("Invalid metrics specified:", paste(invalid_metrics, collapse = ", ")))
    }
  } else {
    metrics <- c("stability", "quality", "complexity", "transitions")
  }
  
  if (!output_format %in% c("wide", "long", "list")) {
    stop("output_format must be one of: 'wide', 'long', 'list'")
  }
  
  # Calculate requested metrics
  metric_results <- list()
  
  if ("stability" %in% metrics) {
    metric_results$stability <- calculate_employment_stability_metrics(
      data, id_column, period_column
    )
  }
  
  if ("quality" %in% metrics) {
    metric_results$quality <- calculate_contract_quality_metrics(
      data, id_column, period_column, contract_code_column,
      permanent_codes, temporary_codes, internship_codes
    )
  }
  
  if ("complexity" %in% metrics) {
    metric_results$complexity <- calculate_impact_career_complexity_metrics(
      data, id_column, period_column
    )
  }
  
  if ("transitions" %in% metrics) {
    metric_results$transitions <- calculate_transition_pattern_metrics(
      data, id_column, period_column
    )
  }
  
  # Calculate career metrics if requested
  if (career_metrics) {
    # Determine which career metrics to calculate based on requested metrics
    career_metrics_to_calc <- if ("all" %in% metrics) {
      "all"  # Get all career metrics
    } else {
      # Map specific career metric requests to career system metrics
      career_specific <- c("career_success_index", "career_advancement_index", "career_quality",
                          "career_stability", "career_complexity", "career_transitions")
      if (any(career_specific %in% metrics)) {
        "all"  # If any career-specific metrics requested, get all to ensure dependencies
      } else {
        "all"  # Default to all career metrics when career_metrics = TRUE
      }
    }
    
    # Call calculate_comprehensive_career_metrics 
    # Note: Due to compatibility issues with time period handling in career metrics,
    # we calculate overall career metrics and then replicate for each period
    career_metrics_result <- calculate_comprehensive_career_metrics(
      data = data,
      metrics = career_metrics_to_calc,
      id_column = id_column,
      time_period_column = NULL,  # Calculate overall, then handle periods manually
      output_format = "wide",
      contract_code_column = contract_code_column,
      employment_intensity_column = "prior",
      min_spell_duration = 7
    )
    
    # Integrate career metrics with impact metrics
    if (nrow(career_metrics_result) > 0) {
      # Career metrics were calculated overall (without time periods)
      # Need to replicate for each period in the impact data to enable proper merging
      unique_periods <- unique(data[[period_column]])
      unique_periods <- unique_periods[!is.na(unique_periods)]
      
      if (length(unique_periods) > 1) {
        # Multiple periods exist - replicate career metrics for each period
        career_expanded <- career_metrics_result[rep(seq_len(nrow(career_metrics_result)), length(unique_periods))]
        career_expanded[, period := rep(unique_periods, each = nrow(career_metrics_result))]
        career_metrics_result <- career_expanded
      } else if (length(unique_periods) == 1) {
        # Single period - just add the period column
        career_metrics_result[, period := unique_periods[1]]
      } else {
        # No valid periods found - use default
        career_metrics_result[, period := "overall"]
      }
      
      # Ensure consistent ID column naming
      if (id_column != "cf" && id_column %in% names(career_metrics_result)) {
        setnames(career_metrics_result, id_column, "cf")
      }
      
      # Add career metrics to the metric_results list
      metric_results$career <- career_metrics_result
    }
  } else {
    # Calculate career_success_index if multiple impact metrics are calculated (original behavior)
    if (length(metric_results) >= 2) {
      career_success_metrics <- calculate_career_success_index_for_impact(
        metric_results, id_column, period_column
      )
      
      # Merge career success index into the results
      if (length(metric_results) > 0) {
        # Add career_success_index to the first available metric result
        first_metric <- names(metric_results)[1]
        metric_results[[first_metric]] <- merge(
          metric_results[[first_metric]], 
          career_success_metrics, 
          by = c("cf", "period"), 
          all.x = TRUE
        )
      }
    }
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
  
  # Handle column naming conflicts between impact and career metrics
  # Career metrics take precedence to maintain validated calculations
  if (career_metrics && "career" %in% names(metric_results)) {
    # Identify overlapping columns (excluding id and period columns)
    career_cols <- names(metric_results$career)
    id_period_cols <- c("cf", "period")
    career_metric_cols <- setdiff(career_cols, id_period_cols)
    
    # Remove overlapping columns from impact metrics to prefer career metrics
    for (metric_name in names(metric_results)) {
      if (metric_name != "career") {
        overlapping_cols <- intersect(names(metric_results[[metric_name]]), career_metric_cols)
        if (length(overlapping_cols) > 0) {
          metric_results[[metric_name]] <- metric_results[[metric_name]][, !..overlapping_cols]
        }
      }
    }
  }
  
  # Merge all metric tables (all functions now return "cf" and "period" columns)
  merged_metrics <- Reduce(function(x, y) {
    merge(x, y, by = c("cf", "period"), all = TRUE)
  }, metric_results)
  
  if (output_format == "wide") {
    return(merged_metrics)
  }
  
  # Convert to long format
  id_vars <- c("cf", "period")
  measure_vars <- setdiff(names(merged_metrics), id_vars)
  
  long_metrics <- melt(merged_metrics, id.vars = id_vars, measure.vars = measure_vars,
                       variable.name = "metric_name", value.name = "metric_value")
  
  # Add metric category
  long_metrics[, metric_category := fcase(
    grepl("employment|spell|turnover|stability", metric_name), "stability",
    grepl("contract|permanent|temporary|quality", metric_name), "quality", 
    grepl("concurrent|diversity|complexity|fragmentation", metric_name), "complexity",
    grepl("transition|continuity|search|time", metric_name), "transitions",
    grepl("career_success_index|career_advancement_index", metric_name), "career_success",
    grepl("^career_|employment_intensity_score|growth_opportunity_score", metric_name), "career_metrics",
    default = "other"
  )]
  
  return(long_metrics[])
}