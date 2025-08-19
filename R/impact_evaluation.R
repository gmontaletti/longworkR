#' Impact Evaluation: Event Identification and Treatment Group Definition
#'
#' This module provides comprehensive event identification functionality for impact evaluation
#' studies using employment data. It supports flexible treatment conditions, multiple events
#' per person, and robust event timing identification.
#'
#' @name impact_evaluation
#' @author vecshift package
NULL

#' Identify Treatment Groups for Impact Evaluation
#'
#' Identifies people who experienced treatment events and collects their complete
#' employment histories for impact evaluation. Uses a person-centered approach that
#' compares entire career trajectories of treated vs. control individuals.
#'
#' @param data A data.table containing employment records (output from vecshift)
#' @param treatment_conditions List of conditions defining treatment events.
#'   Each condition can be:
#'   - A string expression (e.g., "COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'")
#'   - A named list with 'column', 'operator', and 'value'
#'   - A function that takes the data and returns a logical vector
#' @param event_window Numeric vector of length 2 defining the event window
#'   relative to the treatment date. Default: c(-365, 365) (1 year before/after)
#' @param min_pre_period Minimum number of days required before the event.
#'   Default: 90
#' @param min_post_period Minimum number of days required after the event.
#'   Default: 90
#' @param multiple_events How to handle multiple treatment events per person:
#'   - "first": Use only the first treatment event date
#'   - "last": Use only the last treatment event date
#'   - "all": Keep all treatment events (creates multiple treatment dates per person)
#'   Default: "first"
#' @param require_employment_before Logical. Require employment before the treatment event?
#'   Default: TRUE
#' @param id_column Character. Name of the person identifier column. Default: "cf"
#' @param date_column Character. Name of the date column to use for event timing.
#'   Default: "inizio"
#'
#' @return A data.table with ALL employment events for both treated and control people including:
#'   \item{cf}{Person identifier}
#'   \item{is_treated}{Logical indicator: TRUE for people who experienced treatment, FALSE for controls}
#'   \item{treatment_event_date}{Date of first/last treatment event (NA for control people)}
#'   \item{days_to_event}{Days from each observation to treatment event (negative = before)}
#'   \item{in_event_window}{Logical indicator for observations within event window}
#'   \item{pre_event_period}{Logical indicator for pre-treatment observations}
#'   \item{post_event_period}{Logical indicator for post-treatment observations}
#'   \item{treatment_condition_met}{Description of which condition triggered treatment}
#'   \item{...}{All original employment data columns}
#'
#' @examples
#' \dontrun{
#' # Identify people who got permanent contracts and collect their full careers
#' impact_data <- identify_treatment_events(
#'   data = employment_data,
#'   treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
#'   event_window = c(-180, 365),
#'   multiple_events = "first"
#' )
#' 
#' # Result contains:
#' # - ALL employment events for people who got permanent contracts (is_treated=TRUE)
#' # - ALL employment events for people who never got permanent contracts (is_treated=FALSE)
#' # - treatment_event_date marks when treated people first got permanent contracts
#' 
#' # Multiple conditions example
#' impact_data <- identify_treatment_events(
#'   data = employment_data,
#'   treatment_conditions = list(
#'     list(column = "COD_TIPOLOGIA_CONTRATTUALE", operator = "==", value = "C.01.00"),
#'     list(column = "durata", operator = ">", value = 365)
#'   ),
#'   event_window = c(-365, 730)
#' )
#' }
#'
#' @export
identify_treatment_events <- function(data,
                                    treatment_conditions,
                                    event_window = c(-365, 365),
                                    min_pre_period = 90,
                                    min_post_period = 90,
                                    multiple_events = "first",
                                    require_employment_before = TRUE,
                                    id_column = "cf",
                                    date_column = "inizio") {
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!id_column %in% names(data)) {
    stop(paste("ID column", id_column, "not found in data"))
  }
  
  if (!date_column %in% names(data)) {
    stop(paste("Date column", date_column, "not found in data"))
  }
  
  if (length(event_window) != 2 || event_window[2] <= event_window[1]) {
    stop("event_window must be a numeric vector of length 2 with window[2] > window[1]")
  }
  
  if (!multiple_events %in% c("first", "last", "all")) {
    stop("multiple_events must be one of: 'first', 'last', 'all'")
  }
  
  # Ensure date column is properly formatted
  if (!inherits(data[[date_column]], "Date")) {
    data[[date_column]] <- as.Date(data[[date_column]])
  }
  
  # Create working copy with standardized column names
  dt <- copy(data)
  setnames(dt, c(id_column, date_column), c("cf", "obs_date"))
  
  # STEP 1: Find people (cf) who experienced treatment condition
  # Apply treatment conditions to identify treatment events
  treatment_events_list <- list()
  
  for (i in seq_along(treatment_conditions)) {
    condition <- treatment_conditions[[i]]
    
    # Apply condition based on type
    if (is.character(condition)) {
      # String expression
      condition_met <- dt[, eval(parse(text = condition))]
      condition_desc <- condition
    } else if (is.list(condition) && all(c("column", "operator", "value") %in% names(condition))) {
      # Structured condition
      col_name <- condition$column
      operator <- condition$operator
      value <- condition$value
      
      if (!col_name %in% names(dt)) {
        warning(paste("Column", col_name, "not found in data, skipping condition"))
        next
      }
      
      condition_expr <- paste0(col_name, " ", operator, " ", 
                              if (is.character(value)) paste0("'", value, "'") else value)
      condition_met <- dt[, eval(parse(text = condition_expr))]
      condition_desc <- condition_expr
    } else if (is.function(condition)) {
      # Function condition
      condition_met <- condition(dt)
      condition_desc <- paste("Custom function", i)
    } else {
      stop(paste("Invalid treatment condition format at position", i))
    }
    
    # Collect treatment events (events that meet the condition)
    if (sum(condition_met, na.rm = TRUE) > 0) {
      treatment_events <- dt[condition_met == TRUE, .(
        cf,
        treatment_event_date = obs_date,
        treatment_condition_met = condition_desc
      )]
      treatment_events_list[[i]] <- treatment_events
    }
  }
  
  if (length(treatment_events_list) == 0) {
    warning("No treatment events identified with the given conditions")
    return(dt[0]) # Return empty data.table with same structure
  }
  
  # Combine all treatment events
  all_treatment_events <- rbindlist(treatment_events_list)
  
  if (nrow(all_treatment_events) == 0) {
    warning("No treatment events found after applying conditions")
    return(dt[0])
  }
  
  # STEP 2: For treated people, identify their treatment event date(s)
  # Handle multiple treatment events per person
  if (multiple_events == "first") {
    person_treatment_dates <- all_treatment_events[, .SD[which.min(treatment_event_date)], by = cf]
  } else if (multiple_events == "last") {
    person_treatment_dates <- all_treatment_events[, .SD[which.max(treatment_event_date)], by = cf]
  } else { # "all"
    person_treatment_dates <- unique(all_treatment_events)
  }
  
  # Get unique treated people
  treated_people <- unique(person_treatment_dates$cf)
  
  # STEP 3 & 4: Check minimum observation periods and employment requirements
  # Get data range for each person
  person_data_ranges <- dt[, .(
    first_obs = min(obs_date, na.rm = TRUE),
    last_obs = max(obs_date, na.rm = TRUE)
  ), by = cf]
  
  # Merge with treatment dates and filter
  person_treatment_dates <- merge(person_treatment_dates, person_data_ranges, by = "cf")
  
  # Filter people with sufficient pre/post periods
  valid_treated_people <- person_treatment_dates[
    treatment_event_date - first_obs >= min_pre_period &
    last_obs - treatment_event_date >= min_post_period
  ]
  
  # Optional: require employment before treatment event
  if (require_employment_before && "over_id" %in% names(dt)) {
    # Find people who had employment before their treatment event
    employment_before <- dt[obs_date < person_treatment_dates[, treatment_event_date][1], .( # simplified for now
      employed_before_event = any(over_id > 0, na.rm = TRUE)
    ), by = cf]
    
    valid_treated_people <- merge(valid_treated_people, employment_before, by = "cf")
    valid_treated_people <- valid_treated_people[employed_before_event == TRUE]
    valid_treated_people[, employed_before_event := NULL]
  }
  
  if (nrow(valid_treated_people) == 0) {
    warning("No people meet the minimum pre/post period and employment requirements")
    return(dt[0])
  }
  
  # Get final list of treated people
  final_treated_people <- unique(valid_treated_people$cf)
  
  # STEP 5: Identify control people (who never experienced treatment)
  all_people <- unique(dt$cf)
  control_people <- setdiff(all_people, final_treated_people)
  
  # STEP 6: Collect ALL events for both treated and control people
  # Create person-level treatment information
  person_treatment_info <- valid_treated_people[, .(
    cf,
    treatment_event_date,
    treatment_condition_met
  )]
  
  # If multiple_events = "all", we might have multiple rows per person
  # For now, let's take the first one for the merge
  if (multiple_events == "all" && person_treatment_info[, .N, by = cf][, max(N)] > 1) {
    warning("multiple_events = 'all' creates multiple treatment dates per person. Using first event date for timing calculations.")
    person_treatment_info <- person_treatment_info[, .SD[1], by = cf]
  }
  
  # Merge all employment data with person-level treatment information
  result <- merge(dt, person_treatment_info, by = "cf", all.x = TRUE)
  
  # Create person-level treatment indicator
  result[, is_treated := !is.na(treatment_event_date)]
  
  # Calculate days to treatment event for all observations
  result[is_treated == TRUE, days_to_event := as.numeric(obs_date - treatment_event_date)]
  result[is_treated == FALSE, days_to_event := NA_real_]
  
  # Calculate event window indicators
  result[is_treated == TRUE, `:=`(
    in_event_window = (days_to_event >= event_window[1] & days_to_event <= event_window[2]),
    pre_event_period = (days_to_event < 0 & days_to_event >= event_window[1]),
    post_event_period = (days_to_event > 0 & days_to_event <= event_window[2])
  )]
  
  # Set indicators to FALSE for control people
  result[is_treated == FALSE, `:=`(
    in_event_window = FALSE,
    pre_event_period = FALSE,
    post_event_period = FALSE
  )]
  
  # Fill missing treatment condition for controls
  result[is.na(treatment_condition_met), treatment_condition_met := "Control (never treated)"]
  
  # Restore original column names
  setnames(result, c("cf", "obs_date"), c(id_column, date_column))
  
  return(result[])
}

#' Create Treatment and Control Groups
#'
#' Creates matched treatment and control groups for impact evaluation based on
#' event identification results.
#'
#' @param event_data Data.table from identify_treatment_events()
#' @param control_conditions Optional conditions for control group eligibility.
#'   Similar format to treatment_conditions in identify_treatment_events()
#' @param matching_variables Character vector of variables to use for matching.
#'   Default: NULL (no matching, uses random eligible controls).
#'   Note: If provided, this serves as a reminder but actual matching should be
#'   done using propensity_score_matching() after group creation
#' @param exact_match_variables Character vector of variables requiring exact matches.
#'   Default: NULL
#' @param control_ratio Numeric. Ratio of control to treatment units. Default: 1
#' @param caliper Numeric. Maximum distance for propensity score matching. Default: 0.1
#' @param exclude_future_treated Logical. Exclude future-treated units from control group?
#'   Default: TRUE
#' @param replacement Logical. Sample control units with replacement? Default: FALSE
#'
#' @return A data.table with treatment and control group assignments including:
#'   \item{group_assignment}{Factor: "treatment" or "control"}
#'   \item{match_id}{Matching pair identifier}
#'   \item{propensity_score}{Estimated propensity score (if matching used)}
#'   \item{match_distance}{Distance to matched unit}
#'   \item{control_weight}{Weight for control observations}
#'
#' @examples
#' \dontrun{
#' # Create treatment and control groups
#' groups <- create_treatment_control_groups(
#'   event_data = identified_events,
#'   matching_variables = c("age", "sector", "region"),
#'   exact_match_variables = c("gender"),
#'   control_ratio = 2
#' )
#' }
#'
#' @export
create_treatment_control_groups <- function(event_data,
                                          control_conditions = NULL,
                                          matching_variables = NULL,
                                          exact_match_variables = NULL,
                                          control_ratio = 1,
                                          caliper = 0.1,
                                          exclude_future_treated = TRUE,
                                          replacement = FALSE) {
  
  if (!inherits(event_data, "data.table")) {
    stop("event_data must be a data.table")
  }
  
  if (!"is_treated" %in% names(event_data)) {
    stop("event_data must contain 'is_treated' column from identify_treatment_events()")
  }
  
  # Create working copy
  dt <- copy(event_data)
  
  # Initialize group assignment
  dt[, `:=`(
    group_assignment = factor(NA, levels = c("treatment", "control")),
    match_id = NA_integer_,
    propensity_score = NA_real_,
    match_distance = NA_real_,
    control_weight = 1.0
  )]
  
  # Assign treatment group
  dt[is_treated == TRUE, group_assignment := "treatment"]
  
  # Identify potential control units
  potential_controls <- dt[is_treated == FALSE]
  
  # Apply control conditions if specified
  if (!is.null(control_conditions)) {
    for (condition in control_conditions) {
      if (is.character(condition)) {
        eligible <- potential_controls[, eval(parse(text = condition))]
      } else if (is.list(condition)) {
        col_name <- condition$column
        operator <- condition$operator
        value <- condition$value
        condition_expr <- paste0(col_name, " ", operator, " ", 
                                if (is.character(value)) paste0("'", value, "'") else value)
        eligible <- potential_controls[, eval(parse(text = condition_expr))]
      } else if (is.function(condition)) {
        eligible <- condition(potential_controls)
      }
      potential_controls <- potential_controls[eligible]
    }
  }
  
  # Exclude future treated if requested
  if (exclude_future_treated) {
    # Find units that become treated later
    future_treated_ids <- dt[is_treated == TRUE, unique(get(names(dt)[1]))]  # assuming first column is ID
    potential_controls <- potential_controls[!get(names(potential_controls)[1]) %in% future_treated_ids]
  }
  
  # Simple assignment if no matching
  if (is.null(matching_variables)) {
    n_treatment <- dt[group_assignment == "treatment", .N]
    n_controls_needed <- min(nrow(potential_controls), n_treatment * control_ratio)
    
    if (n_controls_needed > 0) {
      control_sample <- potential_controls[sample(.N, n_controls_needed)]
      
      # Update group assignments
      merge_cols <- intersect(names(dt), names(control_sample))
      dt[control_sample, on = merge_cols, group_assignment := "control"]
    }
  } else {
    # When matching_variables are provided, inform user to use propensity_score_matching
    warning("Matching variables provided but not used. For propensity score matching, use propensity_score_matching() from the impact_matching module after creating groups.")
    
    # Still assign some controls for basic functionality
    n_treatment <- dt[group_assignment == "treatment", .N]
    n_controls_needed <- min(nrow(potential_controls), n_treatment * control_ratio)
    
    if (n_controls_needed > 0) {
      control_sample <- potential_controls[sample(.N, n_controls_needed)]
      merge_cols <- intersect(names(dt), names(control_sample))
      dt[control_sample, on = merge_cols, group_assignment := "control"]
    }
    
    message("To perform propensity score matching, use:\n",
            "  matched_data <- propensity_score_matching(groups_data, \n",
            "                     matching_variables = c('var1', 'var2'),\n",
            "                     treatment_variable = 'is_treated')")
  }
  
  # Add match IDs for simple pairing
  treatment_units <- dt[group_assignment == "treatment"]
  control_units <- dt[group_assignment == "control"]
  
  if (nrow(treatment_units) > 0 && nrow(control_units) > 0) {
    n_matches <- min(nrow(treatment_units), floor(nrow(control_units) / control_ratio))
    
    if (n_matches > 0) {
      match_ids <- rep(1:n_matches, each = (1 + control_ratio))
      
      # Assign match IDs
      treatment_units[1:n_matches, match_id := 1:n_matches]
      control_units[1:(n_matches * control_ratio), 
                    match_id := rep(1:n_matches, each = control_ratio)]
      
      # Update main data
      dt[treatment_units, on = intersect(names(dt), names(treatment_units)), 
         match_id := i.match_id]
      dt[control_units, on = intersect(names(dt), names(control_units)), 
         match_id := i.match_id]
    }
  }
  
  return(dt[!is.na(group_assignment)])
}

#' Assess Treatment Event Quality
#'
#' Provides diagnostic information about treatment event identification including
#' timing distributions, balance assessment, and data quality metrics.
#'
#' @param event_data Data.table from identify_treatment_events()
#' @param assessment_variables Character vector of variables to include in balance assessment
#' @param output_format Character. Output format: "summary", "detailed", or "both"
#'
#' @return A list containing:
#'   \item{event_summary}{Summary statistics of identified events}
#'   \item{timing_distribution}{Distribution of event timing}
#'   \item{balance_table}{Balance assessment between treatment and control}
#'   \item{data_quality}{Data quality metrics}
#'   \item{recommendations}{Recommendations for improving identification}
#'
#' @examples
#' \dontrun{
#' assessment <- assess_treatment_event_quality(
#'   event_data = identified_events,
#'   assessment_variables = c("age", "sector", "prior_employment")
#' )
#' print(assessment$event_summary)
#' }
#'
#' @export
assess_treatment_event_quality <- function(event_data,
                                         assessment_variables = NULL,
                                         output_format = "summary") {
  
  if (!inherits(event_data, "data.table")) {
    stop("event_data must be a data.table")
  }
  
  # Event summary statistics
  event_summary <- list(
    total_observations = nrow(event_data),
    total_treated = sum(event_data$is_treated, na.rm = TRUE),
    total_controls = sum(!event_data$is_treated, na.rm = TRUE),
    unique_treated_persons = event_data[is_treated == TRUE, uniqueN(cf)],
    treatment_rate = mean(event_data$is_treated, na.rm = TRUE),
    events_per_person = event_data[is_treated == TRUE, .N, by = cf][, mean(N)],
    date_range = range(event_data$treatment_event_date, na.rm = TRUE)
  )
  
  # Timing distribution
  timing_dist <- NULL
  if ("days_to_event" %in% names(event_data)) {
    timing_dist <- event_data[!is.na(days_to_event), .(
      min_days = min(days_to_event),
      q25_days = quantile(days_to_event, 0.25),
      median_days = median(days_to_event),
      q75_days = quantile(days_to_event, 0.75),
      max_days = max(days_to_event),
      mean_days = mean(days_to_event),
      sd_days = sd(days_to_event)
    )]
  }
  
  # Balance assessment
  balance_table <- NULL
  if (!is.null(assessment_variables)) {
    available_vars <- intersect(assessment_variables, names(event_data))
    if (length(available_vars) > 0) {
      balance_list <- list()
      for (var in available_vars) {
        if (is.numeric(event_data[[var]])) {
          balance_list[[var]] <- event_data[, .(
            treated_mean = mean(get(var)[is_treated == TRUE], na.rm = TRUE),
            control_mean = mean(get(var)[is_treated == FALSE], na.rm = TRUE),
            treated_sd = sd(get(var)[is_treated == TRUE], na.rm = TRUE),
            control_sd = sd(get(var)[is_treated == FALSE], na.rm = TRUE)
          )]
          balance_list[[var]][, `:=`(
            variable = var,
            standardized_diff = (treated_mean - control_mean) / 
              sqrt((treated_sd^2 + control_sd^2) / 2)
          )]
        }
      }
      if (length(balance_list) > 0) {
        balance_table <- rbindlist(balance_list)
      }
    }
  }
  
  # Data quality metrics
  data_quality <- list(
    missing_event_dates = sum(is.na(event_data$treatment_event_date)),
    missing_treatment_indicators = sum(is.na(event_data$is_treated)),
    invalid_event_windows = sum(event_data$in_event_window & is.na(event_data$days_to_event), na.rm = TRUE),
    duplicate_events = event_data[is_treated == TRUE, .N - uniqueN(cf)],
    data_completeness = 1 - sum(is.na(event_data)) / (nrow(event_data) * ncol(event_data))
  )
  
  # Recommendations
  recommendations <- character()
  
  if (event_summary$treatment_rate < 0.01) {
    recommendations <- c(recommendations, 
                        "Very low treatment rate - consider relaxing treatment conditions")
  }
  
  if (event_summary$treatment_rate > 0.8) {
    recommendations <- c(recommendations, 
                        "Very high treatment rate - consider more specific treatment conditions")
  }
  
  if (!is.null(balance_table) && any(abs(balance_table$standardized_diff) > 0.25)) {
    recommendations <- c(recommendations, 
                        "Large standardized differences detected - consider matching or stratification")
  }
  
  if (data_quality$data_completeness < 0.9) {
    recommendations <- c(recommendations, 
                        "High missing data rate - consider data cleaning or imputation")
  }
  
  # Compile results
  result <- list(
    event_summary = event_summary,
    timing_distribution = timing_dist,
    balance_table = balance_table,
    data_quality = data_quality,
    recommendations = recommendations
  )
  
  # Set class for printing
  class(result) <- c("treatment_event_assessment", "list")
  
  return(result)
}

#' Print method for treatment event assessment
#' @param x A treatment_event_assessment object
#' @param ... Additional arguments passed to print
#' @method print treatment_event_assessment
#' @export
print.treatment_event_assessment <- function(x, ...) {
  cat("Treatment Event Assessment\n")
  cat("==========================\n\n")
  
  cat("Event Summary:\n")
  cat("  Total observations:", x$event_summary$total_observations, "\n")
  cat("  Treated units:", x$event_summary$total_treated, "\n")
  cat("  Control units:", x$event_summary$total_controls, "\n")
  cat("  Treatment rate:", sprintf("%.2f%%", x$event_summary$treatment_rate * 100), "\n")
  cat("  Unique treated persons:", x$event_summary$unique_treated_persons, "\n")
  
  if (!is.null(x$timing_distribution)) {
    cat("\nTiming Distribution (days to event):\n")
    cat("  Min:", x$timing_distribution$min_days, "\n")
    cat("  Median:", x$timing_distribution$median_days, "\n")
    cat("  Max:", x$timing_distribution$max_days, "\n")
  }
  
  if (!is.null(x$balance_table)) {
    cat("\nBalance Assessment:\n")
    print(x$balance_table[, .(variable, treated_mean, control_mean, standardized_diff)])
  }
  
  if (length(x$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for (i in seq_along(x$recommendations)) {
      cat("  ", i, ". ", x$recommendations[i], "\n", sep = "")
    }
  }
  
  invisible(x)
}