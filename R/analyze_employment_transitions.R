# Helper function to process chain values like "val1->val2->val3"
.process_chain_value <- function(value, eval_chain) {
  # Handle NULL input
  if (is.null(value)) {
    return(value)
  }
  
  # Convert to character if not already
  value_char <- as.character(value)
  
  # Use vectorized operations for NA and empty string checks
  is_na_or_empty <- is.na(value_char) | value_char == ""
  
  # If eval_chain is "none", return original character values
  if (eval_chain == "none") {
    return(value_char)
  }
  
  # Initialize result vector
  result <- value_char
  
  # Find values that contain "->" indicating a chain
  has_chain <- grepl("->", value_char, fixed = TRUE) & !is_na_or_empty
  
  if (any(has_chain)) {
    # Process only values with chains
    chain_values <- value_char[has_chain]
    
    if (eval_chain == "last") {
      # Extract last part of each chain
      processed <- sapply(chain_values, function(x) {
        parts <- strsplit(x, "->", fixed = TRUE)[[1]]
        parts <- trimws(parts)
        parts[length(parts)]
      }, USE.NAMES = FALSE)
    } else if (eval_chain == "first") {
      # Extract first part of each chain
      processed <- sapply(chain_values, function(x) {
        parts <- strsplit(x, "->", fixed = TRUE)[[1]]
        parts <- trimws(parts)
        parts[1]
      }, USE.NAMES = FALSE)
    } else {
      # For any other eval_chain value, return original
      processed <- chain_values
    }
    
    # Update result for values with chains
    result[has_chain] <- processed
  }
  
  # Preserve NA values in the original positions
  result[is_na_or_empty] <- value_char[is_na_or_empty]
  
  return(result)
}

#' Analyze Employment Transitions from Pipeline Output
#'
#' @description
#' Analyzes employment transitions from the output of process_employment_pipeline().
#' Identifies transitions between employment periods that are separated by unemployment
#' periods and provides transition pattern analysis. The function allows specification
#' of which variable to use for transition analysis and which variables to compute
#' statistics for.
#'
#' With the \code{use_consolidated_periods} option, the function can first consolidate
#' overlapping and/or consecutive employment periods using the \code{over_id} column
#' from vecshift() output, providing more accurate transition analysis by treating
#' continuous employment episodes as single periods rather than administrative splits.
#'
#' @details
#' A transition occurs when there are consecutive employment periods (arco >= 1) 
#' separated by an unemployment period (arco = 0) of at least the minimum duration.
#' The function analyzes patterns in the "from" → "to" transitions for one specified
#' transition variable, while computing summary statistics for additional variables.
#' 
#' The \code{eval_chain} parameter provides flexible handling of chained values that 
#' contain "->" separators, allowing extraction of the first value, last value, or 
#' preservation of the complete chain for complex transition analysis scenarios.
#' 
#' When \code{use_consolidated_periods = TRUE}, the function first applies 
#' \code{merge_consecutive_employment()} to consolidate employment periods based on
#' the \code{over_id} column. This provides several key benefits:
#' \itemize{
#'   \item{\strong{Accurate Transitions}}: Analyzes transitions between true employment
#'     episodes rather than administrative contract splits
#'   \item{\strong{Better Unemployment Duration}}: More precise calculation of time
#'     between actual employment periods
#'   \item{\strong{Cleaner Patterns}}: Reduces noise from overlapping contracts that
#'     represent the same underlying employment relationship
#'   \item{\strong{Career Progression}}: Better identification of genuine career moves
#'     vs. contract renewals or administrative changes
#' }
#' 
#' For each transition, the function provides:
#' \itemize{
#'   \item{\strong{from}}: Value in the employment period before unemployment (transition variable)
#'   \item{\strong{to}}: Value in the employment period after unemployment (transition variable)
#'   \item{\strong{weight}}: Number of transitions (.N)
#'   \item{\strong{transition_duration}}: Mean duration of intermediate unemployment periods
#'   \item{\strong{For numeric statistics variables}}: [variable]_from_median, [variable]_to_median
#'   \item{\strong{For character statistics variables}}: [variable]_from_mode, [variable]_to_mode
#' }
#'
#' @param pipeline_result Output from process_employment_pipeline(). Must be a data.table
#'   with columns: cf (person identifier), arco (employment overlap count), 
#'   inizio/fine (period dates), durata (period duration), and optionally over_id
#'   (overlap identifier, required if use_consolidated_periods = TRUE).
#' @param transition_variable Character string specifying the variable to use for 
#'   transition analysis (from/to values). If NULL (default), uses the first 
#'   non-standard attribute in the data.table.
#' @param statistics_variables Character vector specifying variables to compute 
#'   summary statistics for. If NULL (default), uses all non-standard attributes 
#'   except the transition variable.
#' @param min_unemployment_duration Minimum duration (in days) of unemployment period 
#'   to consider a transition (default: 1).
#' @param max_unemployment_duration Maximum duration (in days) of unemployment period 
#'   to consider a transition. If NULL (default), no upper limit is applied. When not 
#'   NULL, only transitions with unemployment duration between min_unemployment_duration 
#'   and max_unemployment_duration (inclusive) are included.
#' @param output_transition_matrix Logical. If TRUE, returns a square transition matrix 
#'   instead of the normal aggregated data.table. Rows represent "from" states, columns 
#'   represent "to" states, and values are transition weights (counts). Non-populated 
#'   cells contain zeros. Matrix uses unique values from the transition_variable as 
#'   row/column names (default: FALSE).
#' @param eval_chain Character string specifying how to handle chained values in 
#'   from/to columns that contain "->" separators (default: "last"). Options:
#'   \itemize{
#'     \item{\code{"last"}}: Extract the last value from chains like "val1->val2->val3" (returns "val3")
#'     \item{\code{"first"}}: Extract the first value from chains (returns "val1")
#'     \item{\code{"none"}}: Leave chain values unchanged (returns "val1->val2->val3")
#'   }
#'   When there is only one value (no "->"), the original value is always used regardless 
#'   of this parameter.
#' @param use_consolidated_periods Logical. If TRUE (default), first consolidate
#'   employment periods using merge_consecutive_employment() before analyzing transitions.
#'   This provides more accurate transition analysis by treating overlapping and/or
#'   consecutive employment periods as single episodes. Requires over_id column from
#'   vecshift() output.
#' @param consolidation_type Character string specifying consolidation approach when
#'   use_consolidated_periods = TRUE (default: "both"). Options:
#'   \itemize{
#'     \item{\code{"both"}}: First consolidate overlapping periods (same over_id > 0),
#'       then merge consecutive periods. Provides complete employment history consolidation.
#'     \item{\code{"overlapping"}}: Only consolidate segments with same over_id > 0.
#'       Merges simultaneous/overlapping contracts into single periods.
#'     \item{\code{"consecutive"}}: Only merge periods that are contiguous in time,
#'       regardless of over_id. Traditional consecutive period merging.
#'     \item{\code{"none"}}: No consolidation (equivalent to use_consolidated_periods = FALSE).
#'   }
#' @param show_progress Logical. If TRUE (default), displays a progress bar showing
#'   the current processing step, percentage completion, and estimated time remaining.
#'   Uses the 'progress' package if available, falls back to utils::txtProgressBar or
#'   simple messages if not available.
#'
#' @return When \code{output_transition_matrix} is FALSE (default), returns a data.table with columns:
#'   \itemize{
#'     \item{\code{from}}: Value before transition (from transition_variable, processed according to eval_chain parameter)
#'     \item{\code{to}}: Value after transition (from transition_variable, processed according to eval_chain parameter)
#'     \item{\code{weight}}: Number of transitions
#'     \item{\code{transition_duration}}: Mean unemployment duration
#'     \item{\code{[variable]_from_median/[variable]_from_mode}}: For each statistics variable, 
#'       duration-weighted aggregated values from the "from" period (median for numeric, mode for character)
#'     \item{\code{[variable]_to_median/[variable]_to_mode}}: For each statistics variable, 
#'       duration-weighted aggregated values from the "to" period (median for numeric, mode for character)
#'   }
#'   
#'   When \code{output_transition_matrix} is TRUE, returns a square matrix where rows represent 
#'   "from" states, columns represent "to" states, and values are transition weights (counts). 
#'   Non-populated cells contain zeros.
#'   
#'   Note: If the transition variable is also in statistics_variables, it will have
#'   corresponding [transition_variable]_from_median and [transition_variable]_to_median columns.
#'
#' @export
#' @importFrom data.table data.table setorder copy setnames shift set :=
#' @importFrom stats median weighted.mean
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with transitions
#' employment_data <- data.table(
#'   id = 1:6,
#'   cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
#'                      "2023-02-01", "2023-06-01", "2023-10-01")),
#'   FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
#'                    "2023-04-30", "2023-08-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 1, 0),
#'   company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
#'   salary = c(50000, 25000, 60000, 55000, 65000, 30000)
#' )
#' 
#' # Process through pipeline
#' result <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = c("company", "salary")
#' )
#' 
#' # Analyze company transitions with salary statistics (using consolidated periods)
#' transitions <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   statistics_variables = c("salary"),
#'   use_consolidated_periods = TRUE,
#'   consolidation_type = "both"
#' )
#' print(transitions)
#' 
#' # Compare with original (non-consolidated) analysis
#' transitions_original <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   statistics_variables = c("salary"),
#'   use_consolidated_periods = FALSE
#' )
#' print(transitions_original)
#' 
#' # Analyze salary transitions with company statistics and minimum unemployment duration
#' transitions_salary <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "salary",
#'   statistics_variables = c("company"),
#'   min_unemployment_duration = 7,
#'   show_progress = FALSE
#' )
#' 
#' # Analyze transitions with duration constraints (unemployment between 7-30 days)
#' transitions_constrained <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   min_unemployment_duration = 7,
#'   max_unemployment_duration = 30
#' )
#' 
#' # Get transition matrix instead of data.table
#' transition_matrix <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   output_transition_matrix = TRUE
#' )
#' print(transition_matrix)
#' 
#' # Example using eval_chain parameter with chained values
#' # Suppose your data has chained company transitions like "CompanyA->CompanyB->CompanyC"
#' sample_data_chains <- copy(result)
#' sample_data_chains[, company := ifelse(company == "CompanyA", "StartupA->CompanyA->MegaCorp", company)]
#' 
#' # Extract last company in chain (default behavior)
#' transitions_last <- analyze_employment_transitions(
#'   pipeline_result = sample_data_chains,
#'   transition_variable = "company",
#'   eval_chain = "last"  # "StartupA->CompanyA->MegaCorp" becomes "MegaCorp"
#' )
#' 
#' # Extract first company in chain
#' transitions_first <- analyze_employment_transitions(
#'   pipeline_result = sample_data_chains,
#'   transition_variable = "company",
#'   eval_chain = "first"  # "StartupA->CompanyA->MegaCorp" becomes "StartupA"
#' )
#' 
#' # Keep full chain unchanged
#' transitions_full <- analyze_employment_transitions(
#'   pipeline_result = sample_data_chains,
#'   transition_variable = "company",
#'   eval_chain = "none"  # Keeps "StartupA->CompanyA->MegaCorp" as is
#' )
#' 
#' # Example showing impact of consolidation:
#' # Without consolidation: May show transitions between overlapping contracts
#' # CompanyA -> CompanyA (due to contract renewals during same employment episode)
#' # With consolidation: Shows true transitions between different employers
#' # CompanyA -> CompanyB (actual career moves)
#' 
#' # Output will include columns like:
#' # from, to (salary values for transitions)
#' # company_from_mode, company_to_mode (company statistics)
#' # salary_from_median, salary_to_median (if salary is in statistics_variables)
#' }
analyze_employment_transitions <- function(pipeline_result,
                                         transition_variable = NULL,
                                         statistics_variables = NULL,
                                         min_unemployment_duration = 1,
                                         max_unemployment_duration = NULL,
                                         use_consolidated_periods = TRUE,
                                         consolidation_type = "both",
                                         output_transition_matrix = FALSE,
                                         eval_chain = "last",
                                         show_progress = TRUE) {
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Check for collapse package (use base R alternatives if not available)
  use_collapse <- requireNamespace("collapse", quietly = TRUE)
  
  # Input validation
  if (!inherits(pipeline_result, "data.table")) {
    stop("Parameter 'pipeline_result' must be a data.table object from process_employment_pipeline().")
  }
  
  # Check for required columns
  required_cols <- c("cf", "arco", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(pipeline_result))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in pipeline_result:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate parameters
  if (!is.numeric(min_unemployment_duration) || min_unemployment_duration < 0) {
    stop("Parameter 'min_unemployment_duration' must be a non-negative numeric value.")
  }
  
  if (!is.null(max_unemployment_duration)) {
    if (!is.numeric(max_unemployment_duration) || max_unemployment_duration < 0) {
      stop("Parameter 'max_unemployment_duration' must be NULL or a non-negative numeric value.")
    }
    if (max_unemployment_duration < min_unemployment_duration) {
      stop("Parameter 'max_unemployment_duration' must be greater than or equal to 'min_unemployment_duration'.")
    }
  }
  
  if (!is.logical(output_transition_matrix) || length(output_transition_matrix) != 1) {
    stop("Parameter 'output_transition_matrix' must be a single logical value (TRUE or FALSE).")
  }
  
  if (!is.logical(show_progress)) {
    stop("Parameter 'show_progress' must be a logical value (TRUE or FALSE).")
  }
  
  if (!is.logical(use_consolidated_periods) || length(use_consolidated_periods) != 1) {
    stop("Parameter 'use_consolidated_periods' must be a single logical value (TRUE or FALSE).")
  }
  
  # Validate consolidation_type
  valid_consolidation_types <- c("both", "overlapping", "consecutive", "none")
  if (!consolidation_type %in% valid_consolidation_types) {
    stop(paste("Parameter 'consolidation_type' must be one of:", 
               paste(valid_consolidation_types, collapse = ", ")))
  }
  
  # Validate eval_chain parameter
  valid_eval_chain <- c("last", "first", "none")
  if (!eval_chain %in% valid_eval_chain) {
    stop(paste("Parameter 'eval_chain' must be one of:", 
               paste(valid_eval_chain, collapse = ", ")))
  }
  
  # Check for over_id column if consolidation is requested
  if (use_consolidated_periods && consolidation_type != "none") {
    if (!"over_id" %in% names(pipeline_result)) {
      stop("Parameter 'use_consolidated_periods = TRUE' requires 'over_id' column from vecshift() output. ", 
           "Either set use_consolidated_periods = FALSE or ensure pipeline_result contains over_id column.")
    }
  }
  
  # Identify available columns (exclude standard vecshift columns)
  standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                     "collapsed", "n_periods", "over_id")
  available_cols <- setdiff(names(pipeline_result), standard_cols)
  
  if (length(available_cols) == 0) {
    stop("No additional columns found for analysis. Pipeline result should contain variables beyond standard vecshift output columns.")
  }
  
  # Determine transition variable
  if (is.null(transition_variable)) {
    # Use first available column as default
    transition_variable <- available_cols[1]
    if (show_progress) {
      message(sprintf("Using '%s' as transition variable (first available attribute)", transition_variable))
    }
  }
  
  # Validate transition variable
  if (!is.character(transition_variable) || length(transition_variable) != 1) {
    stop("Parameter 'transition_variable' must be a single character string.")
  }
  
  if (!transition_variable %in% names(pipeline_result)) {
    stop(sprintf("Transition variable '%s' not found in pipeline_result.", transition_variable))
  }
  
  # Determine statistics variables
  if (is.null(statistics_variables)) {
    # Use all available columns except the transition variable
    statistics_variables <- setdiff(available_cols, transition_variable)
    if (show_progress && length(statistics_variables) > 0) {
      message(sprintf("Using %d variables for statistics: %s", 
                     length(statistics_variables), 
                     paste(statistics_variables, collapse = ", ")))
    }
  }
  
  # Validate statistics variables
  if (!is.character(statistics_variables)) {
    stop("Parameter 'statistics_variables' must be a character vector.")
  }
  
  missing_stats_cols <- setdiff(statistics_variables, names(pipeline_result))
  if (length(missing_stats_cols) > 0) {
    stop(paste("Statistics variables not found in pipeline_result:",
               paste(missing_stats_cols, collapse = ", ")))
  }
  
  # Initialize progress tracking
  start_time <- Sys.time()
  
  if (show_progress) {
    message("Starting employment transition analysis...")
  }
  
  # Apply period consolidation if requested
  if (use_consolidated_periods && consolidation_type != "none") {
    # Check if consolidation has already been done
    if ("collapsed" %in% names(pipeline_result)) {
      if (show_progress) {
        message("Data already contains 'collapsed' column - skipping consolidation step")
      }
      dt <- copy(pipeline_result)
    } else {
      if (show_progress) {
        message(sprintf("Consolidating employment periods using '%s' strategy...", consolidation_type))
      }
      
      # Use fast consolidation function with type support
      if (exists("merge_consecutive_employment_fast_with_type", mode = "function")) {
        # Pre-process data to ensure type consistency before consolidation
        # This prevents data.table column type mismatch errors during aggregation
        pipeline_copy <- copy(pipeline_result)
        
        # Ensure consistent column types to prevent aggregation errors
        # Convert potentially mixed integer/numeric columns to numeric
        for (col_name in names(pipeline_copy)) {
          col_class <- class(pipeline_copy[[col_name]])[1]
          if (col_class %in% c("integer", "numeric")) {
            # Check if column has mixed types that could cause issues during aggregation
            if (col_class == "integer") {
              # Convert integers to numeric to avoid type conflicts during consolidation
              # This is safer as numeric can handle both integer and decimal aggregations
              data.table::set(pipeline_copy, j = col_name, value = as.numeric(pipeline_copy[[col_name]]))
            }
          }
        }
        
        dt <- merge_consecutive_employment_fast_with_type(pipeline_copy, consolidation_type = consolidation_type)
      } else if (exists("merge_consecutive_employment_fast", mode = "function")) {
        # Use fast version without type parameter (vecshift 0.9.0+)
        if (show_progress) {
          message("Using fast consolidation method")
        }
        
        # Pre-process data to ensure type consistency before consolidation
        # This prevents data.table column type mismatch errors during aggregation
        pipeline_copy <- copy(pipeline_result)
        
        # Ensure consistent column types to prevent aggregation errors
        # Convert potentially mixed integer/numeric columns to numeric
        for (col_name in names(pipeline_copy)) {
          col_class <- class(pipeline_copy[[col_name]])[1]
          if (col_class %in% c("integer", "numeric")) {
            # Check if column has mixed types that could cause issues during aggregation
            if (col_class == "integer") {
              # Convert integers to numeric to avoid type conflicts during consolidation
              # This is safer as numeric can handle both integer and decimal aggregations
              data.table::set(pipeline_copy, j = col_name, value = as.numeric(pipeline_copy[[col_name]]))
            }
          }
        }
        
        dt <- merge_consecutive_employment_fast(pipeline_copy)
      } else if (exists("merge_consecutive_employment", mode = "function")) {
        # Fall back to slow function if fast version not available
        if (show_progress) {
          message("Using standard (slower) consolidation method - consider updating vecshift package")
        }
        
        # Pre-process data to ensure type consistency before consolidation
        # This prevents data.table column type mismatch errors during aggregation
        pipeline_copy <- copy(pipeline_result)
        
        # Ensure consistent column types to prevent aggregation errors
        # Convert potentially mixed integer/numeric columns to numeric
        for (col_name in names(pipeline_copy)) {
          col_class <- class(pipeline_copy[[col_name]])[1]
          if (col_class %in% c("integer", "numeric")) {
            # Check if column has mixed types that could cause issues during aggregation
            if (col_class == "integer") {
              # Convert integers to numeric to avoid type conflicts during consolidation
              # This is safer as numeric can handle both integer and decimal aggregations
              data.table::set(pipeline_copy, j = col_name, value = as.numeric(pipeline_copy[[col_name]]))
            }
          }
        }
        
        dt <- merge_consecutive_employment(pipeline_copy, consolidation_type = consolidation_type)
      } else {
        stop("No consolidation function found. Ensure the vecshift package is properly loaded.")
      }
      
      # Update statistics variables list to only include columns that still exist after consolidation
      missing_after_consolidation <- setdiff(statistics_variables, names(dt))
      if (length(missing_after_consolidation) > 0) {
        if (show_progress) {
          message(sprintf("Removing %d statistics variables that were dropped during consolidation: %s", 
                         length(missing_after_consolidation),
                         paste(missing_after_consolidation, collapse = ", ")))
        }
        statistics_variables <- intersect(statistics_variables, names(dt))
      }
      
      if (show_progress) {
        n_original <- nrow(pipeline_result)
        n_consolidated <- nrow(dt)
        reduction_pct <- round((1 - n_consolidated/n_original) * 100, 1)
        message(sprintf("Consolidated %d periods to %d (%.1f%% reduction)", 
                       n_original, n_consolidated, reduction_pct))
      }
    }
  } else {
    # Work with a copy to avoid modifying original data
    dt <- copy(pipeline_result)
  }
  
  # Ensure proper ordering by person and time
  setorder(dt, cf, inizio)
  
  if (show_progress) {
    message("Filtering persons with sufficient periods for transitions...")
  }
  
  # Count periods per person and filter out those with ≤ 2 periods (can't have transitions)
  person_counts <- dt[, .N, by = cf]
  multi_period_persons <- person_counts[N > 2, cf]
  
  if (length(multi_period_persons) == 0) {
    if (show_progress) {
      message("No persons found with sufficient periods for transition analysis.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns for each statistics variable
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  # Filter dataset to only multi-period persons
  dt <- dt[cf %in% multi_period_persons]
  
  if (show_progress) {
    message("Identifying employment transitions...")
  }
  
  # Use shift operations to get previous, current, and next period information
  # Note: When consolidated periods are used, this operates on consolidated episodes
  # rather than individual contracts, providing cleaner transition patterns
  dt[, `:=`(
    prev_arco = shift(arco, n = 1, type = "lag"),
    next_arco = shift(arco, n = 1, type = "lead")
  ), by = cf]
  
  # Add from/to values for transition variable
  from_transition_col <- paste0("from_", transition_variable)
  to_transition_col <- paste0("to_", transition_variable)
  dt[, (from_transition_col) := shift(get(transition_variable), n = 1, type = "lag"), by = cf]
  dt[, (to_transition_col) := shift(get(transition_variable), n = 1, type = "lead"), by = cf]
  
  # Add from/to values for statistics variables
  for (stat_var in statistics_variables) {
    from_stat_col <- paste0("from_", stat_var)
    to_stat_col <- paste0("to_", stat_var)
    dt[, (from_stat_col) := shift(get(stat_var), n = 1, type = "lag"), by = cf]
    dt[, (to_stat_col) := shift(get(stat_var), n = 1, type = "lead"), by = cf]
  }
  
  # Also capture the durations of the from and to employment periods for weighting
  dt[, from_durata := shift(durata, n = 1, type = "lag"), by = cf]
  dt[, to_durata := shift(durata, n = 1, type = "lead"), by = cf]
  
  # Identify unemployment periods that are part of transitions
  # Current period: unemployment (arco == 0), previous: employment (prev_arco >= 1), next: employment (next_arco >= 1)
  if (is.null(max_unemployment_duration)) {
    dt[, is_transition_unemployment := (
      !is.na(prev_arco) & prev_arco >= 1 &          # Previous: employment
      !is.na(arco) & arco == 0 &                    # Current: unemployment
      !is.na(next_arco) & next_arco >= 1 &          # Next: employment  
      !is.na(durata) & durata >= min_unemployment_duration  # Min duration
    )]
  } else {
    dt[, is_transition_unemployment := (
      !is.na(prev_arco) & prev_arco >= 1 &          # Previous: employment
      !is.na(arco) & arco == 0 &                    # Current: unemployment
      !is.na(next_arco) & next_arco >= 1 &          # Next: employment  
      !is.na(durata) & durata >= min_unemployment_duration &  # Min duration
      !is.na(durata) & durata <= max_unemployment_duration    # Max duration
    )]
  }
  
  # Filter to only transition unemployment periods
  transition_periods <- dt[is_transition_unemployment == TRUE]
  
  if (nrow(transition_periods) == 0) {
    if (show_progress) {
      message("No employment transitions found matching criteria.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns for each statistics variable
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  if (show_progress) {
    message("Computing transition statistics...")
  }
  
  # Create subset with required columns for aggregation
  required_cols <- c("cf", "durata", from_transition_col, to_transition_col, "from_durata", "to_durata")
  
  # Add statistics variable columns
  stat_from_cols <- paste0("from_", statistics_variables)
  stat_to_cols <- paste0("to_", statistics_variables)
  all_stat_cols <- c(stat_from_cols, stat_to_cols)
  
  # Filter to only include columns that exist in the dataset
  existing_stat_cols <- intersect(all_stat_cols, names(transition_periods))
  
  # Create subset
  col_subset <- transition_periods[, c(required_cols, existing_stat_cols), with = FALSE]
  
  # Rename transition columns for consistent processing
  setnames(col_subset, c(from_transition_col, to_transition_col), c("from", "to"))
  
  # Process chain values in from/to columns based on eval_chain parameter
  if (eval_chain != "none") {
    col_subset[, from := .process_chain_value(from, eval_chain)]
    col_subset[, to := .process_chain_value(to, eval_chain)]
  }
  
  # Remove rows where either from or to is NA for the transition variable
  transitions_data <- col_subset[!is.na(from) & !is.na(to)]
  
  if (nrow(transitions_data) == 0) {
    if (show_progress) {
      message("No valid transitions found after filtering missing values.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  # Create base aggregation with transition counts and duration
  if (use_collapse) {
    base_agg <- transitions_data[, .(
      weight = .N,
      transition_duration = collapse::fmean(durata)
    ), by = .(from, to)]
  } else {
    base_agg <- transitions_data[, .(
      weight = .N,
      transition_duration = mean(durata, na.rm = TRUE)
    ), by = .(from, to)]
  }
  
  # Add statistics for each statistics variable
  for (stat_var in statistics_variables) {
    from_stat_col <- paste0("from_", stat_var)
    to_stat_col <- paste0("to_", stat_var)
    
    # Check if both from and to columns exist in the data
    if (from_stat_col %in% names(transitions_data) && to_stat_col %in% names(transitions_data)) {
      
      if (is.numeric(pipeline_result[[stat_var]])) {
        # For numeric variables: compute duration-weighted median
        from_col_name <- paste0(stat_var, "_from_median")
        to_col_name <- paste0(stat_var, "_to_median")
        
        stat_agg <- transitions_data[, {
          from_value <- {
            w <- as.numeric(from_durata[!is.na(get(from_stat_col)) & !is.na(from_durata)])
            v <- as.numeric(get(from_stat_col))[!is.na(get(from_stat_col)) & !is.na(from_durata)]
            if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
              # For weighted median, use repeated values approach
              rep_v <- rep(v, times = pmax(1, round(w)))  # Ensure minimum weight of 1
              as.numeric(median(rep_v, na.rm = TRUE))
            } else {
              as.numeric(median(as.numeric(get(from_stat_col)), na.rm = TRUE))
            }
          }
          to_value <- {
            w <- as.numeric(to_durata[!is.na(get(to_stat_col)) & !is.na(to_durata)])
            v <- as.numeric(get(to_stat_col))[!is.na(get(to_stat_col)) & !is.na(to_durata)]
            if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
              # For weighted median, use repeated values approach
              rep_v <- rep(v, times = pmax(1, round(w)))  # Ensure minimum weight of 1
              as.numeric(median(rep_v, na.rm = TRUE))
            } else {
              as.numeric(median(as.numeric(get(to_stat_col)), na.rm = TRUE))
            }
          }
          
          result <- list(from_value, to_value)
          names(result) <- c(from_col_name, to_col_name)
          result
        }, by = .(from, to)]
        
      } else {
        # For character/factor variables: compute mode
        from_col_name <- paste0(stat_var, "_from_mode")
        to_col_name <- paste0(stat_var, "_to_mode")
        
        stat_agg <- transitions_data[, {
          if (use_collapse) {
            from_value <- collapse::fmode(get(from_stat_col))
            to_value <- collapse::fmode(get(to_stat_col))
          } else {
            # Base R mode calculation (most frequent value)
            from_value <- {
              tbl <- table(get(from_stat_col), useNA = "no")
              if (length(tbl) > 0) names(tbl)[which.max(tbl)] else NA_character_
            }
            to_value <- {
              tbl <- table(get(to_stat_col), useNA = "no")
              if (length(tbl) > 0) names(tbl)[which.max(tbl)] else NA_character_
            }
          }
          
          result <- list(from_value, to_value)
          names(result) <- c(from_col_name, to_col_name)
          result
        }, by = .(from, to)]
      }
      
      # Merge with base aggregation
      base_agg <- merge(base_agg, stat_agg, by = c("from", "to"), all.x = TRUE)
    }
  }
  
  # Order by weight (most common transitions first)
  setorder(base_agg, -weight)
  
  # Show completion message
  if (show_progress) {
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    total_transitions <- sum(base_agg$weight, na.rm = TRUE)
    
    message(sprintf("Transition analysis completed in %.2f seconds. Found %d unique transition patterns with %d total transitions.", 
                   as.numeric(elapsed_time), nrow(base_agg), total_transitions))
  }
  
  # Return transition matrix if requested
  if (output_transition_matrix) {
    # Get unique from/to values to create matrix dimensions
    unique_states <- sort(unique(c(base_agg$from, base_agg$to)))
    
    # Create empty matrix with zeros
    transition_matrix <- matrix(0, 
                               nrow = length(unique_states), 
                               ncol = length(unique_states),
                               dimnames = list(unique_states, unique_states))
    
    # Fill matrix with transition weights
    for (i in 1:nrow(base_agg)) {
      from_state <- as.character(base_agg$from[i])
      to_state <- as.character(base_agg$to[i])
      weight <- base_agg$weight[i]
      
      transition_matrix[from_state, to_state] <- weight
    }
    
    return(transition_matrix)
  }
  
  return(base_agg)
}


#' Analyze Consolidated Employment Periods Using over_id
#'
#' @description
#' Provides comprehensive analysis of consolidated employment periods leveraging the over_id 
#' functionality from vecshift() output. This function analyzes employment periods grouped by 
#' over_id to understand the benefits and patterns of employment consolidation, comparing
#' individual contracts to consolidated employment episodes.
#'
#' @details
#' The over_id column from vecshift() identifies continuous overlapping employment periods:
#' \itemize{
#'   \item \strong{over_id = 0}: Unemployment periods (no active contracts)
#'   \item \strong{over_id > 0}: Employment periods with same value for overlapping/continuous contracts
#'   \item \strong{Same over_id}: All contracts belonging to same continuous overlapping time period
#' }
#' 
#' This function provides insights into:
#' \itemize{
#'   \item \strong{Consolidation Benefits}: How much employment history is simplified
#'   \item \strong{Employment Complexity}: Patterns of multiple simultaneous jobs
#'   \item \strong{Administrative Efficiency}: Time savings from consolidated view
#'   \item \strong{Employment Patterns}: Duration and frequency of consolidated vs individual periods
#' }
#'
#' @param segments Input data from vecshift() with over_id column. Must be a data.table
#'   with required columns: cf, inizio, fine, arco, durata, over_id, and optionally
#'   additional employment attributes for detailed analysis.
#' @param level Character string specifying analysis granularity:
#'   \itemize{
#'     \item{\code{"person"}}: Person-level analysis with individual statistics
#'     \item{\code{"aggregate"}}: Population-level aggregate statistics
#'     \item{\code{"both"}}: Both person and aggregate analysis (default)
#'   }
#' @param include_unemployment Logical. If TRUE (default), includes unemployment periods
#'   (over_id = 0) in the analysis. If FALSE, focuses only on employment consolidation.
#' @param consolidation_type Character string specifying consolidation approach for comparison:
#'   \itemize{
#'     \item{\code{"both"}}: Compare against both overlapping and consecutive consolidation (default)
#'     \item{\code{"overlapping"}}: Compare against overlapping consolidation only
#'     \item{\code{"employment_only"}}: Focus on employment periods only, ignore unemployment
#'   }
#' @param min_employment_duration Minimum duration (days) for employment periods to include
#'   in analysis (default: 1). Helps filter out very short-term contracts.
#' @param include_details Logical. If TRUE (default), includes detailed breakdowns and
#'   comparisons. If FALSE, returns only summary statistics for performance.
#'
#' @return When \code{level} is "person" or "both", returns a list containing:
#'   \itemize{
#'     \item{\code{person_analysis}}: data.table with person-level statistics including:
#'       \itemize{
#'         \item{\code{cf}}: Person identifier
#'         \item{\code{total_periods_original}}: Count of original vecshift segments
#'         \item{\code{total_periods_consolidated}}: Count of consolidated periods (unique over_id)
#'         \item{\code{employment_periods_original}}: Employment segments (arco >= 1)
#'         \item{\code{employment_periods_consolidated}}: Consolidated employment periods (over_id > 0)
#'         \item{\code{unemployment_periods}}: Unemployment segments (over_id = 0)
#'         \item{\code{consolidation_ratio}}: Reduction in period count (1 - consolidated/original)
#'         \item{\code{employment_complexity_avg}}: Average arco for employment periods
#'         \item{\code{max_simultaneous_jobs}}: Maximum concurrent employment (max arco)
#'         \item{\code{total_employment_days_original}}: Sum of original employment durations
#'         \item{\code{total_employment_days_consolidated}}: Sum of consolidated employment durations
#'         \item{\code{avg_employment_duration_original}}: Mean duration of original employment segments
#'         \item{\code{avg_employment_duration_consolidated}}: Mean duration of consolidated periods
#'         \item{\code{duration_efficiency}}: Duration consistency (consolidated/original ratio)
#'         \item{\code{overlapping_episodes}}: Count of over_id periods with multiple contracts
#'       }
#'     \item{\code{aggregate_analysis}}: data.table with population-level summary statistics
#'     \item{\code{consolidation_benefits}}: data.table showing consolidation impact analysis
#'     \item{\code{employment_complexity}}: data.table with employment complexity patterns
#'   }
#'   
#'   When \code{level} is "aggregate", returns only the aggregate analysis components.
#'   
#'   All results include attributes:
#'   \itemize{
#'     \item{\code{analysis_parameters}}: Analysis configuration used
#'     \item{\code{validation_results}}: Data validation and quality checks
#'     \item{\code{computation_time}}: Time taken for analysis
#'   }
#'
#' @export
#' @importFrom data.table data.table setDT copy .N .SD := setorder uniqueN
#' @importFrom stats median quantile
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with overlapping periods
#' employment_data <- data.table(
#'   id = 1:8,
#'   cf = c(rep("PERSON001", 5), rep("PERSON002", 3)),
#'   INIZIO = as.Date(c("2023-01-01", "2023-03-01", "2023-03-15", "2023-06-01", "2023-08-01",
#'                      "2023-02-01", "2023-05-01", "2023-07-01")),
#'   FINE = as.Date(c("2023-02-28", "2023-03-31", "2023-05-31", "2023-07-31", "2023-12-31",
#'                    "2023-04-30", "2023-06-30", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 1, 1, 0, 1),
#'   company = c("CompanyA", "CompanyB", "CompanyA", "CompanyC", "CompanyD",
#'               "CompanyE", "CompanyF", "CompanyG"),
#'   salary = c(50000, 25000, 52000, 60000, 55000, 45000, 30000, 65000)
#' )
#' 
#' # Apply vecshift to get segments with over_id
#' segments <- vecshift(employment_data)
#' 
#' # Comprehensive analysis with both person and aggregate levels
#' analysis_full <- analyze_consolidated_periods(
#'   segments = segments,
#'   level = "both",
#'   include_unemployment = TRUE,
#'   consolidation_type = "both",
#'   include_details = TRUE
#' )
#' 
#' # View person-level results
#' print(analysis_full$person_analysis)
#' print(analysis_full$consolidation_benefits)
#' 
#' # View aggregate statistics
#' print(analysis_full$aggregate_analysis)
#' print(analysis_full$employment_complexity)
#' 
#' # Focus on employment consolidation only
#' analysis_employment <- analyze_consolidated_periods(
#'   segments = segments,
#'   level = "aggregate",
#'   include_unemployment = FALSE,
#'   consolidation_type = "employment_only",
#'   min_employment_duration = 7
#' )
#' print(analysis_employment$aggregate_analysis)
#' 
#' # Person-level analysis for detailed insights
#' analysis_person <- analyze_consolidated_periods(
#'   segments = segments,
#'   level = "person",
#'   consolidation_type = "overlapping"
#' )
#' print(analysis_person$person_analysis)
#' 
#' # Check analysis parameters and validation
#' print(attr(analysis_full, "analysis_parameters"))
#' print(attr(analysis_full, "validation_results"))
#' }
analyze_consolidated_periods <- function(segments,
                                       level = "both",
                                       include_unemployment = TRUE,
                                       consolidation_type = "both",
                                       min_employment_duration = 1,
                                       include_details = TRUE) {
  
  # Record start time for performance tracking
  start_time <- Sys.time()
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Input validation
  if (!inherits(segments, "data.table")) {
    stop("Parameter 'segments' must be a data.table object from vecshift() output.")
  }
  
  # Validate level parameter
  valid_levels <- c("person", "aggregate", "both")
  if (!level %in% valid_levels) {
    stop(paste("Parameter 'level' must be one of:", paste(valid_levels, collapse = ", ")))
  }
  
  # Validate consolidation_type parameter
  valid_consolidation_types <- c("both", "overlapping", "employment_only")
  if (!consolidation_type %in% valid_consolidation_types) {
    stop(paste("Parameter 'consolidation_type' must be one of:", paste(valid_consolidation_types, collapse = ", ")))
  }
  
  # Check for required columns
  required_cols <- c("cf", "inizio", "fine", "arco", "durata")
  missing_cols <- setdiff(required_cols, names(segments))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in segments:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for over_id column (essential for this analysis)
  if (!"over_id" %in% names(segments)) {
    stop("Parameter 'segments' must contain 'over_id' column from vecshift() output. ",
         "This function specifically analyzes consolidated periods using over_id.")
  }
  
  # Validate numeric parameters
  if (!is.numeric(min_employment_duration) || min_employment_duration < 0) {
    stop("Parameter 'min_employment_duration' must be a non-negative numeric value.")
  }
  
  # Validate logical parameters
  if (!is.logical(include_unemployment) || !is.logical(include_details)) {
    stop("Parameters 'include_unemployment' and 'include_details' must be logical values.")
  }
  
  # Work with a copy to avoid modifying original data
  dt <- copy(segments)
  setDT(dt)
  
  # Ensure proper ordering
  setorder(dt, cf, inizio)
  
  # Validation results storage
  validation_results <- list(
    input_validation = list(
      has_over_id = "over_id" %in% names(dt),
      total_records = nrow(dt),
      total_persons = uniqueN(dt$cf),
      date_range = if (nrow(dt) > 0) c(min(dt$inizio, na.rm = TRUE), max(dt$fine, na.rm = TRUE)) else c(NA, NA),
      has_employment_periods = any(dt$arco >= 1, na.rm = TRUE),
      has_unemployment_periods = any(dt$arco == 0, na.rm = TRUE),
      over_id_range = if (nrow(dt) > 0) c(min(dt$over_id, na.rm = TRUE), max(dt$over_id, na.rm = TRUE)) else c(NA, NA)
    )
  )
  
  # Filter by minimum employment duration if specified
  if (min_employment_duration > 1) {
    original_nrow <- nrow(dt)
    dt <- dt[arco == 0 | (arco >= 1 & durata >= min_employment_duration)]
    validation_results$duration_filter <- list(
      records_before = original_nrow,
      records_after = nrow(dt),
      records_filtered = original_nrow - nrow(dt)
    )
  }
  
  # Early exit if no data after filtering
  if (nrow(dt) == 0) {
    warning("No records remaining after filtering. Returning empty results.")
    empty_result <- list(
      person_analysis = data.table(),
      aggregate_analysis = data.table(),
      consolidation_benefits = data.table(),
      employment_complexity = data.table()
    )
    
    # Add attributes
    setattr(empty_result, "analysis_parameters", list(
      level = level,
      include_unemployment = include_unemployment,
      consolidation_type = consolidation_type,
      min_employment_duration = min_employment_duration,
      include_details = include_details
    ))
    setattr(empty_result, "validation_results", validation_results)
    setattr(empty_result, "computation_time", difftime(Sys.time(), start_time, units = "secs"))
    
    return(empty_result)
  }
  
  # Initialize results list
  results <- list()
  
  # Person-level analysis
  if (level %in% c("person", "both")) {
    
    # Calculate person-level statistics
    person_stats <- dt[, {
      
      # Basic period counts
      total_original <- .N
      employment_original <- sum(arco >= 1, na.rm = TRUE)
      unemployment_original <- sum(arco == 0, na.rm = TRUE)
      
      # Consolidated period counts (unique over_id values)
      consolidated_employment <- uniqueN(over_id[over_id > 0])
      consolidated_unemployment <- if(include_unemployment) uniqueN(over_id[over_id == 0]) else 0
      total_consolidated <- consolidated_employment + consolidated_unemployment
      
      # Employment complexity metrics
      employment_complexity_avg <- if(employment_original > 0) mean(arco[arco >= 1], na.rm = TRUE) else 0
      max_simultaneous_jobs <- if(employment_original > 0) max(arco[arco >= 1], na.rm = TRUE) else 0
      
      # Duration analysis
      total_employment_days_original <- sum(durata[arco >= 1], na.rm = TRUE)
      avg_employment_duration_original <- if(employment_original > 0) mean(durata[arco >= 1], na.rm = TRUE) else 0
      
      # Calculate consolidated employment duration (sum by over_id groups)
      if (consolidated_employment > 0) {
        consolidated_durations <- dt[cf == .BY$cf & over_id > 0, 
                                   .(consolidated_duration = sum(durata)), 
                                   by = over_id]
        total_employment_days_consolidated <- sum(consolidated_durations$consolidated_duration, na.rm = TRUE)
        avg_employment_duration_consolidated <- mean(consolidated_durations$consolidated_duration, na.rm = TRUE)
      } else {
        total_employment_days_consolidated <- 0
        avg_employment_duration_consolidated <- 0
      }
      
      # Consolidation ratios
      consolidation_ratio <- if(total_original > 0) 1 - (total_consolidated / total_original) else 0
      duration_efficiency <- if(total_employment_days_original > 0) total_employment_days_consolidated / total_employment_days_original else 1
      
      # Count overlapping episodes (over_id periods with multiple segments)
      if (consolidated_employment > 0) {
        overlapping_episodes <- dt[cf == .BY$cf & over_id > 0, .N, by = over_id][N > 1, .N]
        overlapping_episodes <- ifelse(length(overlapping_episodes) == 0, 0, overlapping_episodes)
      } else {
        overlapping_episodes <- 0
      }
      
      list(
        cf = .BY$cf,
        total_periods_original = total_original,
        total_periods_consolidated = total_consolidated,
        employment_periods_original = employment_original,
        employment_periods_consolidated = consolidated_employment,
        unemployment_periods = if(include_unemployment) consolidated_unemployment else unemployment_original,
        consolidation_ratio = round(consolidation_ratio, 4),
        employment_complexity_avg = round(employment_complexity_avg, 2),
        max_simultaneous_jobs = max_simultaneous_jobs,
        total_employment_days_original = total_employment_days_original,
        total_employment_days_consolidated = total_employment_days_consolidated,
        avg_employment_duration_original = round(avg_employment_duration_original, 1),
        avg_employment_duration_consolidated = round(avg_employment_duration_consolidated, 1),
        duration_efficiency = round(duration_efficiency, 4),
        overlapping_episodes = overlapping_episodes
      )
    }, by = cf]
    
    results$person_analysis <- person_stats
  }
  
  # Aggregate analysis (always computed for "both" level)
  if (level %in% c("aggregate", "both")) {
    
    # Population-level aggregate statistics
    aggregate_stats <- list(
      total_persons = uniqueN(dt$cf),
      total_segments_original = nrow(dt),
      total_employment_segments = sum(dt$arco >= 1, na.rm = TRUE),
      total_unemployment_segments = sum(dt$arco == 0, na.rm = TRUE),
      total_consolidated_employment_periods = uniqueN(dt[over_id > 0, .(cf, over_id)]),
      total_consolidated_unemployment_periods = if(include_unemployment) uniqueN(dt[over_id == 0, .(cf, over_id)]) else 0,
      avg_periods_per_person_original = round(nrow(dt) / uniqueN(dt$cf), 2),
      avg_employment_periods_per_person = round(sum(dt$arco >= 1, na.rm = TRUE) / uniqueN(dt$cf), 2),
      median_employment_duration = round(median(dt[arco >= 1, durata], na.rm = TRUE), 1),
      avg_employment_complexity = round(mean(dt[arco >= 1, arco], na.rm = TRUE), 2),
      max_employment_complexity = max(dt[arco >= 1, arco], na.rm = TRUE),
      pct_with_overlapping_employment = round(100 * uniqueN(dt[arco > 1, cf]) / uniqueN(dt$cf), 1)
    )
    
    # Calculate consolidation benefits
    if (nrow(dt) > 0 && any(dt$over_id > 0)) {
      # Calculate reduction statistics
      total_original <- nrow(dt)
      
      # Count unique consolidated periods (employment + unemployment if included)
      consolidated_employment <- uniqueN(dt[over_id > 0, .(cf, over_id)])
      consolidated_unemployment <- if(include_unemployment) sum(dt$arco == 0, na.rm = TRUE) else 0
      total_consolidated <- consolidated_employment + consolidated_unemployment
      
      aggregate_stats$total_consolidated_periods <- total_consolidated
      aggregate_stats$overall_consolidation_ratio <- round(1 - (total_consolidated / total_original), 4)
      aggregate_stats$consolidation_efficiency_pct <- round(100 * (1 - total_consolidated / total_original), 1)
    } else {
      aggregate_stats$total_consolidated_periods <- nrow(dt)
      aggregate_stats$overall_consolidation_ratio <- 0
      aggregate_stats$consolidation_efficiency_pct <- 0
    }
    
    results$aggregate_analysis <- data.table(
      metric = names(aggregate_stats),
      value = as.character(unlist(aggregate_stats))
    )
  }
  
  # Detailed analyses if requested
  if (include_details) {
    
    # Consolidation benefits analysis
    consolidation_benefits <- dt[over_id > 0, {
      segments_in_period <- .N
      total_duration <- sum(durata, na.rm = TRUE)
      avg_arco <- mean(arco, na.rm = TRUE)
      max_arco <- max(arco, na.rm = TRUE)
      complexity_reduction <- segments_in_period - 1  # How many segments were consolidated
      
      list(
        cf = cf[1],
        over_id = over_id[1],
        original_segments = segments_in_period,
        consolidated_segments = 1,
        complexity_reduction = complexity_reduction,
        total_duration = total_duration,
        avg_employment_intensity = round(avg_arco, 2),
        max_employment_intensity = max_arco,
        administrative_efficiency = if(segments_in_period > 1) "High" else "Standard"
      )
    }, by = .(cf, over_id)]
    
    results$consolidation_benefits <- consolidation_benefits
    
    # Employment complexity patterns
    employment_complexity <- dt[arco >= 1, {
      list(
        employment_intensity = arco[1],
        segments_count = .N,
        total_person_days = sum(durata, na.rm = TRUE),
        avg_duration = round(mean(durata, na.rm = TRUE), 1),
        persons_affected = uniqueN(cf)
      )
    }, by = arco]
    
    setorder(employment_complexity, arco)
    results$employment_complexity <- employment_complexity
  }
  
  # Add analysis metadata as attributes
  analysis_params <- list(
    level = level,
    include_unemployment = include_unemployment,
    consolidation_type = consolidation_type,
    min_employment_duration = min_employment_duration,
    include_details = include_details,
    analysis_date = Sys.Date()
  )
  
  # Calculate computation time
  end_time <- Sys.time()
  computation_time <- difftime(end_time, start_time, units = "secs")
  
  setattr(results, "analysis_parameters", analysis_params)
  setattr(results, "validation_results", validation_results)
  setattr(results, "computation_time", computation_time)
  
  # Return appropriate subset based on level
  if (level == "person") {
    return(list(
      person_analysis = results$person_analysis,
      consolidation_benefits = if(include_details) results$consolidation_benefits else NULL,
      employment_complexity = if(include_details) results$employment_complexity else NULL
    ))
  } else if (level == "aggregate") {
    return(list(
      aggregate_analysis = results$aggregate_analysis,
      consolidation_benefits = if(include_details) results$consolidation_benefits else NULL,
      employment_complexity = if(include_details) results$employment_complexity else NULL
    ))
  } else {
    # Return all results for "both"
    return(results)
  }
}


#' Helper Function: Calculate Consolidation Impact Metrics
#'
#' @description
#' Internal helper function to calculate detailed consolidation impact metrics
#' comparing original vecshift segments to consolidated periods using over_id.
#'
#' @param dt A data.table with vecshift output containing over_id column
#' @param person_id Character string specifying person identifier for focused analysis
#'
#' @return A list containing detailed consolidation impact metrics
#'
#' @keywords internal
#' @noRd
.calculate_consolidation_impact <- function(dt, person_id = NULL) {
  
  if (!is.null(person_id)) {
    dt <- dt[cf == person_id]
  }
  
  if (nrow(dt) == 0) {
    return(list(
      original_segments = 0,
      consolidated_periods = 0,
      impact_score = 0,
      complexity_reduction = 0
    ))
  }
  
  # Original segments count
  original_segments <- nrow(dt)
  
  # Consolidated periods (unique over_id values)
  consolidated_periods <- uniqueN(dt$over_id)
  
  # Impact metrics
  consolidation_ratio <- 1 - (consolidated_periods / original_segments)
  
  # Complexity reduction (segments that were merged)
  complexity_reduction <- original_segments - consolidated_periods
  
  # Impact score (weighted by duration and employment complexity)
  if (any(dt$arco >= 1)) {
    avg_employment_complexity <- mean(dt[arco >= 1, arco], na.rm = TRUE)
    total_employment_days <- sum(dt[arco >= 1, durata], na.rm = TRUE)
    impact_score <- consolidation_ratio * (1 + log10(1 + avg_employment_complexity)) * log10(1 + total_employment_days)
  } else {
    impact_score <- consolidation_ratio
  }
  
  return(list(
    original_segments = original_segments,
    consolidated_periods = consolidated_periods,
    consolidation_ratio = round(consolidation_ratio, 4),
    complexity_reduction = complexity_reduction,
    impact_score = round(impact_score, 4)
  ))
}


#' Helper Function: Validate over_id Consistency for Analysis
#'
#' @description
#' Internal helper function to validate over_id consistency and data quality
#' for consolidated periods analysis.
#'
#' @param dt A data.table with vecshift output containing over_id column
#'
#' @return A list containing validation results and quality metrics
#'
#' @keywords internal
#' @noRd
.validate_over_id_for_analysis <- function(dt) {
  
  validation <- list(
    has_over_id = "over_id" %in% names(dt),
    over_id_completeness = if("over_id" %in% names(dt)) sum(!is.na(dt$over_id)) / nrow(dt) else 0,
    unemployment_properly_coded = if("over_id" %in% names(dt)) all(dt[arco == 0, over_id] == 0, na.rm = TRUE) else FALSE,
    employment_properly_coded = if("over_id" %in% names(dt)) all(dt[arco >= 1, over_id] > 0, na.rm = TRUE) else FALSE,
    over_id_range = if("over_id" %in% names(dt)) range(dt$over_id, na.rm = TRUE) else c(NA, NA),
    persons_with_overlapping = if("over_id" %in% names(dt)) uniqueN(dt[arco > 1, cf]) else 0,
    total_over_id_groups = if("over_id" %in% names(dt)) uniqueN(dt$over_id) else 0
  )
  
  # Quality assessment
  validation$quality_score <- mean(c(
    validation$over_id_completeness,
    as.numeric(validation$unemployment_properly_coded),
    as.numeric(validation$employment_properly_coded)
  ), na.rm = TRUE)
  
  validation$quality_grade <- if(validation$quality_score >= 0.95) "Excellent" else
                            if(validation$quality_score >= 0.85) "Good" else  
                            if(validation$quality_score >= 0.70) "Fair" else "Poor"
  
  return(validation)
}


#' Create Consolidated Transition Matrix Using over_id
#'
#' @description
#' Creates transition matrices using consolidated employment periods leveraging the over_id
#' functionality from vecshift() output. This function provides cleaner transition matrices
#' by consolidating overlapping and/or consecutive employment periods before analyzing
#' transitions, reducing administrative noise and providing more accurate career movement patterns.
#'
#' @details
#' This function leverages the over_id column to create consolidated transition matrices:
#' \itemize{
#'   \item{\strong{over_id = 0}}: Unemployment periods (no active contracts)
#'   \item{\strong{over_id > 0}}: Employment periods with same value for overlapping/continuous contracts
#' }
#' 
#' Key benefits of consolidated matrices:
#' \itemize{
#'   \item{\strong{Reduced Noise}}: Eliminates transitions between administrative contract splits
#'   \item{\strong{True Career Moves}}: Focuses on genuine employment transitions between different states
#'   \item{\strong{Cleaner Patterns}}: Consolidates overlapping contracts into single employment episodes
#'   \item{\strong{Better Analysis}}: More accurate transition probabilities and frequencies
#'   \item{\strong{Matrix Comparison}}: Quantifies improvement over raw transition matrices
#' }
#' 
#' The function supports multiple consolidation types:
#' \itemize{
#'   \item{\code{"both"}}: Consolidate overlapping periods first, then merge consecutive periods
#'   \item{\code{"overlapping"}}: Only consolidate segments with same over_id > 0
#'   \item{\code{"consecutive"}}: Only merge periods that are contiguous in time
#'   \item{\code{"none"}}: No consolidation (equivalent to raw transition matrix)
#' }
#' 
#' Matrix types and normalization:
#' \itemize{
#'   \item{\strong{Frequency}}: Raw transition counts (default)
#'   \item{\strong{Probability}}: Normalized transition probabilities
#'   \item{\strong{Row normalization}}: Each row sums to 1 (conditional probability given "from" state)
#'   \item{\strong{Column normalization}}: Each column sums to 1 (reverse conditional probability)
#'   \item{\strong{Total normalization}}: Entire matrix sums to 1 (joint probability)
#' }
#'
#' @param pipeline_result Output from process_employment_pipeline() or vecshift(). Must be a 
#'   data.table with columns: cf (person identifier), arco (employment overlap count), 
#'   inizio/fine (period dates), durata (period duration), over_id (overlap identifier), 
#'   and the specified transition_variable.
#' @param transition_variable Character string specifying the variable to create transitions 
#'   for (e.g., "employment_type", "company", "salary_level"). This variable defines the 
#'   states in the transition matrix.
#' @param consolidation_type Character string specifying consolidation approach (default: "both"):
#'   \itemize{
#'     \item{\code{"both"}}: Complete consolidation - overlapping then consecutive periods
#'     \item{\code{"overlapping"}}: Only consolidate segments with same over_id > 0
#'     \item{\code{"consecutive"}}: Only merge contiguous periods regardless of over_id  
#'     \item{\code{"none"}}: No consolidation, equivalent to raw matrix
#'   }
#' @param matrix_type Character vector specifying output matrix types (default: c("frequency", "probability")):
#'   \itemize{
#'     \item{\code{"frequency"}}: Raw transition counts
#'     \item{\code{"probability"}}: Normalized transition probabilities  
#'     \item{\code{"both"}}: Both frequency and probability matrices
#'   }
#' @param include_comparison Logical. If TRUE (default), includes comparison between raw and 
#'   consolidated matrices with improvement metrics.
#' @param normalize_by Character string specifying probability normalization method when 
#'   matrix_type includes "probability" (default: "row"):
#'   \itemize{
#'     \item{\code{"row"}}: Row normalization - P(to|from), each row sums to 1
#'     \item{\code{"column"}}: Column normalization - P(from|to), each column sums to 1  
#'     \item{\code{"total"}}: Total normalization - P(from,to), entire matrix sums to 1
#'   }
#' @param min_unemployment_duration Minimum duration (days) of unemployment period to consider 
#'   a transition (default: 1).
#' @param max_unemployment_duration Maximum duration (days) of unemployment period to consider 
#'   a transition. If NULL (default), no upper limit is applied.
#' @param show_progress Logical. If TRUE (default), displays progress messages.
#'
#' @return A list containing:
#'   \itemize{
#'     \item{\code{consolidated_matrix}}: Primary consolidated transition matrix (frequency or probability)
#'     \item{\code{consolidated_frequency_matrix}}: Consolidated frequency matrix (if "both" requested)
#'     \item{\code{consolidated_probability_matrix}}: Consolidated probability matrix (if "both" requested)
#'     \item{\code{raw_matrix}}: Raw transition matrix for comparison (if include_comparison = TRUE)
#'     \item{\code{matrix_comparison}}: data.table with comparison metrics (if include_comparison = TRUE):
#'       \itemize{
#'         \item{\code{metric}}: Comparison metric name
#'         \item{\code{raw_value}}: Value for raw matrix
#'         \item{\code{consolidated_value}}: Value for consolidated matrix
#'         \item{\code{improvement}}: Improvement measure (lower sparsity, complexity reduction, etc.)
#'       }
#'     \item{\code{consolidation_impact}}: Summary of consolidation benefits
#'     \item{\code{matrix_statistics}}: Detailed matrix statistics and properties
#'   }
#'   
#'   All results include attributes:
#'   \itemize{
#'     \item{\code{analysis_parameters}}: Parameters used for matrix creation
#'     \item{\code{consolidation_summary}}: Summary of consolidation applied
#'     \item{\code{matrix_properties}}: Mathematical properties of resulting matrices
#'   }
#'
#' @export
#' @importFrom data.table data.table copy setorder uniqueN := .N
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with overlapping contracts
#' employment_data <- data.table(
#'   id = 1:10,
#'   cf = c(rep("PERSON001", 6), rep("PERSON002", 4)),
#'   INIZIO = as.Date(c("2023-01-01", "2023-02-15", "2023-03-01", "2023-05-01", 
#'                      "2023-08-01", "2023-10-01", "2023-01-15", "2023-04-01", 
#'                      "2023-07-01", "2023-09-15")),
#'   FINE = as.Date(c("2023-02-28", "2023-04-30", "2023-04-15", "2023-06-30",
#'                    "2023-09-30", "2023-12-31", "2023-03-31", "2023-06-15",
#'                    "2023-08-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1),
#'   company = c("CompanyA", "CompanyB", "CompanyA", "CompanyC", "CompanyD", "CompanyE",
#'               "CompanyF", "CompanyG", "CompanyH", "CompanyI"),
#'   employment_type = c("FT", "PT", "FT", "FT", "PT", "FT", "FT", "PT", "FT", "FT")
#' )
#' 
#' # Process through vecshift to get over_id
#' result <- vecshift(employment_data)
#' 
#' # Create consolidated transition matrix for companies with full comparison
#' consolidated_result <- create_consolidated_transition_matrix(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   consolidation_type = "both",
#'   matrix_type = "both",
#'   include_comparison = TRUE,
#'   normalize_by = "row"
#' )
#' 
#' # View consolidated frequency matrix
#' print(consolidated_result$consolidated_frequency_matrix)
#' 
#' # View consolidated probability matrix  
#' print(consolidated_result$consolidated_probability_matrix)
#' 
#' # Compare improvement over raw matrix
#' print(consolidated_result$matrix_comparison)
#' 
#' # View consolidation impact
#' print(consolidated_result$consolidation_impact)
#' 
#' # Create employment type transitions with overlapping consolidation only
#' employment_transitions <- create_consolidated_transition_matrix(
#'   pipeline_result = result,
#'   transition_variable = "employment_type", 
#'   consolidation_type = "overlapping",
#'   matrix_type = "probability",
#'   normalize_by = "row",
#'   min_unemployment_duration = 7,
#'   include_comparison = FALSE
#' )
#' print(employment_transitions$consolidated_matrix)
#' 
#' # Compare different consolidation strategies
#' no_consolidation <- create_consolidated_transition_matrix(
#'   result, "company", "none", "frequency", FALSE
#' )
#' overlapping_only <- create_consolidated_transition_matrix(
#'   result, "company", "overlapping", "frequency", FALSE  
#' )
#' full_consolidation <- create_consolidated_transition_matrix(
#'   result, "company", "both", "frequency", FALSE
#' )
#' 
#' # Output demonstrates:
#' # - Raw matrix: May show A->A transitions due to overlapping contracts
#' # - Consolidated: Shows true A->B career transitions
#' # - Reduced sparsity and cleaner transition patterns
#' # - Quantified improvement metrics
#' }
create_consolidated_transition_matrix <- function(pipeline_result,
                                                transition_variable,
                                                consolidation_type = "both",
                                                matrix_type = c("frequency", "probability"),
                                                include_comparison = TRUE,
                                                normalize_by = "row",
                                                min_unemployment_duration = 1,
                                                max_unemployment_duration = NULL,
                                                show_progress = TRUE) {
  
  # Record start time
  start_time <- Sys.time()
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Input validation
  if (!inherits(pipeline_result, "data.table")) {
    stop("Parameter 'pipeline_result' must be a data.table object from vecshift() or process_employment_pipeline().")
  }
  
  # Check for required columns
  required_cols <- c("cf", "arco", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(pipeline_result))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in pipeline_result:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for over_id column (critical for this function)
  if (!"over_id" %in% names(pipeline_result)) {
    stop("Parameter 'pipeline_result' must contain 'over_id' column from vecshift() output. ",
         "This function specifically creates consolidated matrices using over_id.")
  }
  
  # Validate transition_variable
  if (!is.character(transition_variable) || length(transition_variable) != 1) {
    stop("Parameter 'transition_variable' must be a single character string.")
  }
  
  if (!transition_variable %in% names(pipeline_result)) {
    stop(sprintf("Transition variable '%s' not found in pipeline_result.", transition_variable))
  }
  
  # Validate consolidation_type
  valid_consolidation_types <- c("both", "overlapping", "consecutive", "none")
  if (!consolidation_type %in% valid_consolidation_types) {
    stop(paste("Parameter 'consolidation_type' must be one of:", 
               paste(valid_consolidation_types, collapse = ", ")))
  }
  
  # Validate matrix_type
  matrix_type <- match.arg(matrix_type, c("frequency", "probability", "both"), several.ok = TRUE)
  if ("both" %in% matrix_type) {
    matrix_type <- c("frequency", "probability")
  }
  
  # Validate normalize_by
  normalize_by <- match.arg(normalize_by, c("row", "column", "total"))
  
  # Validate numeric parameters
  if (!is.numeric(min_unemployment_duration) || min_unemployment_duration < 0) {
    stop("Parameter 'min_unemployment_duration' must be a non-negative numeric value.")
  }
  
  if (!is.null(max_unemployment_duration)) {
    if (!is.numeric(max_unemployment_duration) || max_unemployment_duration < 0) {
      stop("Parameter 'max_unemployment_duration' must be NULL or a non-negative numeric value.")
    }
    if (max_unemployment_duration < min_unemployment_duration) {
      stop("Parameter 'max_unemployment_duration' must be greater than or equal to 'min_unemployment_duration'.")
    }
  }
  
  # Validate logical parameters
  if (!is.logical(include_comparison) || !is.logical(show_progress)) {
    stop("Parameters 'include_comparison' and 'show_progress' must be logical values.")
  }
  
  if (show_progress) {
    message(sprintf("Creating consolidated transition matrix for '%s' using '%s' consolidation...", 
                   transition_variable, consolidation_type))
  }
  
  # Initialize results list
  results <- list()
  
  # Store analysis parameters
  analysis_params <- list(
    transition_variable = transition_variable,
    consolidation_type = consolidation_type,
    matrix_type = matrix_type,
    normalize_by = normalize_by,
    min_unemployment_duration = min_unemployment_duration,
    max_unemployment_duration = max_unemployment_duration,
    include_comparison = include_comparison,
    analysis_timestamp = Sys.time()
  )
  
  # Create consolidated matrix
  if (show_progress) {
    message("Applying consolidation and creating transition matrix...")
  }
  
  consolidated_transitions <- analyze_employment_transitions(
    pipeline_result = pipeline_result,
    transition_variable = transition_variable,
    statistics_variables = NULL,  # Focus only on transitions
    min_unemployment_duration = min_unemployment_duration,
    max_unemployment_duration = max_unemployment_duration,
    use_consolidated_periods = ifelse(consolidation_type == "none", FALSE, TRUE),
    consolidation_type = consolidation_type,
    output_transition_matrix = TRUE,
    show_progress = FALSE  # Suppress detailed progress from analyze_employment_transitions
  )
  
  # Handle case where no transitions are found
  if (is.null(consolidated_transitions) || 
      (is.matrix(consolidated_transitions) && nrow(consolidated_transitions) == 0)) {
    if (show_progress) {
      message("No transitions found for matrix creation.")
    }
    
    # Return empty results structure
    empty_matrix <- matrix(0, nrow = 0, ncol = 0, 
                          dimnames = list(character(0), character(0)))
    
    results$consolidated_matrix <- empty_matrix
    results$matrix_statistics <- data.table(
      metric = character(0),
      value = numeric(0)
    )
    
    # Add attributes and return
    setattr(results, "analysis_parameters", analysis_params)
    setattr(results, "computation_time", difftime(Sys.time(), start_time, units = "secs"))
    
    return(results)
  }
  
  # Store consolidated frequency matrix
  consolidated_freq_matrix <- consolidated_transitions
  
  # Create probability matrix if requested
  if ("probability" %in% matrix_type) {
    consolidated_prob_matrix <- .normalize_transition_matrix(consolidated_freq_matrix, normalize_by)
    
    if (length(matrix_type) == 1 && matrix_type == "probability") {
      results$consolidated_matrix <- consolidated_prob_matrix
    } else {
      results$consolidated_frequency_matrix <- consolidated_freq_matrix
      results$consolidated_probability_matrix <- consolidated_prob_matrix
      results$consolidated_matrix <- consolidated_freq_matrix  # Default to frequency
    }
  } else {
    results$consolidated_matrix <- consolidated_freq_matrix
  }
  
  if (length(matrix_type) > 1) {
    results$consolidated_frequency_matrix <- consolidated_freq_matrix
    if ("probability" %in% matrix_type) {
      results$consolidated_probability_matrix <- .normalize_transition_matrix(consolidated_freq_matrix, normalize_by)
    }
  }
  
  # Calculate matrix statistics
  matrix_stats <- .calculate_matrix_statistics(consolidated_freq_matrix, "consolidated")
  results$matrix_statistics <- matrix_stats
  
  # Create comparison with raw matrix if requested
  if (include_comparison && consolidation_type != "none") {
    if (show_progress) {
      message("Creating raw transition matrix for comparison...")
    }
    
    # Create raw (non-consolidated) matrix
    raw_transitions <- analyze_employment_transitions(
      pipeline_result = pipeline_result,
      transition_variable = transition_variable,
      statistics_variables = NULL,
      min_unemployment_duration = min_unemployment_duration,
      max_unemployment_duration = max_unemployment_duration,
      use_consolidated_periods = FALSE,  # No consolidation for raw matrix
      output_transition_matrix = TRUE,
      show_progress = FALSE
    )
    
    if (!is.null(raw_transitions) && nrow(raw_transitions) > 0) {
      results$raw_matrix <- raw_transitions
      
      # Calculate comparison metrics
      raw_stats <- .calculate_matrix_statistics(raw_transitions, "raw")
      
      # Create comprehensive comparison
      comparison <- .compare_transition_matrices(raw_transitions, consolidated_freq_matrix, 
                                               raw_stats, matrix_stats)
      results$matrix_comparison <- comparison
      
      # Calculate consolidation impact
      consolidation_impact <- .calculate_consolidation_impact_matrix(raw_transitions, consolidated_freq_matrix)
      results$consolidation_impact <- consolidation_impact
    }
  }
  
  # Add summary information
  if (show_progress) {
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    n_states <- nrow(consolidated_freq_matrix)
    total_transitions <- sum(consolidated_freq_matrix, na.rm = TRUE)
    sparsity <- round(100 * sum(consolidated_freq_matrix == 0) / length(consolidated_freq_matrix), 1)
    
    message(sprintf("Matrix creation completed in %.2f seconds. ", as.numeric(elapsed_time)),
            sprintf("%d states, %d total transitions, %.1f%% sparsity", 
                   n_states, total_transitions, sparsity))
  }
  
  # Add metadata attributes
  setattr(results, "analysis_parameters", analysis_params)
  setattr(results, "consolidation_summary", list(
    consolidation_type = consolidation_type,
    transition_variable = transition_variable,
    matrix_dimensions = if(!is.null(consolidated_freq_matrix)) dim(consolidated_freq_matrix) else c(0, 0),
    total_transitions = if(!is.null(consolidated_freq_matrix)) sum(consolidated_freq_matrix, na.rm = TRUE) else 0
  ))
  setattr(results, "computation_time", difftime(Sys.time(), start_time, units = "secs"))
  
  return(results)
}


#' Helper Function: Normalize Transition Matrix
#'
#' @description
#' Internal helper function to normalize transition matrices to create probability matrices.
#'
#' @param matrix A numeric transition matrix
#' @param normalize_by Character string specifying normalization method
#'
#' @return Normalized probability matrix
#'
#' @keywords internal
#' @noRd
.normalize_transition_matrix <- function(matrix, normalize_by = "row") {
  
  if (nrow(matrix) == 0 || ncol(matrix) == 0) {
    return(matrix)
  }
  
  normalized_matrix <- matrix
  
  if (normalize_by == "row") {
    # Row normalization: P(to|from) - each row sums to 1
    row_sums <- rowSums(matrix, na.rm = TRUE)
    for (i in 1:nrow(matrix)) {
      if (row_sums[i] > 0) {
        normalized_matrix[i, ] <- matrix[i, ] / row_sums[i]
      }
    }
  } else if (normalize_by == "column") {
    # Column normalization: P(from|to) - each column sums to 1
    col_sums <- colSums(matrix, na.rm = TRUE)
    for (j in 1:ncol(matrix)) {
      if (col_sums[j] > 0) {
        normalized_matrix[, j] <- matrix[, j] / col_sums[j]
      }
    }
  } else if (normalize_by == "total") {
    # Total normalization: P(from,to) - entire matrix sums to 1
    total_sum <- sum(matrix, na.rm = TRUE)
    if (total_sum > 0) {
      normalized_matrix <- matrix / total_sum
    }
  }
  
  # Handle NAs and ensure proper matrix structure
  normalized_matrix[is.na(normalized_matrix)] <- 0
  
  return(normalized_matrix)
}


#' Helper Function: Calculate Matrix Statistics
#'
#' @description
#' Internal helper function to calculate comprehensive statistics for transition matrices.
#'
#' @param matrix A transition matrix
#' @param matrix_name Character string name for the matrix type
#'
#' @return data.table with matrix statistics
#'
#' @keywords internal
#' @noRd
.calculate_matrix_statistics <- function(matrix, matrix_name) {
  
  if (nrow(matrix) == 0 || ncol(matrix) == 0) {
    return(data.table(
      matrix_type = matrix_name,
      metric = character(0),
      value = numeric(0)
    ))
  }
  
  # Basic matrix properties
  n_states <- nrow(matrix)
  total_transitions <- sum(matrix, na.rm = TRUE)
  n_nonzero <- sum(matrix > 0, na.rm = TRUE)
  n_possible <- n_states * n_states
  sparsity <- 1 - (n_nonzero / n_possible)
  
  # Diagonal elements (self-transitions)
  n_self_transitions <- sum(diag(matrix), na.rm = TRUE)
  pct_self_transitions <- if (total_transitions > 0) n_self_transitions / total_transitions else 0
  
  # State activity
  row_sums <- rowSums(matrix, na.rm = TRUE)
  col_sums <- colSums(matrix, na.rm = TRUE)
  
  active_from_states <- sum(row_sums > 0)
  active_to_states <- sum(col_sums > 0)
  
  # Transition distribution
  nonzero_values <- matrix[matrix > 0]
  if (length(nonzero_values) > 0) {
    mean_transition_weight <- mean(nonzero_values)
    median_transition_weight <- median(nonzero_values)
    max_transition_weight <- max(nonzero_values)
  } else {
    mean_transition_weight <- 0
    median_transition_weight <- 0
    max_transition_weight <- 0
  }
  
  # Create statistics data.table
  statistics <- data.table(
    matrix_type = matrix_name,
    metric = c(
      "n_states", "total_transitions", "n_nonzero_cells", "n_possible_cells",
      "sparsity", "density", "n_self_transitions", "pct_self_transitions",
      "active_from_states", "active_to_states", "mean_transition_weight",
      "median_transition_weight", "max_transition_weight"
    ),
    value = c(
      n_states, total_transitions, n_nonzero, n_possible,
      round(sparsity, 4), round(1 - sparsity, 4), n_self_transitions, round(pct_self_transitions, 4),
      active_from_states, active_to_states, round(mean_transition_weight, 2),
      round(median_transition_weight, 2), max_transition_weight
    )
  )
  
  return(statistics)
}


#' Helper Function: Compare Transition Matrices
#'
#' @description
#' Internal helper function to compare raw and consolidated transition matrices.
#'
#' @param raw_matrix Raw transition matrix
#' @param consolidated_matrix Consolidated transition matrix  
#' @param raw_stats Statistics for raw matrix
#' @param consolidated_stats Statistics for consolidated matrix
#'
#' @return data.table with comparison metrics
#'
#' @keywords internal
#' @noRd
.compare_transition_matrices <- function(raw_matrix, consolidated_matrix, raw_stats, consolidated_stats) {
  
  # Extract key metrics for comparison
  extract_metric <- function(stats_dt, metric_name) {
    value <- stats_dt[metric == metric_name, value]
    if (length(value) == 0) return(0)
    return(value)
  }
  
  # Key metrics to compare
  metrics <- c("n_states", "total_transitions", "sparsity", "n_self_transitions", 
               "pct_self_transitions", "density", "n_nonzero_cells")
  
  comparison_data <- data.table(
    metric = metrics,
    raw_value = sapply(metrics, function(m) extract_metric(raw_stats, m)),
    consolidated_value = sapply(metrics, function(m) extract_metric(consolidated_stats, m))
  )
  
  # Calculate improvements (positive values indicate improvement)
  comparison_data[, improvement := ifelse(
    metric == "sparsity", raw_value - consolidated_value,  # Lower sparsity is better
    ifelse(metric == "n_self_transitions", raw_value - consolidated_value,  # Fewer self-transitions is better
    ifelse(metric == "pct_self_transitions", raw_value - consolidated_value,  # Lower % self-transitions is better
    ifelse(metric == "density", consolidated_value - raw_value,  # Higher density is better
    ifelse(metric %in% c("n_states", "total_transitions", "n_nonzero_cells"), raw_value - consolidated_value,  # Simplification is better
    0)))))]
  
  # Add improvement interpretation
  comparison_data[, improvement_pct := round(100 * improvement / pmax(raw_value, 1e-10), 1)]
  
  comparison_data[, interpretation := ifelse(
    metric == "sparsity" & improvement > 0, paste0("Reduced sparsity by ", round(improvement, 4)),
    ifelse(metric == "n_self_transitions" & improvement > 0, paste0("Eliminated ", improvement, " administrative self-transitions"),
    ifelse(metric == "pct_self_transitions" & improvement > 0, paste0("Reduced self-transition rate by ", round(100 * improvement, 1), "%"),
    ifelse(metric == "density" & improvement > 0, paste0("Increased matrix density by ", round(improvement, 4)),
    ifelse(metric == "n_states" & improvement != 0, ifelse(improvement > 0, 
                                                    paste0("Simplified by ", improvement, " states"),
                                                    paste0("Added ", abs(improvement), " states")),
    ifelse(improvement == 0, "No change", "Other change"))))))]
  
  return(comparison_data)
}


#' Helper Function: Calculate Consolidation Impact for Matrices
#'
#' @description
#' Internal helper function to calculate the impact of consolidation on matrix structure.
#'
#' @param raw_matrix Raw transition matrix
#' @param consolidated_matrix Consolidated transition matrix
#'
#' @return Summary of consolidation impact
#'
#' @keywords internal
#' @noRd
.calculate_consolidation_impact_matrix <- function(raw_matrix, consolidated_matrix) {
  
  # Calculate key impact metrics
  raw_transitions <- sum(raw_matrix, na.rm = TRUE)
  consolidated_transitions <- sum(consolidated_matrix, na.rm = TRUE)
  
  raw_self_transitions <- sum(diag(raw_matrix), na.rm = TRUE) 
  consolidated_self_transitions <- sum(diag(consolidated_matrix), na.rm = TRUE)
  
  administrative_noise_removed <- raw_self_transitions - consolidated_self_transitions
  transition_reduction_pct <- if (raw_transitions > 0) {
    round(100 * (raw_transitions - consolidated_transitions) / raw_transitions, 1)
  } else 0
  
  self_transition_reduction_pct <- if (raw_self_transitions > 0) {
    round(100 * administrative_noise_removed / raw_self_transitions, 1)
  } else 0
  
  # Matrix complexity measures
  raw_sparsity <- sum(raw_matrix == 0) / length(raw_matrix)
  consolidated_sparsity <- sum(consolidated_matrix == 0) / length(consolidated_matrix)
  sparsity_improvement <- raw_sparsity - consolidated_sparsity
  
  impact_summary <- data.table(
    impact_metric = c(
      "transitions_consolidated", "administrative_noise_removed", 
      "transition_reduction_pct", "self_transition_reduction_pct",
      "sparsity_improvement", "matrix_efficiency_gain"
    ),
    value = c(
      raw_transitions - consolidated_transitions,
      administrative_noise_removed,
      transition_reduction_pct,
      self_transition_reduction_pct,
      round(sparsity_improvement, 4),
      round(administrative_noise_removed / pmax(raw_transitions, 1), 4)
    ),
    interpretation = c(
      paste(raw_transitions - consolidated_transitions, "transitions consolidated into cleaner patterns"),
      paste(administrative_noise_removed, "administrative self-transitions eliminated"),
      paste0(transition_reduction_pct, "% reduction in total transition volume"),
      paste0(self_transition_reduction_pct, "% reduction in self-transition noise"),
      ifelse(sparsity_improvement < 0, 
             paste("Matrix density improved by", round(abs(sparsity_improvement), 4)),
             paste("Matrix sparsity reduced by", round(sparsity_improvement, 4))),
      paste0(round(100 * administrative_noise_removed / pmax(raw_transitions, 1), 1), "% efficiency gain from noise reduction")
    )
  )
  
  return(impact_summary)
}


#' Analyze Employment Overlaps Using over_id
#'
#' @description
#' Provides comprehensive analysis of overlapping employment periods using the over_id 
#' functionality from vecshift() output. This function specifically focuses on periods with 
#' multiple simultaneous employment contracts (arco > 1) to understand patterns of 
#' multiple job holding, employment intensity, and temporal overlap characteristics.
#'
#' @details
#' The over_id column from vecshift() identifies continuous overlapping employment periods:
#' \itemize{
#'   \item \strong{over_id = 0}: Unemployment periods (no active contracts)
#'   \item \strong{over_id > 0}: Employment periods with same value for overlapping/continuous contracts
#'   \item \strong{Same over_id}: All contracts belonging to same continuous overlapping time period
#' }
#' 
#' This function analyzes employment overlap patterns including:
#' \itemize{
#'   \item \strong{Overlap Episodes}: Periods where arco > 1 (multiple concurrent jobs)
#'   \item \strong{Employment Intensity}: How many jobs are held simultaneously
#'   \item \strong{Duration Patterns}: How long overlapping employment lasts
#'   \item \strong{Temporal Analysis}: When overlaps occur most frequently
#'   \item \strong{Person-level Patterns}: Individual multiple job holding behaviors
#'   \item \strong{Overlap Efficiency}: Time spent in multiple vs single employment
#' }
#' 
#' Key metrics calculated:
#' \itemize{
#'   \item \strong{Total overlapping episodes}: Count of periods with arco > 1
#'   \item \strong{Average/maximum concurrent jobs}: Employment intensity measures
#'   \item \strong{Overlap duration distribution}: How long multiple employment lasts
#'   \item \strong{Overlap intensity percentage}: Proportion of employment time with multiple jobs
#'   \item \strong{Peak employment complexity}: Maximum simultaneous jobs per person
#'   \item \strong{Temporal patterns}: Monthly/seasonal overlap frequency (optional)
#' }
#'
#' @param segments Input data from vecshift() with over_id column. Must be a data.table
#'   with required columns: cf, inizio, fine, arco, durata, over_id, and optionally
#'   additional employment attributes for detailed analysis.
#' @param analysis_level Character string specifying analysis granularity (default: "person"):
#'   \itemize{
#'     \item{\code{"person"}}: Person-level analysis with individual overlap statistics
#'     \item{\code{"aggregate"}}: Population-level aggregate overlap patterns
#'     \item{\code{"detailed"}}: Both person and aggregate analysis with comprehensive breakdowns
#'   }
#' @param min_overlap_duration Minimum duration (days) to consider as meaningful overlap
#'   (default: 1). Helps filter out very short-term overlapping contracts.
#' @param include_temporal_patterns Logical. If TRUE (default), analyzes when overlaps occur
#'   most frequently (monthly/seasonal patterns). Requires date columns.
#' @param group_by Optional character string specifying grouping variable (e.g., "industry", 
#'   "region") for comparative overlap analysis across different categories. Must be a column 
#'   name in segments (default: NULL).
#' @param temporal_aggregation Character string specifying temporal aggregation for pattern
#'   analysis when include_temporal_patterns = TRUE (default: "month"):
#'   \itemize{
#'     \item{\code{"month"}}: Monthly overlap frequency patterns
#'     \item{\code{"quarter"}}: Quarterly overlap frequency patterns  
#'     \item{\code{"year"}}: Annual overlap frequency patterns
#'   }
#' @param include_unemployment_context Logical. If TRUE (default), includes context about
#'   unemployment periods for comparison with overlapping employment patterns.
#'
#' @return When \code{analysis_level} is "person", returns a list containing:
#'   \itemize{
#'     \item{\code{person_overlaps}}: data.table with person-level overlap statistics:
#'       \itemize{
#'         \item{\code{cf}}: Person identifier
#'         \item{\code{total_employment_periods}}: Total employment segments (arco >= 1)
#'         \item{\code{overlapping_episodes}}: Count of periods with arco > 1
#'         \item{\code{single_job_periods}}: Count of periods with arco = 1
#'         \item{\code{overlap_frequency}}: Proportion of employment periods with overlaps
#'         \item{\code{avg_concurrent_jobs}}: Mean arco for employment periods
#'         \item{\code{max_concurrent_jobs}}: Maximum simultaneous jobs (peak employment complexity)
#'         \item{\code{total_overlap_days}}: Sum of days spent in overlapping employment
#'         \item{\code{total_employment_days}}: Sum of all employment days
#'         \item{\code{overlap_intensity_pct}}: Percentage of employment time with multiple jobs
#'         \item{\code{avg_overlap_duration}}: Mean duration of overlapping episodes
#'         \item{\code{max_overlap_duration}}: Longest overlapping episode duration
#'         \item{\code{unique_over_id_groups}}: Count of distinct overlapping employment episodes
#'         \item{\code{employment_complexity_score}}: Weighted complexity measure
#'       }
#'   }
#'   
#'   When \code{analysis_level} is "aggregate", returns aggregate-level analysis.
#'   
#'   When \code{analysis_level} is "detailed", returns comprehensive analysis including:
#'   \itemize{
#'     \item{\code{person_overlaps}}: Person-level statistics
#'     \item{\code{aggregate_overlaps}}: Population-level aggregate patterns
#'     \item{\code{overlap_duration_distribution}}: Distribution of overlap durations
#'     \item{\code{employment_intensity_distribution}}: Distribution of concurrent job counts
#'     \item{\code{temporal_patterns}}: Temporal overlap frequency (if include_temporal_patterns = TRUE)
#'     \item{\code{group_comparisons}}: Comparative analysis by group (if group_by specified)
#'   }
#'   
#'   All results include attributes:
#'   \itemize{
#'     \item{\code{analysis_parameters}}: Analysis configuration used
#'     \item{\code{overlap_validation}}: Data validation and quality checks specific to overlaps
#'     \item{\code{computation_time}}: Time taken for analysis
#'   }
#'
#' @export
#' @importFrom data.table data.table setDT copy .N .SD := setorder uniqueN month quarter year
#' @importFrom stats median quantile
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with overlapping periods
#' employment_data <- data.table(
#'   id = 1:12,
#'   cf = c(rep("PERSON001", 8), rep("PERSON002", 4)),
#'   INIZIO = as.Date(c("2023-01-01", "2023-02-01", "2023-02-15", "2023-05-01", 
#'                      "2023-05-15", "2023-08-01", "2023-10-01", "2023-11-01",
#'                      "2023-01-15", "2023-03-01", "2023-06-01", "2023-09-01")),
#'   FINE = as.Date(c("2023-01-31", "2023-04-30", "2023-04-30", "2023-07-31",
#'                    "2023-07-15", "2023-09-30", "2023-10-31", "2023-12-31",
#'                    "2023-02-28", "2023-05-31", "2023-08-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1),
#'   company = c("CompanyA", "CompanyB", "CompanyA", "CompanyC", "CompanyD", 
#'               "CompanyE", "CompanyF", "CompanyG", "CompanyH", "CompanyI", 
#'               "CompanyJ", "CompanyK"),
#'   industry = c("Tech", "Retail", "Tech", "Finance", "Tech", "Retail", 
#'                "Finance", "Tech", "Healthcare", "Education", "Finance", "Tech")
#' )
#' 
#' # Apply vecshift to get segments with over_id
#' segments <- vecshift(employment_data)
#' 
#' # Comprehensive overlap analysis
#' overlap_analysis <- analyze_employment_overlaps(
#'   segments = segments,
#'   analysis_level = "detailed",
#'   min_overlap_duration = 1,
#'   include_temporal_patterns = TRUE,
#'   temporal_aggregation = "month"
#' )
#' 
#' # View person-level overlap patterns
#' print(overlap_analysis$person_overlaps)
#' 
#' # View aggregate overlap statistics
#' print(overlap_analysis$aggregate_overlaps)
#' 
#' # View overlap duration distribution
#' print(overlap_analysis$overlap_duration_distribution)
#' 
#' # View employment intensity patterns
#' print(overlap_analysis$employment_intensity_distribution)
#' 
#' # View temporal patterns (when overlaps occur)
#' if (!is.null(overlap_analysis$temporal_patterns)) {
#'   print(overlap_analysis$temporal_patterns)
#' }
#' 
#' # Person-level analysis only
#' person_overlaps <- analyze_employment_overlaps(
#'   segments = segments,
#'   analysis_level = "person",
#'   min_overlap_duration = 7,  # Focus on overlaps lasting at least a week
#'   include_temporal_patterns = FALSE
#' )
#' print(person_overlaps$person_overlaps)
#' 
#' # Aggregate analysis with industry comparison
#' industry_overlaps <- analyze_employment_overlaps(
#'   segments = segments,
#'   analysis_level = "aggregate",
#'   group_by = "industry",
#'   min_overlap_duration = 1
#' )
#' print(industry_overlaps$aggregate_overlaps)
#' print(industry_overlaps$group_comparisons)
#' 
#' # Quarterly temporal patterns
#' quarterly_patterns <- analyze_employment_overlaps(
#'   segments = segments,
#'   analysis_level = "detailed",
#'   include_temporal_patterns = TRUE,
#'   temporal_aggregation = "quarter"
#' )
#' print(quarterly_patterns$temporal_patterns)
#' 
#' # Check analysis validation and parameters
#' print(attr(overlap_analysis, "analysis_parameters"))
#' print(attr(overlap_analysis, "overlap_validation"))
#' 
#' # Example output interpretation:
#' # - person_overlaps shows which individuals have complex employment patterns
#' # - overlap_intensity_pct indicates how much time is spent in multiple employment
#' # - employment_complexity_score provides weighted measure of job juggling behavior
#' # - temporal_patterns reveal seasonal trends in multiple job holding
#' # - group_comparisons show industry-specific overlap behaviors
#' }
analyze_employment_overlaps <- function(segments,
                                      analysis_level = "person",
                                      min_overlap_duration = 1,
                                      include_temporal_patterns = TRUE,
                                      group_by = NULL,
                                      temporal_aggregation = "month",
                                      include_unemployment_context = TRUE) {
  
  # Record start time for performance tracking
  start_time <- Sys.time()
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Input validation
  if (!inherits(segments, "data.table")) {
    stop("Parameter 'segments' must be a data.table object from vecshift() output.")
  }
  
  # Validate analysis_level parameter
  valid_levels <- c("person", "aggregate", "detailed")
  if (!analysis_level %in% valid_levels) {
    stop(paste("Parameter 'analysis_level' must be one of:", paste(valid_levels, collapse = ", ")))
  }
  
  # Validate temporal_aggregation parameter
  valid_temporal <- c("month", "quarter", "year")
  if (!temporal_aggregation %in% valid_temporal) {
    stop(paste("Parameter 'temporal_aggregation' must be one of:", paste(valid_temporal, collapse = ", ")))
  }
  
  # Check for required columns
  required_cols <- c("cf", "inizio", "fine", "arco", "durata")
  missing_cols <- setdiff(required_cols, names(segments))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in segments:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for over_id column (essential for this analysis)
  if (!"over_id" %in% names(segments)) {
    stop("Parameter 'segments' must contain 'over_id' column from vecshift() output. ",
         "This function specifically analyzes overlapping employment using over_id.")
  }
  
  # Validate numeric parameters
  if (!is.numeric(min_overlap_duration) || min_overlap_duration < 0) {
    stop("Parameter 'min_overlap_duration' must be a non-negative numeric value.")
  }
  
  # Validate logical parameters
  if (!is.logical(include_temporal_patterns) || !is.logical(include_unemployment_context)) {
    stop("Parameters 'include_temporal_patterns' and 'include_unemployment_context' must be logical values.")
  }
  
  # Validate group_by parameter if provided
  if (!is.null(group_by)) {
    if (!is.character(group_by) || length(group_by) != 1) {
      stop("Parameter 'group_by' must be a single character string or NULL.")
    }
    if (!group_by %in% names(segments)) {
      stop(sprintf("Group variable '%s' not found in segments.", group_by))
    }
  }
  
  # Work with a copy to avoid modifying original data
  dt <- copy(segments)
  setDT(dt)
  
  # Ensure proper ordering
  setorder(dt, cf, inizio)
  
  # Overlap validation results storage
  overlap_validation <- list(
    input_validation = list(
      has_over_id = "over_id" %in% names(dt),
      total_records = nrow(dt),
      total_persons = uniqueN(dt$cf),
      total_employment_periods = sum(dt$arco >= 1, na.rm = TRUE),
      total_overlapping_periods = sum(dt$arco > 1, na.rm = TRUE),
      overlapping_persons = uniqueN(dt[arco > 1, cf]),
      date_range = if (nrow(dt) > 0) c(min(dt$inizio, na.rm = TRUE), max(dt$fine, na.rm = TRUE)) else c(NA, NA),
      arco_range = if (nrow(dt) > 0) c(min(dt$arco, na.rm = TRUE), max(dt$arco, na.rm = TRUE)) else c(NA, NA),
      over_id_range = if (nrow(dt) > 0) c(min(dt$over_id, na.rm = TRUE), max(dt$over_id, na.rm = TRUE)) else c(NA, NA)
    )
  )
  
  # Filter by minimum overlap duration if specified
  if (min_overlap_duration > 1) {
    original_nrow <- nrow(dt)
    # Keep unemployment periods (arco = 0) and employment periods meeting duration criteria
    dt <- dt[arco == 0 | (arco >= 1 & durata >= min_overlap_duration)]
    overlap_validation$duration_filter <- list(
      records_before = original_nrow,
      records_after = nrow(dt),
      records_filtered = original_nrow - nrow(dt)
    )
  }
  
  # Early exit if no data after filtering
  if (nrow(dt) == 0) {
    warning("No records remaining after filtering. Returning empty results.")
    empty_result <- .create_empty_overlap_results(analysis_level, group_by, include_temporal_patterns)
    
    # Add attributes
    setattr(empty_result, "analysis_parameters", list(
      analysis_level = analysis_level,
      min_overlap_duration = min_overlap_duration,
      include_temporal_patterns = include_temporal_patterns,
      group_by = group_by,
      temporal_aggregation = temporal_aggregation,
      include_unemployment_context = include_unemployment_context
    ))
    setattr(empty_result, "overlap_validation", overlap_validation)
    setattr(empty_result, "computation_time", difftime(Sys.time(), start_time, units = "secs"))
    
    return(empty_result)
  }
  
  # Check if there are any overlapping employment periods
  if (sum(dt$arco > 1, na.rm = TRUE) == 0) {
    message("No overlapping employment periods found (no periods with arco > 1).")
    
    # Still return person-level analysis but with zero overlap metrics
    if (analysis_level %in% c("person", "detailed")) {
      person_results <- .calculate_person_overlaps_no_overlaps(dt)
    }
    
    # Create results based on analysis level
    results <- .create_no_overlap_results(analysis_level, dt, person_results, group_by, include_temporal_patterns)
    
    # Add attributes and return
    setattr(results, "analysis_parameters", list(
      analysis_level = analysis_level,
      min_overlap_duration = min_overlap_duration,
      include_temporal_patterns = include_temporal_patterns,
      group_by = group_by,
      temporal_aggregation = temporal_aggregation,
      include_unemployment_context = include_unemployment_context
    ))
    setattr(results, "overlap_validation", overlap_validation)
    setattr(results, "computation_time", difftime(Sys.time(), start_time, units = "secs"))
    
    return(results)
  }
  
  # Initialize results list
  results <- list()
  
  # Person-level analysis
  if (analysis_level %in% c("person", "detailed")) {
    person_overlaps <- .calculate_person_level_overlaps(dt, include_unemployment_context)
    results$person_overlaps <- person_overlaps
  }
  
  # Aggregate analysis
  if (analysis_level %in% c("aggregate", "detailed")) {
    aggregate_overlaps <- .calculate_aggregate_overlaps(dt, include_unemployment_context)
    results$aggregate_overlaps <- aggregate_overlaps
  }
  
  # Detailed analysis components
  if (analysis_level == "detailed") {
    
    # Overlap duration distribution
    overlap_duration_dist <- .calculate_overlap_duration_distribution(dt)
    results$overlap_duration_distribution <- overlap_duration_dist
    
    # Employment intensity distribution (arco distribution)
    employment_intensity_dist <- .calculate_employment_intensity_distribution(dt)
    results$employment_intensity_distribution <- employment_intensity_dist
    
    # Temporal patterns if requested
    if (include_temporal_patterns) {
      temporal_patterns <- .calculate_temporal_overlap_patterns(dt, temporal_aggregation)
      if (!is.null(temporal_patterns)) {
        results$temporal_patterns <- temporal_patterns
      }
    }
    
    # Group comparisons if specified
    if (!is.null(group_by)) {
      group_comparisons <- .calculate_group_overlap_comparisons(dt, group_by, include_unemployment_context)
      results$group_comparisons <- group_comparisons
    }
  }
  
  # Add analysis metadata as attributes
  analysis_params <- list(
    analysis_level = analysis_level,
    min_overlap_duration = min_overlap_duration,
    include_temporal_patterns = include_temporal_patterns,
    group_by = group_by,
    temporal_aggregation = temporal_aggregation,
    include_unemployment_context = include_unemployment_context,
    analysis_date = Sys.Date()
  )
  
  # Calculate computation time
  end_time <- Sys.time()
  computation_time <- difftime(end_time, start_time, units = "secs")
  
  setattr(results, "analysis_parameters", analysis_params)
  setattr(results, "overlap_validation", overlap_validation)
  setattr(results, "computation_time", computation_time)
  
  return(results)
}


#' Helper Function: Calculate Person-Level Employment Overlaps
#'
#' @description
#' Internal helper function to calculate detailed person-level employment overlap statistics.
#'
#' @param dt A data.table with vecshift output containing over_id column
#' @param include_unemployment_context Logical indicating whether to include unemployment context
#'
#' @return A data.table with person-level overlap statistics
#'
#' @keywords internal
#' @noRd
.calculate_person_level_overlaps <- function(dt, include_unemployment_context = TRUE) {
  
  person_overlaps <- dt[, {
    
    # Basic employment period counts
    total_employment_periods <- sum(arco >= 1, na.rm = TRUE)
    overlapping_episodes <- sum(arco > 1, na.rm = TRUE)
    single_job_periods <- sum(arco == 1, na.rm = TRUE)
    
    # Overlap frequency metrics
    overlap_frequency <- if (total_employment_periods > 0) overlapping_episodes / total_employment_periods else 0
    
    # Employment intensity metrics
    if (total_employment_periods > 0) {
      avg_concurrent_jobs <- mean(arco[arco >= 1], na.rm = TRUE)
      max_concurrent_jobs <- max(arco[arco >= 1], na.rm = TRUE)
    } else {
      avg_concurrent_jobs <- 0
      max_concurrent_jobs <- 0
    }
    
    # Duration metrics
    total_overlap_days <- sum(durata[arco > 1], na.rm = TRUE)
    total_employment_days <- sum(durata[arco >= 1], na.rm = TRUE)
    overlap_intensity_pct <- if (total_employment_days > 0) (total_overlap_days / total_employment_days) * 100 else 0
    
    # Overlap episode characteristics
    if (overlapping_episodes > 0) {
      avg_overlap_duration <- mean(durata[arco > 1], na.rm = TRUE)
      max_overlap_duration <- max(durata[arco > 1], na.rm = TRUE)
    } else {
      avg_overlap_duration <- 0
      max_overlap_duration <- 0
    }
    
    # Unique over_id groups for overlapping periods
    unique_over_id_groups <- uniqueN(over_id[arco > 1 & over_id > 0])
    
    # Employment complexity score (weighted by duration and intensity)
    if (total_employment_days > 0) {
      complexity_weights <- durata[arco >= 1] * (arco[arco >= 1] - 1)  # Weight by excess jobs
      employment_complexity_score <- sum(complexity_weights, na.rm = TRUE) / total_employment_days
    } else {
      employment_complexity_score <- 0
    }
    
    # Unemployment context if requested
    if (include_unemployment_context) {
      total_unemployment_periods <- sum(arco == 0, na.rm = TRUE)
      total_unemployment_days <- sum(durata[arco == 0], na.rm = TRUE)
      employment_to_unemployment_ratio <- if (total_unemployment_days > 0) total_employment_days / total_unemployment_days else Inf
    } else {
      total_unemployment_periods <- NA_integer_
      total_unemployment_days <- NA_real_
      employment_to_unemployment_ratio <- NA_real_
    }
    
    list(
      cf = .BY$cf,
      total_employment_periods = total_employment_periods,
      overlapping_episodes = overlapping_episodes,
      single_job_periods = single_job_periods,
      overlap_frequency = round(overlap_frequency, 4),
      avg_concurrent_jobs = round(avg_concurrent_jobs, 2),
      max_concurrent_jobs = max_concurrent_jobs,
      total_overlap_days = total_overlap_days,
      total_employment_days = total_employment_days,
      overlap_intensity_pct = round(overlap_intensity_pct, 2),
      avg_overlap_duration = round(avg_overlap_duration, 1),
      max_overlap_duration = max_overlap_duration,
      unique_over_id_groups = unique_over_id_groups,
      employment_complexity_score = round(employment_complexity_score, 4),
      total_unemployment_periods = total_unemployment_periods,
      total_unemployment_days = total_unemployment_days,
      employment_to_unemployment_ratio = if (is.infinite(employment_to_unemployment_ratio)) NA_real_ else round(employment_to_unemployment_ratio, 2)
    )
  }, by = cf]
  
  return(person_overlaps)
}


#' Helper Function: Calculate Aggregate Employment Overlaps
#'
#' @description
#' Internal helper function to calculate population-level aggregate employment overlap statistics.
#'
#' @param dt A data.table with vecshift output containing over_id column
#' @param include_unemployment_context Logical indicating whether to include unemployment context
#'
#' @return A data.table with aggregate overlap statistics
#'
#' @keywords internal
#' @noRd
.calculate_aggregate_overlaps <- function(dt, include_unemployment_context = TRUE) {
  
  # Population-level aggregate statistics
  total_persons <- uniqueN(dt$cf)
  total_employment_periods <- sum(dt$arco >= 1, na.rm = TRUE)
  total_overlapping_periods <- sum(dt$arco > 1, na.rm = TRUE)
  
  # Persons with overlapping employment
  persons_with_overlaps <- uniqueN(dt[arco > 1, cf])
  pct_persons_with_overlaps <- round(100 * persons_with_overlaps / total_persons, 1)
  
  # Employment intensity metrics
  overlap_frequency_population <- if (total_employment_periods > 0) total_overlapping_periods / total_employment_periods else 0
  avg_employment_intensity <- mean(dt[arco >= 1, arco], na.rm = TRUE)
  max_employment_intensity <- max(dt[arco >= 1, arco], na.rm = TRUE)
  
  # Duration metrics
  total_overlap_days <- sum(dt[arco > 1, durata], na.rm = TRUE)
  total_employment_days <- sum(dt[arco >= 1, durata], na.rm = TRUE)
  population_overlap_intensity <- if (total_employment_days > 0) (total_overlap_days / total_employment_days) * 100 else 0
  
  # Overlap episode characteristics
  avg_overlap_duration <- if (total_overlapping_periods > 0) mean(dt[arco > 1, durata], na.rm = TRUE) else 0
  median_overlap_duration <- if (total_overlapping_periods > 0) median(dt[arco > 1, durata], na.rm = TRUE) else 0
  
  # Over_id analysis for overlapping periods
  unique_overlap_episodes <- uniqueN(dt[arco > 1 & over_id > 0, .(cf, over_id)])
  avg_overlap_episodes_per_person <- if (persons_with_overlaps > 0) unique_overlap_episodes / persons_with_overlaps else 0
  
  # Employment complexity distribution
  arco_distribution <- dt[arco >= 1, .N, by = arco][order(arco)]
  arco_distribution[, percentage := round(100 * N / sum(N), 1)]
  
  # Create aggregate results
  aggregate_stats <- list(
    total_persons = total_persons,
    total_employment_periods = total_employment_periods,
    total_overlapping_periods = total_overlapping_periods,
    persons_with_overlaps = persons_with_overlaps,
    pct_persons_with_overlaps = pct_persons_with_overlaps,
    overlap_frequency_population = round(overlap_frequency_population, 4),
    avg_employment_intensity = round(avg_employment_intensity, 2),
    max_employment_intensity = max_employment_intensity,
    total_overlap_days = total_overlap_days,
    total_employment_days = total_employment_days,
    population_overlap_intensity_pct = round(population_overlap_intensity, 2),
    avg_overlap_duration = round(avg_overlap_duration, 1),
    median_overlap_duration = round(median_overlap_duration, 1),
    unique_overlap_episodes = unique_overlap_episodes,
    avg_overlap_episodes_per_person = round(avg_overlap_episodes_per_person, 2)
  )
  
  # Add unemployment context if requested
  if (include_unemployment_context) {
    total_unemployment_periods <- sum(dt$arco == 0, na.rm = TRUE)
    total_unemployment_days <- sum(dt[arco == 0, durata], na.rm = TRUE)
    employment_to_unemployment_days_ratio <- if (total_unemployment_days > 0) total_employment_days / total_unemployment_days else Inf
    
    aggregate_stats$total_unemployment_periods <- total_unemployment_periods
    aggregate_stats$total_unemployment_days <- total_unemployment_days
    aggregate_stats$employment_to_unemployment_ratio <- if (is.infinite(employment_to_unemployment_days_ratio)) NA_real_ else round(employment_to_unemployment_days_ratio, 2)
  }
  
  # Convert to data.table format
  aggregate_result <- data.table(
    metric = names(aggregate_stats),
    value = as.character(unlist(aggregate_stats))
  )
  
  # Add employment intensity distribution as separate component
  aggregate_result_list <- list(
    aggregate_metrics = aggregate_result,
    employment_intensity_breakdown = arco_distribution
  )
  
  return(aggregate_result_list)
}


#' Helper Function: Calculate Overlap Duration Distribution
#'
#' @description
#' Internal helper function to calculate distribution of overlap durations.
#'
#' @param dt A data.table with vecshift output containing over_id column
#'
#' @return A data.table with overlap duration distribution
#'
#' @keywords internal
#' @noRd
.calculate_overlap_duration_distribution <- function(dt) {
  
  if (sum(dt$arco > 1, na.rm = TRUE) == 0) {
    return(data.table(
      duration_category = character(0),
      n_periods = integer(0),
      percentage = numeric(0),
      total_days = numeric(0)
    ))
  }
  
  # Extract overlapping periods
  overlap_periods <- dt[arco > 1]
  
  # Create duration categories
  overlap_periods[, duration_category := ifelse(
    durata <= 7, "1-7 days",
    ifelse(durata <= 30, "8-30 days",
           ifelse(durata <= 90, "31-90 days",
                  ifelse(durata <= 180, "91-180 days",
                         ifelse(durata <= 365, "181-365 days", "366+ days")))))]
  
  # Calculate distribution
  duration_dist <- overlap_periods[, .(
    n_periods = .N,
    total_days = sum(durata, na.rm = TRUE),
    avg_duration = round(mean(durata, na.rm = TRUE), 1),
    median_duration = round(median(durata, na.rm = TRUE), 1),
    min_duration = min(durata, na.rm = TRUE),
    max_duration = max(durata, na.rm = TRUE)
  ), by = duration_category]
  
  # Add percentages
  duration_dist[, percentage := round(100 * n_periods / sum(n_periods), 1)]
  
  # Order by duration
  duration_order <- c("1-7 days", "8-30 days", "31-90 days", "91-180 days", "181-365 days", "366+ days")
  duration_dist[, duration_category := factor(duration_category, levels = duration_order)]
  setorder(duration_dist, duration_category)
  duration_dist[, duration_category := as.character(duration_category)]
  
  return(duration_dist)
}


#' Helper Function: Calculate Employment Intensity Distribution
#'
#' @description
#' Internal helper function to calculate distribution of employment intensity (arco values).
#'
#' @param dt A data.table with vecshift output containing over_id column
#'
#' @return A data.table with employment intensity distribution
#'
#' @keywords internal
#' @noRd
.calculate_employment_intensity_distribution <- function(dt) {
  
  # Calculate distribution of arco values for employment periods
  intensity_dist <- dt[arco >= 1, .(
    n_periods = .N,
    total_days = sum(durata, na.rm = TRUE),
    avg_duration = round(mean(durata, na.rm = TRUE), 1),
    median_duration = round(median(durata, na.rm = TRUE), 1),
    n_persons = uniqueN(cf),
    n_unique_over_id = uniqueN(over_id[over_id > 0])
  ), by = arco]
  
  # Add percentages
  intensity_dist[, percentage_of_periods := round(100 * n_periods / sum(n_periods), 1)]
  intensity_dist[, percentage_of_days := round(100 * total_days / sum(total_days), 1)]
  
  # Add employment type labels
  intensity_dist[, employment_type := ifelse(arco == 1, "Single employment", 
                                           paste0("Multiple employment (", arco, " jobs)"))]
  
  # Order by arco
  setorder(intensity_dist, arco)
  
  return(intensity_dist)
}


#' Helper Function: Calculate Temporal Overlap Patterns
#'
#' @description
#' Internal helper function to calculate when employment overlaps occur most frequently.
#'
#' @param dt A data.table with vecshift output containing over_id column
#' @param temporal_aggregation Character string specifying temporal aggregation level
#'
#' @return A data.table with temporal overlap patterns or NULL if dates not available
#'
#' @keywords internal
#' @noRd
.calculate_temporal_overlap_patterns <- function(dt, temporal_aggregation = "month") {
  
  # Check if date columns are properly formatted
  if (!inherits(dt$inizio, "Date") || !inherits(dt$fine, "Date")) {
    warning("Date columns not properly formatted for temporal analysis. Skipping temporal patterns.")
    return(NULL)
  }
  
  if (sum(dt$arco > 1, na.rm = TRUE) == 0) {
    return(NULL)
  }
  
  # Extract overlapping periods
  overlap_periods <- dt[arco > 1]
  
  # Create temporal aggregation
  if (temporal_aggregation == "month") {
    overlap_periods[, temporal_period := format(inizio, "%Y-%m")]
    overlap_periods[, temporal_label := format(inizio, "%B %Y")]
  } else if (temporal_aggregation == "quarter") {
    overlap_periods[, temporal_period := paste0(format(inizio, "%Y"), "-Q", quarter(inizio))]
    overlap_periods[, temporal_label := paste0("Q", quarter(inizio), " ", format(inizio, "%Y"))]
  } else if (temporal_aggregation == "year") {
    overlap_periods[, temporal_period := format(inizio, "%Y")]
    overlap_periods[, temporal_label := format(inizio, "%Y")]
  }
  
  # Calculate temporal patterns
  temporal_patterns <- overlap_periods[, .(
    n_overlapping_periods = .N,
    total_overlap_days = sum(durata, na.rm = TRUE),
    n_persons_with_overlaps = uniqueN(cf),
    avg_concurrent_jobs = round(mean(arco, na.rm = TRUE), 2),
    max_concurrent_jobs = max(arco, na.rm = TRUE),
    avg_overlap_duration = round(mean(durata, na.rm = TRUE), 1),
    n_unique_over_id_episodes = uniqueN(over_id[over_id > 0])
  ), by = .(temporal_period, temporal_label)]
  
  # Add percentages and rankings
  temporal_patterns[, percentage_of_overlaps := round(100 * n_overlapping_periods / sum(n_overlapping_periods), 1)]
  temporal_patterns[, percentage_of_overlap_days := round(100 * total_overlap_days / sum(total_overlap_days), 1)]
  
  # Order by temporal period
  setorder(temporal_patterns, temporal_period)
  
  return(temporal_patterns)
}


#' Helper Function: Calculate Group Overlap Comparisons
#'
#' @description
#' Internal helper function to compare overlap patterns across different groups.
#'
#' @param dt A data.table with vecshift output containing over_id column
#' @param group_by Character string specifying grouping variable
#' @param include_unemployment_context Logical indicating whether to include unemployment context
#'
#' @return A data.table with group-based overlap comparisons
#'
#' @keywords internal
#' @noRd
.calculate_group_overlap_comparisons <- function(dt, group_by, include_unemployment_context = TRUE) {
  
  # Calculate group-level overlap statistics
  group_comparisons <- dt[, {
    
    # Basic metrics
    total_persons <- uniqueN(cf)
    total_employment_periods <- sum(arco >= 1, na.rm = TRUE)
    total_overlapping_periods <- sum(arco > 1, na.rm = TRUE)
    persons_with_overlaps <- uniqueN(cf[arco > 1])
    
    # Overlap intensity metrics
    overlap_frequency <- if (total_employment_periods > 0) total_overlapping_periods / total_employment_periods else 0
    pct_persons_with_overlaps := if (total_persons > 0) 100 * persons_with_overlaps / total_persons else 0
    
    # Employment intensity
    avg_employment_intensity <- if (total_employment_periods > 0) mean(arco[arco >= 1], na.rm = TRUE) else 0
    max_employment_intensity <- if (total_employment_periods > 0) max(arco[arco >= 1], na.rm = TRUE) else 0
    
    # Duration metrics
    total_overlap_days <- sum(durata[arco > 1], na.rm = TRUE)
    total_employment_days <- sum(durata[arco >= 1], na.rm = TRUE)
    overlap_intensity_pct <- if (total_employment_days > 0) (total_overlap_days / total_employment_days) * 100 else 0
    avg_overlap_duration := if (total_overlapping_periods > 0) mean(durata[arco > 1], na.rm = TRUE) else 0
    
    # Unemployment context if requested
    if (include_unemployment_context) {
      total_unemployment_days <- sum(durata[arco == 0], na.rm = TRUE)
      employment_to_unemployment_ratio <- if (total_unemployment_days > 0) total_employment_days / total_unemployment_days else NA_real_
    } else {
      employment_to_unemployment_ratio := NA_real_
    }
    
    list(
      group_value = get(group_by)[1],  # Take first value for the group
      total_persons = total_persons,
      total_employment_periods = total_employment_periods,
      total_overlapping_periods = total_overlapping_periods,
      persons_with_overlaps = persons_with_overlaps,
      pct_persons_with_overlaps = round(pct_persons_with_overlaps, 1),
      overlap_frequency = round(overlap_frequency, 4),
      avg_employment_intensity = round(avg_employment_intensity, 2),
      max_employment_intensity = max_employment_intensity,
      overlap_intensity_pct = round(overlap_intensity_pct, 2),
      avg_overlap_duration = round(avg_overlap_duration, 1),
      employment_to_unemployment_ratio = if (is.na(employment_to_unemployment_ratio)) NA_real_ else round(employment_to_unemployment_ratio, 2)
    )
  }, by = get(group_by)]
  
  # Rename the grouping column
  setnames(group_comparisons, "get", group_by)
  
  # Add rankings for key metrics
  group_comparisons[, overlap_frequency_rank := rank(-overlap_frequency, ties.method = "min")]
  group_comparisons[, overlap_intensity_rank := rank(-overlap_intensity_pct, ties.method = "min")]
  group_comparisons[, persons_with_overlaps_rank := rank(-pct_persons_with_overlaps, ties.method = "min")]
  
  # Order by overlap frequency (descending)
  setorder(group_comparisons, -overlap_frequency)
  
  return(group_comparisons)
}


#' Helper Function: Create Empty Overlap Results Structure
#'
#' @description
#' Internal helper function to create empty results structure when no data is available.
#'
#' @param analysis_level Character string specifying analysis level
#' @param group_by Character string or NULL for grouping variable
#' @param include_temporal_patterns Logical for temporal analysis inclusion
#'
#' @return Empty results structure appropriate for analysis level
#'
#' @keywords internal
#' @noRd
.create_empty_overlap_results <- function(analysis_level, group_by = NULL, include_temporal_patterns = FALSE) {
  
  results <- list()
  
  if (analysis_level %in% c("person", "detailed")) {
    results$person_overlaps <- data.table(
      cf = character(0),
      total_employment_periods = integer(0),
      overlapping_episodes = integer(0),
      single_job_periods = integer(0),
      overlap_frequency = numeric(0),
      avg_concurrent_jobs = numeric(0),
      max_concurrent_jobs = integer(0),
      total_overlap_days = numeric(0),
      total_employment_days = numeric(0),
      overlap_intensity_pct = numeric(0),
      avg_overlap_duration = numeric(0),
      max_overlap_duration = numeric(0),
      unique_over_id_groups = integer(0),
      employment_complexity_score = numeric(0)
    )
  }
  
  if (analysis_level %in% c("aggregate", "detailed")) {
    results$aggregate_overlaps <- list(
      aggregate_metrics = data.table(metric = character(0), value = character(0)),
      employment_intensity_breakdown = data.table(
        arco = integer(0), n_periods = integer(0), percentage = numeric(0)
      )
    )
  }
  
  if (analysis_level == "detailed") {
    results$overlap_duration_distribution <- data.table(
      duration_category = character(0), n_periods = integer(0), percentage = numeric(0)
    )
    results$employment_intensity_distribution <- data.table(
      arco = integer(0), n_periods = integer(0), percentage_of_periods = numeric(0)
    )
    
    if (include_temporal_patterns) {
      results$temporal_patterns <- data.table(
        temporal_period = character(0), n_overlapping_periods = integer(0), percentage_of_overlaps = numeric(0)
      )
    }
    
    if (!is.null(group_by)) {
      results$group_comparisons <- data.table()
      setnames(results$group_comparisons, names(results$group_comparisons), 
               c(group_by, "total_persons", "overlap_frequency"))
    }
  }
  
  return(results)
}


#' Helper Function: Calculate Person Overlaps When No Overlaps Exist
#'
#' @description
#' Internal helper function to create person-level results when no overlapping employment exists.
#'
#' @param dt A data.table with vecshift output
#'
#' @return A data.table with person-level statistics showing zero overlaps
#'
#' @keywords internal
#' @noRd
.calculate_person_overlaps_no_overlaps <- function(dt) {
  
  person_results <- dt[, {
    total_employment_periods <- sum(arco >= 1, na.rm = TRUE)
    total_employment_days <- sum(durata[arco >= 1], na.rm = TRUE)
    total_unemployment_periods <- sum(arco == 0, na.rm = TRUE)
    total_unemployment_days <- sum(durata[arco == 0], na.rm = TRUE)
    
    list(
      cf = .BY$cf,
      total_employment_periods = total_employment_periods,
      overlapping_episodes = 0L,
      single_job_periods = total_employment_periods,
      overlap_frequency = 0.0,
      avg_concurrent_jobs = if (total_employment_periods > 0) 1.0 else 0.0,
      max_concurrent_jobs = if (total_employment_periods > 0) 1L else 0L,
      total_overlap_days = 0,
      total_employment_days = total_employment_days,
      overlap_intensity_pct = 0.0,
      avg_overlap_duration = 0.0,
      max_overlap_duration = 0,
      unique_over_id_groups = 0L,
      employment_complexity_score = 0.0,
      total_unemployment_periods = total_unemployment_periods,
      total_unemployment_days = total_unemployment_days,
      employment_to_unemployment_ratio = if (total_unemployment_days > 0) round(total_employment_days / total_unemployment_days, 2) else NA_real_
    )
  }, by = cf]
  
  return(person_results)
}


#' Helper Function: Create Results When No Overlaps Exist
#'
#' @description
#' Internal helper function to create appropriate results structure when no overlapping employment exists.
#'
#' @param analysis_level Character string specifying analysis level
#' @param dt A data.table with vecshift output
#' @param person_results Person-level results (if calculated)
#' @param group_by Character string or NULL for grouping variable
#' @param include_temporal_patterns Logical for temporal analysis inclusion
#'
#' @return Results structure with zero-overlap data
#'
#' @keywords internal
#' @noRd
.create_no_overlap_results <- function(analysis_level, dt, person_results = NULL, group_by = NULL, include_temporal_patterns = FALSE) {
  
  results <- list()
  
  if (analysis_level %in% c("person", "detailed")) {
    results$person_overlaps <- person_results
  }
  
  if (analysis_level %in% c("aggregate", "detailed")) {
    # Create aggregate results showing zero overlaps but basic employment info
    total_persons <- uniqueN(dt$cf)
    total_employment_periods <- sum(dt$arco >= 1, na.rm = TRUE)
    
    aggregate_stats <- list(
      total_persons = total_persons,
      total_employment_periods = total_employment_periods,
      total_overlapping_periods = 0,
      persons_with_overlaps = 0,
      pct_persons_with_overlaps = 0,
      overlap_frequency_population = 0,
      avg_employment_intensity = if (total_employment_periods > 0) 1.0 else 0.0,
      max_employment_intensity = if (total_employment_periods > 0) 1 else 0
    )
    
    results$aggregate_overlaps <- list(
      aggregate_metrics = data.table(
        metric = names(aggregate_stats),
        value = as.character(unlist(aggregate_stats))
      ),
      employment_intensity_breakdown = if (total_employment_periods > 0) {
        data.table(arco = 1L, n_periods = total_employment_periods, 
                   percentage = 100.0, employment_type = "Single employment")
      } else {
        data.table(arco = integer(0), n_periods = integer(0), percentage = numeric(0), employment_type = character(0))
      }
    )
  }
  
  return(results)
}