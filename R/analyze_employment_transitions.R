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





#' @title Consolidate Employment Contracts by Employer
#' @description
#' Helper function that consolidates consecutive contracts with the same employer
#' within a specified time gap, while never consolidating contracts from different employers.
#'
#' @param pipeline_result data.table object from process_employment_pipeline()
#' @param employer_var Character string specifying column name containing employer identifiers
#' @param min_lag Numeric value specifying maximum gap (in days) between contracts
#'   from the same employer to be consolidated (default: 8)
#'
#' @return data.table with consolidated employment periods
#'
#' @details
#' This function:
#' \itemize{
#'   \item Groups contracts by person (cf) and employer
#'   \item Identifies consecutive contracts within min_lag days
#'   \item Consolidates them preserving appropriate column values
#'   \item Never consolidates across different employers
#' }
#'
#' Column consolidation strategy:
#' \itemize{
#'   \item Dates: Use first 'inizio', last 'fine'
#'   \item Duration: Recalculate as fine - inizio + 1
#'   \item Numeric columns: Use sum for additive values, mean for rates
#'   \item Character columns: Use first value (mode if available)
#' }
#'
#' @keywords internal
consolidate_by_employer <- function(pipeline_result, employer_var, min_lag = 8) {
  
  # Input validation
  if (!inherits(pipeline_result, "data.table")) {
    stop("pipeline_result must be a data.table object")
  }
  
  if (is.null(employer_var) || !employer_var %in% names(pipeline_result)) {
    stop("employer_var must specify a valid column name in pipeline_result")
  }
  
  if (!is.numeric(min_lag) || min_lag < 0) {
    stop("min_lag must be a non-negative numeric value")
  }
  
  # Create working copy
  dt <- copy(pipeline_result)
  
  # Ensure required columns exist
  required_cols <- c("cf", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Ensure date columns are Date objects
  if (!inherits(dt$inizio, "Date")) {
    dt[, inizio := as.Date(inizio)]
  }
  if (!inherits(dt$fine, "Date")) {
    dt[, fine := as.Date(fine)]
  }
  
  # Sort by person, employer, and start date
  setorderv(dt, cols = c("cf", employer_var, "inizio"))
  
  # Create grouping for consolidation
  # Contracts are consolidated if:
  # 1. Same person (cf)
  # 2. Same employer 
  # 3. Gap between contracts <= min_lag days
  
  dt[, `:=`(
    prev_employer = shift(.SD[[employer_var]], 1L),
    prev_fine = shift(fine, 1L)
  ), by = "cf"]
  
  # Calculate gap between current start and previous end
  dt[, gap_days := as.numeric(inizio - prev_fine)]
  
  # Mark records that should NOT start a new consolidation group
  # (same employer and gap <= threshold)
  dt[, new_group := is.na(prev_employer) | 
                   .SD[[employer_var]] != prev_employer | 
                   is.na(gap_days) | 
                   gap_days > min_lag]
  
  # Create consolidation group IDs
  dt[, consolidation_group := cumsum(new_group), by = "cf"]
  
  # Get column names for aggregation (exclude helper columns)
  exclude_cols <- c("prev_employer", "prev_fine", "gap_days", "new_group", "consolidation_group")
  agg_cols <- setdiff(names(dt), exclude_cols)
  
  # Store original column classes for type restoration
  original_classes <- sapply(dt, class, simplify = FALSE)
  
  # Perform consolidation using simple aggregation approach to avoid type conflicts
  consolidated <- dt[, {
    # Calculate aggregated values using consistent types
    min_inizio <- min(inizio, na.rm = TRUE)
    max_fine <- max(fine, na.rm = TRUE)
    new_durata <- as.numeric(max_fine - min_inizio + 1)
    
    # Start with core columns (cf is automatically included by the grouping)
    result <- list(
      inizio = min_inizio,
      fine = max_fine,
      durata = new_durata
    )
    
    # Add employer variable if different from cf
    if (employer_var != "cf" && employer_var %in% agg_cols) {
      result[[employer_var]] <- .SD[[employer_var]][1L]  # Should be same within group
    }
    
    # Add all other columns with simple aggregation
    # Note: cf is excluded because it's already in the grouping variables
    other_cols <- setdiff(agg_cols, c("inizio", "fine", "durata", "cf", employer_var))
    for (col in other_cols) {
      col_vals <- .SD[[col]]
      if (is.numeric(col_vals)) {
        # For numeric: use mean to ensure consistent double type
        result[[col]] <- mean(col_vals, na.rm = TRUE)
      } else {
        # For non-numeric: use first value to ensure consistent type
        result[[col]] <- col_vals[1L]
      }
    }
    
    result
  }, by = .(cf, consolidation_group)]
  
  # Restore original column types after aggregation
  for (col in names(consolidated)) {
    if (col %in% names(original_classes) && !col %in% c("cf", "consolidation_group")) {
      orig_class <- original_classes[[col]]
      
      if (inherits(orig_class, "integer") && is.numeric(consolidated[[col]])) {
        # Convert back to integer if original was integer and result is numeric
        consolidated[, (col) := as.integer(round(get(col)))]
      } else if (any(c("Date", "IDate") %in% orig_class)) {
        # Restore date classes
        consolidated[, (col) := structure(get(col), class = orig_class)]
      }
    }
  }
  
  # Remove helper column
  consolidated[, consolidation_group := NULL]
  
  # Restore original column order (approximately)
  original_cols <- intersect(names(pipeline_result), names(consolidated))
  data.table::setcolorder(consolidated, original_cols)
  
  return(consolidated)
}

#' @title Analyze Employment Transitions from Pipeline Output
#' @description
#' Analyzes employment transitions from the output of process_employment_pipeline().
#' Identifies transitions between employment periods that are separated by unemployment
#' periods and provides transition pattern analysis. The function allows specification
#' of which variable to use for transition analysis and which variables to compute
#' statistics for.
#'
#' The function supports multiple consolidation strategies controlled by \code{consolidation_mode}
#' to consolidate overlapping and/or consecutive employment periods using the \code{over_id} column
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
#' \subsection{Consolidation Strategies}{
#' The function supports four distinct consolidation approaches controlled by \code{consolidation_mode}:
#' 
#' \strong{No Consolidation (consolidation_mode = "none")}:
#' Analyzes transitions using original employment periods without any consolidation.
#' Provides the most granular view of transitions but may include administrative 
#' contract splits that don't represent actual job changes.
#' 
#' \strong{Temporal Consolidation (consolidation_mode = "temporal")}:
#' Uses vecshift package temporal consolidation based on over_id and time proximity.
#' Consolidates overlapping/consecutive periods regardless of employer using the 
#' consolidation_type parameter. Benefits include:
#' \itemize{
#'   \item{\strong{Accurate Transitions}}: Analyzes transitions between true employment
#'     episodes rather than administrative contract splits
#'   \item{\strong{Better Unemployment Duration}}: More precise calculation of time
#'     between actual employment periods
#'   \item{\strong{Cleaner Patterns}}: Reduces noise from overlapping contracts
#' }
#' 
#' \strong{Employer-Based Consolidation (consolidation_mode = "employer")}:
#' Consolidates consecutive contracts with the same employer within min_lag days,
#' while never consolidating contracts from different employers. Characteristics:
#' \itemize{
#'   \item{\strong{Employer Respect}}: Never merges contracts from different employers,
#'     even if temporally adjacent
#'   \item{\strong{Same-Employer Consolidation}}: Consolidates contracts from the same
#'     employer if separated by ≤ min_lag days  
#'   \item{\strong{True Job Changes}}: Better identification of actual job changes vs.
#'     contract renewals with the same employer
#' }
#' 
#' \strong{Sequential Consolidation (consolidation_mode = "both")}:
#' Applies both consolidation methods sequentially - first employer-based, then temporal.
#' This provides maximum consolidation while respecting employer boundaries:
#' \itemize{
#'   \item{\strong{Step 1}}: Employer consolidation respects organizational boundaries
#'   \item{\strong{Step 2}}: Temporal consolidation further reduces administrative splits
#'   \item{\strong{Optimal Balance}}: Combines employer-aware job change detection with
#'     comprehensive period consolidation
#' }
#' }
#' 
#' \subsection{When to Use Each Mode}{
#' \itemize{
#'   \item{\strong{Use "temporal" mode}} when analyzing administrative employment data
#'     where contract splits don't necessarily represent job changes, and you want to
#'     focus on employment episode continuity regardless of employer.
#'   \item{\strong{Use "employer" mode}} when you have reliable employer identifiers and
#'     want to analyze true job mobility between different organizations, ensuring that
#'     transitions represent actual employer changes rather than contract renewals.
#' }
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
#'   (overlap identifier, required when consolidation_mode is "temporal" or "both").
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
#' @param consolidation_type Character string specifying consolidation approach when
#'   consolidation_mode is "temporal" or "both" (default: "both"). Options:
#'   \itemize{
#'     \item{\code{"both"}}: First consolidate overlapping periods (same over_id > 0),
#'       then merge consecutive periods. Provides complete employment history consolidation.
#'     \item{\code{"overlapping"}}: Only consolidate segments with same over_id > 0.
#'       Merges simultaneous/overlapping contracts into single periods.
#'     \item{\code{"consecutive"}}: Only merge periods that are contiguous in time,
#'       regardless of over_id. Traditional consecutive period merging.
#'   }
#'   Ignored when consolidation_mode is "employer" or "none".
#' @param consolidation_mode Character string specifying consolidation strategy (default: "none"). Options:
#'   \itemize{
#'     \item{\code{"none"}}: No consolidation - analyze transitions using original employment periods
#'     \item{\code{"temporal"}}: Uses vecshift package temporal consolidation based on over_id
#'       and time proximity. Consolidates overlapping/consecutive periods regardless of employer
#'       using the consolidation_type parameter ("both", "overlapping", "consecutive").
#'     \item{\code{"employer"}}: Uses employer-based consolidation that respects employer boundaries.
#'       Only consolidates consecutive contracts from the same employer (identified by employer_var)
#'       if they are separated by ≤ min_lag days. Never consolidates across different
#'       employers, ensuring transitions represent true job changes. Requires employer_var parameter.
#'     \item{\code{"both"}}: Sequential consolidation combining both approaches. First applies
#'       employer-based consolidation to respect employer boundaries, then applies temporal
#'       consolidation to the result. Requires both employer_var and consolidation_type parameters.
#'   }
#' @param employer_var Character string specifying the column name containing employer identifiers
#'   (default: NULL). Required when consolidation_mode = "employer". This column should contain
#'   unique identifiers for each employer (e.g., company IDs, tax codes, or standardized company names).
#'   Contracts with the same employer_var value will be considered for consolidation if within
#'   min_lag days. Ignored and produces a warning when consolidation_mode = "temporal".
#' @param min_lag Numeric value specifying the maximum gap in days between consecutive
#'   contracts from the same employer to be consolidated when consolidation_mode = "employer"
#'   (default: 8). Contracts from the same employer separated by more than this number of days
#'   will remain as separate employment periods, allowing for detection of re-hiring patterns.
#'   Lower values (e.g., 7) provide stricter consolidation, while higher values (e.g., 90) are
#'   more permissive for employers with seasonal or project-based hiring patterns.
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
#' # Analyze company transitions with temporal consolidation
#' transitions <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   statistics_variables = c("salary"),
#'   consolidation_mode = "temporal",
#'   consolidation_type = "both"
#' )
#' 
#' # Compare with no consolidation
#' transitions_original <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company", 
#'   statistics_variables = c("salary"),
#'   consolidation_mode = "none"
#' )
#' 
#' # Employer-based consolidation
#' transitions_employer <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "employer",
#'   employer_var = "employer_id",
#'   min_lag = 8
#' )
#' 
#' # Sequential consolidation (both methods)
#' transitions_both <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "both",
#'   employer_var = "employer_id", 
#'   min_lag = 8,
#'   consolidation_type = "both"
#' )
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
#' # ========================================================================
#' # CONSOLIDATION MODE EXAMPLES: Temporal vs Employer-Based
#' # ========================================================================
#' 
#' # Create realistic example data with employer information
#' result_with_employer <- copy(result)
#' result_with_employer[, employer_id := c("ACME_CORP", "ACME_CORP", "BETA_LTD", 
#'                                        "GAMMA_INC", "GAMMA_INC", "DELTA_CO")]
#' 
#' # Example 1: TEMPORAL CONSOLIDATION (default behavior)
#' # Consolidates based on over_id and time proximity, regardless of employer
#' transitions_temporal <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "temporal",      # Uses over_id for consolidation
#'   consolidation_type = "both"          # Consolidate overlapping + consecutive
#' )
#' 
#' # Example 2: EMPLOYER-BASED CONSOLIDATION  
#' # Only consolidates contracts from same employer within time gap
#' transitions_employer <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "employer",      # NEW: employer-based consolidation
#'   employer_var = "employer_id",         # Column containing employer identifiers
#'   min_lag = 8                 # Max 8-day gap for same-employer consolidation
#' )
#' 
#' # Example 3: Compare consolidation approaches
#' # Temporal: May consolidate CompanyA -> CompanyB if temporally adjacent
#' # Employer: Never consolidates CompanyA -> CompanyB (different employers)
#' print("Temporal consolidation results:")
#' print(transitions_temporal)
#' print("Employer-based consolidation results:")
#' print(transitions_employer)
#' 
#' # Example 4: Stricter employer-based consolidation (7-day gap)
#' # Useful for identifying very close contract renewals vs. true separations
#' transitions_strict <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "employer",
#'   employer_var = "employer_id",
#'   min_lag = 7                  # Only 7-day gap allowed
#' )
#' 
#' # Example 5: Permissive employer-based consolidation (90-day gap)
#' # Useful for seasonal employers or project-based work patterns
#' transitions_permissive <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",
#'   consolidation_mode = "employer",
#'   employer_var = "employer_id",
#'   min_lag = 90                 # Allow 3-month gaps for same employer
#' )
#' 
#' # Example 6: Real-world use case comparison
#' # When you want to analyze job mobility vs. contract administration:
#' 
#' # For JOB MOBILITY analysis (focus on employer changes):
#' job_mobility <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "employer_id",   # Analyze employer-to-employer transitions
#'   consolidation_mode = "employer",
#'   employer_var = "employer_id",
#'   min_lag = 8
#' )
#' 
#' # For ROLE/POSITION analysis within and between employers:
#' role_transitions <- analyze_employment_transitions(
#'   pipeline_result = result_with_employer,
#'   transition_variable = "company",       # Analyze role/position transitions
#'   consolidation_mode = "employer",       # But respect employer boundaries
#'   employer_var = "employer_id",
#'   min_lag = 8
#' )
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
                                         consolidation_type = "both",
                                         consolidation_mode = "none",
                                         employer_var = NULL,
                                         min_lag = 8,
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
  
  # Validate consolidation_mode
  valid_consolidation_modes <- c("employer", "temporal", "none", "both")
  if (!consolidation_mode %in% valid_consolidation_modes) {
    stop("Parameter 'consolidation_mode' must be one of: ", paste(valid_consolidation_modes, collapse = ", "))
  }
  
  # Mode-specific parameter validation
  if (consolidation_mode %in% c("employer", "both")) {
    if (is.null(employer_var)) {
      stop("Parameter 'employer_var' is required when consolidation_mode = '", consolidation_mode, "'")
    }
    if (!is.character(employer_var) || length(employer_var) != 1) {
      stop("Parameter 'employer_var' must be a single character string")
    }
    if (!employer_var %in% names(pipeline_result)) {
      stop(paste("Column", employer_var, "not found in pipeline_result"))
    }
    if (!is.numeric(min_lag) || min_lag < 0) {
      stop("Parameter 'min_lag' must be a non-negative numeric value")
    }
  }
  
  if (consolidation_mode %in% c("temporal", "both")) {
    # Validate consolidation_type for temporal modes
    valid_consolidation_types <- c("both", "overlapping", "consecutive")
    if (!consolidation_type %in% valid_consolidation_types) {
      stop(paste("Parameter 'consolidation_type' must be one of:", 
                 paste(valid_consolidation_types, collapse = ", "), 
                 "when consolidation_mode includes temporal consolidation"))
    }
  }
  
  # Warn if employer_var provided but not needed
  if (!is.null(employer_var) && !consolidation_mode %in% c("employer", "both")) {
    warning("Parameter 'employer_var' is ignored when consolidation_mode = '", consolidation_mode, "'")
  }
  
  # Handle output_transition_matrix interaction with statistics_variables
  if (output_transition_matrix && length(statistics_variables) > 0) {
    if (show_progress) {
      message(sprintf("Note: output_transition_matrix = TRUE, disabling statistics calculation for %d variables: %s", 
                     length(statistics_variables),
                     paste(statistics_variables, collapse = ", ")))
    }
    statistics_variables <- character(0)
  }
  
  # Validate eval_chain parameter
  valid_eval_chain <- c("last", "first", "none")
  if (!eval_chain %in% valid_eval_chain) {
    stop(paste("Parameter 'eval_chain' must be one of:", 
               paste(valid_eval_chain, collapse = ", ")))
  }
  
  # Check for over_id column if temporal consolidation is requested  
  if (consolidation_mode %in% c("temporal", "both")) {
    if (!"over_id" %in% names(pipeline_result)) {
      if (show_progress) {
        message("Note: 'over_id' column not found in data. Temporal consolidation disabled.")
      }
      # Adjust consolidation_mode if over_id is missing
      if (consolidation_mode == "temporal") {
        consolidation_mode <- "none"
      } else if (consolidation_mode == "both") {
        consolidation_mode <- "employer"
      }
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
  
  # Helper function for temporal consolidation
  .apply_temporal_consolidation <- function(data, consolidation_type, show_progress = FALSE) {
    # Use fast consolidation function with type support
    if (exists("merge_consecutive_employment_fast_with_type", mode = "function")) {
      # Pre-process data to ensure type consistency before consolidation
      data_copy <- copy(data)
      
      # Ensure consistent column types to prevent aggregation errors
      for (col_name in names(data_copy)) {
        col_class <- class(data_copy[[col_name]])[1]
        if (col_class %in% c("integer", "numeric")) {
          if (col_class == "integer") {
            # Convert integers to numeric to avoid type conflicts during consolidation
            data.table::set(data_copy, j = col_name, value = as.numeric(data_copy[[col_name]]))
          }
        }
      }
      
      return(merge_consecutive_employment_fast_with_type(data_copy, consolidation_type = consolidation_type))
      
    } else if (exists("merge_consecutive_employment_fast", mode = "function")) {
      # Use fast version without type parameter (vecshift 0.9.0+)
      if (show_progress) {
        message("Using fast consolidation method")
      }
      
      data_copy <- copy(data)
      
      # Ensure consistent column types
      for (col_name in names(data_copy)) {
        col_class <- class(data_copy[[col_name]])[1]
        if (col_class %in% c("integer", "numeric")) {
          if (col_class == "integer") {
            data.table::set(data_copy, j = col_name, value = as.numeric(data_copy[[col_name]]))
          }
        }
      }
      
      return(merge_consecutive_employment_fast(data_copy))
      
    } else if (exists("merge_consecutive_employment", mode = "function")) {
      # Fall back to slow function if fast version not available
      if (show_progress) {
        message("Using standard (slower) consolidation method - consider updating vecshift package")
      }
      
      data_copy <- copy(data)
      
      # Ensure consistent column types
      for (col_name in names(data_copy)) {
        col_class <- class(data_copy[[col_name]])[1]
        if (col_class %in% c("integer", "numeric")) {
          if (col_class == "integer") {
            data.table::set(data_copy, j = col_name, value = as.numeric(data_copy[[col_name]]))
          }
        }
      }
      
      return(merge_consecutive_employment(data_copy, consolidation_type = consolidation_type))
      
    } else {
      stop("No consolidation function found. Ensure the vecshift package is properly loaded.")
    }
  }
  
  if (show_progress) {
    message("Starting employment transition analysis...")
  }
  
  # Apply period consolidation based on consolidation_mode
  if (consolidation_mode != "none") {
    # Check if consolidation has already been done
    if ("collapsed" %in% names(pipeline_result)) {
      if (show_progress) {
        message("Data already contains 'collapsed' column - skipping consolidation step")
      }
      dt <- copy(pipeline_result)
    } else {
      if (show_progress) {
        if (consolidation_mode == "employer") {
          message(sprintf("Consolidating employment periods by employer using '%s' variable and %d-day lag...", 
                         employer_var, min_lag))
        } else if (consolidation_mode == "temporal") {
          message(sprintf("Consolidating employment periods using '%s' temporal strategy...", consolidation_type))
        } else if (consolidation_mode == "both") {
          message(sprintf("Consolidating employment periods: first by employer ('%s', %d-day lag), then temporal ('%s')...", 
                         employer_var, min_lag, consolidation_type))
        }
      }
      
      # Execute consolidation based on mode
      if (consolidation_mode == "employer") {
        # Use employer-based consolidation only
        dt <- consolidate_by_employer(pipeline_result, employer_var, min_lag)
        
      } else if (consolidation_mode == "temporal") {
        # Use temporal consolidation only (original vecshift method)
        dt <- .apply_temporal_consolidation(pipeline_result, consolidation_type, show_progress)
        
      } else if (consolidation_mode == "both") {
        # Sequential consolidation: employer first, then temporal
        if (show_progress) {
          message("Step 1: Applying employer-based consolidation...")
        }
        
        # Step 1: Employer consolidation
        dt_employer <- consolidate_by_employer(pipeline_result, employer_var, min_lag)
        
        if (show_progress) {
          n_original <- nrow(pipeline_result)
          n_employer <- nrow(dt_employer)
          reduction_pct <- round((1 - n_employer/n_original) * 100, 1)
          message(sprintf("Employer consolidation: %d → %d periods (%.1f%% reduction)", 
                         n_original, n_employer, reduction_pct))
          message("Step 2: Applying temporal consolidation...")
        }
        
        # Step 2: Temporal consolidation on employer-consolidated data
        dt <- .apply_temporal_consolidation(dt_employer, consolidation_type, show_progress)
        
        if (show_progress) {
          n_final <- nrow(dt)
          total_reduction_pct <- round((1 - n_final/n_original) * 100, 1)
          message(sprintf("Combined consolidation: %d → %d periods (%.1f%% total reduction)", 
                         n_original, n_final, total_reduction_pct))
        }
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
      
      if (show_progress && consolidation_mode != "both") {
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
    consolidation_mode = ifelse(consolidation_type == "none", "none", "temporal"),
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
      consolidation_mode = "none",  # No consolidation for raw matrix
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


#' Helper Function: Extract Global State Space from Dataset for Memory Management
#'
#' @description
#' Internal helper function that analyzes the complete dataset to identify all unique 
#' states in a transition variable. This state space is used for memory estimation 
#' and determining whether to use global or period-specific matrix dimensions. 
#' Critical for preventing out-of-memory errors on large datasets with many states 
#' (e.g., 290,575+ unique employment transitions).
#'
#' @param pipeline_result Output from process_employment_pipeline(). Must contain 
#'   the specified transition variable.
#' @param transition_variable Character string specifying the variable to analyze 
#'   for state extraction
#' @param eval_chain Character string specifying chain evaluation method ("first", 
#'   "last", "none") for processing composite state values
#'
#' @return Character vector of all unique states sorted alphabetically. Used for 
#'   memory estimation and matrix dimension planning.
#'
#' @details
#' This function processes transition values through the chain evaluation logic 
#' before extracting unique states, ensuring consistency with downstream matrix 
#' creation. The resulting state count is used to estimate memory requirements 
#' and trigger automatic fallback mechanisms in create_monthly_transition_matrices().
#'
#' @keywords internal
#' @noRd
.extract_global_state_space <- function(pipeline_result, transition_variable, eval_chain) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Extract the transition variable values
  transition_values <- pipeline_result[[transition_variable]]
  
  # Process chain values if needed
  processed_values <- .process_chain_value(transition_values, eval_chain)
  
  # Get unique non-NA values and sort them
  unique_states <- sort(unique(processed_values[!is.na(processed_values)]))
  
  return(unique_states)
}


#' Helper Function: Generate Time Period Boundaries and Names for Matrix Creation
#'
#' @description
#' Internal helper function that creates time period boundaries and generates consistent 
#' naming schemes for temporal transition matrices. Supports multiple time formats and 
#' naming conventions to enable flexible temporal analysis while maintaining consistency 
#' across different analysis periods.
#'
#' @param pipeline_result Output from process_employment_pipeline(). Used to determine 
#'   data date range when date_range parameter is NULL.
#' @param time_column Character string specifying the date column to use for determining 
#'   time periods (typically "fine" for end dates)
#' @param time_format Character string specifying period format: "monthly" (calendar months), 
#'   "quarterly" (calendar quarters), or "custom" (user-defined period length)
#' @param custom_period_days Integer number of days for custom periods. Required and 
#'   used only when time_format = "custom"
#' @param date_range Optional vector of two Date objects specifying analysis start and 
#'   end dates. If NULL, uses full range from pipeline_result data
#' @param name_format Character string specifying naming convention: "date" (e.g., "jan2022"), 
#'   "period" (e.g., "period_1"), or "custom" (user-provided names)
#' @param custom_names Optional character vector of custom period names. Required when 
#'   name_format = "custom", must match length of generated periods
#'
#' @return List containing:
#'   \itemize{
#'     \item{period_boundaries}: Date vector defining start/end boundaries for each time period
#'     \item{period_names}: Character vector of corresponding period names for matrix identification
#'   }
#'
#' @details
#' This function handles the complexity of creating consistent time periods across different 
#' formats while ensuring proper boundary handling. The period boundaries are used to filter 
#' transitions for each matrix, and the names become the matrix identifiers in the final output.
#'
#' @keywords internal
#' @noRd
.generate_time_periods <- function(pipeline_result, time_column, time_format, 
                                   custom_period_days, date_range, name_format, custom_names) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Determine date range from data or user input
  if (is.null(date_range)) {
    min_date <- min(pipeline_result[[time_column]], na.rm = TRUE)
    max_date <- max(pipeline_result[[time_column]], na.rm = TRUE)
  } else {
    min_date <- as.Date(date_range[1])
    max_date <- as.Date(date_range[2])
  }
  
  # Generate period boundaries based on time_format
  if (time_format == "monthly") {
    # Create monthly boundaries
    start_month <- as.Date(format(min_date, "%Y-%m-01"))
    end_month <- as.Date(format(max_date, "%Y-%m-01"))
    
    # Generate sequence of month starts
    period_starts <- seq.Date(from = start_month, to = end_month, by = "month")
    
    # Add one more period to get the end boundary
    period_boundaries <- c(period_starts, seq.Date(from = period_starts[length(period_starts)], 
                                                   length.out = 2, by = "month")[2])
    
    # Generate names based on name_format
    if (name_format == "date") {
      # Force English month names to avoid locale issues
      month_names <- c("jan", "feb", "mar", "apr", "may", "jun", 
                       "jul", "aug", "sep", "oct", "nov", "dec")
      month_nums <- as.numeric(format(period_starts, "%m"))
      years <- format(period_starts, "%Y")
      period_names <- paste0(month_names[month_nums], years)  # "jan2022", "feb2022", etc.
    } else if (name_format == "period") {
      period_names <- paste0("period_", seq_along(period_starts))
    } else if (name_format == "custom" && !is.null(custom_names)) {
      if (length(custom_names) != length(period_starts)) {
        stop("Length of custom_names must match number of periods")
      }
      period_names <- custom_names
    } else {
      period_names <- format(period_starts, "%Y-%m")  # "2022-01", "2022-02", etc.
    }
    
  } else if (time_format == "quarterly") {
    # Create quarterly boundaries
    start_quarter <- as.Date(paste0(format(min_date, "%Y"), "-", 
                                   sprintf("%02d", (as.numeric(format(min_date, "%m")) - 1) %/% 3 * 3 + 1), "-01"))
    
    # Generate sequence of quarter starts
    quarter_months <- seq(from = as.numeric(format(start_quarter, "%m")), 
                         to = 12 + as.numeric(format(max_date, "%m")), by = 3)
    quarter_years <- rep(as.numeric(format(start_quarter, "%Y")):as.numeric(format(max_date, "%Y")), each = 4)
    
    valid_quarters <- quarter_months <= 12
    quarter_dates <- as.Date(paste0(quarter_years[valid_quarters], "-", 
                                   sprintf("%02d", quarter_months[valid_quarters]), "-01"))
    quarter_dates <- quarter_dates[quarter_dates <= max_date]
    
    period_starts <- quarter_dates
    period_boundaries <- c(period_starts, seq.Date(from = period_starts[length(period_starts)], 
                                                   length.out = 2, by = "3 months")[2])
    
    # Generate quarterly names
    if (name_format == "date") {
      quarters <- ceiling(as.numeric(format(period_starts, "%m")) / 3)
      years <- format(period_starts, "%Y")
      period_names <- paste0(years, "Q", quarters)
      period_names <- tolower(period_names)  # "2022q1", "2022q2", etc.
    } else if (name_format == "period") {
      period_names <- paste0("period_", seq_along(period_starts))
    } else if (name_format == "custom" && !is.null(custom_names)) {
      if (length(custom_names) != length(period_starts)) {
        stop("Length of custom_names must match number of periods")
      }
      period_names <- custom_names
    } else {
      quarters <- ceiling(as.numeric(format(period_starts, "%m")) / 3)
      years <- format(period_starts, "%Y")
      period_names <- paste0(years, "-Q", quarters)  # "2022-Q1", "2022-Q2", etc.
    }
    
  } else if (time_format == "custom") {
    if (is.null(custom_period_days) || !is.numeric(custom_period_days) || custom_period_days <= 0) {
      stop("custom_period_days must be a positive integer when time_format is 'custom'")
    }
    
    # Create custom period boundaries
    period_starts <- seq.Date(from = min_date, to = max_date, by = custom_period_days)
    period_boundaries <- c(period_starts, period_starts[length(period_starts)] + custom_period_days)
    
    # Generate custom period names
    if (name_format == "date") {
      period_names <- format(period_starts, "%Y%m%d")
      period_names <- tolower(period_names)
    } else if (name_format == "period") {
      period_names <- paste0("period_", seq_along(period_starts))
    } else if (name_format == "custom" && !is.null(custom_names)) {
      if (length(custom_names) != length(period_starts)) {
        stop("Length of custom_names must match number of periods")
      }
      period_names <- custom_names
    } else {
      period_names <- paste0("custom_", seq_along(period_starts))
    }
  } else {
    stop("time_format must be one of: 'monthly', 'quarterly', 'custom'")
  }
  
  return(list(
    period_boundaries = period_boundaries,
    period_names = period_names
  ))
}


#' Create Monthly Transition Matrices from Vecshift Data with Advanced Memory Management
#'
#' @description
#' Generates a named list of transition matrices, one for each time period (month/quarter/custom),
#' from vecshift employment data. Features intelligent memory management for large datasets 
#' (290,575+ unique states) with automatic fallback mechanisms to prevent memory errors. 
#' Supports both sparse and dense matrix formats for optimal performance across different 
#' dataset sizes.
#'
#' @details
#' This function creates time-series transition matrices with advanced memory management:
#' \enumerate{
#'   \item **Memory Assessment**: Estimates memory requirements before allocation
#'   \item **State Space Strategy**: Chooses between global or period-specific state spaces
#'   \item **Smart Fallback**: Automatically switches modes when memory limits are exceeded
#'   \item **Matrix Format Selection**: Uses sparse matrices for large state spaces (>10,000 states)
#'   \item **Progress Monitoring**: Reports memory usage and performance warnings
#' }
#' 
#' **Key Features:**
#' \itemize{
#'   \item{\strong{Memory Efficiency}}: Intelligent memory management prevents out-of-memory errors
#'   \item{\strong{Automatic Optimization}}: Switches between global/local state spaces based on memory constraints
#'   \item{\strong{Sparse Matrix Support}}: Uses Matrix package for large datasets
#'   \item{\strong{Progress Reporting}}: Real-time memory usage and performance feedback
#'   \item{\strong{Flexible Periods}}: Monthly, quarterly, or custom time periods supported
#'   \item{\strong{Consistent Dimensions}}: Matrices maintain identical structure when using global state space
#'   \item{\strong{Transition Assignment}}: Uses "end" assignment - transitions assigned when "to" state begins
#' }
#' 
#' **Memory Management Modes:**
#' \itemize{
#'   \item{\strong{Global State Space} (use_global_state_space = TRUE)}: All matrices use same dimensions, higher memory usage
#'   \item{\strong{Period-Specific State Space} (use_global_state_space = FALSE, default)}: Each matrix uses only its states, memory efficient
#'   \item{\strong{Automatic Fallback}}: Switches to period-specific mode when memory exceeds limit
#' }
#' 
#' **Matrix Naming:**
#' \itemize{
#'   \item{\strong{date}}: "jan2022", "feb2022" (monthly), "2022q1", "2022q2" (quarterly)
#'   \item{\strong{period}}: "period_1", "period_2", etc.
#'   \item{\strong{custom}}: User-provided names
#' }
#'
#' @param pipeline_result Output from process_employment_pipeline(). Must be a data.table
#'   with columns: cf (person identifier), arco (employment overlap count), 
#'   inizio/fine (period dates), durata (period duration), and the transition variable.
#'   Optionally over_id if consolidation is used.
#' @param transition_variable Character string specifying the variable to use for 
#'   transition analysis (from/to values). If NULL (default), uses the first 
#'   non-standard attribute in the data.table.
#' @param time_column Character string specifying the date column for transition timing.
#'   Default: "fine" (end date of employment periods).
#' @param time_format Character string specifying time period format. One of:
#'   "monthly" (default), "quarterly", "custom".
#' @param custom_period_days Integer number of days for custom periods. Required when
#'   time_format = "custom".
#' @param date_range Optional vector of two Date objects specifying analysis start and 
#'   end dates. If NULL (default), uses full range from data.
#' @param name_format Character string specifying matrix naming convention. One of:
#'   "date" (default), "period", "custom".
#' @param custom_names Optional character vector of custom period names. Required when
#'   name_format = "custom".
#' @param matrix_format Character string specifying matrix output format. Options:
#'   \itemize{
#'     \item{"dense" (default)}: Standard R matrices, suitable for small to medium datasets
#'     \item{"sparse"}: Matrix package sparse matrices, optimal for large datasets with many zero entries
#'   }
#'   For datasets with >10,000 states, the function automatically switches to sparse format 
#'   regardless of this setting to prevent memory issues.
#' @param consolidation_type Character string specifying consolidation approach 
#'   (default: "both"). Options: "both", "overlapping", "consecutive", "none".
#' @param min_unemployment_duration Minimum duration (in days) of unemployment period 
#'   to consider a transition (default: 1).
#' @param max_unemployment_duration Maximum duration (in days) of unemployment period 
#'   to consider a transition. If NULL (default), no upper limit is applied.
#' @param matrix_type Character string specifying output matrix type. One of:
#'   "frequency" (default), "probability".
#' @param normalize_by Character string for probability matrices. One of: 
#'   "row" (default), "column", "total".
#' @param eval_chain Character string specifying how to handle chained values 
#'   (default: "last"). Options: "last", "first", "none".
#' @param include_summary Logical. If TRUE (default), includes summary information
#'   and metadata in the output.
#' @param show_progress Logical. If TRUE (default), displays progress messages.
#' @param use_global_state_space Logical. Memory management mode selection (default: FALSE). 
#'   \itemize{
#'     \item{FALSE (default)}: Uses period-specific state spaces for maximum memory efficiency. 
#'       Each matrix only includes states present in that period, dramatically reducing memory usage 
#'       for large datasets.
#'     \item{TRUE}: Uses global state space for consistent matrix dimensions across all periods. 
#'       All matrices have identical structure, but requires significant memory for large datasets 
#'       (e.g., 290,575+ states may require >1GB RAM).
#'   }
#' @param memory_limit_gb Numeric. Memory threshold in GB for automatic fallback protection 
#'   (default: 1.0). When estimated memory usage for dense matrices exceeds this limit, 
#'   the function automatically switches to period-specific state spaces regardless of 
#'   use_global_state_space setting. This prevents out-of-memory errors on large datasets. 
#'   Set higher (e.g., 4.0) if you have sufficient RAM and need global state space consistency.
#'
#' @return A named list containing:
#'   \itemize{
#'     \item{\strong{matrices}}: Named list of transition matrices, one per time period
#'     \item{\strong{metadata}} (if include_summary = TRUE): Information about the analysis including:
#'       \itemize{
#'         \item{global_state_space}: All unique states used across matrices
#'         \item{period_info}: Time period boundaries and names
#'         \item{matrix_dimensions}: Consistent dimensions (nrow x ncol)
#'         \item{total_periods}: Number of time periods analyzed
#'         \item{periods_with_transitions}: Number of periods containing transitions
#'         \item{analysis_parameters}: Function parameters used
#'       }
#'   }
#'
#' @examples
#' \dontrun{
#' # Load sample data
#' sample_data <- readRDS("data/sample.rds")
#' 
#' # Basic monthly transition matrices (memory-efficient, default)
#' monthly_matrices <- create_monthly_transition_matrices(
#'   sample_data,
#'   transition_variable = "COD_TIPOLOGIA_CONTRATTUALE"
#' )
#' 
#' # Access individual matrices
#' jan_matrix <- monthly_matrices$matrices$jan2022
#' feb_matrix <- monthly_matrices$matrices$feb2022
#' 
#' # Check memory usage and matrix info
#' monthly_matrices$metadata$memory_mode  # Shows which mode was used
#' monthly_matrices$metadata$estimated_memory_gb  # Memory estimation
#' 
#' # Global state space mode (consistent dimensions, higher memory)
#' global_matrices <- create_monthly_transition_matrices(
#'   sample_data,
#'   transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
#'   use_global_state_space = TRUE,
#'   memory_limit_gb = 2.0  # Allow higher memory usage
#' )
#' 
#' # All matrices have identical dimensions in global mode
#' identical(dim(global_matrices$matrices$jan2022), 
#'           dim(global_matrices$matrices$feb2022))  # TRUE
#' 
#' # Sparse matrices for large datasets
#' sparse_matrices <- create_monthly_transition_matrices(
#'   sample_data,
#'   transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
#'   matrix_format = "sparse",
#'   show_progress = TRUE  # Monitor memory usage
#' )
#' 
#' # Quarterly matrices with probability normalization
#' quarterly_prob <- create_monthly_transition_matrices(
#'   sample_data,
#'   transition_variable = "COD_TIPOLOGIA_CONTRATTUALE", 
#'   time_format = "quarterly",
#'   matrix_type = "probability",
#'   normalize_by = "row"
#' )
#' 
#' # Custom 90-day periods
#' custom_matrices <- create_monthly_transition_matrices(
#'   sample_data,
#'   transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
#'   time_format = "custom",
#'   custom_period_days = 90,
#'   name_format = "custom",
#'   custom_names = c("Q1_extended", "Q2_extended", "Q3_extended", "Q4_extended")
#' )
#' 
#' # Monitor performance on large datasets
#' large_result <- create_monthly_transition_matrices(
#'   large_data,
#'   show_progress = TRUE,      # Shows memory warnings
#'   memory_limit_gb = 0.5,    # Conservative memory limit
#'   matrix_format = "sparse"  # Use sparse matrices
#' )
#' }
#'
#' @importFrom data.table data.table setDT copy .N .SD := setorder setnames
#' @importFrom Matrix Matrix sparseMatrix
#' @export
create_monthly_transition_matrices <- function(pipeline_result,
                                             transition_variable = NULL,
                                             time_column = "fine",
                                             time_format = c("monthly", "quarterly", "custom"),
                                             custom_period_days = NULL,
                                             date_range = NULL,
                                             name_format = c("date", "period", "custom"),
                                             custom_names = NULL,
                                             matrix_format = c("dense", "sparse"),
                                             consolidation_type = "both",
                                             min_unemployment_duration = 1,
                                             max_unemployment_duration = NULL,
                                             matrix_type = c("frequency", "probability"),
                                             normalize_by = "row",
                                             eval_chain = "last",
                                             include_summary = TRUE,
                                             show_progress = TRUE,
                                             use_global_state_space = FALSE,
                                             memory_limit_gb = 1.0) {
  
  # Record start time
  start_time <- Sys.time()
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Match arguments
  time_format <- match.arg(time_format)
  name_format <- match.arg(name_format)
  matrix_format <- match.arg(matrix_format)
  matrix_type <- match.arg(matrix_type)
  
  # Load Matrix package for sparse matrix support
  if (matrix_format == "sparse" && !requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required for sparse matrix format but not installed.")
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
  
  # Check time_column exists
  if (!time_column %in% names(pipeline_result)) {
    stop(sprintf("Time column '%s' not found in pipeline_result.", time_column))
  }
  
  # Auto-detect transition variable if not provided
  if (is.null(transition_variable)) {
    standard_cols <- c("cf", "arco", "inizio", "fine", "durata", "over_id")
    available_vars <- setdiff(names(pipeline_result), standard_cols)
    if (length(available_vars) == 0) {
      stop("No suitable transition variable found. Please specify transition_variable parameter.")
    }
    transition_variable <- available_vars[1]
    if (show_progress) {
      message(sprintf("Using '%s' as transition variable (auto-detected).", transition_variable))
    }
  }
  
  # Validate transition_variable
  if (!transition_variable %in% names(pipeline_result)) {
    stop(sprintf("Transition variable '%s' not found in pipeline_result.", transition_variable))
  }
  
  # Validate consolidation requirements
  if (consolidation_type != "none" && !"over_id" %in% names(pipeline_result)) {
    stop("Parameter 'pipeline_result' must contain 'over_id' column from vecshift() output when consolidation_type is not 'none'.")
  }
  
  # Validate custom parameters
  if (time_format == "custom" && (is.null(custom_period_days) || !is.numeric(custom_period_days) || custom_period_days <= 0)) {
    stop("custom_period_days must be a positive integer when time_format is 'custom'")
  }
  
  if (name_format == "custom" && is.null(custom_names)) {
    stop("custom_names must be provided when name_format is 'custom'")
  }
  
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
  
  # Validate memory parameters
  if (!is.logical(use_global_state_space)) {
    stop("Parameter 'use_global_state_space' must be TRUE or FALSE.")
  }
  
  if (!is.numeric(memory_limit_gb) || memory_limit_gb <= 0) {
    stop("Parameter 'memory_limit_gb' must be a positive numeric value.")
  }
  
  # Progress message
  if (show_progress) {
    message("Starting monthly transition matrices creation...")
    message(sprintf("Dataset contains %d rows across %d individuals", 
                   nrow(pipeline_result), 
                   length(unique(pipeline_result$cf))))
  }
  
  # Step 1: Handle state space extraction with memory considerations
  if (show_progress) {
    message("Step 1: Analyzing state space...")
  }
  
  global_states <- .extract_global_state_space(pipeline_result, transition_variable, eval_chain)
  n_states <- length(global_states)
  
  if (show_progress) {
    states_preview <- paste(global_states[1:min(5, n_states)], collapse = ", ")
    states_preview <- if (n_states > 5) paste0(states_preview, "...") else states_preview
    message(sprintf("Found %d unique states: %s", n_states, states_preview))
  }
  
  # Step 2: Generate time period boundaries and names
  if (show_progress) {
    message("Step 2: Generating time period boundaries...")
  }
  
  time_periods <- .generate_time_periods(pipeline_result, time_column, time_format, 
                                          custom_period_days, date_range, name_format, custom_names)
  
  period_boundaries <- time_periods$period_boundaries
  period_names <- time_periods$period_names
  n_periods <- length(period_names)
  
  # Calculate estimated memory usage for dense matrices
  estimated_memory_gb <- (n_states^2 * 8 * n_periods) / (1024^3)  # 8 bytes per double, convert to GB
  
  if (show_progress) {
    message(sprintf("Created %d time periods from %s to %s", 
                   n_periods, 
                   format(period_boundaries[1], "%Y-%m-%d"),
                   format(period_boundaries[length(period_boundaries)], "%Y-%m-%d")))
    message(sprintf("Estimated memory for dense matrices: %.2f GB", estimated_memory_gb))
  }
  
  # Automatically switch to period-specific state spaces if memory would be too high
  if (estimated_memory_gb > memory_limit_gb && use_global_state_space) {
    if (show_progress) {
      message(sprintf("WARNING: Estimated memory usage (%.2f GB) exceeds limit (%.2f GB)", 
                     estimated_memory_gb, memory_limit_gb))
      message("Automatically switching to period-specific state spaces for memory efficiency...")
    }
    use_global_state_space <- FALSE
  }
  
  # Force sparse format for large state spaces
  if (n_states > 10000 && matrix_format == "dense") {
    if (show_progress) {
      message(sprintf("Large state space (%d states) detected. Switching to sparse matrix format.", n_states))
    }
    matrix_format <- "sparse"
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("Package 'Matrix' is required for large state spaces but not installed. Please install it or reduce the state space size.")
    }
  }
  
  # Step 3: Create matrices for each time period
  if (show_progress) {
    if (use_global_state_space) {
      message("Step 3: Creating transition matrices with global state space...")
    } else {
      message("Step 3: Creating transition matrices with period-specific state spaces...")
    }
  }
  
  matrices_list <- vector("list", n_periods)
  names(matrices_list) <- period_names
  periods_with_transitions <- 0
  
  # Create template matrix only if using global state space
  template_matrix <- NULL
  if (use_global_state_space) {
    if (matrix_format == "sparse") {
      template_matrix <- Matrix::Matrix(0, 
                                       nrow = n_states, 
                                       ncol = n_states,
                                       dimnames = list(global_states, global_states),
                                       sparse = TRUE)
    } else {
      template_matrix <- matrix(0, 
                               nrow = n_states, 
                               ncol = n_states,
                               dimnames = list(global_states, global_states))
    }
  }
  
  # STEP 3: Extract ALL transitions with their timing ONCE
  if (show_progress) {
    message("Extracting all transitions with timing information...")
  }
  
  # First, consolidate data if required
  if (consolidation_type != "none") {
    # Pre-process data to ensure type consistency before consolidation
    # This prevents data.table column type mismatch errors during aggregation
    # MEMORY-EFFICIENT: Convert columns in-place on original data using := operator
    for (col_name in names(pipeline_result)) {
      col_class <- class(pipeline_result[[col_name]])[1]
      if (col_class %in% c("integer", "numeric")) {
        # Check if column has mixed types that could cause issues during aggregation
        if (col_class == "integer") {
          # Convert integers to numeric to avoid type conflicts during consolidation
          # This is safer as numeric can handle both integer and decimal aggregations
          # Use := for in-place modification without copying data
          pipeline_result[, (col_name) := as.numeric(get(col_name))]
        }
      }
    }
    
    # Use existing merge_consecutive_employment function
    if (exists("merge_consecutive_employment", mode = "function")) {
      consolidated_data <- merge_consecutive_employment(
        pipeline_result, 
        consolidation_type = consolidation_type
      )
    } else if (requireNamespace("vecshift", quietly = TRUE)) {
      # Try to get the function from vecshift package
      if (exists("merge_consecutive_employment", envir = asNamespace("vecshift"))) {
        merge_func <- get("merge_consecutive_employment", envir = asNamespace("vecshift"))
        consolidated_data <- merge_func(
          pipeline_result, 
          consolidation_type = consolidation_type
        )
      } else {
        if (show_progress) {
          message("Warning: merge_consecutive_employment function not found. Proceeding without consolidation.")
        }
        consolidated_data <- pipeline_result
      }
    } else {
      if (show_progress) {
        message("Warning: vecshift package not available for consolidation. Proceeding without consolidation.")
      }
      consolidated_data <- pipeline_result
    }
  } else {
    consolidated_data <- pipeline_result
  }
  
  # Get employment periods only and order by person and start date
  employment_data <- consolidated_data[arco >= 1]
  setorder(employment_data, cf, inizio)
  
  # Extract ALL transitions with their to_date
  all_transitions_list <- list()
  
  # Filter to people with at least 2 employment periods (need transitions)
  person_period_counts <- employment_data[, .N, by = cf]
  multi_period_people <- person_period_counts[N >= 2, cf]
  
  if (length(multi_period_people) == 0) {
    if (show_progress) {
      message("No people with multiple employment periods found - no transitions possible")
    }
    # Return empty results
    matrices_list <- vector("list", n_periods)
    names(matrices_list) <- period_names
    for (i in 1:n_periods) {
      if (matrix_format == "sparse") {
        matrices_list[[i]] <- Matrix::Matrix(0, nrow = 0, ncol = 0, sparse = TRUE)
      } else {
        matrices_list[[i]] <- matrix(0, nrow = 0, ncol = 0)
      }
    }
    results <- list(matrices = matrices_list)
    if (include_summary) {
      results$metadata <- list(
        periods_with_transitions = 0,
        total_periods = n_periods
      )
    }
    return(results)
  }
  
  # Process only people with multiple periods
  employment_data <- employment_data[cf %in% multi_period_people]
  
  # OPTIMIZED: Vectorized transition extraction using data.table operations
  if (show_progress) {
    message("Extracting transitions using optimized vectorized approach...")
  }
  
  # Create lagged values for each person using data.table shift
  employment_data[, `:=`(
    prev_fine = shift(fine, 1, type = "lag"),
    prev_transition_var = shift(get(transition_variable), 1, type = "lag")
  ), by = cf]
  
  # Filter to periods that have a previous period (excluding first period for each person)
  potential_transitions <- employment_data[!is.na(prev_fine)]
  
  # Calculate unemployment duration vectorized
  potential_transitions[, unemployment_duration := as.numeric(inizio - prev_fine - 1)]
  
  # Apply unemployment duration filters
  if (!is.null(max_unemployment_duration)) {
    valid_transitions <- potential_transitions[
      unemployment_duration >= min_unemployment_duration & 
      unemployment_duration <= max_unemployment_duration
    ]
  } else {
    valid_transitions <- potential_transitions[unemployment_duration >= min_unemployment_duration]
  }
  
  if (nrow(valid_transitions) == 0) {
    if (show_progress) {
      message("No transitions meet unemployment duration criteria")
    }
    all_transitions_dt <- data.table(
      cf = integer(0), from = character(0), to = character(0), 
      to_date = as.Date(character(0)), unemployment_duration = numeric(0)
    )
  } else {
    # Process chain values vectorized (much more efficient)
    from_processed <- .process_chain_value(valid_transitions$prev_transition_var, eval_chain)
    to_processed <- .process_chain_value(valid_transitions[[transition_variable]], eval_chain)
    
    # Create transitions data.table directly (no list growing)
    all_transitions_dt <- data.table(
      cf = valid_transitions$cf,
      from = from_processed,
      to = to_processed,
      to_date = valid_transitions$inizio,
      unemployment_duration = valid_transitions$unemployment_duration
    )
    
    # Filter out invalid transitions (NA values or same from/to)
    all_transitions_dt <- all_transitions_dt[
      !is.na(from) & !is.na(to) & from != to
    ]
  }
  
  # Clean up temporary columns
  employment_data[, c("prev_fine", "prev_transition_var") := NULL]
  
  # Check if we have valid transitions
  if (nrow(all_transitions_dt) == 0) {
    if (show_progress) {
      message("No valid transitions found in the data")
    }
    # Return empty results
    matrices_list <- vector("list", n_periods)
    names(matrices_list) <- period_names
    for (i in 1:n_periods) {
      if (matrix_format == "sparse") {
        matrices_list[[i]] <- Matrix::Matrix(0, nrow = 0, ncol = 0, sparse = TRUE)
      } else {
        matrices_list[[i]] <- matrix(0, nrow = 0, ncol = 0)
      }
    }
    results <- list(matrices = matrices_list)
    if (include_summary) {
      results$metadata <- list(
        periods_with_transitions = 0,
        total_periods = n_periods
      )
    }
    return(results)
  }
  
  # Ensure consistent column types
  all_transitions_dt[, unemployment_duration := as.double(unemployment_duration)]
  
  if (show_progress) {
    message(sprintf("Extracted %d total transitions across all time periods", nrow(all_transitions_dt)))
  }
  
  # Now create matrices for each time period by filtering transitions by to_date
  for (i in 1:n_periods) {
    period_start <- period_boundaries[i]
    period_end <- period_boundaries[i + 1] - 1  # Subtract 1 day to avoid overlap
    
    if (show_progress && (i == 1 || i %% 5 == 0 || i == n_periods)) {
      message(sprintf("Processing period %d/%d: %s (%s to %s)", 
                     i, n_periods, period_names[i],
                     format(period_start, "%Y-%m-%d"),
                     format(period_end, "%Y-%m-%d")))
    }
    
    # Filter transitions where to_date falls in this period
    period_transitions <- all_transitions_dt[to_date >= period_start & to_date <= period_end]
    
    if (nrow(period_transitions) == 0) {
      # No transitions in this period - create empty matrix
      if (use_global_state_space && !is.null(template_matrix)) {
        matrices_list[[i]] <- template_matrix
      } else {
        if (matrix_format == "sparse") {
          matrices_list[[i]] <- Matrix::Matrix(0, nrow = 0, ncol = 0, sparse = TRUE)
        } else {
          matrices_list[[i]] <- matrix(0, nrow = 0, ncol = 0)
        }
      }
      next
    }
    
    # Aggregate transitions by from-to pairs
    aggregated_transitions <- period_transitions[, .(weight = .N), by = .(from, to)]
    
    # Create matrix from aggregated transitions
    if (use_global_state_space) {
      # Use global state space for consistent dimensions
      if (matrix_format == "sparse") {
        period_matrix <- Matrix::Matrix(0, nrow = n_states, ncol = n_states,
                                       dimnames = list(global_states, global_states),
                                       sparse = TRUE)
      } else {
        period_matrix <- matrix(0, nrow = n_states, ncol = n_states,
                               dimnames = list(global_states, global_states))
      }
      
      # OPTIMIZED: Fill matrix using vectorized operations
      if (nrow(aggregated_transitions) > 0) {
        # Convert states to indices for faster matrix access
        from_indices <- match(as.character(aggregated_transitions$from), global_states)
        to_indices <- match(as.character(aggregated_transitions$to), global_states)
        
        # Filter out any NA matches (states not in global space)
        valid_idx <- !is.na(from_indices) & !is.na(to_indices)
        
        if (any(valid_idx)) {
          from_indices <- from_indices[valid_idx]
          to_indices <- to_indices[valid_idx]
          weights <- aggregated_transitions$weight[valid_idx]
          
          # Use matrix indexing instead of character-based access
          for (k in seq_along(from_indices)) {
            period_matrix[from_indices[k], to_indices[k]] <- period_matrix[from_indices[k], to_indices[k]] + weights[k]
          }
        }
      }
      
    } else {
      # Use period-specific state space
      unique_states_period <- sort(unique(c(aggregated_transitions$from, aggregated_transitions$to)))
      
      if (matrix_format == "sparse") {
        period_matrix <- Matrix::Matrix(0, 
                                       nrow = length(unique_states_period), 
                                       ncol = length(unique_states_period),
                                       dimnames = list(unique_states_period, unique_states_period),
                                       sparse = TRUE)
      } else {
        period_matrix <- matrix(0, 
                               nrow = length(unique_states_period), 
                               ncol = length(unique_states_period),
                               dimnames = list(unique_states_period, unique_states_period))
      }
      
      # OPTIMIZED: Fill matrix with vectorized operations
      if (nrow(aggregated_transitions) > 0) {
        # Convert to indices for faster access
        from_indices <- match(as.character(aggregated_transitions$from), unique_states_period)
        to_indices <- match(as.character(aggregated_transitions$to), unique_states_period)
        
        # Use matrix indexing instead of character-based access
        for (k in 1:nrow(aggregated_transitions)) {
          period_matrix[from_indices[k], to_indices[k]] <- period_matrix[from_indices[k], to_indices[k]] + aggregated_transitions$weight[k]
        }
      }
    }
    
    matrices_list[[i]] <- period_matrix
    if (sum(period_matrix) > 0) {
      periods_with_transitions <- periods_with_transitions + 1
    }
  }
  
  # Step 4: Convert to probability matrices if requested
  if (matrix_type == "probability") {
    if (show_progress) {
      message("Step 4: Converting to probability matrices...")
    }
    
    for (i in 1:n_periods) {
      if (!is.null(matrices_list[[i]]) && (is.matrix(matrices_list[[i]]) || inherits(matrices_list[[i]], "Matrix"))) {
        matrices_list[[i]] <- .normalize_transition_matrix(matrices_list[[i]], normalize_by)
      }
    }
  }
  
  # Calculate elapsed time
  elapsed_time <- Sys.time() - start_time
  
  if (show_progress) {
    if (use_global_state_space) {
      message(sprintf("Completed in %.2f seconds. Created %d matrices (%d with transitions) of %dx%d dimensions.", 
                     as.numeric(elapsed_time), n_periods, periods_with_transitions, n_states, n_states))
    } else {
      # Calculate average matrix dimensions for period-specific approach
      actual_dims <- lapply(matrices_list, function(m) if (is.matrix(m) || inherits(m, "Matrix")) dim(m) else c(0, 0))
      avg_rows <- mean(sapply(actual_dims, function(d) d[1]), na.rm = TRUE)
      avg_cols <- mean(sapply(actual_dims, function(d) d[2]), na.rm = TRUE)
      message(sprintf("Completed in %.2f seconds. Created %d matrices (%d with transitions). Average dimensions: %.0fx%.0f.", 
                     as.numeric(elapsed_time), n_periods, periods_with_transitions, avg_rows, avg_cols))
    }
  }
  
  # Prepare results
  results <- list(matrices = matrices_list)
  
  # Add summary information if requested
  if (include_summary) {
    results$metadata <- list(
      global_state_space = if (use_global_state_space) global_states else NULL,
      all_unique_states = global_states,  # Always include for reference
      period_info = list(
        boundaries = period_boundaries,
        names = period_names
      ),
      matrix_dimensions = if (use_global_state_space) c(n_states, n_states) else "variable (period-specific)",
      matrix_format = matrix_format,
      use_global_state_space = use_global_state_space,
      estimated_memory_gb = estimated_memory_gb,
      total_periods = n_periods,
      periods_with_transitions = periods_with_transitions,
      analysis_parameters = list(
        transition_variable = transition_variable,
        time_column = time_column,
        time_format = time_format,
        matrix_format = matrix_format,
        matrix_type = matrix_type,
        consolidation_type = consolidation_type,
        eval_chain = eval_chain,
        min_unemployment_duration = min_unemployment_duration,
        max_unemployment_duration = max_unemployment_duration
      ),
      processing_time = elapsed_time
    )
  }
  
  return(results)
}